## Recast the deterministic Figure 2 (profits.R) as a probability object.
## Referee 2 (R2), main comment on Fig 2: "It would be useful to know what the
## market for recycled seeds looks like ... I am wondering whether it would
## make sense to put some probabilities on this rather than what I assume are
## average [price] and average yield ... this figure could become central to
## the argument [about downside risk]."
##
## This script does NOT touch profits.R or any other existing file. It builds
## a Monte Carlo companion to Fig 2 using the empirical distribution of grain
## sale prices observed in the endline, combined with a grid of assumed yield
## penalties for recycled (vs. fresh) Bazooka seed, and reports the
## probability that fresh Bazooka purchase pays off relative to recycling and
## relative to local seed.
##
## Point-estimate inputs (from the paper, sections 6.1/6.2, and profits.R):
##   fresh Bazooka yield   : 535 kg/acre (5.35 cwt/acre; profits.R uses slope
##                           5.3, we keep 535 kg exactly as stated in the task)
##   recycled-once Bazooka : 375 kg/acre (slope 3.75 in profits.R)
##   local seed             : 323 kg/acre (slope 3.23 in profits.R)
##   fresh seed cost        : ~8 kg/acre * UGX 12,000/kg = UGX 96,000/acre
##   recycled & local seed cost: assumed ~0 (as in profits.R, no cost term)
##   observed one-cycle yield penalty: (535-375)/535 = 0.2991
##
## Run from the repository root:
##   Rscript papers/increasing_seed_varietal_turnover/analysis/fig2_probability.R

suppressMessages({
  library(ggplot2)
})

set.seed(20260911)  # reproducibility for the Monte Carlo draws

## ---------------------------------------------------------------------------
## 1. Load endline data and apply the paper's standard sample restriction
##    (same restriction used in recycling_decomposition.R): drop the
##    companion-study paid/discounted trial-pack households, and keep only
##    farmers who gave consent to be interviewed at endline.
## ---------------------------------------------------------------------------

d_raw <- read.csv("endline/data/public/endline.csv", stringsAsFactors = FALSE)

n0 <- nrow(d_raw)
d1 <- subset(d_raw, !(paid_pac == "TRUE" | discounted == "TRUE"))
n1 <- nrow(d1)
d2 <- subset(d1, consent == "Yes")
n2 <- nrow(d2)

cat("=== Sample restriction funnel (endline.csv) ===\n")
cat(sprintf("  raw rows                                   : %d\n", n0))
cat(sprintf("  after dropping paid/discounted trial packs  : %d  (-%d)\n", n1, n0 - n1))
cat(sprintf("  after keeping consent == 'Yes'              : %d  (-%d)\n", n2, n1 - n2))
d <- d2

## ---------------------------------------------------------------------------
## 2. Construct a per-100kg-bag grain sale price.
##
## Variable choice: endline.csv has bag_harv (bags harvested), bag_kg (kg per
## bag, i.e. bag size), bag_sell (NUMBER of bags sold -- not a price), and
## bag_charge (UGX charged per bag sold). analysis.R (lines ~425-437,
## 774-781) confirms bag_charge is the price field: it is later thresholded
## (>200000 -> NA) and used to build a "high price" indicator. bag_sell is a
## quantity (bags sold), not a price, despite the superficially price-like
## name -- we do NOT use it as a price.
##
## price_per_100kg = bag_charge (UGX per bag) / bag_kg (kg per bag) * 100
##
## bag_charge is only observed for farmers who actually sold grain (about
## 63% of the restricted sample report no sale / n/a), so this is a
## seller-conditional sale-price distribution, not a full-sample distribution
## of "prices households could get." That is the appropriate concept here:
## Fig 2 is about the price a farmer selling grain will actually realize.
##
## Missing codes: "n/a" and the numeric missing code "999" are set to NA in
## both bag_charge and bag_kg before division.
## ---------------------------------------------------------------------------

bag_charge <- d$bag_charge
bag_charge[bag_charge %in% c("n/a", "999")] <- NA
bag_charge <- as.numeric(bag_charge)

bag_kg <- d$bag_kg
bag_kg[bag_kg %in% c("n/a", "999")] <- NA
bag_kg <- as.numeric(bag_kg)

price_100kg_raw <- bag_charge / bag_kg * 100

cat(sprintf("\nN with non-missing bag_charge & bag_kg (pre-trim) : %d\n",
            sum(!is.na(price_100kg_raw))))

## Winsorize/trim the extreme 1% in each tail (2% total), reusing the same
## logic and default threshold as the trim() helper in analysis.R (line 54),
## applied here to price_100kg rather than re-sourcing that function on a
## data.frame column, to keep this script fully self-contained.
trim_perc <- 0.02
trim_bounds <- quantile(price_100kg_raw, c(trim_perc / 2, 1 - trim_perc / 2), na.rm = TRUE)
price_100kg <- price_100kg_raw
price_100kg[price_100kg < trim_bounds[1] | price_100kg > trim_bounds[2]] <- NA
price_100kg <- price_100kg[!is.na(price_100kg)]

cat(sprintf("Trimming applied                                    : bottom/top %.0f%% (analysis.R trim() convention, trim_perc = %.2f)\n",
            100 * trim_perc / 2, trim_perc))
cat(sprintf("Trim bounds (UGX per 100kg)                         : [%.0f, %.0f]\n",
            trim_bounds[1], trim_bounds[2]))
cat(sprintf("N used to build empirical price distribution        : %d\n", length(price_100kg)))

price_summary <- summary(price_100kg)
price_iqr <- IQR(price_100kg)
cat("\n=== Empirical grain sale price distribution (UGX / 100kg bag) ===\n")
print(round(price_summary, 0))
cat(sprintf("IQR: %.0f\n", price_iqr))

## ---------------------------------------------------------------------------
## 3. Fixed accounting inputs (paper point estimates)
## ---------------------------------------------------------------------------

fresh_yield_kg   <- 535    # kg/acre, fresh Bazooka hybrid
local_yield_kg   <- 323    # kg/acre, local (recycled/farmer-saved) seed
seed_cost_fresh  <- 96000  # UGX/acre, 8kg * 12,000 UGX/kg
observed_penalty <- (fresh_yield_kg - 375) / fresh_yield_kg  # ~0.2991, one recycling cycle

## Yield-penalty grid for recycled Bazooka, expressed relative to fresh yield.
## 0.30 is labeled as the observed one-cycle penalty; 0.396 would degrade
## recycled yield all the way down to the local-seed benchmark
## ((535-323)/535 = 0.396) -- we use 0.40 as a round grid point close to that.
penalty_grid <- c(0.00, 0.10, 0.20, 0.30, 0.40)
penalty_labels <- ifelse(
  abs(penalty_grid - round(observed_penalty, 2)) < 0.005,
  sprintf("%.0f%% (observed, one recycling cycle)", 100 * penalty_grid),
  sprintf("%.0f%%", 100 * penalty_grid)
)

## ---------------------------------------------------------------------------
## 4. Monte Carlo: draw grain prices (with replacement) from the empirical,
##    trimmed price distribution, B times, and evaluate profit comparisons
##    for each yield-penalty grid point.
##
## Assumptions carried over unchanged from profits.R:
##   - recycled and local seed have zero (or negligible) monetary seed cost
##   - fresh Bazooka costs UGX 96,000/acre in seed
##   - yields are treated as fixed (point-estimate) conditional on penalty;
##     all of the uncertainty modeled here is in the sale PRICE, which is
##     empirical, not assumed -- exactly what R2 asked for. Yield uncertainty
##     is handled separately via the penalty grid (a sensitivity axis), not
##     as a further stochastic draw, since the paper does not report a yield
##     distribution for recycled seed by cycle count.
## ---------------------------------------------------------------------------

B <- 10000
price_draws <- sample(price_100kg, size = B, replace = TRUE)

run_penalty <- function(penalty) {
  recycled_yield_kg <- fresh_yield_kg * (1 - penalty)

  profit_fresh    <- fresh_yield_kg / 100 * price_draws - seed_cost_fresh
  profit_recycled <- recycled_yield_kg / 100 * price_draws            # zero cost
  profit_local    <- local_yield_kg / 100 * price_draws               # zero cost

  prob_fresh_beats_recycled <- mean(profit_fresh > profit_recycled)
  prob_recycled_beats_local <- mean(profit_recycled > profit_local)
  prob_fresh_beats_local    <- mean(profit_fresh > profit_local)

  ## Closed-form breakeven price: fresh > recycled iff
  ##   (fresh_yield - recycled_yield)/100 * price > seed_cost_fresh
  yield_gap <- (fresh_yield_kg - recycled_yield_kg) / 100
  breakeven_price <- if (yield_gap > 0) seed_cost_fresh / yield_gap else Inf
  pct_below_breakeven <- if (is.finite(breakeven_price)) {
    mean(price_draws < breakeven_price)
  } else {
    1  # fresh never beats recycled at zero yield penalty, for any price
  }

  data.frame(
    yield_penalty                    = penalty,
    recycled_yield_kg_acre            = round(recycled_yield_kg, 1),
    breakeven_price_UGX_per_100kg     = round(breakeven_price, 0),
    pct_of_price_draws_below_breakeven = round(pct_below_breakeven, 4),
    MC_prob_recycled_beats_fresh      = round(1 - prob_fresh_beats_recycled, 4),
    MC_prob_fresh_beats_recycled      = round(prob_fresh_beats_recycled, 4),
    prob_recycled_beats_local        = round(prob_recycled_beats_local, 4),
    prob_fresh_beats_local           = round(prob_fresh_beats_local, 4)
  )
}

results <- do.call(rbind, lapply(penalty_grid, run_penalty))

cat("\n=== Monte Carlo probability table (B = ", B, " price draws per row) ===\n", sep = "")
cat("Note: pct_of_price_draws_below_breakeven is a closed-form sanity check;\n")
cat("it should equal (1 - MC_prob_fresh_beats_recycled) up to MC noise.\n")
cat("prob_recycled_beats_local is DEGENERATE (0 or 1) by construction: both\n")
cat("recycled and local seed carry zero assumed cost in this model, so their\n")
cat("profit ranking is fixed by the yield ranking alone, independent of price.\n\n")
print(results, row.names = FALSE)

## ---------------------------------------------------------------------------
## 5. Figure 1: probability fresh Bazooka beats recycled Bazooka, vs. yield
##    penalty grid. This is the headline "downside risk" figure R2 asked for.
## ---------------------------------------------------------------------------

fig_df <- results
fig_df$label <- penalty_labels

p_main <- ggplot(fig_df, aes(x = yield_penalty, y = MC_prob_fresh_beats_recycled)) +
  geom_line(linewidth = 1.1, color = "#1b4f72") +
  geom_point(size = 3, color = "#1b4f72") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                      breaks = penalty_grid) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                      limits = c(0, 1)) +
  labs(
    x = "Assumed yield penalty for recycled Bazooka (vs. fresh)",
    y = "P(fresh beats recycled\nprofit per acre)",
    title = "Probability that buying fresh seed pays off vs. recycling",
    subtitle = sprintf("Monte Carlo over empirical endline grain sale prices (N = %d, B = %d draws)",
                        length(price_100kg), B),
    caption = "Vertical reference: 30% penalty = observed one-cycle yield loss in this sample."
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

## If the "scales" package is unavailable, fall back to plain proportions
## rather than failing the whole script (scales is a ggplot2 dependency and
## should normally be present, but we guard defensively).
if (!requireNamespace("scales", quietly = TRUE)) {
  p_main <- ggplot(fig_df, aes(x = yield_penalty, y = MC_prob_fresh_beats_recycled)) +
    geom_line(linewidth = 1.1, color = "#1b4f72") +
    geom_point(size = 3, color = "#1b4f72") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    scale_x_continuous(breaks = penalty_grid) +
    ylim(0, 1) +
    labs(
      x = "Assumed yield penalty for recycled Bazooka (vs. fresh)",
      y = "P(fresh > recycled profit per acre)",
      title = "Probability that buying fresh seed pays off vs. recycling",
      subtitle = sprintf("Monte Carlo over empirical endline grain sale prices (N = %d, B = %d draws)",
                          length(price_100kg), B)
    ) +
    theme_minimal(base_size = 13)
}

## ---------------------------------------------------------------------------
## 6. Figure 2 (companion panel): empirical price distribution with the
##    breakeven price for each penalty grid point marked as a vertical line.
## ---------------------------------------------------------------------------

price_df <- data.frame(price = price_100kg)
breakeven_df <- results[is.finite(results$breakeven_price_UGX_per_100kg), ]
breakeven_df$label <- penalty_labels[is.finite(results$breakeven_price_UGX_per_100kg)]

p_dist <- ggplot(price_df, aes(x = price)) +
  geom_histogram(bins = 40, fill = "#aed6f1", color = "white") +
  geom_vline(data = breakeven_df,
             aes(xintercept = breakeven_price_UGX_per_100kg, color = label),
             linetype = "dashed", linewidth = 0.9) +
  labs(
    x = "Grain sale price (UGX per 100kg bag)",
    y = "Number of farmer-sellers",
    color = "Breakeven price at\nyield penalty",
    title = "Empirical grain sale price distribution vs. breakeven prices",
    subtitle = "Fresh Bazooka beats recycled Bazooka to the right of each dashed line"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

## Combine main probability plot and price-distribution companion into one
## saved object per format, stacked, since the task asks for "a single
## figure" with an optional companion panel -- we save the probability plot
## as the primary figure and the distribution panel underneath it.
combined <- tryCatch({
  if (requireNamespace("patchwork", quietly = TRUE)) {
    library(patchwork)
    p_main / p_dist
  } else {
    NULL
  }
}, error = function(e) NULL)

out_dir <- "papers/increasing_seed_varietal_turnover/results"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

if (!is.null(combined)) {
  ggsave(file.path(out_dir, "fig2_probability.png"), combined, width = 9.5, height = 11, dpi = 300)
  ggsave(file.path(out_dir, "fig2_probability.pdf"), combined, width = 9.5, height = 11)
} else {
  ## patchwork not available: save the main probability figure as the
  ## primary output (this is the figure explicitly requested by the task),
  ## and save the price-distribution companion as a clearly named sidecar
  ## file so no information is lost.
  ggsave(file.path(out_dir, "fig2_probability.png"), p_main, width = 8, height = 6, dpi = 300)
  ggsave(file.path(out_dir, "fig2_probability.pdf"), p_main, width = 8, height = 6)
  ggsave(file.path(out_dir, "fig2_probability_pricedist.png"), p_dist, width = 8, height = 6, dpi = 300)
  ggsave(file.path(out_dir, "fig2_probability_pricedist.pdf"), p_dist, width = 8, height = 6)
  cat("\n[Note] 'patchwork' package not found: main probability figure and price-\n")
  cat("distribution companion figure saved as separate files (see console for names).\n")
}

cat("\n=== Output files ===\n")
cat(sprintf("Figures written to: %s\n", normalizePath(out_dir, mustWork = FALSE)))
cat("Done.\n")
