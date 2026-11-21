## Who buys fresh Bazooka seed in the cooking-demonstration arm?
## Referee 2 (R2): "Perhaps check who buys the seed in the cooking treatment?
## Are those the wealthier farmers?"
##
## IMPORTANT CAVEAT (stated again at the bottom in the printed banner):
## buying fresh Bazooka seed is a POST-TREATMENT, ENDOGENOUS CHOICE made by
## the farmer after being exposed to the cooking demonstration. Comparing
## buyers vs. non-buyers on baseline characteristics is therefore purely
## DESCRIPTIVE and SELECTION-PRONE -- it tells us who selects into fresh
## purchase conditional on demo exposure, not the causal effect of wealth on
## purchase, and not a causal effect of anything on wealth. Treat all
## p-values/normalized differences below as descriptive correlates, not
## treatment effects.
##
## Run from the repository root:
##   Rscript papers/increasing_seed_varietal_turnover/analysis/fresh_buyers_profile.R

suppressMessages({
  library(ggplot2)
  library(clubSandwich)  # analysis.R lines 1:318 call vcovCR()/coef_test() internally
})

set.seed(20260911)

## ---------------------------------------------------------------------------
## 1. Build `dta` exactly as the paper does, by sourcing analysis.R lines
##    1:318 into a fresh environment -- same pattern used in
##    interaction_equivalence.R (grepped and reused verbatim below). This
##    gives us p_outcome_2 (fresh Bazooka purchase indicator, confirmed by
##    grepping analysis.R: line ~233 assigns it as "purchase" outcome and
##    line ~1982 has the bare comment "dta$p_outcome_2 #purchase"), cont,
##    trial_P, and cluster_ID.
##
##    NOTE on b_cons_exp: analysis.R constructs baseline per-capita
##    consumption expenditure at lines ~900-941, which is AFTER line 318 and
##    also after the point (line 385) where the script calls
##    anderson_sharp_q() from the andersonTools package (needed because
##    `sharp <- TRUE` at the top of analysis.R). That package is not a
##    dependency we want to introduce here, and extending the sourced range
##    past line 385 fails with "could not find function anderson_sharp_q" in
##    this environment. We therefore keep the 1:318 source range (same as
##    interaction_equivalence.R) and reconstruct b_cons_exp independently,
##    below, using the identical recipe from analysis.R lines 900-941 applied
##    directly to baseline.csv.
## ---------------------------------------------------------------------------

ANADIR <- "papers/increasing_seed_varietal_turnover/analysis"

src <- readLines(file.path(ANADIR, "analysis.R"))[1:318]
src <- gsub("cons_intervention_merged2.csv", "cons_intervention_merged.csv", src, fixed = TRUE)
src <- src[!grepl("^\\s*library\\(", src)]
e <- new.env()
e$path <- normalizePath(ANADIR)
suppressWarnings(suppressMessages(
  eval(parse(text = paste(src, collapse = "\n")), envir = e)
))
dta <- e$dta
cat("constructed dta rows (post paid/discounted + consent restriction):", nrow(dta), "\n\n")

## Sanity checks on the fields we depend on.
stopifnot("p_outcome_2" %in% names(dta))
stopifnot("cont" %in% names(dta), "trial_P" %in% names(dta))
stopifnot(is.logical(dta$cont) || all(dta$cont %in% c(TRUE, FALSE, NA)))
stopifnot(is.logical(dta$trial_P) || all(dta$trial_P %in% c(TRUE, FALSE, NA)))

## ---------------------------------------------------------------------------
## 2. Merge in additional baseline wealth/liquidity proxies not carried by the
##    sourced analysis.R construction: total land, rooms, household size,
##    subjective welfare (in_com, in_six), and livestock ownership indicators.
##    Baseline header inspection confirms NO explicit credit/loan variable
##    exists in baseline.csv -- we do not invent one; this gap is flagged
##    explicitly in the printed output below.
## ---------------------------------------------------------------------------

bse_raw <- read.csv("baseline/data/public/baseline.csv", stringsAsFactors = FALSE)
## Apply the same sample restriction analysis.R applies to `bse` (line ~104):
## drop companion-study paid/discounted trial-pack households, so the
## baseline frame used here lines up with the restriction already baked
## into `dta`.
bse_raw <- subset(bse_raw, !(paid_pac == "TRUE" | discounted == "TRUE"))

extra_vars <- c("farmer_ID", "ttl_land", "rooms", "hh_size", "in_com", "in_six",
                "cr_lvst_grp.cr_lvst_ctl", "cr_lvst_grp.cr_lvst_shp",
                "cr_lvst_grp.cr_lvst_goat", "cr_lvst_grp.cr_lvst_pig")
missing_vars <- setdiff(extra_vars, names(bse_raw))
if (length(missing_vars) > 0) {
  stop("Expected baseline columns not found: ", paste(missing_vars, collapse = ", "))
}
bse_extra <- bse_raw[extra_vars]
## endline.csv ALSO has columns named in_com/in_six (asked again at endline),
## and dta already carries those from the sourced construction. Rename the
## baseline versions here so the merge below cannot silently collide into
## in_com.x/in_com.y -- we want the BASELINE (pre-treatment) subjective
## welfare measures as wealth proxies, not the endline (post-treatment) ones.
names(bse_extra)[names(bse_extra) == "in_com"] <- "b_in_com"
names(bse_extra)[names(bse_extra) == "in_six"] <- "b_in_six"

## ---- reconstruct b_cons_exp (baseline per-capita consumption expenditure) --
## Replicates analysis.R lines ~900-941 exactly: sum the value.* consumption
## categories (with 999 recoded to NA), trim 2% (analysis.R's trim() default),
## then divide by baseline household size. Done independently here because
## the original construction lives past line 318 in analysis.R (see note
## above) and pulling it in would require the andersonTools dependency.
value_cols <- c(
  "value.maize_value_sp", "value.sorghum_value_sp", "value.millet_value_sp",
  "value.rice_value_sp", "value.cassava_value_sp", "value.sweetpotatoes_value_sp",
  "value.beans_value_sp", "value.gnuts_value_sp", "value.fruits_value_sp",
  "value.veg_value_sp", "value.sugar_value_sp", "value.cooking_oil_value_sp",
  "value.soap_value_sp", "value.airtime_value_sp"
)
missing_value_cols <- setdiff(value_cols, names(bse_raw))
if (length(missing_value_cols) > 0) {
  stop("Expected consumption value columns not found in baseline.csv: ",
       paste(missing_value_cols, collapse = ", "))
}
cons_mat <- bse_raw[value_cols]
cons_mat[] <- lapply(cons_mat, function(x) { x[x == "999"] <- NA; as.numeric(x) })
cons_exp <- rowSums(cons_mat, na.rm = TRUE)

trim_perc_cons <- 0.02
cons_bounds <- quantile(cons_exp, c(trim_perc_cons / 2, 1 - trim_perc_cons / 2), na.rm = TRUE)
cons_exp[cons_exp < cons_bounds[1] | cons_exp > cons_bounds[2]] <- NA

bse_extra$b_cons_exp <- cons_exp / bse_raw$hh_size

## Livestock fields are stored as character percent-like values with "n/a"
## for households that do not raise that species (confirmed by inspecting
## unique values: e.g. cr_lvst_grp.cr_lvst_ctl takes values like "n/a","0",
## "60","100",...). We convert to a binary OWNERSHIP indicator: owns == TRUE
## if the field is present and numeric > 0, FALSE if "n/a" or "0". This is a
## simplification (the underlying field looks like a share/count, not a
## clean binary), so we treat it as a coarse ownership proxy only.
to_owns <- function(x) {
  x[x == "n/a"] <- NA
  xn <- suppressWarnings(as.numeric(x))
  ifelse(is.na(xn), NA, xn > 0)
}
bse_extra$owns_cattle <- to_owns(bse_extra$cr_lvst_grp.cr_lvst_ctl)
bse_extra$owns_shoats <- to_owns(bse_extra$cr_lvst_grp.cr_lvst_shp)
bse_extra$owns_goats  <- to_owns(bse_extra$cr_lvst_grp.cr_lvst_goat)
bse_extra$owns_pigs   <- to_owns(bse_extra$cr_lvst_grp.cr_lvst_pig)
## simple count of livestock species owned (0-4), a rough asset-diversity proxy
bse_extra$livestock_species_owned <- rowSums(
  bse_extra[c("owns_cattle", "owns_shoats", "owns_goats", "owns_pigs")], na.rm = TRUE
)

n_before_merge <- nrow(dta)
dta <- merge(dta, bse_extra, by.x = "ID", by.y = "farmer_ID", all.x = TRUE)
cat(sprintf("rows before/after merging extra baseline wealth vars: %d -> %d (should be unchanged)\n",
            n_before_merge, nrow(dta)))

## ---------------------------------------------------------------------------
## 3. Restrict to the pure cooking-demonstration arm: cont == TRUE & trial_P
##    == FALSE. This deliberately excludes the free-seed-pack arm so that
##    "buying fresh Bazooka" in this subsample reflects a genuine market
##    purchase decision prompted by demo exposure, not confounded by having
##    already received free seed.
## ---------------------------------------------------------------------------

cat("\n=== Sample restriction funnel (pure cooking-demo arm) ===\n")
cat(sprintf("  dta rows (paper's analysis sample)          : %d\n", nrow(dta)))
demo_arm <- subset(dta, cont == TRUE & trial_P == FALSE)
cat(sprintf("  after restricting to cont==TRUE & trial_P==FALSE : %d  (-%d)\n",
            nrow(demo_arm), nrow(dta) - nrow(demo_arm)))

demo_arm <- subset(demo_arm, !is.na(p_outcome_2))
cat(sprintf("  after dropping missing p_outcome_2 (no_grow etc.): %d\n", nrow(demo_arm)))

n_buyers <- sum(demo_arm$p_outcome_2 == TRUE, na.rm = TRUE)
n_nonbuyers <- sum(demo_arm$p_outcome_2 == FALSE, na.rm = TRUE)
share_buyers <- n_buyers / nrow(demo_arm)

cat(sprintf("\nFresh Bazooka buyers in demo arm   : %d (%.1f%%)\n", n_buyers, 100 * share_buyers))
cat(sprintf("Non-buyers in demo arm              : %d (%.1f%%)\n", n_nonbuyers, 100 * (1 - share_buyers)))

if (n_buyers < 30) {
  cat(sprintf("\n[WARNING] Only %d buyers identified -- comparisons below are likely\n", n_buyers))
  cat("underpowered; treat differences as suggestive at most.\n")
}

## ---------------------------------------------------------------------------
## 4. Compare buyers vs. non-buyers on baseline wealth/liquidity proxies.
##    For each variable: N (non-missing per group), mean per group, raw
##    difference, Welch t-test p-value, and the Imbens-Rubin normalized
##    difference (mean1-mean2)/sqrt((sd1^2+sd2^2)/2), which is more robust to
##    unequal and small group sizes than the t-test p-value alone.
## ---------------------------------------------------------------------------

wealth_vars <- c(
  b_cons_exp               = "Baseline per-capita consumption expenditure (UGX)",
  ttl_land                 = "Baseline total land owned (acres)",
  rooms                    = "Number of rooms in dwelling",
  hh_size                  = "Household size",
  b_in_com                 = "Baseline subjective welfare vs. community (ordinal)",
  b_in_six                 = "Baseline subjective welfare vs. 6 months ago (ordinal)",
  livestock_species_owned  = "Number of livestock species owned (0-4)"
)

normalized_diff <- function(x1, x2) {
  m1 <- mean(x1, na.rm = TRUE); m2 <- mean(x2, na.rm = TRUE)
  s1 <- sd(x1, na.rm = TRUE);   s2 <- sd(x2, na.rm = TRUE)
  (m1 - m2) / sqrt((s1^2 + s2^2) / 2)
}

compare_var <- function(v) {
  x <- demo_arm[[v]]
  buyer_x <- x[demo_arm$p_outcome_2 == TRUE]
  nonbuyer_x <- x[demo_arm$p_outcome_2 == FALSE]

  n_buyer <- sum(!is.na(buyer_x))
  n_nonbuyer <- sum(!is.na(nonbuyer_x))

  if (n_buyer < 2 || n_nonbuyer < 2) {
    return(data.frame(
      variable = v, N_buyers = n_buyer, N_nonbuyers = n_nonbuyer,
      mean_buyers = NA, mean_nonbuyers = NA, diff = NA,
      p_value_welch = NA, normalized_diff = NA
    ))
  }

  tt <- t.test(buyer_x, nonbuyer_x)
  data.frame(
    variable = v,
    N_buyers = n_buyer,
    N_nonbuyers = n_nonbuyer,
    mean_buyers = round(mean(buyer_x, na.rm = TRUE), 3),
    mean_nonbuyers = round(mean(nonbuyer_x, na.rm = TRUE), 3),
    diff = round(mean(buyer_x, na.rm = TRUE) - mean(nonbuyer_x, na.rm = TRUE), 3),
    p_value_welch = round(tt$p.value, 4),
    normalized_diff = round(normalized_diff(buyer_x, nonbuyer_x), 3)
  )
}

profile_table <- do.call(rbind, lapply(names(wealth_vars), compare_var))
profile_table$label <- wealth_vars[profile_table$variable]

cat("\n=== DESCRIPTIVE comparison: fresh Bazooka buyers vs. non-buyers, cooking-demo arm ===\n")
cat("(post-treatment, endogenous split -- see caveat banner at end of script)\n\n")
print(profile_table[, c("variable", "label", "N_buyers", "N_nonbuyers",
                          "mean_buyers", "mean_nonbuyers", "diff",
                          "p_value_welch", "normalized_diff")], row.names = FALSE)

cat("\n[DATA GAP] No explicit credit/loan/access-to-credit variable exists in\n")
cat("baseline.csv (confirmed by full header inspection) -- liquidity is proxied\n")
cat("only indirectly here via consumption expenditure, land, rooms, and\n")
cat("livestock ownership. No credit-access comparison is reported because no\n")
cat("such variable exists; this is a data limitation, not an omission.\n")

## ---------------------------------------------------------------------------
## 5. Visualization: dot/point-range comparison of buyers vs. non-buyers
##    across the standardized (normalized-difference) scale, so variables
##    measured in different units (UGX, acres, counts, ordinal scales) can be
##    shown on one panel.
## ---------------------------------------------------------------------------

plot_df <- profile_table
plot_df <- plot_df[!is.na(plot_df$normalized_diff), ]
plot_df$label <- factor(plot_df$label, levels = plot_df$label[order(plot_df$normalized_diff)])

p <- ggplot(plot_df, aes(x = normalized_diff, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 3, color = "#1b4f72") +
  labs(
    x = "Normalized difference (buyers - non-buyers), Imbens-Rubin scale",
    y = NULL,
    title = paste(strwrap("Baseline wealth/liquidity proxies: fresh Bazooka buyers vs. non-buyers",
                          width = 60), collapse = "\n"),
    subtitle = paste(strwrap(sprintf(
      "Cooking-demonstration arm only (cont==TRUE & trial_P==FALSE); N buyers = %d, N non-buyers = %d",
      n_buyers, n_nonbuyers), width = 75), collapse = "\n"),
    caption = paste(strwrap(
      "DESCRIPTIVE / SELECTION-PRONE: purchase is a post-treatment, endogenous choice -- not a causal comparison.",
      width = 90), collapse = "\n")
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot")

out_dir <- "papers/increasing_seed_varietal_turnover/results"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ggsave(file.path(out_dir, "fresh_buyers_profile.png"), p, width = 10.5, height = 6, dpi = 300)
ggsave(file.path(out_dir, "fresh_buyers_profile.pdf"), p, width = 10.5, height = 6)

cat(sprintf("\nFigures written to: %s/fresh_buyers_profile.png and .pdf\n", normalizePath(out_dir, mustWork = FALSE)))

## ---------------------------------------------------------------------------
## 6. Final banner: restate the identification caveat explicitly, and state
##    the finding in whichever direction the data actually show (not forced
##    toward "wealthier farmers buy fresh").
## ---------------------------------------------------------------------------

cat("\n==================== SUMMARY (for response-letter text) ====================\n")
cat("CAVEAT: buying fresh Bazooka in the demo arm is a post-treatment, endogenous\n")
cat("choice. All comparisons below describe who SELECTS INTO fresh purchase after\n")
cat("demo exposure; they are not causal estimates of a wealth effect on purchase.\n\n")
sig_rows <- profile_table[!is.na(profile_table$p_value_welch) & profile_table$p_value_welch < 0.10, ]
if (nrow(sig_rows) == 0) {
  cat("No baseline wealth/liquidity proxy differs between buyers and non-buyers\n")
  cat("at even the 10% level in this sample -- the data do not support a simple\n")
  cat("'wealthier farmers buy fresh' story, at least on the proxies available.\n")
} else {
  cat("Variables differing between buyers and non-buyers (p < 0.10, Welch t-test):\n")
  print(sig_rows[, c("variable", "diff", "p_value_welch", "normalized_diff")], row.names = FALSE)
}
cat("==============================================================================\n")
cat("Done.\n")
