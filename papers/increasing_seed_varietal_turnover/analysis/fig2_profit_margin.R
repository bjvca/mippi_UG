## Exploratory alternative to fig2_probability.R: keep the original
## profit-vs-price, three-line format (profits.R), attach a marginal
## density of the REAL empirical grain sale price distribution (via ggside),
## and now also overlay actual per-farmer (price, profit) points by seed
## source, so farmer-to-farmer yield variability is shown directly from data
## rather than via an assumed ribbon.
##
## Not wired into the manuscript. For side-by-side comparison only.
## Run from the repository root.

suppressMessages({
  library(ggplot2)
  library(ggside)
  library(dplyr)
  library(clubSandwich)
  library(moments)
  library(andersonTools)
})

ANADIR <- "papers/increasing_seed_varietal_turnover/analysis"

## ---- 1. Reconstruct dta exactly as in analysis.R, through the point where ----
## productivity_mean (raw kg/acre on the randomly selected plot, pre-log) and
## the seed-source classification variables for that plot are available.
src <- readLines(file.path(ANADIR, "analysis.R"))[1:676]
src <- gsub("cons_intervention_merged2.csv", "cons_intervention_merged.csv", src, fixed = TRUE)
src <- src[!grepl("^\\s*library\\(", src)]
e <- new.env()
e$path <- normalizePath(ANADIR)
suppressWarnings(suppressMessages(
  eval(parse(text = paste(src, collapse = "\n")), envir = e)
))
dta <- e$dta

## ---- 2. Per-farmer price (UGX/100kg), trimmed as in fig2_probability.R ----
## NOTE: analysis.R's merge with the baseline frame (bse) leaves dta$bag_charge
## pointing at the BASELINE value (renamed from bag_charge.y); the endline
## value survives as bag_charge.x. We need the endline price here, to match
## the endline-measured productivity_mean used below.
bag_charge <- dta$bag_charge.x
bag_charge[bag_charge %in% c("n/a", "999")] <- NA
bag_charge <- as.numeric(bag_charge)

bag_kg <- dta$bag_kg
bag_kg[bag_kg %in% c("n/a", "999")] <- NA
bag_kg <- as.numeric(bag_kg)

dta$price_100kg <- bag_charge / bag_kg * 100
trim_bounds <- quantile(dta$price_100kg, c(0.01, 0.99), na.rm = TRUE)
dta$price_100kg[dta$price_100kg < trim_bounds[1] | dta$price_100kg > trim_bounds[2]] <- NA

price_100kg_all <- dta$price_100kg[!is.na(dta$price_100kg)]
cat(sprintf("N prices used (density): %d | median %.0f\n", length(price_100kg_all), median(price_100kg_all)))

## ---- 3. Per-farmer seed-source category on the randomly selected plot ----
## fresh: Bazooka, first use (times_recycled_selected == 1)
## recycled: Bazooka, replanted from a prior harvest (times_recycled_selected 2-5)
## local: a local/landrace variety (maize_var_selected == "Local")
seed_cost_fresh <- 8 * 12000  # UGX/acre, as in profits.R

dta$Model <- NA_character_
dta$Model[dta$maize_var_selected == "Bazooka" & dta$times_recycled_selected %in% "1"] <- "Fresh Bazooka"
dta$Model[dta$maize_var_selected == "Bazooka" & dta$times_recycled_selected %in% c("2","3","4","5")] <- "Recycled Bazooka"
dta$Model[dta$maize_var_selected == "Local"] <- "Local Seed"

dta$Profit <- dta$productivity_mean / 100 * dta$price_100kg
is_fresh <- dta$Model %in% "Fresh Bazooka"
dta$Profit[is_fresh] <- dta$Profit[is_fresh] - seed_cost_fresh

points_df <- dta[!is.na(dta$Model) & !is.na(dta$price_100kg) & !is.na(dta$Profit),
                  c("Model", "price_100kg", "Profit")]
names(points_df) <- c("Model", "Price", "Profit")

cat("N farmer points plotted, by group:\n")
print(table(points_df$Model))

## ---- 4. Original fixed-yield model lines (profits.R accounting) ----
price_grid <- seq(min(price_100kg_all), max(price_100kg_all), length.out = 200)

fresh_yield_kg    <- 5.35   # cwt/acre = 535 kg/acre, as in profits.R's "5.3" slope
local_yield_kg    <- 3.23
recycled_yield_kg <- 3.75

line_df <- data.frame(
  Price  = rep(price_grid, 3),
  Profit = c(fresh_yield_kg * price_grid - seed_cost_fresh,
             local_yield_kg * price_grid,
             recycled_yield_kg * price_grid),
  Model  = rep(c("Fresh Bazooka", "Local Seed", "Recycled Bazooka"), each = length(price_grid))
)

## Colors/shapes for the point cloud (lines reuse the same palette)
pcols  <- c("Fresh Bazooka" = "#0072B2", "Recycled Bazooka" = "#E69F00", "Local Seed" = "#009E73")
## Fillable shapes (21/22/24 = circle/square/triangle, outline + fill) so both
## color and shape distinguish groups under heavy overplotting.
pshape <- c("Fresh Bazooka" = 21,        "Recycled Bazooka" = 24,        "Local Seed" = 22)

## ---- 5. Single combined plot via ggside: model lines + real farmer points ----
## Display range capped to [30,000, 95,000] (xlim, not a data filter) to crop
## the sparse low tail and the cluster of exact-100,000-UGX reports out of view.
## Profit axis also capped to keep the real (noisier) points readable, since a
## few farmer-level points have much more extreme profit values than the
## fixed-yield model lines ever reach.
x_display_min <- 30000
x_display_max <- 95000
y_display_min <- 0
y_display_max <- 450000

set.seed(20261020)  # reproducible jitter

p <- ggplot(points_df, aes(x = Price, y = Profit, shape = Model, fill = Model, color = Model)) +
  geom_point(color = "black", stroke = 0.2, alpha = 0.6, size = 2.2,
             position = position_jitter(width = 1800, height = 0)) +
  geom_line(data = line_df, linewidth = 1.2) +
  geom_xsidedensity(alpha = 0.35, linewidth = 0.9, adjust = 2) +
  scale_xsidey_continuous(name = "Density (x1e-5)",
                           labels = function(x) x * 1e5) +
  scale_fill_manual(values = pcols) +
  scale_color_manual(values = pcols) +
  scale_shape_manual(values = pshape) +
  scale_x_continuous(limits = c(x_display_min, x_display_max)) +
  scale_y_continuous(labels = function(x) x / 1000) +
  coord_cartesian(ylim = c(y_display_min, y_display_max)) +
  labs(x = "Grain sale price (UGX per 100kg)",
       y = "Profit (UGX thousands/acre)",
       fill = "Farmer type", shape = "Farmer type", color = "Farmer type") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        ggside.panel.scale = 0.3)

ggsave("papers/increasing_seed_varietal_turnover/results/fig2_profit_margin.png",
       p, width = 8, height = 7, dpi = 300)
cat("Saved: papers/increasing_seed_varietal_turnover/results/fig2_profit_margin.png\n")
