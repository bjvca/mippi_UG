## R2-5 (referee report, Food Policy R&R): "random plot" complications.
##
## Referee asks three things:
##   (a) the full distribution of the number of maize plots per household,
##   (b) whether random-plot adoption estimates differ from aggregate
##       (any-plot / household-level) adoption estimates -- if not, the
##       random-plot elicitation is an unnecessary complication,
##   (c) an explanation of why random-plot elicitation was used at all, and
##       the dilution/conditionality point: a randomly selected plot without
##       Bazooka on it cannot show the yield/learning effect even if the
##       household grows Bazooka elsewhere.
##
## To guarantee identical variable construction to the paper, we source the
## paper's own analysis.R up to the point where both the household-level
## adoption outcomes (p_outcome_2alt, p_outcome_2, ..., line ~318) and the
## random-plot outcomes (rnd_bazo_alt, rnd_bazo, line ~626) are built. This
## requires sourcing further than heterogeneity.R / interaction_equivalence.R
## (which stop at line 318), because the random-plot variables are
## constructed later in the script. We stop at line 630, just after the
## rnd_bazo/rnd_bazo_alt merges and before unrelated seed-quantity code.
##
## Run from the repository root (mippi_UG/).

suppressMessages({
  library(clubSandwich)
  library(dplyr)
  library(andersonTools)   # needed by the sourced sharpened-q-value block
})

ANADIR <- "papers/increasing_seed_varietal_turnover/analysis"
STOP_LINE <- 630

## ---- build dta exactly as in the paper (sourced construction) -------------
src <- readLines(file.path(ANADIR, "analysis.R"))[1:STOP_LINE]
src <- gsub("cons_intervention_merged2.csv", "cons_intervention_merged.csv", src, fixed = TRUE)
src <- src[!grepl("^\\s*library\\(", src)]   # we already loaded what's needed above
e <- new.env()
e$path <- normalizePath(ANADIR)              # analysis.R strips this suffix to get repo root
suppressWarnings(suppressMessages(
  eval(parse(text = paste(src, collapse = "\n")), envir = e)
))
dta <- e$dta
cat("constructed dta rows:", nrow(dta), "\n\n")

## =============================================================================
## (a) Full distribution of the number of maize plots per household, endline
## =============================================================================
## plot_count is a string field ("0","1",...,"n/a"); "n/a" arises when the
## household did not complete the plot roster (treated as missing here, as
## in the paper's own no_grow construction, which uses plot_count == "0").
cat("=========== (a) Distribution of number of maize plots per household (endline) ===========\n")
pc <- dta$plot_count
pc_num <- suppressWarnings(as.numeric(pc))
tab <- table(pc_num, useNA = "ifany")
print(tab)
cat("\nAs share of non-missing households:\n")
print(round(prop.table(tab), 3))
cat(sprintf("\nMean number of plots (non-missing): %.2f\n", mean(pc_num, na.rm = TRUE)))
cat(sprintf("Median: %.0f | Max: %.0f | N non-missing: %d | N missing/'n/a': %d\n",
            median(pc_num, na.rm = TRUE), max(pc_num, na.rm = TRUE),
            sum(!is.na(pc_num)), sum(is.na(pc_num))))
cat(sprintf("Share with exactly 1 plot: %.3f | Share with >1 plot: %.3f\n",
            mean(pc_num == 1, na.rm = TRUE), mean(pc_num > 1, na.rm = TRUE)))

## Same distribution, by trial-pack status (does the pack itself change how
## many maize plots households cultivate? relevant since trial-pack recipients
## by definition farm >=1 plot with the 1kg pack, on top of their usual plots)
cat("\n--- by trial-pack assignment ---\n")
for (tp in c(FALSE, TRUE)) {
  sub <- pc_num[dta$trial_P == tp]
  cat(sprintf("trial_P = %s : mean = %.2f, median = %.0f, N = %d\n",
              tp, mean(sub, na.rm = TRUE), median(sub, na.rm = TRUE), sum(!is.na(sub))))
}

## =============================================================================
## (b) Random-plot vs household-level (any-plot) adoption estimates
## =============================================================================
## Household-level outcomes (Table 3): p_outcome_2alt = used Bazooka (fresh or
## recycled) on ANY maize plot; p_outcome_2 = used FRESH Bazooka from a
## trusted source on any plot.
## Random-plot outcomes (Table A12/A3): rnd_bazo_alt = grew Bazooka on the
## single randomly selected plot; rnd_bazo = fresh Bazooka on that plot.
cat("\n\n=========== (b) Household-level vs random-plot adoption estimates ===========\n")

run_itt <- function(y, data = dta) {
  dd <- data[!is.na(data[[y]]) & !is.na(data$cluster_ID), ]
  m  <- lm(as.formula(paste(y, "~ trial_P*cont")), data = dd)
  vc <- vcovCR(m, cluster = dd$cluster_ID, type = "CR2")
  ct <- as.data.frame(coef_test(m, vc))
  cm <- mean(dd[[y]][dd$trial_P == FALSE & dd$cont == FALSE], na.rm = TRUE)
  list(control_mean = cm, N = nrow(dd), coef = ct)
}

pairs <- list(
  "any Bazooka"   = c(household = "p_outcome_2alt", random_plot = "rnd_bazo_alt"),
  "fresh Bazooka" = c(household = "p_outcome_2",    random_plot = "rnd_bazo")
)

results <- list()
for (nm in names(pairs)) {
  cat(sprintf("\n--- %s ---\n", nm))
  hh  <- run_itt(pairs[[nm]]["household"])
  rp  <- run_itt(pairs[[nm]]["random_plot"])
  results[[nm]] <- list(household = hh, random_plot = rp)

  cat(sprintf("household-level  (%-14s): control mean = %.3f | trial-pack ITT = %6.3f (SE %.3f, p=%.3f) | N=%d\n",
              pairs[[nm]]["household"], hh$control_mean,
              hh$coef["trial_PTRUE", "beta"], hh$coef["trial_PTRUE", "SE"], hh$coef["trial_PTRUE", "p_Satt"], hh$N))
  cat(sprintf("random-plot      (%-14s): control mean = %.3f | trial-pack ITT = %6.3f (SE %.3f, p=%.3f) | N=%d\n",
              pairs[[nm]]["random_plot"], rp$control_mean,
              rp$coef["trial_PTRUE", "beta"], rp$coef["trial_PTRUE", "SE"], rp$coef["trial_PTRUE", "p_Satt"], rp$N))
  cat(sprintf("ratio random-plot ITT / household ITT = %.3f  (1.0 = no dilution from using a random plot)\n",
              rp$coef["trial_PTRUE", "beta"] / hh$coef["trial_PTRUE", "beta"]))
  cat(sprintf("cooking-demo ITT: household = %6.3f (p=%.3f) | random-plot = %6.3f (p=%.3f)\n",
              hh$coef["contTRUE", "beta"], hh$coef["contTRUE", "p_Satt"],
              rp$coef["contTRUE", "beta"], rp$coef["contTRUE", "p_Satt"]))
}

## Mechanical dilution check: under a naive model where (i) each household's
## plots are drawn i.i.d. with Bazooka grown on exactly one plot when the
## household is an adopter, and (ii) the random plot is drawn uniformly from
## the household's plot_count, the random-plot estimate of the *effect on the
## probability the sampled plot is Bazooka* would equal the household-level
## effect divided by the average number of plots among adopting households.
## We do not impose this model; we simply report the ingredients so the
## arithmetic is transparent for the response letter.
cat("\n--- dilution arithmetic (descriptive) ---\n")
adopters <- dta$trial_P == TRUE & dta$p_outcome_2alt %in% TRUE
cat(sprintf("Mean number of maize plots among trial-pack households that grew Bazooka: %.2f (N=%d)\n",
            mean(pc_num[adopters], na.rm = TRUE), sum(adopters, na.rm = TRUE)))
cat(sprintf("=> naive 1/plots dilution factor implied: %.2f\n",
            1 / mean(pc_num[adopters], na.rm = TRUE)))
cat("Observed ratio (random-plot ITT / household ITT, computed above) can be compared\n")
cat("against this naive 1/plots benchmark: a ratio close to it is consistent with Bazooka\n")
cat("being grown on close to one plot per adopting household and roughly evenly likely to\n")
cat("be the plot drawn; a ratio much larger than 1/plots (i.e., closer to 1) indicates\n")
cat("Bazooka is grown on a large share of the household's plots, not just the trial-pack plot.\n")

cat("\n--- share of maize area planted to Bazooka, trial-pack group (context for (c)) ---\n")
cat(sprintf("share_area_imp (trial-pack group, non-missing): mean = %.3f, N = %d\n",
            mean(dta$share_area_imp[dta$trial_P == TRUE], na.rm = TRUE),
            sum(!is.na(dta$share_area_imp[dta$trial_P == TRUE]))))
cat(sprintf("share_plots_imp (trial-pack group, non-missing): mean = %.3f, N = %d\n",
            mean(dta$share_plots_imp[dta$trial_P == TRUE], na.rm = TRUE),
            sum(!is.na(dta$share_plots_imp[dta$trial_P == TRUE]))))

## =============================================================================
## (c) Why random-plot elicitation, and the yield-conditionality caveat
## =============================================================================
## This is prose for the response letter / manuscript, not a computation:
##
## Why a random plot: detailed plot-level questions (seed source, recycling
## history, input use, yield) were collected for a single randomly selected
## plot per household rather than for every plot, to limit respondent burden
## (footnote 10 in the submitted manuscript: "detailed plot-level questions
## ... were collected for a single, randomly selected plot per household to
## limit respondent burden"). This is a standard measurement-cost trade-off in
## plot-level agricultural surveys: yield and input recall are far more
## accurate and less burdensome per plot than a full household-level roster
## of yields across every plot, especially for households cultivating several
## small, non-contiguous plots. Because the plot is drawn by the same random
## procedure independent of treatment assignment, the resulting ITT estimand
## is well defined (an unbiased estimate of the effect on a randomly drawn
## plot), but -- as the referee notes -- it is a *diluted* proxy for the
## effect on the specific plot that received the trial-pack seed whenever a
## household farms more than one maize plot.
##
## The yield/learning conditionality point: yield and productivity outcomes
## are only informative about the trial pack's production-learning channel
## when the sampled plot actually carries Bazooka. If the random draw lands
## on a plot without Bazooka -- which happens whenever the household grows
## Bazooka on only a subset of its plots -- the yield outcome on that plot
## reflects local/other-variety yields, not the treatment's effect on Bazooka
## yields, and is uninformative in this respect (a form of the same dilution
## as in (a)/(b), but with no adoption signal to offset it since yield is
## continuous, not a plot-selection indicator). The paper's own within-adopter
## comparisons of fresh/recycled/local yields (Section 6.1) sidestep this by
## conditioning on the plot actually carrying Bazooka -- which the text
## should flag explicitly as a descriptive, non-random subsample.
cat("\n(c) See in-file comments above for the prose explanation to adapt for the manuscript;\n")
cat("    key quantitative inputs for that prose are the ratios and shares printed above.\n")
