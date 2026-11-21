## Equivalence analysis for the treatment interaction (R1, major comment 3).
## R1: "report confidence intervals, formal tests of economically meaningful
## interaction effects, and ideally equivalence tests before concluding that
## policymakers should use only one intervention."
##
## Outcomes: adoption index, used any Bazooka, used fresh Bazooka.
## Spec: the paper's saturated model y ~ trial_P*cont (+ baseline outcome),
##       CR2 SEs clustered at village, Satterthwaite df (identical to Table 3).
## For each outcome we report:
##   (1) beta_I with 95% and 90% CIs (Satterthwaite df)
##   (2) one-sided test of H0: beta_I >= +delta (can we reject a meaningful
##       positive synergy?) and Wald test of beta_I = +delta
##   (3) TOST equivalence test against +/- delta (reject both one-sided H0s
##       at 5% <=> 90% CI inside [-delta, +delta])
##   (4) empirical equivalence bound delta* = smallest delta at which TOST
##       rejects = max(|90% CI bounds|)
##   (5) MDE for the interaction at 80% power
## Plus the cost-effectiveness-implied delta: the interaction needed for the
## bundled program to match the trial pack's cost per turnover adopter.
##
## Equivalence margins (pre-specified in the response letter):
##   any Bazooka : delta = 0.10 (> demo's entire standalone effect ~0.07;
##                 ~15% of the pack's main effect)
##   fresh Bazooka: delta = 0.05 (= demo's own standalone effect)
##   index       : delta = 0.20 control-group SDs (conventional small effect)
##
## Run from the repository root.

suppressMessages({library(clubSandwich)})

ANADIR <- "papers/increasing_seed_varietal_turnover/analysis"

## ---- build dta exactly as in the paper (sourced construction) -------------
src <- readLines(file.path(ANADIR, "analysis.R"))[1:318]
src <- gsub("cons_intervention_merged2.csv", "cons_intervention_merged.csv", src, fixed = TRUE)
src <- src[!grepl("^\\s*library\\(", src)]
e <- new.env()
e$path <- normalizePath(ANADIR)
suppressWarnings(suppressMessages(
  eval(parse(text = paste(src, collapse = "\n")), envir = e)
))
dta <- e$dta
cat("constructed dta rows:", nrow(dta), "\n\n")

outcomes <- c(index = "index", anyBazooka = "p_outcome_2alt", freshBazooka = "p_outcome_2")
bctrl    <- c(index = NA,      anyBazooka = "b_p_outcome_2alt", freshBazooka = "b_p_outcome_2")
deltas   <- c(index = 0.20,    anyBazooka = 0.10,              freshBazooka = 0.05)

res <- list()
for (nm in names(outcomes)) {
  y <- outcomes[nm]; bc <- bctrl[nm]; delta <- deltas[nm]
  rhs <- "trial_P*cont"; if (!is.na(bc)) rhs <- paste(rhs, bc, sep = " + ")
  dd <- dta[!is.na(dta[[y]]), ]
  m  <- lm(as.formula(paste(y, "~", rhs)), data = dd)
  vc <- vcovCR(m, cluster = dd$cluster_ID, type = "CR2")
  ct <- as.data.frame(coef_test(m, vc))
  cf <- "trial_PTRUE:contTRUE"
  b  <- ct[cf, "beta"]; se <- ct[cf, "SE"]; df <- ct[cf, "df_Satt"]
  p2 <- ct[cf, "p_Satt"]

  ci95 <- b + c(-1, 1) * qt(0.975, df) * se
  ci90 <- b + c(-1, 1) * qt(0.950, df) * se

  ## one-sided: reject H0: beta_I >= +delta ?
  p_meaningful <- pt((b - delta) / se, df)              # small => meaningful synergy ruled out
  ## Wald, two-sided, H0: beta_I = +delta
  p_wald_delta <- 2 * pt(-abs((b - delta) / se), df)
  ## TOST: H0a beta <= -delta (t1) ; H0b beta >= +delta (t2)
  p_tost <- max(1 - pt((b + delta) / se, df), pt((b - delta) / se, df))
  dstar  <- max(abs(ci90))                              # empirical equivalence bound
  mde    <- (qt(0.975, df) + qt(0.80, df)) * se         # 80% power, 5% two-sided

  res[[nm]] <- c(beta = b, se = se, df = df, p = p2,
                 ci95_lo = ci95[1], ci95_hi = ci95[2],
                 ci90_lo = ci90[1], ci90_hi = ci90[2],
                 delta = delta, p_onesided_ge_delta = p_meaningful,
                 p_wald_at_delta = p_wald_delta, p_TOST = p_tost,
                 dstar = dstar, MDE = mde, N = nobs(m))

  cat(sprintf("== %s (delta = %.2f) ==\n", nm, delta))
  cat(sprintf("  beta_I = %6.3f (SE %.3f, df %.1f, p = %.3f), N = %d\n", b, se, df, p2, nobs(m)))
  cat(sprintf("  95%% CI [%6.3f, %6.3f] | 90%% CI [%6.3f, %6.3f]\n", ci95[1], ci95[2], ci90[1], ci90[2]))
  cat(sprintf("  H0: beta_I >= +delta, one-sided p = %.3f %s\n", p_meaningful,
              ifelse(p_meaningful < 0.05, "-> meaningful synergy REJECTED", "-> cannot reject")))
  cat(sprintf("  Wald H0: beta_I = +delta, p = %.3f\n", p_wald_delta))
  cat(sprintf("  TOST p = %.3f %s\n", p_tost,
              ifelse(p_tost < 0.05, "-> EQUIVALENT within +/-delta", "-> equivalence NOT established")))
  cat(sprintf("  empirical equivalence bound delta* = %.3f | interaction MDE (80%%) = %.3f\n\n", dstar, mde))
}

## ---- CE-implied delta ------------------------------------------------------
## Bundling beats spending the same budget on more packs iff the add-on
## produces adopters at least as cheaply: (beta_D + beta_I)/c_demo >= beta_S/c_pack
ce <- new.env()
suppressWarnings(suppressMessages(
  try(source(file.path(ANADIR, "cost_effectiveness.R"), local = ce), silent = TRUE)
))
if (!is.null(ce$pack_pf_realized) && !is.null(ce$demo_pf_realized_scalable)) {
  ct3 <- res[["anyBazooka"]]
  ## main effects from the same saturated model
  y <- "p_outcome_2alt"; dd <- dta[!is.na(dta[[y]]), ]
  m  <- lm(p_outcome_2alt ~ trial_P*cont + b_p_outcome_2alt, data = dd)
  vc <- vcovCR(m, cluster = dd$cluster_ID, type = "CR2")
  cs <- as.data.frame(coef_test(m, vc))
  bS <- cs["trial_PTRUE", "beta"]; bD <- cs["contTRUE", "beta"]
  cP <- ce$pack_pf_realized; cD <- ce$demo_pf_realized_scalable
  delta_ce_demo_on_pack <- cD * bS / cP - bD   # interaction needed to justify demo add-on
  delta_ce_pack_on_demo <- cP * bS / cP - bS   # trivially 0 by construction; report proper form
  cat("---- cost-effectiveness-implied delta ----\n")
  cat(sprintf("  per-farmer cost: pack = %.0f, demo (scalable) = %.0f UGX\n", cP, cD))
  cat(sprintf("  saturated main effects (any Bazooka): beta_S = %.3f, beta_D = %.3f\n", bS, bD))
  cat(sprintf("  demo add-on justified iff beta_D + beta_I >= c_demo * beta_S / c_pack = %.3f\n",
              cD * bS / cP))
  cat(sprintf("  => CE-implied delta (interaction needed) = %.3f  %s\n",
              delta_ce_demo_on_pack,
              ifelse(delta_ce_demo_on_pack > 1, "(> 1: infeasible for a probability)", "")))
} else {
  cat("cost_effectiveness.R objects not available; skipped CE-implied delta\n")
}
