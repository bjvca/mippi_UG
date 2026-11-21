## Robustness of the null treatment interaction to baseline imbalance in prior
## Bazooka use (R2, major comment 6; linked to R1-3).
##
## R2: prior Bazooka use is lower in the interaction (both-treatment) cell.
## Question: does the null estimate of the interaction term (beta_I) on the
## three primary outcomes survive (a) covariate adjustment for the imbalanced
## baseline variable, and (b) letting the treatment effects vary flexibly by
## baseline prior-use status (i.e. not just adjusting linearly but allowing
## effect modification, which is the standard test for whether an imbalanced
## covariate is driving the interaction estimate)?
##
## Baseline variable: b_p_outcome_2 (household reported using Bazooka on at
## least one plot in the season prior to baseline; bse$bazo_use=="Yes").
## b_p_outcome_2alt is constructed identically (same source variable) so the
## two are interchangeable; we use b_p_outcome_2 throughout for consistency
## with the outcome-specific baseline control already used for freshBazooka
## in the paper's own saturated spec.
##
## Three specifications per outcome:
##   (A) Paper's spec: y ~ trial_P*cont [+ own-outcome baseline control if the
##       paper already includes one, i.e. anyBazooka/freshBazooka; index has
##       none in the submitted spec]
##   (B) + baseline prior-Bazooka-use control added (linear adjustment) to ALL
##       three outcomes, including index
##   (C) Fully saturated by prior-use group: y ~ trial_P*cont*b_p_outcome_2,
##       reporting beta_I separately for the no-prior-use and prior-use strata
##       and citing the paper's own reported average (a weighted combination)
##       -- this directly tests whether the imbalance (fewer prior users in
##       the both-treatment cell) is masking a heterogeneous interaction.
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

## ---- Step 1: quantify the imbalance ---------------------------------------
cat("================ STEP 1: baseline imbalance in prior Bazooka use ================\n")
dd0 <- dta[!is.na(dta$b_p_outcome_2), ]
cat("cell means, b_p_outcome_2 (prior Bazooka use):\n")
print(round(tapply(dd0$b_p_outcome_2, list(dd0$trial_P, dd0$cont), mean, na.rm = TRUE), 3))

m_bal <- lm(b_p_outcome_2 ~ trial_P * cont, data = dd0)
vc_bal <- vcovCR(m_bal, cluster = dd0$cluster_ID, type = "CR2")
ct_bal <- as.data.frame(coef_test(m_bal, vc_bal))
print(round(ct_bal[, c("beta", "SE", "df_Satt", "p_Satt")], 4))
cat("Interaction (both-treatment) cell prior-use rate vs the other three cells,\n",
    "beta on trial_PTRUE:contTRUE is the imbalance of interest.\n\n", sep = "")

## ---- Step 2: outcome models -----------------------------------------------
outcomes <- c(index = "index", anyBazooka = "p_outcome_2alt", freshBazooka = "p_outcome_2")
## paper's own-outcome baseline control, where the submitted spec includes one
paper_bctrl <- c(index = NA, anyBazooka = "b_p_outcome_2alt", freshBazooka = "b_p_outcome_2")

run_model <- function(dd, rhs, y) {
  m  <- lm(as.formula(paste(y, "~", rhs)), data = dd)
  vc <- vcovCR(m, cluster = dd$cluster_ID, type = "CR2")
  ct <- as.data.frame(coef_test(m, vc))
  ct
}

res_table <- data.frame()
strata_table <- data.frame()

for (nm in names(outcomes)) {
  y  <- outcomes[nm]
  bc <- paper_bctrl[nm]
  dd <- dta[!is.na(dta[[y]]) & !is.na(dta$b_p_outcome_2), ]

  ## (A) paper's spec, as submitted
  rhs_A <- "trial_P*cont"
  if (!is.na(bc)) rhs_A <- paste(rhs_A, bc, sep = " + ")
  ctA <- run_model(dd, rhs_A, y)

  ## (B) + baseline prior-Bazooka-use control (added even where the paper
  ##     already controls for the outcome-specific baseline, since for
  ##     anyBazooka/freshBazooka that IS b_p_outcome_2 already -- so (B) only
  ##     changes the index row in practice; kept identical spec elsewhere to
  ##     show it is a no-op there)
  rhs_B <- "trial_P*cont + b_p_outcome_2"
  ctB <- run_model(dd, rhs_B, y)

  ## (C) fully saturated by prior-use group
  rhs_C <- "trial_P*cont*b_p_outcome_2"
  ctC <- run_model(dd, rhs_C, y)

  cf <- "trial_PTRUE:contTRUE"
  grab <- function(ct, label) {
    if (!(cf %in% rownames(ct))) return(NULL)
    data.frame(outcome = nm, spec = label,
               beta = ct[cf, "beta"], se = ct[cf, "SE"],
               df = ct[cf, "df_Satt"], p = ct[cf, "p_Satt"], N = nrow(dd))
  }
  res_table <- rbind(res_table, grab(ctA, "A: paper spec"))
  res_table <- rbind(res_table, grab(ctB, "B: + prior-use control"))
  res_table <- rbind(res_table, grab(ctC, "C: saturated (avg./no-prior-use row)"))

  ## strata-specific interaction under (C): beta_I for prior-use = 0 is the
  ## trial_PTRUE:contTRUE coefficient itself (reference group); for
  ## prior-use = 1 it is that coefficient plus the triple interaction
  cf3 <- "trial_PTRUE:contTRUE:b_p_outcome_2TRUE"
  b0 <- ctC[cf, "beta"]; se0 <- ctC[cf, "SE"]
  if (cf3 %in% rownames(ctC)) {
    b3 <- ctC[cf3, "beta"]; se3 <- ctC[cf3, "SE"]
    b1 <- b0 + b3
    ## approximate SE for the sum via the coefficient covariance (from vcovCR)
    vc <- vcovCR(lm(as.formula(paste(y, "~", rhs_C)), data = dd),
                 cluster = dd$cluster_ID, type = "CR2")
    v0 <- vc[cf, cf]; v3 <- vc[cf3, cf3]; cov03 <- vc[cf, cf3]
    se1 <- sqrt(v0 + v3 + 2 * cov03)
    p1  <- 2 * pt(-abs(b1 / se1), ctC[cf3, "df_Satt"])
    strata_table <- rbind(strata_table, data.frame(
      outcome = nm,
      beta_I_no_prior_use = b0, se_no_prior_use = se0,
      beta_I_prior_use = b1, se_prior_use = se1, p_prior_use = p1,
      beta_triple = b3, se_triple = se3, p_triple = ctC[cf3, "p_Satt"]))
  }
}

cat("================ STEP 2: interaction (beta_I) across specifications ================\n")
res_table$beta <- round(res_table$beta, 4)
res_table$se   <- round(res_table$se, 4)
res_table$df   <- round(res_table$df, 1)
res_table$p    <- round(res_table$p, 4)
print(res_table, row.names = FALSE)

cat("\n================ STEP 3: interaction by prior-use stratum (spec C) ================\n")
print(round(strata_table[, -1], 4))
cat("\n(outcome column: ", paste(strata_table$outcome, collapse = ", "), ")\n", sep = "")

cat("\nInterpretation: if beta_I in (B) is essentially unchanged from (A), linear\n",
    "adjustment for the imbalanced covariate does not move the null result. If\n",
    "beta_I in the no-prior-use and prior-use strata under (C) are both close to\n",
    "zero (and not significantly different from each other via the triple\n",
    "interaction), the null interaction is not an artifact of the imbalance --\n",
    "it holds within each prior-use group separately, not just on average.\n", sep = "")
