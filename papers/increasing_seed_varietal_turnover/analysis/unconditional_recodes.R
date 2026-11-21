## Unconditional recodes of familiarity-conditioned Bazooka-specific outcomes
## (R1, major comment 2 / R1-2 in FP/reviews/response_to_reviewers.md).
##
## R1's objection: two of the outcomes in the "Mechanisms" section / Table 4
## ("Recommended Bazooka to others", "Thinks Bazooka is risky") are, by survey
## design, only asked of respondents who report being familiar with Bazooka
## (variable knw_bazo in analysis.R). Because the seed trial pack itself raises
## Bazooka familiarity by ~29-30pp (Table 4, row "Knows Bazooka"), conditioning
## the estimation sample on knw_bazo==TRUE conditions on a post-treatment
## variable and breaks the randomization: the "familiar" subsample is no longer
## comparable across arms.
##
## Diagnosis (see scratch diagnostics, reproduced in the checks below):
##   - knw_bazo:            unconditional, N = 1538, no missingness from the skip pattern.
##   - share_bazo    ("Recommended Bazooka to others"): missing <=> knw_bazo==FALSE,
##                    exactly (all 278 unfamiliar respondents are NA; no other missingness).
##                    Original paper sample: N = 1260.
##   - downside_risk_bazo ("Thinks Bazooka is risky"): missing = 278 (unfamiliar, skip
##                    pattern) + 53 additional item non-response ("don't know"/"98")
##                    among the familiar. Original paper sample: N = 1207.
##   - future_bazo ("Will use Bazooka in the future"): NOT conditioned on familiarity in
##                    the same way -- the analysis.R construction (future_bazo == "1")
##                    already maps the unfamiliar respondents' (non-"98") raw values to
##                    FALSE rather than dropping them (all 278 unfamiliar have
##                    future_bazo == FALSE, none NA). This outcome is therefore already
##                    an unconditional recode in the current script and needs no fix; it
##                    is included below only as a consistency check.
##   - downside_risk_imp, share_imp, nr_vars: general ("improved varieties") analogues,
##                    asked of everyone regardless of Bazooka familiarity; unconditional.
##
## Fix: for share_bazo and downside_risk_bazo, recode the 278 respondents who are
## unfamiliar with Bazooka (knw_bazo == FALSE) from NA to a substantively motivated
## value instead of dropping them:
##   - share_bazo: a farmer who does not know a variety exists cannot have recommended
##     it, so the neutral recode is 0 ("did not recommend").
##   - downside_risk_bazo: a farmer with no exposure to the variety has no basis to
##     perceive it as risky, so the neutral recode is 0 ("does not perceive downside
##     risk"). This is also the conservative choice given the paper finds a precise
##     null on this outcome among the familiar subsample.
## Item non-response unrelated to familiarity (the 53 "don't know" answers among
## knw_bazo==TRUE for downside_risk_bazo) is left untouched (still dropped via lm's
## listwise deletion), since that is ordinary missingness, not post-treatment
## conditioning, and is not what R1 flagged.
##
## In addition to the neutral (0) recode we report worst-/best-case bounds by also
## recoding the 278 unfamiliar respondents to 1, bracketing the true (unobserved)
## response the unfamiliar would have given had they been asked. Reporting
## [recode-as-0, recode-as-1] is a simple, transparent bounding exercise in the spirit
## of Lee (2009) trimming bounds, without imposing Lee's monotonicity/selection
## assumptions (which do not obviously apply here -- familiarity is not a selection
## into observing an outcome driven by a single monotone response to treatment, it's a
## survey skip pattern).
##
## Spec: identical to the paper's Table 4 -- y ~ trial_P*cont, CR2 SEs clustered on
## cluster_ID, Satterthwaite df (via clubSandwich::coef_test). No baseline control is
## available for Bazooka-specific outcomes (Bazooka was not yet on the market at
## baseline), matching the original script's treatment of these two outcomes.
##
## Run from the repository root: Rscript papers/increasing_seed_varietal_turnover/analysis/unconditional_recodes.R

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(clubSandwich)
  library(andersonTools)
})

ANADIR <- "papers/increasing_seed_varietal_turnover/analysis"

## ---- build dta exactly as in the paper (sourced construction) -------------
## We source lines 1:1026, which cover everything through construction of
## knw_bazo / downside_risk_bazo / share_bazo / future_bazo, but drop lines
## 801:987 -- an unrelated graph_cash plotting/descriptive digression (uses
## tidyr::pivot_longer + forcats::fct_relevel) that does not feed into any of
## the outcomes used here and is orthogonal to R1-2.
all_lines <- readLines(file.path(ANADIR, "analysis.R"))
src <- all_lines[c(1:800, 988:1026)]
src <- gsub("cons_intervention_merged2.csv", "cons_intervention_merged.csv", src, fixed = TRUE)
src <- src[!grepl("^\\s*library\\(", src)]
e <- new.env()
e$path <- normalizePath(ANADIR)
suppressWarnings(suppressMessages(
  eval(parse(text = paste(src, collapse = "\n")), envir = e)
))
dta <- e$dta
cat("constructed dta rows:", nrow(dta), "\n\n")

## ---- confirm the conditioning diagnosis before doing anything else --------
stopifnot(sum(is.na(dta$knw_bazo)) == 0)
n_unfamiliar <- sum(!dta$knw_bazo)
cat("N unfamiliar with Bazooka (knw_bazo == FALSE):", n_unfamiliar, "\n")
cat("share_bazo: N NA =", sum(is.na(dta$share_bazo)),
    "| all attributable to unfamiliarity:", all(is.na(dta$share_bazo) == !dta$knw_bazo), "\n")
cat("downside_risk_bazo: N NA =", sum(is.na(dta$downside_risk_bazo)),
    "| N NA among unfamiliar:", sum(is.na(dta$downside_risk_bazo) & !dta$knw_bazo),
    "| N NA among familiar (item non-response):", sum(is.na(dta$downside_risk_bazo) & dta$knw_bazo), "\n")
cat("future_bazo: N NA among unfamiliar (should be 0 -- already unconditional):",
    sum(is.na(dta$future_bazo) & !dta$knw_bazo), "\n\n")

## ---- build recoded / bounded versions --------------------------------------
## share_bazo and downside_risk_bazo: recode unfamiliar (knw_bazo == FALSE) NAs.
## Item non-response among the familiar is left as NA (ordinary missing data).
dta$share_bazo_neutral <- dta$share_bazo
dta$share_bazo_neutral[!dta$knw_bazo] <- 0

dta$share_bazo_lo <- dta$share_bazo_neutral         # lower bound == neutral recode (0)
dta$share_bazo_hi <- dta$share_bazo
dta$share_bazo_hi[!dta$knw_bazo] <- 1               # upper bound: unfamiliar recoded 1

dta$downside_risk_bazo_neutral <- dta$downside_risk_bazo
dta$downside_risk_bazo_neutral[!dta$knw_bazo] <- 0

dta$downside_risk_bazo_lo <- dta$downside_risk_bazo_neutral
dta$downside_risk_bazo_hi <- dta$downside_risk_bazo
dta$downside_risk_bazo_hi[!dta$knw_bazo] <- 1

## ---- estimation function: y ~ trial_P*cont, CR2 clustered SEs, Satterthwaite --
fit_one <- function(y, dat) {
  dd <- dat[!is.na(dat[[y]]), ]
  m  <- lm(as.formula(paste(y, "~ trial_P*cont")), data = dd)
  vc <- vcovCR(m, cluster = dd$cluster_ID, type = "CR2")
  ct <- as.data.frame(coef_test(m, vc))
  data.frame(
    term  = rownames(ct),
    beta  = ct$beta, se = ct$SE, df = ct$df_Satt, p = ct$p_Satt,
    N     = nobs(m)
  )
}

specs <- list(
  share_bazo          = c(original = "share_bazo",          neutral = "share_bazo_neutral",
                          lower = "share_bazo_lo",          upper = "share_bazo_hi"),
  downside_risk_bazo  = c(original = "downside_risk_bazo",  neutral = "downside_risk_bazo_neutral",
                          lower = "downside_risk_bazo_lo",  upper = "downside_risk_bazo_hi")
)

results <- list()
for (outcome in names(specs)) {
  for (version in names(specs[[outcome]])) {
    yv <- specs[[outcome]][version]
    r  <- fit_one(yv, dta)
    r$outcome <- outcome
    r$version <- version
    results[[paste(outcome, version)]] <- r
  }
}
res_df <- do.call(rbind, results)
rownames(res_df) <- NULL

## keep only the coefficients of interest (drop intercept)
res_df <- res_df[res_df$term != "(Intercept)", ]

fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)

cat("================================================================================\n")
cat("Table: original (familiar-only) vs. unconditional recodes vs. bounds\n")
cat("Spec: y ~ trial_P*cont, CR2 SEs clustered on cluster_ID, Satterthwaite df\n")
cat("================================================================================\n\n")

for (outcome in names(specs)) {
  cat(sprintf("---- %s ----\n", outcome))
  sub <- res_df[res_df$outcome == outcome, ]
  for (version in c("original", "neutral", "lower", "upper")) {
    s <- sub[sub$version == version, ]
    cat(sprintf("  [%s] N = %d\n", version, s$N[1]))
    for (i in seq_len(nrow(s))) {
      stars <- ifelse(s$p[i] < 0.01, "***", ifelse(s$p[i] < 0.05, "**", ifelse(s$p[i] < 0.10, "*", "")))
      cat(sprintf("    %-22s beta = %7s (SE %6s), df = %5s, p = %6s %s\n",
                  s$term[i], fmt(s$beta[i]), fmt(s$se[i]), fmt(s$df[i], 1), fmt(s$p[i]), stars))
    }
  }
  cat("\n")
}

## ---- save for use in the manuscript appendix table -------------------------
save(res_df, file = file.path(dirname(ANADIR), "results", "df_unconditional_recodes.Rdata"))
write.csv(res_df, file = file.path(dirname(ANADIR), "results", "df_unconditional_recodes.csv"), row.names = FALSE)
cat("Saved: papers/increasing_seed_varietal_turnover/results/df_unconditional_recodes.{Rdata,csv}\n")
