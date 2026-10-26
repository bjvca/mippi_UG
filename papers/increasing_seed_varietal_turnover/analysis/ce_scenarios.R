## Cost-effectiveness scenario table with confidence intervals (R1-5, R2-3).
##
## Fixes and extends analysis/cost_effectiveness.R WITHOUT editing it:
##   (a) R1-5 flagged that cost_effectiveness.R divides the demo "turnover"
##       cost by a hard-coded ITT of 0.09. That 0.09 is the ADOPTION INDEX
##       coefficient on the cooking-demo main effect (Table 3, row 1), not
##       the "has used Bazooka on any plot" coefficient (Table 3, row 2),
##       which is what the pack-side numerator (0.67) and the cost-per-
##       adopter narrative in Section 7 actually use. The correct row-2
##       demo coefficient is 0.071 (rounds to the "0.07" printed in Table 3;
##       see saturated-model refit below, which reproduces beta_S=0.667,
##       beta_D=0.071 exactly as in interaction_equivalence.R). This script
##       uses the refit coefficients throughout instead of any hard-coded
##       ITT constants, so the 0.09-vs-0.07 class of bug cannot recur.
##   (b) Adds 95% CIs on every ITT effect (CR2/Satterthwaite, matching the
##       paper's saturated-model spec) and on every cost-per-adopter RATIO,
##       via parametric bootstrap of the effect's sampling distribution
##       (t_df) with a closed-form Fieller-style inversion reported
##       alongside as a cross-check. When an effect's CI includes 0, the
##       ratio's bootstrap distribution is heavy-tailed/unbounded; we flag
##       this explicitly rather than report a spuriously finite interval.
##   (c) Produces one row per policy objective (any Bazooka use / fresh
##       Bazooka use / fresh improved hybrid+OPV use), each broken out by
##       arm (pack, demo), i.e. the recycled-hybrid-counted vs
##       recycled-hybrid-excluded scenarios are the "any Bazooka" and
##       "fresh improved" rows respectively (Table 3 rows 2 and 7).
##   (d) Separates the OBSERVED cost side (mechanical function of session
##       attendance, no extrapolation) from the EXTRAPOLATED effectiveness
##       side (assumes the demo's per-attendee effect is unchanged at
##       attendance levels beyond the ~10/session realized in the trial;
##       this is an assumption, not an estimate, and cannot be tested with
##       these data).
##
## Run from the repository root:
##   Rscript papers/increasing_seed_varietal_turnover/analysis/ce_scenarios.R

suppressMessages({
  library(clubSandwich)
})

ANADIR <- "papers/increasing_seed_varietal_turnover/analysis"
set.seed(20260911)  # parametric bootstrap seed, reproducible

## ============================================================
## 1. Reconstruct the estimation dataset exactly as the paper does
##    (same pattern as interaction_equivalence.R / heterogeneity.R)
## ============================================================
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

## ============================================================
## 2. Refit the saturated model (Table 3 spec) for the three
##    objective-defining outcomes and pull out BOTH main effects
##    (pack = trial_PTRUE, demo = contTRUE) with CR2 SEs.
## ============================================================
outcomes <- c(anyBazooka   = "p_outcome_2alt",  # Table 3 row 2: has used Bazooka, any plot (recycling COUNTS as turnover)
              freshBazooka = "p_outcome_2",     # Table 3 row 6: has used FRESH Bazooka (trusted source)
              freshImproved= "p_outcome_1")     # Table 3 row 7: fresh hybrid OR fresh/appropriately-recycled OPV
                                                 #   (recycled HYBRID grain explicitly EXCLUDED -> recycling does NOT count)
bctrl <- c(anyBazooka    = "b_p_outcome_2alt",
           freshBazooka  = "b_p_outcome_2",
           freshImproved = "b_p_outcome_1")

fit_one <- function(y, bc) {
  rhs <- "trial_P*cont"
  if (!is.na(bc)) rhs <- paste(rhs, bc, sep = " + ")
  dd <- dta[!is.na(dta[[y]]), ]
  m  <- lm(as.formula(paste(y, "~", rhs)), data = dd)
  vc <- vcovCR(m, cluster = dd$cluster_ID, type = "CR2")
  ct <- as.data.frame(coef_test(m, vc))
  get_row <- function(cf) {
    b  <- ct[cf, "beta"]; se <- ct[cf, "SE"]; df <- ct[cf, "df_Satt"]; p <- ct[cf, "p_Satt"]
    ci <- b + c(-1, 1) * qt(0.975, df) * se
    c(beta = b, se = se, df = df, p = p, ci_lo = ci[1], ci_hi = ci[2])
  }
  list(pack = get_row("trial_PTRUE"), demo = get_row("contTRUE"), N = nobs(m))
}

fits <- Map(fit_one, outcomes, bctrl)
names(fits) <- names(outcomes)

cat("---- saturated-model main effects (sanity check vs Table 3) ----\n")
for (nm in names(fits)) {
  cat(sprintf("%-14s pack beta=%.3f (SE %.3f)  demo beta=%.3f (SE %.3f)  N=%d\n",
              nm, fits[[nm]]$pack["beta"], fits[[nm]]$pack["se"],
              fits[[nm]]$demo["beta"], fits[[nm]]$demo["se"], fits[[nm]]$N))
}
cat("Expect anyBazooka: pack ~0.667, demo ~0.071 (Table 3 prints 0.67**, 0.07)\n\n")

## ============================================================
## 3. Cost side (unchanged inputs from cost_effectiveness.R;
##    sourced, not edited, not re-derived)
## ============================================================
ce <- new.env()
.oldwd <- setwd(dirname(ANADIR))  # cost_effectiveness.R writes to "analysis/cost_scaling.png"
                                  # relative to the paper directory, not the analysis/ dir itself
suppressWarnings(suppressMessages(
  capture.output(source("analysis/cost_effectiveness.R", local = ce))
))
setwd(.oldwd)

cost_pack <- ce$pack_pf_realized            # ~27,100 UGX/farmer (standalone pack distribution)
cost_demo <- ce$demo_pf_realized_scalable    # ~64,361 UGX/farmer; this is the "scalable" (research-
                                             # overhead-stripped) figure and is the one that matches
                                             # the ~64,000 the manuscript currently cites as the demo's
                                             # per-farmer cost in Section 7 -- NOTE for Bjorn: the
                                             # manuscript labels this "full economic cost", but in
                                             # cost_effectiveness.R the object with overhead INCLUDED
                                             # is demo_pf_realized_full (~75,335). The manuscript's
                                             # label and the script's variable name disagree; we use
                                             # the number that matches the manuscript's printed figure
                                             # (64k) so nothing else in Section 7 has to be re-derived,
                                             # but flag this naming mismatch as an open question below.
cost_demo_full <- ce$demo_pf_realized_full   # ~75,335 UGX/farmer, INCLUDING research overhead

cat(sprintf("cost_pack = %.0f, cost_demo (scalable, cited as '%.0f' in ms) = %.0f, cost_demo_full = %.0f\n\n",
            cost_pack, cost_demo, cost_demo, cost_demo_full))

## ============================================================
## 4. CE ratio + CI: parametric bootstrap of the effect's sampling
##    distribution, cross-checked against a closed-form (Fieller-
##    style) inversion of the effect's own CI. Cost is treated as
##    fixed/known (it is an accounting total from the budget, not
##    a sample statistic with its own sampling error).
## ============================================================
B <- 100000

ce_ratio <- function(cost, beta_row, label) {
  beta <- beta_row["beta"]; se <- beta_row["se"]; df <- beta_row["df"]
  ci_lo <- beta_row["ci_lo"]; ci_hi <- beta_row["ci_hi"]

  ## Three mutually exclusive regimes:
  ##  - "identified"        : CI excludes 0 and beta > 0 -> ratio well-defined and boundable
  ##  - "unbounded"         : CI straddles 0 -> the ratio's sign and magnitude are both
  ##                          undetermined by the data (can't rule out zero or negative effect)
  ##  - "negative_effect"   : CI excludes 0 and lies entirely BELOW 0 -> the instrument
  ##                          reliably moves this outcome in the WRONG direction for a
  ##                          "cost per additional adopter" framing; a ratio would divide
  ##                          a positive cost by a negative effect and is not meaningful
  straddles0 <- (ci_lo <= 0 & ci_hi >= 0)
  neg_effect <- (!straddles0 & ci_hi < 0)
  regime <- if (neg_effect) "negative_effect" else if (straddles0) "unbounded" else "identified"

  point <- if (regime == "identified") cost / beta else NA

  ## closed-form Fieller-style inversion of the effect CI (monotone
  ## decreasing map beta -> cost/beta for beta>0); only meaningful when "identified"
  fieller <- if (regime == "identified") sort(cost / c(ci_lo, ci_hi)) else c(NA, NA)

  ## parametric bootstrap: beta_sim ~ beta + se * t_df, ratio computed only over
  ## the region of the simulated distribution consistent with the regime (positive
  ## effect -> cost per adopter GAINED). For "unbounded" rows this correctly yields
  ## a heavy-tailed/very wide interval; for "negative_effect" rows we do not force a
  ## bootstrap ratio at all, since >99% of the mass is on the wrong side of zero and
  ## any resulting "CI" would be an artifact of the vanishing positive tail.
  if (regime == "negative_effect") {
    boot_lo <- NA; boot_hi <- NA; share_nonpos <- mean(beta + se * rt(B, df) <= 0)
  } else {
    beta_sim <- beta + se * rt(B, df)
    ratio_sim <- ifelse(beta_sim > 0, cost / beta_sim, NA)
    boot_ci <- quantile(ratio_sim, c(0.025, 0.975), na.rm = TRUE)
    boot_lo <- unname(boot_ci[1]); boot_hi <- unname(boot_ci[2])
    share_nonpos <- mean(beta_sim <= 0)
  }

  data.frame(label = label, beta = beta, se = se, df = df,
             ci_lo = ci_lo, ci_hi = ci_hi,
             point_ratio = point,
             fieller_lo = fieller[1], fieller_hi = fieller[2],
             boot_lo = boot_lo, boot_hi = boot_hi,
             regime = regime, share_beta_nonpos = share_nonpos)
}

rows <- list(
  ce_ratio(cost_pack, fits$anyBazooka$pack,    "Any Bazooka use  x  Pack"),
  ce_ratio(cost_demo, fits$anyBazooka$demo,    "Any Bazooka use  x  Demo"),
  ce_ratio(cost_pack, fits$freshBazooka$pack,  "Fresh Bazooka use  x  Pack"),
  ce_ratio(cost_demo, fits$freshBazooka$demo,  "Fresh Bazooka use  x  Demo"),
  ce_ratio(cost_pack, fits$freshImproved$pack, "Fresh improved (hybrid+OPV) use  x  Pack"),
  ce_ratio(cost_demo, fits$freshImproved$demo, "Fresh improved (hybrid+OPV) use  x  Demo")
)
tab <- do.call(rbind, rows)
rownames(tab) <- NULL

cat("==================== CE SCENARIO TABLE (raw numbers) ====================\n")
print(tab, digits = 3)
cat("\n")

## ============================================================
## 5. Assemble the manuscript-ready table with caveats
## ============================================================
fmt_ci <- function(lo, hi) sprintf("[%.3f, %.3f]", lo, hi)
fmt_money <- function(x) if (is.na(x)) "n/a" else format(round(x), big.mark = ",", scientific = FALSE)
fmt_ratio_point <- function(regime, p) {
  if (regime == "negative_effect") return("n/a (wrong sign)")
  if (regime == "unbounded") return("unbounded")
  fmt_money(p)
}
fmt_ratio_ci <- function(regime, lo, hi) {
  if (regime == "negative_effect") return("n/a (effect CI entirely negative)")
  if (regime == "unbounded" || is.na(lo)) return("unbounded (effect CI includes 0)")
  sprintf("[%s, %s]", fmt_money(lo), fmt_money(hi))
}

caveats <- c(
  "Any Bazooka use  x  Pack"   = "Effect precisely estimated and survives FDR; ratio well identified.",
  "Any Bazooka use  x  Demo"   = "Demo effect (0.07) does NOT survive FDR correction (q=0.110, Table 3); ratio should be read as an order of magnitude, not a precise figure.",
  "Fresh Bazooka use  x  Pack" = "Pack effect on fresh-seed use is statistically indistinguishable from zero (turnover here runs through recycling); ratio is unbounded and not economically meaningful.",
  "Fresh Bazooka use  x  Demo" = "Demo effect survives FDR (Table 3, *); ratio is the paper's most defensible fresh-market figure but still imprecise given the small effect size.",
  "Fresh improved (hybrid+OPV) use  x  Pack" = "Pack effect is NEGATIVE (-0.14**): packs REDUCE fresh-market purchases via substitution. Cost-per-adopter is not applicable; report as a cost per fresh-market adopter FOREGONE.",
  "Fresh improved (hybrid+OPV) use  x  Demo" = "Demo effect (0.03) is far from significant and does not survive FDR; ratio is unbounded."
)

manuscript_tab <- data.frame(
  Objective_Arm = tab$label,
  ITT = sprintf("%.3f", tab$beta),
  CI95 = fmt_ci(tab$ci_lo, tab$ci_hi),
  Cost_per_farmer_UGX = sapply(tab$label, function(l) if (grepl("Pack", l)) fmt_money(cost_pack) else fmt_money(cost_demo)),
  Cost_per_additional_adopter_UGX = mapply(fmt_ratio_point, tab$regime, tab$point_ratio),
  Bootstrap_95CI_on_ratio = mapply(fmt_ratio_ci, tab$regime, tab$boot_lo, tab$boot_hi),
  Caveat = caveats[tab$label]
)

cat("==================== MANUSCRIPT SCENARIO TABLE ====================\n")
print(manuscript_tab, row.names = FALSE)

out_csv <- file.path(ANADIR, "ce_scenarios_table.csv")
write.csv(manuscript_tab, out_csv, row.names = FALSE)
cat(sprintf("\nWritten to %s\n", out_csv))

## ============================================================
## 6. Observed cost scaling vs extrapolated effectiveness at scale
##    (Section 7 / Figure 5 discussion)
## ============================================================
cat("\n==================== SCALING: OBSERVED vs EXTRAPOLATED ====================\n")
cat(sprintf("Breakeven attendance (cost side only, purely mechanical/observed): n = %.1f (no take-home) / %.1f (with 2kg take-home)\n",
            ce$be_without, ce$be_with))
cat("These breakeven figures use ONLY realized unit costs (S_demo, D_pack, seed_cost, v) and simple\n")
cat("arithmetic; they involve no extrapolation and hold regardless of what the demo's effectiveness\n")
cat("does at those attendance levels.\n")
cat("What Figure 5 does NOT show, and cannot show from this trial, is whether the demo's per-attendee\n")
cat("EFFECT on adoption (identified at ~10 attendees/session) is unchanged at 22-68+ attendees. That\n")
cat("is an assumption needed to translate the cost crossover into a cost-per-ADOPTER crossover, not an\n")
cat("estimated quantity; we flag it as such wherever cost-per-adopter figures are extrapolated beyond\n")
cat("n=10.\n")

cat("\nDraft caveat sentence for Section 7:\n")
cat("\"The breakeven attendance in Figure 5 is a mechanical function of observed unit costs and requires\n")
cat("no extrapolation; translating it into a cost-per-adopter crossover, however, requires assuming that\n")
cat("the demonstration's per-attendee effect on adoption -- identified here at roughly ten attendees per\n")
cat("session -- is unchanged at the larger group sizes (22 to 68 or more attendees) needed to reach the\n")
cat("cost crossover, an assumption our design cannot test and that plausibly fails if attendees further\n")
cat("from the cooking and tasting learn less.\"\n")
