## Exploratory heterogeneity analyses (requested by co-authors; R4).
## (1) Prior exposure to improved seed (baseline use of OPV/hybrid, ~42%)
## (2) Cooking-demo attendance composition: both co-heads vs one (DESCRIPTIVE,
##     post-randomization -- not a causal/ITT contrast)
##
## Outcomes: adoption index, used any Bazooka, used fresh Bazooka.
## Spec: pooled treatment x moderator (paper's orthogonal-demeaning approach),
##       CR2 SEs clustered at village level. Exploratory: no FDR adjustment.
##
## To guarantee identical variable construction, we source the paper's own
## analysis.R up to the point where the outcomes and index are built (line 318),
## redirecting one non-public intermediate file to the public merged file.
## Run from the repository root.

suppressMessages({library(clubSandwich)})

ANADIR <- "papers/increasing_seed_varietal_turnover/analysis"

## ---- build dta exactly as in the paper (sourced construction) -------------
src <- readLines(file.path(ANADIR, "analysis.R"))[1:318]
src <- gsub("cons_intervention_merged2.csv", "cons_intervention_merged.csv", src, fixed = TRUE)
src <- src[!grepl("^\\s*library\\(", src)]   # drop package loads (e.g. moments) not needed for construction
e <- new.env()
e$path <- normalizePath(ANADIR)             # analysis.R strips this suffix to get repo root
suppressWarnings(suppressMessages(
  eval(parse(text = paste(src, collapse = "\n")), envir = e)
))
dta <- e$dta
cat("constructed dta rows:", nrow(dta), "\n")

dta$tp <- as.integer(dta$trial_P)            # integer indicators -> clean coef names
dta$co <- as.integer(dta$cont)
dta$M  <- as.integer(dta$b_p_outcome_1)       # prior improved-seed user (baseline)
dta$co_dem <- dta$co - mean(dta$co, na.rm = TRUE)
dta$tp_dem <- dta$tp - mean(dta$tp, na.rm = TRUE)

outcomes <- c(index = "index", anyBazooka = "p_outcome_2alt", freshBazooka = "p_outcome_2")
bctrl    <- c(index = NA,       anyBazooka = "b_p_outcome_2alt", freshBazooka = "b_p_outcome_2")

het <- function(y, treat, other_dem, bc) {
  dd <- dta[!is.na(dta[[y]]) & !is.na(dta$M) & !is.na(dta$cluster_ID), ]
  dd$Mr <- 1 - dd$M
  rhs  <- paste0(treat, "*M + ",  other_dem); if (!is.na(bc)) rhs  <- paste0(rhs,  " + ", bc)
  rhs2 <- paste0(treat, "*Mr + ", other_dem); if (!is.na(bc)) rhs2 <- paste0(rhs2, " + ", bc)
  m  <- lm(as.formula(paste(y, "~", rhs)),  data = dd)
  m2 <- lm(as.formula(paste(y, "~", rhs2)), data = dd)
  vc  <- vcovCR(m,  cluster = dd$cluster_ID, type = "CR2")
  vc2 <- vcovCR(m2, cluster = dd$cluster_ID, type = "CR2")
  ct  <- as.data.frame(coef_test(m,  vc));  ct2 <- as.data.frame(coef_test(m2, vc2))
  e0 <- ct[treat, c("beta","SE","p_Satt")]                 # effect for M=0 (non-users)
  e1 <- ct2[treat, c("beta","SE","p_Satt")]                # effect for M=1 (prior users)
  intr <- ct[paste0(treat, ":M"), c("beta","SE","p_Satt")] # difference (interaction)
  cat(sprintf("  %-5s | M0: %6.3f (%.3f) p=%.3f | M1: %6.3f (%.3f) p=%.3f | diff: %6.3f (%.3f) p=%.3f\n",
              treat, e0$beta,e0$SE,e0$p_Satt, e1$beta,e1$SE,e1$p_Satt, intr$beta,intr$SE,intr$p_Satt))
}

cat("\n=========== PART 1: heterogeneity by prior improved-seed use (M) ===========\n")
cat(sprintf("M (b_p_outcome_1) share = %.3f, N nonmissing = %d\n\n",
            mean(dta$M, na.rm = TRUE), sum(!is.na(dta$M))))
for (nm in names(outcomes)) {
  cat(nm, ":\n")
  het(outcomes[nm], "tp", "co_dem", bctrl[nm])
  het(outcomes[nm], "co", "tp_dem", bctrl[nm])
}

## sanity: pooled main effects without M should reproduce paper values
cat("\n--- sanity: pooled main effects (no moderator) ---\n")
for (nm in names(outcomes)) {
  y <- outcomes[nm]; dd <- dta[!is.na(dta[[y]]), ]
  for (tr in c("tp", "co")) {
    od <- ifelse(tr == "tp", "co_dem", "tp_dem")
    m <- lm(as.formula(paste(y, "~", tr, "*", od)), data = dd)
    vc <- vcovCR(m, cluster = dd$cluster_ID, type = "CR2")
    b <- coef_test(m, vc)[tr, c("beta", "p_Satt")]
    cat(sprintf("  %-12s %-8s beta=%6.3f p=%.3f\n", nm, tr, b$beta, b$p_Satt))
  }
}

## =========== PART 2: attendance composition (DESCRIPTIVE, demo arm) ==========
cat("\n=========== PART 2: demo attendance composition (descriptive) ===========\n")
cons <- read.csv(file.path(ANADIR, "../../..", "midline/consumption_treatment/data/public/cons_intervention_merged.csv"),
                 stringsAsFactors = FALSE)
cons$attended <- cons$grDetails.attend == "Yes"
cons$both_att <- cons$grDetails.attend == "Yes" & cons$grDetails.sp_att == "Yes"
cons <- cons[cons$attended %in% TRUE, c("farmer_ID", "both_att")]
d2 <- merge(dta, cons, by.x = "ID", by.y = "farmer_ID")
d2 <- d2[d2$cont %in% TRUE, ]
cat(sprintf("demo attendees matched: %d ; both co-heads attended: %.3f\n\n",
            nrow(d2), mean(d2$both_att, na.rm = TRUE)))
for (nm in names(outcomes)) {
  y <- outcomes[nm]; dd <- d2[!is.na(d2[[y]]) & !is.na(d2$both_att), ]
  m <- lm(as.formula(paste(y, "~ both_att")), data = dd)
  vc <- vcovCR(m, cluster = dd$cluster_ID, type = "CR2")
  ct <- as.data.frame(coef_test(m, vc))
  m0 <- mean(dd[[y]][!dd$both_att]); m1 <- mean(dd[[y]][dd$both_att])
  cat(sprintf("  %-12s one=%.3f both=%.3f diff=%6.3f (%.3f) p=%.3f  N=%d\n",
              nm, m0, m1, ct["both_attTRUE","beta"], ct["both_attTRUE","SE"],
              ct["both_attTRUE","p_Satt"], nrow(dd)))
}
