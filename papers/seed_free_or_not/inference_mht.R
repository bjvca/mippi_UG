## inference_mht.R
##
## Referee-requested robustness checks for the sunk-cost ("paid vs free seed", village-level
## randomized) coefficient, plus multiple-hypothesis-testing (MHT) correction across the
## individual outcomes that make up the three main-text ICW indices.
##
## Does NOT modify analysis.R / analysis_nocensored.R / the .lyx file / any existing .Rdata.
## Self-contained: re-derives every variable it needs directly from data/{baseline,midline,
## endline}.csv, following analysis.R's own construction code line-for-line (binary "sunk"
## coding, file suffix "_01", which is what feeds the main-text tables -- confirmed below by
## exactly reproducing the two anchor numbers from Table 2).
##
## TASK A (sunk-cost coefficient, all 20 outcomes = 17 individual + 3 indices):
##   - wild cluster bootstrap (WCB), Rademacher weights, null imposed on the sunk coefficient,
##     B = 999, village-level clusters (the same "cluster_ID" analysis.R uses for the main-text
##     vcovCL/HC0 clustered SEs)
##   - randomization inference (RI): 2000 fresh 38/38 permutations of "discounted" across the
##     true baseline-level villages (76 villages, pure by construction -- see note below on why
##     this differs from the "cluster_ID" used for SE clustering in the midline tables)
##   - both repeated with baseline controls (hh_size, male_head) added, for whichever outcomes
##     were found imbalanced between arms
##
## TASK B (MHT correction, individual outcomes only, all three margins: sunk/screening/signaling):
##   - Romano-Wolf stepdown p-values, reusing the WCB draws (same Rademacher weight matrix
##     shared across all outcomes within a family, so the joint null distribution captures the
##     correlation across outcomes)
##   - Benjamini-Hochberg q-values on the cluster-robust p-values
##
## NOTE on the clustering variable: analysis.R recomputes "cluster_ID" separately at each survey
## wave from that wave's own district/sub-county/village ID fields. At baseline this yields
## exactly 76 villages, pure 38 paid / 38 free (no farmer-level mixing) -- the true randomization
## unit. At midline the same construction (from dist_ID/sub_ID/vil_ID) yields 115 distinct
## combinations across the two midline tables' regression samples (only ~76-78 of which actually
## appear in the Table 2 / Table 3 regressions, and a handful of those are NOT pure: a few
## farmers within the same recomputed midline "cluster_ID" have different "discounted" status).
## This is very likely mis-recorded/re-coded village identifiers between survey rounds, not real
## sub-village variation in treatment -- but it is exactly the variable vcovCL(...,cluster=...)
## uses for the published midline SEs, and using it exactly reproduces the published numbers to
## 3 decimals (checked below), so it is also what we use here for cluster-robust SEs and for the
## WCB resampling clusters (matches "the main-text SEs" the referee is asking about). At endline,
## analysis.R instead merges the ORIGINAL baseline cluster_ID onto farmers by ID, so Table 4's
## clustering variable is much closer to the true 76-village design (77-78 distinct levels in
## the regression samples, only 1-2 farmers with a baseline record that doesn't merge onto a
## village used in the bargaining game). For RANDOMIZATION INFERENCE specifically -- which must
## respect the *true* assignment mechanism -- we permute "discounted" at the level of the
## baseline-derived village identifier (76 villages, pure by construction), not at the level of
## the (partially noisy) per-wave "cluster_ID". This is flagged again in the final report.
##
## Runtime note: B=999 (WCB) / 2000 (RI) throughout; see printed timing at the end. If this
## exceeded ~20 minutes we would drop to B=499 (WCB) -- see final printed message for whether
## that was necessary.

rm(list = ls())
t0 <- Sys.time()
set.seed(20260927)

suppressMessages({
  library(dplyr)
  library(sandwich)
  library(lmtest)
  library(clubSandwich)
})

path <- "/home/claude/workspace/mippi_UG/papers/seed_free_or_not"
datapath <- paste0(path, "/data")

B_WCB <- 999
B_RI  <- 2000

## ---------------------------------------------------------------------
## 0. helper functions (copied/adapted from analysis.R)
## ---------------------------------------------------------------------

icwIndex <- function(xmat, revcols = NULL, sgroup = rep(TRUE, nrow(xmat))) {
  matStand <- function(x, sgroup = rep(TRUE, nrow(x))) {
    for (j in 1:ncol(x)) x[, j] <- (x[, j] - mean(x[sgroup, j], na.rm = TRUE)) / sd(x[sgroup, j], na.rm = TRUE)
    return(x)
  }
  X <- matStand(xmat, sgroup)
  if (length(revcols) > 0) X[, revcols] <- -1 * X[, revcols]
  i.vec <- as.matrix(rep(1, ncol(xmat)))
  Sx <- cov(X, use = "pairwise.complete.obs")
  index <- t(solve(t(i.vec) %*% solve(Sx) %*% i.vec) %*% t(i.vec) %*% solve(Sx) %*% t(X))
  return(list(index = index))
}

trim <- function(var, dataset, trim_perc = .02) {
  dataset[var][dataset[var] < quantile(dataset[var], c(trim_perc / 2, 1 - (trim_perc / 2)), na.rm = TRUE)[1] |
    dataset[var] > quantile(dataset[var], c(trim_perc / 2, 1 - (trim_perc / 2)), na.rm = TRUE)[2]] <- NA
  return(dataset)
}

ihs <- function(x) log(x + sqrt(x^2 + 1))

cr_extract <- function(ols, cluster, coef_row = 2) {
  cr <- coeftest(ols, vcov = vcovCL(ols, cluster = cluster, type = "HC0"))
  c(cr[coef_row, 1], cr[coef_row, 2], cr[coef_row, 4])
}

## ---------------------------------------------------------------------
## 1. baseline data (bse / bse_reg): bargaining game, WTP, baseline covariates,
##    and the TRUE village-level randomization unit (76 villages, pure 38/38)
## ---------------------------------------------------------------------

bse <- read.csv(paste(datapath, "baseline.csv", sep = "/"))
bse$cluster_ID <- as.factor(paste(paste(bse$distID, bse$subID, sep = "_"), bse$vilID, sep = "_"))

bse$bid <- ifelse(!is.na(as.numeric(bse$paid.P2_pric_11)), as.numeric(bse$paid.P2_pric_11),
  ifelse(!is.na(as.numeric(bse$paid.P2_pric_10)), as.numeric(bse$paid.P2_pric_10),
    ifelse(!is.na(as.numeric(bse$paid.P2_pric_9)), as.numeric(bse$paid.P2_pric_9),
      ifelse(!is.na(as.numeric(bse$paid.P2_pric_8)), as.numeric(bse$paid.P2_pric_8),
        ifelse(!is.na(as.numeric(bse$paid.P2_pric_7)), as.numeric(bse$paid.P2_pric_7),
          ifelse(!is.na(as.numeric(bse$paid.P2_pric_6)), as.numeric(bse$paid.P2_pric_6),
            ifelse(!is.na(as.numeric(bse$paid.P2_pric_5)), as.numeric(bse$paid.P2_pric_5),
              ifelse(!is.na(as.numeric(bse$paid.P2_pric_4)), as.numeric(bse$paid.P2_pric_4),
                ifelse(!is.na(as.numeric(bse$paid.P2_pric_3)), as.numeric(bse$paid.P2_pric_3),
                  ifelse(!is.na(as.numeric(bse$paid.P2_pric_2)), as.numeric(bse$paid.P2_pric_2),
                    as.numeric(bse$paid.P2_pric)
                  ))))))))))
bse$bid[bse$bid > 20000] <- NA

bse$ask <- ifelse(!is.na(as.numeric(bse$paid.P3_pric_10)), as.numeric(bse$paid.P3_pric_10),
  ifelse(!is.na(as.numeric(bse$paid.P3_pric_9)), as.numeric(bse$paid.P3_pric_9),
    ifelse(!is.na(as.numeric(bse$paid.P3_pric_8)), as.numeric(bse$paid.P3_pric_8),
      ifelse(!is.na(as.numeric(bse$paid.P3_pric_7)), as.numeric(bse$paid.P3_pric_7),
        ifelse(!is.na(as.numeric(bse$paid.P3_pric_6)), as.numeric(bse$paid.P3_pric_6),
          ifelse(!is.na(as.numeric(bse$paid.P3_pric_5)), as.numeric(bse$paid.P3_pric_5),
            ifelse(!is.na(as.numeric(bse$paid.P3_pric_4)), as.numeric(bse$paid.P3_pric_4),
              ifelse(!is.na(as.numeric(bse$paid.P3_pric_3)), as.numeric(bse$paid.P3_pric_3),
                ifelse(!is.na(as.numeric(bse$paid.P3_pric_2)), as.numeric(bse$paid.P3_pric_2),
                  ifelse(!is.na(as.numeric(bse$paid.P3_pric)), as.numeric(bse$paid.P3_pric),
                    as.numeric(bse$P1_pric)
                  ))))))))))
bse$ask[bse$ask > 14000] <- NA

bse$accepts <- "seller"
bse$accepts[bse$paid.start_neg == "Yes" | bse$paid.start_neg_2 == "Yes" | bse$paid.start_neg_3 == "Yes" |
  bse$paid.start_neg_4 == "Yes" | bse$paid.start_neg_5 == "Yes" | bse$paid.start_neg_6 == "Yes" | bse$paid.start_neg_7 == "Yes" |
  bse$paid.start_neg_8 == "Yes" | bse$paid.start_neg_9 == "Yes" | bse$paid.start_neg_10 == "Yes" | bse$paid.start_neg_11 == "Yes"] <- "buyer"

bse$final_price <- NA
bse$final_price[bse$accepts == "buyer"] <- bse$ask[bse$accepts == "buyer"]
bse$final_price[bse$accepts == "seller"] <- bse$bid[bse$accepts == "seller"]

bse <- subset(bse, (cont == FALSE | trial_P == FALSE) & (paid_pac == TRUE | discounted == TRUE))

bse$age_head <- as.numeric(as.character(bse$age)); bse$age_head[bse$age_head == 999] <- NA
bse$prim_head <- bse$edu %in% c("c", "d", "e", "f")
bse$male_head <- bse$gender == "Male"
bse$hh_size <- as.numeric(as.character(bse$hh_size))
bse$dist_ag <- as.numeric(as.character(bse$dist_ag)); bse$dist_ag[bse$dist_ag == 999] <- NA
bse$quality_use <- bse$quality_use == "Yes"
bse$promo_use_rand <- bse$maize_var == "Bazooka"
bse$source_rand <- bse$source %in% letters[seq(from = 4, to = 9)]
bse$often_rand <- bse$often %in% letters[seq(from = 1, to = 5)]
bse$bag_harv[bse$bag_harv == "999"] <- NA
bse$prod_rand <- as.numeric(as.character(bse$bag_harv)) * as.numeric(as.character(bse$bag_kg))
bse$acre_rand <- as.numeric(as.character(bse$plot_size)); bse$acre_rand[bse$acre_rand == 999] <- NA
bse$yield_rand <- bse$prod_rand / bse$acre_rand
bse <- trim("yield_rand", bse, trim_perc = .01)

bse_reg <- subset(bse, !trial_P)
bse_reg$screening <- as.numeric(as.character(bse_reg$final_price)) / 1000
bse_reg$signaling <- as.numeric(as.character(bse_reg$P1_pric)) / 1000
bse_reg$sunk <- as.numeric(!bse_reg$discounted)

## true village-level randomization unit: 76 villages, pure 38/38 (verified below)
vil_tab <- unique(bse_reg[c("cluster_ID", "discounted")])
vil_tab$cluster_ID <- droplevels(vil_tab$cluster_ID)
stopifnot(nrow(vil_tab) == length(unique(vil_tab$cluster_ID))) # one row per village, no mixing
cat("TRUE baseline villages:", nrow(vil_tab), " | paid:", sum(!vil_tab$discounted), " free:", sum(vil_tab$discounted), "\n")

## per-farmer lookup: farmer_ID -> true baseline village (used only for RI)
true_vil_lookup <- bse_reg[c("farmer_ID", "cluster_ID")]
names(true_vil_lookup) <- c("farmer_ID", "true_vil")

## baseline covariates for the "with controls" runs, + imbalance screen
outcomes_bal <- c("age_head", "prim_head", "male_head", "hh_size", "dist_ag", "quality_use",
                   "promo_use_rand", "source_rand", "often_rand", "acre_rand", "yield_rand")
imbalance <- data.frame(covariate = outcomes_bal, coef = NA, p = NA)
for (i in seq_along(outcomes_bal)) {
  ols <- lm(as.formula(paste(outcomes_bal[i], "~ discounted")), data = bse_reg)
  cr <- coeftest(ols, vcov = vcovCL(ols, cluster = bse_reg$cluster_ID, type = "HC0"))
  imbalance$coef[i] <- cr[2, 1]
  imbalance$p[i] <- cr[2, 4]
}
imbalance <- imbalance[order(imbalance$p), ]
cat("\nBaseline imbalance screen (discounted vs not), village-clustered HC0:\n")
print(imbalance, digits = 3)
imbalanced_covariates <- imbalance$covariate[imbalance$p < 0.10]
cat("\nImbalanced at 10%:", paste(imbalanced_covariates, collapse = ", "), "\n")
control_vars <- c("hh_size", "male_head") # confirmed below to match imbalanced_covariates
stopifnot(setequal(control_vars, imbalanced_covariates))

covariate_lookup <- bse_reg[c("farmer_ID", control_vars)]

## ---------------------------------------------------------------------
## 2. midline data (Table 2 "use" family, Table 3 "plan" family)
## ---------------------------------------------------------------------

dta <- read.csv(paste(datapath, "midline.csv", sep = "/"))
dta <- merge(dta, bse[c("farmer_ID", "P1_pric", "final_price")], by.x = "ID", by.y = "farmer_ID", all.x = TRUE)
dta$cluster_ID <- as.factor(paste(paste(dta$dist_ID, dta$sub_ID, sep = "_"), dta$vil_ID, sep = "_"))

dta$used_TP[dta$used_TP == "n/a"] <- NA
dta$used_TP <- dta$used_TP == "Yes"
dta$TP_separate[dta$TP_separate == "n/a"] <- NA
dta$TP_separate <- dta$TP_separate == 1
dta$space[dta$space == "n/a"] <- NA
dta$space[dta$space == "98"] <- NA
dta$seed_no[dta$seed_no == "n/a"] <- NA
dta$cor_plant <- (dta$space == "2" & dta$seed_no == "2") | (dta$space == "3" & dta$seed_no == "1")
dta$dap_app[dta$dap_app == "n/a"] <- NA
dta$ure_app[dta$ure_app == "n/a"] <- NA
dta$use_fert_inorg <- dta$dap_app == "Yes" | dta$ure_app == "Yes"
dta$org_app[dta$org_app == "n/a"] <- NA
dta$use_fert_org <- dta$org_app == "Yes"
dta$use_fert <- dta$use_fert_inorg | dta$use_fert_org
dta$cide_use[dta$cide_use == "n/a"] <- NA
dta$use_chem <- dta$cide_use == "Yes"
dta$sep_post_harvest[dta$sep_post_harvest == "n/a"] <- NA
dta$sep_post_harvest <- dta$sep_post_harvest == "Yes"
dta$who_used[dta$who_used == "n/a"] <- NA
dta$who_used <- dta$who_used == "1"

dta$plan_imp <- (dta$seed_nxt == 1 | dta$seed_nxt == 2)
dta$plan_bazooka <- dta$imp_var.Bazooka == "True"
dta$plan_bought <- dta$buy_plan == "Yes"
dta$plan_area <- as.numeric(as.character(dta$area_plan))
dta$plan_area[dta$plan_area > 50] <- NA

dta_reg <- subset(dta, !trial_P)
dta_reg$screening <- as.numeric(as.character(dta_reg$final_price)) / 1000
dta_reg$signaling <- as.numeric(as.character(dta_reg$P1_pric)) / 1000
dta_reg$sunk <- as.numeric(!dta_reg$discounted) # binary sunk cost ("_01" main-text spec)
dta_reg$d_screening <- dta_reg$screening - mean(dta_reg$screening, na.rm = TRUE)
dta_reg$d_signaling <- dta_reg$signaling - mean(dta_reg$signaling, na.rm = TRUE)
dta_reg$d_sunk <- dta_reg$sunk - mean(dta_reg$sunk, na.rm = TRUE)

outcomes_use <- c("used_TP", "TP_separate", "who_used", "sep_post_harvest", "cor_plant", "use_fert", "use_chem")
dta_reg$index_use <- icwIndex(xmat = dta_reg[outcomes_use])$index

outcomes_plan <- c("plan_imp", "plan_bazooka", "plan_area", "plan_bought")
dta_reg$index_plan <- icwIndex(xmat = dta_reg[outcomes_plan])$index

## merge farmer ID, baseline covariates, true village
dta_reg <- merge(dta_reg, true_vil_lookup, by.x = "ID", by.y = "farmer_ID", all.x = TRUE)
dta_reg <- merge(dta_reg, covariate_lookup, by.x = "ID", by.y = "farmer_ID", all.x = TRUE)

## ---------------------------------------------------------------------
## 3. REPLICATION GATE: reproduce -0.103 (index_use) and -0.122 (sep_post_harvest) exactly
## ---------------------------------------------------------------------

ols_chk1 <- lm(index_use ~ sunk * d_screening * d_signaling, data = dta_reg)
cr_chk1 <- cr_extract(ols_chk1, dta_reg$cluster_ID)
ols_chk2 <- lm(sep_post_harvest ~ sunk * d_screening * d_signaling, data = dta_reg)
cr_chk2 <- cr_extract(ols_chk2, dta_reg$cluster_ID)

cat("\n=== REPLICATION GATE (must match published Table 2 exactly) ===\n")
cat(sprintf("index_use sunk coef = %.3f (target -0.103)\n", cr_chk1[1]))
cat(sprintf("sep_post_harvest sunk coef = %.3f (target -0.122)\n", cr_chk2[1]))
if (round(cr_chk1[1], 3) != -0.103 || round(cr_chk2[1], 3) != -0.122) {
  stop("REPLICATION GATE FAILED -- do not proceed. Check data construction against analysis.R.")
}
cat("REPLICATION GATE PASSED.\n\n")

## ---------------------------------------------------------------------
## 4. endline data (Table 4 "next_season" family)
## ---------------------------------------------------------------------

dta2 <- read.csv(paste(datapath, "endline.csv", sep = "/"))
dta2 <- subset(dta2, cont == FALSE & (trial_P == TRUE | paid_pac == TRUE | discounted == TRUE))
bse_full_tmp <- read.csv(paste(datapath, "baseline.csv", sep = "/"))
bse_full_tmp$cluster_ID <- as.factor(paste(paste(bse_full_tmp$distID, bse_full_tmp$subID, sep = "_"), bse_full_tmp$vilID, sep = "_"))
dta2 <- merge(dta2, bse_full_tmp[c("farmer_ID", "cluster_ID")], by.x = "ID", by.y = "farmer_ID", all.x = TRUE)
dta2 <- merge(dta2, bse[c("farmer_ID", "P1_pric", "final_price")], by.x = "ID", by.y = "farmer_ID", all.x = TRUE)
rm(bse_full_tmp)
dta2 <- subset(dta2, !is.na(cluster_ID))

dta2$rnd_num <- as.numeric(dta2$plot_select)
dta2_sub <- subset(dta2, !is.na(rnd_num))
dta2_sub <- dta2_sub %>% rowwise() %>%
  mutate(times_recycled_selected = get(paste0("plot.", rnd_num, "..plot_times_rec"))) %>% as.data.frame()
dta2_sub <- dta2_sub %>% rowwise() %>%
  mutate(single_source_selected = get(paste0("plot.", rnd_num, "..single_source"))) %>% as.data.frame()
dta2_sub <- dta2_sub %>% rowwise() %>%
  mutate(recycled_source_selected = get(paste0("plot.", rnd_num, "..recycle_source_rest"))) %>% as.data.frame()
dta2 <- merge(dta2, dta2_sub[c("ID", "times_recycled_selected", "single_source_selected", "recycled_source_selected")],
  by.x = "ID", by.y = "ID", all.x = TRUE)

dta2$rnd_adopt <- (((dta2$maize_var_selected %in%
  c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) &
  (dta2$times_recycled_selected %in% 1) & (dta2$single_source_selected %in% letters[4:9])) |
  ((dta2$maize_var_selected %in% c("Longe_5", "Longe_5D", "Longe_4", "MM3", "other_opv")) &
    (dta2$times_recycled_selected %in% 1:4) &
    (((dta2$single_source_selected %in% letters[4:9])) | (dta2$recycled_source_selected %in% letters[4:9]))))
dta2$rnd_adopt[dta2$no_grow] <- NA
dta2$rnd_adopt[dta2$maize_var_selected == "n/a"] <- NA

dta2$rnd_bazo <- ((dta2$maize_var_selected == "Bazooka") & (dta2$single_source_selected %in% letters[4:9] & (dta2$times_recycled_selected %in% 1)))
dta2$rnd_bazo[dta2$no_grow] <- NA
dta2$rnd_bazo[dta2$maize_var_selected == "n/a"] <- NA

dta2$bag_harv[dta2$bag_harv == "999"] <- NA
dta2$production <- as.numeric(as.character(dta2$bag_harv)) * as.numeric(as.character(dta2$bag_kg))
dta2 <- trim("production", dta2)
dta2$productivity <- dta2$production / dta2$size_selected
dta2 <- trim("productivity", dta2)
dta2$production_ihs <- ihs(dta2$production)
dta2$productivity_ihs <- ihs(dta2$productivity)

dta2_reg <- subset(dta2, !trial_P)
dta2_reg$screening <- as.numeric(as.character(dta2_reg$final_price)) / 1000
dta2_reg$signaling <- as.numeric(as.character(dta2_reg$P1_pric)) / 1000
dta2_reg$sunk <- as.numeric(!dta2_reg$discounted)
dta2_reg$d_screening <- dta2_reg$screening - mean(dta2_reg$screening, na.rm = TRUE)
dta2_reg$d_signaling <- dta2_reg$signaling - mean(dta2_reg$signaling, na.rm = TRUE)
dta2_reg$d_sunk <- dta2_reg$sunk - mean(dta2_reg$sunk, na.rm = TRUE)

dta2_reg$org_ap[dta2_reg$org_ap == "n/a"] <- NA
dta2_reg$dap_ap[dta2_reg$dap_ap == "n/a" | dta2_reg$dap_ap == "98"] <- NA
dta2_reg$ur_ap[dta2_reg$ur_ap == "n/a" | dta2_reg$ur_ap == "98"] <- NA
dta2_reg$pest_ap[dta2_reg$pest_ap == "n/a" | dta2_reg$pest_ap == "98"] <- NA
dta2_reg$use_fert_end <- (dta2_reg$dap_ap == "Yes") | (dta2_reg$ur_ap == "Yes") | (dta2_reg$org_ap == "Yes")
dta2_reg$use_chem_end <- dta2_reg$pest_ap == "Yes"

outcomes_ns <- c("rnd_adopt", "rnd_bazo", "use_fert_end", "use_chem_end", "production_ihs", "productivity_ihs")
dta2_reg$index_next_season <- icwIndex(xmat = dta2_reg[outcomes_ns])$index

dta2_reg <- merge(dta2_reg, true_vil_lookup, by.x = "ID", by.y = "farmer_ID", all.x = TRUE)
dta2_reg <- merge(dta2_reg, covariate_lookup, by.x = "ID", by.y = "farmer_ID", all.x = TRUE)

cr_chk3 <- cr_extract(lm(index_next_season ~ sunk * d_screening * d_signaling, data = dta2_reg), dta2_reg$cluster_ID)
cat(sprintf("(info) index_next_season sunk coef (Table 4) = %.3f, p=%.3f\n\n", cr_chk3[1], cr_chk3[3]))

## ---------------------------------------------------------------------
## 5. assemble the three families
## ---------------------------------------------------------------------

families <- list(
  Table2_use  = list(data = dta_reg,  outcomes = outcomes_use, index = "index_use"),
  Table3_plan = list(data = dta_reg,  outcomes = outcomes_plan, index = "index_plan"),
  Table4_next = list(data = dta2_reg, outcomes = outcomes_ns,   index = "index_next_season")
)

## ---------------------------------------------------------------------
## 6. generic inference machinery
## ---------------------------------------------------------------------

## build a clean (complete-case) design for one outcome/margin/control-set
build_design <- function(data, outcome, margin, controls = NULL) {
  other <- setdiff(c("sunk", "screening", "signaling"), margin)
  d_others <- paste0("d_", other)
  form_rhs <- paste0(margin, "*", d_others[1], "*", d_others[2])
  if (!is.null(controls)) form_rhs <- paste0(form_rhs, "+", paste(controls, collapse = "+"))
  form <- as.formula(paste(outcome, "~", form_rhs))
  vars <- all.vars(form)
  vars <- unique(c(vars, "cluster_ID", "true_vil"))
  vars <- intersect(vars, names(data))
  dd <- data[vars]
  dd <- dd[complete.cases(dd[setdiff(vars, "true_vil")]), ] # true_vil allowed to be NA (RI-only var)
  mm <- model.matrix(form, data = dd)
  y <- as.vector(dd[[outcome]]) * 1 # coerce logical/matrix-column to plain numeric vector
  list(X = mm, y = y, cl = droplevels(as.factor(dd$cluster_ID)), true_vil = dd$true_vil,
       target = margin, n = nrow(mm))
}

## CR0 (village-clustered HC0) coef/se/t, with the same G/(G-1) small-sample factor sandwich
## applies by default (verified below to match sandwich::vcovCL / clubSandwich::vcovCR exactly)
cr0_fit <- function(X, y, cl) {
  XtX_inv <- solve(t(X) %*% X)
  b <- XtX_inv %*% (t(X) %*% y)
  resid <- y - X %*% b
  S <- rowsum(X * as.vector(resid), group = cl)
  meat <- t(S) %*% S
  V <- XtX_inv %*% meat %*% XtX_inv
  G <- length(unique(cl))
  adj <- G / (G - 1)
  se <- sqrt(diag(V) * adj)
  list(b = as.vector(b), se = as.vector(se), t = as.vector(b) / se, XtX_inv = XtX_inv, G = G)
}

## wild cluster bootstrap: null imposed on column `target`, B draws, shared weight matrix W
## (B x G_master), mapped onto obs via cluster names. Returns vector of B bootstrap t-stats.
wcb_tstar <- function(X, y, cl, target_col, W, master_names) {
  j <- which(colnames(X) == target_col)
  Xr <- X[, -j, drop = FALSE]
  XtX_inv_r <- solve(t(Xr) %*% Xr)
  b_r <- XtX_inv_r %*% (t(Xr) %*% y)
  fitted_r <- as.vector(Xr %*% b_r)
  u_r <- y - fitted_r

  XtX_inv <- solve(t(X) %*% X)
  P <- XtX_inv %*% t(X) # k x n, precomputed once
  G <- length(unique(cl))
  adj <- G / (G - 1)
  obs_idx <- match(as.character(cl), master_names)
  B <- nrow(W)
  tstar <- numeric(B)
  for (b in 1:B) {
    w_obs <- W[b, obs_idx]
    y_star <- fitted_r + w_obs * u_r
    b_star <- as.vector(P %*% y_star)
    resid_star <- y_star - as.vector(X %*% b_star)
    S <- rowsum(X * resid_star, group = cl)
    meat <- t(S) %*% S
    V <- XtX_inv %*% meat %*% XtX_inv
    se_j <- sqrt(V[j, j] * adj)
    tstar[b] <- b_star[j] / se_j
  }
  tstar
}

## Romano-Wolf stepdown across a set of outcomes within one family x margin group.
## Tmat: B x n_outcomes matrix of |t*| (already shared draws); t_obs: n_outcomes vector.
romano_wolf <- function(Tmat, t_obs) {
  n <- length(t_obs)
  ord <- order(abs(t_obs), decreasing = TRUE)
  p <- numeric(n)
  running_max_p <- 0
  for (k in 1:n) {
    remaining <- ord[k:n]
    max_t <- if (length(remaining) > 1) apply(Tmat[, remaining, drop = FALSE], 1, max) else Tmat[, remaining]
    p_k <- mean(max_t >= abs(t_obs[ord[k]]))
    p_k <- max(p_k, running_max_p)
    running_max_p <- p_k
    p[ord[k]] <- p_k
  }
  p
}

## ---------------------------------------------------------------------
## 7. spot-check hand-rolled CR0 against sandwich::vcovCL and clubSandwich::vcovCR
## ---------------------------------------------------------------------

cat("=== spot-checks: hand-rolled CR0 vs sandwich / clubSandwich ===\n")
spotcheck <- function(data, outcome, margin, label) {
  des <- build_design(data, outcome, margin)
  fit <- cr0_fit(des$X, des$y, des$cl)
  j <- which(colnames(des$X) == margin)
  ols <- lm(des$y ~ des$X - 1)
  colnames(des$X) -> cn
  cr_sw <- coeftest(ols, vcov = vcovCL(ols, cluster = des$cl, type = "HC0"))
  cr_cs <- coef_test(ols, vcov = "CR0", cluster = des$cl, test = "naive-t")
  cat(sprintf("%-28s hand se=%.5f  sandwich se=%.5f  clubSandwich se=%.5f\n",
              label, fit$se[j], cr_sw[j, 2], cr_cs$SE[j]))
}
spotcheck(dta_reg, "index_use", "sunk", "index_use / sunk")
spotcheck(dta2_reg, "production_ihs", "sunk", "production_ihs / sunk")
cat("\n")

## ---------------------------------------------------------------------
## 8. Task A + Task B main loop
## ---------------------------------------------------------------------

results <- list() # per family: list of per-outcome/margin objects (t_obs, tstar, etc.)
summary_rows <- list()

RI_draws_done <- FALSE
PermMat <- NULL

for (fam_name in names(families)) {
  fam <- families[[fam_name]]
  data <- fam$data
  outcomes_all <- c(fam$outcomes, fam$index)
  n_ind <- length(fam$outcomes)

  ## master cluster set + shared Rademacher weight matrix for this family (reused across all
  ## outcomes AND margins AND the with-controls variant, so RW draws are properly joint)
  master_names <- as.character(sort(unique(data$cluster_ID)))
  G_master <- length(master_names)
  W_fam <- matrix(sample(c(-1, 1), size = B_WCB * G_master, replace = TRUE), nrow = B_WCB, ncol = G_master)

  fam_res <- list()

  for (margin in c("sunk", "screening", "signaling")) {
    Tmat_store <- matrix(NA_real_, nrow = B_WCB, ncol = n_ind) # for RW, individual outcomes only
    t_obs_ind <- numeric(n_ind)
    p_cluster_ind <- numeric(n_ind)

    for (oi in seq_along(outcomes_all)) {
      outcome <- outcomes_all[oi]
      is_index <- outcome == fam$index

      des <- build_design(data, outcome, margin)
      fit <- cr0_fit(des$X, des$y, des$cl)
      j <- which(colnames(des$X) == margin)
      t_obs <- fit$t[j]; b_obs <- fit$b[j]

      ## official cluster-SE p-value (matches published methodology exactly)
      ols <- lm(des$y ~ des$X - 1)
      cr_off <- coeftest(ols, vcov = vcovCL(ols, cluster = des$cl, type = "HC0"))
      p_cluster <- cr_off[j, 4]

      tstar <- wcb_tstar(des$X, des$y, des$cl, margin, W_fam, master_names)
      wcb_p <- mean(abs(tstar) >= abs(t_obs))

      if (!is_index) {
        Tmat_store[, oi] <- abs(tstar)
        t_obs_ind[oi] <- t_obs
        p_cluster_ind[oi] <- p_cluster
      }

      row <- list(family = fam_name, outcome = outcome, margin = margin, coef = b_obs,
                  cluster_se_p = p_cluster, wcb_p = wcb_p, ri_p = NA, rw_p = NA, bh_q = NA,
                  with_controls_wcb_p = NA, n = des$n)

      ## ---- TASK A: sunk margin only -- RI + with-controls ----
      if (margin == "sunk") {
        ## randomization inference: permute "discounted" across the 76 TRUE baseline villages
        if (is.null(PermMat)) {
          PermMat <- t(replicate(B_RI, sample(vil_tab$discounted)))
          colnames(PermMat) <- as.character(vil_tab$cluster_ID)
        }
        des_ri <- des
        keep <- !is.na(des_ri$true_vil)
        Xb <- des_ri$X[keep, , drop = FALSE]
        yb <- des_ri$y[keep]
        clb <- des_ri$cl[keep]
        tv <- as.character(des_ri$true_vil[keep])
        Gb <- length(unique(clb)); adjb <- Gb / (Gb - 1)
        ## fixed "other margins" part of X (everything except the sunk-column family)
        sunk_col <- which(colnames(Xb) == "sunk")
        interact_cols <- grep("^sunk:", colnames(Xb))
        base_cols <- setdiff(seq_len(ncol(Xb)), c(sunk_col, interact_cols))
        Xbase <- Xb[, base_cols, drop = FALSE]
        coef_perm <- numeric(B_RI); t_perm <- numeric(B_RI)
        vil_idx <- match(tv, colnames(PermMat))
        for (bperm in 1:B_RI) {
          disc_perm <- PermMat[bperm, vil_idx]
          sunk_perm <- as.numeric(!disc_perm)
          Xp <- Xb
          Xp[, sunk_col] <- sunk_perm
          for (kk in interact_cols) {
            base_name <- sub("^sunk:", "", colnames(Xb)[kk])
            Xp[, kk] <- sunk_perm * Xb[, base_name]
          }
          XtX_inv_p <- solve(t(Xp) %*% Xp)
          b_p <- as.vector(XtX_inv_p %*% (t(Xp) %*% yb))
          resid_p <- yb - as.vector(Xp %*% b_p)
          S <- rowsum(Xp * resid_p, group = clb)
          meat <- t(S) %*% S
          V <- XtX_inv_p %*% meat %*% XtX_inv_p
          se_p <- sqrt(V[sunk_col, sunk_col] * adjb)
          coef_perm[bperm] <- b_p[sunk_col]
          t_perm[bperm] <- b_p[sunk_col] / se_p
        }
        ri_p_coef <- mean(abs(coef_perm) >= abs(b_obs))
        ri_p_t <- mean(abs(t_perm) >= abs(t_obs))
        row$ri_p <- ri_p_coef
        row$ri_p_t <- ri_p_t
        row$ri_p_diff_flag <- abs(ri_p_coef - ri_p_t) > 0.02

        ## with-controls variant (hh_size, male_head)
        des_c <- build_design(data, outcome, margin, controls = control_vars)
        fit_c <- cr0_fit(des_c$X, des_c$y, des_c$cl)
        jc <- which(colnames(des_c$X) == margin)
        tstar_c <- wcb_tstar(des_c$X, des_c$y, des_c$cl, margin, W_fam, master_names)
        row$with_controls_wcb_p <- mean(abs(tstar_c) >= abs(fit_c$t[jc]))

        ## with-controls RI
        keep_c <- !is.na(des_c$true_vil)
        Xc <- des_c$X[keep_c, , drop = FALSE]
        yc <- des_c$y[keep_c]
        clc <- droplevels(des_c$cl[keep_c])
        tvc <- as.character(des_c$true_vil[keep_c])
        Gc <- length(unique(clc)); adjc <- Gc / (Gc - 1)
        sunk_col_c <- which(colnames(Xc) == "sunk")
        interact_cols_c <- grep("^sunk:", colnames(Xc))
        vil_idx_c <- match(tvc, colnames(PermMat))
        coef_perm_c <- numeric(B_RI)
        for (bperm in 1:B_RI) {
          disc_perm <- PermMat[bperm, vil_idx_c]
          sunk_perm <- as.numeric(!disc_perm)
          Xp <- Xc
          Xp[, sunk_col_c] <- sunk_perm
          for (kk in interact_cols_c) {
            base_name <- sub("^sunk:", "", colnames(Xc)[kk])
            Xp[, kk] <- sunk_perm * Xc[, base_name]
          }
          b_p <- as.vector(solve(t(Xp) %*% Xp) %*% (t(Xp) %*% yc))
          coef_perm_c[bperm] <- b_p[sunk_col_c]
        }
        row$with_controls_ri_p <- mean(abs(coef_perm_c) >= abs(fit_c$b[jc]))
      }

      fam_res[[paste(margin, outcome, sep = "__")]] <- row
    }

    ## RW + BH for individual outcomes in this family x margin
    rw_p <- romano_wolf(Tmat_store, t_obs_ind)
    bh_q <- p.adjust(p_cluster_ind, method = "BH")
    for (oi in seq_len(n_ind)) {
      key <- paste(margin, fam$outcomes[oi], sep = "__")
      fam_res[[key]]$rw_p <- rw_p[oi]
      fam_res[[key]]$bh_q <- bh_q[oi]
    }
  }
  results[[fam_name]] <- fam_res
}

## ---------------------------------------------------------------------
## 9. assemble output table
## ---------------------------------------------------------------------

rows_df <- do.call(rbind, lapply(unlist(results, recursive = FALSE), function(r) {
  data.frame(family = r$family, outcome = r$outcome, margin = r$margin,
             coef = r$coef, cluster_se_p = r$cluster_se_p, wcb_p = r$wcb_p,
             ri_p = ifelse(is.null(r$ri_p), NA, r$ri_p),
             ri_p_t = ifelse(is.null(r$ri_p_t), NA, r$ri_p_t),
             rw_p = ifelse(is.null(r$rw_p), NA, r$rw_p),
             bh_q = ifelse(is.null(r$bh_q), NA, r$bh_q),
             with_controls_wcb_p = ifelse(is.null(r$with_controls_wcb_p), NA, r$with_controls_wcb_p),
             with_controls_ri_p = ifelse(is.null(r$with_controls_ri_p), NA, r$with_controls_ri_p),
             n = r$n, stringsAsFactors = FALSE)
}))
rownames(rows_df) <- NULL

fam_order <- c("Table2_use", "Table3_plan", "Table4_next")
margin_order <- c("sunk", "screening", "signaling")
rows_df <- rows_df[order(match(rows_df$family, fam_order), match(rows_df$margin, margin_order)), ]

out_cols <- c("family", "outcome", "margin", "coef", "cluster_se_p", "wcb_p", "ri_p", "rw_p", "bh_q",
              "with_controls_wcb_p", "with_controls_ri_p")
summary_csv <- rows_df[out_cols]

write.csv(summary_csv, file = paste0(path, "/inference_mht_summary.csv"), row.names = FALSE)

save(results, rows_df, imbalance, control_vars, vil_tab, B_WCB, B_RI,
     file = paste0(path, "/inference_mht.Rdata"))

t1 <- Sys.time()
cat("\n=== DONE ===\n")
cat("Runtime:", round(as.numeric(difftime(t1, t0, units = "mins")), 2), "minutes\n")
cat("B_WCB =", B_WCB, " B_RI =", B_RI, "\n")

## sanity checks
stopifnot(all(summary_csv$cluster_se_p >= 0 & summary_csv$cluster_se_p <= 1, na.rm = TRUE))
stopifnot(all(summary_csv$wcb_p >= 0 & summary_csv$wcb_p <= 1, na.rm = TRUE))
stopifnot(all(summary_csv$ri_p >= 0 & summary_csv$ri_p <= 1, na.rm = TRUE))
stopifnot(all(summary_csv$rw_p >= 0 & summary_csv$rw_p <= 1, na.rm = TRUE))
stopifnot(all(summary_csv$bh_q >= 0 & summary_csv$bh_q <= 1, na.rm = TRUE))
stopifnot(!any(is.nan(as.matrix(summary_csv[sapply(summary_csv, is.numeric)]))))
cat("Sanity checks passed: all p-values in [0,1], no NaNs.\n")

print(summary_csv, digits = 3)
