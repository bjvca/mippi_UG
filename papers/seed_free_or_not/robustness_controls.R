rm(list=ls())
path <- getwd()
library(dplyr)
library(sandwich)
library(lmtest)

## ---------------------------------------------------------------------------
## Robustness check: does the screening (negotiated final-price) coefficient
## survive the addition of baseline wealth/liquidity controls?
##
## This script reproduces the exact main-text specifications from analysis.R
## (the sunk_binary = TRUE / "_01" spec, confirmed against the compiled PDF:
## Table 2 sunk-cost coefficient on the index = -0.103, on "kept harvest
## separate" = -0.122) and re-estimates them with a vector of baseline
## controls added additively. It does NOT modify analysis.R, analysis_nocensored.R,
## the .lyx file, or any existing .Rdata file. All new objects are written to
## res_tab_controls.Rdata and controls_comparison.csv.
##
## IMPORTANT: only baseline.csv variables are used as controls (pre-treatment,
## measured before randomization of P1_pric / discounted / negotiated price).
## Midline/endline variables are never used as controls, since they are
## themselves post-treatment.
## ---------------------------------------------------------------------------

datapath <- paste0(path, "/data")

## functions (copied verbatim from analysis.R so results are directly comparable)
matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  for(j in 1:ncol(x)){
    x[,j] <- (x[,j] - mean(x[sgroup,j],na.rm = T))/sd(x[sgroup,j],na.rm = T)
  }
  return(x)
}

icwIndex <- function(xmat, revcols = NULL, sgroup = rep(TRUE, nrow(xmat))){
  X <- matStand(xmat, sgroup)
  if(length(revcols)>0){
    X[,revcols] <-  -1*X[,revcols]
  }
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  Sx <- cov(X,use = "pairwise.complete.obs")
  weights <- solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)
  index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(X))
  return(list(weights = weights, index = index))
}

trim <- function(var,dataset,trim_perc=.02){
  dataset[var][dataset[var]<quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[1]|dataset[var]>quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]] <- NA
  return(dataset)}

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)}

cr_extract <- function(ols, cluster, coef_row=2) {
  cr <- coeftest(ols, vcov=vcovCL(ols, cluster=cluster, type="HC0"))
  c(cr[coef_row,1], cr[coef_row,2], cr[coef_row,4])
}

## =============================================================================
## STEP 1: replicate the bargaining-game data construction from baseline.csv
## (produces final_price / P1_pric / discounted, i.e. the screening /
## signaling / sunk-cost regressors). Copied from analysis.R lines ~63-181,
## trimmed to what is needed downstream (no balance table, no WTP graph --
## those are untouched by this robustness check).
## =============================================================================
bse <- read.csv(paste(datapath,"baseline.csv",sep="/"))
bse$cluster_ID <- as.factor(paste(paste(bse$distID,bse$subID, sep="_"), bse$vilID, sep="_"))

bse$bid <- ifelse(!is.na(as.numeric(bse$paid.P2_pric_11)),as.numeric(bse$paid.P2_pric_11),
                  ifelse(!is.na(as.numeric(bse$paid.P2_pric_10)),as.numeric(bse$paid.P2_pric_10),
                         ifelse(!is.na(as.numeric(bse$paid.P2_pric_9)),as.numeric(bse$paid.P2_pric_9),
                                ifelse(!is.na(as.numeric(bse$paid.P2_pric_8)),as.numeric(bse$paid.P2_pric_8),
                                       ifelse(!is.na(as.numeric(bse$paid.P2_pric_7)),as.numeric(bse$paid.P2_pric_7),
                                              ifelse(!is.na(as.numeric(bse$paid.P2_pric_6)),as.numeric(bse$paid.P2_pric_6),
                                                     ifelse(!is.na(as.numeric(bse$paid.P2_pric_5)),as.numeric(bse$paid.P2_pric_5),
                                                            ifelse(!is.na(as.numeric(bse$paid.P2_pric_4)),as.numeric(bse$paid.P2_pric_4),
                                                                   ifelse(!is.na(as.numeric(bse$paid.P2_pric_3)),as.numeric(bse$paid.P2_pric_3),
                                                                          ifelse(!is.na(as.numeric(bse$paid.P2_pric_2)),as.numeric(bse$paid.P2_pric_2),
                                                                                 as.numeric(bse$paid.P2_pric)
                                                                          ))))))))))
bse$bid[bse$bid>20000] <- NA

bse$ask <-      ifelse(!is.na(as.numeric(bse$paid.P3_pric_10)),as.numeric(bse$paid.P3_pric_10),
                       ifelse(!is.na(as.numeric(bse$paid.P3_pric_9)),as.numeric(bse$paid.P3_pric_9),
                              ifelse(!is.na(as.numeric(bse$paid.P3_pric_8)),as.numeric(bse$paid.P3_pric_8),
                                     ifelse(!is.na(as.numeric(bse$paid.P3_pric_7)),as.numeric(bse$paid.P3_pric_7),
                                            ifelse(!is.na(as.numeric(bse$paid.P3_pric_6)),as.numeric(bse$paid.P3_pric_6),
                                                   ifelse(!is.na(as.numeric(bse$paid.P3_pric_5)),as.numeric(bse$paid.P3_pric_5),
                                                          ifelse(!is.na(as.numeric(bse$paid.P3_pric_4)),as.numeric(bse$paid.P3_pric_4),
                                                                 ifelse(!is.na(as.numeric(bse$paid.P3_pric_3)),as.numeric(bse$paid.P3_pric_3),
                                                                        ifelse(!is.na(as.numeric(bse$paid.P3_pric_2)),as.numeric(bse$paid.P3_pric_2),
                                                                               ifelse(!is.na(as.numeric(bse$paid.P3_pric)),as.numeric(bse$paid.P3_pric),
                                                                                      as.numeric(bse$P1_pric)
                                                                               ))))))))))
bse$ask[bse$ask>14000] <- NA

bse$rounds <- 1
bse$rounds[ bse$paid.P3_pric!="n/a" ] <- 2
bse$rounds[ bse$paid.P3_pric_2!="n/a" ] <- 3
bse$rounds[ bse$paid.P3_pric_3!="n/a" ] <-  4
bse$rounds[bse$paid.P3_pric_4!="n/a" ] <-  5
bse$rounds[ bse$paid.P3_pric_5!="n/a" ] <-  6
bse$rounds[bse$paid.P3_pric_6!="n/a" ] <-  7
bse$rounds[bse$paid.P3_pric_7!="n/a" ] <-  8
bse$rounds[bse$paid.P3_pric_8!="n/a" ] <-  9
bse$rounds[bse$paid.P3_pric_9!="n/a" ] <-  10
bse$rounds[bse$paid.P3_pric_10!="n/a" ] <-  11
bse$rounds[bse$paid.P3_pric_11!="n/a" ] <-  12

bse$accepts <- "seller"
bse$accepts[bse$paid.start_neg=="Yes"| bse$paid.start_neg_2=="Yes" | bse$paid.start_neg_3=="Yes"
            | bse$paid.start_neg_4=="Yes" | bse$paid.start_neg_5=="Yes" | bse$paid.start_neg_6=="Yes"| bse$paid.start_neg_7=="Yes"
            | bse$paid.start_neg_8=="Yes" | bse$paid.start_neg_9=="Yes" | bse$paid.start_neg_10=="Yes" | bse$paid.start_neg_11=="Yes"] <- "buyer"

bse$final_price <- NA
bse$final_price[bse$accepts=="buyer"] <- bse$ask[bse$accepts=="buyer"]
bse$final_price[bse$accepts=="seller"] <- bse$bid[bse$accepts=="seller"]

## keep only those that participated in the bargaining experiment (mirrors analysis.R)
bse <- subset(bse, (cont == FALSE | trial_P == FALSE) & ( paid_pac == TRUE | discounted == TRUE))

## =============================================================================
## STEP 2: baseline wealth/liquidity control vector.
## Constructed from the FULL baseline.csv (all 2,319 households, before any
## experiment-specific subsetting) so it can be merged onto any of the three
## regression samples (midline-trial/plan, endline) by farmer ID, exactly as
## analysis.R merges P1_pric/final_price.
##
## Controls: household size, distance to agro-dealer, plot size, total land,
## number of rooms, food reserves, group/association membership, and a
## household expenditure proxy (row-sum of the 14 weekly consumption items,
## value.*_value_sp).
##
## Missingness handling: baseline.csv uses "999" (numeric) and "98" (categorical)
## as not-applicable/don't-know sentinels. For continuous controls with any
## missingness we mean-impute (full-baseline-sample mean) and add a companion
## missing-indicator, so that adding controls does not itself drop additional
## observations. Variables with zero missingness (hh_size, rooms, fd_res) are
## used as-is with no missing dummy.
## =============================================================================
bse_full <- read.csv(paste(datapath,"baseline.csv",sep="/"))

bse_full$hh_size   <- as.numeric(as.character(bse_full$hh_size))
bse_full$dist_ag   <- as.numeric(as.character(bse_full$dist_ag));  bse_full$dist_ag[bse_full$dist_ag==999]   <- NA
bse_full$acre_rand <- as.numeric(as.character(bse_full$plot_size)); bse_full$acre_rand[bse_full$acre_rand==999] <- NA
bse_full$ttl_land  <- as.numeric(as.character(bse_full$ttl_land)); bse_full$ttl_land[bse_full$ttl_land==999]  <- NA
bse_full$rooms     <- as.numeric(as.character(bse_full$rooms))
bse_full$fd_res_bin <- as.numeric(bse_full$fd_res=="Yes")
bse_full$membership_bin <- ifelse(bse_full$membership %in% c("Yes","No"), as.numeric(bse_full$membership=="Yes"), NA)

## household expenditure proxy: row-sum of 14 weekly consumption items
## ("n/a" entries treated as 0 spent on that item that week; an observation is
## only NA on the total if literally all 14 items are missing)
value_cols <- grep("^value\\..*_value_sp$", names(bse_full), value=TRUE)
stopifnot(length(value_cols)==14)
val_mat <- sapply(value_cols, function(v) as.numeric(as.character(bse_full[[v]])))
all_missing <- rowSums(!is.na(val_mat))==0
bse_full$hh_expenditure <- rowSums(val_mat, na.rm=TRUE)
bse_full$hh_expenditure[all_missing] <- NA

## mean-impute + missing dummy for variables with any missingness
mean_impute <- function(df, var) {
  miss <- is.na(df[[var]])
  df[[paste0(var,"_miss")]] <- as.numeric(miss)
  df[[var]][miss] <- mean(df[[var]], na.rm=TRUE)
  df
}
for (v in c("dist_ag","acre_rand","ttl_land","membership_bin","hh_expenditure")) {
  bse_full <- mean_impute(bse_full, v)
}

control_vars <- c("hh_size","dist_ag","acre_rand","ttl_land","rooms","fd_res_bin","membership_bin","hh_expenditure",
                   "dist_ag_miss","acre_rand_miss","ttl_land_miss","membership_bin_miss","hh_expenditure_miss")

controls_df <- bse_full[, c("farmer_ID", control_vars)]
controls_rhs <- paste(control_vars, collapse=" + ")

cat("=== Baseline control variables (from baseline.csv, all pre-treatment) ===\n")
print(control_vars)

## =============================================================================
## generic estimator: runs the three margin regressions (screening, sunk,
## signaling) for a vector of outcomes, with and without a controls RHS string
## added additively, and returns arrays parallel in structure to analysis.R's
## res_tab objects (dim1: mean/SE/p-value, dim2: mean|screening|sunk|signaling|N,
## dim3: outcome).
## =============================================================================
run_family <- function(outcomes, data, controls_rhs) {
  res_nc <- array(NA, dim=c(3,5,length(outcomes)))
  res_wc <- array(NA, dim=c(3,5,length(outcomes)))
  res_nc[1,1,] <- colMeans(data[outcomes], na.rm=TRUE)
  res_nc[2,1,] <- apply(data[outcomes], 2, sd, na.rm=TRUE)
  res_wc[1,1,] <- res_nc[1,1,]
  res_wc[2,1,] <- res_nc[2,1,]
  for (i in seq_along(outcomes)) {
    base_rhs <- "screening*d_sunk*d_signaling"
    sunk_rhs <- "sunk*d_screening*d_signaling"
    sig_rhs  <- "signaling*d_screening*d_sunk"

    ols <- lm(as.formula(paste(outcomes[i], base_rhs, sep="~")), data=data)
    cr <- cr_extract(ols, data$cluster_ID); res_nc[1,2,i] <- cr[1]; res_nc[2,2,i] <- cr[2]; res_nc[3,2,i] <- cr[3]
    ols <- lm(as.formula(paste(outcomes[i], sunk_rhs, sep="~")), data=data)
    cr <- cr_extract(ols, data$cluster_ID); res_nc[1,3,i] <- cr[1]; res_nc[2,3,i] <- cr[2]; res_nc[3,3,i] <- cr[3]
    ols <- lm(as.formula(paste(outcomes[i], sig_rhs, sep="~")), data=data)
    cr <- cr_extract(ols, data$cluster_ID); res_nc[1,4,i] <- cr[1]; res_nc[2,4,i] <- cr[2]; res_nc[3,4,i] <- cr[3]
    res_nc[1,5,i] <- nobs(ols)

    ols <- lm(as.formula(paste(outcomes[i], paste(base_rhs, controls_rhs, sep=" + "), sep="~")), data=data)
    cr <- cr_extract(ols, data$cluster_ID); res_wc[1,2,i] <- cr[1]; res_wc[2,2,i] <- cr[2]; res_wc[3,2,i] <- cr[3]
    ols <- lm(as.formula(paste(outcomes[i], paste(sunk_rhs, controls_rhs, sep=" + "), sep="~")), data=data)
    cr <- cr_extract(ols, data$cluster_ID); res_wc[1,3,i] <- cr[1]; res_wc[2,3,i] <- cr[2]; res_wc[3,3,i] <- cr[3]
    ols <- lm(as.formula(paste(outcomes[i], paste(sig_rhs, controls_rhs, sep=" + "), sep="~")), data=data)
    cr <- cr_extract(ols, data$cluster_ID); res_wc[1,4,i] <- cr[1]; res_wc[2,4,i] <- cr[2]; res_wc[3,4,i] <- cr[3]
    res_wc[1,5,i] <- nobs(ols)
  }
  list(nocontrols=round(res_nc,3), controls=round(res_wc,3), outcomes=outcomes)
}

## =============================================================================
## STEP 3: midline data (Table 2 "res_tab" and Table 3 "res_tab_plan")
## Copied from analysis.R, sunk_binary = TRUE branch only (confirmed main-text
## spec), with baseline controls merged in.
## =============================================================================
dta <- read.csv(paste(datapath,"midline.csv", sep="/"))
dta <- merge(dta, bse[c("farmer_ID","P1_pric","final_price")], by.x="ID", by.y="farmer_ID", all.x=TRUE)
dta <- merge(dta, controls_df, by.x="ID", by.y="farmer_ID", all.x=TRUE)
dta$cluster_ID <- as.factor(paste(paste(dta$dist_ID,dta$sub_ID, sep="_"), dta$vil_ID, sep="_"))
dta$used_TP[dta$used_TP=="n/a"] <- NA
dta$used_TP <- dta$used_TP == "Yes"
dta$TP_separate[dta$TP_separate=="n/a"] <- NA
dta$TP_separate <- dta$TP_separate == 1

dta$space[dta$space == "n/a"] <- NA
dta$space[dta$space == "98"] <- NA
dta$seed_no[dta$seed_no == "n/a"] <- NA
dta$cor_plant <- (dta$space=="2" & dta$seed_no=="2") | (dta$space=="3" & dta$seed_no=="1")

dta$dap_app[dta$dap_app == "n/a"] <- NA
dta$ure_app[dta$ure_app == "n/a"] <- NA
dta$use_fert_inorg <-  dta$dap_app== "Yes" | dta$ure_app== "Yes"
dta$org_app[dta$org_app == "n/a"] <- NA
dta$use_fert_org <-  dta$org_app== "Yes"
dta$use_fert <- dta$use_fert_inorg | dta$use_fert_org
dta$cide_use[dta$cide_use == "n/a"] <- NA
dta$use_chem <-  dta$cide_use== "Yes"

dta$sep_post_harvest[dta$sep_post_harvest == "n/a"] <- NA
dta$sep_post_harvest <- dta$sep_post_harvest=="Yes"

dta$who_used[dta$who_used == "n/a"] <- NA
dta$who_used <- dta$who_used == "1"

dta$plan_imp <- (dta$seed_nxt == 1 |  dta$seed_nxt == 2)
dta$plan_bazooka <- dta$imp_var.Bazooka == "True"
dta$plan_bought <- dta$buy_plan=="Yes"
dta$plan_area <-  as.numeric(as.character(dta$area_plan))
dta$plan_area[dta$plan_area > 50] <- NA

dta_reg_mid <- subset(dta,!trial_P)

dta_reg_mid$screening <- (as.numeric(as.character(dta_reg_mid$final_price)))/1000
dta_reg_mid$signaling <- (as.numeric(as.character(dta_reg_mid$P1_pric)))/1000
dta_reg_mid$sunk <- as.numeric(!dta_reg_mid$discounted)   # sunk_binary = TRUE, matches main text

dta_reg_mid$d_screening <- dta_reg_mid$screening - mean(dta_reg_mid$screening, na.rm=TRUE)
dta_reg_mid$d_signaling <- dta_reg_mid$signaling - mean(dta_reg_mid$signaling, na.rm=TRUE)
dta_reg_mid$d_sunk <- dta_reg_mid$sunk - mean(dta_reg_mid$sunk, na.rm=TRUE)

## ---- Table 2 family: trial pack use/management ----
outcomes_use <- c("used_TP","TP_separate","who_used","sep_post_harvest","cor_plant","use_fert","use_chem")
dta_reg_mid$index_use <- icwIndex(xmat=dta_reg_mid[outcomes_use])$index
outcomes_use <- c(outcomes_use,"index_use")
fam_use <- run_family(outcomes_use, dta_reg_mid, controls_rhs)

## ---- Table 3 family: intentions ----
outcomes_plan <- c("plan_imp","plan_bazooka","plan_area","plan_bought")
dta_reg_mid$index_plan <- icwIndex(xmat=dta_reg_mid[outcomes_plan])$index
outcomes_plan <- c(outcomes_plan,"index_plan")
fam_plan <- run_family(outcomes_plan, dta_reg_mid, controls_rhs)

miss_flag_cols <- grep("_miss$", control_vars, value=TRUE)
n_mid_total <- nrow(dta_reg_mid)
n_mid_any_imputed <- sum(rowSums(dta_reg_mid[miss_flag_cols], na.rm=TRUE) > 0)
cat("\nMidline dta_reg N =", n_mid_total,
    "; N with at least one control mean-imputed =", n_mid_any_imputed,
    "; N with all controls fully observed =", n_mid_total - n_mid_any_imputed, "\n")

## =============================================================================
## STEP 4: endline data (Table 4 "res_tab_next_season")
## Copied from analysis.R, sunk_binary = TRUE branch, with baseline controls
## merged in as above.
## =============================================================================
dta <- read.csv(paste(datapath,"endline.csv", sep="/"))
dta <- subset(dta, cont == FALSE & (trial_P== TRUE | paid_pac == TRUE | discounted == TRUE))

bse_full_tmp <- read.csv(paste(datapath,"baseline.csv",sep="/"))
bse_full_tmp$cluster_ID <- as.factor(paste(paste(bse_full_tmp$distID,bse_full_tmp$subID, sep="_"), bse_full_tmp$vilID, sep="_"))
dta <- merge(dta, bse_full_tmp[c("farmer_ID","cluster_ID")], by.x="ID", by.y="farmer_ID", all.x=TRUE)
dta <- merge(dta, bse[c("farmer_ID","P1_pric","final_price")], by.x="ID", by.y="farmer_ID", all.x=TRUE)
dta <- merge(dta, controls_df, by.x="ID", by.y="farmer_ID", all.x=TRUE)
rm(bse_full_tmp)
dta <- subset(dta, !is.na(cluster_ID))

num_plots <- max(as.numeric(dta$plot_count), na.rm=TRUE)
dta$rnd_num <- as.numeric(dta$plot_select)
dta_sub <- subset(dta,!is.na(rnd_num))
dta_sub <- dta_sub %>% rowwise() %>%
  mutate(times_recycled_selected = get(paste0("plot.", rnd_num, "..plot_times_rec"))) %>% as.data.frame()
dta_sub <- dta_sub %>% rowwise() %>%
  mutate(single_source_selected = get(paste0("plot.", rnd_num, "..single_source"))) %>% as.data.frame()
dta_sub <- dta_sub %>% rowwise() %>%
  mutate(recycled_source_selected = get(paste0("plot.", rnd_num, "..recycle_source_rest"))) %>% as.data.frame()
dta <- merge(dta, dta_sub[c("ID","times_recycled_selected","single_source_selected","recycled_source_selected")], by.x="ID", by.y="ID", all.x=TRUE)

dta$rnd_adopt <-   (((dta$maize_var_selected %in%
                        c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta$times_recycled_selected %in% 1) & (dta$single_source_selected %in% letters[4:9])  ) |
                      ((dta$maize_var_selected  %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3","other_opv")) & (dta$times_recycled_selected %in% 1:4) &  (((dta$single_source_selected %in% letters[4:9])) | (dta$recycled_source_selected %in% letters[4:9]))))
dta$rnd_adopt[dta$no_grow] <- NA
dta$rnd_adopt[dta$maize_var_selected == "n/a"] <- NA

dta$rnd_bazo <-  ((dta$maize_var_selected == "Bazooka") & (dta$single_source_selected %in% letters[4:9]  & (dta$times_recycled_selected %in% 1)))
dta$rnd_bazo[dta$no_grow] <- NA
dta$rnd_bazo[dta$maize_var_selected =="n/a"] <- NA

dta$bag_harv[dta$bag_harv == "999"] <- NA
dta$production <- as.numeric(as.character(dta$bag_harv))*as.numeric(as.character(dta$bag_kg))
dta <- trim("production", dta)
dta$productivity <- dta$production/dta$size_selected
dta <- trim("productivity", dta)
dta$production_ihs <- ihs(dta$production)
dta$productivity_ihs <- ihs(dta$productivity)

dta_reg_end <- subset(dta,!trial_P)

dta_reg_end$screening <- (as.numeric(as.character(dta_reg_end$final_price)))/1000
dta_reg_end$signaling <- (as.numeric(as.character(dta_reg_end$P1_pric)))/1000
dta_reg_end$sunk <- as.numeric(!dta_reg_end$discounted)   # sunk_binary = TRUE, matches main text

dta_reg_end$d_screening <- dta_reg_end$screening - mean(dta_reg_end$screening, na.rm=TRUE)
dta_reg_end$d_signaling <- dta_reg_end$signaling - mean(dta_reg_end$signaling, na.rm=TRUE)
dta_reg_end$d_sunk <- dta_reg_end$sunk - mean(dta_reg_end$sunk, na.rm=TRUE)

dta_reg_end$org_ap[dta_reg_end$org_ap == "n/a"] <- NA
dta_reg_end$dap_ap[dta_reg_end$dap_ap == "n/a" | dta_reg_end$dap_ap == "98"] <- NA
dta_reg_end$ur_ap[dta_reg_end$ur_ap == "n/a" | dta_reg_end$ur_ap == "98"] <- NA
dta_reg_end$pest_ap[dta_reg_end$pest_ap == "n/a" | dta_reg_end$pest_ap == "98"] <- NA
dta_reg_end$use_fert_end <- (dta_reg_end$dap_ap == "Yes") | (dta_reg_end$ur_ap == "Yes") | (dta_reg_end$org_ap == "Yes")
dta_reg_end$use_chem_end <- dta_reg_end$pest_ap == "Yes"

outcomes_next <- c("rnd_adopt", "rnd_bazo","use_fert_end", "use_chem_end", "production_ihs", "productivity_ihs")
dta_reg_end$index_next_season <- icwIndex(xmat=dta_reg_end[outcomes_next])$index
outcomes_next <- c(outcomes_next,"index_next_season")

fam_next <- run_family(outcomes_next, dta_reg_end, controls_rhs)

n_end_total <- nrow(dta_reg_end)
n_end_any_imputed <- sum(rowSums(dta_reg_end[miss_flag_cols], na.rm=TRUE) > 0)
cat("Endline dta_reg N =", n_end_total,
    "; N with at least one control mean-imputed =", n_end_any_imputed,
    "; N with all controls fully observed =", n_end_total - n_end_any_imputed, "\n")

## =============================================================================
## STEP 5: kitchen-sink check on the screening margin only, index outcomes.
## Candidate pool = the additional baseline characteristics used in the paper's
## own balance table (age, education, gender, quality perception, promotion
## exposure, source, frequency, yield -- computed on the bargaining-game
## analysis sample, as in analysis.R's outcomes_bal). Ranked by |correlation|
## with final_price (the screening regressor before dividing by 1000); the
## five most correlated (beyond the baseline control vector) are added on top
## of the baseline controls, for each of the three index outcomes.
## =============================================================================
bse$age_head <- as.numeric(as.character(bse$age)); bse$age_head[bse$age_head==999] <- NA
bse$prim_head <- as.numeric(bse$edu %in% c("c","d","e","f"))
bse$male_head <- as.numeric(bse$gender == "Male")
bse$quality_use <- as.numeric(bse$quality_use=="Yes")
bse$promo_use_rand <- as.numeric(bse$maize_var=="Bazooka")
bse$source_rand <- as.numeric(bse$source %in% letters[seq(4,9)])
bse$often_rand <- as.numeric(bse$often %in% letters[seq(1,5)])
bse$bag_harv_ks <- bse$bag_harv; bse$bag_harv_ks[bse$bag_harv_ks == "999"] <- NA
bse$prod_rand <- as.numeric(as.character(bse$bag_harv_ks))*as.numeric(as.character(bse$bag_kg))
bse$acre_rand_ks <- as.numeric(as.character(bse$plot_size)); bse$acre_rand_ks[bse$acre_rand_ks==999] <- NA
bse$yield_rand <- bse$prod_rand/bse$acre_rand_ks
bse <- trim("yield_rand", bse, trim_perc=.01)
bse$final_price_num <- as.numeric(as.character(bse$final_price))

candidate_vars <- c("age_head","prim_head","male_head","quality_use","promo_use_rand","source_rand","often_rand","yield_rand")
corrs <- sapply(candidate_vars, function(v) suppressWarnings(cor(bse[[v]], bse$final_price_num, use="pairwise.complete.obs")))
corrs_sorted <- sort(abs(corrs), decreasing=TRUE)
cat("\n=== Correlation of candidate baseline covariates with final_price (bargaining sample) ===\n")
print(round(corrs[names(corrs_sorted)], 3))

kitchen_extra <- head(names(corrs_sorted), 5)
cat("\nKitchen-sink additional covariates (top 5 by |corr| with final_price, beyond baseline controls):\n")
print(kitchen_extra)

extra_df <- bse[, c("farmer_ID", kitchen_extra)]
for (v in kitchen_extra) {
  miss <- is.na(extra_df[[v]])
  if (any(miss)) {
    extra_df[[paste0(v,"_miss")]] <- as.numeric(miss)
    extra_df[[v]][miss] <- mean(extra_df[[v]], na.rm=TRUE)
  }
}
kitchen_extra_full <- setdiff(names(extra_df), "farmer_ID")
kitchen_rhs <- paste(c(control_vars, kitchen_extra_full), collapse=" + ")

attach_extra <- function(data) merge(data, extra_df, by.x="ID", by.y="farmer_ID", all.x=TRUE)

dta_use_ks  <- attach_extra(dta_reg_mid)
dta_next_ks <- attach_extra(dta_reg_end)

screen_ks <- function(outcome, data, rhs) {
  ols0 <- lm(as.formula(paste(outcome, "screening*d_sunk*d_signaling", sep="~")), data=data)
  cr0 <- cr_extract(ols0, data$cluster_ID)
  ols1 <- lm(as.formula(paste(outcome, paste("screening*d_sunk*d_signaling", rhs, sep=" + "), sep="~")), data=data)
  cr1 <- cr_extract(ols1, data$cluster_ID)
  data.frame(outcome=outcome, coef_baseline_controls=cr0[1], se_baseline_controls=cr0[2],
             coef_kitchen_sink=cr1[1], se_kitchen_sink=cr1[2], N=nobs(ols1))
}

ks_use  <- screen_ks("index_use",  dta_use_ks,  kitchen_rhs)
ks_plan <- screen_ks("index_plan", dta_use_ks,  kitchen_rhs)
ks_next <- screen_ks("index_next_season", dta_next_ks, kitchen_rhs)
## re-run the "baseline controls only" column with the SAME (kitchen-sink) sample
## restriction so coef_baseline_controls above is comparable N-for-N; the
## reported "controls" column from fam_* uses the full baseline-control sample.
ks_use$coef_baseline_controls  <- fam_use$controls[1,2,which(outcomes_use=="index_use")]
ks_plan$coef_baseline_controls <- fam_plan$controls[1,2,which(outcomes_plan=="index_plan")]
ks_next$coef_baseline_controls <- fam_next$controls[1,2,which(outcomes_next=="index_next_season")]
ks_use$se_baseline_controls  <- fam_use$controls[2,2,which(outcomes_use=="index_use")]
ks_plan$se_baseline_controls <- fam_plan$controls[2,2,which(outcomes_plan=="index_plan")]
ks_next$se_baseline_controls <- fam_next$controls[2,2,which(outcomes_next=="index_next_season")]

kitchen_sink_tab <- rbind(ks_use, ks_plan, ks_next)
cat("\n=== Kitchen-sink check: screening coefficient on index outcomes ===\n")
print(kitchen_sink_tab)

## =============================================================================
## STEP 6: save outputs
## =============================================================================
res_tab_controls       <- list(nocontrols=fam_use$nocontrols,  controls=fam_use$controls,  outcomes=fam_use$outcomes)
res_tab_plan_controls  <- list(nocontrols=fam_plan$nocontrols, controls=fam_plan$controls, outcomes=fam_plan$outcomes)
res_tab_next_season_controls <- list(nocontrols=fam_next$nocontrols, controls=fam_next$controls, outcomes=fam_next$outcomes)

save(res_tab_controls, res_tab_plan_controls, res_tab_next_season_controls,
     kitchen_sink_tab, control_vars, kitchen_extra,
     n_mid_total, n_mid_any_imputed, n_end_total, n_end_any_imputed,
     file=paste0(path,"/res_tab_controls.Rdata"))

## human-readable long-format comparison table: family x outcome x margin, no-controls vs controls
margin_names <- c("mean","screening","sunk","signaling")
build_long <- function(fam, family_label) {
  outcomes <- fam$outcomes
  out <- do.call(rbind, lapply(seq_along(outcomes), function(i) {
    do.call(rbind, lapply(2:4, function(col) {
      data.frame(
        family = family_label,
        outcome = outcomes[i],
        margin = margin_names[col],
        coef_nocontrols = fam$nocontrols[1,col,i],
        se_nocontrols   = fam$nocontrols[2,col,i],
        p_nocontrols    = fam$nocontrols[3,col,i],
        coef_controls   = fam$controls[1,col,i],
        se_controls     = fam$controls[2,col,i],
        p_controls      = fam$controls[3,col,i],
        N_nocontrols    = fam$nocontrols[1,5,i],
        N_controls      = fam$controls[1,5,i]
      )
    }))
  }))
  out
}

comparison <- rbind(
  build_long(fam_use,  "Table2_use"),
  build_long(fam_plan, "Table3_plan"),
  build_long(fam_next, "Table4_next_season")
)
write.csv(comparison, file=paste0(path,"/controls_comparison.csv"), row.names=FALSE)

cat("\nDone. Wrote res_tab_controls.Rdata and controls_comparison.csv to", path, "\n")
