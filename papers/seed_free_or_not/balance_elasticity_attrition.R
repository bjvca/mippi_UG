rm(list=ls())
path <- getwd()
datapath <- paste0(path, "/data")

## Packages: sandwich/lmtest for cluster-robust (CR0) SEs (consistent with analysis.R),
## car for joint (multi-coefficient) Wald/F tests under a cluster-robust vcov.
library(car)
library(sandwich)
library(lmtest)

set.seed(20260927)  # reproducibility for the cluster bootstrap in Task B

## ---------------------------------------------------------------------
## 0. DATA CONSTRUCTION -- reused verbatim from analysis.R (do not re-derive)
## ---------------------------------------------------------------------

bse <- read.csv(paste(datapath, "baseline.csv", sep="/"))
bse$cluster_ID <- as.factor(paste(paste(bse$distID, bse$subID, sep="_"), bse$vilID, sep="_"))

## bid (buyer's final offer) -- analysis.R lines ~66-77
bse$bid <- ifelse(!is.na(as.numeric(bse$paid.P2_pric_11)), as.numeric(bse$paid.P2_pric_11),
           ifelse(!is.na(as.numeric(bse$paid.P2_pric_10)), as.numeric(bse$paid.P2_pric_10),
           ifelse(!is.na(as.numeric(bse$paid.P2_pric_9)),  as.numeric(bse$paid.P2_pric_9),
           ifelse(!is.na(as.numeric(bse$paid.P2_pric_8)),  as.numeric(bse$paid.P2_pric_8),
           ifelse(!is.na(as.numeric(bse$paid.P2_pric_7)),  as.numeric(bse$paid.P2_pric_7),
           ifelse(!is.na(as.numeric(bse$paid.P2_pric_6)),  as.numeric(bse$paid.P2_pric_6),
           ifelse(!is.na(as.numeric(bse$paid.P2_pric_5)),  as.numeric(bse$paid.P2_pric_5),
           ifelse(!is.na(as.numeric(bse$paid.P2_pric_4)),  as.numeric(bse$paid.P2_pric_4),
           ifelse(!is.na(as.numeric(bse$paid.P2_pric_3)),  as.numeric(bse$paid.P2_pric_3),
           ifelse(!is.na(as.numeric(bse$paid.P2_pric_2)),  as.numeric(bse$paid.P2_pric_2),
                  as.numeric(bse$paid.P2_pric)
           ))))))))))
bse$bid[bse$bid>20000] <- NA   # this is where the one bid=50000 data-entry error is dropped (Task B note)

## ask (seller's final offer) -- analysis.R lines ~79-93
bse$ask <-      ifelse(!is.na(as.numeric(bse$paid.P3_pric_10)), as.numeric(bse$paid.P3_pric_10),
                ifelse(!is.na(as.numeric(bse$paid.P3_pric_9)),  as.numeric(bse$paid.P3_pric_9),
                ifelse(!is.na(as.numeric(bse$paid.P3_pric_8)),  as.numeric(bse$paid.P3_pric_8),
                ifelse(!is.na(as.numeric(bse$paid.P3_pric_7)),  as.numeric(bse$paid.P3_pric_7),
                ifelse(!is.na(as.numeric(bse$paid.P3_pric_6)),  as.numeric(bse$paid.P3_pric_6),
                ifelse(!is.na(as.numeric(bse$paid.P3_pric_5)),  as.numeric(bse$paid.P3_pric_5),
                ifelse(!is.na(as.numeric(bse$paid.P3_pric_4)),  as.numeric(bse$paid.P3_pric_4),
                ifelse(!is.na(as.numeric(bse$paid.P3_pric_3)),  as.numeric(bse$paid.P3_pric_3),
                ifelse(!is.na(as.numeric(bse$paid.P3_pric_2)),  as.numeric(bse$paid.P3_pric_2),
                ifelse(!is.na(as.numeric(bse$paid.P3_pric)),    as.numeric(bse$paid.P3_pric),
                       as.numeric(bse$P1_pric)
                ))))))))))
bse$ask[bse$ask>14000] <- NA

## who accepts -> negotiated (final) price -- analysis.R lines ~118-135
bse$accepts <- "seller"
bse$accepts[bse$paid.start_neg=="Yes" | bse$paid.start_neg_2=="Yes" | bse$paid.start_neg_3=="Yes"
          | bse$paid.start_neg_4=="Yes" | bse$paid.start_neg_5=="Yes" | bse$paid.start_neg_6=="Yes"
          | bse$paid.start_neg_7=="Yes" | bse$paid.start_neg_8=="Yes" | bse$paid.start_neg_9=="Yes"
          | bse$paid.start_neg_10=="Yes" | bse$paid.start_neg_11=="Yes"] <- "buyer"

bse$final_price <- NA
bse$final_price[bse$accepts=="buyer"]  <- bse$ask[bse$accepts=="buyer"]
bse$final_price[bse$accepts=="seller"] <- bse$bid[bse$accepts=="seller"]

## restrict to the 759-farmer bargaining sample -- analysis.R line ~146
bse <- subset(bse, (cont == FALSE | trial_P == FALSE) & (paid_pac == TRUE | discounted == TRUE))

## balance covariates -- analysis.R lines ~149-166
bse$age_head <- as.numeric(as.character(bse$age))
bse$age_head[bse$age_head==999] <- NA
bse$prim_head <- bse$edu %in% c("c","d","e","f")
bse$male_head <- bse$gender == "Male"
bse$hh_size <- as.numeric(as.character(bse$hh_size))
bse$dist_ag <- as.numeric(as.character(bse$dist_ag))
bse$dist_ag[bse$dist_ag==999] <- NA
bse$quality_use <- bse$quality_use=="Yes"
bse$promo_use_rand <- bse$maize_var=="Bazooka"
bse$source_rand <- bse$source %in% letters[seq(from=4, to=9)]
bse$often_rand <- bse$often %in% letters[seq(from=1, to=5)]
bse$bag_harv[bse$bag_harv == "999"] <- NA
bse$prod_rand <- as.numeric(as.character(bse$bag_harv)) * as.numeric(as.character(bse$bag_kg))
bse$acre_rand <- as.numeric(as.character(bse$plot_size))
bse$acre_rand[bse$acre_rand==999] <- NA
bse$yield_rand <- bse$prod_rand / bse$acre_rand

trim <- function(var, dataset, trim_perc=.02) {
  dataset[var][dataset[var] < quantile(dataset[var], c(trim_perc/2, 1-(trim_perc/2)), na.rm=T)[1] |
               dataset[var] > quantile(dataset[var], c(trim_perc/2, 1-(trim_perc/2)), na.rm=T)[2]] <- NA
  return(dataset)
}
bse <- trim("yield_rand", bse, trim_perc=.01)

## bse is the 759-farmer bargaining sample (== bse_reg in analysis.R, since trial_P is FALSE
## throughout this subsample: verified n=759 with discounted 380/379, P1_pric 223/154/155/227)
stopifnot(nrow(bse) == 759)

outcomes_bal <- c("age_head","prim_head","male_head","hh_size","dist_ag","quality_use",
                   "promo_use_rand","source_rand","often_rand","acre_rand","yield_rand")

n_villages <- length(unique(bse$cluster_ID))

cat("N bargaining sample:", nrow(bse), " | N villages:", n_villages, "\n")

## ---------------------------------------------------------------------
## TASK A: BALANCE FOR BOTH RANDOMIZATIONS + JOINT F-TESTS
## ---------------------------------------------------------------------

## --- A1. Discount randomization: replicate the paper's balance table -------
## dim1: estimate/SE/p ; dim2: overall,control(non-discounted),treated(discounted),diff,N ; dim3: outcome
bal_tab_uga <- array(NA, dim=c(3, 5, length(outcomes_bal)), dimnames=list(
  c("est","se","p"), c("overall","non_discounted","discounted","diff","N"), outcomes_bal))
for (i in seq_along(outcomes_bal)) {
  v <- outcomes_bal[i]
  bal_tab_uga["est","overall",v]        <- mean(bse[[v]], na.rm=TRUE)
  bal_tab_uga["se","overall",v]         <- sd(bse[[v]], na.rm=TRUE)
  bal_tab_uga["est","non_discounted",v] <- mean(bse[[v]][!bse$discounted], na.rm=TRUE)
  bal_tab_uga["se","non_discounted",v]  <- sd(bse[[v]][!bse$discounted], na.rm=TRUE)
  bal_tab_uga["est","discounted",v]     <- mean(bse[[v]][bse$discounted==TRUE], na.rm=TRUE)
  bal_tab_uga["se","discounted",v]      <- sd(bse[[v]][bse$discounted==TRUE], na.rm=TRUE)
  ols <- lm(as.formula(paste(v, "discounted", sep="~")), data=bse)
  cr  <- coeftest(ols, vcov=vcovCL(ols, cluster=bse$cluster_ID, type="HC0"))
  bal_tab_uga["est","diff",v] <- cr[2,1]
  bal_tab_uga["se","diff",v]  <- cr[2,2]
  bal_tab_uga["p","diff",v]   <- cr[2,4]
  bal_tab_uga["est","N",v]    <- nobs(ols)
}

## --- A2. Offer-price randomization: linear-in-P1 differences ---------------
bse$P1000 <- bse$P1_pric / 1000
bal_tab_offer <- array(NA, dim=c(3, 2, length(outcomes_bal)), dimnames=list(
  c("est","se","p"), c("coef_per_1000UGX","N"), outcomes_bal))
for (i in seq_along(outcomes_bal)) {
  v <- outcomes_bal[i]
  ols <- lm(as.formula(paste(v, "P1000", sep="~")), data=bse)
  cr  <- coeftest(ols, vcov=vcovCL(ols, cluster=bse$cluster_ID, type="HC0"))
  bal_tab_offer["est","coef_per_1000UGX",v] <- cr[2,1]
  bal_tab_offer["se","coef_per_1000UGX",v]  <- cr[2,2]
  bal_tab_offer["p","coef_per_1000UGX",v]   <- cr[2,4]
  bal_tab_offer["est","N",v] <- nobs(ols)
}

## --- A3. Joint (McKenzie-style) orthogonality tests -------------------------
## Regress each randomized treatment on ALL balance covariates jointly, on the
## common complete-case sample, and test joint significance of the covariates
## under a cluster-robust (village) vcov.
cc <- complete.cases(bse[, outcomes_bal])
bse_cc <- bse[cc, ]
n_joint <- sum(cc)

joint_test <- function(treat_var, data) {
  form <- as.formula(paste(treat_var, "~", paste(outcomes_bal, collapse=" + ")))
  ols  <- lm(form, data=data)
  vc   <- vcovCL(ols, cluster=data$cluster_ID, type="HC0")
  cov_coef_names <- setdiff(names(coef(ols)), "(Intercept)")  # handles logical -> "xTRUE" naming
  lh   <- linearHypothesis(ols, cov_coef_names, vcov.=vc, test="F")
  list(F=lh[2,"F"], df1=lh[2,"Df"], df2=lh[2,"Res.Df"], p=lh[2,"Pr(>F)"], N=nobs(ols))
}

jt_discounted <- joint_test("discounted", bse_cc)
jt_offer      <- joint_test("P1_pric",    bse_cc)

bal_joint_tests <- list(
  discounted = jt_discounted,
  offer      = jt_offer,
  N_complete_cases = n_joint
)

cat("\n--- Joint orthogonality tests (covariates -> treatment), N=", n_joint, "villages=",
    length(unique(bse_cc$cluster_ID)), "---\n")
cat("Discount randomization: F(", jt_discounted$df1, ",", jt_discounted$df2, ")=",
    round(jt_discounted$F,3), " p=", round(jt_discounted$p,3), "\n")
cat("Offer-price randomization: F(", jt_offer$df1, ",", jt_offer$df2, ")=",
    round(jt_offer$F,3), " p=", round(jt_offer$p,3), "\n")

save(bal_tab_uga, bal_tab_offer, file=paste(path, "bal_tab_offer.Rdata", sep="/"))
save(bal_joint_tests, file=paste(path, "bal_joint_tests.Rdata", sep="/"))

## --- readable combined CSV --------------------------------------------------
sig_star <- function(p) ifelse(is.na(p), "", ifelse(p<0.01,"**",ifelse(p<0.05,"*",ifelse(p<0.1,"+",""))))
bal_csv <- data.frame(
  covariate            = outcomes_bal,
  mean_non_discounted  = round(bal_tab_uga["est","non_discounted",],3),
  mean_discounted      = round(bal_tab_uga["est","discounted",],3),
  diff_discounted      = round(bal_tab_uga["est","diff",],3),
  p_discounted         = round(bal_tab_uga["p","diff",],3),
  sig_discounted       = sig_star(bal_tab_uga["p","diff",]),
  N_discounted         = bal_tab_uga["est","N",],
  coef_per_1000UGX_offer = round(bal_tab_offer["est","coef_per_1000UGX",],4),
  p_offer                = round(bal_tab_offer["p","coef_per_1000UGX",],3),
  sig_offer               = sig_star(bal_tab_offer["p","coef_per_1000UGX",]),
  N_offer                 = bal_tab_offer["est","N",]
)
bal_csv <- rbind(bal_csv, data.frame(
  covariate="JOINT TEST (all covariates)", mean_non_discounted=NA, mean_discounted=NA,
  diff_discounted=NA, p_discounted=round(jt_discounted$p,3),
  sig_discounted=sig_star(jt_discounted$p), N_discounted=jt_discounted$N,
  coef_per_1000UGX_offer=NA, p_offer=round(jt_offer$p,3),
  sig_offer=sig_star(jt_offer$p), N_offer=jt_offer$N))
write.csv(bal_csv, paste(path, "balance_both_randomizations.csv", sep="/"), row.names=FALSE)

## ---------------------------------------------------------------------
## TASK B: DEMAND ELASTICITY POINT ESTIMATE
## ---------------------------------------------------------------------
## Demand curve is built exactly as in the paper's Figure 1 (analysis.R ~249-302):
## share of farmers with NEGOTIATED (final) price >= p. This is a proxy for WTP,
## not a clean uncompensated demand curve for maize seed -- final_price is the
## outcome of a bargaining game seeded by the randomized opening offer P1_pric,
## so the elasticity below describes how the empirical distribution of
## transacted prices responds to price, not a structurally estimated demand
## elasticity for seed. All numbers refer to NEGOTIATED PRICE (final_price).

bse_dem <- bse
## the paper's exclusions: literal 3000 outlier, and cases where the buyer
## simply accepted the opening offer (P1_pric == final_price, i.e. no genuine
## negotiated outcome was recorded)
bse_dem$final_price[bse_dem$final_price == 3000] <- NA
bse_dem$final_price[bse_dem$final_price == bse_dem$P1_pric] <- NA
## defensive: the 50000 data-entry error is already dropped upstream by the
## bid>20000 cap; this line is a no-op safety net in case that cap is ever relaxed
bse_dem$final_price[bse_dem$final_price > 20000] <- NA

n_dem <- sum(!is.na(bse_dem$final_price))
cat("\nDemand-curve N (after 3000-outlier / P1==final exclusions):", n_dem, "\n")

## empirical survival function S(p) = share with final_price >= p, on a
## village-level dataset so the cluster bootstrap below can resample villages
surv <- function(data, p) mean(data$final_price >= p, na.rm=TRUE)

grid <- seq(3000, 12000, by=1000)
S <- sapply(grid, function(p) surv(bse_dem, p))
names(S) <- grid

## (i) arc elasticity over the randomized offer support 9000 -> 12000
arc_elasticity <- function(data, p_lo=9000, p_hi=12000) {
  Q_lo <- surv(data, p_lo); Q_hi <- surv(data, p_hi)
  dQ <- Q_hi - Q_lo; Qm <- (Q_lo + Q_hi)/2
  dP <- p_hi - p_lo; Pm <- (p_lo + p_hi)/2
  (dQ/Qm) / (dP/Pm)
}
E_arc <- arc_elasticity(bse_dem)

## (ii) point elasticity at the median (7000) and at the mean, using a local
## slope between adjacent 1000-UGX grid points bracketing the target price
point_elasticity <- function(data, p0, step=1000) {
  p_lo <- p0 - step; p_hi <- p0 + step
  Q_lo <- surv(data, p_lo); Q_hi <- surv(data, p_hi)
  slope <- (Q_hi - Q_lo) / (p_hi - p_lo)     # dQ/dP, local
  Q0 <- surv(data, p0)
  slope * (p0 / Q0)
}
median_price <- 7000
mean_price   <- mean(bse_dem$final_price, na.rm=TRUE)
## round the mean to the nearest 1000 so the bracketing grid points are also
## multiples of 1000 (consistent with the local-slope convention used above)
mean_price_grid <- round(mean_price/1000)*1000

E_point_median <- point_elasticity(bse_dem, median_price)
E_point_mean   <- point_elasticity(bse_dem, mean_price_grid)

## IMPORTANT CONSTRUCTION NOTE: P1_pric only ever takes the 4 randomized values
## {9000,10000,11000,12000}. The paper's own exclusion rule (drop rows where
## final_price == P1_pric) therefore strips out exactly the point mass located
## AT each of those four prices before computing the survival function. At the
## right end of the offer support this is fatal: S(12000) is mechanically forced
## to 0 (nothing can be strictly greater than the maximum possible price, and the
## only farmers who ever transacted at exactly 12000 are, by construction, the
## ones just excluded). The arc elasticity over 9000->12000 below is therefore a
## construction artifact, not an economic estimate, and should not be quoted on
## its own. The point elasticities at 7000 (median) and near the mean sit off
## the offer-price grid entirely, so they are NOT touched by this exclusion and
## are the more defensible numbers.

## (iii) cluster bootstrap (villages), 999 reps, 95% CI. We bootstrap BOTH the
## arc elasticity (for transparency, flagged as degenerate above) and the point
## elasticity at the median, which is the number we recommend quoting.
villages <- unique(bse_dem$cluster_ID)
n_boot <- 999
boot_arc <- numeric(n_boot)
boot_point_median <- numeric(n_boot)
for (b in 1:n_boot) {
  samp_v <- sample(villages, length(villages), replace=TRUE)
  boot_rows <- do.call(c, lapply(samp_v, function(v) which(bse_dem$cluster_ID==v)))
  boot_data <- bse_dem[boot_rows, ]
  boot_arc[b] <- arc_elasticity(boot_data)
  boot_point_median[b] <- point_elasticity(boot_data, median_price)
}
ci_arc <- quantile(boot_arc, c(0.025, 0.975), na.rm=TRUE)
ci_point_median <- quantile(boot_point_median, c(0.025, 0.975), na.rm=TRUE)

cat("\nDemand curve S(p):\n"); print(round(S,3))
cat("\nArc elasticity (9000->12000, negotiated price) -- DEGENERATE, see note:", round(E_arc,3),
    " 95% CI:", round(ci_arc[1],3), "-", round(ci_arc[2],3), "\n")
cat("Point elasticity at median price (7000) -- RECOMMENDED HEADLINE:", round(E_point_median,3),
    " 95% CI:", round(ci_point_median[1],3), "-", round(ci_point_median[2],3), "\n")
cat("Point elasticity at mean price (", round(mean_price,0), ", grid ", mean_price_grid, "):",
    round(E_point_mean,3), "\n")

elasticity <- list(
  price_concept = "negotiated (final) transaction price from the bargaining game, not a raw stated-WTP measure",
  N = n_dem,
  survival_function = S,
  arc_elasticity_9000_12000 = E_arc,
  arc_elasticity_ci95 = ci_arc,
  arc_elasticity_note = "degenerate: S(12000) is forced to 0 by the P1==final exclusion, since P1_pric only takes values in {9000,10000,11000,12000}; do not quote on its own",
  point_elasticity_median7000 = E_point_median,
  point_elasticity_median7000_ci95 = ci_point_median,
  point_elasticity_mean = E_point_mean,
  mean_price = mean_price,
  mean_price_grid = mean_price_grid,
  median_price = median_price,
  n_boot = n_boot,
  headline = "point elasticity at the median negotiated price (UGX 7,000); the arc elasticity over 9000-12000 is a construction artifact (see arc_elasticity_note) and should not be used as the headline"
)
save(elasticity, file=paste(path, "elasticity.Rdata", sep="/"))

writeLines(c(
  sprintf("Demand for maize seed is estimated over the distribution of negotiated (final) transaction prices from the bargaining game (N=%d), not a direct WTP elicitation.", n_dem),
  sprintf("The arc elasticity of demand computed over the randomized offer-price support (UGX 9,000 to UGX 12,000) is %.2f, but this number is a construction artifact: because P1_pric only ever takes the values 9,000/10,000/11,000/12,000, the paper's own exclusion of cases where the negotiated price equals the opening offer mechanically forces the survival share at 12,000 to zero, so this arc elasticity should not be quoted on its own.", E_arc),
  sprintf("The recommended headline number is the point elasticity of demand at the median negotiated price (UGX 7,000), computed from the local slope of the empirical survival function between adjacent UGX 1,000 price points (UGX 6,000 and UGX 8,000): %.2f (cluster bootstrap 95%% CI over %d village-resampling reps: %.2f to %.2f).", E_point_median, n_boot, ci_point_median[1], ci_point_median[2]),
  sprintf("For comparison, the analogous point elasticity evaluated near the mean negotiated price (UGX %.0f, nearest UGX 1,000 grid point %d) is %.2f.", mean_price, mean_price_grid, E_point_mean),
  "Recommended phrasing: 'the estimated elasticity of demand for maize seed with respect to the negotiated price, evaluated at the median transacted price of UGX 7,000, is [E_point_median] (95% CI [lo, hi], cluster bootstrap over villages).'"
), con=paste(path, "elasticity_summary.txt", sep="/"))

## ---------------------------------------------------------------------
## TASK C: FORMAL ATTRITION TESTS
## ---------------------------------------------------------------------

## --- midline: dedup 1150 rows to farmer_ID level ---------------------------
mid <- read.csv(paste(datapath, "midline.csv", sep="/"))
mid$consent_yes <- mid$consent == "Yes"
## order rows so that, within an ID, consented interviews come first, then
## take the first row per ID -- this keeps "the first consented interview"
## where one exists, and falls back to the first row otherwise
ord <- order(mid$ID, -mid$consent_yes)
mid_sorted <- mid[ord, ]
dup_ids <- unique(mid_sorted$ID[duplicated(mid_sorted$ID)])
n_dup_ids <- length(dup_ids)
n_dup_rows_dropped <- sum(duplicated(mid_sorted$ID))
mid_dedup <- mid_sorted[!duplicated(mid_sorted$ID), ]
cat("\nMidline: ", nrow(mid), "raw rows,", n_dup_ids, "duplicated farmer_IDs (",
    n_dup_rows_dropped, "extra rows dropped), ", nrow(mid_dedup), "unique IDs after dedup\n")

mid_dedup$resp_mid <- (mid_dedup$frm == "Yes") & (mid_dedup$consent == "Yes")

end <- read.csv(paste(datapath, "endline.csv", sep="/"))
stopifnot(sum(duplicated(end$ID)) == 0)
end$resp_end <- (end$frm == "Yes") & (end$consent == "Yes")

## --- attach response indicators to the 759 bargaining-sample farmers -------
att <- bse[, c("farmer_ID","cluster_ID","discounted","P1_pric","final_price",
               "hh_size","acre_rand","dist_ag")]
att <- merge(att, mid_dedup[, c("ID","resp_mid")], by.x="farmer_ID", by.y="ID", all.x=TRUE)
att <- merge(att, end[, c("ID","resp_end")], by.x="farmer_ID", by.y="ID", all.x=TRUE)
## farmers absent from midline/endline entirely were not interviewed
att$resp_mid[is.na(att$resp_mid)] <- FALSE
att$resp_end[is.na(att$resp_end)] <- FALSE
att$P1000 <- att$P1_pric/1000
att$final1000 <- att$final_price/1000

stopifnot(nrow(att) == 759)

cat("\nResponse rates (N=759):\n")
cat("Midline overall:", round(mean(att$resp_mid),3), " Endline overall:", round(mean(att$resp_end),3), "\n")
cat("Midline by discounted:\n"); print(tapply(att$resp_mid, att$discounted, mean))
cat("Endline by discounted:\n"); print(tapply(att$resp_end, att$discounted, mean))
cat("Midline by P1_pric:\n"); print(tapply(att$resp_mid, att$P1_pric, mean))
cat("Endline by P1_pric:\n"); print(tapply(att$resp_end, att$P1_pric, mean))

## --- regressions: response ~ treatment(s), village-clustered SEs -----------
reg_att <- function(outcome, rhs, data) {
  form <- as.formula(paste(outcome, "~", rhs))
  ols  <- lm(form, data=data)
  cr   <- coeftest(ols, vcov=vcovCL(ols, cluster=data$cluster_ID, type="HC0"))
  list(model=ols, coefs=cr, N=nobs(ols))
}

att_reg <- list(
  mid_discounted = reg_att("resp_mid", "discounted", att),
  mid_P1         = reg_att("resp_mid", "P1000", att),
  mid_final      = reg_att("resp_mid", "final1000", att),
  mid_joint      = reg_att("resp_mid", "discounted + P1000 + final1000", att),
  end_discounted = reg_att("resp_end", "discounted", att),
  end_P1         = reg_att("resp_end", "P1000", att),
  end_final      = reg_att("resp_end", "final1000", att),
  end_joint      = reg_att("resp_end", "discounted + P1000 + final1000", att)
)

cat("\n--- Attrition regressions (cluster-robust, village) ---\n")
for (nm in names(att_reg)) {
  cat("\n", nm, " (N=", att_reg[[nm]]$N, ")\n", sep="")
  print(att_reg[[nm]]$coefs)
}

## --- differential attrition on covariates: response ~ treatment*(hh_size, acre_rand, dist_ag)
## Run for BOTH randomizations, since "treatment" is not unique in this design:
## discounted is the paper's headline manipulation; P1_pric (rescaled) is the
## secondary (signaling/offer-price) randomization.
diff_att_test <- function(outcome, treat, data) {
  cc <- complete.cases(data[, c(outcome, treat, "hh_size","acre_rand","dist_ag","cluster_ID")])
  d  <- data[cc, ]
  form <- as.formula(paste0(outcome, " ~ ", treat, "*hh_size + ", treat, "*acre_rand + ", treat, "*dist_ag"))
  ols <- lm(form, data=d)
  vc  <- vcovCL(ols, cluster=d$cluster_ID, type="HC0")
  int_terms <- grep(":", names(coef(ols)), value=TRUE)
  lh  <- linearHypothesis(ols, int_terms, vcov.=vc, test="F")
  list(F=lh[2,"F"], df1=lh[2,"Df"], df2=lh[2,"Res.Df"], p=lh[2,"Pr(>F)"], N=nobs(ols))
}

diff_attrition <- list(
  mid_discounted = diff_att_test("resp_mid", "discounted", att),
  mid_P1000      = diff_att_test("resp_mid", "P1000", att),
  end_discounted = diff_att_test("resp_end", "discounted", att),
  end_P1000      = diff_att_test("resp_end", "P1000", att)
)

cat("\n--- Differential attrition joint tests (treatment x hh_size/acre_rand/dist_ag) ---\n")
for (nm in names(diff_attrition)) {
  dd <- diff_attrition[[nm]]
  cat(nm, ": F(", dd$df1, ",", dd$df2, ")=", round(dd$F,3), " p=", round(dd$p,3), " N=", dd$N, "\n")
}

save(att, att_reg, diff_attrition, n_dup_ids, n_dup_rows_dropped,
     file=paste(path, "attrition_tests.Rdata", sep="/"))

## --- readable summary CSV ---------------------------------------------------
extract_row <- function(lab, robj, rowname) {
  cr <- robj$coefs
  if (!(rowname %in% rownames(cr))) return(NULL)
  data.frame(spec=lab, coef=rowname, estimate=round(cr[rowname,1],4),
             se=round(cr[rowname,2],4), p=round(cr[rowname,4],4), N=robj$N)
}
rows <- list(
  extract_row("midline ~ discounted",              att_reg$mid_discounted, "discountedTRUE"),
  extract_row("midline ~ P1000",                    att_reg$mid_P1,         "P1000"),
  extract_row("midline ~ final1000",                att_reg$mid_final,      "final1000"),
  extract_row("midline ~ discounted+P1000+final1000 (discounted)", att_reg$mid_joint, "discountedTRUE"),
  extract_row("midline ~ discounted+P1000+final1000 (P1000)",      att_reg$mid_joint, "P1000"),
  extract_row("midline ~ discounted+P1000+final1000 (final1000)",  att_reg$mid_joint, "final1000"),
  extract_row("endline ~ discounted",               att_reg$end_discounted, "discountedTRUE"),
  extract_row("endline ~ P1000",                     att_reg$end_P1,         "P1000"),
  extract_row("endline ~ final1000",                 att_reg$end_final,      "final1000"),
  extract_row("endline ~ discounted+P1000+final1000 (discounted)", att_reg$end_joint, "discountedTRUE"),
  extract_row("endline ~ discounted+P1000+final1000 (P1000)",      att_reg$end_joint, "P1000"),
  extract_row("endline ~ discounted+P1000+final1000 (final1000)",  att_reg$end_joint, "final1000")
)
att_csv <- do.call(rbind, rows[!sapply(rows, is.null)])

diff_csv <- data.frame(
  spec = names(diff_attrition),
  F    = sapply(diff_attrition, function(x) round(x$F,3)),
  df1  = sapply(diff_attrition, function(x) x$df1),
  df2  = sapply(diff_attrition, function(x) x$df2),
  p    = sapply(diff_attrition, function(x) round(x$p,3)),
  N    = sapply(diff_attrition, function(x) x$N)
)

response_rates <- data.frame(
  group = c("overall","non_discounted","discounted", paste0("P1_pric=",sort(unique(att$P1_pric)))),
  resp_mid = c(mean(att$resp_mid), mean(att$resp_mid[!att$discounted]), mean(att$resp_mid[att$discounted]),
               as.numeric(tapply(att$resp_mid, att$P1_pric, mean))),
  resp_end = c(mean(att$resp_end), mean(att$resp_end[!att$discounted]), mean(att$resp_end[att$discounted]),
               as.numeric(tapply(att$resp_end, att$P1_pric, mean))),
  N = c(nrow(att), sum(!att$discounted), sum(att$discounted), as.numeric(table(att$P1_pric)))
)

## attrition_summary.csv holds the response-rate-by-arm table (task deliverable);
## the regression coefficients and the differential-attrition joint tests are
## written to companion CSVs for readability (all three also live in the .Rdata)
write.csv(response_rates, paste(path, "attrition_summary.csv", sep="/"), row.names=FALSE)
write.csv(att_csv, paste(path, "attrition_regressions.csv", sep="/"), row.names=FALSE)
write.csv(diff_csv, paste(path, "attrition_differential_jointtests.csv", sep="/"), row.names=FALSE)

cat("\nDone. Outputs written to:", path, "\n")
