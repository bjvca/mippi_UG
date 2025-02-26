rm(list=ls())
setwd("~/data/projects/OneCG/MIPP/papers/reg_report/analysis")
library(clubSandwich)
library(moments)
path <- getwd()
path <- strsplit(path,"/papers/reg_report/analysis")[[1]]

trim <- function(var,dataset,trim_perc=.02){
  dataset[var][dataset[var]<quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[1]|dataset[var]>quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]] <- NA
  return(dataset)}

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)}

# Function to evaluate skewness and recommend transformation
evaluate_skewness <- function(data) {
  # Ensure the data is numeric
  if (!is.numeric(data)) {
    stop("Input data must be numeric.")
  }
  
  # Calculate skewness
  skew <- skewness(data, na.rm = TRUE)
  
  # Calculate SE of skewness
  n <- length(data[!is.na(data)])  # Exclude NAs for sample size
  se_skew <- sqrt(6 / n)
  
  # Determine critical value
  critical_value <- 2.5 * se_skew
  
  # Check if skewness is significant
  is_significant <- abs(skew) > critical_value
  
  # Recommendation
  recommendation <- if (is_significant) {
    "Data is significantly skewed. Consider transforming."
  } else {
    "Data is not significantly skewed. Transformation is likely unnecessary."
  }
  
  # Return results
  list(
    Skewness = round(skew, 3),
    SE_Skewness = round(se_skew, 3),
    Critical_Value = round(critical_value, 3),
    Is_Significant = is_significant,
    Recommendation = recommendation
  )
}

###

baseline_dta <- read.csv(paste(path,"/baseline/data/public/baseline.csv",sep="/"), stringsAsFactors = TRUE)
#keep only the ones that are in free subgroup
baseline_dta <- subset(baseline_dta,!(paid_pac=="TRUE" | discounted=="TRUE"))

###creat unique village_ID for clustering
baseline_dta$clusters <- paste(paste(baseline_dta$distID,baseline_dta$subID, sep="_"), baseline_dta$vilID, sep="_")
baseline_dta$prim_ed <- (baseline_dta$edu %in% letters[3:6])
baseline_dta$prim_ed[baseline_dta$edu=="g"] <- NA ## not specified
baseline_dta$age <- as.numeric(as.character(baseline_dta$age))
baseline_dta$age[baseline_dta$age==999] <- NA
baseline_dta$HH_male <- (baseline_dta$gender == "Male")
baseline_dta$dist_ag <- as.numeric(as.character(baseline_dta$dist_ag))
baseline_dta$dist_ag[baseline_dta$dist_ag==999] <- NA
sum(is.na(baseline_dta$dist_ag))
baseline_dta <- trim("dist_ag",baseline_dta)
###this has a seriously skewed distribution - try an inverse hyperholic sine transformation
evaluate_skewness(baseline_dta$dist_ag)
baseline_dta$ihs_dist_ag <- ihs(baseline_dta$dist_ag)
evaluate_skewness(baseline_dta$ihs_dist_ag)

baseline_dta$quality_use_any <-  (baseline_dta$quality_use=="Yes")
baseline_dta$quality_use_any[baseline_dta$quality_use=="98"] <- NA 

baseline_dta$baz_rand <- baseline_dta$maize_var=="Bazooka"
baseline_dta$baz_rand[baseline_dta$maize_var=="98"] <- NA

baseline_dta$source_rand <- (baseline_dta$source %in% letters[4:9])
baseline_dta$source_rand[baseline_dta$source == "96"] <- NA

baseline_dta$recycler <- baseline_dta$often %in% c("d","e","f")
baseline_dta$recycler[baseline_dta$often == "98"] <- NA 

baseline_dta$bag_kg[baseline_dta$bag_kg == 999] <- NA
baseline_dta$bag_harv[baseline_dta$bag_harv == 999] <- NA
baseline_dta$plot_size[baseline_dta$plot_size <.1] <- NA

baseline_dta$yield <- baseline_dta$bag_harv*baseline_dta$bag_kg/baseline_dta$plot_size
baseline_dta <- trim("yield",baseline_dta,trim_perc=.02)
baseline_dta$yield_level <- baseline_dta$yield 

evaluate_skewness(baseline_dta$yield_level)

baseline_dta$ihs_yield <-  ihs(baseline_dta$yield)
evaluate_skewness(baseline_dta$ihs_yield)

evaluate_skewness(baseline_dta$hh_size)
baseline_dta <- trim("hh_size",baseline_dta,trim_perc=.02)
baseline_dta$ihs_hh_size <-  ihs(baseline_dta$hh_size)
evaluate_skewness(baseline_dta$ihs_hh_size)

bal_vars <- c("age","prim_ed","HH_male", "hh_size", "dist_ag", "quality_use_any","baz_rand","source_rand", "recycler", "yield_level" )
df_means <- array(NA,c(2,length(bal_vars )))
for (i in 1:length(bal_vars)){
  ##means
  
  df_means[1,i] <- mean(unlist(baseline_dta[bal_vars[i]]), na.rm=TRUE)
  df_means[2,i] <- sd(unlist(baseline_dta[bal_vars[i]]), na.rm=TRUE)
}


bal_vars <- c("age","prim_ed","HH_male", "ihs_hh_size", "ihs_dist_ag", "quality_use_any","baz_rand","source_rand", "recycler", "ihs_yield" )


## demean indicators
baseline_dta$cont_demeaned <-  baseline_dta$cont - mean(baseline_dta$cont,na.rm = T)
baseline_dta$trial_P_demeaned <-  baseline_dta$trial_P - mean(baseline_dta$trial_P,na.rm = T)

df_balance <- array(NA,c(3,3,length(bal_vars )))
df_balance_pool  <- array(NA,c(2,3,length(bal_vars )))
for (i in 1:length(bal_vars)){
  ##means

  formula1 <- as.formula(paste(bal_vars[i],paste("trial_P*cont"),sep="~"))
  ols <- lm(formula1, data=baseline_dta)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dta$clusters,type="CR2")
  
  df_balance[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
  df_balance[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
  df_balance[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
  
  formula2 <- as.formula(paste(bal_vars[i],paste("trial_P*cont_demeaned"),sep="~"))
  ols <- lm(formula2, data=baseline_dta)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dta$clusters,type="CR2")
  
  df_balance_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_balance_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_balance_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(bal_vars[i],paste("cont*trial_P_demeaned"),sep="~"))
  ols <- lm(formula3, data=baseline_dta)
  vcov_cluster <- vcovCR(ols,cluster=baseline_dta$clusters,type="CR2")
  
  df_balance_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_balance_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_balance_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
}
#F-test


summary(lm(trial_P~age+prim_ed+HH_male+ihs_hh_size+ihs_dist_ag+quality_use_any+baz_rand+source_rand+recycler+ihs_yield,data=baseline_dta))

summary(lm(cont~age+prim_ed+HH_male+ihs_hh_size+ihs_dist_ag+quality_use_any+baz_rand+source_rand+recycler+ihs_yield,data=baseline_dta))

summary(lm(trial_P*cont~age+prim_ed+HH_male+ihs_hh_size+ihs_dist_ag+quality_use_any+baz_rand+source_rand+recycler+ihs_yield,data=baseline_dta))

save(df_balance,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_balance.Rdata",sep="/"))
save(df_balance_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_balance_pool.Rdata",sep="/"))
save(df_means,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means.Rdata",sep="/"))
