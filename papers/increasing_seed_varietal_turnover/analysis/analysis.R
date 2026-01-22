#### this file runs the analysis of endline data for "the "Increasing Adoption and Varietal Turnover of Seed—The Role of Producer and Consumer traits"
#### b.vancampenhout@cgiar.org 
rm(list=ls())
path <- getwd()
library(dplyr)
library(clubSandwich)
library(moments)
library(andersonTools)
# set to true if you want to use the Anderson q-values
sharp <- TRUE

########################################################################################################################################
######################################################### start function definitions ###################################################
########################################################################################################################################

###2. Demean and divide outcomes by control group standard deviation (normalizes outcomes to be on comparable scale)
#https://github.com/cdsamii/make_index/blob/master/r/index_comparison.R
#function to standardize columns of matrix, sgroup = control group = logical vector
matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  for(j in 1:ncol(x)){
    x[,j] <- (x[,j] - mean(x[sgroup,j],na.rm = T))/sd(x[sgroup,j],na.rm = T)
  }
  return(x)
}

#function that takes in data in matrix format and returns IC weights and ICW index
#wgts argument: weights can be incorporated
#revcols argument: takes vector indicating which columns should have reversed values (standardized values * -1) prior to construction of index
#because: For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.
icwIndex <- function(	xmat,
                      #wgts=rep(1, nrow(xmat)), #nrow: number of rows present in xmat --> many 1s
                      revcols = NULL,
                      sgroup = rep(TRUE, nrow(xmat))){
  X <- matStand(xmat, sgroup)
  if(length(revcols)>0){
    X[,revcols] <-  -1*X[,revcols]
  }
  
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  #Sx <- cov.wt(X, wt=wgts)[[1]]
  #list with estimates of the weighted covariance matrix and the mean of the data
  Sx <- cov(X,use = "pairwise.complete.obs")
  #cov: covariance of x and y if these are vectors/covariances between columns of x and columns of y are computed if these are matrices
  #use = "everything" produces NAs for the index.
  #use = "all.obs" produces an error.
  #use = "complete.obs" and use = "na.or.complete": works, NAs are handled by casewise deletion.
  #use = "pairwise.complete.obs": works, covariance between each pair of variables is computed using all complete pairs of observations on those variables
  weights <- solve(t(i.vec)%*%solve(Sx, tol = 1e-30)%*%i.vec, tol = 1e-30)%*%t(i.vec)%*%solve(Sx, tol = 1e-30)
  index <- t(solve(t(i.vec)%*%solve(Sx, tol = 1e-30)%*%i.vec, tol = 1e-30)%*%t(i.vec)%*%solve(Sx, tol = 1e-30)%*%t(X))
  return(list(weights = weights, index = index))
}

### function definition for trimming contineous variables (default 1 percent on each end)
trim <- function(var,dataset,trim_perc=.02){
  dataset[var][dataset[var]<quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[1]|dataset[var]>quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]] <- NA
  return(dataset)}

### function definition for inverse hyperbolic sine transform
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

########################################################################################################################################
######################################################### end function definitions #####################################################
########################################################################################################################################

path <- strsplit(path,"papers/increasing_seed_varietal_turnover/analysis")[[1]]
#read in baseline data
bse <- read.csv(paste(path,"baseline/data/public/baseline.csv",sep="/"))
bse <- subset(bse, !(paid_pac=="TRUE" | discounted=="TRUE"))
### create unique ID at level of randomization for clustering standard errors 
bse$cluster_ID <- as.factor(paste(paste(bse$distID,bse$subID, sep="_"), bse$vilID, sep="_"))

### read in endline data (anonymized version)
dta <- read.csv(paste(path,"endline/data/public/endline.csv", sep="/"))

## drop non-free trial packs and non-free + discount trial packs
dta <- subset(dta, !(paid_pac=="TRUE" | discounted=="TRUE"))
## create an indicator for pure control to standardize the indices (sgroup in icsIndex)
dta$s_ind <- (!dta$cont & !dta$trial_P)
#merge cluster)ID to baseline data - we lose 4 observations
dta <-merge(dta, bse[c("farmer_ID","cluster_ID")], by.x="ID", by.y="farmer_ID")


### look at attrition - we recontacted 100 procent of farmers but some were not interviewed or refused to be interviewed
dim(bse)[1] - dim(dta)[1]  # we lost 4 observations
### look at consent which is last point where enumerators are asked to abort
table(dta$consent)

### check is attrition is related to treatment
ols <- lm((consent == "Yes")~trial_P*cont,data= dta)
   vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
   coef_test(ols, vcov_cluster)

##if we also consider the 4 that were lost, we get an attrition rate of 1.4 percent
###drop these 18 that were not interviewed
dta <- subset(dta, consent == "Yes")

### do people recall the treatment (correctly)
table(dta$check3.Rec_TP,dta$trial_P)
table(dta$check3.cons_TP, dta$cont)

## these give treatment coverage, excess coverage, failure to reach and control group coverage:
## for trial pack treatment
prop.table(table(dta$check3.Rec_TP, dta$trial_P),2)
## for demo treatment
prop.table(table(dta$check3.cons_TP, dta$cont),2)

### some farmers do not cultivate in the second season of 2023

sum(dta$plot_count == "0") ## 61 or just under 4 percent
### while this is below the thresold of what we would deem acceptable for analysis we use this as a dependent variable to look at differential "attrition"
dta$no_grow <- (dta$plot_count == "0")

ols <- lm(no_grow~trial_P*cont,data= dta)
vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
coef_test(ols, vcov_cluster)

dta$cont_demeaned <- dta$cont - mean(dta$cont)
ols <- lm(no_grow~trial_P*cont_demeaned,data= dta)
vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
coef_test(ols, vcov_cluster)

dta$trial_P_demeaned <- dta$trial_P - mean(dta$trial_P)
ols <- lm(no_grow~cont*trial_P_demeaned,data= dta)
vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
coef_test(ols, vcov_cluster)

##ANALYSIS STARTS HERE

## primary outcome 1: uses improved seed on at least one plot
num_plots <- max(as.numeric(dta$plot_count), na.rm=TRUE)
logical_result <- logical(nrow(dta))

## drop if type is unkown
for (i in 1:num_plots) {
  dta[[paste0("plot.", i, "..plot_imp_type")]][ dta[[paste0("plot.", i, "..plot_imp_type")]] == "unknown"] <- NA
  dta[[paste0("plot.", i, "..plot_times_rec")]][ dta[[paste0("plot.", i, "..plot_times_rec")]] == 99 ] <- NA
  dta[[paste0("plot.", i, "..single_source")]][dta[[paste0("plot.", i, "..single_source")]] == 98] <- NA
  dta[[paste0("plot.", i, "..recycle_source_rest")]][dta[[paste0("plot.", i, "..recycle_source_rest")]] == 98] <- NA
}


for (i in 1:num_plots) {
### definition of improved seed: fresh hybrid from trusted source or OPV recycled max 3 times (use max 4 times) from trusted source
 condition <- (((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  
       c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta[[paste0("plot.", i, "..plot_times_rec")]] %in% 1) & (dta[[paste0("plot.", i, "..single_source")]]%in% letters[4:9])  ) |
      ((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3", "other_opv")) & (dta[[paste0("plot.", i, "..plot_times_rec")]] %in% 1:4) &  ((dta[[paste0("plot.", i, "..recycle_source_rest")]] %in% letters[4:9]) | (dta[[paste0("plot.", i, "..single_source")]]%in% letters[4:9])))) 
 # Combine the condition with the logical OR operator
 logical_result <- logical_result | condition
}



# Assign the result to the new column p_outcome_1
dta$p_outcome_1 <- logical_result
###only for those that 
dta$p_outcome_1[dta$no_grow] <- NA

## to control for this outcome at baseline, we use the question "Q20. Did you use any quality maize seed like **OPV or hybrid seed** in the previous season (Nsambya of 2022) on any of your plots?"
bse$b_p_outcome_1 <- bse$quality_use=="Yes"
bse$b_p_outcome_1[bse$quality_use==98] <- NA

dta <- merge(dta, bse[c("farmer_ID","b_p_outcome_1")], by.x="ID", by.y="farmer_ID")

## primary outcome 2: uses fresh bazooka on at least one plot
num_plots <- max(as.numeric(dta$plot_count), na.rm=TRUE)
logical_result <- logical(nrow(dta))

for (i in 1:num_plots) {
  condition <- (((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  
                    c("Bazooka")) & (dta[[paste0("plot.", i, "..plot_times_rec")]] %in% 1) & (dta[[paste0("plot.", i, "..single_source")]]%in% letters[4:9])  )) 
  # Combine the condition with the logical OR operator
  logical_result <- logical_result | condition
}

# Assign the result to the new column p_outcome_1
dta$p_outcome_2 <- logical_result

dta$p_outcome_2[dta$no_grow] <- NA

bse$b_p_outcome_2 <- bse$bazo_use=="Yes"

dta <- merge(dta, bse[c("farmer_ID","b_p_outcome_2")], by.x="ID", by.y="farmer_ID")

## primary outcome 2alt: uses bazooka on at least one plot
num_plots <- max(as.numeric(dta$plot_count), na.rm=TRUE)
logical_result <- logical(nrow(dta))

for (i in 1:num_plots) {
  condition <- (((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  
                    c("Bazooka")) )  ) 
  # Combine the condition with the logical OR operator
  logical_result <- logical_result | condition
}

# Assign the result to the new column p_outcome_1
dta$p_outcome_2alt <- logical_result

dta$p_outcome_2alt[dta$no_grow] <- NA


bse$b_p_outcome_2alt <- bse$bazo_use=="Yes"

dta <- merge(dta, bse[c("farmer_ID","b_p_outcome_2alt")], by.x="ID", by.y="farmer_ID")


## third primary outcome <- number of plots under improved maize cultivation
num_plots <- max(as.numeric(dta$plot_count), na.rm=TRUE)
logical_result <- logical(nrow(dta))

for (i in 1:num_plots) {
	  ### definition of improved seed: fresh hybrid from trusted source or OPV recycled max 3 times from trusted source
  condition <- (((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  
                    c("Bazooka")) )  )
  # Combine the condition with the logical OR operator
  logical_result <- logical_result + condition
}     


# Assign the result to the new column p_outcome_1
dta$nr_improved <- logical_result

dta$nr_improved[dta$no_grow] <- NA



##area under improved maize cultivation
num_plots <-  max(as.numeric(dta$plot_count), na.rm=TRUE)
areas <- matrix(NA,nrow(dta),num_plots)
tot_area <- matrix(NA,nrow(dta),num_plots)

for (i in 1:num_plots) {
#create a matrix if size of plots with adoption
  areas[,i] <- (((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  
                    c("Bazooka")) )  )
  tot_area[,i] <- as.numeric(as.character(dta[[paste0("plot.", i, "..plot_size")]] ))
}


dta$nr_improvedxsize <- rowSums(areas * tot_area, na.rm = TRUE)

dta$nr_improvedxsize[dta$no_grow] <- NA
dta$totsize <- rowSums( tot_area, na.rm=TRUE)
dta$totsize[dta$no_grow] <- NA

##share of plots under improved cultivation
dta$plot_no <- as.numeric(dta$plot_no)
dta$share_plots_imp <-  dta$nr_improved/dta$plot_no
dta$share_plots_imp[dta$no_grow] <- NA
dta$share_plots_imp[dta$share_plots_imp>1] <- NA
## share of area under improved cultivation
dta$share_area_imp <-  dta$nr_improvedxsize/dta$totsize
dta$share_area_imp[dta$no_grow] <- NA


#iterate over outcomes
outcomes <- c("p_outcome_2alt", "p_outcome_2","p_outcome_1","nr_improved", "share_plots_imp", "nr_improvedxsize", "share_area_imp")
b_outcomes <- c("b_p_outcome_2alt","b_p_outcome_2","b_p_outcome_1",NA,NA,NA,NA)
### do not include outcome 2 (adoption of bazooka) as this results in singularity - on second thought, maybe p_outcome_2 should be a key outcome and included
## in fact, the singularity disappeared in the real data
dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]), sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
b_outcomes <- c(b_outcomes,NA)
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  
  if (i %in% 4:8) {
  formula1 <- as.formula(paste(outcomes[i],paste("trial_P*cont"),sep="~"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
  df_res[1,4,i] <- nobs(ols) 
  } else {
  formula1 <- as.formula(paste(paste(outcomes[i],paste("trial_P*cont"),sep="~"), b_outcomes[i],sep="+"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[c(2:3,5)]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[c(2:3,5)]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[c(2:3,5)]
  df_res[1,4,i] <- nobs(ols) 
  }  

  if (i %in% 4:8) {  
  formula2 <- as.formula(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"))
  } else {
  formula2 <- as.formula(paste(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"), b_outcomes[i],sep="+"))   
  }  
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  if (i %in% 4:8) {   
  formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  } else {
  formula3 <- as.formula(paste(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"), b_outcomes[i],sep="+"))    
  }  
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
}
if (sharp == TRUE){
a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))

df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]

a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}
#### this is for table 2 (impact on adoption)
save(df_res_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_res_pool.Rdata",sep="/"))
save(df_res,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_res.Rdata",sep="/"))
save(df_means_out,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_out.Rdata",sep="/"))

###################################################### interlude: heterogeneity

### those that live far from agro-input shop are more likely to recycle? Not really
# dta <- merge(dta, bse[c("farmer_ID","dist_ag")], by.x="ID", by.y="farmer_ID")
# dta$dist_ag[dta$dist_ag==999] <- NA
# dta$het_1 <- dta$dist_ag>3

# ### those that recycled at baseline
# dta <- merge(dta, bse[c("farmer_ID","source")], by.x="ID", by.y="farmer_ID")
# dta$het_1 <- dta$source == "a" | dta$source == "b" 
# 
# ### thos that have lot of land
# dta <- merge(dta, bse[c("farmer_ID","ttl_land")], by.x="ID", by.y="farmer_ID")
# dta$ttl_land[dta$ttl_land==999] <- NA
# dta$het_1 <- dta$ttl_land>3
# 
# ### thos that had experience with bazo at baseline
# dta <- merge(dta, bse[c("farmer_ID","bazo_use")], by.x="ID", by.y="farmer_ID")
# # 
#  dta$het_1 <- dta$bazo_use =="Yes"
# ### thos that were not satisfied with seed used at baseline - indication of trust? hypo is that these are more likely to recycle our seed
# dta <- merge(dta, bse[c("farmer_ID","satfy")], by.x="ID", by.y="farmer_ID")
# 
# dta$het_1 <- dta$satfy =="No"
# ###those that sell at high prices - baseline
 dta <- merge(dta, bse[c("farmer_ID","bag_charge")], by.x="ID", by.y="farmer_ID")
# 
 dta$bag_charge <- as.numeric(dta$bag_charge.y)
 dta$bag_charge[dta$bag_charge >200000] <- NA
 dta$bag_charge_high <- as.numeric(dta$bag_charge)>90000
 # 

# 
#dta$bag_charge <- as.numeric(dta$bag_charge.x)
#dta$bag_charge[dta$bag_charge >200000] <- NA
#dta$bag_charge_high <- as.numeric(dta$bag_charge)>64000

dta$het_1 <- dta$bag_charge_high

### those that are poor



#iterate over outcomes
outcomes <- c("p_outcome_1","p_outcome_2","nr_improved", "share_plots_imp", "nr_improvedxsize", "share_area_imp" )
b_outcomes <- c("b_p_outcome_1", "b_p_outcome_2",NA,NA,NA,NA)
### do not include outcome 2 (adoption of bazooka) as this results in singularity - on second thought, maybe p_outcome_2 should be a key outcome and included
## in fact, the singularity disappeared in the real data
dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]), sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))

i <- 2
formula1 <- as.formula(paste(outcomes[i],paste("het_1"),sep="~"))
ols <- lm(formula1, data=dta[dta$trial_P == TRUE,])
summary(ols)


for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  
  if (i %in% 3:7) {
    formula1 <- as.formula(paste(outcomes[i],paste("trial_P*cont*het_1"),sep="~"))
    ols <- lm(formula1, data=dta)
    vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
    
    df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
    df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
    df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
    df_res[1,4,i] <- nobs(ols) 
  } else {
    formula1 <- as.formula(paste(paste(outcomes[i],paste("trial_P*cont*het_1"),sep="~"), b_outcomes[i],sep="+"))
    ols <- lm(formula1, data=dta)
    vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
    
    df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[c(2:3,5)]
    df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[c(2:3,5)]
    df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[c(2:3,5)]
    df_res[1,4,i] <- nobs(ols) 


  }  
  
  if (i %in% 3:7) {  
    formula2 <- as.formula(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"))
  } else {
    formula2 <- as.formula(paste(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"), b_outcomes[i],sep="+"))   
  }  
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  if (i %in% 3:7) {   
    formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  } else {
    formula3 <- as.formula(paste(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"), b_outcomes[i],sep="+"))    
  }  
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
}
if (sharp == TRUE){
  a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))
  
  df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
  df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
  df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]
  
  a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
  df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
  df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}
#### this is for table 2 (impact on adoption)
save(df_res_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_res_pool_het1.Rdata",sep="/"))
save(df_res,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_res.Rdata_het1",sep="/"))
save(df_means_out,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_out_het1.Rdata",sep="/"))

###interlude 2: likert plots for attributes to see if farmers that are not satisfied are more likley to recycle

### are farmers not happy with seed from agro traders
#this is for seed they used
dta$used_yield_rate[dta$used_yield_rate == "n/a"] <- NA
dta$used_drt_tol[dta$used_drt_tol == "n/a"] <- NA
dta$used_drt_tol[dta$used_drt_tol == "98"] <- NA
dta$used_dies_tol[dta$used_dies_tol == "n/a"] <- NA
dta$used_dies_tol[dta$used_dies_tol == "98"] <- NA
dta$used_erly_mat[dta$used_erly_mat == "n/a"] <- NA
dta$used_erly_mat[dta$used_erly_mat == "98"] <- NA
dta$used_germ_rate[dta$used_germ_rate == "n/a"] <- NA
dta$used_germ_rate[dta$used_germ_rate == "98"] <- NA
dta$used_happy[dta$used_happy == "n/a"] <- NA
dta$used_happy[dta$used_happy == "98"] <- NA

### this is for seed they received from us
dta$Trial_group.trial_yield_rate[dta$Trial_group.trial_yield_rate == "n/a"] <- NA 
dta$Trial_group.trial_drt_tol[dta$Trial_group.trial_drt_tol == "n/a"] <- NA
dta$Trial_group.trial_drt_tol[dta$Trial_group.trial_drt_tol == "98"] <- NA
dta$Trial_group.trial_dies_tol[dta$Trial_group.trial_dies_tol == "n/a"] <- NA
dta$Trial_group.trial_dies_tol[dta$Trial_group.trial_dies_tol == "98"] <- NA

dta$Trial_group.trial_erly_mat[dta$Trial_group.trial_erly_mat == "n/a"] <- NA
dta$Trial_group.trial_erly_mat[dta$Trial_group.trial_erly_mat == "98"] <- NA
dta$Trial_group.trial_germ_rate[dta$Trial_group.trial_germ_rate == "n/a"] <- NA
dta$Trial_group.trial_germ_rate[dta$Trial_group.trial_germ_rate == "98"] <- NA
dta$Trial_group.trial_happy[dta$Trial_group.trial_happy == "n/a"] <- NA
dta$Trial_group.trial_happy[dta$Trial_group.trial_happy == "98"] <- NA





########################
### on random plot (controlling for baseline)
dta$rnd_num <- as.numeric(dta$plot_select)
## some did not plant, work on subset
dta_sub <- subset(dta,!is.na(rnd_num))
library(dplyr)
## maize_var_selected is in the data
## this gets corresponding times recycled
dta_sub <- dta_sub %>%
  rowwise() %>%
  mutate(times_recycled_selected = get(paste0("plot.", rnd_num, "..plot_times_rec")))  %>%
  as.data.frame()
## this gets corresponding..recycle_source_rest ..single_source
dta_sub <- dta_sub %>%
  rowwise() %>%
  mutate(single_source_selected = get(paste0("plot.", rnd_num, "..single_source")))  %>%
  as.data.frame()
## this gets corresponding..recycle_source_rest ..single_source
dta_sub <- dta_sub %>%
  rowwise() %>%
  mutate(recycled_source_selected = get(paste0("plot.", rnd_num, "..recycle_source_rest")))  %>%
  as.data.frame()

dta_sub <- dta_sub %>%
  rowwise() %>%
  mutate(recycled_source_bazooka = get(paste0("plot.", rnd_num, "..recycle_source_bazooka")))  %>%
  as.data.frame()


##merge this back into dta
dta <- merge(dta, dta_sub[c("ID","times_recycled_selected","single_source_selected","recycled_source_selected","recycled_source_bazooka")], by.x="ID", by.y="ID", all.x=TRUE)


dta$rnd_adopt <-   (((dta$maize_var_selected %in%  
                        c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta$times_recycled_selected %in% 1) & (dta$single_source_selected %in% letters[4:9])  ) |
                      ((dta$maize_var_selected  %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3","other_opv")) & (dta$times_recycled_selected %in% 1:4) &  (((dta$single_source_selected %in% letters[4:9])) | (dta$recycled_source_selected %in% letters[4:9]))))
dta$rnd_adopt[dta$no_grow] <- NA 
## we assume here that seed from official sources has not been recycled
bse$b_rnd_adopt <- (((bse$maize_var  %in% c("Longe_10H"," Longe_7H","Longe_7R_Kayongo-go", "Bazooka","DK","Longe_6H")) & (bse$source %in% letters[4:9]) )
                  |
                    ((bse$maize_var  %in% c("Longe_5","Longe_4"," Panner", "Wema","KH_series"))  & (bse$source %in% letters[4:9]) ))
dta <- merge(dta, bse[c("farmer_ID","b_rnd_adopt")], by.x="ID", by.y="farmer_ID")

dta$rnd_bazo <-  ((dta$maize_var_selected == "Bazooka") & (dta$single_source_selected %in% letters[4:9]  & (dta$times_recycled_selected %in% 1)))
dta$rnd_bazo[dta$no_grow] <- NA 

## we assume here that seed from official sources has not been recycled
bse$b_rnd_bazo <- ((bse$maize_var == "Bazooka") & (bse$source %in% letters[4:9]) )
                   
dta <- merge(dta, bse[c("farmer_ID","b_rnd_bazo")], by.x="ID", by.y="farmer_ID")

### alternative -  bazo (fresh or recycled)
dta$rnd_bazo_alt <-  (dta$maize_var_selected == "Bazooka")
dta$rnd_bazo_alt[dta$no_grow] <- NA 

## we assume here that seed from official sources has not been recycled
bse$b_rnd_bazo_alt <- (bse$maize_var == "Bazooka")





### seed quantity
dta$seed_qty <- as.numeric(as.character(dta$seed_qty))
dta$seed_qty[dta$seed_qty == 999] <- NA 

dta$imp_seed_qty_rnd <- dta$rnd_adopt*dta$seed_qty
#dta$imp_seed_qty_rnd[is.na(dta$imp_seed_qty_rnd)] <- 0
bse$b_imp_seed_qty_rnd <- bse$b_rnd_adopt*bse$seed_qty
#bse$b_imp_seed_qty_rnd[is.na(bse$b_imp_seed_qty_rnd)] <- 0


dta <- trim("imp_seed_qty_rnd", dta)
bse  <- trim("b_imp_seed_qty_rnd", bse)

dta <- merge(dta, bse[c("farmer_ID","b_imp_seed_qty_rnd")], by.x="ID", by.y="farmer_ID")

dta$size_selected <- as.numeric(as.character(dta$size_selected)) 
### seed quantity per area
dta$imp_seed_qty_rnd_acre <- dta$imp_seed_qty_rnd/dta$size_selected  
bse$b_imp_seed_qty_rnd_acre <- bse$b_imp_seed_qty_rnd/bse$plot_size

dta <- trim("imp_seed_qty_rnd_acre", dta)
bse  <- trim("b_imp_seed_qty_rnd_acre", bse)

dta <- merge(dta, bse[c("farmer_ID","b_imp_seed_qty_rnd_acre")], by.x="ID", by.y="farmer_ID")
###production
dta$bag_harv[dta$bag_harv == "999"] <- NA
dta$production <- as.numeric(as.character(dta$bag_harv))*as.numeric(as.character(dta$bag_kg))
dta <- trim("production", dta)

bse$b_production <- bse$bag_harv*bse$bag_kg
bse <- trim("b_production", bse)

dta <- merge(dta, bse[c("farmer_ID","b_production")], by.x="ID", by.y="farmer_ID")
## productivity
dta$productivity <- dta$production/dta$size_selected
dta <- trim("productivity", dta)
bse$b_productivity <- bse$b_production/bse$plot_size
bse <- trim("b_productivity", bse)
dta <- merge(dta, bse[c("farmer_ID","b_productivity")], by.x="ID", by.y="farmer_ID")
dta$production_mean <- dta$production
dta$productivity_mean <- dta$productivity

dta$production <- log(dta$production)
dta$b_production <- log(dta$b_production)
dta$productivity <- log(dta$productivity)
dta$b_productivity <- log(dta$b_productivity)

dta$production[dta$production=="-Inf"] <- NA
dta$b_production[dta$b_production=="-Inf"] <- NA
dta$productivity[dta$productivity=="-Inf"] <- NA
dta$b_productivity[dta$b_productivity=="-Inf"] <- NA
#iterate over outcomes
outcomes <- c("rnd_adopt", "rnd_bazo", "imp_seed_qty_rnd", "imp_seed_qty_rnd_acre","production", "productivity" )
b_outcomes <- c("b_rnd_adopt", "b_rnd_bazo","b_imp_seed_qty_rnd","b_imp_seed_qty_rnd_acre", "b_production", "b_productivity")


dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
dta$b_index <- icwIndex(xmat= as.matrix(dta[b_outcomes]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index","rnd_bazo_alt")
b_outcomes <- c(b_outcomes, "b_index","b_rnd_bazo")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))





for (i in 1:length(outcomes)){
  ##means
  

  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  
  formula1 <- as.formula(paste(paste(outcomes[i],paste("trial_P*cont"),sep="~"), b_outcomes[i],sep="+"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[c(2:3,5)]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[c(2:3,5)]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[c(2:3,5)]
  df_res[1,4,i] <- nobs(ols) 
  
  formula2 <- as.formula(paste(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"), b_outcomes[i],sep="+"))
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"), b_outcomes[i],sep="+"))
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
}



if (sharp == TRUE){
a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))

df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]

a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}

df_rnd_pool <- df_res_pool
df_rnd <- df_res
df_means_rnd <- df_means_out

save(df_rnd_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_rnd_pool.Rdata",sep="/"))
save(df_rnd,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_rnd.Rdata",sep="/"))
save(df_means_rnd,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_rnd.Rdata",sep="/"))
###interlude: check if fresh bazooka is adopted more in treatment group when price is high


dta$bag_charge_base <- as.numeric(dta$bag_charge.y)
dta$bag_charge_base[dta$bag_charge_base > 250000] <- NA
dta$bag_charge_base[dta$bag_charge_base < 20000] <- NA
dta$bag_charge_end <- as.numeric(dta$bag_charge.x)
dta$bag_charge_end[dta$bag_charge_end<20000] <- NA
dta$bag_charge_end[dta$bag_charge_end>200000] <- NA
dta$bag_charge_end <- dta$bag_charge_end > 60000
dta$bag_charge_base <- dta$bag_charge_base > 100000

formula1 <- as.formula(paste("recycled_source_bazooka == 'b'",paste("bag_charge_base"),sep="~"))
ols <- lm(formula1, data=dta[dta$trial_P == TRUE & dta$maize_var_selected =='Bazooka',])
vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID[dta$trial_P  & dta$maize_var_selected =='Bazooka'],type="CR2")
coef_test(ols, vcov_cluster)

### distance to agro dealer  - actually suggests that those who are further away from agro dealer are less likely to recycle...
dta <- merge(dta, bse[c("farmer_ID","dist_ag")], by.x="ID", by.y="farmer_ID")
dta$dist_ag[dta$dist_ag>50] <- NA
dta$het_1 <- dta$dist_ag>3

formula1 <- as.formula(paste("recycled_source_bazooka == 'b'",paste("dist_ag"),sep="~"))
ols <- lm(formula1, data=dta[dta$trial_P == TRUE & dta$maize_var_selected =='Bazooka',])
vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID[dta$trial_P  & dta$maize_var_selected =='Bazooka'],type="CR2")
coef_test(ols, vcov_cluster)

###interlude: do farmers recycle because they do not trust agro-dealers


###compare ratings of bazo users in control (or in general?) to recyclers
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Used Bazooka on farmer plots
used_ctrl <- dta %>%
  filter(rnd_bazo == TRUE & trial_P == FALSE) %>%
  transmute(
    group = "Bazooka from agro-dealer",
    yield = used_yield_rate,
    germ_rate = used_germ_rate,
    drt_tol = used_drt_tol,
    dies_tol = used_dies_tol,
    erly_mat = used_erly_mat,
    happy = used_happy
  )

# Used Bazooka on farmer plots
used_treat <- dta %>%
  filter(rnd_bazo == TRUE & trial_P == TRUE) %>%
  transmute(
    group = "Recycled Bazooka",
    yield = used_yield_rate,
    germ_rate = used_germ_rate,
    drt_tol = used_drt_tol,
    dies_tol = used_dies_tol,
    erly_mat = used_erly_mat,
    happy = used_happy
  )

# Trial seed on trial plot
trial <- dta %>%
  filter(recycled_source_bazooka == "b" & trial_P == TRUE) %>%  # only non-missing
  transmute(
    group = "Trial pack seed",
    yield = Trial_group.trial_yield_rate,
    germ_rate = Trial_group.trial_germ_rate,
    drt_tol = Trial_group.trial_drt_tol,
    dies_tol = Trial_group.trial_dies_tol,
    erly_mat = Trial_group.trial_erly_mat,
    happy = Trial_group.trial_happy
  )

# Step 3: Combine both sources and reshape to long format
long_data <- bind_rows(used_treat, used_ctrl, trial) %>%
  pivot_longer(cols = -group, names_to = "attribute", values_to = "score") %>%
  # Recode Likert scores: 1 becomes 5, 2 → 4, 3 → 3, 4 → 2, 5 → 1
  mutate(
    score = case_when(
      score == 1 ~ 5,
      score == 2 ~ 4,
      score == 3 ~ 3,
      score == 4 ~ 2,
      score == 5 ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(score)) %>%
  mutate(
    score = factor(score, levels = 1:5, ordered = TRUE),
    attribute = recode(attribute,
                       yield = "Yield",
                       germ_rate = "Germination rate",
                       drt_tol = "Abiotic stress resistance",
                       dies_tol = "Biotic stress resistance",
                       erly_mat = "Time to maturity",
                       happy = "Overall satisfaction (3-point scale)"
    ),
    attribute = fct_relevel(attribute,
                            "Yield", "Germination rate", "Abiotic stress resistance",
                            "Biotic stress resistance", "Time to maturity", "Overall satisfaction (3-point scale)"
    )
  )

# Step 4: Define color palette (shades of blue, darker = better)
blue_shades <- c(
  "1" = "#c6dbef",  # now worst
  "2" = "#9ecae1",
  "3" = "#6baed6",
  "4" = "#3182bd",
  "5" = "#08519c"   # now best
)

# Step 5: Plot horizontal stacked bar chart
my_plot <- ggplot(long_data, aes(y = group, fill = score)) +
  geom_bar(position = "fill") +
  facet_wrap(~ attribute, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = blue_shades, name = "Perceived Quality\n(5 = best)", drop = TRUE) +
  labs(
    y = NULL, x = "Share of Respondents"  ) +guides(fill = guide_legend(reverse = TRUE)) +  
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "bottom")
ggsave(file=paste(path,"/papers/increasing_seed_varietal_turnover/results/percpetions.png",sep="/"), plot = my_plot, width = 8, height = 8, dpi = 300)


## test if richer farmers are less likely to recylce
columns_to_replace <- c(
  "value.maize_value_sp",
  "value.sorghum_value_sp",
  "value.millet_value_sp",
  "value.rice_value_sp",
  "value.cassava_value_sp",
  "value.sweetpotatoes_value_sp",
  "value.beans_value_sp",
  "value.gnuts_value_sp",
  "value.fruits_value_sp",
  "value.veg_value_sp",
  "value.sugar_value_sp",
  "value.cooking_oil_value_sp",
  "value.soap_value_sp",
  "value.airtime_value_sp"
)

# Replace 999 with NA for each specified column
for (col in columns_to_replace) {
  bse[[col]][bse[[col]] == "999"] <- NA
}

bse$cons_exp <- rowSums(cbind(as.numeric(bse$value.maize_value_sp),
                              as.numeric(bse$value.sorghum_value_sp),
                              as.numeric(bse$value.millet_value_sp),
                              as.numeric(bse$value.rice_value_sp),
                              as.numeric(bse$value.cassava_value_sp),
                              as.numeric(bse$value.sweetpotatoes_value_sp),
                              as.numeric(bse$value.beans_value_sp),
                              as.numeric(bse$value.gnuts_value_sp),
                              as.numeric(bse$value.fruits_value_sp),
                              as.numeric(bse$value.veg_value_sp),
                              as.numeric(bse$value.sugar_value_sp),
                              as.numeric(bse$value.cooking_oil_value_sp),
                              as.numeric(bse$value.soap_value_sp),
                              as.numeric(bse$value.airtime_value_sp)), na.rm=T)

bse <- trim("cons_exp",bse)

bse$b_cons_exp <- bse$cons_exp/bse$hh_size
dta <- merge(dta, bse[c("farmer_ID","b_cons_exp")], by.x="ID", by.y="farmer_ID")

graph_cash <- subset(dta, trial_P == TRUE & ( recycled_source_bazooka == "b" | rnd_bazo))
###plot distribution of
graph_cash$recycled <-  graph_cash$recycled_source_bazooka == "b" 

graph_cash <- subset(graph_cash , b_cons_exp<40000)

my_plot <- ggplot(graph_cash, aes(x = b_cons_exp, fill = recycled)) +
  geom_density(alpha = 0.7, bw=1500) +
  labs( x = "Consumption Expenditure per capita in UGX", y = "Density") +
  theme_minimal()

library(dplyr)
library(ggplot2)

# Create quintiles of consumption expenditure
graph_cash <- graph_cash %>%
  mutate(wealth_quintile = ntile(b_cons_exp, 5))   # 5 quintiles

# Compute mean recycling by quintile
mean_recycled <- graph_cash %>%
  group_by(wealth_quintile) %>%
  summarise(mean_recycled = mean(recycled, na.rm = TRUE))

# Plot: mean recycling rate by quintile
my_plot <- ggplot(mean_recycled, aes(x = factor(wealth_quintile), 
                                     y = mean_recycled)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(x = "Wealth Quintile (by consumption expenditure)", 
       y = "Share recycling Bazooka") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14)

my_plot

ggsave(file=paste(path,"/papers/increasing_seed_varietal_turnover/results/cash_contraints.png",sep="/"), plot = my_plot, width = 8, height = 6, dpi = 300)

tapply(!(graph_cash$b_rnd_adopt),as.factor(graph_cash$recycled), FUN=mean, na.rm=TRUE)
prop.table(table(!(graph_cash$b_rnd_adopt),as.factor(graph_cash$recycled)),2)

# Contingency table
tab <- table(graph_cash$b_rnd_adopt, graph_cash$recycled)

# Chi-squared test
chisq.test(tab)

### some interesting impact pathways
## knowledge
dta$nr_vars <- as.numeric(as.character(dta$nr_vars))
dta$nr_vars[dta$nr_vars == 999] <- NA

dta$knw_bazo <- dta$knw_bazo =="Yes"
bse$b_knw_bazo <- bse$knw_bazo == "Yes"
dta <- merge(dta, bse[c("farmer_ID","b_knw_bazo")], by.x="ID", by.y="farmer_ID")


dta$risk_imp[dta$risk_imp =="98"] <- NA
dta$downside_risk_imp <- dta$risk_imp == "1" | dta$risk_imp=="2"


dta$risk_bazo[dta$risk_bazo == "98"] <- NA
dta$risk_bazo[dta$risk_bazo == "n/a"] <- NA

dta$downside_risk_bazo <- dta$risk_bazo == "1" | dta$risk_bazo == "2"


dta$share_imp  <- dta$share_imp == "Yes"
dta$future_imp[dta$future_imp == "98"] <- NA
dta$future_imp <- dta$future_imp == "1" | dta$future_imp == "2"


dta$share_bazo[dta$share_bazo == "n/a"] <- NA
dta$share_bazo <- dta$share_bazo == "Yes"
dta$future_bazo[dta$future_bazo == "98"] <- NA

dta$future_bazo <- dta$future_bazo == "1"



#iterate over outcomes
outcomes <- c("knw_bazo","nr_vars" , "downside_risk_imp", "downside_risk_bazo","share_imp","share_bazo","future_imp", "future_bazo")
b_outcomes <- c("b_knw_bazo",rep(NA, times=7))

dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind, revcols = c(3,4))$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  if (i %in% 2:9) {
    formula1 <- as.formula(paste(outcomes[i],paste("trial_P*cont"),sep="~"))
    ols <- lm(formula1, data=dta)
    vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
    
    df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
    df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
    df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
    df_res[1,4,i] <- nobs(ols) 
  } else {
    formula1 <- as.formula(paste(paste(outcomes[i],paste("trial_P*cont"),sep="~"), b_outcomes[i],sep="+"))
    ols <- lm(formula1, data=dta)
    vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
    
    df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
    df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
    df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
    df_res[1,4,i] <- nobs(ols) 
  }  
  
  if (i %in% 2:9) {  
    formula2 <- as.formula(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"))
  } else {
    formula2 <- as.formula(paste(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"), b_outcomes[i],sep="+"))   
  }  
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  if (i %in% 2:9) {   
    formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  } else {
    formula3 <- as.formula(paste(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"), b_outcomes[i],sep="+"))    
  }  
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
}

if (sharp == TRUE){
a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))

df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]

a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}

df_path_pool <- df_res_pool
df_path <- df_res
df_means_path <- df_means_out

save(df_path_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_path_pool.Rdata",sep="/"))
save(df_path,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_path.Rdata",sep="/"))
save(df_means_path,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_path.Rdata",sep="/"))

#### table with impact on consumer traits of seed used
##taste was better

dta$used_cons_taste[dta$used_cons_taste == "98"] <- NA
dta$used_cons_taste_better <- dta$used_cons_taste == "1" | dta$used_cons_taste == "2"


dta$used_portions[dta$used_portions == "98"] <- NA
dta$used_portions_larger <- dta$used_portions == "1" | dta$used_portions == "2"


dta$used_cons_appear[dta$used_cons_appear == "98"] <- NA
dta$used_cons_appear_better <- dta$used_cons_appear == "1" | dta$used_cons_appear == "2"


dta$used_cons_cook[dta$used_cons_cook == "98"] <- NA
dta$used_cons_cook_easier <- dta$used_cons_cook == "1" | dta$used_cons_cook == "2"


#iterate over outcomes
outcomes <- c("used_cons_taste_better", "used_portions_larger",  "used_cons_appear_better", "used_cons_cook_easier")


dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  
  formula1 <- as.formula(paste(outcomes[i],paste("trial_P*cont"),sep="~"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
  df_res[1,4,i] <- nobs(ols) 
  
  formula2 <- as.formula(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"))
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
}

if (sharp == TRUE){
a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))

df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]

a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}
df_cons_traits_seed_pool <- df_res_pool
df_cons_traits_seed <- df_res
df_means_cons_traits_seed <- df_means_out

save(df_cons_traits_seed_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_cons_traits_seed_pool.Rdata",sep="/"))
save(df_cons_traits_seed,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_cons_traits_seed.Rdata",sep="/"))
save(df_means_cons_traits_seed,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_cons_traits_seed.Rdata",sep="/"))

#### table with impact of producer traits of seed used

dta$used_yield_rate[dta$used_yield_rate == "98"] <- NA
dta$used_yield_rate_higher <- dta$used_yield_rate == "1" | dta$used_yield_rate == "2"


dta$used_drt_tol[dta$used_drt_tol == "98"] <- NA
dta$used_drt_tol_better <- dta$used_drt_tol == "1" | dta$used_drt_tol == "2"


dta$used_dies_tol[dta$used_dies_tol == "98"] <- NA
dta$used_dies_tol_better <- dta$used_dies_tol == "1" | dta$used_dies_tol == "2"


dta$used_erly_mat[dta$used_erly_mat == "98"] <- NA
dta$used_erly_mat_shorter <- dta$used_erly_mat == "1" | dta$used_erly_mat == "2"


dta$used_germ_rate[dta$used_germ_rate == "98"] <- NA
dta$used_germ_rate_higher <- dta$used_germ_rate == "1" | dta$used_germ_rate == "2"


#iterate over outcomes
outcomes <- c("used_yield_rate_higher","used_drt_tol_better","used_dies_tol_better","used_erly_mat_shorter","used_germ_rate_higher")


dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  
  formula1 <- as.formula(paste(outcomes[i],paste("trial_P*cont"),sep="~"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
  df_res[1,4,i] <- nobs(ols) 
  
  formula2 <- as.formula(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"))
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]  
  
}

if (sharp == TRUE){
a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))

df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]

a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}
df_prod_traits_seed_pool <- df_res_pool
df_prod_traits_seed <- df_res
df_means_prod_traits_seed <- df_means_out

save(df_prod_traits_seed_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_prod_traits_seed_pool.Rdata",sep="/"))
save(df_prod_traits_seed,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_prod_traits_seed.Rdata",sep="/"))
save(df_means_prod_traits_seed,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_prod_traits_seed.Rdata",sep="/"))

#### table with impact on consumer traits (comparing improved and local seed)
##taste was better

dta$compare_cons_taste[dta$compare_cons_taste == "98"] <- NA
dta$compare_cons_taste_better <- dta$compare_cons_taste == "1" | dta$compare_cons_taste == "2"


dta$compare_portions[dta$compare_portions == "98"] <- NA
dta$compare_portions_larger <- dta$compare_portions == "1" | dta$compare_portions == "2"


dta$compare_cons_appear[dta$compare_cons_appear == "98"] <- NA
dta$compare_cons_appear_better <- dta$compare_cons_appear == "1" | dta$compare_cons_appear == "2"


dta$compare_cons_cook[dta$compare_cons_cook == "98"] <- NA
dta$compare_cons_cook_easier <- dta$compare_cons_cook == "1" | dta$compare_cons_cook == "2"


#iterate over outcomes
outcomes <- c("compare_cons_taste_better", "compare_portions_larger",  "compare_cons_appear_better", "compare_cons_cook_easier")


dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  
  formula1 <- as.formula(paste(outcomes[i],paste("trial_P*cont"),sep="~"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
  df_res[1,4,i] <- nobs(ols) 
  
  formula2 <- as.formula(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"))
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
}

if (sharp == TRUE){
a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))

df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]

a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}

df_compare_cons_traits_seed_pool <- df_res_pool
df_compare_cons_traits_seed <- df_res
df_means_compare_cons_traits_seed <- df_means_out

save(df_compare_cons_traits_seed_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_compare_cons_traits_seed_pool.Rdata",sep="/"))
save(df_compare_cons_traits_seed,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_compare_cons_traits_seed.Rdata",sep="/"))
save(df_means_compare_cons_traits_seed,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_compare_cons_traits_seed.Rdata",sep="/"))

#### table with impact of producer traits of seed used (comparing improved and local seed)

dta$compare_yield_rate[dta$compare_yield_rate == "98"] <- NA
dta$compare_yield_rate_higher <- dta$compare_yield_rate =="1" | dta$compare_yield_rate=="2"

dta$compare_drt_tol[dta$compare_drt_tol == "98"] <- NA
dta$compare_drt_tol_better <- dta$compare_drt_tol =="1" | dta$compare_drt_tol=="2"

dta$compare_dies_tol[dta$compare_dies_tol == "98"] <- NA
dta$compare_dies_tol_better <- dta$compare_dies_tol =="1" | dta$compare_dies_tol=="2"

dta$compare_erly_mat[dta$compare_erly_mat == "98"] <- NA
dta$compare_erly_mat_shorter <- dta$compare_erly_mat =="1" | dta$compare_erly_mat=="2"

dta$compare_germ_rate[dta$compare_germ_rate == "98"] <- NA
dta$compare_germ_rate_higher <- dta$compare_germ_rate =="1" | dta$compare_germ_rate=="2"

#iterate over outcomes
outcomes <- c("compare_yield_rate_higher","compare_drt_tol_better","compare_dies_tol_better","compare_erly_mat_shorter", "compare_germ_rate_higher")

dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  
  formula1 <- as.formula(paste(outcomes[i],paste("trial_P*cont"),sep="~"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
  df_res[1,4,i] <- nobs(ols) 
  
  formula2 <- as.formula(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"))
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
}

if (sharp == TRUE){
a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))

df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]

a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}

df_compare_prod_traits_seed_pool <- df_res_pool
df_compare_prod_traits_seed <- df_res
df_means_compare_prod_traits_seed <- df_means_out

save(df_compare_prod_traits_seed_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_compare_prod_traits_seed_pool.Rdata",sep="/"))
save(df_compare_prod_traits_seed,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_compare_prod_traits_seed.Rdata",sep="/"))
save(df_means_compare_prod_traits_seed,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_compare_prod_traits_seed.Rdata",sep="/"))
  
#### table with impact of post harves traits of seed used
dta$used_mrkt_dem[dta$used_mrkt_dem == "98"] <- NA
dta$used_mrkt_dem_higher <- (dta$used_mrkt_dem =="1" | dta$used_mrkt_dem =="2")


dta$used_biomass[dta$used_biomass=="98"] <- NA
dta$used_biomass_higher <- ( dta$used_biomass =="1" |  dta$used_biomass=="2")

dta$used_process[dta$used_process=="98"] <- NA
dta$used_process_higher <- ( dta$used_process =="1" |  dta$used_process=="2")


#iterate over outcomes
outcomes <- c("used_mrkt_dem_higher","used_biomass_higher","used_process_higher")


dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  
  formula1 <- as.formula(paste(outcomes[i],paste("trial_P*cont"),sep="~"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
  df_res[1,4,i] <- nobs(ols) 
  
  formula2 <- as.formula(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"))
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
}

if (sharp == TRUE){
a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))

df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]

a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}

df_used_ph_pool <- df_res_pool
df_used_ph <- df_res
df_means_used_ph <- df_means_out

save(df_used_ph_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_used_ph_pool.Rdata",sep="/"))
save(df_used_ph,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_used_ph.Rdata",sep="/"))
save(df_means_used_ph,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_used_ph.Rdata",sep="/"))

#### table with impact of post harves traits of improved seed compared to local

dta$compare_mrkt_dem[dta$compare_mrkt_dem == "98"] <- NA
dta$compare_mrkt_dem_higher <- (dta$compare_mrkt_dem =="1" | dta$compare_mrkt_dem =="2")


dta$compare_biomass[dta$compare_biomass=="98"] <- NA
dta$compare_biomass_higher <- ( dta$compare_biomass =="1" |  dta$compare_biomass=="2")

dta$compare_process[dta$compare_process=="98"] <- NA
dta$compare_process_higher <- ( dta$compare_process =="1" |  dta$compare_process=="2")


#iterate over outcomes
outcomes <- c("compare_mrkt_dem_higher","compare_biomass_higher","compare_process_higher")


dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  
  formula1 <- as.formula(paste(outcomes[i],paste("trial_P*cont"),sep="~"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
  df_res[1,4,i] <- nobs(ols) 
  
  formula2 <- as.formula(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"))
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
}

if (sharp == TRUE){
a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))

df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]

a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}

df_compare_ph_pool <- df_res_pool
df_compare_ph <- df_res
df_means_compare_ph <- df_means_out


save(df_compare_ph_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_compare_ph_pool.Rdata",sep="/"))
save(df_compare_ph,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_compare_ph.Rdata",sep="/"))
save(df_means_compare_ph,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_compare_ph.Rdata",sep="/"))

## table with impact on decision making
#note: there is a significant share of households where women are taking decisions alone
#most likely this is due to high levels of polygamy in the area
#for these households, we thus do not expect a change in decisionmaking
#can we drop?

dta$who1[dta$who1 == 'n/a'] <- NA
dta$who1[(dta$gender_resp == "Female" & dta$head_resp == "Yes") | dta$head_gender == "Female"] <- NA
dta$who1 <-  dta$who1 == "1" | dta$who1 == "3" | dta$who1 == "4"
dta$who2[dta$who2 == 'n/a'] <- NA
dta$who2[(dta$gender_resp == "Female" & dta$head_resp == "Yes") | dta$head_gender == "Female"] <- NA
dta$who2 <-  dta$who2 == "1" | dta$who2 == "3" | dta$who2 == "4"

bse$b_who1 <- bse$who1 == 1 | bse$who1 == 3 | bse$who1 == 4
dta <- merge(dta, bse[c("farmer_ID","b_who1")], by.x="ID", by.y="farmer_ID")

bse$b_who2 <- bse$who2 == 1 | bse$who2 == 3 | bse$who2 == 4
dta <- merge(dta, bse[c("farmer_ID","b_who2")], by.x="ID", by.y="farmer_ID")

#iterate over outcomes
outcomes <- c("who1","who2")
b_outcomes <- c("b_who1","b_who2")
###can we control for baseline outcomes? Yes


dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
dta$b_index <- icwIndex(xmat= as.matrix(dta[b_outcomes]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
b_outcomes <- c(b_outcomes, "b_index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  
  formula1 <- as.formula(paste(paste(outcomes[i],paste("trial_P*cont"),sep="~"), b_outcomes[i],sep="+"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[c(2:3,5)]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[c(2:3,5)]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[c(2:3,5)]
  df_res[1,4,i] <- nobs(ols) 
  
  formula2 <- as.formula(paste(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"), b_outcomes[i],sep="+"))
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"), b_outcomes[i],sep="+"))
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
} 

if (sharp == TRUE){
a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))

df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]

a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}

df_decision_pool <- df_res_pool
df_decision <- df_res
df_means_decision <- df_means_out

save(df_decision_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_decision_pool.Rdata",sep="/"))
save(df_decision,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_decision.Rdata",sep="/"))
save(df_means_decision,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_decision.Rdata",sep="/"))

## table with impact on decision making

dta$perc_cons <- as.numeric(dta$bag_keep)/as.numeric(dta$bag_harv)*100
bse$bag_keep[bse$bag_keep > 900] <- NA
bse$bag_harv[bse$bag_harv > 900] <- NA

bse$b_perc_cons <- as.numeric(bse$bag_keep)/as.numeric(bse$bag_harv)*100

dta$perc_cons[dta$perc_cons>100] <- NA
dta$perc_cons[dta$no_grow] <- NA
dta$perc_sell <- as.numeric(as.character(dta$bag_sell))/as.numeric(dta$bag_harv)*100
dta$perc_sell[is.na(dta$perc_sell)] <- 0
dta$perc_sell[dta$perc_sell>100] <- NA
dta$perc_sell[dta$no_grow] <- NA
dta$seed_keep[dta$seed_keep == "999"] <- NA

dta$seed_keep[dta$no_grow] <- NA
dta$seed_keep <- as.numeric(as.character(dta$seed_keep))
dta$seed_keep[dta$seed_keep>50] <- NA
#iterate over outcomes
outcomes <- c("perc_cons","perc_sell","seed_keep")
#can we control for baseline outcomes here

dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),revcols = c(2,3),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  
  formula1 <- as.formula(paste(outcomes[i],paste("trial_P*cont"),sep="~"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
  df_res[1,4,i] <- nobs(ols) 
  
  formula2 <- as.formula(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"))
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
}

if (sharp == TRUE){
a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))

df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]

a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}

df_disposal_pool <- df_res_pool
df_disposal <- df_res
df_means_disposal <- df_means_out

save(df_disposal_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_disposal_pool.Rdata",sep="/"))
save(df_disposal,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_disposal.Rdata",sep="/"))
save(df_means_disposal,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_disposal.Rdata",sep="/"))

## table with welfare and food security

dta <-merge(dta, bse[c("farmer_ID","hh_size")], by.x="ID", by.y="farmer_ID", all.x=TRUE)

dta$in_com[dta$in_com == "98" ] <- NA
dta$better_off_vil <- dta$in_com == "1"
dta$in_six[dta$in_six == "98"] <- NA
dta$better_off_six <- dta$in_six == "1"
dta$food_secure_pref <- dta$fd_pref =="No"
dta$food_secure_quant <- dta$fd_less =="No"

columns_to_replace <- c(
  "maize_value_sp",
  "sorghum_value_sp",
  "millet_value_sp",
  "rice_value_sp",
  "cassava_value_sp",
  "sweetpotatoes_value_sp",
  "beans_value_sp",
  "gnuts_value_sp",
  "fruits_value_sp",
  "veg_value_sp",
  "sugar_value_sp",
  "cooking_oil_value_sp",
  "soap_value_sp",
  "airtime_value_sp"
)

# Replace 999 with NA for each specified column
for (col in columns_to_replace) {
  dta[[col]][dta[[col]] == "999"] <- NA
}

dta$cons_exp <- rowSums(cbind(as.numeric(dta$maize_value_sp),
as.numeric(dta$sorghum_value_sp),
as.numeric(dta$millet_value_sp),
as.numeric(dta$rice_value_sp),
as.numeric(dta$cassava_value_sp),
as.numeric(dta$sweetpotatoes_value_sp),
as.numeric(dta$beans_value_sp),
as.numeric(dta$gnuts_value_sp),
as.numeric(dta$fruits_value_sp),
as.numeric(dta$veg_value_sp),
as.numeric(dta$sugar_value_sp),
as.numeric(dta$cooking_oil_value_sp),
as.numeric(dta$soap_value_sp),
as.numeric(dta$airtime_value_sp)), na.rm=T)

dta <- trim("cons_exp",dta)

dta$cons_exp <- dta$cons_exp/dta$hh_size

#iterate over outcomes
outcomes <- c("better_off_vil","better_off_six","food_secure_pref","food_secure_quant","cons_exp")


dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(6,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[(dta$trial_P == FALSE & dta$cont == FALSE),outcomes[i]]), na.rm=TRUE)
  
  df_means_out[3,i] <- mean(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[4,i] <- sd(unlist(dta[dta$trial_P == FALSE,outcomes[i]]), na.rm=TRUE)
  
  df_means_out[5,i] <- mean(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  df_means_out[6,i] <- sd(unlist(dta[dta$cont == FALSE,outcomes[i]]), na.rm=TRUE)
  
  
  
  formula1 <- as.formula(paste(outcomes[i],paste("trial_P*cont"),sep="~"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
  df_res[1,4,i] <- nobs(ols) 
  
  formula2 <- as.formula(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"))
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
}

if (sharp == TRUE){
a_sharp <-  anderson_sharp_q(c(df_res[1,3,1:(length(outcomes)-1)],df_res[2,3,1:(length(outcomes)-1)],df_res[3,3,1:(length(outcomes)-1)]))

df_res[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)] 
df_res[2,3,1:(length(outcomes)-1)] <-  a_sharp[length(outcomes):(2*length(outcomes)-2)]  
df_res[3,3,1:(length(outcomes)-1)] <-  a_sharp[(2*length(outcomes)-1):(3*length(outcomes)-3)]

a_sharp <-  anderson_sharp_q(c(df_res_pool[1,3,1:(length(outcomes)-1)],df_res_pool[2,3,1:(length(outcomes)-1)]))					       
df_res_pool[1,3,1:(length(outcomes)-1)] <- a_sharp[1:(length(outcomes)-1)]
df_res_pool[2,3,1:(length(outcomes)-1)] <- a_sharp[length(outcomes):(2*length(outcomes)-2)]  
}

df_welfare_pool <- df_res_pool
df_welfare <- df_res
df_means_welfare <- df_means_out


save(df_welfare_pool,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_welfare_pool.Rdata",sep="/"))
save(df_welfare,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_welfare.Rdata",sep="/"))
save(df_means_welfare,file=paste(path,"/papers/increasing_seed_varietal_turnover/results/df_means_welfare.Rdata",sep="/"))


#select variables for carly
dta$p_outcome_2alt ## adoption_pv

## primary outcome 1: uses improved seed on at least one plot
num_plots <- max(as.numeric(dta$plot_count), na.rm=TRUE)
logical_result <- logical(nrow(dta))

for (i in 1:num_plots) {
  ### definition of improved seed: fresh hybrid from trusted source or OPV recycled max 3 times from trusted source
  condition <- (((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  
                    c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) ) |
                  ((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3", "other_opv")) ))
  # Combine the condition with the logical OR operator
  logical_result <- logical_result | condition
}

# Assign the result to the new column p_outcome_1
dta$p_outcome_1alt <- logical_result
###only for those that 
dta$p_outcome_1alt[dta$no_grow] <- NA

###not interviewed
dta$p_outcome_1alt[is.na(dta$plot_no)] <- NA

dta$adoption_riv <- FALSE

dta$p_outcome_2 #purchase

##area under improved maize cultivation
num_plots <-  max(as.numeric(dta$plot_count), na.rm=TRUE)
areas <- matrix(NA,nrow(dta),num_plots)
tot_area <- matrix(NA,nrow(dta),num_plots)

for (i in 1:num_plots) {
  #create a matrix if size of plots with adoption
  areas[,i] <-  (((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  
                     c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) ) |
                   ((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3", "other_opv")) ))*as.numeric(as.character(dta[[paste0("plot.", i, "..plot_size")]] ))
  tot_area[,i] <- as.numeric(as.character(dta[[paste0("plot.", i, "..plot_size")]] ))
}


dta$nr_improvedxsize <- rowSums( areas, na.rm=TRUE)
dta$nr_improvedxsize[dta$no_grow] <- NA
dta$totsize <- rowSums( tot_area, na.rm=TRUE)
dta$totsize[dta$no_grow] <- NA

##share of plots under improved cultivation
dta$plot_no <- as.numeric(dta$plot_no)
dta$share_plots_imp <-  dta$nr_improved/dta$plot_no
dta$share_plots_imp[dta$no_grow] <- NA
dta$share_plots_imp[dta$share_plots_imp>1] <- NA
## share of area under improved cultivation
dta$share_area_imp <-  dta$nr_improvedxsize/dta$totsize
dta$share_area_imp[dta$no_grow] <- NA

##area under bazooka cultivation
num_plots <-  max(as.numeric(dta$plot_count), na.rm=TRUE)
areas <- matrix(NA,nrow(dta),num_plots)
tot_area <- matrix(NA,nrow(dta),num_plots)

for (i in 1:num_plots) {
  #create a matrix if size of plots with adoption
  areas[,i] <-  (((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  
                     c("Bazooka")) ))*as.numeric(as.character(dta[[paste0("plot.", i, "..plot_size")]] ))
  tot_area[,i] <- as.numeric(as.character(dta[[paste0("plot.", i, "..plot_size")]] ))
}


dta$nr_bazxsize <- rowSums( areas, na.rm=TRUE)
dta$nr_bazxsize[dta$no_grow] <- NA
dta$totsize <- rowSums( tot_area, na.rm=TRUE)
dta$totsize[dta$no_grow] <- NA

##share of plots under improveid cultivation
dta$plot_no <- as.numeric(dta$plot_no)
dta$share_plots_baz <-  dta$nr_baz/dta$plot_no
dta$share_plots_baz[dta$no_grow] <- NA
dta$share_plots_baz[dta$share_plots_imp>1] <- NA
## share of area under improved cultivation
dta$share_area_baz <-  dta$nr_bazxsize/dta$totsize
dta$share_area_baz[dta$no_grow] <- NA






##avearge varietal age of the crop being planted
### first merge in varietal age for the seed varieties used on the different plots



dta_meta <- data.frame(cbind(dta$ID,dta$cluster_ID,dta$cont,dta$trial_P,dta$p_outcome_2alt,dta$p_outcome_1alt, dta$adoption_riv,dta$p_outcome_2, dta$share_area_baz, dta$share_area_imp))
names(dta_meta) <- c("farmer_ID","cluster_ID", "T_cons", "T_prod", "adoption_pv","adoption_iv","adoption_riv", "purchase", "area_pv","area_iv")

