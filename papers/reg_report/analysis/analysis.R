
rm(list=ls())
path <- getwd()
library(dplyr)

#to use vcovCR
library(clubSandwich)
### functions definitions
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

path <- strsplit(path,"papers/reg_report/analysis")[[1]]
#read in baseline data
bse <- read.csv(paste(path,"baseline/data/public/baseline.csv",sep="/"))

### create unique ID at level of randomization for clustering standard errors 
bse$cluster_ID <- as.factor(paste(paste(bse$distID,bse$subID, sep="_"), bse$vilID, sep="_"))

### read in endline data (ananymized version)
dta <- read.csv(paste(path,"endline/data/public/endline.csv", sep="/"))

## drop non-free trial packs and non-free + discount trial packs
dta <- subset(dta, paid_pac == FALSE & discounted == FALSE)
## create an indicator for pure control to standardize the indices (sgroup in icsIndex)
dta$s_ind <- (!dta$cont & !dta$trial_P)
#merge to baseline data
dta <-merge(dta, bse[c("farmer_ID","cluster_ID")], by.x="ID", by.y="farmer_ID")


## primary outcome 1: uses improved seed on at least one plot
num_plots <- max(as.numeric(dta$plot_count), na.rm=TRUE)
logical_result <- logical(nrow(dta))

for (i in 1:num_plots) {
### definition of improved seed: fresh hybrid from trusted source or OPV recycled max 3 times from trusted source
 condition <- (((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  
       c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta[[paste0("plot.", i, "..plot_times_rec")]] %in% 1) & (dta[[paste0("plot.", i, "..single_source")]]%in% letters[4:9])  ) |
      ((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3")) & (dta[[paste0("plot.", i, "..plot_times_rec")]] %in% 1:4) &  ((dta[[paste0("plot.", i, "..recycle_source_rest")]] %in% letters[4:9]) | (dta[[paste0("plot.", i, "..single_source")]]%in% letters[4:9])))) 
 # Combine the condition with the logical OR operator
 logical_result <- logical_result | condition
}

# Assign the result to the new column p_outcome_1
dta$p_outcome_1 <- logical_result


###not interviewed
dta$p_outcome_1[is.na(dta$plot_no)] <- NA


## to control for this outcome at baseline, we use the question "Q20. Did you use any quality maize seed like **OPV or hybrid seed** in the previous season (Nsambya of 2022) on any of your plots?"
bse$b_p_outcome_1 <- bse$quality_use=="Yes"

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

dta$p_outcome_2[is.na(dta$plot_no)] <- NA


bse$b_p_outcome_2 <- bse$bazo_use=="Yes"

dta <- merge(dta, bse[c("farmer_ID","b_p_outcome_2")], by.x="ID", by.y="farmer_ID")

## third primary outcome <- number of plots under improved maize cultivation
num_plots <- max(as.numeric(dta$plot_count), na.rm=TRUE)
logical_result <- logical(nrow(dta))

for (i in 1:num_plots) {
  ### definition of improved seed: fresh hybrid from trusted source or OPV recycled max 3 times from trusted source
  condition <- (((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  
                    c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta[[paste0("plot.", i, "..plot_times_rec")]] %in% 1) & (dta[[paste0("plot.", i, "..single_source")]]%in% letters[4:9])  ) |
                  ((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3")) & (dta[[paste0("plot.", i, "..plot_times_rec")]] %in% 1:4) &  ((dta[[paste0("plot.", i, "..recycle_source_rest")]] %in% letters[4:9]) | (dta[[paste0("plot.", i, "..single_source")]]%in% letters[4:9])))) 
  # Combine the condition with the logical OR operator
  logical_result <- logical_result + condition
}

# Assign the result to the new column p_outcome_1
dta$nr_improved <- logical_result


dta$nr_improved[is.na(dta$plot_no)] <- NA


##area under improved maize cultivation
num_plots <-  max(as.numeric(dta$plot_count), na.rm=TRUE)
areas <- matrix(NA,nrow(dta),num_plots)
tot_area <- matrix(NA,nrow(dta),num_plots)

for (i in 1:num_plots) {
#create a matrix if size of plots with adoption
  areas[,i] <- (((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  
                    c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta[[paste0("plot.", i, "..plot_times_rec")]] %in% 1) & (dta[[paste0("plot.", i, "..single_source")]]%in% letters[4:9])  ) |
                  ((dta[[paste0("plot.", i, "..plot_imp_type")]] %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3")) & (dta[[paste0("plot.", i, "..plot_times_rec")]] %in% 1:4) &  ((dta[[paste0("plot.", i, "..recycle_source_rest")]] %in% letters[4:9]) | (dta[[paste0("plot.", i, "..single_source")]]%in% letters[4:9]))))*as.numeric(as.character(dta[[paste0("plot.", i, "..plot_size")]] ))
  tot_area[,i] <- as.numeric(as.character(dta[[paste0("plot.", i, "..plot_size")]] ))
}

dta$nr_improvedxsize <- rowSums( areas, na.rm=TRUE)
dta$totsize <- rowSums( tot_area, na.rm=TRUE)

##share of plots under improved cultivation
dta$plot_no <- as.numeric(dta$plot_no)
dta$share_plots_imp <-  dta$nr_improved/dta$plot_no
## share of area under improved cultivation
dta$share_area_imp <-  dta$nr_improvedxsize/dta$totsize


#iterate over outcomes
outcomes <- c("p_outcome_1","p_outcome_2","nr_improved", "share_plots_imp", "nr_improvedxsize", "share_area_imp" )
b_outcomes <- c("b_p_outcome_1", "b_p_outcome_2",NA,NA)
### do not include outcome 2 (adoption of bazooka) as this results in singularity
dta$index <- icwIndex(xmat= as.matrix(dta[ setdiff(outcomes,"p_outcome_2")]), sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
  if (i %in% 3:7) {
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

save(df_res_pool,file=paste(path,"/papers/reg_report/results/df_res_pool.Rdata",sep="/"))
save(df_res,file=paste(path,"/papers/reg_report/results/df_res.Rdata",sep="/"))
save(df_means_out,file=paste(path,"/papers/reg_report/results/df_means_out.Rdata",sep="/"))

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

##merge this back into dta
dta <- merge(dta, dta_sub[c("ID","times_recycled_selected","single_source_selected","recycled_source_selected")], by.x="ID", by.y="ID", all.x=TRUE)


dta$rnd_adopt <-   (((dta$maize_var_selected %in%  
                        c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta$times_recycled_selected %in% 1) & (dta$single_source_selected %in% letters[4:9])  ) |
                      ((dta$maize_var_selected  %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3")) & (dta$times_recycled_selected %in% 1:4) &  (((dta$single_source_selected %in% letters[4:9])) | (dta$recycled_source_selected %in% letters[4:9]))))

## we assume here that seed from official sources has not been recycled
bse$b_rnd_adopt <- (((bse$maize_var  %in% c("Longe_10H"," Longe_7H","Longe_7R_Kayongo-go", "Bazooka","DK","Longe_6H")) & (bse$source %in% letters[4:9]) )
                  |
                    ((bse$maize_var  %in% c("Longe_5","Longe_4"," Panner", "Wema","KH_series"))  & (bse$source %in% letters[4:9]) ))
dta <- merge(dta, bse[c("farmer_ID","b_rnd_adopt")], by.x="ID", by.y="farmer_ID")

dta$rnd_bazo <-  ((dta$maize_var_selected == "Bazooka") & (dta$single_source_selected %in% letters[4:9]  & (dta$times_recycled_selected %in% 1)))
## we assume here that seed from official sources has not been recycled
bse$b_rnd_bazo <- ((bse$maize_var == "Bazooka") & (bse$source %in% letters[4:9]) )
                   
dta <- merge(dta, bse[c("farmer_ID","b_rnd_bazo")], by.x="ID", by.y="farmer_ID")

### seed quantity
dta$imp_seed_qty_rnd <- dta$rnd_adopt*as.numeric(as.character(dta$seed_qty))
dta$imp_seed_qty_rnd[is.na(dta$imp_seed_qty_rnd)] <- 0
bse$b_imp_seed_qty_rnd <- bse$b_rnd_adopt*bse$seed_qty
bse$b_imp_seed_qty_rnd[is.na(bse$b_imp_seed_qty_rnd)] <- 0
dta <- merge(dta, bse[c("farmer_ID","b_imp_seed_qty_rnd")], by.x="ID", by.y="farmer_ID")

dta$size_selected <- as.numeric(as.character(dta$size_selected)) 
### seed quantity per area
dta$imp_seed_qty_rnd_acre <- dta$imp_seed_qty_rnd/dta$size_selected  
bse$b_imp_seed_qty_rnd_acre <- bse$b_imp_seed_qty_rnd/bse$plot_size
dta <- merge(dta, bse[c("farmer_ID","b_imp_seed_qty_rnd_acre")], by.x="ID", by.y="farmer_ID")
###production
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
#iterate over outcomes
outcomes <- c("rnd_adopt", "rnd_bazo", "imp_seed_qty_rnd", "imp_seed_qty_rnd_acre","production", "productivity" )
b_outcomes <- c("b_rnd_adopt", "b_rnd_bazo","b_imp_seed_qty_rnd","b_imp_seed_qty_rnd_acre", "b_production", "b_productivity")

dta$index <- icwIndex(xmat= as.matrix(dta[setdiff(outcomes,c("rnd_bazo"))]),sgroup=dta$s_ind)$index
dta$b_index <- icwIndex(xmat= as.matrix(dta[setdiff(b_outcomes,"b_rnd_bazo")]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
b_outcomes <- c(b_outcomes, "b_index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
  
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

df_rnd_pool <- df_res_pool
df_rnd <- df_res
df_means_rnd <- df_means_out


save(df_rnd_pool,file=paste(path,"/papers/reg_report/results/df_rnd_pool.Rdata",sep="/"))
save(df_rnd,file=paste(path,"/papers/reg_report/results/df_rnd.Rdata",sep="/"))
save(df_means_rnd,file=paste(path,"/papers/reg_report/results/df_means_rnd.Rdata",sep="/"))

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
dta$downside_risk_bazo <- dta$risk_bazo == "1" | dta$risk_bazo == "2"


dta$share_imp  <- dta$share_imp == "Yes"
dta$future_imp[dta$future_imp == "98"] <- NA
dta$future_imp <- dta$future_imp == "1" | dta$future_imp == "2"



dta$share_bazo <- dta$share_bazo == "Yes"
dta$future_bazo[dta$future_bazo == "98"] <- NA
dta$future_bazo <- dta$future_bazo == "1" | dta$future_bazo == "2"



#iterate over outcomes
outcomes <- c("knw_bazo","nr_vars" , "downside_risk_imp", "downside_risk_bazo","share_imp","share_bazo","future_imp", "future_bazo")
b_outcomes <- c("b_knw_bazo",rep(NA, times=7))

dta$index <- icwIndex(xmat= as.matrix(dta[setdiff(outcomes,c("downside_risk_bazo","share_bazo","future_bazo"))]),sgroup=dta$s_ind, revcols = c(0,0,1,0,0))$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
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

df_path_pool <- df_res_pool
df_path <- df_res
df_means_path <- df_means_out


save(df_path_pool,file=paste(path,"/papers/reg_report/results/df_path_pool.Rdata",sep="/"))
save(df_path,file=paste(path,"/papers/reg_report/results/df_path.Rdata",sep="/"))
save(df_means_path,file=paste(path,"/papers/reg_report/results/df_means_path.Rdata",sep="/"))

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
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
  
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

df_cons_traits_seed_pool <- df_res_pool
df_cons_traits_seed <- df_res
df_means_cons_traits_seed <- df_means_out


save(df_cons_traits_seed_pool,file=paste(path,"/papers/reg_report/results/df_cons_traits_seed_pool.Rdata",sep="/"))
save(df_cons_traits_seed,file=paste(path,"/papers/reg_report/results/df_cons_traits_seed.Rdata",sep="/"))
save(df_means_cons_traits_seed,file=paste(path,"/papers/reg_report/results/df_means_cons_traits_seed.Rdata",sep="/"))

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
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
  
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

df_prod_traits_seed_pool <- df_res_pool
df_prod_traits_seed <- df_res
df_means_prod_traits_seed <- df_means_out


save(df_prod_traits_seed_pool,file=paste(path,"/papers/reg_report/results/df_prod_traits_seed_pool.Rdata",sep="/"))
save(df_prod_traits_seed,file=paste(path,"/papers/reg_report/results/df_prod_traits_seed.Rdata",sep="/"))
save(df_means_prod_traits_seed,file=paste(path,"/papers/reg_report/results/df_means_prod_traits_seed.Rdata",sep="/"))

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
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
  
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

df_compare_cons_traits_seed_pool <- df_res_pool
df_compare_cons_traits_seed <- df_res
df_means_compare_cons_traits_seed <- df_means_out


save(df_compare_cons_traits_seed_pool,file=paste(path,"/papers/reg_report/results/df_compare_cons_traits_seed_pool.Rdata",sep="/"))
save(df_compare_cons_traits_seed,file=paste(path,"/papers/reg_report/results/df_compare_cons_traits_seed.Rdata",sep="/"))
save(df_means_compare_cons_traits_seed,file=paste(path,"/papers/reg_report/results/df_means_compare_cons_traits_seed.Rdata",sep="/"))


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
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
  
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

df_compare_prod_traits_seed_pool <- df_res_pool
df_compare_prod_traits_seed <- df_res
df_means_compare_prod_traits_seed <- df_means_out


save(df_compare_prod_traits_seed_pool,file=paste(path,"/papers/reg_report/results/df_compare_prod_traits_seed_pool.Rdata",sep="/"))
save(df_compare_prod_traits_seed,file=paste(path,"/papers/reg_report/results/df_compare_prod_traits_seed.Rdata",sep="/"))
save(df_means_compare_prod_traits_seed,file=paste(path,"/papers/reg_report/results/df_means_compare_prod_traits_seed.Rdata",sep="/"))
  
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
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
  
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

df_used_ph_pool <- df_res_pool
df_used_ph <- df_res
df_means_used_ph <- df_means_out


save(df_used_ph_pool,file=paste(path,"/papers/reg_report/results/df_used_ph_pool.Rdata",sep="/"))
save(df_used_ph,file=paste(path,"/papers/reg_report/results/df_used_ph.Rdata",sep="/"))
save(df_means_used_ph,file=paste(path,"/papers/reg_report/results/df_means_used_ph.Rdata",sep="/"))

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
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
  
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

df_compare_ph_pool <- df_res_pool
df_compare_ph <- df_res
df_means_compare_ph <- df_means_out


save(df_compare_ph_pool,file=paste(path,"/papers/reg_report/results/df_compare_ph_pool.Rdata",sep="/"))
save(df_compare_ph,file=paste(path,"/papers/reg_report/results/df_compare_ph.Rdata",sep="/"))
save(df_means_compare_ph,file=paste(path,"/papers/reg_report/results/df_means_compare_ph.Rdata",sep="/"))

## table with impact on decision making

dta$who1 <- dta$who1 != "2"
dta$who2 <- dta$who2 != "2"


#iterate over outcomes
outcomes <- c("who1","who2")


dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
  
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

df_decision_pool <- df_res_pool
df_decision <- df_res
df_means_decision <- df_means_out


save(df_decision_pool,file=paste(path,"/papers/reg_report/results/df_decision_pool.Rdata",sep="/"))
save(df_decision,file=paste(path,"/papers/reg_report/results/df_decision.Rdata",sep="/"))
save(df_means_decision,file=paste(path,"/papers/reg_report/results/df_means_decision.Rdata",sep="/"))

## table with impact on decision making

dta$perc_cons <- as.numeric(dta$bag_keep)/as.numeric(dta$bag_harv)*100
dta$perc_cons[dta$perc_cons>100] <- NA
dta$perc_sell <- as.numeric(as.character(dta$bag_sell))/as.numeric(dta$bag_harv)*100
dta$perc_sell[is.na(dta$perc_sell)] <- 0
dta$perc_sell[dta$perc_sell>100] <- NA
dta$perc_seed <- as.numeric(as.character(dta$seed_keep))/as.numeric(dta$harv_kgs)*100
dta$perc_seed[dta$perc_seed>50] <- NA

#iterate over outcomes
outcomes <- c("perc_cons","perc_sell","perc_seed")


dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
  
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

df_disposal_pool <- df_res_pool
df_disposal <- df_res
df_means_disposal <- df_means_out


save(df_disposal_pool,file=paste(path,"/papers/reg_report/results/df_disposal_pool.Rdata",sep="/"))
save(df_disposal,file=paste(path,"/papers/reg_report/results/df_disposal.Rdata",sep="/"))
save(df_means_disposal,file=paste(path,"/papers/reg_report/results/df_means_disposal.Rdata",sep="/"))

## table with wellfare and food security

dta$in_com[dta$in_com == "98" ] <- NA
dta$better_off_vil <- dta$in_com == "1"
dta$in_six[dta$in_six == "98"] <- NA
dta$better_off_six <- dta$in_six == "1"
dta$food_secure_pref <- dta$fd_pref =="No"
dta$food_secure_quant <-dta$fd_less =="No"


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


#iterate over outcomes
outcomes <- c("better_off_vil","better_off_six","food_secure_pref","food_secure_quant","cons_exp")


dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
outcomes <- c(outcomes, "index")
## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,4,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
  
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

df_welfare_pool <- df_res_pool
df_welfare <- df_res
df_means_welfare <- df_means_out


save(df_welfare_pool,file=paste(path,"/papers/reg_report/results/df_welfare_pool.Rdata",sep="/"))
save(df_welfare,file=paste(path,"/papers/reg_report/results/df_welfare.Rdata",sep="/"))
save(df_means_welfare,file=paste(path,"/papers/reg_report/results/df_means_welfare.Rdata",sep="/"))