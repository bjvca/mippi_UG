
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

path <- strsplit(path,"papers/reg_report/analysis")[[1]]
#read in baseline data
bse <- read.csv(paste(path,"baseline/data/public/baseline.csv",sep="/"))
bse$cluster_ID <- as.factor(paste(paste(bse$distID,bse$subID, sep="_"), bse$vilID, sep="_"))

### read in test data - this needs to be replace when we get the real data

dta <- read.csv(paste(path,"endline/data/dummy/dummy.csv", sep="/"))
## drop non-free trial packs and non-free + discount trial packs
dta <- subset(dta, paid_pac == FALSE & discounted == FALSE)
## create an indicator for pure control to standardize the indices (sgroup in icsIndex)
dta$s_ind <- (!dta$cont & !dta$trial_P)
#merge to baseline data
dta <-merge(dta, bse[c("farmer_ID","cluster_ID")], by.x="ID", by.y="farmer_ID")

## primary outcome 1: uses improved seed on at least one plot (fresh hybrid from formal source or OPV that is used 3 times max from formal source) - increase for max number of plots
dta$p_outcome_1 <- (((dta$plot.1..plot_imp_type %in%  
  c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta$plot.1..plot_times_rec %in% 1) & (dta$plot.1..source %in% letters[4:9])  ) |
((dta$plot.1..plot_imp_type %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3")) & (dta$plot.1..plot_times_rec %in% 1:3) &  (dta$plot.1..source %in% letters[4:9]))) |
(((dta$plot.2..plot_imp_type %in%  
     c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta$plot.2..plot_times_rec %in% 1) & (dta$plot.2..source %in% letters[4:9])  ) |
    ((dta$plot.2..plot_imp_type %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3")) & (dta$plot.2..plot_times_rec %in% 1:3) &  (dta$plot.2..source %in% letters[4:9])))

## to control for this outcome at baseline, we use the question "Q20. Did you use any quality maize seed like **OPV or hybrid seed** in the previous season (Nsambya of 2022) on any of your plots?"
bse$b_p_outcome_1 <- bse$quality_use=="Yes"

dta <- merge(dta, bse[c("farmer_ID","b_p_outcome_1")], by.x="ID", by.y="farmer_ID")

## primary outcome 2: uses fresh bazooka on at least one plot
dta$p_outcome_2 <- ((dta$plot.1..plot_imp_type=="Bazooka") & (dta$plot.1..plot_times_rec %in% 1) & (dta$plot.1..source %in% letters[4:9]))  |
  ((dta$plot.2..plot_imp_type=="Bazooka") & (dta$plot.2..plot_times_rec %in% 1) & (dta$plot.2..source %in% letters[4:9])) 
## to control for this outcome at baseline, we use the question "Q20. Did you use any quality maize seed like **OPV or hybrid seed** in the previous season (Nsambya of 2022) on any of your plots?"
bse$b_p_outcome_2 <- bse$bazo_use=="Yes"

dta <- merge(dta, bse[c("farmer_ID","b_p_outcome_2")], by.x="ID", by.y="farmer_ID")

## number of plots under improved maize cultivation
dta$nr_improved <- (((dta$plot.1..plot_imp_type %in%  
                        c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta$plot.1..plot_times_rec %in% 1) & (dta$plot.1..source %in% letters[4:9])  ) |
                      ((dta$plot.1..plot_imp_type %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3")) & (dta$plot.1..plot_times_rec %in% 1:3) &  (dta$plot.1..source %in% letters[4:9]))) +
  (((dta$plot.2..plot_imp_type %in%  
       c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta$plot.2..plot_times_rec %in% 1) & (dta$plot.2..source %in% letters[4:9])  ) |
     ((dta$plot.2..plot_imp_type %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3")) & (dta$plot.2..plot_times_rec %in% 1:3) &  (dta$plot.2..source %in% letters[4:9])))
##area under improved maize cultivation
dta$nr_improvedxsize <- rowSums(cbind((((dta$plot.1..plot_imp_type %in%  
                             c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta$plot.1..plot_times_rec %in% 1) & (dta$plot.1..source %in% letters[4:9])  ) |
                           ((dta$plot.1..plot_imp_type %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3")) & (dta$plot.1..plot_times_rec %in% 1:3) &  (dta$plot.1..source %in% letters[4:9])))*dta$plot.1..plot_size ,
  (((dta$plot.2..plot_imp_type %in%  
       c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta$plot.2..plot_times_rec %in% 1) & (dta$plot.2..source %in% letters[4:9])  ) |
     ((dta$plot.2..plot_imp_type %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3")) & (dta$plot.2..plot_times_rec %in% 1:3) &  (dta$plot.2..source %in% letters[4:9])))*dta$plot.2..plot_size),na.rm=TRUE)

## total area under maize cultivation
dta$totsize <- rowSums(dta[c("plot.1..plot_size" ,"plot.2..plot_size")], na.rm=TRUE)

##share of plots under improved cultivation
dta$share_plots_imp <-  dta$nr_improved/dta$plot_no
## share of area under improved cultivation
dta$share_area_imp <-  dta$nr_improvedxsize/dta$totsize


#iterate over outcomes
outcomes <- c("p_outcome_1","p_outcome_2","nr_improved", "share_plots_imp", "nr_improvedxsize", "share_area_imp" )
b_outcomes <- c("b_p_outcome_1", "b_p_outcome_2",NA,NA)

dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]), sgroup=dta$s_ind)$index
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
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
  df_res[1,4,i] <- nobs(ols) 
  } else {
  formula1 <- as.formula(paste(paste(outcomes[i],paste("trial_P*cont"),sep="~"), b_outcomes[i],sep="+"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
  
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
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  if (i %in% 3:7) {   
  formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  } else {
  formula3 <- as.formula(paste(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"), b_outcomes[i],sep="+"))    
  }  
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
}

save(df_res_pool,file=paste(path,"/papers/reg_report/results/df_res_pool.Rdata",sep="/"))
save(df_res,file=paste(path,"/papers/reg_report/results/df_res.Rdata",sep="/"))
save(df_means_out,file=paste(path,"/papers/reg_report/results/df_means_out.Rdata",sep="/"))

### on random plot (controling for baseline)

dta$rnd_adopt <-   (((dta$maize_var_selected %in%  
                        c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka", "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")) & (dta$times_rec_selected %in% 1) & (dta$source_selected %in% letters[4:9])  ) |
                      ((dta$maize_var_selected  %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3")) & (dta$times_rec_selected %in% 1:3) &  (dta$source_selected %in% letters[4:9])))

## we assume here that seed from official sources has not been recycled
bse$b_rnd_adopt <- (((bse$maize_var  %in% c("Longe_10H"," Longe_7H","Longe_7R_Kayongo-go", "Bazooka","DK","Longe_6H")) & (bse$source %in% letters[4:9]) )
                  |
                    ((bse$maize_var  %in% c("Longe_5","Longe_4"," Panner", "Wema","KH_series"))  & (bse$source %in% letters[4:9]) ))
dta <- merge(dta, bse[c("farmer_ID","b_rnd_adopt")], by.x="ID", by.y="farmer_ID")

dta$rnd_bazo <-  ((dta$maize_var_selected == "Bazooka") & (dta$source_selected %in% letters[4:9]  & (dta$times_rec_selected %in% 1)))
## we assume here that seed from official sources has not been recycled
bse$b_rnd_bazo <- ((bse$maize_var == "Bazooka") & (bse$source %in% letters[4:9]) )
                   
dta <- merge(dta, bse[c("farmer_ID","b_rnd_bazo")], by.x="ID", by.y="farmer_ID")

### seed quantity
dta$imp_seed_qty_rnd <- dta$rnd_adopt*dta$seed_qty
dta$imp_seed_qty_rnd[is.na(dta$imp_seed_qty_rnd)] <- 0
bse$b_imp_seed_qty_rnd <- bse$b_rnd_adopt*bse$seed_qty
bse$b_imp_seed_qty_rnd[is.na(bse$b_imp_seed_qty_rnd)] <- 0
dta <- merge(dta, bse[c("farmer_ID","b_imp_seed_qty_rnd")], by.x="ID", by.y="farmer_ID")
### seed quantity per area
dta$imp_seed_qty_rnd_acre <- dta$imp_seed_qty_rnd/dta$size_selected  
bse$b_imp_seed_qty_rnd_acre <- bse$b_imp_seed_qty_rnd/bse$plot_size
dta <- merge(dta, bse[c("farmer_ID","b_imp_seed_qty_rnd_acre")], by.x="ID", by.y="farmer_ID")
###production
dta$production <- dta$bag_harv*dta$bag_kg
bse$b_production <- bse$bag_harv*bse$bag_kg
dta <- merge(dta, bse[c("farmer_ID","b_production")], by.x="ID", by.y="farmer_ID")
## productivity
dta$productivity <- dta$production/dta$size_selected 
bse$b_productivity <-bse$b_production/bse$plot_size
dta <- merge(dta, bse[c("farmer_ID","b_productivity")], by.x="ID", by.y="farmer_ID")
#iterate over outcomes
outcomes <- c("rnd_adopt", "rnd_bazo", "imp_seed_qty_rnd", "imp_seed_qty_rnd_acre","production", "productivity" )
b_outcomes <- c("b_rnd_adopt", "b_rnd_bazo","b_imp_seed_qty_rnd","b_imp_seed_qty_rnd_acre", "b_production", "b_productivity")

dta$index <- icwIndex(xmat= as.matrix(dta[outcomes]),sgroup=dta$s_ind)$index
dta$b_index <- icwIndex(xmat= as.matrix(dta[b_outcomes]),sgroup=dta$s_ind)$index
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
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[c(2:3,5)]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[c(2:3,5)]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[c(2:3,5)]
  df_res[1,4,i] <- nobs(ols) 
  
  formula2 <- as.formula(paste(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"), b_outcomes[i],sep="+"))
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"), b_outcomes[i],sep="+"))
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
  
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


  