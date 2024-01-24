
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

bse <- read.csv(paste(path,"baseline/data/public/baseline.csv",sep="/"))
bse$cluster_ID <- as.factor(paste(paste(bse$distID,bse$subID, sep="_"), bse$vilID, sep="_"))#read in data

### read in test data - this needs to be replace when we get the real data

dta <- read.csv(paste(path,"endline/data/dummy/dummy.csv", sep="/"))
## drop non-free trial packs and non-free + discount trial packs
dta <- subset(dta, paid_pac == FALSE & discounted == FALSE)

dta <-merge(dta, bse[c("farmer_ID","cluster_ID")], by.x="ID", by.y="farmer_ID")

## uses improved seed on at least one plot


dta$p_outcome_1 <- ((dta$plot.1..plot_imp_type %in% c("Longe_10H","Longe_7H","Longe_7R_Kayongo-go", "Bazooka","DK","Longe_6H") & (dta$plot.1..source %in% letters[4:9]) & (dta$plot.1..plot_times_rec == 1)) |
  (dta$plot.1..plot_imp_type %in% c("Longe_5","Longe_4","Panner", "Wema","KH_series")  & (dta$plot.1..source %in% letters[4:9]) & (dta$plot.1..plot_times_rec %in% 1:3))) |
((dta$plot.2..plot_imp_type %in% c("Longe_10H","Longe_7H","Longe_7R_Kayongo-go", "Bazooka","DK","Longe_6H") & (dta$plot.2..source %in% letters[4:9]) & (dta$plot.2..plot_times_rec == 1)) |
    (dta$plot.2..plot_imp_type %in% c("Longe_5","Longe_4","Panner", "Wema","KH_series")  & (dta$plot.2..source %in% letters[4:9]) & (dta$plot.2..plot_times_rec %in% 1:3)))
dta$p_outcome_2 <- dta$plot.1..plot_imp_type=="Bazooka" | dta$plot.2..plot_imp_type=="Bazooka"

dta$nr_improved <- ((dta$plot.1..plot_imp_type %in% c("Longe_10H","Longe_7H","Longe_7R_Kayongo-go", "Bazooka","DK","Longe_6H") & (dta$plot.1..source %in% letters[4:9]) & (dta$plot.1..plot_times_rec == 1)) |
    (dta$plot.1..plot_imp_type %in% c("Longe_5","Longe_4","Panner", "Wema","KH_series")  & (dta$plot.1..source %in% letters[4:9]) & (dta$plot.1..plot_times_rec %in% 1:3))) +
  ((dta$plot.2..plot_imp_type %in% c("Longe_10H","Longe_7H","Longe_7R_Kayongo-go", "Bazooka","DK","Longe_6H") & (dta$plot.2..source %in% letters[4:9]) & (dta$plot.2..plot_times_rec == 1)) |
     (dta$plot.2..plot_imp_type %in% c("Longe_5","Longe_4","Panner", "Wema","KH_series")  & (dta$plot.2..source %in% letters[4:9]) & (dta$plot.2..plot_times_rec %in% 1:3)))

dta$nr_improvedxsize <- ((dta$plot.1..plot_imp_type %in% c("Longe_10H","Longe_7H","Longe_7R_Kayongo-go", "Bazooka","DK","Longe_6H") & (dta$plot.1..source %in% letters[4:9]) & (dta$plot.1..plot_times_rec == 1)) |
                      (dta$plot.1..plot_imp_type %in% c("Longe_5","Longe_4","Panner", "Wema","KH_series")  & (dta$plot.1..source %in% letters[4:9]) & (dta$plot.1..plot_times_rec %in% 1:3)))*dta$plot.1..plot_size +
  ((dta$plot.2..plot_imp_type %in% c("Longe_10H","Longe_7H","Longe_7R_Kayongo-go", "Bazooka","DK","Longe_6H") & (dta$plot.2..source %in% letters[4:9]) & (dta$plot.2..plot_times_rec == 1)) |
     (dta$plot.2..plot_imp_type %in% c("Longe_5","Longe_4","Panner", "Wema","KH_series")  & (dta$plot.2..source %in% letters[4:9]) & (dta$plot.2..plot_times_rec %in% 1:3)))*dta$plot.2..plot_size

dta$totsize <- dta$plot.1..plot_size +dta$plot.2..plot_size

dta$share_plots_imp <-  dta$nr_improved/dta$plot_no
dta$share_area_imp <-  dta$nr_improvedxsize/dta$totsize

#iterate over outcomes
outcomes <- c("p_outcome_1","p_outcome_2", "share_plots_imp", "share_area_imp" )

## demean indicators
dta$cont_demeaned <-  dta$cont - mean(dta$cont,na.rm = T)
dta$trial_P_demeaned <-  dta$trial_P - mean(dta$trial_P,na.rm = T)
df_means_out <- array(NA,c(2,length(outcomes )))
df_res <- array(NA,c(3,3,length(outcomes )))
df_res_pool  <- array(NA,c(2,3,length(outcomes )))
for (i in 1:length(outcomes)){
  ##means
  
  df_means_out[1,i] <- mean(unlist(dta[outcomes[i]]), na.rm=TRUE)
  df_means_out[2,i] <- sd(unlist(dta[outcomes[i]]), na.rm=TRUE)
  
  formula1 <- as.formula(paste(outcomes[i],paste("trial_P*cont"),sep="~"))
  ols <- lm(formula1, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
  
  df_res[1:3,1,i] <- coef_test(ols, vcov_cluster)$beta[2:4]
  df_res[1:3,2,i] <- coef_test(ols, vcov_cluster)$SE[2:4]
  df_res[1:3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2:4]
  
  formula2 <- as.formula(paste(outcomes[i],paste("trial_P*cont_demeaned"),sep="~"))
  ols <- lm(formula2, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
  
  df_res_pool[1,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[1,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[1,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  formula3 <- as.formula(paste(outcomes[i],paste("cont*trial_P_demeaned"),sep="~"))
  ols <- lm(formula3, data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
  
  df_res_pool[2,1,i] <- coef_test(ols, vcov_cluster)$beta[2]
  df_res_pool[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  df_res_pool[2,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
}

save(df_res_pool,file=paste(path,"/papers/reg_report/results/df_res_pool.Rdata",sep="/"))
save(df_res,file=paste(path,"/papers/reg_report/results/df_res.Rdata",sep="/"))
save(df_means_out,file=paste(path,"/papers/reg_report/results/df_means_out.Rdata",sep="/"))
  