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
  


path <- strsplit(path,"papers/seed_free_or_not")[[1]]
dta <- read.csv(paste(path,"midline/data/public/midline.csv",sep="/"))

##create unique village level identifier for clustering of standard errors
dta$cluster_ID <- as.factor(paste(paste(dta$dist_ID,dta$sub_ID, sep="_"), dta$vil_ID, sep="_"))

dta$used_TP <- dta$used_TP == "Yes"
dta$remembers <- dta$Rec_TP == "Yes" |  dta$Buy_TP  == "Yes"
dta$TP_separate <- dta$TP_separate == 1
dta$use_fert <- dta$org_app== "Yes" | dta$dap_app== "Yes" | dta$ure_app== "Yes"

### HERE simulate data - remove if real data comes in 
dta <- sample_n(dta, size=1170,replace = TRUE)

dta$sim_treat <- c(rep("trial",390),rep("paid",390),rep("discount",390))
dta$paid_pac <- dta$sim_treat=="paid" 
dta$discounted <- dta$sim_treat=="discount"
dta$trial_P <-  dta$sim_treat=="trial"

#define paid_pack and discounted as incremental contrasts
dta$paid_pac[dta$discounted==TRUE] <- TRUE 
dta$layout <- dta$layout == 3
#iterate over outcomes
outcomes <- c("remembers","used_TP","TP_separate","layout" )

index_use <- icwIndex(xmat=dta[outcomes]) #x
dta <- data.frame(dta,index_use)

outcomes <- c("remembers","used_TP","TP_separate","index","layout" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,3,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[dta$trial_P==TRUE,outcomes])
res_tab[2,1,1:length(outcomes)] <- apply(dta[dta$trial_P==TRUE,outcomes], 2, sd)

for (i in 1:length(outcomes)) {
  ### pooled regression (for marginal effects)
 ols <- lm(as.formula( paste(outcomes[i],"paid_pac+discounted",sep="~")), data=dta)

 
 vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
 
  res_tab[1,2,i]  <- coef_test(ols, vcov_cluster)$beta[2]
  res_tab[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  res_tab[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
  res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
  res_tab[3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]
  
  
}
res_tab <- round(res_tab,digits=3)
#res_tab <- round(res_tab,digits=3)
save(res_tab, file=paste(path,"papers/seed_free_or_not/res_tab.Rdata",sep="/"))

