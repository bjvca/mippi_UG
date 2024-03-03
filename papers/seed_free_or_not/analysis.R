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
#get baseline data
path <- strsplit(path,"papers/seed_free_or_not")[[1]]
#create treatmetn cluster indicator for clustering SE
bse <- read.csv(paste(path,"baseline/data/public/baseline.csv",sep="/"))
bse$cluster_ID <- as.factor(paste(paste(bse$distID,bse$subID, sep="_"), bse$vilID, sep="_"))

## for this paper, keep only the 1000 or so obs that received seed for free (and are not allocated to crossed treatment), paid for seed or paid and got discount

bse <- subset(bse, (paid_pac==TRUE | discounted == TRUE) & cont == FALSE)

bse$age_head <- as.numeric(as.character(bse$age))
bse$prim_head <-bse$edu %in% c("c","d","e","f")
bse$male_head <- bse$gender == "Male"
bse$hh_size <- as.numeric(as.character(bse$hh_size))
bse$dist_ag <- as.numeric(as.character(bse$dist_ag))
bse$quality_use <- bse$quality_use=="Yes"
bse$bazooka_use_rand <- bse$maize_var=="Bazooka"
bse$source_rand <- bse$source %in%  letters[seq( from = 4, to = 9 )]
bse$often_rand <-  bse$often %in%  letters[seq( from = 1, to = 5 )]
bse$bag_harv[bse$bag_harv == "999"] <- NA
bse$prod_rand <-  as.numeric(as.character(bse$bag_harv))* as.numeric(as.character(bse$bag_kg))
bse$acre_rand <- as.numeric(as.character(bse$plot_size))
bse$yield_rand <- bse$prod_rand/bse$acre_rand
bse <- trim("yield_rand",bse,trim_perc=.01)
bse$yield_rand_ihs <- ihs(bse$yield_rand )


###balance table
#iterate over outcomes
outcomes <- c("age_head","prim_head","male_head","hh_size","dist_ag","quality_use","bazooka_use_rand","source_rand","often_rand","yield_rand_ihs" )


#matrix to store results
res_tab <-  array(NA,dim=c(3,4,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(bse[bse$trial_P==TRUE,outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(bse[bse$trial_P==TRUE,outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### pooled regression (for marginal effects)
  ols <- lm(as.formula( paste(outcomes[i],"paid_pac+discounted",sep="~")), data=bse)
  
  
  vcov_cluster <- vcovCR(ols,cluster=bse$cluster_ID,type="CR3")
  
  res_tab[1,2,i]  <- coef_test(ols, vcov_cluster)$beta[2]
  res_tab[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  res_tab[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
  res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
  res_tab[3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]
  res_tab[1,4,i] <- nobs(ols)
  
}
base_balance <- round(res_tab,digits=3)
#res_tab <- round(res_tab,digits=3)
save(base_balance, file=paste(path,"papers/seed_free_or_not/base_balance.Rdata",sep="/"))

### here analysis of midline data starts

dta <- read.csv(paste(path,"midline/data/public/midline.csv",sep="/"))

dta <- subset(dta, !(trial_P))

## merge in randomized staring price
dta <- merge(dta, bse[c("farmer_ID","P1_pric")], by.x="ID", by.y="farmer_ID", all.x=TRUE)


##create unique village level identifier for clustering of standard errors
dta$cluster_ID <- as.factor(paste(paste(dta$dist_ID,dta$sub_ID, sep="_"), dta$vil_ID, sep="_"))

dta$used_TP <- dta$used_TP == "Yes"
dta$remembers <- dta$Rec_TP == "Yes" |  dta$Buy_TP  == "Yes"
dta$TP_separate <- dta$TP_separate == 1

dta$cor_plant <- dta$space==2 & dta$seed_no==2
dta$use_fert_inorg <-  dta$dap_app== "Yes" | dta$ure_app== "Yes"
dta$use_fert_org <-  dta$org_app== "Yes" 
dta$use_chem <-  dta$cide_use== "Yes" 
dta$gap_fill <-  dta$resow== "Yes"
dta$nr_weed <- as.numeric(as.character(dta$weed_no))
dta$nr_weed[dta$nr_weed == 999] <- NA
dta$plant_date <- as.numeric(as.character(dta$plant_date))
dta$plant_date[dta$plant_date == 999] <- NA
dta$timely_planting <- dta$plant_date < 5
dta$sep_post_harvest <- dta$sep_post_harvest=="Yes"

dta$happy_yield <- dta$happy_yield == 1 
dta$happy_drought <- dta$happy_drought ==1
dta$happy_disease <- dta$happy_disease == 1
dta$happy_germinate <- dta$happy_germinate ==1
dta$happy <- dta$happy == 1

dta$screening <- (as.numeric(as.character(dta$price_paid)))/1000
dta$signaling <- (as.numeric(as.character(dta$P1_pric)))/1000
dta$sunk <- dta$discounted

dta$d_screening <- dta$screening - mean(dta$screening, na.rm=TRUE)
dta$d_signaling <- dta$signaling - mean(dta$signaling, na.rm=TRUE)
dta$d_sunk <- dta$sunk - mean(dta$sunk, na.rm=TRUE)
### HERE simulate data - remove if real data comes in 
#dta <- sample_n(dta, size=1170,replace = TRUE)

#dta$sim_treat <- c(rep("trial",390),rep("paid",390),rep("discount",390))
#dta$paid_pac <- dta$sim_treat=="paid"
#dta$discounted <- dta$sim_treat=="discount"
#dta$trial_P <-  dta$sim_treat=="trial"
#### remove this part ^^^^


#define paid_pack and discounted as incremental contrasts
#dta$paid_pac[dta$discounted==TRUE] <- TRUE 
dta$layout <- dta$layout == 3

###table 1 - impact on use
#iterate over outcomes
outcomes <- c("used_TP","TP_separate","layout","sep_post_harvest" )

index_use <- icwIndex(xmat=dta[outcomes]) #x
dta <- data.frame(dta,index_use)
names(dta)[names(dta) == 'index'] <- 'index_use'

outcomes <- c(outcomes,"index_use" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### pooled regression (for marginal effects)
 ols <- lm(as.formula( paste(outcomes[i],"screening+d_signaling+d_sunk",sep="~")), data=dta)

 
 vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
 
  res_tab[1,2,i]  <- coef_test(ols, vcov_cluster)$beta[2]
  res_tab[2,2,i] <- coef_test(ols, vcov_cluster)$SE[2]
  res_tab[3,2,i] <- coef_test(ols, vcov_cluster)$p_Satt[2]
  
  ols <- lm(as.formula( paste(outcomes[i],"d_screening+signaling+d_sunk",sep="~")), data=dta)
  
  
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
  
  res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
  res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
  res_tab[3,3,i] <- coef_test(ols, vcov_cluster)$p_Satt[3]
  ols <- lm(as.formula( paste(outcomes[i],"d_screening+d_signaling+sunk",sep="~")), data=dta)
  
  
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR3")
  
  res_tab[1,4,i]  <- coef_test(ols, vcov_cluster)$beta[4]
  res_tab[2,4,i] <- coef_test(ols, vcov_cluster)$SE[4]
  res_tab[3,4,i] <- coef_test(ols, vcov_cluster)$p_Satt[4]
  
   res_tab[1,5,i] <- nobs(ols)
  
}

#res_tab <- round(res_tab,digits=3)
save(res_tab, file=paste(path,"papers/seed_free_or_not/res_tab.Rdata",sep="/"))

#table 2: impact on practices

#iterate over outcomes
outcomes <- c("cor_plant","use_fert_inorg","use_fert_org","use_chem","gap_fill","nr_weed","timely_planting")

index_use <- icwIndex(xmat=dta[outcomes],sgroup = dta$trial_P) #x
dta <- data.frame(dta,index_use)
names(dta)[names(dta) == 'index'] <- 'index_pract'

outcomes <- c(outcomes,"index_pract" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,4,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[dta$trial_P==TRUE,outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[dta$trial_P==TRUE,outcomes], 2, sd, na.rm=T)

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
  res_tab[1,4,i] <- nobs(ols)
  
}
res_tab_pract <- round(res_tab,digits=3)
save(res_tab_pract, file=paste(path,"papers/seed_free_or_not/res_tab_pract.Rdata",sep="/"))

#table 3: impact on characteristics

#iterate over outcomes
outcomes <- c("happy_yield",
"happy_drought",
"happy_disease","happy_germinate", "happy")

index_use <- icwIndex(xmat=dta[outcomes],sgroup = dta$trial_P) #x
dta <- data.frame(dta,index_use)
names(dta)[names(dta) == 'index'] <- 'index_char'

outcomes <- c(outcomes,"index_char" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,4,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[dta$trial_P==TRUE,outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[dta$trial_P==TRUE,outcomes], 2, sd, na.rm=T)

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
  res_tab[1,4,i] <- nobs(ols)
  
}
res_tab_char <- round(res_tab,digits=3)
save(res_tab_char, file=paste(path,"papers/seed_free_or_not/res_tab_char.Rdata",sep="/"))

#table 4: impact on yield
dta$area_tot <- as.numeric(as.character(dta$plot_size_all))
dta$area_tot[is.na(dta$area_tot)] <- as.numeric(as.character(dta$plot_size_all_no_rem[is.na(dta$area_tot)]))

dta$bags_tot <- as.numeric(as.character(dta$bags_all))
dta$bags_tot[is.na(dta$bags_tot)] <- as.numeric(as.character(dta$bags_all_no_rem[is.na(dta$bags_tot)]))

dta$bag_size_tot <- as.numeric(as.character(dta$bag_size_all))
dta$bag_size_tot[is.na(dta$bag_size_tot)] <- as.numeric(as.character(dta$bag_size_all_no_rem[is.na(dta$bag_size_tot)]))

dta$prod_kg_tot <- dta$bag_size_tot*dta$bags_tot

dta$yield_tot <- dta$prod_kg_tot/dta$area_tot
#
dta <- trim("prod_kg_tot",dta,trim_perc=.01)
dta <- trim("yield_tot",dta,trim_perc=.01)
dta$yield_tot_ihs <- ihs(dta$yield_tot)
dta$prod_kg_tot_ihs  <- ihs(dta$prod_kg_tot)

dta$area_trial <- as.numeric(as.character(dta$plot_size))
dta$bags_trial <- as.numeric(as.character(dta$bags))
dta$bag_size_trial <- as.numeric(as.character(dta$bag_size))

dta$prod_kg_trial <- dta$bag_size_trial*dta$bags_trial
dta$yield_trial <- dta$prod_kg_trial/dta$area_trial
dta <- trim("prod_kg_trial",dta,trim_perc=.01)
dta <- trim("yield_trial",dta,trim_perc=.01)
dta$yield_trial_ihs <- ihs(dta$yield_trial)
dta$prod_kg_trial_ihs  <- ihs(dta$prod_kg_trial)

#iterate over outcomes
outcomes <- c("area_tot",
              "prod_kg_tot_ihs",
              "yield_tot_ihs","area_trial",
              "prod_kg_trial_ihs",
              "yield_trial_ihs")

index_use <- icwIndex(xmat=dta[outcomes],sgroup = dta$trial_P) #x
dta <- data.frame(dta,index_use)

names(dta)[names(dta) == 'index'] <- 'index_yield'

outcomes <- c(outcomes,"index_yield" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,4,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[dta$trial_P==TRUE,outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[dta$trial_P==TRUE,outcomes], 2, sd, na.rm=T)

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
  res_tab[1,4,i] <- nobs(ols)
  
}
res_tab_yield <- round(res_tab,digits=3)
save(res_tab_yield, file=paste(path,"papers/seed_free_or_not/res_tab_yield.Rdata",sep="/"))

#table 4: intentions
dta$plan_imp <- (dta$seed_nxt == 1 |  dta$seed_nxt == 2) 
dta$plan_bazooka <- dta$imp_var.Bazooka == "True"
dta$plan_bought <- dta$buy_plan=="Yes"
dta$plan_area <-  as.numeric(as.character(dta$area_plan))
dta$plan_area[dta$plan_area > 50] <- NA
#iterate over outcomes
outcomes <- c("plan_imp",
              "plan_bazooka",
              "plan_area",
              "plan_bought")

index_use <- icwIndex(xmat=dta[outcomes],sgroup = dta$trial_P) #x
dta <- data.frame(dta,index_use)

names(dta)[names(dta) == 'index'] <- 'index_plan'

outcomes <- c(outcomes,"index_plan" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,4,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[dta$trial_P==TRUE,outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[dta$trial_P==TRUE,outcomes], 2, sd, na.rm=T)

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
  res_tab[1,4,i] <- nobs(ols)
  
}
res_tab_plan <- round(res_tab,digits=3)
save(res_tab_plan, file=paste(path,"papers/seed_free_or_not/res_tab_plan.Rdata",sep="/"))


dta$remembers_seed <- dta$rem_sdType == "Yes"
dta$remembers_comp <- dta$rem_comp == "Yes"
dta$value_shop <- as.numeric(as.character(dta$value))
dta$value_paid <- as.numeric(as.character(dta$price_paid))

#iterate over outcomes
outcomes <- c("remembers_seed",
              "remembers_comp",
              "value_shop")

index_use <- icwIndex(xmat=dta[outcomes],sgroup = dta$trial_P) #x
dta <- data.frame(dta,index_use)

names(dta)[names(dta) == 'index'] <- 'index_plan'

outcomes <- c(outcomes,"value_paid", "index_plan" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,4,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[dta$trial_P==TRUE,outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[dta$trial_P==TRUE,outcomes], 2, sd, na.rm=T)

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
  res_tab[1,4,i] <- nobs(ols)
  
}
## for subset of those that bought
res_tab[1,1, 4] <- mean(dta$value_paid[dta$paid_pac==TRUE & dta$discounted==FALSE], na.rm=TRUE)
res_tab[2,1, 4] <- sd(dta$value_paid[dta$paid_pac==TRUE & dta$discounted==FALSE], na.rm=TRUE)

res_tab_path <- round(res_tab,digits=3)
save(res_tab_path, file=paste(path,"papers/seed_free_or_not/res_tab_path.Rdata",sep="/"))



### additional analysis - on subset
### does farmer remember price paid better if no discount?
### pull price paid from baseline data
### calculate difference between price paid and recall
### compare between paid and paid+discount

