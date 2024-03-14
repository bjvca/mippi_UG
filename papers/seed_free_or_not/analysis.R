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



####create transaction price for bargaining game


##determine maximum bid price
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
##determine minimum ask price
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


bids <- cbind(as.numeric(bse$paid.P2_pric_2)- as.numeric(bse$paid.P2_pric), 
              as.numeric(bse$paid.P2_pric_3)- as.numeric(bse$paid.P2_pric_2) ,
              as.numeric(bse$paid.P2_pric_4)- as.numeric(bse$paid.P2_pric_3) ,
              as.numeric(bse$paid.P2_pric_5)- as.numeric(bse$paid.P2_pric_4) ,
              as.numeric(bse$paid.P2_pric_6)- as.numeric(bse$paid.P2_pric_5) ,
              as.numeric(bse$paid.P2_pric_7)- as.numeric(bse$paid.P2_pric_6) ,
              as.numeric(bse$paid.P2_pric_8)- as.numeric(bse$paid.P2_pric_7) ,
              as.numeric(bse$paid.P2_pric_9)- as.numeric(bse$paid.P2_pric_8) ,
              as.numeric(bse$paid.P2_pric_10)- as.numeric(bse$paid.P2_pric_9) ,
              as.numeric(bse$paid.P2_pric_11)- as.numeric(bse$paid.P2_pric_10))

bse$av_bid_step <- rowMeans(bids, na.rm=T)
###number of rounds after which farmer agrees
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



###number of rounds after which bidder accepts

bse$accepts <- "seller"
bse$accepts[bse$paid.start_neg=="Yes"| bse$paid.start_neg_2=="Yes" | bse$paid.start_neg_3=="Yes"
            | bse$paid.start_neg_4=="Yes" | bse$paid.start_neg_5=="Yes" | bse$paid.start_neg_6=="Yes"| bse$paid.start_neg_7=="Yes" 
            | bse$paid.start_neg_8=="Yes" | bse$paid.start_neg_9=="Yes" | bse$paid.start_neg_10=="Yes" | bse$paid.start_neg_11=="Yes"] <- "buyer"

##determine willingness to pay
bse$final_price <- NA
### if buyer accepts, this is the last ask price
bse$final_price[bse$accepts=="buyer"] <- bse$ask[bse$accepts=="buyer"]
### if seller accepts this is the last bid price
bse$final_price[bse$accepts=="seller"] <- bse$bid[bse$accepts=="seller"]

bse <- subset(bse, cont == FALSE & (trial_P== TRUE | paid_pac == TRUE | discounted == TRUE))


bse$age_head <- as.numeric(as.character(bse$age))
bse$age_head[bse$age_head==999] <- NA
bse$prim_head <-bse$edu %in% c("c","d","e","f")
bse$male_head <- bse$gender == "Male"
bse$hh_size <- as.numeric(as.character(bse$hh_size))
bse$dist_ag <- as.numeric(as.character(bse$dist_ag))
bse$dist_ag[bse$dist_ag==999] <- NA
bse$quality_use <- bse$quality_use=="Yes"
bse$bazooka_use_rand <- bse$maize_var=="Bazooka"
bse$source_rand <- bse$source %in%  letters[seq( from = 4, to = 9 )]
bse$often_rand <-  bse$often %in%  letters[seq( from = 1, to = 5 )]
bse$bag_harv[bse$bag_harv == "999"] <- NA
bse$prod_rand <-  as.numeric(as.character(bse$bag_harv))* as.numeric(as.character(bse$bag_kg))
bse$acre_rand <- as.numeric(as.character(bse$plot_size))
bse$acre_rand[bse$acre_rand==999] <- NA
bse$yield_rand <- bse$prod_rand/bse$acre_rand
bse <- trim("yield_rand",bse,trim_perc=.01)
bse$yield_rand_ihs <- ihs(bse$yield_rand )

##merge in price paid from midline data (this was calculated and pulled into the app at midline)



bse$screening <- (as.numeric(as.character(bse$final_price)))/1000
bse$signaling <- (as.numeric(as.character(bse$P1_pric)))/1000
##important: it is the non-discounted (those that pay the full price) that identify the sunk cost
##they are the filtered ones that also pay a postive price
bse$sunk <- as.numeric(!bse$discounted)*bse$screening

bse$d_screening <- bse$screening - mean(bse$screening, na.rm=TRUE)
bse$d_signaling <- bse$signaling - mean(bse$signaling, na.rm=TRUE)
bse$d_sunk <- bse$sunk - mean(bse$sunk, na.rm=TRUE)

###balance table
#iterate over outcomes
outcomes <- c("age_head","prim_head","male_head","hh_size","dist_ag","quality_use","bazooka_use_rand","source_rand","often_rand","yield_rand_ihs" )


#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(bse[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(bse[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=bse[!bse$trial,])
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")),  data=bse[!bse$trial,])

  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")),  data=bse[!bse$trial,])
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
}
base_balance <- round(res_tab,digits=3)
#res_tab <- round(res_tab,digits=3)
save(base_balance, file=paste(path,"papers/seed_free_or_not/base_balance.Rdata",sep="/"))

### now run binary analysis 

##important: it is the non-discounted (those that pay the full price) that identify the sunk cost
##they are the filtered ones that also pay a postive price



###balance table
#iterate over outcomes
outcomes <- c("age_head","prim_head","male_head","hh_size","dist_ag","quality_use","bazooka_use_rand","source_rand","often_rand","yield_rand_ihs" )


#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))


for (i in 1:length(outcomes)) {
  ### here we need clustering
  ols <- lm(as.formula( paste(outcomes[i],"discounted+trial_P",sep="~")), data=bse)
  vcov_cluster <- vcovCR(ols,cluster=bse$cluster_ID,type="CR2")
  coef_test(ols, vcov_cluster)$beta
  
  res_tab[1,1,i] <-  coef_test(ols, vcov_cluster)$beta[1]
  res_tab[2,1,i] <- coef_test(ols, vcov_cluster)$SE[1]
  
  res_tab[1,2,i]  <-  coef_test(ols, vcov_cluster)$beta[2]
  res_tab[2,2,i] <-  coef_test(ols, vcov_cluster)$SE[2]
  res_tab[3,2,i] <-  coef_test(ols, vcov_cluster)$p_Satt[2]
  ### regression for sunk cost effect - clustering
  
  
  res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
  res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
  res_tab[3,3,i] <-  coef_test(ols, vcov_cluster)$p_Satt[3]
  
  lh <-   linearHypothesis(ols, c("discountedTRUE = trial_PTRUE"),vcov=vcov_cluster )
  res_tab[1,4,i] <- lh$F[2]
  res_tab[3,4,i] <- lh$`Pr(>F)`[2]
  
  res_tab[1,5,i] <- nobs(ols)
 
}
base_balance_bin <- round(res_tab,digits=3)
#res_tab <- round(res_tab,digits=3)
save(base_balance_bin, file=paste(path,"papers/seed_free_or_not/base_balance_bin.Rdata",sep="/"))

###this is where midline data analysis starts
dta <- read.csv(paste(path,"midline/data/public/midline.csv", sep="/"))


## merge in randomized staring price
dta <- merge(dta, bse[c("farmer_ID","P1_pric","final_price")], by.x="ID", by.y="farmer_ID", all.x=TRUE)


##create unique village level identifier for clustering of standard errors
dta$cluster_ID <- as.factor(paste(paste(dta$dist_ID,dta$sub_ID, sep="_"), dta$vil_ID, sep="_"))

dta$used_TP <- dta$used_TP == "Yes"
dta$remembers <- dta$Rec_TP == "Yes" |  dta$Buy_TP  == "Yes"
dta$TP_separate <- dta$TP_separate == 1

dta$cor_plant <- dta$space=="4" & dta$seed_no=="2"
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

dta$screening <- (as.numeric(as.character(dta$final_price)))/1000  ###this is the offer price in ashraf et al
dta$signaling <- (as.numeric(as.character(dta$P1_pric)))/1000 
#to measure sunk cost effect, the transaction price is used, that is the amount that is paid after the discount
#as we did a full discount to it is zero for those that got a discount, and the price paid for those that did not get the discount
dta$sunk <- as.numeric(!dta$discounted)*dta$screening ## this would be the transaction price in ahsraf et al

dta$d_screening <- dta$screening - mean(dta$screening, na.rm=TRUE)
dta$d_signaling <- dta$signaling - mean(dta$signaling, na.rm=TRUE)
dta$d_sunk <- dta$sunk - mean(dta$sunk, na.rm=TRUE)


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
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta[!(dta$trial_P),])
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta[!(dta$trial_P),])  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta[!(dta$trial_P),])
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}

res_tab <- round(res_tab,digits=3)
save(res_tab, file=paste(path,"papers/seed_free_or_not/res_tab.Rdata",sep="/"))

###now run binary analysis

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))



for (i in 1:length(outcomes)) {
  ### here we need clustering
  ### here we need clustering
  ols <- lm(as.formula( paste(outcomes[i],"discounted+trial_P",sep="~")), data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  coef_test(ols, vcov_cluster)$beta
  
  res_tab[1,1,i] <-  coef_test(ols, vcov_cluster)$beta[1]
  res_tab[2,1,i] <- coef_test(ols, vcov_cluster)$SE[1]
  
  res_tab[1,2,i]  <-  coef_test(ols, vcov_cluster)$beta[2]
  res_tab[2,2,i] <-  coef_test(ols, vcov_cluster)$SE[2]
  res_tab[3,2,i] <-  coef_test(ols, vcov_cluster)$p_Satt[2]
  ### regression for sunk cost effect - clustering
  
  
  res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
  res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
  res_tab[3,3,i] <-  coef_test(ols, vcov_cluster)$p_Satt[3]
  
  lh <-   linearHypothesis(ols, c("discountedTRUE = trial_PTRUE"),vcov=vcov_cluster )
  res_tab[1,4,i] <- lh$F[2]
  res_tab[3,4,i] <- lh$`Pr(>F)`[2]
  
  res_tab[1,5,i] <- nobs(ols)
  
}

res_tab_bin <- round(res_tab,digits=3)
save(res_tab_bin, file=paste(path,"papers/seed_free_or_not/res_tab_bin.Rdata",sep="/"))


#table 2: impact on practices

#iterate over outcomes
outcomes <- c("cor_plant","use_fert_inorg","use_fert_org","use_chem","gap_fill","nr_weed","timely_planting")

index_use <- icwIndex(xmat=dta[outcomes]) #x
dta <- data.frame(dta,index_use)
names(dta)[names(dta) == 'index'] <- 'index_pract'

outcomes <- c(outcomes,"index_pract" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_pract <- round(res_tab,digits=3)
save(res_tab_pract, file=paste(path,"papers/seed_free_or_not/res_tab_pract.Rdata",sep="/"))

###now run binary analysis

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

for (i in 1:length(outcomes)) {
  ### here we need clustering
  ### here we need clustering
  ols <- lm(as.formula( paste(outcomes[i],"discounted+trial_P",sep="~")), data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  coef_test(ols, vcov_cluster)$beta
  
  res_tab[1,1,i] <-  coef_test(ols, vcov_cluster)$beta[1]
  res_tab[2,1,i] <- coef_test(ols, vcov_cluster)$SE[1]
  
  res_tab[1,2,i]  <-  coef_test(ols, vcov_cluster)$beta[2]
  res_tab[2,2,i] <-  coef_test(ols, vcov_cluster)$SE[2]
  res_tab[3,2,i] <-  coef_test(ols, vcov_cluster)$p_Satt[2]
  ### regression for sunk cost effect - clustering
  
  
  res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
  res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
  res_tab[3,3,i] <-  coef_test(ols, vcov_cluster)$p_Satt[3]
  
  lh <-   linearHypothesis(ols, c("discountedTRUE = trial_PTRUE"),vcov=vcov_cluster )
  res_tab[1,4,i] <- lh$F[2]
  res_tab[3,4,i] <- lh$`Pr(>F)`[2]
  
  res_tab[1,5,i] <- nobs(ols)
  
}

res_tab_pract_bin <- round(res_tab,digits=3)
save(res_tab_pract_bin, file=paste(path,"papers/seed_free_or_not/res_tab_pract_bin.Rdata",sep="/"))

#table 3: impact on characteristics

#iterate over outcomes
outcomes <- c("happy_yield",
"happy_drought",
"happy_disease","happy_germinate", "happy")

index_use <- icwIndex(xmat=dta[outcomes]) #x
dta <- data.frame(dta,index_use)
names(dta)[names(dta) == 'index'] <- 'index_char'

outcomes <- c(outcomes,"index_char" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
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

index_use <- icwIndex(xmat=dta[outcomes]) #x
dta <- data.frame(dta,index_use)

names(dta)[names(dta) == 'index'] <- 'index_yield'

outcomes <- c(outcomes,"index_yield" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
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

index_use <- icwIndex(xmat=dta[outcomes]) #x
dta <- data.frame(dta,index_use)

names(dta)[names(dta) == 'index'] <- 'index_plan'

outcomes <- c(outcomes,"index_plan" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_plan <- round(res_tab,digits=3)
save(res_tab_plan, file=paste(path,"papers/seed_free_or_not/res_tab_plan.Rdata",sep="/"))

###impact on 

dta$remembers_seed <- dta$exp1_lmpr == "Bazooka"
dta$remembers_comp <- dta$seedco == "4"
dta$value_shop <- as.numeric(as.character(dta$value))

dta$remembers_paying <- FALSE
dta$remembers_paying <- dta$pay == "Yes" | dta$pay_d=="Yes"
dta$remembers_paying[dta$pay == "n/a" & dta$pay_d=="n/a"] <- NA
dta$remembers_paying[dta$trial_P] <- NA


dta$value_paid <- as.numeric(as.character(dta$final_price))
### quadratic loss for error in price recall
dta$price_diff_sq <- abs(as.numeric(as.character(dta$final_price)) - as.numeric(as.character(dta$price_paid)))
dta$price_diff_sq[dta$trial_P] <- NA
#iterate over outcomes
outcomes <- c("remembers_paying","price_diff_sq","remembers_seed",
              "remembers_comp")

index_use <- icwIndex(xmat=dta[outcomes]) #x
dta <- data.frame(dta,index_use)

names(dta)[names(dta) == 'index'] <- 'index_plan'

outcomes <- c(outcomes, "index_plan" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}

res_tab_path <- round(res_tab,digits=3)
save(res_tab_path, file=paste(path,"papers/seed_free_or_not/res_tab_path.Rdata",sep="/"))
###now run binary analysis

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))



for (i in 1:length(outcomes)) {
  ### here we need clustering

  ols <- lm(as.formula( paste(outcomes[i],"discounted+trial_P",sep="~")), data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  coef_test(ols, vcov_cluster)$beta
  
  res_tab[1,1,i] <-  coef_test(ols, vcov_cluster)$beta[1]
  res_tab[2,1,i] <- coef_test(ols, vcov_cluster)$SE[1]
  
  res_tab[1,2,i]  <-  coef_test(ols, vcov_cluster)$beta[2]
  res_tab[2,2,i] <-  coef_test(ols, vcov_cluster)$SE[2]
  res_tab[3,2,i] <-  coef_test(ols, vcov_cluster)$p_Satt[2]
  ### regression for sunk cost effect - clustering
  
  
  res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
  res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
  res_tab[3,3,i] <-  coef_test(ols, vcov_cluster)$p_Satt[3]
if (i %in% 3:5) {  
  lh <-   linearHypothesis(ols, c("discountedTRUE = trial_PTRUE"),vcov=vcov_cluster )
  res_tab[1,4,i] <- lh$F[2]
  res_tab[3,4,i] <- lh$`Pr(>F)`[2]
}
  res_tab[1,5,i] <- nobs(ols)
  
}

res_tab_path_bin <- round(res_tab,digits=3)
save(res_tab_path_bin, file=paste(path,"papers/seed_free_or_not/res_tab_path_bin.Rdata",sep="/"))


####analysis of actual behavior in subsequent season
### read in endline data (anonymized version)
dta <- read.csv(paste(path,"endline/data/public/endline.csv", sep="/"))

## keep only 
dta <- subset(dta, cont == FALSE & (trial_P== TRUE | paid_pac == TRUE | discounted == TRUE))

##where do we get dta$final_price and dta$P1_pric
#in bse 


dta <- merge(dta,bse[c("farmer_ID","P1_pric","final_price","cluster_ID")], by.x="ID", by.y="farmer_ID", all.x=TRUE)
dta <- subset(dta, !is.na(cluster_ID))
dta$screening <- (as.numeric(as.character(dta$final_price)))/1000
dta$signaling <- (as.numeric(as.character(dta$P1_pric)))/1000
#to measure sunk cost effect, the transaction price is used, that is the amount that is paid after the discount
#as we did a full discount to it is zero for those that got a discount, and the price paid for those that did not get the discount
dta$sunk <- as.numeric(!dta$discounted)*dta$screening

dta$d_screening <- dta$screening - mean(dta$screening, na.rm=TRUE)
dta$d_signaling <- dta$signaling - mean(dta$signaling, na.rm=TRUE)
dta$d_sunk <- dta$sunk - mean(dta$sunk, na.rm=TRUE)


## primary outcome 1: uses improved seed on at least one plot
num_plots <- max(as.numeric(dta$plot_count), na.rm=TRUE)
logical_result <- logical(nrow(dta))

for (i in 1:num_plots) {
  ### definition of improved seed: fresh hybrid from trusted source or OPV recycled max 3 times from trusted source
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

###not interviewed
dta$p_outcome_1[is.na(dta$plot_no)] <- NA

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
                      ((dta$maize_var_selected  %in%  c("Longe_5", "Longe_5D", "Longe_4", "MM3","other_opv")) & (dta$times_recycled_selected %in% 1:4) &  (((dta$single_source_selected %in% letters[4:9])) | (dta$recycled_source_selected %in% letters[4:9]))))
dta$rnd_adopt[dta$no_grow] <- NA 


dta$rnd_bazo <-  ((dta$maize_var_selected == "Bazooka") & (dta$single_source_selected %in% letters[4:9]  & (dta$times_recycled_selected %in% 1)))
dta$rnd_bazo[dta$no_grow] <- NA 



### seed quantity
dta$seed_qty <- as.numeric(as.character(dta$seed_qty))
dta$seed_qty[dta$seed_qty == 999] <- NA 

dta$imp_seed_qty_rnd <- dta$rnd_adopt*dta$seed_qty
#dta$imp_seed_qty_rnd[is.na(dta$imp_seed_qty_rnd)] <- 0



dta <- trim("imp_seed_qty_rnd", dta)


dta$size_selected <- as.numeric(as.character(dta$size_selected)) 
### seed quantity per area
dta$imp_seed_qty_rnd_acre <- dta$imp_seed_qty_rnd/dta$size_selected  



###production
dta$bag_harv[dta$bag_harv == "999"] <- NA
dta$production <- as.numeric(as.character(dta$bag_harv))*as.numeric(as.character(dta$bag_kg))
dta <- trim("production", dta)

## productivity
dta$productivity <- dta$production/dta$size_selected
dta <- trim("productivity", dta)

#iterate over outcomes
outcomes <- c("p_outcome_1", "p_outcome_2","rnd_adopt", "rnd_bazo", "imp_seed_qty_rnd", "imp_seed_qty_rnd_acre","production", "productivity" )
index_use <- icwIndex(xmat=dta[setdiff(outcomes,c("p_outcome_1","p_outcome_2"))]) #x
dta <- data.frame(dta,index_use)

names(dta)[names(dta) == 'index'] <- 'index_next_season'

outcomes <- c(outcomes,"index_next_season" )

res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[outcomes], 2, sd, na.rm=T)


for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta[!(dta$trial_P),])
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta[!(dta$trial_P),])  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta[!(dta$trial_P),])
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_next_season <- round(res_tab,digits=3)
save(res_tab_next_season, file=paste(path,"papers/seed_free_or_not/res_tab_next_season.Rdata",sep="/"))

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))


for (i in 1:length(outcomes)) {
  ### here we need clustering
  ols <- lm(as.formula( paste(outcomes[i],"discounted+trial_P",sep="~")), data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  coef_test(ols, vcov_cluster)$beta
  
  res_tab[1,1,i] <-  coef_test(ols, vcov_cluster)$beta[1]
  res_tab[2,1,i] <- coef_test(ols, vcov_cluster)$SE[1]
  
  res_tab[1,2,i]  <-  coef_test(ols, vcov_cluster)$beta[2]
  res_tab[2,2,i] <-  coef_test(ols, vcov_cluster)$SE[2]
  res_tab[3,2,i] <-  coef_test(ols, vcov_cluster)$p_Satt[2]
  ### regression for sunk cost effect - clustering
  
  
  res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
  res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
  res_tab[3,3,i] <-  coef_test(ols, vcov_cluster)$p_Satt[3]
  
  lh <-   linearHypothesis(ols, c("discountedTRUE = trial_PTRUE"),vcov=vcov_cluster )
  res_tab[1,4,i] <- lh$F[2]
  res_tab[3,4,i] <- lh$`Pr(>F)`[2]
  
  res_tab[1,5,i] <- nobs(ols)
  
}
res_tab_next_season_bin <- round(res_tab,digits=3)
#res_tab <- round(res_tab,digits=3)
save(res_tab_next_season_bin, file=paste(path,"papers/seed_free_or_not/res_tab_next_season_bin.Rdata",sep="/"))


dta$remembers_seed <- dta$Trial_group.TP_exp1_lmpr == "Bazooka"
dta$remembers_comp <- dta$Trial_group.TP_seedco == "4"
dta$value_shop <- as.numeric(as.character(dta$value))

dta$remembers_paying <- FALSE
dta$remembers_paying <- dta$check3.pay == "Yes" | dta$check3.pay_d=="Yes"
dta$remembers_paying[dta$check3.pay == "n/a" & dta$check3.pay_d=="n/a"] <- NA
dta$remembers_paying[dta$trial_P] <- NA


dta$value_paid <- as.numeric(as.character(dta$final_price))
### quadratic loss for error in price recall
dta$price_diff_sq <- abs(as.numeric(as.character(dta$final_price)) - as.numeric(as.character(dta$check3.price_paid)))
dta$price_diff_sq[dta$trial_P] <- NA

outcomes <- c("remembers_paying","price_diff_sq","remembers_seed",
              "remembers_comp")

index_use <- icwIndex(xmat=dta[outcomes]) #x
dta <- data.frame(dta,index_use)

names(dta)[names(dta) == 'index'] <- 'index_plan'

outcomes <- c(outcomes, "index_plan" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
## for subset of those that bought


res_tab_path_next_season <- round(res_tab,digits=3)
save(res_tab_path_next_season, file=paste(path,"papers/seed_free_or_not/res_tab_path_next_season.Rdata",sep="/"))
###now run binary analysis

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))



for (i in 1:length(outcomes)) {
  ### here we need clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"discounted+trial_P",sep="~")), data=dta)
  vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
  coef_test(ols, vcov_cluster)$beta
  
  res_tab[1,1,i] <-  coef_test(ols, vcov_cluster)$beta[1]
  res_tab[2,1,i] <- coef_test(ols, vcov_cluster)$SE[1]
  
  res_tab[1,2,i]  <-  coef_test(ols, vcov_cluster)$beta[2]
  res_tab[2,2,i] <-  coef_test(ols, vcov_cluster)$SE[2]
  res_tab[3,2,i] <-  coef_test(ols, vcov_cluster)$p_Satt[2]
  ### regression for sunk cost effect - clustering
  
  
  res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
  res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
  res_tab[3,3,i] <-  coef_test(ols, vcov_cluster)$p_Satt[3]
  if (i %in% 3:5) {  
    lh <-   linearHypothesis(ols, c("discountedTRUE = trial_PTRUE"),vcov=vcov_cluster )
    res_tab[1,4,i] <- lh$F[2]
    res_tab[3,4,i] <- lh$`Pr(>F)`[2]
  }
  res_tab[1,5,i] <- nobs(ols)
  
}

res_tab_path_next_season_bin <- round(res_tab,digits=3)
save(res_tab_path_next_season_bin, file=paste(path,"papers/seed_free_or_not/res_tab_path_next_season_bin.Rdata",sep="/"))
