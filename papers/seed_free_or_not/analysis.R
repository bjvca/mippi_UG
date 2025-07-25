rm(list=ls())
path <- getwd()
library(dplyr)
library(haven)
library(car)
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
#get baseline data Uganda
path <- strsplit(path,"papers/seed_free_or_not")[[1]]
#create treatmetn cluster indicator for clustering SE
bse <- read.csv(paste(path,"baseline/data/public/baseline.csv",sep="/"))
bse$cluster_ID <- as.factor(paste(paste(bse$distID,bse$subID, sep="_"), bse$vilID, sep="_"))

### read bse_eth
bse_eth <- data.frame(read_dta(paste(path,"papers/seed_free_or_not/eth_data/2. Phone survey (pre-production)/Eth_phonesurvey_2024.dta", sep = "/")))
end_eth <- data.frame(read_dta(paste(path,"papers/seed_free_or_not/eth_data/3. Seed promotion (endline)/HHLevel.dta", sep = "/")))

prepared <- data.frame(read_dta("~/data/projects/OneCG/MIPP/papers/seed_free_or_not/eth_data/balance_tab_Eth.dta"))

####create transaction price for bargaining game - uganda

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


##keep only those that participated in the bargaining experiment
bse <- subset(bse, (cont == FALSE | trial_P == FALSE) & ( paid_pac == TRUE | discounted == TRUE))

###now do this for ethiopia_teff

teff_bar <- read.csv(paste(path,"papers/seed_free_or_not/eth_data/1. Bargaining exp/teff_bargaining.csv",sep="/"))

##determine maximum bid price
teff_bar$bid <- ifelse(!is.na(as.numeric(teff_bar$p2_pric_11)),as.numeric(teff_bar$p2_pric_11),
                  ifelse(!is.na(as.numeric(teff_bar$p2_pric_10)),as.numeric(teff_bar$p2_pric_10),
                         ifelse(!is.na(as.numeric(teff_bar$p2_pric_9)),as.numeric(teff_bar$p2_pric_9),
                                ifelse(!is.na(as.numeric(teff_bar$p2_pric_8)),as.numeric(teff_bar$p2_pric_8),
                                       ifelse(!is.na(as.numeric(teff_bar$p2_pric_7)),as.numeric(teff_bar$p2_pric_7),
                                              ifelse(!is.na(as.numeric(teff_bar$p2_pric_6)),as.numeric(teff_bar$p2_pric_6),
                                                     ifelse(!is.na(as.numeric(teff_bar$p2_pric_5)),as.numeric(teff_bar$p2_pric_5),
                                                            ifelse(!is.na(as.numeric(teff_bar$p2_pric_4)),as.numeric(teff_bar$p2_pric_4),
                                                                   ifelse(!is.na(as.numeric(teff_bar$p2_pric_3)),as.numeric(teff_bar$p2_pric_3),
                                                                          ifelse(!is.na(as.numeric(teff_bar$p2_pric_2)),as.numeric(teff_bar$p2_pric_2),
                                                                                 as.numeric(teff_bar$p2_pric)
                                                                          ))))))))))

##determine minimum ask price
teff_bar$ask <- ifelse(!is.na(as.numeric(teff_bar$p3_pric_10)),as.numeric(teff_bar$p3_pric_10),
                    ifelse(!is.na(as.numeric(teff_bar$p3_pric_9)),as.numeric(teff_bar$p3_pric_9),
                           ifelse(!is.na(as.numeric(teff_bar$p3_pric_8)),as.numeric(teff_bar$p3_pric_8),
                                  ifelse(!is.na(as.numeric(teff_bar$p3_pric_7)),as.numeric(teff_bar$p3_pric_7),
                                        ifelse(!is.na(as.numeric(teff_bar$p3_pric_6)),as.numeric(teff_bar$p3_pric_6),
                                               ifelse(!is.na(as.numeric(teff_bar$p3_pric_5)),as.numeric(teff_bar$p3_pric_5),
                                                      ifelse(!is.na(as.numeric(teff_bar$p3_pric_4)),as.numeric(teff_bar$p3_pric_4),
                                                             ifelse(!is.na(as.numeric(teff_bar$p3_pric_3)),as.numeric(teff_bar$p3_pric_3),
                                                                    ifelse(!is.na(as.numeric(teff_bar$p3_pric_2)),as.numeric(teff_bar$p3_pric_2),
                                                                           ifelse(!is.na(as.numeric(teff_bar$p3_pric)),as.numeric(teff_bar$p3_pric),
                                                                                  as.numeric(teff_bar$p1_pric)
                                                                               ))))))))))

teff_bar$accepts <- "seller"
teff_bar$accepts[teff_bar$start_neg=="Yes"| teff_bar$start_neg_2=="Yes" | teff_bar$start_neg_3=="Yes"
            | teff_bar$start_neg_4=="Yes" | teff_bar$start_neg_5=="Yes" | teff_bar$start_neg_6=="Yes"| teff_bar$start_neg_7=="Yes" 
            | teff_bar$start_neg_8=="Yes" | teff_bar$start_neg_9=="Yes" | teff_bar$start_neg_10=="Yes" | teff_bar$start_neg_11=="Yes"] <- "buyer"



##determine willingness to pay


teff_bar$trunc <- FALSE
teff_bar$trunc[as.numeric(teff_bar$pr_test_5)<0] <- TRUE

## final price is probably too low if bargaining was truncated
## should probably between previous ask and bid price
## replace (wrong) ask price with previous ask price of round 4
teff_bar$ask[teff_bar$trunc==TRUE] <- teff_bar$p3_pric_4[teff_bar$trunc==TRUE]

teff_bar$final_price <- NA
### if buyer accepts, this is the last ask price
teff_bar$final_price[teff_bar$accepts=="buyer"] <- teff_bar$ask[teff_bar$accepts=="buyer"]
### if seller accepts this is the last bid price
teff_bar$final_price[teff_bar$accepts=="seller"] <- teff_bar$bid[teff_bar$accepts=="seller"]
### if truncated, take average of ask and bid price
teff_bar$final_price[teff_bar$trunc==TRUE] <-   (teff_bar$ask[teff_bar$trunc==TRUE] +  teff_bar$bid[teff_bar$trunc==TRUE])/2

##establish a lower limit 
teff_bar$final_price_ll <- NA
### if buyer accepts, this is the last ask price
teff_bar$final_price_ll[teff_bar$accepts=="buyer"] <- teff_bar$ask[teff_bar$accepts=="buyer"]
### if seller accepts this is the last bid price
teff_bar$final_price_ll[teff_bar$accepts=="seller"] <- teff_bar$bid[teff_bar$accepts=="seller"]

##establish an upper limit 
teff_bar$final_price_ul <- NA
### if buyer accepts, this is the last ask price
teff_bar$final_price_ul[teff_bar$accepts=="buyer"] <- teff_bar$ask[teff_bar$accepts=="buyer"]
### if seller accepts this is the last bid price
teff_bar$final_price_ul[teff_bar$accepts=="seller"] <- teff_bar$bid[teff_bar$accepts=="seller"]
### use upper limit for truncated
teff_bar$final_price_ul[teff_bar$trunc==TRUE] <-   teff_bar$ask[teff_bar$trunc==TRUE]

##keep only those that participated in the bargaining experiment

teff_bar <- subset(teff_bar, ( paid_pac == TRUE | discounted == TRUE))

teff_bar$paid_pac[!(teff_bar$discounted)] <- FALSE

teff_merged <- merge(teff_bar, prepared,by.x="hh_id", by.y="hh_id" ,all.x=TRUE)

##now do this for wheat

wheat_bar <- read.csv(paste(path,"papers/seed_free_or_not/eth_data/1. Bargaining exp/wheat_bargaining.csv",sep="/"))

##determine maximum bid price
wheat_bar$bid <- ifelse(!is.na(as.numeric(wheat_bar$p2_pric_11)),as.numeric(wheat_bar$p2_pric_11),
                       ifelse(!is.na(as.numeric(wheat_bar$p2_pric_10)),as.numeric(wheat_bar$p2_pric_10),
                              ifelse(!is.na(as.numeric(wheat_bar$p2_pric_9)),as.numeric(wheat_bar$p2_pric_9),
                                     ifelse(!is.na(as.numeric(wheat_bar$p2_pric_8)),as.numeric(wheat_bar$p2_pric_8),
                                            ifelse(!is.na(as.numeric(wheat_bar$p2_pric_7)),as.numeric(wheat_bar$p2_pric_7),
                                                   ifelse(!is.na(as.numeric(wheat_bar$p2_pric_6)),as.numeric(wheat_bar$p2_pric_6),
                                                          ifelse(!is.na(as.numeric(wheat_bar$p2_pric_5)),as.numeric(wheat_bar$p2_pric_5),
                                                                 ifelse(!is.na(as.numeric(wheat_bar$p2_pric_4)),as.numeric(wheat_bar$p2_pric_4),
                                                                        ifelse(!is.na(as.numeric(wheat_bar$p2_pric_3)),as.numeric(wheat_bar$p2_pric_3),
                                                                               ifelse(!is.na(as.numeric(wheat_bar$p2_pric_2)),as.numeric(wheat_bar$p2_pric_2),
                                                                                      as.numeric(wheat_bar$p2_pric)
                                                                               ))))))))))

##determine minimum ask price
wheat_bar$ask <- ifelse(!is.na(as.numeric(wheat_bar$p3_pric_10)),as.numeric(wheat_bar$p3_pric_10),
                       ifelse(!is.na(as.numeric(wheat_bar$p3_pric_9)),as.numeric(wheat_bar$p3_pric_9),
                              ifelse(!is.na(as.numeric(wheat_bar$p3_pric_8)),as.numeric(wheat_bar$p3_pric_8),
                                     ifelse(!is.na(as.numeric(wheat_bar$p3_pric_7)),as.numeric(wheat_bar$p3_pric_7),
                                            ifelse(!is.na(as.numeric(wheat_bar$p3_pric_6)),as.numeric(wheat_bar$p3_pric_6),
                                                   ifelse(!is.na(as.numeric(wheat_bar$p3_pric_5)),as.numeric(wheat_bar$p3_pric_5),
                                                          ifelse(!is.na(as.numeric(wheat_bar$p3_pric_4)),as.numeric(wheat_bar$p3_pric_4),
                                                                 ifelse(!is.na(as.numeric(wheat_bar$p3_pric_3)),as.numeric(wheat_bar$p3_pric_3),
                                                                        ifelse(!is.na(as.numeric(wheat_bar$p3_pric_2)),as.numeric(wheat_bar$p3_pric_2),
                                                                               ifelse(!is.na(as.numeric(wheat_bar$p3_pric)),as.numeric(wheat_bar$p3_pric),
                                                                                      as.numeric(wheat_bar$p1_pric)
                                                                               ))))))))))

wheat_bar$accepts <- "seller"
wheat_bar$accepts[wheat_bar$start_neg=="Yes"| wheat_bar$start_neg_2=="Yes" | wheat_bar$start_neg_3=="Yes"
                 | wheat_bar$start_neg_4=="Yes" | wheat_bar$start_neg_5=="Yes" | wheat_bar$start_neg_6=="Yes"| wheat_bar$start_neg_7=="Yes" 
                 | wheat_bar$start_neg_8=="Yes" | wheat_bar$start_neg_9=="Yes" | wheat_bar$start_neg_10=="Yes" | wheat_bar$start_neg_11=="Yes"] <- "buyer"



##determine willingness to pay


wheat_bar$trunc <- FALSE
wheat_bar$trunc[as.numeric(wheat_bar$pr_test_5)<0] <- TRUE

## final price is probably too low if bargaining was truncated
## should probably between previous ask and bid price
## replace (wrong) ask price with previous ask price of round 4
wheat_bar$ask[wheat_bar$trunc==TRUE] <- wheat_bar$p3_pric_4[wheat_bar$trunc==TRUE]

wheat_bar$final_price <- NA
### if buyer accepts, this is the last ask price
wheat_bar$final_price[wheat_bar$accepts=="buyer"] <- wheat_bar$ask[wheat_bar$accepts=="buyer"]
### if seller accepts this is the last bid price
wheat_bar$final_price[wheat_bar$accepts=="seller"] <- wheat_bar$bid[wheat_bar$accepts=="seller"]
### if truncated, take average of ask and bid price
wheat_bar$final_price[wheat_bar$trunc==TRUE] <-   (wheat_bar$ask[wheat_bar$trunc==TRUE] +  wheat_bar$bid[wheat_bar$trunc==TRUE])/2

##establish a lower limit 
wheat_bar$final_price_ll <- NA
### if buyer accepts, this is the last ask price
wheat_bar$final_price_ll[wheat_bar$accepts=="buyer"] <- wheat_bar$ask[wheat_bar$accepts=="buyer"]
### if seller accepts this is the last bid price
wheat_bar$final_price_ll[wheat_bar$accepts=="seller"] <- wheat_bar$bid[wheat_bar$accepts=="seller"]

##establish an upper limit 
wheat_bar$final_price_ul <- NA
### if buyer accepts, this is the last ask price
wheat_bar$final_price_ul[wheat_bar$accepts=="buyer"] <- wheat_bar$ask[wheat_bar$accepts=="buyer"]
### if seller accepts this is the last bid price
wheat_bar$final_price_ul[wheat_bar$accepts=="seller"] <- wheat_bar$bid[wheat_bar$accepts=="seller"]
### use upper limit for truncated
wheat_bar$final_price_ul[wheat_bar$trunc==TRUE] <-   wheat_bar$ask[wheat_bar$trunc==TRUE]

##keep only those that participated in the bargaining experiment
wheat_bar <- subset(wheat_bar, ( paid_pac == TRUE | discounted == TRUE))

wheat_bar$paid_pac[!(wheat_bar$discounted)] <- FALSE

wheat_merged <- merge(wheat_bar, prepared,by.x="hh_id", by.y="hh_id" ,all.x=TRUE)


bse$age_head <- as.numeric(as.character(bse$age))
bse$age_head[bse$age_head==999] <- NA

names(teff_merged)[names(teff_merged) =="yield_teff"]  <- "yield_rand"
names(wheat_merged)[names(wheat_merged) =="yield_teff"]  <- "yield_rand"
teff_merged <- trim("yield_rand",teff_merged,trim_perc=.01)

wheat_merged <- trim("yield_rand",wheat_merged,trim_perc=.01)


###change names we we can iterage over outcomes
names(teff_merged)[names(teff_merged) %in% c("age","primaryeduc","gender","hhsize","quality_teffseed","promoseed_teff","seedformal_teff" )] <- c("male_head","age_head","hh_size","prim_head","quality_use","promo_use_rand","source_rand" )
names(wheat_merged)[names(wheat_merged) %in% c("age","primaryeduc","gender","hhsize","quality_wheatseed","promoseed_wheat","seedformal_wheat" )] <- c("male_head","age_head","hh_size","prim_head","quality_use","promo_use_rand","source_rand" )


bse$prim_head <-bse$edu %in% c("c","d","e","f")
bse$male_head <- bse$gender == "Male"
bse$hh_size <- as.numeric(as.character(bse$hh_size))
bse$dist_ag <- as.numeric(as.character(bse$dist_ag))
bse$dist_ag[bse$dist_ag==999] <- NA
bse$quality_use <- bse$quality_use=="Yes"
bse$promo_use_rand <- bse$maize_var=="Bazooka"
bse$source_rand <- bse$source %in%  letters[seq( from = 4, to = 9 )]
bse$often_rand <-  bse$often %in%  letters[seq( from = 1, to = 5 )]
bse$bag_harv[bse$bag_harv == "999"] <- NA
bse$prod_rand <-  as.numeric(as.character(bse$bag_harv))* as.numeric(as.character(bse$bag_kg))
bse$acre_rand <- as.numeric(as.character(bse$plot_size))
bse$acre_rand[bse$acre_rand==999] <- NA
bse$yield_rand <- bse$prod_rand/bse$acre_rand
bse <- trim("yield_rand",bse,trim_perc=.01)

##merge in price paid from midline data (this was calculated and pulled into the app at midline)

bse_reg <- subset(bse, !trial_P)

bse_reg$screening <- (as.numeric(as.character(bse_reg$final_price)))/1000
bse_reg$signaling <- (as.numeric(as.character(bse_reg$P1_pric)))/1000
##important: it is the non-discounted (those that pay the full price) that identify the sunk cost
##they are the filtered ones that also pay a postive price
bse_reg$sunk <- as.numeric(!bse_reg$discounted)

bse_reg$d_screening <- bse_reg$screening - mean(bse_reg$screening, na.rm=TRUE)
bse_reg$d_signaling <- bse_reg$signaling - mean(bse_reg$signaling, na.rm=TRUE)
bse_reg$d_sunk <- bse_reg$sunk - mean(bse_reg$sunk, na.rm=TRUE)

###descriptives

### we do not have a few outcomes in the eth data

teff_merged$dist_ag <- NA
teff_merged$often_rand <- NA
wheat_merged$dist_ag <- NA
wheat_merged$often_rand <- NA


#iterate over outcomes
outcomes <- c("age_head","prim_head","male_head","hh_size","dist_ag","quality_use","promo_use_rand","source_rand","often_rand","yield_rand" )


#matrix to store results
res_tab <-  array(NA,dim=c(3,3,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(bse_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(bse_reg[outcomes], 2, sd, na.rm=T)
res_tab[1,2,1:length(outcomes)] <- colMeans(teff_merged[outcomes], na.rm=T)
res_tab[2,2,1:length(outcomes)] <- apply(teff_merged[outcomes], 2, sd, na.rm=T)
res_tab[1,3,1:length(outcomes)] <- colMeans(wheat_merged[outcomes], na.rm=T)
res_tab[2,3,1:length(outcomes)] <- apply(wheat_merged[outcomes], 2, sd, na.rm=T)
base_balance <- round(res_tab,digits=3)
#res_tab <- round(res_tab,digits=3)
save(base_balance, file=paste(path,"papers/seed_free_or_not/base_balance.Rdata",sep="/"))

###WTP graph
bse_graph <- bse
bse_graph$final_price[bse_graph$final_price== "3000"] <- NA

hist(matStand(as.matrix(bse_graph$final_price)))
hist(bse_graph$final_price)

bse_graph$final_price[bse_graph$final_price == "9000" & bse_graph$P1_pric== "9000"] <- NA
bse_graph$final_price[bse_graph$final_price == "10000" & bse_graph$P1_pric== "10000"] <- NA
bse_graph$final_price[bse_graph$final_price == "11000" & bse_graph$P1_pric== "11000"] <- NA
bse_graph$final_price[bse_graph$final_price == "12000" & bse_graph$P1_pric== "12000"] <- NA
breaks <- seq(from = 4000, to= 10000, by =1000)
bse_graph$final_price <- cut(bse_graph$final_price, breaks = breaks,labels = c("4000","5000","6000","7000","8000","9000"), right = FALSE)
bse_graph$final_price <- as.numeric(as.character(bse_graph$final_price))
bse_graph$final_price[bse_graph$final_price < 5000] <- NA
bse_graph <- subset(bse_graph, !is.na(final_price) )

library(ggplot2)

# Create a data frame of proportions
price_prop <- as.data.frame(prop.table(table(bse_graph$final_price)))
colnames(price_prop) <- c("Price", "Density")

# If Price is numeric, convert it (for better ordering in the plot)
price_prop$Price <- as.numeric(as.character(price_prop$Price))

# Plot with ggplot2
plot_maize <- ggplot(price_prop, aes(x = Price, y = Density)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Demand for Maize Seed",
    x = "Price",
    y = "Density"
  ) +
  theme_minimal()

###WTP graph for teff

bse_graph <- teff_bar
bse_graph$final_price[bse_graph$final_price>110] <- NA
bse_graph$final_price[bse_graph$p1_pric == bse_graph$final_price] <- NA

breaks <- seq(from = 40, to= 100, by =10)
bse_graph$final_price <- cut(bse_graph$final_price, breaks = breaks,labels = c("40","50","60","70","80","90"),right = FALSE)
bse_graph$final_price <- as.numeric(as.character(bse_graph$final_price))
bse_graph <- subset(bse_graph, !is.na(final_price) )


# Create a data frame of proportions
price_prop <- as.data.frame(prop.table(table(bse_graph$final_price)))
colnames(price_prop) <- c("Price", "Density")

# If Price is numeric, convert it (for better ordering in the plot)
price_prop$Price <- as.numeric(as.character(price_prop$Price))

# Plot with ggplot2
plot_teff <- ggplot(price_prop, aes(x = Price, y = Density)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Demand for Teff Seed",
    x = "Price",
    y = "Density"
  ) +
  theme_minimal()

##WTP graph for teff - lee bounds for truncation

bse_graph <- teff_bar
bse_graph$final_price <- bse_graph$final_price_ul
bse_graph$final_price[bse_graph$final_price>110] <- NA
bse_graph$final_price[bse_graph$p1_pric == bse_graph$final_price] <- NA
#bse_graph$final_price[bse_graph$final_price == "9000" & bse_graph$P1_pric== "9000"] <- NA
#bse_graph$final_price[bse_graph$final_price == "10000" & bse_graph$P1_pric== "10000"] <- NA
#bse_graph$final_price[bse_graph$final_price == "11000" & bse_graph$P1_pric== "11000"] <- NA
#bse_graph$final_price[bse_graph$final_price == "12000" & bse_graph$P1_pric== "12000"] <- NA
breaks <- seq(from = 30, to= 100, by =10)
bse_graph$final_price <- cut(bse_graph$final_price, breaks = breaks,labels = c("30","40","50","60","70","80","90"),right = FALSE)
bse_graph$final_price <- as.numeric(as.character(bse_graph$final_price))
bse_graph <- subset(bse_graph, !is.na(final_price) )


barplot(prop.table(table(bse_graph$final_price)), xlab = "Price", ylab = "Density")
###WTP graph for wheat

bse_graph <- wheat_bar
bse_graph$final_price <- bse_graph$final_price_ul
bse_graph$final_price[bse_graph$final_price>90] <- NA
bse_graph$final_price[bse_graph$p1_pric == bse_graph$final_price] <- NA
#bse_graph$final_price[bse_graph$final_price == "9000" & bse_graph$P1_pric== "9000"] <- NA
#bse_graph$final_price[bse_graph$final_price == "10000" & bse_graph$P1_pric== "10000"] <- NA
#bse_graph$final_price[bse_graph$final_price == "11000" & bse_graph$P1_pric== "11000"] <- NA
#bse_graph$final_price[bse_graph$final_price == "12000" & bse_graph$P1_pric== "12000"] <- NA
breaks <- seq(from = 30, to= 80, by =10)
bse_graph$final_price <- cut(bse_graph$final_price, breaks = breaks,labels = c("30","40","50","60","70"),right = FALSE)
bse_graph$final_price <- as.numeric(as.character(bse_graph$final_price))
bse_graph <- subset(bse_graph, !is.na(final_price) )

# Create a data frame of proportions
price_prop <- as.data.frame(prop.table(table(bse_graph$final_price)))
colnames(price_prop) <- c("Price", "Density")

# If Price is numeric, convert it (for better ordering in the plot)
price_prop$Price <- as.numeric(as.character(price_prop$Price))

# Plot with ggplot2
plot_wheat <- ggplot(price_prop, aes(x = Price, y = Density)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Demand for Wheat Seed",
    x = "Price",
    y = "Density"
  ) +
  theme_minimal()

library(patchwork)
combined_plot <- plot_maize /plot_teff / plot_wheat   # "/" stacks vertically

# Save as PNG
ggsave(paste(path,"papers/seed_free_or_not/demand.png",sep="/"), combined_plot, width = 8, height = 12, dpi = 300)

### now run binary analysis 

##important: it is the non-discounted (those that pay the full price) that identify the sunk cost
##they are the filtered ones that also pay a positive price



# ###balance table
# #iterate over outcomes
# outcomes <- c("age_head","prim_head","male_head","hh_size","dist_ag","quality_use","bazooka_use_rand","source_rand","often_rand","yield_rand_ihs" )
# 
# 
# #matrix to store results
# res_tab <-  array(NA,dim=c(3,5,length(outcomes)))
# 
# 
# for (i in 1:length(outcomes)) {
#   ### here we need clustering
#   ols <- lm(as.formula( paste(outcomes[i],"discounted+trial_P",sep="~")), data=bse)
#   vcov_cluster <- vcovCR(ols,cluster=bse$cluster_ID,type="CR2")
#   coef_test(ols, vcov_cluster)$beta
#   
#   res_tab[1,1,i] <-  coef_test(ols, vcov_cluster)$beta[1]
#   res_tab[2,1,i] <- coef_test(ols, vcov_cluster)$SE[1]
#   
#   res_tab[1,2,i]  <-  coef_test(ols, vcov_cluster)$beta[2]
#   res_tab[2,2,i] <-  coef_test(ols, vcov_cluster)$SE[2]
#   res_tab[3,2,i] <-  coef_test(ols, vcov_cluster)$p_Satt[2]
#   ### regression for sunk cost effect - clustering
#   
#   
#   res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
#   res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
#   res_tab[3,3,i] <-  coef_test(ols, vcov_cluster)$p_Satt[3]
#   
#   lh <-   linearHypothesis(ols, c("discountedTRUE = trial_PTRUE"),vcov=vcov_cluster )
#   res_tab[1,4,i] <- lh$F[2]
#   res_tab[3,4,i] <- lh$`Pr(>F)`[2]
#   
#   res_tab[1,5,i] <- nobs(ols)
#  
# }
# base_balance_bin <- round(res_tab,digits=3)
# #res_tab <- round(res_tab,digits=3)
# save(base_balance_bin, file=paste(path,"papers/seed_free_or_not/base_balance_bin.Rdata",sep="/"))

###this is where midline data analysis starts
dta <- read.csv(paste(path,"midline/data/public/midline.csv", sep="/"))
## merge in randomized staring price
dta <- merge(dta, bse[c("farmer_ID","P1_pric","final_price")], by.x="ID", by.y="farmer_ID", all.x=TRUE)
##create unique village level identifier for clustering of standard errors
dta$cluster_ID <- as.factor(paste(paste(dta$dist_ID,dta$sub_ID, sep="_"), dta$vil_ID, sep="_"))
dta$used_TP[dta$used_TP=="n/a"] <- NA
dta$used_TP <- dta$used_TP == "Yes"
dta$remembers <- dta$Rec_TP == "Yes" |  dta$Buy_TP  == "Yes"
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
dta$resow[dta$resow == "n/a"] <- NA
dta$gap_fill <-  dta$resow== "Yes"
dta$nr_weed <- as.numeric(as.character(dta$weed_no))
dta$nr_weed[dta$nr_weed == 999] <- NA
dta$plant_date <- as.numeric(as.character(dta$plant_date))
dta$plant_date[dta$plant_date == 999] <- NA
dta$timely_planting <- dta$plant_date < 5

dta$sep_post_harvest[dta$sep_post_harvest == "n/a"] <- NA
dta$sep_post_harvest <- dta$sep_post_harvest=="Yes"

dta$happy_yield <- dta$happy_yield == 1 
dta$happy_drought <- dta$happy_drought ==1
dta$happy_disease <- dta$happy_disease == 1
dta$happy_germinate <- dta$happy_germinate ==1
dta$happy <- dta$happy == 1
dta$layout[dta$layout=="n/a"] <- NA
dta$layout[dta$layout=="98"] <- NA
dta$layout <- dta$layout == 3

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

dta$plan_imp <- (dta$seed_nxt == 1 |  dta$seed_nxt == 2) 
dta$plan_bazooka <- dta$imp_var.Bazooka == "True"
dta$plan_bought <- dta$buy_plan=="Yes"
dta$plan_area <-  as.numeric(as.character(dta$area_plan))
dta$plan_area[dta$plan_area > 50] <- NA

dta$exp1_lmpr[dta$rem_sdType == "n/a"] <- NA
dta$remembers_seed <- dta$exp1_lmpr == "Bazooka"
dta$remembers_seed[dta$remembers_seed == "n/a"] <- NA
dta$seedco[dta$rem_comp == "n/a"] <- NA
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

dta$who_used[dta$who_used == "n/a"] <- NA
dta$who_used <- dta$who_used == "1"

### subset for regressions

dta_reg <- subset(dta,!trial_P)

dta_reg$screening <- (as.numeric(as.character(dta_reg$final_price)))/1000  ###this is the offer price in ashraf et al
dta_reg$signaling <- (as.numeric(as.character(dta_reg$P1_pric)))/1000 

teff_merged$screening  <- (as.numeric(as.character(teff_merged$final_price)))/10  ###this is the offer price in ashraf et al
wheat_merged$screening  <- (as.numeric(as.character(wheat_merged$final_price)))/10  ###this is the offer price in ashraf et al
teff_merged$signaling <- (as.numeric(as.character(teff_merged$p1_pric)))/10 
wheat_merged$signaling <- (as.numeric(as.character(wheat_merged$p1_pric)))/10 

#to measure sunk cost effect, the transaction price is used, that is the amount that is paid after the discount
#as we did a full discount to it is zero for those that got a discount, and the price paid for those that did not get the discount
dta_reg$sunk <- as.numeric(!dta_reg$discounted)*dta_reg$screening ## this would be the transaction price in ahsraf et al
#dta_reg$sunk <- as.numeric(!dta_reg$discounted) ## this would be the transaction price in ahsraf et al
teff_merged$sunk <- as.numeric(teff_merged$discounted*teff_merged$screening) 
wheat_merged$sunk <- as.numeric(wheat_merged$discounted*wheat_merged$screening) 

dta_reg$d_screening <- dta_reg$screening - mean(dta_reg$screening, na.rm=TRUE)
dta_reg$d_signaling <- dta_reg$signaling - mean(dta_reg$signaling, na.rm=TRUE)
dta_reg$d_sunk <- dta_reg$sunk - mean(dta_reg$sunk, na.rm=TRUE)

teff_merged$d_screening <- teff_merged$screening - mean(teff_merged$screening, na.rm=TRUE)
teff_merged$d_signaling <- teff_merged$signaling - mean(teff_merged$signaling, na.rm=TRUE)
teff_merged$d_sunk <- teff_merged$sunk - mean(teff_merged$sunk, na.rm=TRUE)

wheat_merged$d_screening <- wheat_merged$screening - mean(wheat_merged$screening, na.rm=TRUE)
wheat_merged$d_signaling <- wheat_merged$signaling - mean(wheat_merged$signaling, na.rm=TRUE)
wheat_merged$d_sunk <- wheat_merged$sunk - mean(wheat_merged$sunk, na.rm=TRUE)

names(teff_merged)[names(teff_merged) == "ttpack_used"] <- "used_TP"
names(teff_merged)[names(teff_merged) == "pureseed_t"] <- "TP_separate"
names(teff_merged)[names(teff_merged) == "barusedseed_t"] <- "who_used"
names(teff_merged)[names(teff_merged) == "harvseparate_t"] <- "sep_post_harvest"

names(wheat_merged)[names(wheat_merged) == "wtpack_used"] <- "used_TP"
names(wheat_merged)[names(wheat_merged) == "pureseed_w"] <- "TP_separate"
names(wheat_merged)[names(wheat_merged) == "barusedseed_w"] <- "who_used"
names(wheat_merged)[names(wheat_merged) == "harvseparate_w"] <- "sep_post_harvest"


###table 1 - impact on use
#iterate over outcomes - start with Uganda maize seed
outcomes <- c("used_TP","TP_separate","who_used","sep_post_harvest" )

index_use <- icwIndex(xmat=dta_reg[outcomes]) #x
dta_reg <- data.frame(dta_reg,index_use)
names(dta_reg)[names(dta_reg) == 'index'] <- 'index_use'

outcomes <- c(outcomes,"index_use" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta_reg)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta_reg)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta_reg)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}

res_tab <- round(res_tab,digits=3)
save(res_tab, file=paste(path,"papers/seed_free_or_not/res_tab.Rdata",sep="/"))

#iterate over outcomes - now for ethiopia - teff
outcomes <- c("used_TP","TP_separate","who_used","sep_post_harvest" )

index_use <- icwIndex(xmat=teff_merged[outcomes]) #x
teff_merged <- data.frame(teff_merged,index_use)
names(teff_merged)[names(teff_merged) == 'index'] <- 'index_use'

outcomes <- c(outcomes,"index_use" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(teff_merged[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(teff_merged[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=teff_merged)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=teff_merged)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=teff_merged)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}

res_tab_teff <- round(res_tab,digits=3)
save(res_tab_teff, file=paste(path,"papers/seed_free_or_not/res_tab_teff.Rdata",sep="/"))


#iterate over outcomes - now for ethiopia - wheat
outcomes <- c("used_TP","TP_separate","who_used","sep_post_harvest" )

index_use <- icwIndex(xmat=wheat_merged[outcomes]) #x
wheat_merged <- data.frame(wheat_merged,index_use)
names(wheat_merged)[names(wheat_merged) == 'index'] <- 'index_use'

outcomes <- c(outcomes,"index_use" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(wheat_merged[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(wheat_merged[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"d_screening+d_sunk+d_signaling",sep="~")), data=wheat_merged)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"d_sunk+d_screening+d_signaling",sep="~")), data=wheat_merged)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"d_signaling+d_screening+d_sunk",sep="~")), data=wheat_merged)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}

res_tab_wheat <- round(res_tab,digits=3)
save(res_tab_wheat, file=paste(path,"papers/seed_free_or_not/res_tab_wheat.Rdata",sep="/"))

###pooled
### standard normalize all outcome variables
dta_reg_pool <- rbind(cbind(dta_reg[c("d_sunk","d_screening","d_signaling")],matStand(dta_reg[outcomes])),
                      cbind(teff_merged[c("d_sunk","d_screening","d_signaling")],matStand(teff_merged[outcomes])),
                      cbind(wheat_merged[c("d_sunk","d_screening","d_signaling")],matStand(wheat_merged[outcomes])))



#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg_pool[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg_pool[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"d_screening+d_sunk+d_signaling",sep="~")), data=dta_reg_pool)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"d_sunk+d_screening+d_signaling",sep="~")), data=dta_reg_pool)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"d_signaling+d_screening+d_sunk",sep="~")), data=dta_reg_pool)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}

res_tab_pooled <- round(res_tab,digits=3)
save(res_tab_pooled, file=paste(path,"papers/seed_free_or_not/res_tab_pooled.Rdata",sep="/"))







#table 2: impact on practices



#iterate over outcomes
outcomes <- c("cor_plant","use_fert","use_chem","gap_fill","nr_weed","timely_planting")

index_use <- icwIndex(xmat=dta_reg[outcomes]) #x
dta_reg <- data.frame(dta_reg,index_use)
names(dta_reg)[names(dta_reg) == 'index'] <- 'index_pract'

outcomes <- c(outcomes,"index_pract" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening+d_sunk+d_signaling",sep="~")), data=dta_reg)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk+d_screening+d_signaling",sep="~")), data=dta_reg)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling+d_screening+d_sunk",sep="~")), data=dta_reg)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_pract <- round(res_tab,digits=3)
save(res_tab_pract, file=paste(path,"papers/seed_free_or_not/res_tab_pract.Rdata",sep="/"))

## now for eth - teff 

names(teff_merged)[names(teff_merged) == "fertilizer_t"] <- "use_fert"
names(wheat_merged)[names(wheat_merged) == "fertilizer_w"] <- "use_fert"

names(teff_merged)[names(teff_merged) == "chemicals_t"] <- "use_chem"
names(wheat_merged)[names(wheat_merged) == "chemicals_w"] <- "use_chem"

#iterate over outcomes
outcomes <- c("use_fert","use_chem")

index_use <- icwIndex(xmat=teff_merged[outcomes]) #x
teff_merged <- data.frame(teff_merged,index_use)
names(teff_merged)[names(teff_merged) == 'index'] <- 'index_pract'

outcomes <- c(outcomes,"index_pract" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(teff_merged[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(teff_merged[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening+d_sunk+d_signaling",sep="~")), data=teff_merged)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk+d_screening+d_signaling",sep="~")), data=teff_merged)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling+d_screening+d_sunk",sep="~")), data=teff_merged)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_pract_teff <- round(res_tab,digits=3)
save(res_tab_pract_teff, file=paste(path,"papers/seed_free_or_not/res_tab_pract_teff.Rdata",sep="/"))

### now for wheat
#iterate over outcomes
outcomes <- c("use_fert","use_chem")

index_use <- icwIndex(xmat=wheat_merged[outcomes]) #x
wheat_merged <- data.frame(wheat_merged,index_use)
names(wheat_merged)[names(wheat_merged) == 'index'] <- 'index_pract'

outcomes <- c(outcomes,"index_pract" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(wheat_merged[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(wheat_merged[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening+d_sunk+d_signaling",sep="~")), data=wheat_merged)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk+d_screening+d_signaling",sep="~")), data=wheat_merged)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling+d_screening+d_sunk",sep="~")), data=wheat_merged)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_pract_wheat <- round(res_tab,digits=3)
save(res_tab_pract_wheat, file=paste(path,"papers/seed_free_or_not/res_tab_pract_wheat.Rdata",sep="/"))

###pooled regressions

### standard normalize all outcome variables
dta_reg_pool <- rbind(cbind(dta_reg[c("d_sunk","d_screening","d_signaling")],matStand(dta_reg[outcomes])),
                      cbind(teff_merged[c("d_sunk","d_screening","d_signaling")],matStand(teff_merged[outcomes])),
                      cbind(wheat_merged[c("d_sunk","d_screening","d_signaling")],matStand(wheat_merged[outcomes])))



#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg_pool[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg_pool[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"d_screening+d_sunk+d_signaling",sep="~")), data=dta_reg_pool)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"d_sunk+d_screening+d_signaling",sep="~")), data=dta_reg_pool)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"d_signaling+d_screening+d_sunk",sep="~")), data=dta_reg_pool)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}

res_tab_pract_pooled <- round(res_tab,digits=3)
save(res_tab_pract_pooled, file=paste(path,"papers/seed_free_or_not/res_tab_pract_pooled.Rdata",sep="/"))




# ###now run binary analysis
# outcomes <- c("cor_plant","use_fert_inorg","use_fert_org","use_chem","gap_fill","nr_weed","timely_planting")
# 
# 
# index_use <- icwIndex(xmat=dta[outcomes]) #x
# dta <- data.frame(dta,index_use)
# names(dta)[names(dta) == 'index'] <- 'index_pract'
# 
# outcomes <- c(outcomes,"index_pract" )
# 
# #matrix to store results
# res_tab <-  array(NA,dim=c(3,5,length(outcomes)))
# 
# for (i in 1:length(outcomes)) {
#   ### here we need clustering
#   ### here we need clustering
#   ols <- lm(as.formula( paste(outcomes[i],"discounted+trial_P",sep="~")), data=dta)
#   vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
#   coef_test(ols, vcov_cluster)$beta
#   
#   res_tab[1,1,i] <-  coef_test(ols, vcov_cluster)$beta[1]
#   res_tab[2,1,i] <- coef_test(ols, vcov_cluster)$SE[1]
#   
#   res_tab[1,2,i]  <-  coef_test(ols, vcov_cluster)$beta[2]
#   res_tab[2,2,i] <-  coef_test(ols, vcov_cluster)$SE[2]
#   res_tab[3,2,i] <-  coef_test(ols, vcov_cluster)$p_Satt[2]
#   ### regression for sunk cost effect - clustering
#   
#   
#   res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
#   res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
#   res_tab[3,3,i] <-  coef_test(ols, vcov_cluster)$p_Satt[3]
#   
#   lh <-   linearHypothesis(ols, c("discountedTRUE = trial_PTRUE"),vcov=vcov_cluster )
#   res_tab[1,4,i] <- lh$F[2]
#   res_tab[3,4,i] <- lh$`Pr(>F)`[2]
#   
#   res_tab[1,5,i] <- nobs(ols)
#   
# }
# 
# res_tab_pract_bin <- round(res_tab,digits=3)
# save(res_tab_pract_bin, file=paste(path,"papers/seed_free_or_not/res_tab_pract_bin.Rdata",sep="/"))

#table 3: impact on characteristics

#iterate over outcomes
outcomes <- c("happy_yield",
"happy_drought",
"happy_disease","happy_germinate", "happy")

index_use <- icwIndex(xmat=dta_reg[outcomes]) #x
dta_reg <- data.frame(dta_reg,index_use)
names(dta_reg)[names(dta_reg) == 'index'] <- 'index_char'

outcomes <- c(outcomes,"index_char" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta_reg)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta_reg)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta_reg)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_char <- round(res_tab,digits=3)
save(res_tab_char, file=paste(path,"papers/seed_free_or_not/res_tab_char.Rdata",sep="/"))

#table 4: impact on yield


#iterate over outcomes
outcomes <- c("area_tot",
              "prod_kg_tot_ihs",
              "yield_tot_ihs","area_trial",
              "prod_kg_trial_ihs",
              "yield_trial_ihs")

index_use <- icwIndex(xmat=dta_reg[outcomes]) #x
dta_reg <- data.frame(dta_reg,index_use)

names(dta_reg)[names(dta_reg) == 'index'] <- 'index_yield'

outcomes <- c(outcomes,"index_yield" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta_reg)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta_reg)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta_reg)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_yield <- round(res_tab,digits=3)
save(res_tab_yield, file=paste(path,"papers/seed_free_or_not/res_tab_yield.Rdata",sep="/"))

#table 4: intentions

#iterate over outcomes
outcomes <- c("plan_imp",
              "plan_bazooka",
              "plan_area",
              "plan_bought")

index_use <- icwIndex(xmat=dta_reg[outcomes]) #x
dta_reg <- data.frame(dta_reg,index_use)

names(dta_reg)[names(dta_reg) == 'index'] <- 'index_plan'

outcomes <- c(outcomes,"index_plan" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta_reg)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta_reg)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta_reg)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_plan <- round(res_tab,digits=3)
save(res_tab_plan, file=paste(path,"papers/seed_free_or_not/res_tab_plan.Rdata",sep="/"))

###impact on 

#iterate over outcomes
outcomes <- c("remembers_seed","remembers_paying","price_diff_sq")

index_use <- icwIndex(xmat=dta_reg[outcomes]) #x
dta_reg <- data.frame(dta_reg,index_use)

names(dta_reg)[names(dta_reg) == 'index'] <- 'index_plan'

outcomes <- c(outcomes, "index_plan" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening+d_sunk+d_signaling",sep="~")), data=dta_reg)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk+d_screening+d_signaling",sep="~")), data=dta_reg)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling+d_screening+d_sunk",sep="~")), data=dta_reg)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}

res_tab_path <- round(res_tab,digits=3)
save(res_tab_path, file=paste(path,"papers/seed_free_or_not/res_tab_path.Rdata",sep="/"))


### now for eth teff
names(teff_merged)[names(teff_merged) == "remseedtype_t"] <- "remembers_seed"
names(teff_merged)[names(teff_merged) == "remprice_t"] <- "remembers_paying"
names(teff_merged)[names(teff_merged) == "pricediff_t"] <- "price_diff_sq"

names(wheat_merged)[names(wheat_merged) == "remseedtype_w"] <- "remembers_seed"
names(wheat_merged)[names(wheat_merged) == "remprice_w"] <- "remembers_paying"
names(wheat_merged)[names(wheat_merged) == "pricediff_w"] <- "price_diff_sq"

#iterate over outcomes
outcomes <- c("remembers_seed","remembers_paying","price_diff_sq")
#### there is an issue with remembers seed causing a crash. Nobody remembers so I just add 10 random - this needs to be deleted if the data is corrected (as it seems very unlikely nobody remembers teff seed type) 
# set.seed(123)  # for reproducibility
# flip_indices <- sample(seq_len(nrow(teff_merged)), 10)
# teff_merged$remembers_seed[flip_indices] <- 1

index_use <- icwIndex(xmat=teff_merged[outcomes]) #x
teff_merged <- data.frame(teff_merged,index_use)

names(teff_merged)[names(teff_merged) == 'index'] <- 'index_plan'

outcomes <- c(outcomes, "index_plan" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(teff_merged[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(teff_merged[outcomes], 2, sd, na.rm=T)


for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening+d_sunk+d_signaling",sep="~")), data=teff_merged)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk+d_screening+d_signaling",sep="~")), data=teff_merged)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling+d_screening+d_sunk",sep="~")), data=teff_merged)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_path_teff <- round(res_tab,digits=3)
save(res_tab_path_teff, file=paste(path,"papers/seed_free_or_not/res_tab_path_teff.Rdata",sep="/"))

### now for eth wheat

#iterate over outcomes
outcomes <- c("remembers_seed","remembers_paying","price_diff_sq")

index_use <- icwIndex(xmat=wheat_merged[outcomes]) #x
wheat_merged <- data.frame(wheat_merged,index_use)

names(wheat_merged)[names(wheat_merged) == 'index'] <- 'index_plan'

outcomes <- c(outcomes, "index_plan" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(wheat_merged[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(wheat_merged[outcomes], 2, sd, na.rm=T)


for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening+d_sunk+d_signaling",sep="~")), data=wheat_merged)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk+d_screening+d_signaling",sep="~")), data=wheat_merged)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling+d_screening+d_sunk",sep="~")), data=wheat_merged)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_path_wheat <- round(res_tab,digits=3)
save(res_tab_path_wheat, file=paste(path,"papers/seed_free_or_not/res_tab_path_wheat.Rdata",sep="/"))

###pooled
### standard normalize all outcome variables
dta_reg_pool <- rbind(cbind(dta_reg[c("d_sunk","d_screening","d_signaling")],matStand(dta_reg[outcomes])),
                      cbind(teff_merged[c("d_sunk","d_screening","d_signaling")],matStand(teff_merged[outcomes])),
                      cbind(wheat_merged[c("d_sunk","d_screening","d_signaling")],matStand(wheat_merged[outcomes])))



#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg_pool[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg_pool[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"d_screening+d_sunk+d_signaling",sep="~")), data=dta_reg_pool)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"d_sunk+d_screening+d_signaling",sep="~")), data=dta_reg_pool)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"d_signaling+d_screening+d_sunk",sep="~")), data=dta_reg_pool)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}

res_tab_path_pooled <- round(res_tab,digits=3)
save(res_tab_path_pooled, file=paste(path,"papers/seed_free_or_not/res_tab_path_pooled.Rdata",sep="/"))



# ###now run binary analysis
# outcomes <- c("remembers_paying","price_diff_sq","remembers_seed",
#               "remembers_comp")
# 
# index_use <- icwIndex(xmat=dta[outcomes]) #x
# dta <- data.frame(dta,index_use)
# 
# names(dta)[names(dta) == 'index'] <- 'index_plan'
# 
# outcomes <- c(outcomes, "index_plan" )
# #matrix to store results
# res_tab <-  array(NA,dim=c(3,5,length(outcomes)))
# 
# 
# 
# for (i in 1:length(outcomes)) {
#   ### here we need clustering
# 
#   ols <- lm(as.formula( paste(outcomes[i],"discounted+trial_P",sep="~")), data=dta)
#   vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
#   coef_test(ols, vcov_cluster)$beta
#   
#   res_tab[1,1,i] <-  coef_test(ols, vcov_cluster)$beta[1]
#   res_tab[2,1,i] <- coef_test(ols, vcov_cluster)$SE[1]
#   
#   res_tab[1,2,i]  <-  coef_test(ols, vcov_cluster)$beta[2]
#   res_tab[2,2,i] <-  coef_test(ols, vcov_cluster)$SE[2]
#   res_tab[3,2,i] <-  coef_test(ols, vcov_cluster)$p_Satt[2]
#   ### regression for sunk cost effect - clustering
#   
#   
#   res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
#   res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
#   res_tab[3,3,i] <-  coef_test(ols, vcov_cluster)$p_Satt[3]
# if (i %in% 3:5) {  
#   lh <-   linearHypothesis(ols, c("discountedTRUE = trial_PTRUE"),vcov=vcov_cluster )
#   res_tab[1,4,i] <- lh$F[2]
#   res_tab[3,4,i] <- lh$`Pr(>F)`[2]
# }
#   res_tab[1,5,i] <- nobs(ols)
#   
# }
# 
# res_tab_path_bin <- round(res_tab,digits=3)
# save(res_tab_path_bin, file=paste(path,"papers/seed_free_or_not/res_tab_path_bin.Rdata",sep="/"))
dta_mid <- dta

####analysis of actual behavior in subsequent season
### read in endline data (anonymized version)
dta <- read.csv(paste(path,"endline/data/public/endline.csv", sep="/"))

## keep only 
dta <- subset(dta, cont == FALSE & (trial_P== TRUE | paid_pac == TRUE | discounted == TRUE))

##where do we get dta$final_price and dta$P1_pric
#in bse 


dta <- merge(dta,bse[c("farmer_ID","P1_pric","final_price","cluster_ID")], by.x="ID", by.y="farmer_ID", all.x=TRUE)
dta <- subset(dta, !is.na(cluster_ID))

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
dta$rnd_adopt[dta$maize_var_selected == "n/a"] <- NA 


dta$rnd_bazo <-  ((dta$maize_var_selected == "Bazooka") & (dta$single_source_selected %in% letters[4:9]  & (dta$times_recycled_selected %in% 1)))
dta$rnd_bazo[dta$no_grow] <- NA 
dta$rnd_bazo[dta$maize_var_selected =="n/a"] <- NA 



### seed quantity
dta$seed_qty <- as.numeric(as.character(dta$seed_qty))
dta$seed_qty[dta$seed_qty == 999] <- NA 

dta$imp_seed_qty_rnd <- dta$rnd_adopt*dta$seed_qty
dta$imp_seed_qty_rnd[dta$imp_seed_qty_rnd == 0] <- NA 



dta <- trim("imp_seed_qty_rnd", dta)

dta$imp_seed_qty_rnd_acre <- dta$rnd_bazo*dta$seed_qty
dta$imp_seed_qty_rnd_acre[dta$imp_seed_qty_rnd_acre == 0] <- NA 
dta <- trim("imp_seed_qty_rnd_acre", dta)

#dta$size_selected <- as.numeric(as.character(dta$size_selected)) 
### seed quantity per area
#dta$imp_seed_qty_rnd_acre <- dta$imp_seed_qty_rnd/dta$size_selected  



###production
dta$bag_harv[dta$bag_harv == "999"] <- NA
dta$production <- as.numeric(as.character(dta$bag_harv))*as.numeric(as.character(dta$bag_kg))
dta <- trim("production", dta)

## productivity
dta$productivity <- dta$production/dta$size_selected
dta <- trim("productivity", dta)



dta$remembers_seed <- dta$Trial_group.TP_exp1_lmpr == "Bazooka"
dta$remembers_comp <- dta$Trial_group.TP_seedco == "4"
#dta$value_shop <- as.numeric(as.character(dta$value))

dta$remembers_paying <- FALSE
dta$remembers_paying <- dta$check3.pay == "Yes" | dta$check3.pay_d=="Yes"
dta$remembers_paying[dta$check3.pay == "n/a" & dta$check3.pay_d=="n/a"] <- NA
dta$remembers_paying[dta$trial_P] <- NA


dta$value_paid <- as.numeric(as.character(dta$final_price))
### quadratic loss for error in price recall
dta$price_diff_sq <- abs(as.numeric(as.character(dta$final_price)) - as.numeric(as.character(dta$check3.price_paid)))
dta$price_diff_sq[dta$trial_P] <- NA

dta_reg <- subset(dta,!trial_P)
dta_reg$screening <- (as.numeric(as.character(dta_reg$final_price)))/1000
dta_reg$signaling <- (as.numeric(as.character(dta_reg$P1_pric)))/1000
#to measure sunk cost effect, the transaction price is used, that is the amount that is paid after the discount
#as we did a full discount to it is zero for those that got a discount, and the price paid for those that did not get the discount
dta_reg$sunk <- as.numeric(!dta_reg$discounted)

dta_reg$d_screening <- dta_reg$screening - mean(dta_reg$screening, na.rm=TRUE)
dta_reg$d_signaling <- dta_reg$signaling - mean(dta_reg$signaling, na.rm=TRUE)
dta_reg$d_sunk <- dta_reg$sunk - mean(dta_reg$sunk, na.rm=TRUE)

names(teff_merged)[names(teff_merged) == "recycled3_t"] <- "rnd_adopt"
names(teff_merged)[names(teff_merged) == "usedpromoted_t"] <- "rnd_bazo"
names(teff_merged)[names(teff_merged) == "qtyimpseed_t"] <- "imp_seed_qty_rnd"
names(teff_merged)[names(teff_merged) == "prodkg_t"] <- "production"
names(teff_merged)[names(teff_merged) == "yieldkgha_t"] <- "productivity"

names(wheat_merged)[names(wheat_merged) == "recycled3_w"] <- "rnd_adopt"
names(wheat_merged)[names(wheat_merged) == "usedpromoted_w"] <- "rnd_bazo"
names(wheat_merged)[names(wheat_merged) == "qtyimpseed_w"] <- "imp_seed_qty_rnd"
names(wheat_merged)[names(wheat_merged) == "prodkg_w"] <- "production"
names(wheat_merged)[names(wheat_merged) == "yieldkgha_w"] <- "productivity"



#iterate over outcomes
outcomes <- c("rnd_adopt", "rnd_bazo", "imp_seed_qty_rnd","production", "productivity" )
index_use <- icwIndex(xmat=dta_reg[outcomes]) #x
dta_reg <- data.frame(dta_reg,index_use)

names(dta_reg)[names(dta_reg) == 'index'] <- 'index_next_season'

outcomes <- c(outcomes,"index_next_season" )

res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)


for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta_reg[!(dta_reg$trial_P),])
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta_reg[!(dta_reg$trial_P),])  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta_reg[!(dta_reg$trial_P),])
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_next_season <- round(res_tab,digits=3)
save(res_tab_next_season, file=paste(path,"papers/seed_free_or_not/res_tab_next_season.Rdata",sep="/"))

##now for eth - teff
#iterate over outcomes
outcomes <- c("rnd_adopt", "rnd_bazo", "imp_seed_qty_rnd","production", "productivity" )
index_use <- icwIndex(xmat=teff_merged[outcomes]) #x
teff_merged <- data.frame(teff_merged,index_use)

names(teff_merged)[names(teff_merged) == 'index'] <- 'index_next_season'

outcomes <- c(outcomes,"index_next_season" )

res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(teff_merged[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(teff_merged[outcomes], 2, sd, na.rm=T)


for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=teff_merged)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=teff_merged)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=teff_merged)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_next_season_teff <- round(res_tab,digits=3)
save(res_tab_next_season_teff, file=paste(path,"papers/seed_free_or_not/res_tab_next_season_teff.Rdata",sep="/"))

##now for eth - wheat
#iterate over outcomes
outcomes <- c("rnd_adopt", "rnd_bazo", "imp_seed_qty_rnd","production", "productivity" )
index_use <- icwIndex(xmat=wheat_merged[outcomes]) #x
wheat_merged <- data.frame(wheat_merged,index_use)

names(wheat_merged)[names(wheat_merged) == 'index'] <- 'index_next_season'

outcomes <- c(outcomes,"index_next_season" )

res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(wheat_merged[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(wheat_merged[outcomes], 2, sd, na.rm=T)


for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening+d_sunk+d_signaling",sep="~")), data=wheat_merged)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk+d_screening+d_signaling",sep="~")), data=wheat_merged)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling+d_screening+d_sunk",sep="~")), data=wheat_merged)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
res_tab_next_season_wheat <- round(res_tab,digits=3)
save(res_tab_next_season_wheat, file=paste(path,"papers/seed_free_or_not/res_tab_next_season_wheat.Rdata",sep="/"))


###pooled
### standard normalize all outcome variables
dta_reg_pool <- rbind(cbind(dta_reg[c("d_sunk","d_screening","d_signaling")],matStand(dta_reg[outcomes])),
                      cbind(teff_merged[c("d_sunk","d_screening","d_signaling")],matStand(teff_merged[outcomes])),
                      cbind(wheat_merged[c("d_sunk","d_screening","d_signaling")],matStand(wheat_merged[outcomes])))



#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg_pool[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg_pool[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"d_screening+d_sunk+d_signaling",sep="~")), data=dta_reg_pool)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"d_sunk+d_screening+d_signaling",sep="~")), data=dta_reg_pool)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"d_signaling+d_screening+d_sunk",sep="~")), data=dta_reg_pool)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}

res_tab_next_season_pooled <- round(res_tab,digits=3)
save(res_tab_next_season_pooled, file=paste(path,"papers/seed_free_or_not/res_tab_next_season_pooled.Rdata",sep="/"))










outcomes <- c("remembers_seed","remembers_paying","price_diff_sq"              )

index_use <- icwIndex(xmat=dta_reg[outcomes]) #x
dta_reg <- data.frame(dta_reg,index_use)

names(dta_reg)[names(dta_reg) == 'index'] <- 'index_plan'

outcomes <- c(outcomes, "index_plan" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening+d_sunk+d_signaling",sep="~")), data=dta_reg)
  
  res_tab[1,2,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,2,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,2,i] <- summary(ols)$coefficients[2,4]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk+d_screening+d_signaling",sep="~")), data=dta_reg)  
  
  res_tab[1,3,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,3,i] <- summary(ols)$coefficients[2,2]
  res_tab[3,3,i] <- summary(ols)$coefficients[2,4]
  
  ### regression for signaling effect - no clustering
  
  ols <- lm(as.formula( paste(outcomes[i],"signaling+d_screening+d_sunk",sep="~")), data=dta_reg)
  
  res_tab[1,4,i]  <- summary(ols)$coefficients[2,1]
  res_tab[2,4,i] <-  summary(ols)$coefficients[2,2]
  res_tab[3,4,i] <- summary(ols)$coefficients[2,4]
  res_tab[1,5,i] <- nobs(ols)
  
  
}
## for subset of those that bought


res_tab_path_next_season <- round(res_tab,digits=3)
save(res_tab_path_next_season, file=paste(path,"papers/seed_free_or_not/res_tab_path_next_season.Rdata",sep="/"))
# ###now run binary analysis
# 
# outcomes <- c("remembers_paying","price_diff_sq","remembers_seed",
#               "remembers_comp")
# 
# index_use <- icwIndex(xmat=dta[outcomes]) #x
# dta <- data.frame(dta,index_use)
# 
# names(dta)[names(dta) == 'index'] <- 'index_plan'
# 
# outcomes <- c(outcomes, "index_plan" )
# 
# #matrix to store results
# res_tab <-  array(NA,dim=c(3,5,length(outcomes)))
# 
# 
# 
# for (i in 1:length(outcomes)) {
#   ### here we need clustering
#   
#   ols <- lm(as.formula( paste(outcomes[i],"discounted+trial_P",sep="~")), data=dta)
#   vcov_cluster <- vcovCR(ols,cluster=dta$cluster_ID,type="CR2")
#   coef_test(ols, vcov_cluster)$beta
#   
#   res_tab[1,1,i] <-  coef_test(ols, vcov_cluster)$beta[1]
#   res_tab[2,1,i] <- coef_test(ols, vcov_cluster)$SE[1]
#   
#   res_tab[1,2,i]  <-  coef_test(ols, vcov_cluster)$beta[2]
#   res_tab[2,2,i] <-  coef_test(ols, vcov_cluster)$SE[2]
#   res_tab[3,2,i] <-  coef_test(ols, vcov_cluster)$p_Satt[2]
#   ### regression for sunk cost effect - clustering
#   
#   
#   res_tab[1,3,i]  <- coef_test(ols, vcov_cluster)$beta[3]
#   res_tab[2,3,i] <- coef_test(ols, vcov_cluster)$SE[3]
#   res_tab[3,3,i] <-  coef_test(ols, vcov_cluster)$p_Satt[3]
#   if (i %in% 3:5) {  
#     lh <-   linearHypothesis(ols, c("discountedTRUE = trial_PTRUE"),vcov=vcov_cluster )
#     res_tab[1,4,i] <- lh$F[2]
#     res_tab[3,4,i] <- lh$`Pr(>F)`[2]
#   }
#   res_tab[1,5,i] <- nobs(ols)
#   
# }
# 
# res_tab_path_next_season_bin <- round(res_tab,digits=3)
# save(res_tab_path_next_season_bin, file=paste(path,"papers/seed_free_or_not/res_tab_path_next_season_bin.Rdata",sep="/"))
