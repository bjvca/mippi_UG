rm(list=ls())
path <- getwd()
library(dplyr)
library(haven)
library(car)
#to use vcovCL for cluster-robust SEs
library(sandwich)
library(lmtest)
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

## Helper to extract cluster-robust (CR0) coefficient, SE, and p-value
cr_extract <- function(ols, cluster, coef_row=2) {
  cr <- coeftest(ols, vcov=vcovCL(ols, cluster=cluster, type="HC0"))
  c(cr[coef_row,1], cr[coef_row,2], cr[coef_row,4])
}
#get baseline data Uganda
datapath <- paste0(path, "/data")
#create treatmetn cluster indicator for clustering SE
bse <- read.csv(paste(datapath,"baseline.csv",sep="/"))
bse$cluster_ID <- as.factor(paste(paste(bse$distID,bse$subID, sep="_"), bse$vilID, sep="_"))

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


bse$age_head <- as.numeric(as.character(bse$age))
bse$age_head[bse$age_head==999] <- NA

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

## Drop right-censored farmers (accepted first offer — true WTP unknown)
## These farmers accepted the seller's first ask price without negotiation (rounds==1
## and buyer accepted), so their observed transaction price is a lower bound on true WTP.
n_before <- nrow(bse_reg)
bse_reg <- subset(bse_reg, !(accepts == "buyer" & rounds == 1))
n_after <- nrow(bse_reg)
cat(sprintf("Dropped %d right-censored farmers (accepted first offer). N: %d -> %d\n",
            n_before - n_after, n_before, n_after))


bse_reg$screening <- (as.numeric(as.character(bse_reg$final_price)))/1000
bse_reg$signaling <- (as.numeric(as.character(bse_reg$P1_pric)))/1000
##important: it is the non-discounted (those that pay the full price) that identify the sunk cost
##they are the filtered ones that also pay a postive price
bse_reg$sunk <- as.numeric(!bse_reg$discounted)

bse_reg$d_screening <- bse_reg$screening - mean(bse_reg$screening, na.rm=TRUE)
bse_reg$d_signaling <- bse_reg$signaling - mean(bse_reg$signaling, na.rm=TRUE)
bse_reg$d_sunk <- bse_reg$sunk - mean(bse_reg$sunk, na.rm=TRUE)

###balance tables (appendix) - comparing discounted vs non-discounted
outcomes_bal <- c("age_head","prim_head","male_head","hh_size","dist_ag","quality_use","promo_use_rand","source_rand","often_rand","acre_rand","yield_rand")

## Uganda balance table
# dim 1: estimate, SE, p-value; dim 2: non-discounted mean, discounted mean, difference, N; dim 3: outcomes
bal_tab_uga <- array(NA, dim=c(3, 4, length(outcomes_bal)))
for (i in 1:length(outcomes_bal)) {
  bal_tab_uga[1,1,i] <- mean(bse_reg[!bse_reg$discounted, outcomes_bal[i]], na.rm=T)
  bal_tab_uga[2,1,i] <- sd(bse_reg[!bse_reg$discounted, outcomes_bal[i]], na.rm=T)
  bal_tab_uga[1,2,i] <- mean(bse_reg[bse_reg$discounted==TRUE, outcomes_bal[i]], na.rm=T)
  bal_tab_uga[2,2,i] <- sd(bse_reg[bse_reg$discounted==TRUE, outcomes_bal[i]], na.rm=T)
  ols <- lm(as.formula(paste(outcomes_bal[i], "discounted", sep="~")), data=bse_reg)
  bal_tab_uga[1,3,i] <- summary(ols)$coefficients[2,1]
  bal_tab_uga[2,3,i] <- summary(ols)$coefficients[2,2]
  bal_tab_uga[3,3,i] <- summary(ols)$coefficients[2,4]
  bal_tab_uga[1,4,i] <- nobs(ols)
}
bal_tab_uga <- round(bal_tab_uga, digits=3)
save(bal_tab_uga, file=paste(path,"bal_tab_uga_nocens.Rdata",sep="/"))

###WTP graph
library(ggplot2)

bse_graph <- bse
bse_graph$final_price[bse_graph$final_price== "3000"] <- NA

bse_graph$final_price[bse_graph$final_price == "9000" & bse_graph$P1_pric== "9000"] <- NA
bse_graph$final_price[bse_graph$final_price == "10000" & bse_graph$P1_pric== "10000"] <- NA
bse_graph$final_price[bse_graph$final_price == "11000" & bse_graph$P1_pric== "11000"] <- NA
bse_graph$final_price[bse_graph$final_price == "12000" & bse_graph$P1_pric== "12000"] <- NA
breaks <- seq(from = 4000, to= 10000, by =1000)
bse_graph$final_price <- cut(bse_graph$final_price, breaks = breaks,labels = c("4000","5000","6000","7000","8000","9000"), right = FALSE)
bse_graph$final_price <- as.numeric(as.character(bse_graph$final_price))
bse_graph$final_price[bse_graph$final_price < 5000] <- NA
bse_graph <- subset(bse_graph, !is.na(final_price) )

# Create a data frame of counts and proportions
price_prop <- as.data.frame(table(bse_graph$final_price))
colnames(price_prop) <- c("Price", "Count")

# Convert to numeric for sorting/plotting
price_prop$Price <- as.numeric(as.character(price_prop$Price))

# Total sample size
n_total <- sum(price_prop$Count)

# Compute proportions and 95% confidence intervals
price_prop <- price_prop |>
  mutate(
    Density = Count / n_total,
    se = sqrt(Density * (1 - Density) / n_total),
    lower = pmax(0, Density - 1.96 * se),  # avoid negative lower bound
    upper = pmin(1, Density + 1.96 * se)
  )

# Plot with confidence intervals
plot_maize <- ggplot(price_prop, aes(x = Price, y = Density)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 100, linewidth = 0.5) +
  labs(
    title = "Demand for Maize Seed (Uganda)",
    x = "Price",
    y = "Proportion of farmers purchasing"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Print plot
plot_maize

# Save as PNG
ggsave(paste(path,"demand_nocens.png",sep="/"), plot_maize, width = 8, height = 5, dpi = 300)

###this is where midline data analysis starts
dta <- read.csv(paste(datapath,"midline.csv", sep="/"))
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
dta$seedco[dta$rem_comp == "n/a"] <- NA
dta$remembers_comp <- dta$seedco == "4"
dta$value_shop <- as.numeric(as.character(dta$value))

dta$remembers_paying <- dta$pay == "Yes" | dta$pay_d=="Yes"
dta$remembers_paying[dta$pay == "n/a" & dta$pay_d=="n/a"] <- NA
dta$remembers_paying[dta$trial_P] <- NA


dta$value_paid <- as.numeric(as.character(dta$final_price))
### absolute error in price recall
dta$price_diff_abs <- abs(as.numeric(as.character(dta$final_price)) - as.numeric(as.character(dta$price_paid)))
dta$price_diff_abs[dta$trial_P] <- NA

dta$who_used[dta$who_used == "n/a"] <- NA
dta$who_used <- dta$who_used == "1"

### subset for regressions

dta_reg <- subset(dta,!trial_P)

## Merge baseline covariates for ANCOVA
dta_reg <- merge(dta_reg, bse_reg[c("farmer_ID", "age_head", "prim_head", "male_head", "hh_size", "acre_rand", "quality_use")], by.x="ID", by.y="farmer_ID", all.x=TRUE)

dta_reg$screening <- (as.numeric(as.character(dta_reg$final_price)))/1000  ###this is the offer price in ashraf et al
dta_reg$signaling <- (as.numeric(as.character(dta_reg$P1_pric)))/1000

#to measure sunk cost effect, the transaction price is used, that is the amount that is paid after the discount
#as we did a full discount, it is zero for those that got a discount, and the price paid for those that did not get the discount
## Save midline dta_reg before loop (endline section overwrites it)
dta_reg_midline <- dta_reg

## Run all regressions twice: once with binary sunk cost (main tables, _01 suffix)
## and once with continuous sunk cost (appendix tables, no suffix)
for (sunk_binary in c(TRUE, FALSE)) {

## restore midline dta_reg at start of each iteration
dta_reg <- dta_reg_midline

file_suffix <- if (sunk_binary) "_01" else ""
if (sunk_binary) {
  dta_reg$sunk <- as.numeric(!dta_reg$discounted)
} else {
  dta_reg$sunk <- as.numeric(!dta_reg$discounted)*dta_reg$screening
}




dta_reg$d_screening <- dta_reg$screening - mean(dta_reg$screening, na.rm=TRUE)
dta_reg$d_signaling <- dta_reg$signaling - mean(dta_reg$signaling, na.rm=TRUE)
dta_reg$d_sunk <- dta_reg$sunk - mean(dta_reg$sunk, na.rm=TRUE)


###table 1 - impact on use
#iterate over outcomes - start with Uganda maize seed
outcomes <- c("used_TP","TP_separate","who_used","sep_post_harvest","cor_plant","use_fert","use_chem" )

dta_reg$index_use <- icwIndex(xmat=dta_reg[outcomes])$index

outcomes <- c(outcomes,"index_use" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,2,i]  <- cr[1]
  res_tab[2,2,i] <- cr[2]
  res_tab[3,2,i] <- cr[3]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,3,i]  <- cr[1]
  res_tab[2,3,i] <- cr[2]
  res_tab[3,3,i] <- cr[3]

  ### regression for signaling effect - no clustering

  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,4,i]  <- cr[1]
  res_tab[2,4,i] <- cr[2]
  res_tab[3,4,i] <- cr[3]
  res_tab[1,5,i] <- nobs(ols)


}

res_tab <- round(res_tab,digits=3)
save(res_tab, file=paste0(path,"/res_tab_nocens",file_suffix,".Rdata"))


#table 2: impact on characteristics

#iterate over outcomes
outcomes <- c("happy_yield",
"happy_drought",
"happy_disease","happy_germinate", "happy")

dta_reg$index_char <- icwIndex(xmat=dta_reg[outcomes])$index

outcomes <- c(outcomes,"index_char" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,2,i]  <- cr[1]
  res_tab[2,2,i] <- cr[2]
  res_tab[3,2,i] <- cr[3]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,3,i]  <- cr[1]
  res_tab[2,3,i] <- cr[2]
  res_tab[3,3,i] <- cr[3]

  ### regression for signaling effect - no clustering

  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,4,i]  <- cr[1]
  res_tab[2,4,i] <- cr[2]
  res_tab[3,4,i] <- cr[3]
  res_tab[1,5,i] <- nobs(ols)


}
res_tab_char <- round(res_tab,digits=3)
save(res_tab_char, file=paste0(path,"/res_tab_char_nocens",file_suffix,".Rdata"))

#table 4: impact on yield


#iterate over outcomes
outcomes <- c("area_tot",
              "prod_kg_tot_ihs",
              "yield_tot_ihs","area_trial",
              "prod_kg_trial_ihs",
              "yield_trial_ihs")

dta_reg$index_yield <- icwIndex(xmat=dta_reg[outcomes])$index

outcomes <- c(outcomes,"index_yield" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,2,i]  <- cr[1]
  res_tab[2,2,i] <- cr[2]
  res_tab[3,2,i] <- cr[3]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,3,i]  <- cr[1]
  res_tab[2,3,i] <- cr[2]
  res_tab[3,3,i] <- cr[3]

  ### regression for signaling effect - no clustering

  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,4,i]  <- cr[1]
  res_tab[2,4,i] <- cr[2]
  res_tab[3,4,i] <- cr[3]
  res_tab[1,5,i] <- nobs(ols)


}
res_tab_yield <- round(res_tab,digits=3)
save(res_tab_yield, file=paste0(path,"/res_tab_yield_nocens",file_suffix,".Rdata"))

#table 4: intentions
## NOTE: additive spec (no interactions) for intentions — cross-channel
## interactions are not hypothesized for planning outcomes

#iterate over outcomes
outcomes <- c("plan_imp",
              "plan_bazooka",
              "plan_area",
              "plan_bought")

dta_reg$index_plan <- icwIndex(xmat=dta_reg[outcomes])$index

outcomes <- c(outcomes,"index_plan" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,2,i]  <- cr[1]
  res_tab[2,2,i] <- cr[2]
  res_tab[3,2,i] <- cr[3]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,3,i]  <- cr[1]
  res_tab[2,3,i] <- cr[2]
  res_tab[3,3,i] <- cr[3]

  ### regression for signaling effect - no clustering

  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,4,i]  <- cr[1]
  res_tab[2,4,i] <- cr[2]
  res_tab[3,4,i] <- cr[3]
  res_tab[1,5,i] <- nobs(ols)


}
res_tab_plan <- round(res_tab,digits=3)
save(res_tab_plan, file=paste0(path,"/res_tab_plan_nocens",file_suffix,".Rdata"))

###impact on recall/pathway
## NOTE: additive spec (no interactions) used for pathway outcomes — these are
## mechanism/recall measures where cross-channel interactions are not hypothesized

#iterate over outcomes
outcomes <- c("remembers_seed","remembers_paying","price_diff_abs")

dta_reg$index_path <- icwIndex(xmat=dta_reg[outcomes])$index

outcomes <- c(outcomes, "index_path" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,2,i]  <- cr[1]
  res_tab[2,2,i] <- cr[2]
  res_tab[3,2,i] <- cr[3]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,3,i]  <- cr[1]
  res_tab[2,3,i] <- cr[2]
  res_tab[3,3,i] <- cr[3]

  ### regression for signaling effect - no clustering

  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,4,i]  <- cr[1]
  res_tab[2,4,i] <- cr[2]
  res_tab[3,4,i] <- cr[3]
  res_tab[1,5,i] <- nobs(ols)


}

res_tab_path <- round(res_tab,digits=3)
save(res_tab_path, file=paste0(path,"/res_tab_path_nocens",file_suffix,".Rdata"))

####analysis of actual behavior in subsequent season
### read in endline data (anonymized version)
dta <- read.csv(paste(datapath,"endline.csv", sep="/"))

## keep only
dta <- subset(dta, cont == FALSE & (trial_P== TRUE | paid_pac == TRUE | discounted == TRUE))

##where do we get dta$final_price and dta$P1_pric
#in bse
## build cluster_ID lookup from full baseline (so trial_P farmers also get cluster_ID)
bse_full_tmp <- read.csv(paste(datapath,"baseline.csv",sep="/"))
bse_full_tmp$cluster_ID <- as.factor(paste(paste(bse_full_tmp$distID,bse_full_tmp$subID, sep="_"), bse_full_tmp$vilID, sep="_"))
## merge cluster_ID from full baseline, plus P1_pric/final_price from bargaining subset (bse)
dta <- merge(dta, bse_full_tmp[c("farmer_ID","cluster_ID")], by.x="ID", by.y="farmer_ID", all.x=TRUE)
dta <- merge(dta, bse[c("farmer_ID","P1_pric","final_price")], by.x="ID", by.y="farmer_ID", all.x=TRUE)
rm(bse_full_tmp)
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
dta$production_ihs <- ihs(dta$production)
dta$productivity_ihs <- ihs(dta$productivity)



dta$remembers_seed <- dta$Trial_group.TP_exp1_lmpr == "Bazooka"
dta$remembers_comp <- dta$Trial_group.TP_seedco == "4"
#dta$value_shop <- as.numeric(as.character(dta$value))

dta$remembers_paying <- dta$check3.pay == "Yes" | dta$check3.pay_d=="Yes"
dta$remembers_paying[dta$check3.pay == "n/a" & dta$check3.pay_d=="n/a"] <- NA
dta$remembers_paying[dta$trial_P] <- NA


dta$value_paid <- as.numeric(as.character(dta$final_price))
### absolute error in price recall
dta$price_diff_abs <- abs(as.numeric(as.character(dta$final_price)) - as.numeric(as.character(dta$check3.price_paid)))
dta$price_diff_abs[dta$trial_P] <- NA

dta_reg <- subset(dta,!trial_P)

## Merge baseline covariates for ANCOVA
dta_reg <- merge(dta_reg, bse_reg[c("farmer_ID", "age_head", "prim_head", "male_head", "hh_size", "acre_rand", "quality_use")], by.x="ID", by.y="farmer_ID", all.x=TRUE)

dta_reg$screening <- (as.numeric(as.character(dta_reg$final_price)))/1000
dta_reg$signaling <- (as.numeric(as.character(dta_reg$P1_pric)))/1000
#to measure sunk cost effect, the transaction price is used, that is the amount that is paid after the discount
#as we did a full discount to it is zero for those that got a discount, and the price paid for those that did not get the discount
if (sunk_binary) {
  dta_reg$sunk <- as.numeric(!dta_reg$discounted)
} else {
  dta_reg$sunk <- as.numeric(!dta_reg$discounted)*dta_reg$screening
}

dta_reg$d_screening <- dta_reg$screening - mean(dta_reg$screening, na.rm=TRUE)
dta_reg$d_signaling <- dta_reg$signaling - mean(dta_reg$signaling, na.rm=TRUE)
dta_reg$d_sunk <- dta_reg$sunk - mean(dta_reg$sunk, na.rm=TRUE)



## Complementary input use on randomly selected plot (mirroring midline trial pack measures)
dta_reg$org_ap[dta_reg$org_ap == "n/a"] <- NA
dta_reg$dap_ap[dta_reg$dap_ap == "n/a" | dta_reg$dap_ap == "98"] <- NA
dta_reg$ur_ap[dta_reg$ur_ap == "n/a" | dta_reg$ur_ap == "98"] <- NA
dta_reg$pest_ap[dta_reg$pest_ap == "n/a" | dta_reg$pest_ap == "98"] <- NA
dta_reg$use_fert_end <- (dta_reg$dap_ap == "Yes") | (dta_reg$ur_ap == "Yes") | (dta_reg$org_ap == "Yes")
dta_reg$use_chem_end <- dta_reg$pest_ap == "Yes"

#iterate over outcomes
outcomes <- c("rnd_adopt", "rnd_bazo","use_fert_end", "use_chem_end", "production_ihs", "productivity_ihs" )
dta_reg$index_next_season <- icwIndex(xmat=dta_reg[outcomes])$index

outcomes <- c(outcomes,"index_next_season" )

res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)


for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,2,i]  <- cr[1]
  res_tab[2,2,i] <- cr[2]
  res_tab[3,2,i] <- cr[3]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,3,i]  <- cr[1]
  res_tab[2,3,i] <- cr[2]
  res_tab[3,3,i] <- cr[3]

  ### regression for signaling effect - no clustering

  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,4,i]  <- cr[1]
  res_tab[2,4,i] <- cr[2]
  res_tab[3,4,i] <- cr[3]
  res_tab[1,5,i] <- nobs(ols)


}
res_tab_next_season <- round(res_tab,digits=3)
save(res_tab_next_season, file=paste0(path,"/res_tab_next_season_nocens",file_suffix,".Rdata"))


## NOTE: additive spec (no interactions) for pathway/recall outcomes
outcomes <- c("remembers_seed","remembers_paying","price_diff_abs"              )

dta_reg$index_path_end <- icwIndex(xmat=dta_reg[outcomes])$index

outcomes <- c(outcomes, "index_path_end" )

#matrix to store results
res_tab <-  array(NA,dim=c(3,5,length(outcomes)))

res_tab[1,1,1:length(outcomes)] <- colMeans(dta_reg[outcomes], na.rm=T)
res_tab[2,1,1:length(outcomes)] <- apply(dta_reg[outcomes], 2, sd, na.rm=T)

for (i in 1:length(outcomes)) {
  ### regression for screening effect - no clustering
  ols <- lm(as.formula( paste(outcomes[i],"screening*d_sunk*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,2,i]  <- cr[1]
  res_tab[2,2,i] <- cr[2]
  res_tab[3,2,i] <- cr[3]
  ### regression for sunk cost effect - clustering
  ols <- lm(as.formula( paste(outcomes[i],"sunk*d_screening*d_signaling",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,3,i]  <- cr[1]
  res_tab[2,3,i] <- cr[2]
  res_tab[3,3,i] <- cr[3]

  ### regression for signaling effect - no clustering

  ols <- lm(as.formula( paste(outcomes[i],"signaling*d_screening*d_sunk",sep="~")), data=dta_reg)

  cr <- cr_extract(ols, dta_reg$cluster_ID)
  res_tab[1,4,i]  <- cr[1]
  res_tab[2,4,i] <- cr[2]
  res_tab[3,4,i] <- cr[3]
  res_tab[1,5,i] <- nobs(ols)


}

res_tab_path_next_season <- round(res_tab,digits=3)
save(res_tab_path_next_season, file=paste0(path,"/res_tab_path_next_season_nocens",file_suffix,".Rdata"))

} # end sunk_binary loop

