
### this file prepares raw data collected during consumer intervention for public release
rm(list = ls())
path <- getwd()
dta <- read.csv("latest.csv")
path <- strsplit(path,"raw")[[1]]

names(dta) <- sub("checkx.", "",names(dta))
names(dta) <- sub("group4.", "",names(dta))
names(dta) <- sub("value.", "",names(dta))
names(dta) <- sub("garden.", "",names(dta))
###  we started with a different form and as a result of variable name change, we need to retreive data from randomly selected plot from a dataset that was exported using a different from
dta_first <- read.csv("first_form.csv")
names(dta_first) <- sub("checkx.", "",names(dta))

dta_first$ID[dta_first$X_uuid =="63c1f5f7-a882-4937-93a9-22fbdab6d02a"] <- "F_685"
## duplicate F_589
dta$ID[dta$X_uuid =="63c1f5f7-a882-4937-93a9-22fbdab6d02a"] <- "F_685"
dta$paid_pack[dta$X_uuid =="63c1f5f7-a882-4937-93a9-22fbdab6d02a"] <- FALSE
dta$cont[dta$X_uuid =="63c1f5f7-a882-4937-93a9-22fbdab6d02a"] <- TRUE

## duplicate F_1036
dta$ID[dta$X_uuid =="b08a64a6-7e3d-4ca7-bcb0-b6ebd8b927a7"] <- "F_1781"
dta$discounted[dta$X_uuid =="b08a64a6-7e3d-4ca7-bcb0-b6ebd8b927a7"] <- TRUE
dta$paid_pack[dta$X_uuid =="b08a64a6-7e3d-4ca7-bcb0-b6ebd8b927a7"] <- FALSE
dta$trial_P[dta$X_uuid =="b08a64a6-7e3d-4ca7-bcb0-b6ebd8b927a7"] <- FALSE
dta$cont[dta$X_uuid =="b08a64a6-7e3d-4ca7-bcb0-b6ebd8b927a7"] <- FALSE

## duplicat F_1835
dta$ID[dta$X_uuid =="55092e2d-a208-4c45-8ecb-0583abb5bad3"] <- "F_1838"



#duplicate F_555
dta$ID[dta$X_uuid =="c525024a-58cc-4e89-b225-43ccd9e5c63b"] <- "F_1202"
dta$discounted[dta$X_uuid =="c525024a-58cc-4e89-b225-43ccd9e5c63b"] <- TRUE
dta$cont[dta$X_uuid =="c525024a-58cc-4e89-b225-43ccd9e5c63b"] <- FALSE

#duplicate F_648
dta$ID[dta$X_uuid =="7cc2e6f0-bac9-4dbc-9de1-efd56858961e"] <- "F_2269"
dta$paid_pack[dta$X_uuid =="7cc2e6f0-bac9-4dbc-9de1-efd56858961e"] <- TRUE
dta$discounted[dta$X_uuid =="7cc2e6f0-bac9-4dbc-9de1-efd56858961e"] <- FALSE


#duplicate F_562
dta$ID[dta$X_uuid =="d38614eb-e7c9-4b8b-8e7e-5b418c1bfab1"] <- "F_2098"
dta$paid_pack[dta$X_uuid =="d38614eb-e7c9-4b8b-8e7e-5b418c1bfab1"] <- TRUE
dta$trial_P[dta$X_uuid =="d38614eb-e7c9-4b8b-8e7e-5b418c1bfab1"] <- FALSE


#duplicate F_1785
dta$ID[dta$X_uuid =="fc9f46e5-b62f-438e-b0cc-3dbf5fdb17cf"] <- "F_2208"
dta$paid_pack[dta$X_uuid =="fc9f46e5-b62f-438e-b0cc-3dbf5fdb17cf"] <- TRUE
dta$discounted[dta$X_uuid =="fc9f46e5-b62f-438e-b0cc-3dbf5fdb17cf"] <- FALSE


#duplicate F_1144
dta$ID[dta$X_uuid =="1d28b574-2ace-4cd3-bc3b-8d7bcfd96546"] <- "F_1418"
dta$discounted[dta$X_uuid =="1d28b574-2ace-4cd3-bc3b-8d7bcfd96546"] <- FALSE


## duplicate F_1924
dta$ID[dta$X_uuid =="54b1221b-4ae2-4be8-88da-e4541a387127"] <- "F_1938"
dta$paid_pack[dta$X_uuid =="54b1221b-4ae2-4be8-88da-e4541a387127"] <- FALSE
dta$trial_P[dta$X_uuid =="54b1221b-4ae2-4be8-88da-e4541a387127"] <- FALSE

## duplicate F_1913
dta$ID[dta$X_uuid =="a6637a25-3190-4bd8-99f7-13f5fd581531"] <- "F_1920"

## duplicate F_1892
dta$ID[dta$X_uuid =="06fd8858-3c90-479f-960d-4fb0d6cd78bb"] <- "F_1895"

## duplicate F_540
dta$ID[dta$X_uuid =="734b02de-f9de-431e-9cff-42f2a03e5aa6"] <- "F_1886"
dta$trial_P[dta$X_uuid =="734b02de-f9de-431e-9cff-42f2a03e5aa6"] <- TRUE
dta$cont[dta$X_uuid =="734b02de-f9de-431e-9cff-42f2a03e5aa6"] <- TRUE

## duplicate F_1203
dta$ID[dta$X_uuid =="c525024a-58cc-4e89-b225-43ccd9e5c63b"] <- "F_1202"
dta$discounted[dta$X_uuid =="c525024a-58cc-4e89-b225-43ccd9e5c63b"] <- TRUE
dta$cont[dta$X_uuid =="c525024a-58cc-4e89-b225-43ccd9e5c63b"] <- FALSE

### this is the waiswa fred case...
dta$ID[dta$X_uuid =="39524416-5097-41fc-ba8f-05c7c686e3f0"] <- "F_1210"


## duplicate F_2012
dta$ID[dta$X_uuid =="6c1e8ea2-102a-45b3-b414-fd092ce99fca"] <- "F_2231"
dta$paid_pack[dta$X_uuid =="6c1e8ea2-102a-45b3-b414-fd092ce99fca"] <- TRUE
dta$discounted[dta$X_uuid =="6c1e8ea2-102a-45b3-b414-fd092ce99fca"] <- FALSE


## duplicate F_749
dta$ID[dta$X_uuid =="c24c76c4-f07b-458f-bde7-1cd60e92f9de"] <- "F_741"

## duplicate F_894
dta$ID[dta$X_uuid =="43b2a31d-db2d-4bd8-a748-2c33c2303553"] <- "F_893"

## duplicate F_1060
dta$ID[dta$X_uuid =="c5b59188-9444-4496-87e4-b7813509be88"] <- "F_1157"
dta$cont[dta$X_uuid =="c5b59188-9444-4496-87e4-b7813509be88"] <- FALSE

## duplicate F_329
dta$ID[dta$X_uuid =="d67b552a-2ab1-47e9-b361-6e30cef6b0f7"] <- "F_321"

## duplicate F_1211
dta$ID[dta$X_uuid =="7708ed09-fec5-4084-b4a6-0068007b8b9a"] <- "F_1490"
dta$trial_P[dta$X_uuid =="7708ed09-fec5-4084-b4a6-0068007b8b9a"] <- FALSE
dta$discounted[dta$X_uuid =="7708ed09-fec5-4084-b4a6-0068007b8b9a"] <- TRUE
dta$cont[dta$X_uuid =="7708ed09-fec5-4084-b4a6-0068007b8b9a"] <- FALSE

## duplicate F_1260
dta$ID[dta$X_uuid =="6c37d4cc-1988-4de8-b4e1-ddb5d4cf9c96"] <- "F_2017"

## duplicate F_296
dta$ID[dta$X_uuid =="3cf8f444-fe23-482b-a991-aa8aef70d8bd"] <- "F_1843"
dta$trial_P[dta$X_uuid =="3cf8f444-fe23-482b-a991-aa8aef70d8bd"] <- TRUE
dta$discounted[dta$X_uuid =="3cf8f444-fe23-482b-a991-aa8aef70d8bd"] <- FALSE
dta$cont[dta$X_uuid =="3cf8f444-fe23-482b-a991-aa8aef70d8bd"] <- TRUE

dta$ID[dta$X_uuid =="71f367ce-5fa7-4ef1-ab6f-0a710ab419b7"] <- "F_112"

##duplicat F_91
dta$ID[dta$X_uuid =="1e88fe63-5596-4506-8c35-e4064b40031f"] <- "F_2102"
dta$trail_P[dta$X_uuid =="1e88fe63-5596-4506-8c35-e4064b40031f"] <- FALSE
## duplicate F_739

dta$ID[dta$X_uuid =="3551fbb5-5a64-40d5-a103-baf320fce80c"] <- "F_1813"
## duplicate F_1406

dta$ID[dta$X_uuid =="1cae2c9f-a124-40f4-a0e5-cb3381717e21"] <- "F_1747"
dta$discounted[dta$X_uuid =="1cae2c9f-a124-40f4-a0e5-cb3381717e21"] <- FALSE
dta$cont[dta$X_uuid =="1cae2c9f-a124-40f4-a0e5-cb3381717e21"] <- TRUE

dta$ID[duplicated(dta$ID)]

## get baseline to create list of farmers that still needs to be done
# bse <- read.csv("/home/bjvca/data/projects/OneCG/MIPP/baseline/data/raw/baseline_fixed_dups.csv")
# 
# write.csv(bse[!(bse$farmer_ID %in% dta$ID),c("district", "sub","village","farmer_ID", "Check2.check.maize.name_resp", "Check2.check.maize.nick", "Check2.check.maize.phone", "Check2.check.maize.phone2")], "remnants.csv", row.names = FALSE)
# bse_col <- bse[!(bse$farmer_ID %in% dta$ID),c("district", "sub","village","farmer_ID")]
# bse_col$count <- 1
# aggregate(bse_col$count,list(c(bse_col$village)), sum)
##these are the variables:
garden_vector <- c("crop_inter", 
                   "crop_type.1", 
                   "crop_type.2", 
                   "crop_type.3", 
                   "crop_type.4", 
                   "crop_type.5", 
                   "crop_type.6", 
                   "crop_type.7", 
                   "crop_type.8", 
                   "crop_type.96", 
                   "crop_perc", 
                   "who1", 
                   "long_var", 
                   "who2", 
                   "seed_qty", 
                   "seed_cst", 
                   "org_ap", 
                   "dap_ap", 
                   "ur_ap", 
                   "pest_ap", 
                   "bag_harv", 
                   "bag_kg", 
                   "harv_kgs", 
                   "yield", 
                   "seed_use_again", 
                   "used_yield_rate", 
                   "used_drt_tol", 
                   "used_dies_tol", 
                   "used_erly_mat", 
                   "used_mrkt_dem", 
                   "used_cons_taste", 
                   "used_biomass", 
                   "used_germ_rate", 
                   "used_portions", 
                   "used_cons_appear", 
                   "used_process", 
                   "used_cons_cook", 
                   "used_happy", 
                   "maize_sell", 
                   "bag_sell", 
                   "bag_charge", 
                   "bag_keep", 
                   "seed_keep")

dta[dta$ID  %in% dta_first$ID, garden_vector] <- dta_first[dta_first$ID %in% dta_first$ID,garden_vector]

to_drop <- c("start", "end", "deviceid", "simserial", "phonenumber", "subscriberid", "enumerator", "district", "sub", "village", "farmer_name", 
             "phone1", "phone2", "nick", "lat", "long", "plot.1..plot_name", "plot.2..plot_name", "plot.3..plot_name", "plot.4..plot_name", "plot_select_name",
              "location", "_location_latitude", "_location_longitude", "_location_altitude", "_location_precision", "meta.instanceID", 
             "X_id", "X_uuid", "X_submission_time", "X_date_modified", "X_tags", "X_notes", "X_version", "X_duration", "X_submitted_by", 
             "X_total_media", "X_media_count", "X_media_all_received", "X_xform_id")

dta <- dta[ , !(names(dta) %in% to_drop)]

## [1] "F_1785" "F_584"  "F_553"  "F_810"  "F_1150" "F_573"  "F_827"  "F_830"  "F_824"  "F_2241" "F_855"  "F_879"  "F_857"  "F_592"  "F_1010" "F_2255" "F_1650" "F_1839" "F_1941" "F_1950"
## [21] "F_663"  "F_2012" "F_1874" "F_1939" "F_1048" "F_1179"
dta$plot_select[dta$maize_var_selected == "n/a"] <- NA

# List of variables to be assigned NA based on the condition
vars_to_update <- c("size_selected", "order1", "crop_inter", "crop_type.1", "crop_type.2", 
                    "crop_type.3", "crop_type.4", "crop_type.5", "crop_type.6", "crop_type.7", 
                    "crop_type.8", "crop_type.96", "crop_perc", "who1", "long_var", "who2", 
                    "seed_qty", "seed_cst", "org_ap", "dap_ap", "ur_ap", "pest_ap", "bag_harv",
                    "bag_kg", "harv_kgs", "yield", "seed_use_again", "used_yield_rate", 
                    "used_drt_tol", "used_dies_tol", "used_erly_mat", "used_mrkt_dem", 
                    "used_cons_taste", "used_biomass", "used_germ_rate", "used_portions", 
                    "used_cons_appear", "used_process", "used_cons_cook", "used_happy", 
                    "maize_sell", "bag_sell", "bag_charge", "bag_keep", "seed_keep")

# Check if maize_var_selected is "n/a" and assign NA to selected variables
for (var in vars_to_update) {
  dta[[var]][dta$maize_var_selected == "n/a"] <- NA
}



write.csv(dta,file=paste(path,"public/endline.csv",sep="/"), row.names=FALSE)
