
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
dta$ID[dta$X_uuid =="63c1f5f7-a882-4937-93a9-22fbdab6d02a"] <- "F_685"
dta$ID[dta$X_uuid =="b08a64a6-7e3d-4ca7-bcb0-b6ebd8b927a7"] <- "F_1781"
dta$discounted[dta$X_uuid =="b08a64a6-7e3d-4ca7-bcb0-b6ebd8b927a7"] <- TRUE
dta$paid_pack[dta$X_uuid =="b08a64a6-7e3d-4ca7-bcb0-b6ebd8b927a7"] <- FALSE
dta$trial_P[dta$X_uuid =="b08a64a6-7e3d-4ca7-bcb0-b6ebd8b927a7"] <- FALSE
dta$cont[dta$X_uuid =="b08a64a6-7e3d-4ca7-bcb0-b6ebd8b927a7"] <- FALSE

dta$ID[dta$X_uuid =="55092e2d-a208-4c45-8ecb-0583abb5bad3"] <- "F_1838"
dta$ID[dta$X_uuid =="c525024a-58cc-4e89-b225-43ccd9e5c63b"] <- "F_1202"
dta$discounted[dta$X_uuid =="c525024a-58cc-4e89-b225-43ccd9e5c63b"] <- TRUE
dta$cont[dta$X_uuid =="c525024a-58cc-4e89-b225-43ccd9e5c63b"] <- FALSE

dta$ID[dta$X_uuid =="7cc2e6f0-bac9-4dbc-9de1-efd56858961e"] <- "F_2269"
dta$paid_pack[dta$X_uuid =="7cc2e6f0-bac9-4dbc-9de1-efd56858961e"] <- TRUE
dta$discounted[dta$X_uuid =="7cc2e6f0-bac9-4dbc-9de1-efd56858961e"] <- FALSE

dta$ID[dta$X_uuid =="d38614eb-e7c9-4b8b-8e7e-5b418c1bfab1"] <- "F_2098"
dta$paid_pack[dta$X_uuid =="d38614eb-e7c9-4b8b-8e7e-5b418c1bfab1"] <- TRUE
dta$trial_P[dta$X_uuid =="d38614eb-e7c9-4b8b-8e7e-5b418c1bfab1"] <- FALSE

dta$ID[dta$X_uuid =="fc9f46e5-b62f-438e-b0cc-3dbf5fdb17cf"] <- "F_2208"
dta$paid_pack[dta$X_uuid =="fc9f46e5-b62f-438e-b0cc-3dbf5fdb17cf"] <- TRUE
dta$discounted[dta$X_uuid =="fc9f46e5-b62f-438e-b0cc-3dbf5fdb17cf"] <- FALSE

dta$ID[dta$X_uuid =="1d28b574-2ace-4cd3-bc3b-8d7bcfd96546"] <- "F_1418"
dta$discounted[dta$X_uuid =="1d28b574-2ace-4cd3-bc3b-8d7bcfd96546"] <- FALSE

dta$ID[duplicated(dta$ID)]
dta[dta$ID=="F_1144",]

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

dta[dta$ID== "F_830",]

write.csv(dta,file=paste(path,"public/endline.csv",sep="/"), row.names=FALSE)
