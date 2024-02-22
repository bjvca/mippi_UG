
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

### some issues with people scrolling back
dta$plot_select[dta$ID %in% dta$ID[dta$plot_no<dta$plot_select]] <- "n/a"

write.csv(dta,file=paste(path,"public/endline.csv",sep="/"), row.names=FALSE)
