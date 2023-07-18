path <- getwd()
dta <- read.csv("latest.csv")
path <- strsplit(path,"raw")[[1]]

### remove identifiers
to_drop <- c("start","end","deviceid","simserial","phonenumber","subscriberid","enumerator", "farmer_name", "phone1",	"phone2")
dta <- dta[ , !(names(dta) %in% to_drop)]    

## create IDs for district, subcounty and village (q1, q2, q3)

i_dist <- 1
dta$dist_ID <- NULL
dta$sub_ID <- NULL
dta$vil_ID <- NULL


### we should just drop these and use the IDs from baseline (merge with farmer_IDs)
for (dist in names(table(dta$district))) {
  print(dist)
  i_sub <- 1
  for (sub in names(table(dta$sub[dta$district==dist]))) {
    print(sub)
    i_village <- 1
    for (village in names(table(dta$village[dta$district == dist & dta$sub == sub]))) {
      print(village)
      dta$vil_ID[dta$district == dist & dta$sub == sub & dta$village == village] <- i_village
      i_village <- i_village + 1
    }
    dta$sub_ID[dta$district == dist & dta$sub == sub] <- i_sub
    i_sub <- i_sub + 1
  }
  dta$dist_ID[dta$district==dist] <- i_dist
  i_dist <- i_dist + 1
}

dta$dist_ID <- as.numeric(dta$dist_ID)
dta$sub_ID <- as.numeric(dta$sub_ID)
dta$vil_ID <- as.numeric(dta$vil_ID)

to_drop <- c("district","sub","village")
dta <- dta[ , !(names(dta) %in% to_drop)]

##issue with farmer with same name
dta$ID[dta$X_uuid=="f22a726f-8370-4168-8656-4885339a62af"] <- "F_1330"
dta$trial_P[dta$X_uuid=="f22a726f-8370-4168-8656-4885339a62af"] <- TRUE
dta$discounted[dta$X_uuid=="f22a726f-8370-4168-8656-4885339a62af"] <- FALSE

###remove metadata
to_drop <- c("confirmation.Spo_name","confirmation.check3.gps", "confirmation.check3._gps_latitude", "confirmation.check3._gps_longitude", "confirmation.check3._gps_altitude",              
 "confirmation.check3._gps_precision","meta.instanceID","X_id","X_submission_time","X_tags","X_version","X_duration",
 "X_submitted_by","X_total_media","X_media_count","X_media_all_received","X_xform_id", "X_uuid","X_date_modified", "X_notes")
dta <- dta[ , !(names(dta) %in% to_drop)]


names(dta) <- sub("confirmation.check3.maize.", "",names(dta))
names(dta) <- sub("confirmation.check3.", "",names(dta))
names(dta) <- sub("confirmation.", "",names(dta))

write.csv(dta,file=paste(path,"public/midline.csv",sep="/"), row.names=FALSE)