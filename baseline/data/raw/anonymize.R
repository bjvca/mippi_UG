path <- getwd()
dta <- read.csv(paste(path,"latest.csv", sep="/"))
path <- strsplit(path, "/raw")[[1]]

## we have 40 duplicates
sum(duplicated(dta$farmer_ID))

### enumerator gender:
# create named vector of enumerator genders
enumerator_genders <- c(Arnold = "Male", 
                        Badru = "Male", 
                       'Businge Penny' = "Female",
                        Buyera = "Male",
                        Humphrey = "Male",
                        Ibanda = "Male",
                        Kabali = "Female",
                        Kalule = "Male",
                        Katumba = "Male",
                        Khaukha = "Male",
                        Kibaale = "Female",
                        Komako = "Male",
                        Lukabya = "Male",
                        Mpalanyi = "Female",
                        Mulabiza = "Female",
                        Muwata = "Male",
                        Nakirya = "Female",
                        Nambi = "Female",
                        Nambozo = "Female",
                        Nandhego = "Female",
                        Nemwa = "Female",
                        Oboth = "Male",
                        Segujja = "Male",
                        Sendaula = "Male",
                        Wasike = "Male",
                        Yiga = "Male")

# create new variable in your data frame using the named vector
dta$enumerator_gender <- enumerator_genders[dta$enumerator]

## create IDs for district, subcounty, village

### create IDs for district, TA and village (district, sub, village)


dta$distID <- NULL
dta$subID <- NULL
dta$vilID <- NULL

i_dist <- 1
for (dist in names(table(dta$district))) {
  print(dist)
  i_sub <- 1
  for (sub in names(table(dta$sub[dta$district==dist]))) {
    print(sub)
    i_village <- 1
    for (village in names(table(dta$village[dta$district == dist & dta$sub == sub]))) {
      print(village)
      dta$vilID[dta$district == dist & dta$sub == sub & dta$village == village] <- i_village
      i_village <- i_village + 1
    }
    dta$subID[dta$district == dist & dta$sub == sub] <- i_sub
    i_sub <- i_sub + 1
  }
  dta$distID[dta$district==dist] <- i_dist
  i_dist <- i_dist + 1
}

dta$distID <- as.numeric(dta$distID)
dta$subID <- as.numeric(dta$subID)
dta$vilID <- as.numeric(dta$vilID)



## drop location and metadata
to_drop <- c("Check2.check.maize.pic",
                                     "Check2.check.maize.pic2",
                                     "Check2.check.maize.pic3",
                                     "Check2.check.maize.gps",
                                     "Check2.check.maize._gps_latitude",
                                     "Check2.check.maize._gps_longitude",
                                     "Check2.check.maize._gps_altitude",
                                     "Check2.check.maize._gps_precision",
                                     "meta.instanceID",
                                     "X_id",
                                     "X_uuid",
                                     "X_submission_time",
                                     "X_date_modified",
                                     "X_tags",
                                     "X_notes",
                                     "X_version",
                                     "X_duration",
                                     "X_submitted_by",
                                     "X_total_media",
                                     "X_media_count",
                                     "X_media_all_received",
                                     "X_xform_id")           
dta <- dta[ , !(names(dta) %in% to_drop)]


## drop location, names and contact details
to_drop <- c("start","end","deviceid","simserial","phonenumber", "subscriberid", "enumerator","district","sub","village",  "Check2.check.maize.plot_no",
             "Check2.check.maize.plot_count",
             "Check2.check.maize.plot.1..plot_num",
             "Check2.check.maize.plot.1..plot_name",
             "Check2.check.maize.plot.2..plot_num",
             "Check2.check.maize.plot.2..plot_name",
             "Check2.check.maize.plot.3..plot_num",
             "Check2.check.maize.plot.3..plot_name",
             "Check2.check.maize.plot.4..plot_num",
             "Check2.check.maize.plot.4..plot_name",
             "Check2.check.maize.plot.5..plot_num",
             "Check2.check.maize.plot.5..plot_name",
             "Check2.check.maize.plot_calc1",
             "Check2.check.maize.plot_calc2",
             "Check2.check.maize.plot_select",
             "Check2.check.maize.plot_select_name", "Check2.check.maize.order1","Check2.check.maize.phone", "Check2.check.maize.phone2", "Check2.check.maize.name_resp", "Check2.check.maize.nick" )     
             dta <- dta[ , !(names(dta) %in% to_drop)]
             

             names(dta) <- sub("Check2.check.maize.", "",names(dta))



write.csv(dta, file = paste(path,"public/baseline.csv", sep="/"), row.names = FALSE)
