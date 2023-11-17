path <- getwd()
dta <- read.csv(paste(path,"latest.csv", sep="/"))
path <- strsplit(path, "/raw")[[1]]

### something strange happended for farmer F_872: while this farmer accepted at first offer in the
### bargaining experiment, there are NaNs in the columns for the bargaining script, they should be NAs
dta[dta$farmer_ID=="F_872",c(25:85)] <- "n/a"


##solving duplicates
dta$farmer_ID[dta$X_uuid == "3d667012-96b2-4a4c-b265-626890f43e12"] <- "F_665"
dta$farmer_ID[dta$X_uuid == "9dbc7781-ecf0-4f01-8ec8-a87e914ee21c"] <- "F_586"
dta$farmer_ID[dta$X_uuid == "aade0bbf-dcf2-4078-9d8c-631fda8db492"] <- "F_697"
dta$farmer_ID[dta$X_uuid == "7dbddd19-113e-445e-bec0-99ed780b4499"] <- "F_892"
dta$farmer_ID[dta$X_uuid == "9d555179-cebb-4f1b-9759-a6f8341f4cdf"] <- "F_893"
dta$farmer_ID[dta$X_uuid == "7144a0d7-22ef-48f2-90d7-29ad1e971d31"] <- "F_894"
dta$farmer_ID[dta$X_uuid == "c7e1d688-0527-4bae-b255-d8fc37084ee4"] <- "F_895"
dta$farmer_ID[dta$X_uuid == "cbf4f4cd-401c-473c-8186-e157b7312029"] <- "F_496"
dta$farmer_ID[dta$X_uuid == "87fd8c31-66b6-4472-96c9-c07d9936c353"] <- "F_1171"
dta$farmer_ID[dta$X_uuid == "75b678a9-56db-4864-9f1d-87cad6131425"] <- "F_851"
dta$farmer_ID[dta$X_uuid == "6fdfe7b3-04a1-4dbf-b42d-b35c4ea3262c"] <- "F_106"
dta$farmer_ID[dta$X_uuid == "82e66843-a22d-4724-8857-f2de6bcd27d4"] <- "F_466"
dta$farmer_ID[dta$X_uuid == "15522bb7-fa4f-4b62-afab-9c4d93fcabaa"] <- "F_346"
dta$farmer_ID[dta$X_uuid == "b79f2bc4-70f5-4988-b630-10f40c201a48"] <- "F_115"
dta$farmer_ID[dta$X_uuid == "b6d0e6e9-34cb-431c-a819-3c977c91b733"] <- "F_244"
dta$farmer_ID[dta$X_uuid == "4f8e0180-5fe9-4bcc-832b-b2db3fe80568"] <- "F_296"
dta$farmer_ID[dta$X_uuid == "99f52267-8bb7-437f-8184-7f7d755eacd8"] <- "F_2041"
dta$farmer_ID[dta$X_uuid == "163e31ba-b789-4217-8064-e8eaf5ba0f35"] <- "F_2042"
dta$farmer_ID[dta$X_uuid == "7705d784-f504-4909-99cd-b9469dc56864"] <- "F_2043"
dta$farmer_ID[dta$X_uuid == "dc8bb098-10a5-4b89-a481-5adc7d971c45"] <- "F_2044"
dta$farmer_ID[dta$X_uuid == "128a805b-6002-4feb-892d-3b8579edbc9a"] <- "F_2045"
dta$farmer_ID[dta$X_uuid == "e112411f-0bae-42a5-91ff-376227ca81a0"] <- "F_2236"
dta$farmer_ID[dta$X_uuid == "3d18196b-00ea-475a-a5c7-5c09ce0fc683"] <- "F_2240"
dta$farmer_ID[dta$X_uuid == "1568d92c-c9ac-4d87-9d64-a71a1b2ad40f"] <- "F_2033"
dta$farmer_ID[dta$X_uuid == "9d1201b4-7142-460d-bbbd-e61c758e5684"] <- "F_416"
dta$farmer_ID[dta$X_uuid == "129a7691-5120-487a-a176-5b7db4516ea0"] <- "F_1612"
dta$farmer_ID[dta$X_uuid == "b393ee44-0a20-410c-b913-46508b03964b"] <- "F_1455"
dta$farmer_ID[dta$X_uuid == "562a2a94-ecdd-42d7-8b80-7ea6d794673c"] <- "F_1883"
dta$farmer_ID[dta$X_uuid == "954d2d63-2e1f-4b67-83ec-c2f178ce176a"] <- "F_1884"
dta$farmer_ID[dta$X_uuid == "3b95e2b0-689f-4c98-8cf1-401ebb88ac52"] <- "F_1911"
dta$farmer_ID[dta$X_uuid == "508a39a2-2350-4d17-ab3a-b32b58483e75"] <- "F_1766"

dta$farmer_ID[dta$X_uuid == "5e9fc957-b75a-4584-ab77-d698c6d47682"] <- "F_1251"
dta$farmer_ID[dta$X_uuid == "e4277d1a-ee6e-4c2c-8e3e-d170c6d3bf8d"] <- "F_1252"
dta$farmer_ID[dta$X_uuid == "14578aee-7fe1-4800-8be2-fd2e488a9a3d"] <- "F_1253"
dta$farmer_ID[dta$X_uuid == "daabcdca-d352-403c-a234-9106fec1e134"] <- "F_1254"
dta$farmer_ID[dta$X_uuid == "b4f49143-a029-4863-9e76-d6f5a14ecafc"] <- "F_1255"
dta$farmer_ID[dta$X_uuid == "1ed439a2-5952-461a-aa9b-0a3081fc41b6"] <- "F_1216"
dta$farmer_ID[dta$X_uuid == "e2b74aa2-f27b-4940-bda3-c55265c486e7"] <- "F_1470"
dta$farmer_ID[dta$X_uuid == "3dec7153-1c6a-41c5-83d4-7774075de126"] <- "F_2109"


### this farmer got the wrong treatment (was registered in wrong village)
dta <- subset(dta, X_uuid != "0dea9eae-444e-4b40-af9b-51618cdac700")
write.csv(dta, file = paste(path,"raw/baseline_fixed_dups.csv", sep="/"), row.names = FALSE)
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

write.csv(dta, file = paste(path,"raw/baseline_IDS.csv", sep="/"), row.names = FALSE)

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
