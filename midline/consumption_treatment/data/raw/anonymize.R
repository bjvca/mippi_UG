### this file prepares raw data collected during consumer intervention for public release
path <- getwd()
dta <- read.csv("latest.csv")
path <- strsplit(path,"raw")[[1]]

### just some data analysis:
## taste before cooking
dta$before_cook.taste1.taste1_loc 
dta$before_cook.taste1.taste1_impr
dta$before_cook.taste1.same_tast1
dta$total_votes_before <- dta$before_cook.taste1.taste1_loc +dta$before_cook.taste1.taste1_impr+ dta$before_cook.taste1.same_tast1
sum(dta$before_cook.taste1.taste1_impr)/sum(dta$total_votes_before)*100
sum(dta$before_cook.taste1.taste1_loc)/sum(dta$total_votes_before)*100

## taste after cooking
dta$after_cooking.taste2.taste2_loc
dta$after_cooking.taste2.taste2_impr
dta$after_cooking.taste2.same_tast2
dta$total_votes_after <- dta$after_cooking.taste2.taste2_loc + dta$after_cooking.taste2.taste2_impr + dta$after_cooking.taste2.same_tast2
sum(dta$after_cooking.taste2.taste2_impr)/sum(dta$total_votes_after)*100
sum(dta$after_cooking.taste2.taste2_loc)/sum(dta$total_votes_after)*100

###merge in trial pack treatment to see if farmers who got free seed also think it tastes better
##script that produces input for ODK to be used during consumer intervention
## read raw data from baseline

base <- read.csv("/home/bjvca/data/projects/OneCG/MIPP/baseline/data/raw/baseline_fixed_dups.csv")
### keep only clusters allocated to consumption intervention
base <- base[c("district","sub",  "village", "cont")]

base <- aggregate(base$cont,list(base$district,base$sub,base$village), mean)

dta <- merge(dta, base, by.x=c("cooking.district", "cooking.sub", "cooking.village"), by.y=c("Group.1","Group.2","Group.3" ))
sum(dta$before_cook.taste1.taste1_impr[dta$x==1])/sum(dta$total_votes_before[dta$x==1])*100
sum(dta$before_cook.taste1.taste1_impr[dta$x==0])/sum(dta$total_votes_before[dta$x==0])*100
#merge in gender of main respondent

base <- read.csv("/home/bjvca/data/projects/OneCG/MIPP/baseline/data/raw/baseline_fixed_dups.csv")
## this has 9 duplicate names...
sum(duplicated(base$Check2.check.maize.name_resp))

##determine gender of respondent
base$resp_gender <- NA
base$resp_gender[base$Check2.q1 == "Yes"] <- base$Check2.check.maize.gender[base$Check2.q1 == "Yes"]
base$resp_gender[base$Check2.q1 == "No"] <- base$Check2.check.maize.resp_gender[base$Check2.q1 == "No"]

dta <- merge(dta,base[c("Check2.check.maize.name_resp","village","resp_gender")], by.x=c("grDetails.1..c2","cooking.village"), by.y=c("Check2.check.maize.name_resp","village"), all.x=TRUE)
names(dta)[length(dta)] <- "grDetails.1..respondent_gender"
dta <- merge(dta,base[c("Check2.check.maize.name_resp","village","resp_gender")], by.x=c("grDetails.2..c2","cooking.village"), by.y=c("Check2.check.maize.name_resp","village"), all.x=TRUE)
names(dta)[length(dta)] <- "grDetails.2..respondent_gender"
dta <- merge(dta,base[c("Check2.check.maize.name_resp","village","resp_gender")], by.x=c("grDetails.3..c2","cooking.village"), by.y=c("Check2.check.maize.name_resp","village"), all.x=TRUE)
names(dta)[length(dta)] <- "grDetails.3..respondent_gender"
dta <- merge(dta,base[c("Check2.check.maize.name_resp","village","resp_gender")], by.x=c("grDetails.4..c2","cooking.village"), by.y=c("Check2.check.maize.name_resp","village"), all.x=TRUE)
names(dta)[length(dta)] <- "grDetails.4..respondent_gender"
dta <- merge(dta,base[c("Check2.check.maize.name_resp","village","resp_gender")], by.x=c("grDetails.5..c2","cooking.village"), by.y=c("Check2.check.maize.name_resp","village"), all.x=TRUE)
names(dta)[length(dta)] <- "grDetails.5..respondent_gender"
dta <- merge(dta,base[c("Check2.check.maize.name_resp","village","resp_gender")], by.x=c("grDetails.6..c2","cooking.village"), by.y=c("Check2.check.maize.name_resp","village"), all.x=TRUE)
names(dta)[length(dta)] <- "grDetails.6..respondent_gender"
dta <- merge(dta,base[c("Check2.check.maize.name_resp","village","resp_gender")], by.x=c("grDetails.7..c2","cooking.village"), by.y=c("Check2.check.maize.name_resp","village"), all.x=TRUE)
names(dta)[length(dta)] <- "grDetails.7..respondent_gender"
dta <- merge(dta,base[c("Check2.check.maize.name_resp","village","resp_gender")], by.x=c("grDetails.8..c2","cooking.village"), by.y=c("Check2.check.maize.name_resp","village"), all.x=TRUE)
names(dta)[length(dta)] <- "grDetails.8..respondent_gender"
dta <- merge(dta,base[c("Check2.check.maize.name_resp","village","resp_gender")], by.x=c("grDetails.9..c2","cooking.village"), by.y=c("Check2.check.maize.name_resp","village"), all.x=TRUE)
names(dta)[length(dta)] <- "grDetails.9..respondent_gender"
dta <- merge(dta,base[c("Check2.check.maize.name_resp","village","resp_gender")], by.x=c("grDetails.10..c2","cooking.village"), by.y=c("Check2.check.maize.name_resp","village"), all.x=TRUE)
names(dta)[length(dta)] <- "grDetails.10..respondent_gender"

library(dplyr) 
dta <- dta %>% select(-contains("sp_name"))
dta <- dta %>% select(-contains("rep_name"))
dta <- dta %>% select(-contains("..c2"))

###

to_drop <- c("cooking.district"  ,                 "cooking.sub"       ,                 "cooking.village" ,
"start"           ,                   "end"   ,                            
 "deviceid"    ,                       "simserial"            ,              "phonenumber"       ,                
 "subscriberid"    ,                   "enumerator"     ,                
 "gps"            ,                                            
"X_gps_latitude"     ,                                        
"X_gps_longitude"   ,                                         
"X_gps_altitude"   ,                                          
"X_gps_precision"  ,                                          
"meta.instanceID"  ,                                          
"X_id"     ,                                                  
"X_uuid"    ,                                                 
"X_submission_time"    ,                                      
"X_date_modified"  ,                                          
"X_tags"         ,                                            
"X_notes"       ,                                             
"X_version"      ,                                            
 "X_duration"      ,                                           
 "X_submitted_by"    ,                                         
 "X_total_media"   ,                                           
 "X_media_count"  ,                                            
 "X_media_all_received"  ,                                     
 "X_xform_id"     ,                                            
 "total_votes_before"  ,                                       
 "total_votes_after","cooking.ID", "cooking.c1" )


dta <- dta[ , !(names(dta) %in% to_drop)]
dta <- dta[,-c(2:781)]

write.csv(dta,file=paste(path,"public/cons_intervention.csv",sep="/"), row.names=FALSE)
