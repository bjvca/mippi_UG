path <- getwd()
dta <- read.csv("latest.csv")
path <- strsplit(path,"raw")[[1]]

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
library(dplyr)
base <- read.csv("/home/bjvca/data/projects/OneCG/MIPP/baseline/data/raw/baseline_fixed_dups.csv")
### keep only clusters allocated to consumption intervention
base <- base[c("district","sub",  "village", "trial_P")]

base <- aggregate(base$trial_P,list(base$district,base$sub,base$village), mean)

dta <- merge(dta, base, by.x=c("cooking.district", "cooking.sub", "cooking.village"), by.y=c("Group.1","Group.2","Group.3" ))
sum(dta$before_cook.taste1.taste1_impr[dta$x==1])/sum(dta$total_votes_before[dta$x==1])*100
sum(dta$before_cook.taste1.taste1_impr[dta$x==0])/sum(dta$total_votes_before[dta$x==0])*100

dta <- dta[,-c(12:914)]
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

write.csv(dta,file=paste(path,"public/cons_intervention.csv",sep="/"), row.names=FALSE)
