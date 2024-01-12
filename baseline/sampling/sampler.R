census <- read.csv("/home/bjvca/data/data/UGANDA/SAMPLING_FRAME_2012.csv")
census <- census[c("DistrictName"  , "SubCountyName", "ParishName","VillageName","NoOfHouseholds" )]
census <- data.frame(subset(census, DistrictName == "KAMULI" | DistrictName == "MAYUGE" | DistrictName == "IGANGA" | DistrictName == "BUGIRI"))
set.seed(12345)

table(census$DistrictName)

### collapse to village level
villagelevel <- aggregate(census$NoOfHouseholds,list(census$DistrictName,census$ParishName,census$SubCountyName,census$VillageName),FUN=sum,na.rm=T)
names(villagelevel) <- c("DistrictName"  ,"ParishName", "SubCountyName", "VillageName","NoOfHouseholds" ) 


sclevel <- aggregate(villagelevel$NoOfHouseholds,list(villagelevel$DistrictName,villagelevel$SubCountyName),FUN=sum,na.rm=T)
names(sclevel) <- c("DistrictName"  , "SubCountyName","NoOfHouseholds" ) 
sclevel$one <- 1

districtlevel <- aggregate(sclevel$NoOfHouseholds,list(sclevel$DistrictName),FUN=sum,na.rm=T)

round(districtlevel$x/sum(districtlevel$x)*10)
## select 2 in Kiboga, 2 in Kyankwanzi, 3 in Masindi and 3 in Nakaseke
#Kiboga - Lwamata and Kubomero
#Kyankwanzi - Nsambya and Wattuba
#Masindi - Central Division, Kigulya and Miirya
#Nakaseke - Wakyato, Kinyogoga and Kinoni



villagelevel <- subset(villagelevel,villagelevel$NoOfHouseholds >25)

sample_village <- villagelevel[sample(seq_len(nrow(villagelevel)), 300, prob=villagelevel$NoOfHouseholds,replace = FALSE),]

sample_village <- sample_village[,1:4] 




names(sample_village) <-  c("District","Parish","Sub_County","Village")
sample_village <- sample_village[c("District","Sub_County","Parish","Village")] 
sample_village <- sample_village[with(sample_village, order(sample_village$District, sample_village$Sub_County,  sample_village$Parish,sample_village$Village)),]



write.csv(sample_village,file="/home/bjvca/data/projects/OneCG/MIPP/baseline/sampling/sample.csv",row.names=FALSE)

### edit this to remove town councils etc
sample_village <- read.csv(file="/home/bjvca/data/projects/OneCG/MIPP/baseline/sampling/sample_final.csv")

### generate treatment cells - village level
sample_village <- cbind(sample_village,sample(rep(c(1:6), length=232)))
names(sample_village)[5] <- "treatment_cell" 
### generate random prices to start bargaining from at individual level
sample_village$cons <- FALSE
sample_village$trial_pack <- FALSE
sample_village$paid_pack <- FALSE
sample_village$paid_pack_discount <- FALSE

sample_village$cons[sample_village$treatment_cell %in% c(2,4)] <- TRUE
sample_village$trial_pack[sample_village$treatment_cell %in% c(3,4)] <- TRUE
sample_village$paid_pack[sample_village$treatment_cell==5] <- TRUE
sample_village$paid_pack_discount[sample_village$treatment_cell==6] <- TRUE
# 10 obs in each village
sample_individual <- sample_village[rep(seq_len(nrow(sample_village)), each = 10), ]

prices <- sample(c(9000,9000,9000,10000,10000,11000,11000,12000,12000,12000))
for (i in 1:(sum(sample_village$paid_pack_discount==TRUE)-1)) {
prices <-c(prices,sample(c(9000,9000,9000,10000,10000,11000,11000,12000,12000,12000)))
}
sample_individual$start_price <- NA
sample_individual$start_price[sample_individual$paid_pack==TRUE | sample_individual$paid_pack_discount==TRUE] <- prices

### FARMER IDS
sample_individual$farmer_id <- paste("F",1:dim(sample_individual)[1],sep="_")

sample_individual <- sample_individual[c("farmer_id","District","Sub_County",	"Parish",	"Village",		"cons",	"trial_pack",	"paid_pack",	"paid_pack_discount",	"start_price")]

sample_individual$District<- trimws(sample_individual$District)
sample_individual$Sub_County<- trimws(sample_individual$Sub_County)
sample_individual$Parish<- trimws(sample_individual$Parish)
sample_individual$Village<- trimws(sample_individual$Village)

dta$age[dta$age == 999] <- NA

t.test(as.numeric(gender=="Male")~enumerator_gender,data=dta)
t.test(age~enumerator_gender,data=dta)


write.csv(sample_individual,file="/home/bjvca/data/projects/OneCG/MIPP/baseline/sampling/sample_individual.csv",row.names=FALSE)
write.csv(sample_village,file="/home/bjvca/data/projects/OneCG/MIPP/baseline/sampling/sample_village.csv",row.names=FALSE)

