clear
clear matrix
set more off
capture log close

log using "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG\baseline\2slides_for_Berber\2slides_for_Berber.smcl", replace

import delimited "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG\baseline\data\public\baseline.csv", varnames(1)

count
sort farmer_id
drop if farmer_id == ""
count // 2320 total sample

********** GENDER OF THE HOUSEHOLD HEAD ***********
tab gender // 20% are female headed households.

gen sex = gender
replace sex = "1" if sex == "Female"
replace sex = "2" if sex == "Male"
label define sex 1 "Female" 2 "Male"
destring sex, replace
label values sex sex
label var sex "gender of hh head"
tab sex

**************% OF FARMERS USING IMPROVED SEED ************** 
// MIPPI baseline data, Q20. Did you use any quality maize seed like **OPV or hybrid seed** in the previous season (Nsambya of 2022) on any of your plots?
gen improved = quality_use
replace improved = "1" if improved =="Yes"
replace improved = "2" if improved =="No"
destring improved, replace
recode improved (1=100) (nonmissing=0)
tab improved
mean improved
mean improved, over (sex)

// which varieties: MIPPI baseline data, Q28. What maize variety did you plant in second season (Nsambya) of 2022 on this ${plot_select_name} plot?
gen varieties = maize_var
replace varieties = "1" if varieties == "Bazooka"
replace varieties = "1" if varieties == "KH_series"
replace varieties = "1" if varieties == "Longe_10H"
replace varieties = "1" if varieties == "Longe_6H"
replace varieties = "1" if varieties == "Longe_7H"
replace varieties = "1" if varieties == "Other_hybrid"
replace varieties = "1" if varieties == "Longe_7R_Kayongo-go"
replace varieties = "1" if varieties == "Panner"
replace varieties = "1" if varieties == "Wema"
replace varieties = "1" if varieties == "DK"
replace varieties = "2" if varieties == "Longe_4"
replace varieties = "2" if varieties == "Longe_5"
replace varieties = "3" if varieties == "Land_Races"
replace varieties = "4" if varieties == "98"

label define varieties 1 "Hybrids" 2 "OPV" 3 "Landraces" 4 "Don't know"
destring varieties, replace
label values varieties varieties
tab varieties // Most farmers use land races (52%), then hybrids (29%), then OPV (15%), the rest--- use other hybrid (5%)
tab varieties sex, col nofreq

// used bazooka: MIPPI baseline data, Q21. Did you use Bazooka in the second season of **2022 (Nsambya 2022)** on any of your plots? 
tab bazo_use
tab bazo_use sex, col nofreq

**** NUMBER OF YEARS THE FARMER HAS USED THE VARIETY ************
// MIPPI baseline data: Q28b. For how long in years have you been using ${maize_var}?

mean long_var // overall, farmers have used the varieties they reported for 30years
mean long_var, over (sex) 
 
mean long_var if varieties < 3
mean long_var if varieties < 3, over (sex)

mean long_var if varieties == 1 // hybrids only, not gender-disaggregated
mean long_var if varieties == 1, over (sex) // hybrids only, gender-disaggregated

mean long_var if varieties == 2 // OPVs only, not gender-disaggregated
mean long_var if varieties == 2, over (sex) // OPVs only, gender-disaggregated


// period of use for specific varietyplanted
gen variety = maize_var

replace variety = "1" if variety == "Bazooka"
replace variety = "2" if variety == "KH_series"
replace variety = "3" if variety == "Longe_10H"
replace variety = "4" if variety == "Longe_6H"
replace variety = "5" if variety == "Longe_7H"
replace variety = "6" if variety == "Other_hybrid"
replace variety = "7" if variety == "Longe_7R_Kayongo-go"
replace variety = "8" if variety == "Panner"
replace variety = "9" if variety == "Wema"
replace variety = "10" if variety == "DK"
replace variety = "11" if variety == "Longe_4"
replace variety = "12" if variety == "Longe_5"
replace variety = "13" if variety == "Land_Races"
destring variety, replace

label define variety 1 "Bazooka" 2 "KH_series" 3 "Longe_10H" 4 "Longe_6H" 5 "Longe_7H" 6 "Other_hybrid" 7 "Longe_7R_Kayongo-go" 8 "Panner" 9 "Wema" 10 "DK" 11 "Longe_4" 12 "Longe_5" 13 "Land_Races"

label values variety variety

mean long_var, over (variety) // period (years) that the farmer has used specific varieties
mean long_var if sex == 1, over (variety) // period (years) that the farmer has used specific varieties, female headed hhs
mean long_var if sex == 2, over (variety) // period (years) that the farmer has used specific varieties, male headed hhs

save "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG\baseline\2slides_for_Berber\2slides_for_Berber.dta", replace

**** WHY FARMERS ARE USING IMPROVED VARIETIES **********

// Source: 2019 maize survey data in the same study site. The mippi baseline did not have this question
// 2019 survey question: We now want to get an idea of the reasons why you used this particular bean seed on ${plot_select_name1} plot . Please rate, on a scale of 1 to 5, how important each dimension was for your decision to use this seed:

clear
import delimited "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Dairy_Ug\USAID_SME_project\Country folders\Uganda maize\data\public\wave_0\farmers.csv", varnames(1) 

count
sort id
ed
drop if date == ""


// rating the importance (1 to 5) of the reasons for using the a given variety on a given plot 

// First, varieties planted on the selected plot in 2018
replace hhmaizemaizenq48 = "1" if hhmaizemaizenq48 == "a"
replace hhmaizemaizenq48 = "2" if hhmaizemaizenq48 == "b"
replace hhmaizemaizenq48 = "3" if hhmaizemaizenq48 == "c"
replace hhmaizemaizenq48 = "4" if hhmaizemaizenq48 == "d"
replace hhmaizemaizenq48 = "5" if hhmaizemaizenq48 == "e"
replace hhmaizemaizenq48 = "6" if hhmaizemaizenq48 == "f"
replace hhmaizemaizenq48 = "7" if hhmaizemaizenq48 == "g"
replace hhmaizemaizenq48 = "8" if hhmaizemaizenq48 == "h"
replace hhmaizemaizenq48 = "9" if hhmaizemaizenq48 == "i"
replace hhmaizemaizenq48 = "10" if hhmaizemaizenq48 == "j"
replace hhmaizemaizenq48 = "11" if hhmaizemaizenq48 == "k"
replace hhmaizemaizenq48 = "12" if hhmaizemaizenq48 == "l"

destring hhmaizemaizenq48, replace

gen variety = hhmaizemaizenq48
label define variety 1 "Longe 10H" 2 "Longe 7H" 3 "Longe 7R/Kayongo-go" 4 "Bazooka" 5 "Longe 6H" 6 "Longe 5 (nalongo)" 7 "Longe 4" 8 "Panner" 9 "Wema" 10 "KH series" 11 "Land Races/local varieties" 12 "Others" 98 "Don't know"
label values variety variety
// variety planted on the selected plot
tab variety

// grouping the varieties used on a randomly selected plot in 2019 into hybrids, OPV and landraces
gen varietyCAT = variety
recode varietyCAT (1=1) (2=1) (3=1) (4=1) (5=1) (8=1) (9=1) (10=1) (11=3) (98=98) (12=12) (nonmissing=2) 
tab1 variety varietyCAT

label define varietyCAT 1 "Hybrids" 2 "OPVs" 3 "Landraces" 12 "Others" 98 "Don't know"
label values varietyCAT varietyCAT
tab varietyCAT

// rating the importance (1 to 5) of the reasons for using the a given variety on a given plot 
rename hhmaizemaizenq51a highyield_m
rename hhmaizemaizenq51b drought_m
rename hhmaizemaizenq51c pestdisease_m
rename hhmaizemaizenq51d earlymaturing_m
rename hhmaizemaizenq51e highMktPx_m
rename hhmaizemaizenq51f goodtaste_m
rename hhmaizemaizenq51g lowprice_m
rename hhmaizemaizenq51h availability_m
rename hhmaizemaizenq51i germination_m
rename hhmaizemaizenq51j experience_m

label var highyield_m "High yield"
label var drought_m "Drought tolerant"
lab var pestdisease_m "Pest/disease tolerant"
lab var earlymaturing_m "Early maturing"
lab var highMktPx_m "High market price"
lab var highMktPx_m "Higher market price/demand"
lab var goodtaste_m "Good taste or high nutrition"
lab var lowprice_m "Low price for seed"
lab var availability_m "Availability"
lab var germination_m "Germination rate"
lab var experience_m "Experience(knows and trust the quality"


// 2019 mean ratings of the reasons for using a given variety
sum highyield_m- experience_m // overall sample

// only improved varieties: 2019 mean ratings of the reasons for using a given variety
sum highyield_m- experience_m if varietyCAT < 3 // improved varieties (hybrids + OPVs)
sum highyield_m- experience_m if varietyCAT < 3 & hhmaizeq25 == "Female"
sum highyield_m- experience_m if varietyCAT < 3 & hhmaizeq25 == "Male"


// per variety category: 2019 mean ratings of the reasons for using a given variety 
sum highyield_m- experience_m if varietyCAT == 1 // Hybrids
sum highyield_m- experience_m if varietyCAT == 1 & hhmaizeq25 == "Female"
sum highyield_m- experience_m if varietyCAT == 1 & hhmaizeq25 == "Male"

sum highyield_m- experience_m if varietyCAT == 2 // OPVs
sum highyield_m- experience_m if varietyCAT == 2 & hhmaizeq25 == "Female"
sum highyield_m- experience_m if varietyCAT == 2 & hhmaizeq25 == "Male"

sum highyield_m- experience_m if varietyCAT == 3 // Landraces
sum highyield_m- experience_m if varietyCAT == 3 & hhmaizeq25 == "Female"
sum highyield_m- experience_m if varietyCAT == 3 & hhmaizeq25 == "Male"

**** RECYCLING OF IMPROVED SEED ********
// Source: MIPPI baseline data, Q30. How often was this ${maize_var}  that was used on **${plot_select_name}** in the second season (Nsambya) of 2022 recycled?

use "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG\baseline\2slides_for_Berber\2slides_for_Berber.dta", clear

// % of farmers who recycle more than the recommended times (0 times for hybrids and 3 times for OPVs)
gen oftenx = often
replace oftenx = "1" if oftenx == "a"
replace oftenx = "2" if oftenx == "b"
replace oftenx = "3" if oftenx == "c"
replace oftenx = "4" if oftenx == "d"
replace oftenx = "5" if oftenx == "e"
replace oftenx = "6" if oftenx == "f"

destring oftenx, replace ignore ("n/a")

gen recycle = oftenx
replace recycle = 100 if oftenx > 1 & varieties == 1 // for hybrids
replace recycle = 100 if oftenx > 4 & varieties == 2 // for OPVs
recode recycle (100=100) (nonmissing=0)
tab recycle if varieties < 3

mean recycle if varieties < 3 // 92 of farmers recycle improved seed more than the recommended times (0 times for hybrids and 3 times for OPVs)
mean recycle if varieties < 3, over (sex) // 87% female and 92% male farmers recycle improved seed more than the recommended times (0 times for hybrids and 3 times for OPVs)


mean recycle if varieties == 1 // 99% of farmers recycle seed from hybrid varieties when it is not recommended.
mean recycle if varieties == 1, over (sex) // 99% of both female farmers recycle hybrids seed varieties when it is not recommended. 

mean recycle if varieties == 2 // 92 of farmers recycle hybrids seed more than the recommended times
mean recycle if varieties == 2, over (sex) // 87% female and 92% male farmers recycle hybrids seed more than the recommended times 


**** TRAITS OF BAZOOKA VARIETY BASED ON FARMERS ASSESSMENT ******
// Source: MIPPI baseline data: Please rate bazooka, on these dimensions (q58 to q67)
destring group4gen_qlty1 - group4germ_rate1, replace ignore ("n/a")

gen bazo_genQlty = group4gen_qlty1
gen bazo_yield = group4yield_rate1
gen bazo_drt = group4drt_tol1
gen bazo_dies = group4dies_tol1
gen bazo_early = group4erly_mat1
gen bazo_mkt = group4mrkt_dem1
gen bazo_taste = group4taste1
gen bazo_price = group4price_rate1
gen bazo_avail = group4avail1
gen bazo_germ = group4germ_rate1

replace bazo_genQlty = gen_qlty if bazo_genQlty == . & variety == 1
replace bazo_yield = yield_rate if bazo_yield == . & variety == 1
replace bazo_drt = drt_tol if bazo_drt == . & variety == 1
replace bazo_dies = dies_tol if bazo_dies == . & variety == 1
replace bazo_early = erly_mat if bazo_early == . & variety == 1
replace bazo_mkt = mrkt_dem if bazo_mkt == . & variety == 1
replace bazo_taste = taste if bazo_taste == . & variety == 1
replace bazo_price = price_rate if bazo_price == . & variety == 1
replace bazo_avail = avail if bazo_avail == . & variety == 1
replace bazo_germ = germ_rate if bazo_germ == . & variety == 1

label define bazo_r 6 "Poorly_rated" 7 "Ok/averagely_rated" 8 "Highly_rated" 98 "Don't_know"
recode bazo_genQlty bazo_yield bazo_drt bazo_dies bazo_early bazo_mkt bazo_taste bazo_avail bazo_germ (1/2=6) (3=7) (4/5=8) (98=98)
recode bazo_price (4/5=6) (3=7) (1/2=6) (98=98)
label values bazo_genQlty bazo_yield bazo_drt bazo_dies bazo_early bazo_mkt bazo_taste bazo_avail bazo_germ bazo_price bazo_r
tab1 bazo_genQlty bazo_yield bazo_drt bazo_dies bazo_early bazo_mkt bazo_taste bazo_avail bazo_germ bazo_price

******** FARMER AWARENESS AND ADOPTION OF THE BAZOOKA VARIETY
// Data source: 2021 seed systems baseline data 

import delimited "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\Seed_systems_project_forked\baseline\data\farmer\public\baseline_farmers.csv", varnames(1) clear

replace check2checkmaizeq26longe_10h = "1" if check2checkmaizeq26longe_10h == "True"
replace check2checkmaizeq26longe_7h = "1" if check2checkmaizeq26longe_7h == "True"
replace check2checkmaizeq26longe_7r_kayo = "1" if check2checkmaizeq26longe_7r_kayo =="True"
replace check2checkmaizeq26bazooka = "1" if check2checkmaizeq26bazooka =="True"
replace check2checkmaizeq26longe_6h = "1" if check2checkmaizeq26longe_6h =="True"
replace check2checkmaizeq26longe_5 = "1" if check2checkmaizeq26longe_5 =="True"
replace check2checkmaizeq26longe_4 = "1" if check2checkmaizeq26longe_4 =="True"
replace check2checkmaizeq26panner = "1" if check2checkmaizeq26panner =="True"
replace check2checkmaizeq26wema = "1" if check2checkmaizeq26wema =="True"
replace check2checkmaizeq26kh_series = "1" if check2checkmaizeq26kh_series =="True"
replace check2checkmaizeq26land_races = "1" if check2checkmaizeq26land_races =="True"
replace check2checkmaizeq26other_hybrid = "1" if check2checkmaizeq26other_hybrid =="True"

replace check2checkmaizeq26longe_10h = "2" if check2checkmaizeq26longe_10h == "False"
replace check2checkmaizeq26longe_7h = "2" if check2checkmaizeq26longe_7h == "False"
replace check2checkmaizeq26longe_7r_kayo = "2" if check2checkmaizeq26longe_7r_kayo =="False"
replace check2checkmaizeq26bazooka = "2" if check2checkmaizeq26bazooka =="False"
replace check2checkmaizeq26longe_6h = "2" if check2checkmaizeq26longe_6h =="False"
replace check2checkmaizeq26longe_5 = "2" if check2checkmaizeq26longe_5 =="False"
replace check2checkmaizeq26longe_4 = "2" if check2checkmaizeq26longe_4 =="False"
replace check2checkmaizeq26panner = "2" if check2checkmaizeq26panner =="False"
replace check2checkmaizeq26wema = "2" if check2checkmaizeq26wema =="False"
replace check2checkmaizeq26kh_series = "2" if check2checkmaizeq26kh_series =="False"
replace check2checkmaizeq26land_races = "2" if check2checkmaizeq26land_races =="False"
replace check2checkmaizeq26other_hybrid = "2" if check2checkmaizeq26other_hybrid =="False"

destring check2checkmaizeq26longe_10h- check2checkmaizeq26other_hybrid, replace
recode check2checkmaizeq26longe_10h- check2checkmaizeq26other_hybrid (1=100) (2=0)

// % of farmers that are aware of different varieties: 2021 maize survey
mean check2checkmaizeq26longe_10h- check2checkmaizeq26other_hybrid // 15% were aware about Bazooka 

// 2021 adoption of maize varieties: estimated by looking at the variety that was planted on a randomly selected crop
rename check2checkmaizeq31 varietyplanted
tab varietyplanted, sort

// varieties planted on the selected plot in 2021
replace varietyplanted = "1" if varietyplanted == "Longe_10H"
replace varietyplanted = "2" if varietyplanted == "Longe_7H"
replace varietyplanted = "3" if varietyplanted == "Longe_7R_Kayongo-go"
replace varietyplanted = "4" if varietyplanted == "Bazooka"
replace varietyplanted = "5" if varietyplanted == "Longe_6H"
replace varietyplanted = "6" if varietyplanted == "Longe_5"
replace varietyplanted = "7" if varietyplanted == "Longe_4"
replace varietyplanted = "8" if varietyplanted == "Panner"
replace varietyplanted = "9" if varietyplanted == "Wema"
replace varietyplanted = "10" if varietyplanted == "KH_series"
replace varietyplanted = "11" if varietyplanted == "Land_Races"
replace varietyplanted = "12" if varietyplanted == "Other_hybrid"

destring varietyplanted, replace

label define variety2021 1 "Longe 10H" 2 "Longe 7H" 3 "Longe 7R/Kayongo-go" 4 "Bazooka" 5 "Longe 6H" 6 "Longe 5 (nalongo)" 7 "Longe 4" 8 "Panner" 9 "Wema" 10 "KH series" 11 "Land Races/local varieties" 12 "Others" 98 "Don't know"
label values varietyplanted variety2021
tab varietyplanted // 5% planted Bazooka




