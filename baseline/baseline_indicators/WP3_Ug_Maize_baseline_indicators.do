/* DO FILE HAS BEEN PREPARED BY LEOCARDIA NABWIRE //
** FOR THE CALCULATION OF THE MIPPI WP3 baseline indicators for the Uganda maize project.
*** Date last modified: August 17, 2023. 

*/

clear all

cd "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG\baseline\data\public"

*set maxvar 10000
set mem 500m
set more off
cap log close


*** OPEN DATA FILE  
**************************************************

import delimited "baseline.csv"

cd "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG\baseline\baseline_indicators"

sort farmer_id
ed farmer_id
tab farmer_id
ed
drop if farmer_id == ""

destring today- distid, replace ignore ("n/a")

********* TREATMENT ASSIGNMENT
// Proportion of hhs in control, free trial pack, 100% discount, and 0% discount
tab1 paid_pac cont trial_p discounted

rename cont cons
replace cons = "1" if cons == "TRUE"
replace cons = "0" if cons == "FALSE"

replace trial_p = "1" if trial_p == "TRUE"
replace trial_p = "0" if trial_p == "FALSE"

replace paid_pac = "1" if paid_pac == "TRUE"
replace paid_pac = "0" if paid_pac == "FALSE"

replace discounted = "1" if discounted == "TRUE"
replace discounted = "0" if discounted == "FALSE"

destring paid_pac- discounted, replace ignore ("n/a")
sum paid_pac- discounted

sort cons paid_pac trial_p discounted
ed cons paid_pac trial_p discounted

egen cont = rowtotal (cons paid_pac trial_p discounted)
order cont, before (paid_pac)
recode cont (0=1) (nonmissing=0)

rename trial_p free_pack

egen trial_pack = rowtotal(free_pack paid_pac discounted)
tab trial_pack

asdoc sum cont trial_pack cons paid_pac discounted, stat(N mean sd) dec(3) tzok title(Treatment assignment proportions) save(indicator_results) replace

*____________________________________________________________________________________


********* DEMOGRAPHICS INDICATOR_RESULTS

// Household head is female
tab gender
replace gender = "1" if gender == "Female" // gender of household head
replace gender = "0" if gender == "Male"
destring gender, replace ignore ("n/a")

asdoc sum gender, stat(N mean sd min max) dec(3) tzok title(Household head is female) save(indicator_results)
asdoc sum gender if cont == 1, stat(N mean sd min max) dec(3) tzok title(Household head is female in control) save(indicator_results)
asdoc sum gender if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Household head is female in trial pack treatment) save(indicator_results)
asdoc sum gender if cons == 1, stat(N mean sd min max) dec(3) tzok title(Household head is female in consumption treatment) save(indicator_results)

// Treatment status in male and female headed households
asdoc sum cont trial_pack cons paid_pac discounted if gender == 0, stat(N mean sd) dec(3) tzok title(Treatment assignment proportions in male headed households) save(indicator_results)

asdoc sum cont trial_pack cons paid_pac discounted if gender == 1, stat(N mean sd) dec(3) tzok title(Treatment assignment proportions in female headed households) save(indicator_results)

// Respondent is female
replace resp_gender = "1" if resp_gender == "Female" // gender of respondent
replace resp_gender = "0" if resp_gender == "Male"
destring resp_gender, replace ignore ("n/a")

gen resp_female = resp_gender
tab resp_female
replace resp_female = gender if check2q1 == "Yes"

tab resp_female

asdoc sum resp_female, stat(N mean sd min max) dec(3) tzok title(Respondent is female) save(indicator_results)
asdoc sum resp_female if cont == 1, stat(N mean sd min max) dec(3) tzok title(Respondent is female in control) save(indicator_results)
asdoc sum resp_female if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Respondent is female in trial pack treatment) save(indicator_results)
asdoc sum resp_female if cons == 1, stat(N mean sd min max) dec(3) tzok title(Respondent is female in consumption treatment) save(indicator_results)
asdoc sum resp_female if gender == 0, stat(N mean sd min max) dec(3) tzok title(Respondent is female in male headed households) save(indicator_results)
asdoc sum resp_female if gender == 1, stat(N mean sd min max) dec(3) tzok title(Respondent is female in female headed households) save(indicator_results)


// Respondent is household head
replace check2q1 = "1" if check2q1 == "Yes"
replace check2q1 = "0" if check2q1 == "No"
destring check2q1, replace ignore ("n/a")
sum check2q1

asdoc sum check2q1, stat(N mean sd min max) dec(3) tzok title(Respondent is household head) save(indicator_results)
asdoc sum check2q1 if cont == 1, stat(N mean sd min max) dec(3) tzok title(Respondent is household head in control) save(indicator_results)
asdoc sum check2q1 if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Respondent is household head in trial pack treatment) save(indicator_results)
asdoc sum check2q1 if cons == 1, stat(N mean sd min max) dec(3) tzok title(Respondent is household head in consumption treatment) save(indicator_results)
asdoc sum check2q1 if cons == 0, stat(N mean sd min max) dec(3) tzok title(Respondent is household head in male headed households) save(indicator_results)
asdoc sum check2q1 if cons == 1, stat(N mean sd min max) dec(3) tzok title(Respondent is household head in female headed households) save(indicator_results)


// Respondent's age(years): we only asked about the age of the household head, so respondent's age only applies where respondent was hh head (check2q1 == 1)
sum age if check2q1 == 1
replace age =. if age ==999
sum age if check2q1 == 1

asdoc sum age if check2q1 == 1, stat(N mean sd min max) dec(3) tzok title(Respondent's Age/years) save(indicator_results)
asdoc sum age if check2q1 == 1 & cont == 1, stat(N mean sd min max) dec(3) tzok title(Respondent's Age/years in control group) save(indicator_results)
asdoc sum age if check2q1 == 1 & trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Respondent's Age/years in trial pack group) save(indicator_results)
asdoc sum age if check2q1 == 1 & cons == 1, stat(N mean sd min max) dec(3) tzok title(Respondent's Age/years in consumption group) save(indicator_results)
asdoc sum age if check2q1 == 1 & gender == 0, stat(N mean sd min max) dec(3) tzok title(Respondent's Age/years in male headed hhs) save(indicator_results)
asdoc sum age if check2q1 == 1 & gender == 1, stat(N mean sd min max) dec(3) tzok title(Respondent's Age/years in female headed hhs) save(indicator_results)


// Age of household head
asdoc sum age, stat(N mean sd min max) dec(3) tzok title(Household head's Age/years) save(indicator_results)
asdoc sum age if cont == 1, stat(N mean sd min max) dec(3) tzok title(Household head's Age/years in control group) save(indicator_results)
asdoc sum age if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Household head's Age/years in trial pack group) save(indicator_results)
asdoc sum age if cons == 1, stat(N mean sd min max) dec(3) tzok title(Household head's Age/years in consumption group) save(indicator_results)
asdoc sum age if gender == 0, stat(N mean sd min max) dec(3) tzok title(Household head's Age/years in male headed hhs) save(indicator_results)
asdoc sum age if gender == 1, stat(N mean sd min max) dec(3) tzok title(Household head's Age/years in female headed hhs) save(indicator_results)


// Respondent's Average years of Education: we only asked about the education of the household head, so respondent's years of education only applies where respondent was hh head (check2q1 == 1)
sum sch_yrs
replace sch_yrs =. if sch_yrs == 999
sum sch_yrs
sort sch_yrs
ed sch_yrs edu
replace sch_yrs = . if sch_yrs == 106

sum sch_yrs if check2q1 == 1


asdoc sum sch_yrs if check2q1 == 1, stat(N mean sd min max) dec(3) tzok title(Respondent's Average years of Education) save(indicator_results)

asdoc sum sch_yrs if check2q1 == 1 & cont == 1, stat(N mean sd min max) dec(3) tzok title(Respondent's Average years of Education in control group) save(indicator_results)

asdoc sum sch_yrs if check2q1 == 1 & trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Respondent's Average years of Education in trial pack group) save(indicator_results)

asdoc sum sch_yrs if check2q1 == 1 & cons == 1, stat(N mean sd min max) dec(3) tzok title(Respondent's Average years of Education in consumption group) save(indicator_results)

asdoc sum sch_yrs if check2q1 == 1 & gender == 0, stat(N mean sd min max) dec(3) tzok title(Respondent's Average years of Education in male headed hhs) save(indicator_results)

asdoc sum sch_yrs if check2q1 == 1 & gender == 1, stat(N mean sd min max) dec(3) tzok title(Respondent's Average years of Education in female headed hhs) save(indicator_results)


// Household head's Average years of Education
asdoc sum sch_yrs, stat(N mean sd min max) dec(3) tzok title(Household head's Average years of Education) save(indicator_results)
asdoc sum sch_yrs if cont == 1, stat(N mean sd min max) dec(3) tzok title(Household head's Average years of Education in control group) save(indicator_results)
asdoc sum sch_yrs if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Household head's Average years of Education in trial pack group) save(indicator_results)
asdoc sum sch_yrs if cons == 1, stat(N mean sd min max) dec(3) tzok title(Household head's Average years of Education in consumption group) save(indicator_results)
asdoc sum sch_yrs if gender == 0, stat(N mean sd min max) dec(3) tzok title(Household head's Average years of Education in male headed hhs) save(indicator_results)
asdoc sum sch_yrs if gender == 1, stat(N mean sd min max) dec(3) tzok title(Household head's Average years of Education in female headed hhs) save(indicator_results)



// Respondent is Literate (completed primary education) == Yes: we only asked about the education of the household head, so respondent's years of education only applies where respondent was hh head (check2q1 == 1)
 
replace edu = "0" if edu == "a" // no formal education
replace edu = "0" if edu == "b" // some primary education
replace edu = "1" if edu == "c" // completed primary education
replace edu = "1" if edu == "d" // some secondary education
replace edu = "1" if edu == "e" // completed secondary education
replace edu = "1" if edu == "f" // Higher than secondary
replace edu = "1" if edu == "g" // Other
destring edu, replace ignore ("n/a")
sum edu if check2q1 == 1

asdoc sum edu if check2q1 == 1, stat(N mean sd min max) dec(3) tzok title(Respondent completed primary education) save(indicator_results)

asdoc sum edu if check2q1 == 1 & cont == 1, stat(N mean sd min max) dec(3) tzok title(Respondent completed primary education in control group) save(indicator_results)

asdoc sum edu if check2q1 == 1 & trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Respondent completed primary education in trial pack group) save(indicator_results)

asdoc sum edu if check2q1 == 1 & cons == 1, stat(N mean sd min max) dec(3) tzok title(Respondent completed primary education in consumption group) save(indicator_results)

asdoc sum edu if check2q1 == 1 & gender == 0, stat(N mean sd min max) dec(3) tzok title(Respondent completed primary education in male headed hhs) save(indicator_results)

asdoc sum edu if check2q1 == 1 & gender == 1, stat(N mean sd min max) dec(3) tzok title(Respondent completed primary education in female headed hhs) save(indicator_results)


// Household head is Literate (completed primary education) == Yes 
asdoc sum edu, stat(N mean sd min max) dec(3) tzok title(Household head completed primary education) save(indicator_results)
asdoc sum edu if cont == 1, stat(N mean sd min max) dec(3) tzok title(Household head completed primary education in control group) save(indicator_results)
asdoc sum edu if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Household head completed primary education in trial pack group) save(indicator_results)
asdoc sum edu if cons == 1, stat(N mean sd min max) dec(3) tzok title(Household head completed primary education in consumption group) save(indicator_results)
asdoc sum edu if gender == 0, stat(N mean sd min max) dec(3) tzok title(Household head completed primary education in male headed hhs) save(indicator_results)
asdoc sum edu if gender == 1, stat(N mean sd min max) dec(3) tzok title(Household head completed primary education in female headed hhs) save(indicator_results)

// Household size
sum hh_size

asdoc sum hh_size, stat(N mean sd min max) dec(3) tzok title(Household size) save(indicator_results)
asdoc sum hh_size if cont == 1, stat(N mean sd min max) dec(3) tzok title(Household size in control group) save(indicator_results)
asdoc sum hh_size if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Household size in trial pack group) save(indicator_results)
asdoc sum hh_size if cons == 1, stat(N mean sd min max) dec(3) tzok title(Household size in consumption group) save(indicator_results)
asdoc sum hh_size if gender == 0, stat(N mean sd min max) dec(3) tzok title(Household size in male headed hhs) save(indicator_results)
asdoc sum hh_size if gender == 1, stat(N mean sd min max) dec(3) tzok title(Household size in female headed hhs) save(indicator_results)


// Average Income (/other welfare indicator)
* Living space (percapita rooms) in the household
sum rooms hh_size
gen percapita_rooms = rooms/hh_size
sum percapita_rooms

asdoc sum percapita_rooms, stat(N mean sd min max) dec(3) tzok title(Living space/percapita rooms in the household) save(indicator_results)

asdoc sum percapita_rooms if cont == 1, stat(N mean sd min max) dec(3) tzok title(Living space/percapita rooms in the household in control group) save(indicator_results)

asdoc sum percapita_rooms if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Living space/percapita rooms in the household in trial pack group) save(indicator_results)

asdoc sum percapita_rooms if cons == 1, stat(N mean sd min max) dec(3) tzok title(Living space/percapita rooms in the household in consumption group) save(indicator_results)

asdoc sum percapita_rooms if gender == 0, stat(N mean sd min max) dec(3) tzok title(Living space/percapita rooms in the household in male headed hhs) save(indicator_results)

asdoc sum percapita_rooms if gender == 1, stat(N mean sd min max) dec(3) tzok title(Living space/percapita rooms in the household in female headed hhs) save(indicator_results)


// Distance to Nearest Market (/other remoteness measure) nearest agro-input shop selling maize seed
sum  dist_ag
replace dist_ag = . if dist_ag == 999
sum dist_ag
sort dist_ag
ed dist_ag
replace dist_ag = . if dist_ag == 100
sum dist_ag

asdoc sum dist_ag, stat(N mean sd min max) dec(3) tzok title(Kms to nearest agro-input shop selling maize seed) save(indicator_results)

asdoc sum dist_ag if cont == 1, stat(N mean sd min max) dec(3) tzok title(Kms to nearest agro-input shop selling maize seed in control group) save(indicator_results)

asdoc sum dist_ag if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Kms to nearest agro-input shop selling maize seed in trial pack group) save(indicator_results)

asdoc sum dist_ag if cons == 1, stat(N mean sd min max) dec(3) tzok title(Kms to nearest agro-input shop selling maize seed in consumption group) save(indicator_results)

asdoc sum dist_ag if gender == 0, stat(N mean sd min max) dec(3) tzok title(Kms to nearest agro-input shop selling maize seed in male headed hhs) save(indicator_results)

asdoc sum dist_ag if gender == 1, stat(N mean sd min max) dec(3) tzok title(Kms to nearest agro-input shop selling maize seed in female headed hhs) save(indicator_results)






*_____________________________________________________________________________

******** FARMING AND SEEDS INDICATOR_RESULTS

// Total Land Owned/available for crop production
sum ttl_land
replace ttl_land =. if ttl_land == 999
sum ttl_land
sort ttl_land
ed ttl_land
replace ttl_land =. if ttl_land == 100 // an outlier
sum ttl_land

gen ttl_land_ha = ttl_land/2.5 // converting the amount of land into hectares
sum ttl_land ttl_land_ha

asdoc sum ttl_land_ha, stat(N mean sd min max) dec(3) tzok title(Total Land/ha available for crop production) save(indicator_results)

asdoc sum ttl_land_ha if cont == 1, stat(N mean sd min max) dec(3) tzok title(Total Land/ha available for crop production in control group) save(indicator_results)

asdoc sum ttl_land_ha if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Total Land/ha available for crop production in trial pack group) save(indicator_results)

asdoc sum ttl_land_ha if cons == 1, stat(N mean sd min max) dec(3) tzok title(Total Land/ha available for crop production in consumption group) save(indicator_results)

asdoc sum ttl_land_ha if gender == 0, stat(N mean sd min max) dec(3) tzok title(Total Land/ha available for crop production in male headed hhs) save(indicator_results)

asdoc sum ttl_land_ha if gender == 1, stat(N mean sd min max) dec(3) tzok title(Total Land/ha available for crop production in female headed hhs) save(indicator_results)


// Total Land Cultivated (randomly selected maize plot grown in the 2nd season of 2022)
sum plot_size
replace plot_size =. if plot_size == 999
sum plot_size
sort plot_size
ed plot_size

gen plot_size_ha = plot_size/2.5 // converting the amount of land into hectares
sum plot_size plot_size_ha

asdoc sum plot_size_ha, stat(N mean sd min max) dec(3) tzok title(Average size/ha of randomly selected maize plot) save(indicator_results)

asdoc sum plot_size_ha if cont == 1, stat(N mean sd min max) dec(3) tzok title(Average size/ha of randomly selected maize plot in control group) save(indicator_results)

asdoc sum plot_size_ha if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Average size/ha of randomly selected maize plot in trial pack group) save(indicator_results)

asdoc sum plot_size_ha if cons == 1, stat(N mean sd min max) dec(3) tzok title(Average size/ha of randomly selected maize plot in consumption group) save(indicator_results)

asdoc sum plot_size_ha if gender == 0, stat(N mean sd min max) dec(3) tzok title(Average size/ha of randomly selected maize plot in male headed hhs) save(indicator_results)

asdoc sum plot_size_ha if gender == 1, stat(N mean sd min max) dec(3) tzok title(Average size/ha of randomly selected maize plot in female headed hhs) save(indicator_results)


// Amount (bags) harvested in second season of 2022 
sum bag_harv // 4.5bags on average
sort bag_harv
ed bag_harv
replace bag_harv =. if bag_harv == 999
sum bag_harv // 4.5bags on average

// Amount (bags) sold in second season of 2022 
sum bag_sell
replace bag_sell = 0 if bag_sell == . & bag_harv !=.


// Amount (bags) kept for consumption in second season of 2022 
sum bag_keep
sum bag_harv bag_sell bag_keep
sort bag_keep
ed bag_harv bag_sell bag_keep

replace bag_keep =. if bag_keep ==999
replace bag_keep =. if bag_keep == 100000

gen sell_bal = bag_harv - bag_sell // balance of maize harvest after selling
count if bag_keep > sell_bal
replace bag_keep = . if bag_keep > sell_bal & bag_keep !=.

sort bag_keep
ed bag_harv bag_sell bag_keep
count if bag_keep > bag_harv // 94 cases where the amount kept for consumption exceeds what was harvested
replace bag_keep = . if bag_keep > bag_harv & bag_keep !=.
replace bag_keep = 0 if bag_keep == . & bag_harv !=.
sum bag_keep


// Value (average price charged) for every bag sold to be used to determine the value of total harvest, value of amount sold, and value of amount counsumed

sum bag_charge
sort bag_charge
ed bag_sell bag_kg bag_charge


// removing outliers from the price charged per bag
replace bag_charge = . if bag_charge == 999
replace bag_charge = bag_charge/bag_sell if bag_charge == 5600000 // looks like some enumerators recorded the total revenue from all bags sold instead of 	price per bag

replace bag_charge = 80000 if bag_charge == 800000
replace bag_charge = bag_charge/bag_sell if bag_charge > 270000
sort bag_charge

replace bag_charge =. if bag_charge < 10
replace bag_charge = 1000 if bag_charge == 100 // looks like some enumerators recorded price per kg instead of price per bag 
sort bag_charge

replace bag_charge = 50000 in 1
replace bag_charge= bag_kg* bag_sell* bag_charge if bag_sell == 0.35
replace bag_charge = 100000 if bag_charge == 1000 & bag_sell > 0.99

sort bag_charge
replace bag_charge = 130000 in 2

sort bag_charge
replace bag_charge = bag_charge* bag_kg if bag_charge < 3000

sort bag_charge
replace bag_charge = 60000 in 2
replace bag_charge = 90000 in 3
replace bag_charge = 100000 in 5
replace bag_charge = 100000 in 6
replace bag_charge = 120000 in 7
replace bag_charge = 140000 in 8

sum bag_charge

egen av_price_bag = mean(bag_charge)
gen av_price_bag_usd = av_price_bag/3720.75 // price per bag in usd
sum av_price_bag av_price_bag_usd


// Value of Total harvest for second season of 2022 in usd
gen tt_harv_value_usd = bag_harv * av_price_bag_usd
sum tt_harv_value_usd

// % of Harvest Value Consumed by HH
sum bag_keep av_price_bag_usd

gen consumed_value_usd = (bag_keep*av_price_bag_usd)
sum tt_harv_value_usd consumed_value_usd

gen consumed_percent =(consumed_value_usd/tt_harv_value_usd)*100
sum consumed_percent


// % of Harvest Value Sold by HH
gen bag_sell_usd = bag_sell * av_price_bag_usd // value of total amount sold in usd

gen sold_percent = (bag_sell_usd/tt_harv_value_usd)*100
sum sold_percent

sum sold_percent


// Value to total harvest, % amount consumed & % amount sold
asdoc sum tt_harv_value_usd consumed_percent sold_percent, stat(N mean sd min max) dec(3) tzok title(Value to total harvest, % amount consumed & % amount sold) save(indicator_results)

asdoc sum tt_harv_value_usd consumed_percent sold_percent if cont == 1, stat(N mean sd min max) dec(3) tzok title(Value to total harvest, % amount consumed & % amount sold in control group) save(indicator_results)

asdoc sum tt_harv_value_usd consumed_percent sold_percent if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Value to total harvest, % amount consumed & % amount sold in trial pack group) save(indicator_results)

asdoc sum tt_harv_value_usd consumed_percent sold_percent if cons == 1, stat(N mean sd min max) dec(3) tzok title(Value to total harvest, % amount consumed & % amount sold in consumption group) save(indicator_results)

asdoc sum tt_harv_value_usd consumed_percent sold_percent if gender == 0, stat(N mean sd min max) dec(3) tzok title(Value to total harvest, % amount consumed & % amount sold in male headed hhs) save(indicator_results)

asdoc sum tt_harv_value_usd consumed_percent sold_percent if gender == 1, stat(N mean sd min max) dec(3) tzok title(Value to total harvest, % amount consumed & % amount sold in female headed hhs) save(indicator_results)


// Fraction that uses Any Improved Seeds == Yes
tab quality_use
gen uses_improved = quality_use
tab uses_improved
replace uses_improved = "1" if uses_improved == "Yes"
replace uses_improved = "0" if uses_improved == "No"
destring uses_improved, replace ignore ("98")
sum uses_improved

asdoc sum uses_improved, stat(N mean sd min max) dec(3) tzok title(Fraction that uses Any Improved Seeds) save(indicator_results)
asdoc sum uses_improved if cont == 1, stat(N mean sd min max) dec(3) tzok title(Fraction that uses Any Improved Seeds in control group) save(indicator_results)
asdoc sum uses_improved if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Fraction that uses Any Improved Seeds in trial pack group) save(indicator_results)
asdoc sum uses_improved if cons == 1, stat(N mean sd min max) dec(3) tzok title(Fraction that uses Any Improved Seeds in consumption group) save(indicator_results)
asdoc sum uses_improved if gender == 0, stat(N mean sd min max) dec(3) tzok title(Fraction that uses Any Improved Seeds in male headed hhs) save(indicator_results)
asdoc sum uses_improved if gender == 1, stat(N mean sd min max) dec(3) tzok title(Fraction that uses Any Improved Seeds in female headed hhs) save(indicator_results)


// Fraction that uses Bazooka maize variety == Yes
tab bazo_use
gen uses_bazo = bazo_use
tab uses_bazo
replace uses_bazo = "1" if uses_bazo == "Yes"
replace uses_bazo = "0" if uses_bazo == "No"
tab uses_bazo
destring uses_bazo, replace ignore ("n/a" "98")
tab uses_bazo
sum uses_bazo

asdoc sum uses_bazo, stat(N mean sd min max) dec(3) tzok title(Fraction that used Bazooka maize variety) save(indicator_results)
asdoc sum uses_bazo if cont == 1, stat(N mean sd min max) dec(3) tzok title(Fraction that used Bazooka maize variety in control group) save(indicator_results)
asdoc sum uses_bazo if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Fraction that used Bazooka maize variety in trial pack group) save(indicator_results)
asdoc sum uses_bazo if cons == 1, stat(N mean sd min max) dec(3) tzok title(Fraction that used Bazooka maize variety in consumption group) save(indicator_results)
asdoc sum uses_bazo if gender == 0, stat(N mean sd min max) dec(3) tzok title(Fraction that used Bazooka maize variety in male headed hhs) save(indicator_results)
asdoc sum uses_bazo if gender == 1, stat(N mean sd min max) dec(3) tzok title(Fraction that used Bazooka maize variety in female headed hhs) save(indicator_results)

// Most planted maize seed variety
tab maize_var, sort // Most used varieties are Land races--- 52%, Longe 10H--- 14% and Longe 5--- 13%

// Seed Replacement Rate for key crop (specify- most used variety) (# years that the same seeds are recycled before getting new seeds)
tab often // number of times the seed was recycled
replace often = "1" if often == "a"  // 1st time
replace often = "2" if often == "b" // 2nd time 
replace often = "3" if often == "c" // 3rd time 
replace often = "4" if often == "d" // 4th time
replace often = "5" if often == "e" // 5th time
replace often = "6" if often == "f" //more that 5th time
replace often = "." if often == "98" // other 

destring often, replace ignore ("n/a")
sum often

asdoc sum often, stat(N mean sd min max) dec(3) tzok title(Number of times the planted seed was recycled) save(indicator_results)
asdoc sum often if cont == 1, stat(N mean sd min max) dec(3) tzok title(Number of times the planted seed was recycled in control group) save(indicator_results)
asdoc sum often if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Number of times the planted seed was recycled in trial pack group) save(indicator_results)
asdoc sum often if cons == 1, stat(N mean sd min max) dec(3) tzok title(Number of times the planted seed was recycled in consumption group) save(indicator_results)
asdoc sum often if gender == 0, stat(N mean sd min max) dec(3) tzok title(Number of times the planted seed was recycled in male headed hhs) save(indicator_results)
asdoc sum often if gender == 1, stat(N mean sd min max) dec(3) tzok title(Number of times the planted seed was recycled in female headed hhs) save(indicator_results)


// Sources of seed
tab source
replace source = "1" if source == "a"
replace source = "2" if source == "b"
replace source = "3" if source == "c"
replace source = "4" if source == "d"
replace source = "5" if source == "e"
replace source = "6" if source == "f"
replace source = "7" if source == "g"
replace source = "8" if source == "i"
replace source = "9" if source == "h"

tab source
label define source 1 "Farmer own saved seed" 2 "Farmer saved seed from someone else" 3 "Bought in the local market or grain shop" 4 "Bought from agro-input shop" 5 "From OWC/NAADS (Operation Wealth" 6 "From NGO" 7 "From a seed company" 8 "From a Local Seed Business" 9 "From research station" 96 "Others"
destring source, replace ignore ("96")
label values source source
tab source // 1st source--- Farmer own saved seed;  2nd source--- Bought from agro-input shop; 3rd source-- Farmer saved seed from someone else 


//Gets Seed from Most Common Source (Farmer own saved seed) == Yes
gen saved_seed_farmer = source
recode saved_seed_farmer (1=1) (nonmissing=0)
tab saved_seed_farmer

// Gets Seed from 2nd Most Common Source (agro-input shop) == Yes
gen agro_input_shop = source
recode agro_input_shop (4=1) (nonmissing=0)
tab agro_input_shop

// Gets Seed from 3rd Most Common Source (Farmer saved seed from someone else) == Yes
gen other_farmer_saved = source
recode other_farmer_saved (2=1) (nonmissing=0)
tab other_farmer_saved


asdoc sum saved_seed_farmer agro_input_shop other_farmer_saved, stat(N mean sd min max) dec(3) tzok title(Top 3 sources of seed) save(indicator_results)

asdoc sum saved_seed_farmer agro_input_shop other_farmer_saved if cont == 1, stat(N mean sd min max) dec(3) tzok title(Top 3 sources of seed in control group) save(indicator_results)

asdoc sum saved_seed_farmer agro_input_shop other_farmer_saved if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Top 3 sources of seed in trial pack group) save(indicator_results)

asdoc sum saved_seed_farmer agro_input_shop other_farmer_saved if cons == 1, stat(N mean sd min max) dec(3) tzok title(Top 3 sources of seed in consumption group) save(indicator_results)

asdoc sum saved_seed_farmer agro_input_shop other_farmer_saved if gender == 0, stat(N mean sd min max) dec(3) tzok title(Top 3 sources of seed in male headed hhs) save(indicator_results)

asdoc sum saved_seed_farmer agro_input_shop other_farmer_saved if gender == 1, stat(N mean sd min max) dec(3) tzok title(Top 3 sources of seed in female headed hhs) save(indicator_results)


// Varietal Turnover Rate for key crop (maize) (# years that same variety is grown before adopting a new variety)
sum long_var
sort long_var
ed long_var
replace long_var = . if long_var == 999
replace long_var = . if long_var == 0
ed long_var
replace long_var = 19 if long_var == 2004
sum long_var


asdoc sum long_var, stat(N mean sd min max) dec(3) tzok title(Number of years the variety has been grown) save(indicator_results)

asdoc sum long_var if cont == 1, stat(N mean sd min max) dec(3) tzok title(Number of years the variety has been grown in control group) save(indicator_results)

asdoc sum long_var if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Number of years the variety has been grown in trial pack group) save(indicator_results)

asdoc sum long_var if cons == 1, stat(N mean sd min max) dec(3) tzok title(Number of years the variety has been grown in consumption group) save(indicator_results)

asdoc sum long_var if gender == 0, stat(N mean sd min max) dec(3) tzok title(Number of years the variety has been grown in male headed hhs) save(indicator_results)

asdoc sum long_var if gender == 1, stat(N mean sd min max) dec(3) tzok title(Number of years the variety has been grown in female headed hhs) save(indicator_results)


// Varietal Age for key crop (maize)  (# years ago that varieties farmers are using were released)
tab maize_var // varieties grown by farmers
gen var_year = . // variable for year when the variety was released 

replace var_year = . if maize_var == "Land_Races"
replace var_year = 2009 if maize_var == "Longe_10H"
replace var_year = 2000 if maize_var == "Longe_5"
replace var_year = 2013 if maize_var == "Bazooka"
replace var_year = 2000 if maize_var == "Longe_4"
replace var_year = 2003 if maize_var == "DK"
replace var_year = 2002 if maize_var == "Longe_7R_Kayongo-go"
replace var_year = 2002 if maize_var == "Longe_7H"
replace var_year = . if maize_var == "KH_series"
replace var_year = 2002 if maize_var == "Longe_6H"
replace var_year = 2016 if maize_var == "Panner"
replace var_year = 2014 if maize_var == "Wema"
replace var_year = . if maize_var == "Other_hybrid"

gen var_age = 2023 - var_year // for land races, we replace the age with 67 years, the longest number of years (see variable long_var) a farmer has used landraces

sum long_var if maize_var == "Land_Races" // Max value (number of years) land races have been used is 67 years
replace var_age = 67 if maize_var == "Land_Races"
sum var_age

asdoc sum var_age, stat(N mean sd min max) dec(3) tzok title(Varietal Age for maize varieties grown by farmers) save(indicator_results)

asdoc sum var_age if cont == 1, stat(N mean sd min max) dec(3) tzok title(Varietal Age for maize varieties grown by farmers in control group) save(indicator_results)

asdoc sum var_age if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Varietal Age for maize varieties grown by farmers in trial pack group) save(indicator_results)

asdoc sum var_age if cons == 1, stat(N mean sd min max) dec(3) tzok title(Varietal Age for maize varieties grown by farmers in consumption group) save(indicator_results)

asdoc sum var_age if gender == 0, stat(N mean sd min max) dec(3) tzok title(Varietal Age for maize varieties grown by farmers in male headed hhs) save(indicator_results)

asdoc sum var_age if gender == 1, stat(N mean sd min max) dec(3) tzok title(Varietal Age for maize varieties grown by farmers in female headed hhs) save(indicator_results)


// Preferred production and consumption traits
des gen_qlty- cook
sum gen_qlty- cook
tostring gen_qlty- cook, replace
destring gen_qlty- cook, replace ignore ("98")
sum gen_qlty- cook

// Rating (1 to 5) of the most preferred production trait (high yield) for the variety grown on a randomly selected plot
* NB:The top 3 preferred production traits were identified through qualitative consultations among 36 maize value chain actors. During baseline, we asked farmers to rate (on a scale of 1-5, where higher is better) the varieties they had grown in terms of the identified traits. 

asdoc sum yield_rate erly_mat drt_tol, stat(N mean sd min max) dec(3) tzok title(Grown variety rating/1 to 5/ for the top 3 preferred production triats) save(indicator_results)

asdoc sum yield_rate erly_mat drt_tol if cont == 1, stat(N mean sd min max) dec(3) tzok title(Grown variety rating/1 to 5/ for the top 3 preferred production triats in control group) save(indicator_results)

asdoc sum yield_rate erly_mat drt_tol if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Grown variety rating/1 to 5/ for the top 3 preferred production triats in trial pack group) save(indicator_results)

asdoc sum yield_rate erly_mat drt_tol if cons == 1, stat(N mean sd min max) dec(3) tzok title(Grown variety rating/1 to 5/ for the top 3 preferred production triats in consumption group) save(indicator_results)

asdoc sum yield_rate erly_mat drt_tol if gender == 0, stat(N mean sd min max) dec(3) tzok title(Grown variety rating/1 to 5/ for the top 3 preferred production triats in male headed hhs) save(indicator_results)

asdoc sum yield_rate erly_mat drt_tol if gender == 1, stat(N mean sd min max) dec(3) tzok title(Grown variety rating/1 to 5/ for the top 3 preferred production triats in female headed hhs) save(indicator_results)


// Rating (1 to 5) of the most preferred consumption trait (taste) for the variety grown on a randomly selected plot
* NB:The top 3 preferred production traits were identified through qualitative consultations among 36 maize value chain actors. During baseline, we asked farmers to rate (on a scale of 1-5, where higher is better) the varieties they had grown in terms of the identified traits. 
sum taste

asdoc sum taste, stat(N mean sd min max) dec(3) tzok title(Grown variety rating/1 to 5/ for its taste) save(indicator_results)
asdoc sum taste if cont == 1, stat(N mean sd min max) dec(3) tzok title(Grown variety rating/1 to 5/ for its taste in control group) save(indicator_results)
asdoc sum taste if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Grown variety rating/1 to 5/ for its taste in trial pack group) save(indicator_results)
asdoc sum taste if cons == 1, stat(N mean sd min max) dec(3) tzok title(Grown variety rating/1 to 5/ for its taste in consumption group) save(indicator_results)
asdoc sum taste if gender == 0, stat(N mean sd min max) dec(3) tzok title(Grown variety rating/1 to 5/ for its taste in male headed hhs) save(indicator_results)
asdoc sum taste if gender == 1, stat(N mean sd min max) dec(3) tzok title(Grown variety rating/1 to 5/ for its taste in female headed hhs) save(indicator_results)


*_____________________________________________________________________________________________


*********INDICATOR_RESULTS FOR USE OF MAIZE RESIDUE
sum cr_use_grpcr_use_collfeed - cr_use_grpcr_use_othr
rename cr_use_grpcr_use_collfeed feed_own_animals
rename cr_use_grpcr_use_collsale sold
rename cr_use_grpcr_use_grz left_field_for_animals
rename cr_use_grpcr_use_mlch mulch
rename cr_use_grpcr_use_brnt burnt_in_field
sum feed_own_animals sold left_field_for_animals mulch burnt_in_field

asdoc sum feed_own_animals sold left_field_for_animals mulch burnt_in_field, stat(N mean sd min max) dec(3) tzok title(use of crop residue) save(indicator_results)


// proportions (%) of residues fed the livestock
sum cr_lvst_grpcr_lvst_ctl - cr_lvst_grpcr_lvst_othr_spec
rename cr_lvst_grpcr_lvst_ctl fed_to_cattle
rename cr_lvst_grpcr_lvst_shp fed_to_sheep
rename cr_lvst_grpcr_lvst_goat fed_to_goats
rename cr_lvst_grpcr_lvst_pig fed_to_pigs
rename cr_lvst_grpcr_lvst_othr fed_to_otherlvs

asdoc sum fed_to_cattle fed_to_sheep fed_to_goats fed_to_pigs fed_to_otherlvs, stat(N mean sd min max) dec(3) tzok title(share/% of residues fed the livestock) save(indicator_results)

*____________________________________________________________________________________



************ WOMEN ENGAGEMENT

// Share of women who individually own a maize plot

tab plot_own
des plot_own
gen woman_own = plot_own
recode woman_own (2=1) (nonmissing = 0)
sum woman_own

// Share of women who individually own a maize plot
tab plot_man
des plot_man
gen woman_man = plot_man
recode woman_man (2=1) (nonmissing = 0)
sum woman_man

asdoc sum woman_own woman_man, stat(N mean sd min max) dec(3) tzok title(A woman individually owns or manages a maize plot) save(indicator_results)

asdoc sum woman_own woman_man if cont == 1, stat(N mean sd min max) dec(3) tzok title(A woman individually owns or manages a maize plot in control group) save(indicator_results)

asdoc sum woman_own woman_man if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(A woman individually owns or manages a maize plot in trial pack group) save(indicator_results)

asdoc sum woman_own woman_man if cons == 1, stat(N mean sd min max) dec(3) tzok title(A woman individually owns or manages a maize plot in consumption group) save(indicator_results)

asdoc sum woman_own woman_man if gender == 0, stat(N mean sd min max) dec(3) tzok title(A woman individually owns or manages a maize plot in male headed hhs) save(indicator_results)

asdoc sum woman_own woman_man if gender == 1, stat(N mean sd min max) dec(3) tzok title(A woman individually owns or manages a maize plot in female headed hhs) save(indicator_results)

// Size of maize plot individually owned or managed by a woman
sum plot_size_ha
gen plot_size_woman_own = plot_size_ha if woman_own == 1
gen plot_size_woman_man = plot_size_ha if woman_man == 1
sum plot_size_woman_own plot_size_woman_man

asdoc sum plot_size_woman_own plot_size_woman_man, stat(N mean sd min max) dec(3) tzok title(Size of maize plot individually owned or managed by a woman) save(indicator_results)

asdoc sum plot_size_woman_own plot_size_woman_man if cont == 1, stat(N mean sd min max) dec(3) tzok title(Size of maize plot individually owned or managed by a woman in control group) save(indicator_results)

asdoc sum plot_size_woman_own plot_size_woman_man if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(Size of maize plot individually owned or managed by a woman in trial pack group) save(indicator_results)

asdoc sum plot_size_woman_own plot_size_woman_man if cons == 1, stat(N mean sd min max) dec(3) tzok title(Size of maize plot individually owned or managed by a woman in consumption group) save(indicator_results)

asdoc sum plot_size_woman_own plot_size_woman_man if gender == 0, stat(N mean sd min max) dec(3) tzok title(Size of maize plot individually owned or managed by a woman in male headed hhs) save(indicator_results)

asdoc sum plot_size_woman_own plot_size_woman_man if gender == 1, stat(N mean sd min max) dec(3) tzok title(Size of maize plot individually owned or managed by a woman in female headed hhs) save(indicator_results)

// A female co-head individually decides on the maize seed variety to be planted
tab who1
gen woman_decides_Var = who1
recode woman_decides_Var (1=1) (nonmissing=0)
tab woman_decides_Var

// A female co-head individually decides on what happens to the maize harvested
tab who2
gen woman_decides_harv = who2
recode woman_decides_harv (1=1) (nonmissing=0)
tab woman_decides_harv


asdoc sum woman_decides_Var woman_decides_harv, stat(N mean sd min max) dec(3) tzok title(A female co-head individually decides on variety and harvest) save(indicator_results)

asdoc sum woman_decides_Var woman_decides_harv if cont == 1, stat(N mean sd min max) dec(3) tzok title(A female co-head individually decides on variety and harvest in control group) save(indicator_results)

asdoc sum woman_decides_Var woman_decides_harv if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(A female co-head individually decides on variety and harvest in trial pack group) save(indicator_results)

asdoc sum woman_decides_Var woman_decides_harv if cons == 1, stat(N mean sd min max) dec(3) tzok title(A female co-head individually decides on variety and harvest in consumption group) save(indicator_results)

asdoc sum woman_decides_Var woman_decides_harv if gender == 0, stat(N mean sd min max) dec(3) tzok title(A female co-head individually decides on variety and harvest in male headed hhs) save(indicator_results)

asdoc sum woman_decides_Var woman_decides_harv if gender == 1, stat(N mean sd min max) dec(3) tzok title(A female co-head individually decides on variety and harvest in female headed hhs) save(indicator_results)



// Percentage of days female co-heads spend on different maize activities
sum grp1fld_prep grp1m_prep grp1f_prep grp1mem_prep

replace grp1fld_prep = . if grp1fld_prep == 999 // field preparation
replace grp1f_prep = . if grp1f_prep == 999

replace grp2plant_dys = . if grp2plant_dys == 999 // planting
replace grp2f_plt = . if grp2f_plt == 999

replace grp3wd_dys = . if grp3wd_dys == 999 // weeding
replace grp3f_wd = . if grp3f_wd == 999

replace grp4frt_dys = . if grp4frt_dys == 999 // fertilizer application
replace grp4f_frt = . if grp4f_frt == 999

replace grp5sp_dys = . if grp5sp_dys == 999 // spraying pesticides
replace grp5f_sp = . if grp5f_sp == 999

replace grp6har_dys = . if grp6har_dys == 999 // harvesting
replace grp6f_har = . if grp6f_har == 999

replace grp7dry_dys = . if grp7dry_dys == 999 // drying
replace grp7f_dry = . if grp7f_dry == 999

replace grp8thr_dys = . if grp8thr_dys == 999 // threshing
replace grp8f_thr = . if grp8f_thr == 999

replace grp10mkt_dys = . if grp10mkt_dys == 999 // marketing
replace grp10f_mkt = . if grp10f_mkt == 999

sum grp1fld_prep grp1f_prep grp2plant_dys grp2f_plt grp3wd_dys grp3f_wd grp4frt_dys grp4f_frt grp5sp_dys grp5f_sp grp6har_dys grp6f_har grp7dry_dys grp7f_dry grp8thr_dys grp8f_thr grp10mkt_dys grp10f_mkt

// generating variables for % of women days
gen woman_days_f = (grp1f_prep/grp1fld_prep)*100
gen woman_days_p = (grp2f_plt/grp2plant_dys)*100
gen woman_days_w = (grp3f_wd/grp3wd_dys)*100
gen woman_days_ft = (grp4f_frt/grp4frt_dys)*100
gen woman_days_s = (grp5f_sp/grp5sp_dys)*100
gen woman_days_h = (grp6f_har/grp6har_dys)*100
gen woman_days_d = (grp7f_dry/grp7dry_dys)*100
gen woman_days_th = (grp8f_thr/grp8thr_dys)*100
gen woman_days_m = (grp10f_mkt/grp10mkt_dys)*100

sum woman_days_f woman_days_p woman_days_w woman_days_ft woman_days_s woman_days_h woman_days_d woman_days_th woman_days_m


asdoc sum woman_days_f woman_days_p woman_days_w woman_days_ft woman_days_s woman_days_h woman_days_d woman_days_th woman_days_m, stat(N mean sd min max) dec(3) tzok title(% of woman days as a share of total activity days) save(indicator_results)

asdoc sum woman_days_f woman_days_p woman_days_w woman_days_ft woman_days_s woman_days_h woman_days_d woman_days_th woman_days_m if cont == 1, stat(N mean sd min max) dec(3) tzok title(% of woman days as a share of total activity days in control group) save(indicator_results)

asdoc sum woman_days_f woman_days_p woman_days_w woman_days_ft woman_days_s woman_days_h woman_days_d woman_days_th woman_days_m if trial_pack == 1, stat(N mean sd min max) dec(3) tzok title(% of woman days as a share of total activity days in trial pack group) save(indicator_results)

asdoc sum woman_days_f woman_days_p woman_days_w woman_days_ft woman_days_s woman_days_h woman_days_d woman_days_th woman_days_m if cons == 1, stat(N mean sd min max) dec(3) tzok title(% of woman days as a share of total activity days in consumption group) save(indicator_results)

asdoc sum woman_days_f woman_days_p woman_days_w woman_days_ft woman_days_s woman_days_h woman_days_d woman_days_th woman_days_m if gender == 0, stat(N mean sd min max) dec(3) tzok title(% of woman days as a share of total activity days in male headed hhs) save(indicator_results)

asdoc sum woman_days_f woman_days_p woman_days_w woman_days_ft woman_days_s woman_days_h woman_days_d woman_days_th woman_days_m if gender == 1, stat(N mean sd min max) dec(3) tzok title(% of woman days as a share of total activity days in female headed hhs) save(indicator_results)


