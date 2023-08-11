clear

cd "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG\midline\consumption_treatment\data\public"

import delimited "cons_intervention.csv", clear varnames(1)

***** Retaining only variable related to the sex of the persons that attended the cooking demos, plus variables related to the votes for the traits

keep today grdetails_count grdetails1sp_gen grdetails1rep_gen grdetails1gender2 grdetails2sp_gen grdetails2rep_gen grdetails2gender2 grdetails3sp_gen grdetails3rep_gen grdetails3gender2 grdetails4sp_gen grdetails4rep_gen grdetails4gender2 grdetails5sp_gen grdetails5rep_gen grdetails5gender2 grdetails6sp_gen grdetails6rep_gen grdetails6gender2 grdetails7sp_gen grdetails7rep_gen grdetails7gender2 grdetails8sp_gen grdetails8rep_gen grdetails8gender2 grdetails9sp_gen grdetails9rep_gen grdetails9gender2 grdetails10sp_gen grdetails10rep_gen grdetails10gender2 grdetails1respondent_gender grdetails2respondent_gender grdetails3respondent_gender grdetails4respondent_gender grdetails5respondent_gender grdetails6respondent_gender grdetails7respondent_gender grdetails8respondent_gender grdetails9respondent_gender grdetails10respondent_gender x before_cookgrow - plant

********* changing gender-related variables to numeric in order to generate the total number of men and women that attended the demos
replace grdetails1sp_gen = "1" if grdetails1sp_gen == "Female"
replace grdetails1rep_gen = "1" if grdetails1rep_gen == "Female" 
replace grdetails1gender2 = "1" if grdetails1gender2 == "Female"
replace grdetails2sp_gen = "1" if grdetails2sp_gen == "Female"
replace grdetails2rep_gen = "1" if grdetails2rep_gen == "Female"
replace grdetails2gender2 = "1" if grdetails2gender2 == "Female"
replace grdetails3sp_gen = "1" if grdetails3sp_gen == "Female"
replace grdetails3rep_gen = "1" if grdetails3rep_gen == "Female"
replace grdetails3gender2 = "1" if grdetails3gender2 == "Female"
replace grdetails4sp_gen = "1" if grdetails4sp_gen == "Female"
replace grdetails4rep_gen = "1" if grdetails4rep_gen == "Female"
replace grdetails4gender2 = "1" if grdetails4gender2 == "Female"
replace grdetails5sp_gen = "1" if grdetails5sp_gen == "Female"
replace grdetails5rep_gen = "1" if grdetails5rep_gen == "Female"
replace grdetails5gender2 = "1" if grdetails5gender2 == "Female"
replace grdetails6sp_gen = "1" if grdetails6sp_gen == "Female"
replace grdetails6rep_gen = "1" if grdetails6rep_gen == "Female"
replace grdetails6gender2 = "1" if grdetails6gender2 == "Female"
replace grdetails7sp_gen = "1" if grdetails7sp_gen == "Female"
replace grdetails7rep_gen = "1" if grdetails7rep_gen == "Female"
replace grdetails7gender2 = "1" if grdetails7gender2 == "Female"
replace grdetails8sp_gen = "1" if grdetails8sp_gen == "Female"
replace grdetails8rep_gen = "1" if grdetails8rep_gen == "Female"
replace grdetails8gender2 = "1" if grdetails8gender2 == "Female"
replace grdetails9sp_gen = "1" if grdetails9sp_gen == "Female"
replace grdetails9rep_gen = "1" if grdetails9rep_gen == "Female"
replace grdetails9gender2 = "1" if grdetails9gender2 == "Female"
replace grdetails10sp_gen = "1" if grdetails10sp_gen == "Female"
replace grdetails10rep_gen = "1" if grdetails10rep_gen == "Female"
replace grdetails10gender2 = "1" if grdetails10gender2 == "Female"
replace grdetails1respondent_gender = "1" if grdetails1respondent_gender == "Female"
replace grdetails2respondent_gender = "1" if grdetails2respondent_gender == "Female"
replace grdetails3respondent_gender = "1" if grdetails3respondent_gender == "Female"
replace grdetails4respondent_gender = "1" if grdetails4respondent_gender == "Female"
replace grdetails5respondent_gender = "1" if grdetails5respondent_gender == "Female"
replace grdetails6respondent_gender = "1" if grdetails6respondent_gender == "Female"
replace grdetails7respondent_gender = "1" if grdetails7respondent_gender == "Female"
replace grdetails8respondent_gender = "1" if grdetails8respondent_gender == "Female"
replace grdetails9respondent_gender = "1" if grdetails9respondent_gender == "Female"
replace grdetails10respondent_gender = "1" if grdetails10respondent_gender == "Female"

replace grdetails1sp_gen = "0" if grdetails1sp_gen == "Male"
replace grdetails1rep_gen = "0" if grdetails1rep_gen == "Male" 
replace grdetails1gender2 = "0" if grdetails1gender2 == "Male"
replace grdetails2sp_gen = "0" if grdetails2sp_gen == "Male"
replace grdetails2rep_gen = "0" if grdetails2rep_gen == "Male"
replace grdetails2gender2 = "0" if grdetails2gender2 == "Male"
replace grdetails3sp_gen = "0" if grdetails3sp_gen == "Male"
replace grdetails3rep_gen = "0" if grdetails3rep_gen == "Male"
replace grdetails3gender2 = "0" if grdetails3gender2 == "Male"
replace grdetails4sp_gen = "0" if grdetails4sp_gen == "Male"
replace grdetails4rep_gen = "0" if grdetails4rep_gen == "Male"
replace grdetails4gender2 = "0" if grdetails4gender2 == "Male"
replace grdetails5sp_gen = "0" if grdetails5sp_gen == "Male"
replace grdetails5rep_gen = "0" if grdetails5rep_gen == "Male"
replace grdetails5gender2 = "0" if grdetails5gender2 == "Male"
replace grdetails6sp_gen = "0" if grdetails6sp_gen == "Male"
replace grdetails6rep_gen = "0" if grdetails6rep_gen == "Male"
replace grdetails6gender2 = "0" if grdetails6gender2 == "Male"
replace grdetails7sp_gen = "0" if grdetails7sp_gen == "Male"
replace grdetails7rep_gen = "0" if grdetails7rep_gen == "Male"
replace grdetails7gender2 = "0" if grdetails7gender2 == "Male"
replace grdetails8sp_gen = "0" if grdetails8sp_gen == "Male"
replace grdetails8rep_gen = "0" if grdetails8rep_gen == "Male"
replace grdetails8gender2 = "0" if grdetails8gender2 == "Male"
replace grdetails9sp_gen = "0" if grdetails9sp_gen == "Male"
replace grdetails9rep_gen = "0" if grdetails9rep_gen == "Male"
replace grdetails9gender2 = "0" if grdetails9gender2 == "Male"
replace grdetails10sp_gen = "0" if grdetails10sp_gen == "Male"
replace grdetails10rep_gen = "0" if grdetails10rep_gen == "Male"
replace grdetails10gender2 = "0" if grdetails10gender2 == "Male"
replace grdetails1respondent_gender = "0" if grdetails1respondent_gender == "Male"
replace grdetails2respondent_gender = "0" if grdetails2respondent_gender == "Male"
replace grdetails3respondent_gender = "0" if grdetails3respondent_gender == "Male"
replace grdetails4respondent_gender = "0" if grdetails4respondent_gender == "Male"
replace grdetails5respondent_gender = "0" if grdetails5respondent_gender == "Male"
replace grdetails6respondent_gender = "0" if grdetails6respondent_gender == "Male"
replace grdetails7respondent_gender = "0" if grdetails7respondent_gender == "Male"
replace grdetails8respondent_gender = "0" if grdetails8respondent_gender == "Male"
replace grdetails9respondent_gender = "0" if grdetails9respondent_gender == "Male"
replace grdetails10respondent_gender = "0" if grdetails10respondent_gender == "Male"

order grdetails1respondent_gender- grdetails10respondent_gender, before ( grdetails_count )
destring grdetails1respondent_gender- grdetails10respondent_gender , replace ignore ("NA")
destring grdetails1sp_gen - grdetails10gender2 , replace ignore ("n/a")

order grdetails_count, after (today)

***** total of women participants that attended the cooking and tasting demos
egen women = rowtotal(grdetails1respondent_gender - grdetails10gender2)

recode grdetails1respondent_gender - grdetails10gender2 (1=2) (0=1)
recode grdetails1respondent_gender - grdetails10gender2 (2=0) (1=1)

***** total of men participants that attended the cooking and tasting demos
egen men = rowtotal(grdetails1respondent_gender - grdetails10gender2)

********** binary variable for gender, where 1= a demo was attended by more women (women/men>1), 0 otherwise
gen moreWomen = .
replace moreWomen = 1 if women/men > 1
replace moreWomen = 2 if moreWomen == .
recode moreWomen (1=1) (2=2)
label define sex 1 "Women dominated demos" 2 "Non-women dominated demos"
label values moreWomen sex
tab moreWomen


gen women2 = .
replace women2 = women if moreWomen == 2 // number of women in non-women dominated demos
la var women2 "No. of women in non-women dominated demos"

// total number of people that attended the cooking demos (men+women)
gen attended = women + men
mean attended, over (moreWomen) // mean number of participants in demos that had majority of women vs demos with women that were less or equal to men

gen men_prop = men/attended // mean proportion of men participants in all the demos
gen women_prop = women/attended // mean proportion of women participants in all the demos

order women men moreWomen women2 attended men_prop women_prop, before ( before_cookgrow)



**** GENERATING PROPOTION OF VOTES (as a share of total votes) FOR DIFFERENT ATTRIBUTES BEFORE COOKING AND TASTING ***

// Number of participants that indicated that the grow improved maize for consumption as a share of total votes
gen grow_imp_prior = before_cookgrow/( before_cookgrow + plant) 

// Proportion of votes for color before cooking and tasting
gen color_loc_prior = before_cookcolor1color1_loc/( before_cookcolor1color1_loc+ before_cookcolor1color1_lmpr+ before_cookcolor1same_cl1)
gen color_imp_prior = before_cookcolor1color1_lmpr /( before_cookcolor1color1_loc+ before_cookcolor1color1_lmpr+ before_cookcolor1same_cl1)
gen color_same_prior = before_cookcolor1same_cl1 /( before_cookcolor1color1_loc+ before_cookcolor1color1_lmpr+ before_cookcolor1same_cl1)
sum color_loc_prior color_imp_prior color_same_prior

// Proportion of votes for aroma of the posho before cooking and tasting
gen before_aroma1_loc = (before_cookaromayaroma1_loc_c + before_cookaromaxaroma1_loc_e)/2
gen before_aroma1_imp = ( before_cookaromayaroma1_lmpr_c + before_cookaromaxaroma1_lmpr_e )/2
gen before_aroma1_same = ( before_cookaromaysame_ar1 + before_cookaromaxsame_exp1 )/2
order before_aroma1_loc before_aroma1_imp before_aroma1_same, before ( before_cookaromayaroma1_loc_c )

gen aroma_loc_prior = before_aroma1_loc/( before_aroma1_loc+ before_aroma1_imp+ before_aroma1_same)
gen aroma_imp_prior = before_aroma1_imp/( before_aroma1_loc+ before_aroma1_imp+ before_aroma1_same)
gen aroma_same_prior = before_aroma1_same/( before_aroma1_loc+ before_aroma1_imp+ before_aroma1_same)
sum aroma_loc_prior aroma_imp_prior aroma_same_prior

// Proportion of votes for ease of cooking before cooking and tasting
gen ease_loc_prior = before_cookeas1eas1_loc/( before_cookeas1eas1_loc+ before_cookeas1eas1_lmpr+ before_cookeas1same_eas1)
gen ease_imp_prior = before_cookeas1eas1_lmpr /( before_cookeas1eas1_loc+ before_cookeas1eas1_lmpr+ before_cookeas1same_eas1)
gen ease_same_prior = before_cookeas1same_eas1 /( before_cookeas1eas1_loc+ before_cookeas1eas1_lmpr+ before_cookeas1same_eas1)
sum ease_loc_prior ease_imp_prior ease_same_prior

// Proportion of votes for cooking time before cooking and tasting
gen cooktime_loc_prior = before_cooktime1time1_loc/(before_cooktime1time1_loc + before_cooktime1time1_impr + before_cooktime1same_tm1)
gen cooktime_imp_prior = before_cooktime1time1_impr /(before_cooktime1time1_loc + before_cooktime1time1_impr + before_cooktime1same_tm1)
gen cooktime_same_prior = before_cooktime1same_tm1 /(before_cooktime1time1_loc + before_cooktime1time1_impr + before_cooktime1same_tm1)
sum cooktime_loc_prior cooktime_imp_prior cooktime_same_prior

// Proportion of votes for flour expansion before cooking and tasting
gen exp_loc_prior = before_cookexp1exp1_loc/(before_cookexp1exp1_loc + before_cookexp1exp1_lmpr + before_cookexp1same_exp1)
gen exp_imp_prior = before_cookexp1exp1_lmpr /(before_cookexp1exp1_loc + before_cookexp1exp1_lmpr + before_cookexp1same_exp1)
gen exp_same_prior = before_cookexp1same_exp1 /(before_cookexp1exp1_loc + before_cookexp1exp1_lmpr + before_cookexp1same_exp1)
sum exp_loc_prior exp_imp_prior exp_same_prior

// Proportion of votes for cooking time before cooking and tasting
gen taste_loc_prior = before_cooktaste1taste1_loc/(before_cooktaste1taste1_loc + before_cooktaste1taste1_impr + before_cooktaste1same_tast1)
gen taste_imp_prior = before_cooktaste1taste1_impr /(before_cooktaste1taste1_loc + before_cooktaste1taste1_impr + before_cooktaste1same_tast1)
gen taste_same_prior = before_cooktaste1same_tast1 /(before_cooktaste1taste1_loc + before_cooktaste1taste1_impr + before_cooktaste1same_tast1)
sum taste_loc_prior taste_imp_prior taste_same_prior

// Proportion of votes for texture of posho before cooking and tasting
gen texture_loc_prior = before_cooktext1text1_loc/( before_cooktext1text1_loc + before_cooktext1text1_lmpr + before_cooktext1same_txt1)
gen texture_imp_prior = before_cooktext1text1_lmpr /( before_cooktext1text1_loc + before_cooktext1text1_lmpr + before_cooktext1same_txt1)
gen texture_same_prior = before_cooktext1same_txt1 /( before_cooktext1text1_loc + before_cooktext1text1_lmpr + before_cooktext1same_txt1)
sum texture_loc_prior texture_imp_prior texture_same_prior

order x grow_imp_prior color_loc_prior- texture_same_prior, after ( before_cookgrow )

**** PROPOTION OF VOTES FOR DIFFERENT ATTRIBUTES AFTER COOKING AND TASTING ***

// proportion of votes for participants planning to grow improved maize
gen grow_imp_post = plant/( before_cookgrow + plant)

// Proportion of votes for color after cooking and tasting
gen color_loc_post = after_cookingcolor2color2_loc/(after_cookingcolor2color2_loc + after_cookingcolor2color2_lmpr + after_cookingcolor2same_cl2)
gen color_imp_post = after_cookingcolor2color2_lmpr /(after_cookingcolor2color2_loc + after_cookingcolor2color2_lmpr + after_cookingcolor2same_cl2)
gen color_same_post = after_cookingcolor2same_cl2 /(after_cookingcolor2color2_loc + after_cookingcolor2color2_lmpr + after_cookingcolor2same_cl2)
sum color_loc_post color_imp_post color_same_post

// Proportion of votes for aroma of the posho after cooking and tasting
gen after_aroma1_loc = ( after_cookingaromamaroma2_loc_c+ after_cookingaroma2aroma2_loc_e)/2
gen after_aroma1_imp = ( after_cookingaromamaroma2_lmpr_c + after_cookingaroma2aroma2_lmpr_e )/2
gen after_aroma1_same = ( after_cookingaromamsame_ar2 + after_cookingaroma2same_exp2 )/2

order after_aroma1_loc after_aroma1_imp after_aroma1_same, before (after_cookingaromamaroma2_loc_c)

gen aroma_loc_post = after_aroma1_loc/( after_aroma1_loc+ after_aroma1_imp + after_aroma1_same)
gen aroma_imp_post = after_aroma1_imp/( after_aroma1_loc+ after_aroma1_imp + after_aroma1_same)
gen aroma_same_post = after_aroma1_same/( after_aroma1_loc+ after_aroma1_imp + after_aroma1_same)
sum aroma_loc_post aroma_imp_post aroma_same_post

// Proportion of votes for ease of cooking after cooking and tasting
gen ease_loc_post = after_cookingeas2eas2_loc/(after_cookingeas2eas2_loc + after_cookingeas2eas2_lmpr + after_cookingeas2same_eas2)
gen ease_imp_post = after_cookingeas2eas2_lmpr /(after_cookingeas2eas2_loc + after_cookingeas2eas2_lmpr + after_cookingeas2same_eas2)
gen ease_same_post = after_cookingeas2same_eas2 /(after_cookingeas2eas2_loc + after_cookingeas2eas2_lmpr + after_cookingeas2same_eas2)
sum ease_loc_post ease_imp_post ease_same_post

// Proportion of votes for cooking time after cooking and tasting
gen cooktime_loc_post = after_cookingtime2time2_loc/(after_cookingtime2time2_loc + after_cookingtime2time2_impr + after_cookingtime2same_tm2)
gen cooktime_imp_post = after_cookingtime2time2_impr/(after_cookingtime2time2_loc + after_cookingtime2time2_impr + after_cookingtime2same_tm2)
gen cooktime_same_post = after_cookingtime2same_tm2/(after_cookingtime2time2_loc + after_cookingtime2time2_impr + after_cookingtime2same_tm2)
sum cooktime_loc_post cooktime_imp_post cooktime_same_post

// Proportion of votes for flour expansion after cooking and tasting
gen exp_loc_post = after_cookingexp2exp2_loc/(after_cookingexp2exp2_loc + after_cookingexp2exp2_lmpr + after_cookingexp2same_exp2)
gen exp_imp_post = after_cookingexp2exp2_lmpr/(after_cookingexp2exp2_loc + after_cookingexp2exp2_lmpr + after_cookingexp2same_exp2)
gen exp_same_post = after_cookingexp2same_exp2/(after_cookingexp2exp2_loc + after_cookingexp2exp2_lmpr + after_cookingexp2same_exp2)
sum exp_loc_post exp_imp_post exp_same_post

// Proportion of votes for cooking time after cooking and tasting
gen taste_loc_post = after_cookingtaste2taste2_loc/(after_cookingtaste2taste2_loc + after_cookingtaste2taste2_impr + after_cookingtaste2same_tast2)
gen taste_imp_post = after_cookingtaste2taste2_impr/(after_cookingtaste2taste2_loc + after_cookingtaste2taste2_impr + after_cookingtaste2same_tast2)
gen taste_same_post = after_cookingtaste2same_tast2/(after_cookingtaste2taste2_loc + after_cookingtaste2taste2_impr + after_cookingtaste2same_tast2)
sum taste_loc_post taste_imp_post taste_same_post

// Proportion of votes for texture of posho after cooking and tasting
gen texture_loc_post = after_cookingtext2text2_loc/(after_cookingtext2text2_loc + after_cookingtext2text2_lmpr + after_cookingtext2same_txt2)
gen texture_imp_post = after_cookingtext2text2_lmpr/(after_cookingtext2text2_loc + after_cookingtext2text2_lmpr + after_cookingtext2same_txt2)
gen texture_same_post = after_cookingtext2same_txt2/(after_cookingtext2text2_loc + after_cookingtext2text2_lmpr + after_cookingtext2same_txt2)
sum texture_loc_post texture_imp_post texture_same_post

order grow_imp_prior color_loc_post- texture_same_post, after (texture_same_prior)

rename x trial_pack

la var attended "Number of participants"
la var women "Number of women participants"
la var men "Number of men participants"
la var women_prop "Prop. of women participants"
la var men_prop "Prop. of men participants"
la var moreWomen "Prop. of women dominated demos"

la var color_imp_prior "Has a better color"
la var aroma_imp_prior "Has better aroma"
la var ease_imp_prior "Is easier to cook"
la var cooktime_imp_prior "Cooks faster"
la var exp_imp_prior "Expands more when cooked"
la var taste_imp_prior "Tastes better"
la var texture_imp_prior "Has better texture"

la var color_loc_prior "Has a better color"
la var aroma_loc_prior "Has better aroma"
la var ease_loc_prior "Is easier to cook"
la var cooktime_loc_prior "Cooks faster"
la var exp_loc_prior "Expands more when cooked"
la var taste_loc_prior "Tastes better"
la var texture_loc_prior "Has better texture"


***************************************************************************************************************


******* SUMMARY STATISTCS ***************

cd "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG\midline\consumption_treatment\papers"

gen moreWomen2 = moreWomen
recode moreWomen2 (1=1) (2=0)
tab moreWomen2
label define sex2 0 "Non-women dominated demos" 1 "Women dominated demos"
la var moreWomen2 "Prop. of women dominated demos"


// summary statistics about the participants and cooking demo clusters
asdoc sum attended women men women_prop moreWomen2 women2, stat(sum mean sd min max N) label dec(2) dec(2) tzok title(Summary statistics for participants) save(Results_Tables) replace

// ttest difference of number of women between women and non-women dominated demos
asdoc ttest women, by(moreWomen) stat(obs mean dif p) label dec(2) tzok title(ttest mean difference in number of women in non-women dominated demos)


****** PERCEPTIONS before cooking and tasting demonstration--- Proportion of votes

// overall votes
egen priorTotLOC = rowmean( color_loc_prior aroma_loc_prior ease_loc_prior cooktime_loc_prior exp_loc_prior taste_loc_prior texture_loc_prior)
egen priorTotIMP = rowmean( color_imp_prior aroma_imp_prior ease_imp_prior cooktime_imp_prior exp_imp_prior taste_imp_prior texture_imp_prior)

// perceptions for specific attributes
gen colorPriorLOC = color_loc_prior
gen aromPriorLOC = aroma_loc_prior
gen easePriorLOC = ease_loc_prior
gen timePriorLOC = cooktime_loc_prior
gen expPriorLOC = exp_loc_prior
gen tastePriorLOC = taste_loc_prior
gen textPriorLOC = texture_loc_prior

gen colorPriorIMP = color_imp_prior
gen aromPriorIMP = aroma_imp_prior
gen easePriorIMP = ease_imp_prior
gen timePriorIMP = cooktime_imp_prior
gen expPriorIMP = exp_imp_prior
gen tastePriorIMP = taste_imp_prior
gen textPriorIMP = texture_imp_prior


// Before cooking and tasting--- ttest mean differences in votes for local and improved varieties
asdoc ttest priorTotLOC==priorTotIMP, label dec(3) tzok title(Before cooking and tasting demonstration: ttests mean differences in votes for improved maize and local varieties) // overall voting--- all traits

asdoc ttest priorTotLOC==priorTotIMP if moreWomen == 1, label dec(3) tzok title(Before cooking and tasting demonstration: ttests mean differences in votes for improved maize and local varieties) rowappend // overall voting--- all traits--- in women-dominated demos

asdoc ttest priorTotLOC==priorTotIMP if moreWomen == 2, label dec(3) tzok title(Before cooking and tasting demonstration: ttests mean differences in votes for improved maize and local varieties) rowappend // overall voting--- all traits--- in non-women-dominated demos

asdoc ttest colorPriorLOC==colorPriorIMP, label dec(3) tzok title(Before cooking and tasting demonstration: ttests mean differences in votes for improved maize and local varieties) rowappend

asdoc ttest aromPriorLOC==aromPriorIMP, label dec(3) tzok title(Before cooking and tasting demonstration: ttests mean differences in votes for improved maize and local varieties) rowappend

asdoc ttest easePriorLOC==easePriorIMP, label dec(3) tzok title(Before cooking and tasting demonstration: ttests mean differences in votes for improved maize and local varieties) rowappend

asdoc ttest timePriorLOC==timePriorIMP, label dec(3) tzok title(Before cooking and tasting demonstration: ttests mean differences in votes for improved maize and local varieties) rowappend

asdoc ttest expPriorLOC==expPriorIMP, label dec(3) tzok title(Before cooking and tasting demonstration: ttests mean differences in votes for improved maize and local varieties) rowappend

asdoc ttest tastePriorLOC==tastePriorIMP, label dec(3) tzok title(Before cooking and tasting demonstration: ttests mean differences in votes for improved maize and local varieties) rowappend

asdoc ttest textPriorLOC==textPriorIMP, label dec(3) tzok title(Before cooking and tasting demonstration: ttests mean differences in votes for improved maize and local varieties) rowappend


// Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for local maize varieties

asdoc ttest priorTotLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for local maize varieties)

asdoc ttest colorPriorLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for local maize varieties) rowappend

asdoc ttest aromPriorLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for local maize varieties) rowappend

asdoc ttest easePriorLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for local maize varieties) rowappend

asdoc ttest timePriorLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for local maize varieties) rowappend

asdoc ttest expPriorLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for local maize varieties) rowappend

asdoc ttest tastePriorLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for local maize varieties) rowappend

asdoc ttest textPriorLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for local maize varieties) rowappend


// Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for improved maize varieties

asdoc ttest priorTotIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for improved maize varieties)

asdoc ttest colorPriorIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for improved maize varieties) rowappend

asdoc ttest aromPriorIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for improved maize varieties) rowappend

asdoc ttest easePriorIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for improved maize varieties) rowappend

asdoc ttest timePriorIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for improved maize varieties) rowappend

asdoc ttest expPriorIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for improved maize varieties) rowappend

asdoc ttest tastePriorIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for improved maize varieties) rowappend

asdoc ttest textPriorIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(Before cooking and tasting: Gender disaggregated ttest mean differences in perceptions/votes for improved maize varieties) rowappend



*** CHANGE IN PERCEPTIONS (percentage points in VOTES) AFTER COOKING AND TASTING---- overall
egen postTotLOC = rowmean(color_loc_post aroma_loc_post ease_loc_post cooktime_loc_post exp_loc_post taste_loc_post texture_loc_post)
egen postTotIMP = rowmean(color_imp_post aroma_imp_post ease_imp_post cooktime_imp_post exp_imp_post taste_imp_post texture_imp_post)

gen totalDiffLOC = (postTotLOC - priorTotLOC)*100
la var totalDiffLOC "total_change in percent points"

gen totalDiffIMP = (postTotIMP - priorTotIMP)*100
la var totalDiffIMP "total_change in percent points"


*** CHANGE IN PERCEPTIONS (percentage points in VOTES) AFTER COOKING AND TASTING---- for specific variables
gen colorDiffLOC = (color_loc_post - color_loc_prior)*100
la var colorDiffLOC "color_change in percent points"

gen aromDiffLOC = (aroma_loc_post - aroma_loc_prior)*100
la var aromDiffLOC "aroma_change in percent points"

gen easeDiffLOC = (ease_loc_post - ease_loc_prior)*100
la var easeDiffLOC "easycook_change in percent points"

gen timeDiffLOC = (cooktime_loc_post - cooktime_loc_prior)*100
la var timeDiffLOC "cooktime_change in percent points"

gen expDiffLOC = (exp_loc_post - exp_loc_prior)*100
la var expDiffLOC "expands_change in percent points"

gen tasteDiffLOC = (taste_loc_post - taste_loc_prior)*100
la var tasteDiffLOC "taste_change in percent points"

gen textDiffLOC = (texture_loc_post - texture_loc_prior)*100
la var textDiffLOC "texture_change in percent points"



gen colorDiffIMP = (color_imp_post - color_imp_prior)*100
la var colorDiffIMP "color_change in percent points"

gen aromDiffIMP = (aroma_imp_post - aroma_imp_prior)*100
la var aromDiffIMP "aroma_change in percent points"

gen easeDiffIMP = (ease_imp_post - ease_imp_prior)*100
la var easeDiffIMP "easycook_change in percent points"

gen timeDiffIMP = (cooktime_imp_post - cooktime_imp_prior)*100
la var timeDiffIMP "cooktime_change in percent points"

gen expDiffIMP = (exp_imp_post - exp_imp_prior)*100
la var expDiffIMP "expands_change in percent points"

gen tasteDiffIMP = (taste_imp_post - taste_imp_prior)*100
la var tasteDiffIMP "taste_change in percent points"

gen textDiffIMP = (texture_imp_post - texture_imp_prior)*100
la var textDiffIMP "texture_change in percent points"


*** After cooking and tasting: ttest mean differences in change of perceptions (percentage point changes in votes) between local and improved varieties

asdoc ttest totalDiffLOC==totalDiffIMP, label dec(3) tzok title(After cooking and testing: ttest mean differences in change of perceptions, measured by change in percentage points of votes) // change in overall votes

asdoc ttest totalDiffLOC==totalDiffIMP if moreWomen==1, label dec(3) tzok title(After cooking and testing: ttest mean differences in change of perceptions, measured by change in percentage points of votes) rowappend // change in overall votes in women-dominated demos

asdoc ttest totalDiffLOC==totalDiffIMP if moreWomen==2, label dec(3) tzok title(After cooking and testing: ttest mean differences in change of perceptions, measured by change in percentage points of votes) rowappend // change in overall votes in non-women-dominated demos

asdoc ttest colorDiffLOC==colorDiffIMP, label dec(3) tzok title(After cooking and testing: ttest mean differences in change of perceptions, measured by change in percentage points of votes) rowappend

asdoc ttest aromDiffLOC==aromDiffIMP, label dec(3) tzok title(After cooking and testing: ttest mean differences in change of perceptions, measured by change in percentage points of votes) rowappend

asdoc ttest easeDiffLOC==easeDiffIMP, label dec(3) tzok title(After cooking and testing: ttest mean differences in change of perceptions, measured by change in percentage points of votes) rowappend

asdoc ttest timeDiffLOC==timeDiffIMP, label dec(3) tzok title(After cooking and testing: ttest mean differences in change of perceptions, measured by change in percentage points of votes) rowappend

asdoc ttest expDiffLOC==expDiffIMP, label dec(3) tzok title(After cooking and testing: ttest mean differences in change of perceptions, measured by change in percentage points of votes) rowappend

asdoc ttest tasteDiffLOC==tasteDiffIMP, label dec(3) tzok title(After cooking and testing: ttest mean differences in change of perceptions, measured by change in percentage points of votes) rowappend

asdoc ttest textDiffLOC==textDiffIMP, label dec(3) tzok title(After cooking and testing: ttest mean differences in change of perceptions, measured by change in percentage points of votes) rowappend


// After cooking and tasting: Gender disaggregated ttest mean differences in change of perceptions (percentage point changes in votes) for LOCAL 
asdoc ttest totalDiffLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for local varieties) 

asdoc ttest colorDiffLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for local varieties) rowappend

asdoc ttest aromDiffLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for local varieties) rowappend

asdoc ttest easeDiffLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for local varieties) rowappend

asdoc ttest timeDiffLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for local varieties) rowappend

asdoc ttest expDiffLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for local varieties) rowappend

asdoc ttest tasteDiffLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for local varieties) rowappend

asdoc ttest textDiffLOC, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for local varieties) rowappend


// // After cooking and tasting: Gender disaggregated ttest mean differences in change of perceptions (percentage point changes in votes) for IMPROVED 

asdoc ttest totalDiffIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for improved varieties)

asdoc ttest colorDiffIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for improved varieties) rowappend

asdoc ttest aromDiffIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(AAfter cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for improved varieties) rowappend

asdoc ttest easeDiffIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for improved varieties) rowappend

asdoc ttest timeDiffIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for improved varieties) rowappend

asdoc ttest expDiffIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for improved varieties) rowappend

asdoc ttest tasteDiffIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for improved varieties) rowappend

asdoc ttest textDiffIMP, by(moreWomen) stat(obs mean p) label dec(3) tzok title(After cooking and tasting: ttest mean differences for change/percent point in perceptions/votes by sex_ratio, for improved varieties) rowappend

