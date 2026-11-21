## R2-4 (referee report, Food Policy R&R): gender component.
##
## Referee asks for four things:
##   (a) baseline picture: female-headed households, women's say in seed
##       choice, who does the cooking;
##   (b) who actually participated in each treatment (trial pack, cooking demo);
##   (c) heterogeneity of demo effects by whether the cook / a woman attended
##       (explicitly non-causal, since who attends is not randomly assigned);
##   (d) a caveat that perception/belief outcomes are the interviewed
##       respondent's, not necessarily the cook's.
##
## Data note up front: there is NO baseline or endline variable that asks
## "who in the household usually does the cooking" as a household
## characteristic. We searched the baseline and endline XLSForm questionnaires
## (Baseline_farmer_public.xlsx, Trial_Endline_Survey_public.xlsx) for
## cooking-role questions; the only cooking-related items are (i) a
## PLOT-level seed-quality rating item literally named "cook" ("Please rate
## this seed in terms of ease of cooking") -- a seed attribute rating, not a
## household-roster variable -- and (ii) two post-hoc questions asked only of
## cooking-demo households about who cooked/received the take-home flour
## sample after the session (groupcook.cons_who_cooked / cons_who_received).
## We use (ii) below as an event-specific proxy but flag plainly that it is
## NOT a general "who normally cooks in this household" measure.
##
## Run from the repository root (mippi_UG/).

suppressMessages({
  library(clubSandwich)
  library(dplyr)
  library(andersonTools)
})

ANADIR <- "papers/increasing_seed_varietal_turnover/analysis"
STOP_LINE <- 630

## ---- build dta exactly as in the paper (sourced construction) -------------
src <- readLines(file.path(ANADIR, "analysis.R"))[1:STOP_LINE]
src <- gsub("cons_intervention_merged2.csv", "cons_intervention_merged.csv", src, fixed = TRUE)
src <- src[!grepl("^\\s*library\\(", src)]
e <- new.env()
e$path <- normalizePath(ANADIR)
suppressWarnings(suppressMessages(
  eval(parse(text = paste(src, collapse = "\n")), envir = e)
))
dta <- e$dta
bse <- e$bse
cat("constructed dta rows:", nrow(dta), "| bse rows:", nrow(bse), "\n\n")

## =============================================================================
## (a) Baseline gender picture
## =============================================================================
cat("=========== (a) Baseline gender picture (N =", nrow(bse), ") ===========\n")

cat(sprintf("Female-headed households: %.1f%% (N=%d)\n",
            100 * mean(bse$gender == "Female", na.rm = TRUE), sum(!is.na(bse$gender))))

## who1 = who decided the seed variety planted on the (baseline) randomly
## selected plot; who2 = who decided what happened to the harvested crop.
## Coding (baseline questionnaire, list q34a): 1=female alone, 2=male alone,
## 3=female after consulting male, 4=male after consulting female, 96=other.
## "Woman involved" = 1, 3, or 4 (i.e., not "male alone, no consultation").
cat("\n--- Who decided the seed variety on the (baseline) selected plot (raw codes) ---\n")
print(table(bse$who1, useNA = "always"))
cat("1=female alone | 2=male alone | 3=female after consulting male |\n")
cat("4=male after consulting female | 96=other\n")

mk_woman_involved <- function(who) {
  x <- ifelse(who %in% c(1, 3, 4), TRUE, ifelse(who == 2, FALSE, NA))
  x
}
bse$woman_seed <- mk_woman_involved(bse$who1)
bse$woman_harv <- mk_woman_involved(bse$who2)

cat(sprintf("\nWoman involved in deciding the seed variety planted: %.1f%% (all hh, N=%d)\n",
            100 * mean(bse$woman_seed, na.rm = TRUE), sum(!is.na(bse$woman_seed))))
cat(sprintf("Woman involved in deciding what happened to the harvest: %.1f%% (all hh, N=%d)\n",
            100 * mean(bse$woman_harv, na.rm = TRUE), sum(!is.na(bse$woman_harv))))
mh <- bse$gender == "Male"
cat(sprintf("Same, restricted to male-headed households: seed = %.1f%%, harvest = %.1f%%\n",
            100 * mean(bse$woman_seed[mh], na.rm = TRUE), 100 * mean(bse$woman_harv[mh], na.rm = TRUE)))
cat("(High baseline involvement means little room for either intervention to move this margin --\n")
cat(" consistent with the paper's own null result on Table A13 / women co-head involvement.)\n")

cat("\n--- Who does the cooking (household characteristic) ---\n")
cat("NOT AVAILABLE in baseline or endline data. No question asks who is\n")
cat("responsible for food preparation/cooking in the household generally.\n")
cat("[BJORN: confirm this is correct, or point to a variable we missed --\n")
cat(" we searched both XLSForm questionnaires for 'cook', 'prepar', 'responsible for']\n")

## =============================================================================
## (b) Who participated in each treatment
## =============================================================================
cat("\n\n=========== (b) Participation, by treatment ===========\n")

cat("--- Trial pack: who within the household received / used it ---\n")
cat("(1 = respondent him/herself, 2 = spouse, 3 = someone else in the household)\n")
print(table(dta$Trial_group.TP_who_received, useNA = "always"))
print(table(dta$Trial_group.TP_who_used, useNA = "always"))
cat("Not directly gendered; approximate gender by combining with the respondent's own\n")
cat("gender (gender_resp) -- 'self' takes the respondent's gender, 'spouse' takes the\n")
cat("opposite gender under a two-co-head assumption, 'someone else' is left unknown.\n")
tp_gender <- ifelse(dta$Trial_group.TP_who_received == "1",
                     dta$gender_resp,
                     ifelse(dta$Trial_group.TP_who_received == "2",
                            ifelse(dta$gender_resp == "Female", "Male", "Female"), NA))
print(table(tp_gender, useNA = "always"))
cat(sprintf("Approx. share of pack recipients coded female under this proxy: %.1f%% (N=%d)\n",
            100 * mean(tp_gender == "Female", na.rm = TRUE), sum(!is.na(tp_gender))))

cat("\n--- Cooking demonstration: attendance ---\n")
cons <- read.csv(file.path(ANADIR, "../../..",
                            "midline/consumption_treatment/data/public/cons_intervention_merged.csv"),
                  stringsAsFactors = FALSE)
cons$attended <- cons$grDetails.attend == "Yes"
cons$sp_attended <- cons$grDetails.sp_att == "Yes"
cat(sprintf("Attendance rate among invited (demo-arm) households: %.1f%%\n",
            100 * mean(cons$attended, na.rm = TRUE)))
cat("Spouse/co-head also attended, among attendees:\n")
print(round(prop.table(table(cons$sp_attended[cons$attended %in% TRUE], useNA = "always")), 3))
cat("Gender of the accompanying spouse, when one attended:\n")
print(table(cons$grDetails.sp_gen[cons$sp_attended %in% TRUE], useNA = "always"))

cat("\n--- Cooking demonstration: who received / who cooked the take-home flour sample ---\n")
cat("(event-specific measure, NOT a general household cooking-role variable; see note above)\n")
cat("Q139 'Who within the household received the bag of maize flour?':\n")
print(table(dta$groupcook.cons_who_received, useNA = "always"))
cat("codes: 1=male co-head, 2=female co-head, 3=both equally, 4=someone else in hh, 5=outsider\n")
cat("\nQ141 'Who within the household cooked the meal?':\n")
print(table(dta$groupcook.cons_who_cooked, useNA = "always"))
recv_tab <- table(dta$groupcook.cons_who_cooked)
cat(sprintf("Female co-head cooked the take-home sample in %.1f%% of households reporting (N=%d)\n",
            100 * recv_tab["2"] / sum(recv_tab[c("1","2","3","4","5")]),
            sum(recv_tab[c("1","2","3","4","5")])))

## =============================================================================
## (c) Heterogeneity of demo effects by whether a woman attended/cooked
##     -- DESCRIPTIVE ONLY: who attends/who cooks is not randomly assigned,
##     so this is a selected-sample comparison, not a causal contrast.
## =============================================================================
cat("\n\n=========== (c) Heterogeneity by female participation (DESCRIPTIVE, non-causal) ===========\n")
cat("Who attends a session, and who cooks the take-home sample, is a household choice made\n")
cat("after assignment to the demo arm. Any association below reflects both the treatment and\n")
cat("whatever selected these households into having a woman attend/cook -- it is NOT an ITT\n")
cat("estimate and should not be read as evidence on whether reaching women 'causes' different\n")
cat("adoption responses.\n\n")

outcomes <- c(index = "index", anyBazooka = "p_outcome_2alt", freshBazooka = "p_outcome_2")

## moderator 1: female co-head cooked the take-home flour sample
d_cook <- dta[dta$cont %in% TRUE, ]
d_cook$female_cooked <- d_cook$groupcook.cons_who_cooked == "2"
for (nm in names(outcomes)) {
  y <- outcomes[nm]
  dd <- d_cook[!is.na(d_cook[[y]]) & !is.na(d_cook$female_cooked), ]
  if (nrow(dd) < 20 || length(unique(dd$female_cooked)) < 2) next
  m <- lm(as.formula(paste(y, "~ female_cooked")), data = dd)
  vc <- vcovCR(m, cluster = dd$cluster_ID, type = "CR2")
  ct <- as.data.frame(coef_test(m, vc))
  m0 <- mean(dd[[y]][!dd$female_cooked], na.rm = TRUE)
  m1 <- mean(dd[[y]][dd$female_cooked], na.rm = TRUE)
  cat(sprintf("  %-12s | not-female-cooked mean=%.3f | female-cooked mean=%.3f | diff=%6.3f (SE %.3f, p=%.3f) | N=%d\n",
              nm, m0, m1, ct[2, "beta"], ct[2, "SE"], ct[2, "p_Satt"], nrow(dd)))
}

## moderator 2: spouse/co-head also attended the session (both co-heads present)
cons_sub <- cons[cons$attended %in% TRUE, c("farmer_ID", "sp_attended", "grDetails.sp_gen")]
d2 <- merge(dta[dta$cont %in% TRUE, ], cons_sub, by.x = "ID", by.y = "farmer_ID")
d2$female_joined <- d2$sp_attended %in% TRUE & d2$grDetails.sp_gen == "Female"
cat("\n-- moderator: a female spouse/co-head also attended alongside the invited member --\n")
for (nm in names(outcomes)) {
  y <- outcomes[nm]
  dd <- d2[!is.na(d2[[y]]) & !is.na(d2$female_joined), ]
  if (nrow(dd) < 20 || length(unique(dd$female_joined)) < 2) next
  m <- lm(as.formula(paste(y, "~ female_joined")), data = dd)
  vc <- vcovCR(m, cluster = dd$cluster_ID, type = "CR2")
  ct <- as.data.frame(coef_test(m, vc))
  m0 <- mean(dd[[y]][!dd$female_joined], na.rm = TRUE)
  m1 <- mean(dd[[y]][dd$female_joined], na.rm = TRUE)
  cat(sprintf("  %-12s | woman-not-present mean=%.3f | woman-present mean=%.3f | diff=%6.3f (SE %.3f, p=%.3f) | N=%d\n",
              nm, m0, m1, ct[2, "beta"], ct[2, "SE"], ct[2, "p_Satt"], nrow(dd)))
}

## =============================================================================
## (d) Perception/belief outcomes are the respondent's, not necessarily the cook's
## =============================================================================
cat("\n\n=========== (d) Who answered the survey (perception/belief outcomes) ===========\n")
cat("Endline respondent gender (Q4, gender_resp -- who was actually interviewed):\n")
print(table(dta$gender_resp, useNA = "always"))
cat(sprintf("Share of respondents who are female: %.1f%%\n",
            100 * mean(dta$gender_resp == "Female", na.rm = TRUE)))
cat("\nIs the respondent the household head? (Q5, head_resp):\n")
print(table(dta$head_resp, dta$gender_resp, useNA = "always"))
cat("\nInterpretation for the manuscript: perception and belief outcomes (e.g., recommending\n")
cat("Bazooka, perceived downside risk, stated future intentions) are reported by whoever was\n")
cat("interviewed at endline -- a fairly even mix by gender (55% female), not necessarily the\n")
cat("household head, and NOT the same population as 'who cooked the demonstration meal': the\n")
cat("take-home sample was cooked by the female co-head in ~91% of demo households reporting,\n")
cat("a much higher share than the 55% of respondents who are female. So even though survey\n")
cat("respondents are not predominantly male, belief/perception outcomes still mix in a\n")
cat("substantial share of male respondents describing consumption traits that, in most\n")
cat("households, someone else (the female co-head) actually experienced hands-on.\n")
