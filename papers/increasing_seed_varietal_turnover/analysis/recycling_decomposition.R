## Decomposition of the seed trial pack's recycling effect (random plot)
## Question (B. Kramer): does the trial pack increase the probability of
## recycling seed regardless of variety, or does it only redirect an
## existing recycling habit toward Bazooka?
##
## Outcomes, all on the randomly selected maize plot:
##   1. used recycled Bazooka
##   2. used recycled non-Bazooka improved seed (other hybrids and OPVs)
##   3. used any recycled improved seed (1 + 2)
##   4. used a local (or unknown) variety -- farmer-saved by construction:
##      the recycling question (plot_times_rec) was only asked when the
##      variety was an improved type, so local seed has no recycling code
##   5. used farmer-saved seed broadly (3 OR 4); its complement is fresh
##      purchase, so this row mirrors the fresh hybrid/OPV crowd-out
##
## plot_times_rec coding (endline questionnaire, list "recycled"):
##   1 = fresh (first use in Nsambya 2023); 2-5 = recycled 1 to >3 times;
##   99 = don't know (set to NA).
##
## Run from the repository root (mippi_UG/).

library(clubSandwich)

d <- read.csv("endline/data/public/endline.csv", stringsAsFactors = FALSE)
b <- read.csv("baseline/data/public/baseline.csv", stringsAsFactors = FALSE)

## sample restriction as in analysis.R: drop paid and discounted trial packs
## (companion study); these households received packs but have trial_P == FALSE
d <- subset(d, !(paid_pac == "TRUE" | discounted == "TRUE"))
b <- subset(b, !(paid_pac == "TRUE" | discounted == "TRUE"))

b$cluster_ID <- paste(b$distID, b$subID, b$vilID, sep = "_")
d <- merge(d, b[, c("farmer_ID", "cluster_ID")], by.x = "ID", by.y = "farmer_ID")
d <- subset(d, consent == "Yes")
d$no_grow <- (d$plot_count == "0")

## pull the randomly selected plot's recycling code
d$rnd_num <- suppressWarnings(as.numeric(d$plot_select))
get_rnd <- function(d, stem) {
  out <- rep(NA_character_, nrow(d))
  for (k in 1:5) {
    col <- paste0("plot.", k, "..", stem)
    if (col %in% names(d)) {
      idx <- which(d$rnd_num == k)
      out[idx] <- as.character(d[idx, col])
    }
  }
  out
}
d$tr <- get_rnd(d, "plot_times_rec")
d$tr[d$tr == "99"] <- NA

v <- d$maize_var_selected
hyb <- c("Longe_10H", "Longe_10R", "Longe_7H", "Longe_7R_Kayongo-go", "Bazooka",
         "DK", "Longe_6H", "Panner", "UH5051", "Wema", "KH_series", "other_hybrid")
opv <- c("Longe_5", "Longe_5D", "Longe_4", "MM3", "other_opv")
imp <- v %in% c(hyb, opv)
rec <- d$tr %in% c("2", "3", "4", "5")

d$rec_bazooka <- as.numeric(rec & v == "Bazooka")
d$rec_nonbaz  <- as.numeric(rec & v != "Bazooka" & imp)
d$rec_any_imp <- as.numeric(rec)
d$local_var   <- as.numeric(!imp)
d$saved_broad <- as.numeric(rec | !imp)
outcomes <- c("rec_bazooka", "rec_nonbaz", "rec_any_imp", "local_var", "saved_broad")
for (y in outcomes) d[d$no_grow %in% TRUE, y] <- NA

run <- function(y) {
  dd <- d[!is.na(d[[y]]), ]
  m <- lm(as.formula(paste(y, "~ trial_P*cont")), data = dd)
  vc <- vcovCR(m, cluster = dd$cluster_ID, type = "CR2")
  ct <- as.data.frame(coef_test(m, vc))
  cm <- mean(dd[[y]][dd$trial_P == FALSE & dd$cont == FALSE])
  cat(sprintf("\n=== %s | pure-control mean = %.3f | N = %d ===\n", y, cm, nrow(dd)))
  print(round(ct[2:4, c("beta", "SE", "p_Satt")], 3))
}
invisible(lapply(outcomes, run))
