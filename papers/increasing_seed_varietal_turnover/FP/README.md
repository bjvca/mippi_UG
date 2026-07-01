# Food Policy submission package

Anonymized (double-blind) manuscript + separate title page.

## Files
- `increasing_seed_varietal_turnover_anonymized.lyx` — manuscript with author block,
  affiliations, `\thanks`, the GitHub "latest version" link, and the identifying
  acknowledgments/funding/ethics/registration removed. Title and body unchanged.
- `title_page.tex` — full author list, affiliations, corresponding author,
  acknowledgments, funding, ethics/registration, generative-AI and competing-interest
  declarations, data/code availability. Compiles standalone (`article` class).

## Before compiling the anonymized `.lyx` — VERIFY ONE THING
Because it now lives in `FP/`, the paths were rewritten to point one level up:
- knitr data load: `pathA <- getwd()` → `pathA <- dirname(getwd())`
- figures: `../timeline.png`, `../analysis/...`, `../results/...`
- bibliography: `../MIPPI_PAP`; style: `../../../study_design/IFPRI_Style`

The `dirname(getwd())` change assumes knitr's working directory at compile time is
`FP/`. Confirm the `.Rdata` chunks knit; if not, set `pathA` to the parent paper
directory explicitly.

## Still to decide — self-citations (double-blind)
Removing the author block does not mask cited works authored by the submitting
authors. Cites to your own work that remain in the body:
- `campenhout2021role`, `van2023hiding` (Van Campenhout)
- `kramer2017cooking`, `kramer2024gender` (Kramer; the latter also Trachtman)

`Trachtman2023Accelerating` was cited only in the acknowledgments and is already
gone. Depending on Food Policy's policy, mask these as "Author(s) (year)" or leave
them in third person. Not done automatically.
