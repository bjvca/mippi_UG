## Cost-effectiveness and scaling analysis
## Inputs: budgets/baseline_budget_and_treat_CM.xlsx (seed line) and
##         budgets/budget-consumption_intervention.xlsx (demo, standalone)
## Outputs: analysis/cost_scaling.png + printed summary
## All figures in UGX. Assumptions are stated inline and are deliberately
## conservative against the trial pack (we cost a STANDALONE pack-distribution
## program, even though in our study packs were handed out during the baseline
## visit at near-zero marginal delivery cost).

library(ggplot2)

## ---- shared unit costs (from the consumption-intervention budget) ----
enum_allow   <- 150000   # enumerator field allowance, per person-day
enum_per_team<- 2
car_hire     <- 210000   # per car-day
fuel         <- 130000   # per car-day
mob_lc1      <- 20000    # LC1/VHT mobilization, per village
airtime_vill <- 3000     # team airtime attributable per village
flour_price  <- 3700     # UGX/kg

## ====================================================================
## SEED TRIAL PACK  (standalone program)
## ====================================================================
seed_cost <- 12000       # 1 kg @ retail, per farmer (pure variable, unshared)

## Standalone distribution: light operation (~30 min/village), so a 2-person
## team covers many villages per day. Shared per-village cost D_pack:
pack_villages_per_teamday <- 5
D_pack <- (enum_per_team*enum_allow + car_hire + fuel)/pack_villages_per_teamday +
          mob_lc1 + airtime_vill
## per-farmer pack cost at n farmers/village:
pack_pf <- function(n) seed_cost + D_pack/n

## ====================================================================
## COOKING DEMO  (standalone program, scalable = research overhead stripped)
## ====================================================================
## Per-session SHARED cost S_demo (independent of attendance):
demo_sessions_per_teamday <- 2          # 8 demos/day across 4 teams
S_transport_staff <- (enum_per_team*enum_allow + car_hire + fuel)/demo_sessions_per_teamday +
                     mob_lc1 + airtime_vill
demo_cooking_flour <- 2*flour_price     # 1 kg Bazooka + 1 kg local per session
water_tasting      <- 24000             # per session
sundry_session     <- 1000 + 5128 + 10256 + 5000  # gabbage, wash labour, flour storage, misc
S_demo <- S_transport_staff + demo_cooking_flour + water_tasting + sundry_session

## Per-attendee VARIABLE cost:
takehome        <- 2*flour_price        # 2 kg take-home flour sample
tasting_portion <- 1000                 # incremental tasting flour/water per head
v_with    <- takehome + tasting_portion # demo as we ran it
v_without <- tasting_portion            # "core" demo, no individual take-home

## per-farmer demo cost at n attendees/session (fixed costs amortized away at scale):
demo_pf <- function(n, v) S_demo/n + v

## FIXED costs (independent of village count, given a 4-team config):
durables  <- 1760000+320000+24000+32000+40000+600000+80000+120000+40000+
             160000+840000+120000+16000+16000+60000+8000   # reusable kit
training  <- 6042000
fixed_demo<- durables + training

## ---- breakeven attendance (per-farmer cost: demo = pack) ----
## S_demo/n + v = seed_cost + D_pack/n  ->  n = (S_demo - D_pack)/(seed_cost - v)
breakeven <- function(v) (S_demo - D_pack)/(seed_cost - v)
be_without <- breakeven(v_without)
be_with    <- breakeven(v_with)

## ====================================================================
## REALIZED-DESIGN COST PER ADOPTER (n = 10/session, 78 villages/arm)
## ====================================================================
## demo anchored on ACTUAL budget total:
demo_total_full     <- 58761200
research_overhead   <- 4320000+2160000+720000+360000+1000000   # IFPRI RA/driver/insurance
demo_total_scalable <- demo_total_full - research_overhead
n_demo_hh <- 780
demo_pf_realized_full     <- demo_total_full/n_demo_hh
demo_pf_realized_scalable <- demo_total_scalable/n_demo_hh
pack_pf_realized          <- pack_pf(10)

## ITT effects (Table 3, "any plot"):
itt_seed_turnover <- 0.67   # has used Bazooka on any plot
itt_demo_turnover <- 0.09   # (n.s.)
itt_seed_fresh    <- 0.02   # has used fresh Bazooka (n.s. ~ 0)
itt_demo_fresh    <- 0.05   # *

cpa <- function(cost_pf, itt) cost_pf/itt

cat("================ COST MODEL ================\n")
cat(sprintf("Pack:  seed %d + D_pack/n; D_pack = %.0f/village\n", seed_cost, D_pack))
cat(sprintf("Demo:  S_demo/n + v; S_demo = %.0f/session; v_with=%d, v_without=%d\n",
            S_demo, v_with, v_without))
cat(sprintf("Demo fixed (durables+training) = %.0f\n\n", fixed_demo))

cat("---- per-farmer cost at realized n=10 ----\n")
cat(sprintf("Pack (standalone)          : %.0f\n", pack_pf_realized))
cat(sprintf("Demo (scalable, budget)    : %.0f\n", demo_pf_realized_scalable))
cat(sprintf("Demo (full economic, budget): %.0f\n\n", demo_pf_realized_full))

cat("---- breakeven attendance per session (per-farmer cost) ----\n")
cat(sprintf("core demo (no take-home): n = %.1f\n", be_without))
cat(sprintf("with 2kg take-home      : n = %.1f\n\n", be_with))

cat("---- cost per ADDITIONAL ADOPTER ----\n")
cat("Turnover (any Bazooka):\n")
cat(sprintf("  Pack: %.0f / %.2f = %.0f\n", pack_pf_realized, itt_seed_turnover,
            cpa(pack_pf_realized, itt_seed_turnover)))
cat(sprintf("  Demo: %.0f / %.2f = %.0f\n", demo_pf_realized_scalable, itt_demo_turnover,
            cpa(demo_pf_realized_scalable, itt_demo_turnover)))
cat("Commercial seed (fresh Bazooka):\n")
cat(sprintf("  Pack: ITT=%.2f (n.s. ~0) -> effectively unbounded\n", itt_seed_fresh))
cat(sprintf("  Demo: %.0f / %.2f = %.0f (+ yield gains)\n", demo_pf_realized_scalable,
            itt_demo_fresh, cpa(demo_pf_realized_scalable, itt_demo_fresh)))

## ====================================================================
## CROSSOVER FIGURE
## ====================================================================
n_seq <- seq(5, 100, by = 1)
df <- rbind(
  data.frame(n=n_seq, cost=pack_pf(n_seq),            series="Seed trial pack"),
  data.frame(n=n_seq, cost=demo_pf(n_seq, v_without), series="Cooking demo (no take-home)"),
  data.frame(n=n_seq, cost=demo_pf(n_seq, v_with),    series="Cooking demo (with 2kg take-home)")
)
df$series <- factor(df$series, levels=c("Seed trial pack",
                                        "Cooking demo (no take-home)",
                                        "Cooking demo (with 2kg take-home)"))

p <- ggplot(df, aes(x=n, y=cost, colour=series, linetype=series)) +
  geom_line(linewidth=0.9) +
  geom_vline(xintercept=c(be_without, be_with), colour="grey60", linetype="dotted") +
  annotate("text", x=be_without, y=max(df$cost)*0.95,
           label=sprintf("breakeven\nn=%.0f", be_without), size=3, colour="grey30", hjust=-0.05) +
  annotate("text", x=be_with, y=max(df$cost)*0.78,
           label=sprintf("breakeven\nn=%.0f", be_with), size=3, colour="grey30", hjust=-0.05) +
  geom_vline(xintercept=10, colour="black", linetype="longdash", alpha=0.4) +
  annotate("text", x=10, y=max(df$cost)*0.6, label="our study (n=10)",
           angle=90, vjust=-0.4, size=3, colour="black") +
  scale_colour_manual(values=c("#C0392B", "#2E86C1", "#5DADE2")) +
  scale_linetype_manual(values=c("solid","dashed","dotted")) +
  scale_y_continuous(labels=function(x) format(x, big.mark=",", scientific=FALSE)) +
  labs(x="Farmers reached per session / village",
       y="Cost per farmer reached (UGX)",
       colour=NULL, linetype=NULL) +
  theme_minimal(base_size=12) +
  theme(legend.position="bottom", legend.direction="vertical",
        panel.grid.minor=element_blank())

ggsave("analysis/cost_scaling.png", p, width=7.5, height=5, dpi=200)
cat("\nFigure written to analysis/cost_scaling.png\n")
