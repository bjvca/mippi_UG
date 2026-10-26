#### forest plots of main results tables for presentation
#### Effects are standardized by the control-group SD so all outcomes share a
#### common axis; each point is annotated with the effect in native units.
#### Reads the saved estimate arrays produced by analysis.R. Each table has:
####   df_<tab>       - 3 x 4 x K array: rows = (trial pack, cooking demo,
####                    interaction); cols = (beta, SE, sharpened q, nobs)
####   df_means_<tab> - 6 x K matrix: rows 1-2 = control mean, control SD
#### Array <-> table row mappings are documented per table below.

library(ggplot2)

res_dir <- if (dir.exists("results")) "results" else "../results"

arms <- c("Trial pack", "Cooking demo", "Interaction")
arm_cols <- c("Trial pack" = "#1f78b4", "Cooking demo" = "#e66101",
              "Interaction" = "grey45")

fmt_native <- function(beta, unit) {
  switch(unit,
    pp        = sprintf("%+.0f pp", beta * 100),
    plots     = sprintf("%+.2f plots", beta),
    acres     = sprintf("%+.2f acres", beta),
    varieties = sprintf("%+.2f varieties", beta),
    sd        = sprintf("%+.2f SD", beta))
}

make_forest <- function(res, means, meta, file, height) {
  pd <- do.call(rbind, lapply(seq_len(nrow(meta)), function(r) {
    k  <- meta$idx[r]
    sd <- means[2, k]
    data.frame(
      outcome = meta$label[r],
      arm     = arms,
      d       = res[1:3, 1, k] / sd,
      lo      = (res[1:3, 1, k] - 1.96 * res[1:3, 2, k]) / sd,
      hi      = (res[1:3, 1, k] + 1.96 * res[1:3, 2, k]) / sd,
      q       = res[1:3, 3, k],
      native  = sapply(res[1:3, 1, k], fmt_native, unit = meta$unit[r]),
      stringsAsFactors = FALSE
    )
  }))
  pd$outcome <- factor(pd$outcome, levels = rev(meta$label))
  pd$arm     <- factor(pd$arm, levels = arms)
  pd$sig     <- pd$q < 0.05

  span  <- max(pd$hi) - min(pd$lo)
  x_lab <- max(pd$hi) + 0.45 * span  # right-hand text column position

  p <- ggplot(pd, aes(x = d, y = outcome, colour = arm)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_hline(yintercept = nrow(meta) - 0.5, colour = "grey85",
               linewidth = 0.4) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.18, linewidth = 0.6,
                  orientation = "y") +
    geom_point(aes(fill = ifelse(sig, as.character(arm), "white")),
               shape = 21, size = 2.8, stroke = 0.9) +
    geom_text(aes(x = x_lab, label = native),
              hjust = 1, size = 3.4, colour = "grey20", family = "sans") +
    facet_wrap(~arm, nrow = 1) +
    scale_colour_manual(values = arm_cols, guide = "none") +
    scale_fill_manual(values = c(arm_cols, white = "white"), guide = "none") +
    scale_x_continuous(breaks = pretty(c(min(pd$lo), max(pd$hi)), n = 4),
                       limits = c(min(pd$lo) - 0.05 * span,
                                  x_lab + 0.02 * span)) +
    labs(x = "Intent-to-treat effect (control-group standard deviations)",
         y = NULL,
         caption = paste("Points show ITT effects scaled by the control-group SD; labels give effects in native units.",
                         "Filled points: significant at the 5% level (sharpened q-values); bars: unadjusted 95% CI.",
                         sep = "\n")) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold", size = 12),
          plot.caption = element_text(size = 8.5, colour = "grey35", hjust = 0),
          axis.text.y = element_text(size = 11, colour = "grey10"))

  ggsave(file.path(res_dir, paste0(file, ".png")), p,
         width = 11, height = height, dpi = 300, bg = "white")
  ggsave(file.path(res_dir, paste0(file, ".pdf")), p,
         width = 11, height = height)
  invisible(p)
}

#### Table 3: adoption (df_res / df_means_out)
## array order: 1 used Bazooka any, 2 used fresh Bazooka, 3 used fresh
## hybrid/OPV, 4 n plots, 5 share plots, 6 area, 7 share area, 8 index
load(file.path(res_dir, "df_res.Rdata"))
load(file.path(res_dir, "df_means_out.Rdata"))
make_forest(df_res, df_means_out, data.frame(
  idx   = c(8, 1, 4, 5, 6, 7, 2, 3),
  label = c("Adoption index",
            "Used Bazooka (any seed)",
            "No. of plots with Bazooka",
            "Share of maize plots",
            "Area with Bazooka (acres)",
            "Share of maize area",
            "Used fresh Bazooka seed",
            "Used fresh hybrid or OPV"),
  unit  = c("sd", "pp", "plots", "pp", "acres", "pp", "pp", "pp"),
  stringsAsFactors = FALSE
), "forest_adoption", height = 4.8)

#### Table 4: knowledge, risk, social learning, intentions (df_path)
## array order: 1 knw_bazo, 2 nr_vars, 3 downside_risk_imp,
## 4 downside_risk_bazo, 5 share_imp (recommended improved), 6 share_bazo
## (recommended Bazooka), 7 future_imp, 8 future_bazo, 9 index
load(file.path(res_dir, "df_path.Rdata"))
load(file.path(res_dir, "df_means_path.Rdata"))
make_forest(df_path, df_means_path, data.frame(
  idx   = c(9, 1, 2, 3, 4, 5, 6, 7, 8),
  label = c("Awareness & attitudes index",
            "Knows Bazooka",
            "No. of improved varieties known",
            "Thinks improved varieties are risky",
            "Thinks Bazooka is risky",
            "Recommended improved varieties",
            "Recommended Bazooka",
            "Will use improved varieties",
            "Will use Bazooka"),
  unit  = c("sd", "pp", "varieties", rep("pp", 6)),
  stringsAsFactors = FALSE
), "forest_awareness", height = 5.2)

#### Table 5: perceptions of consumption traits, improved vs local
#### (df_compare_cons_traits_seed)
## array order: 1 taste better, 2 portions larger, 3 better appearance,
## 4 easier to cook, 5 index
load(file.path(res_dir, "df_compare_cons_traits_seed.Rdata"))
load(file.path(res_dir, "df_means_compare_cons_traits_seed.Rdata"))
make_forest(df_compare_cons_traits_seed, df_means_compare_cons_traits_seed,
            data.frame(
  idx   = c(5, 1, 2, 3, 4),
  label = c("Consumption traits index",
            "Tastes better",
            "Larger portions",
            "Better appearance",
            "Easier to cook"),
  unit  = c("sd", rep("pp", 4)),
  stringsAsFactors = FALSE
), "forest_cons_traits", height = 3.6)

#### Table 6: perceptions of production traits, improved vs local
#### (df_compare_prod_traits_seed)
## array order: 1 yields more, 2 abiotic stress resistance, 3 biotic stress
## resistance, 4 matures faster, 5 higher germination, 6 index
load(file.path(res_dir, "df_compare_prod_traits_seed.Rdata"))
load(file.path(res_dir, "df_means_compare_prod_traits_seed.Rdata"))
make_forest(df_compare_prod_traits_seed, df_means_compare_prod_traits_seed,
            data.frame(
  idx   = c(6, 1, 2, 3, 4, 5),
  label = c("Production traits index",
            "Yields more",
            "More resistant to abiotic stress",
            "More resistant to biotic stress",
            "Matures faster",
            "Higher germination rate"),
  unit  = c("sd", rep("pp", 5)),
  stringsAsFactors = FALSE
), "forest_prod_traits", height = 4.0)
