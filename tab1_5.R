# ==============================================================================
# Publication-Ready Plots for Ranking Correlation, Stability & Cayley Distance
# Models: Cardinal Paired Comparison (CPC), Bradley-Terry (BT), and Official
# ==============================================================================

# Install required packages if not already present
# install.packages(c("ggplot2", "dplyr", "tidyr", "patchwork", "scales"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

# ------------------------------------------------------------------------------
# Global Palette & Publication Theme
# ------------------------------------------------------------------------------
model_colors <- c(
  "CPC Ranking"      = "#00468B",
  "Cardinal Model"   = "#00468B",
  "BT Ranking"       = "#ED0000",
  "Bradley-Terry"    = "#ED0000",
  "Official Ranking" = "#2B2D42"
)

model_linetypes <- c(
  "CPC Ranking"      = "solid",
  "Cardinal Model"   = "solid",
  "BT Ranking"       = "dashed",
  "Bradley-Terry"    = "dashed",
  "Official Ranking" = "dotted"
)

sig_shapes <- c(
  "p < 0.05 "      = 19, # Solid filled circle
  "p >= 0.05 " = 21  # Hollow white-filled circle
)

theme_publication <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 1, hjust = 0),
      plot.subtitle    = element_text(color = "grey30", size = base_size - 1, margin = margin(b = 8)),
      axis.title       = element_text(face = "bold", size = base_size),
      axis.text        = element_text(color = "black", size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
      strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.6),
      strip.text       = element_text(face = "bold", size = base_size),
      legend.position  = "bottom",
      legend.box       = "horizontal",
      legend.title     = element_blank(),
      legend.text      = element_text(size = base_size - 1, face = "bold"),
      legend.key.width = unit(1.3, "cm"),
      plot.margin      = margin(10, 12, 10, 10)
    )
}

# ==============================================================================
# TABLE 1 DATA & PLOT: Official Standing vs. Unadjusted Model Rankings
# (\label{tab:ranking_comparison})
# ==============================================================================
df_tab1 <- data.frame(
  Season  = 2008:2019,
  CPC_tau = c(0.643, 0.429, 0.571, 0.378, 0.556, 0.722, 0.857, 0.714, 0.500, 0.643, 0.000, 0.643),
  CPC_p   = c(0.031151, 0.178869, 0.061012, 0.155742, 0.044615, 0.005886, 0.001736, 0.014137, 0.108681, 0.031151, 1.000000, 0.031151),
  BT_tau  = c(0.714, 0.786, 0.786, 0.911, 0.833, 0.944, 0.857, 0.857, 0.429, 1.000, 0.786, 0.643),
  BT_p    = c(0.014137, 0.005506, 0.005506, 0.000030, 0.000854, 0.000050, 0.001736, 0.001736, 0.178869, 0.000050, 0.005506, 0.031151)
) %>%
  pivot_longer(
    cols          = -Season,
    names_to      = c("Model", ".value"),
    names_pattern = "(CPC|BT)_(tau|p)"
  ) %>%
  mutate(
    Model = ifelse(Model == "CPC", "CPC Ranking", "BT Ranking"),
    Model = factor(Model, levels = c("CPC Ranking", "BT Ranking")),
    Sig   = factor(ifelse(p < 0.05, "p < 0.05 ", "p >= 0.05 "),
                   levels = names(sig_shapes))
  )

plot_tab1 <- ggplot(df_tab1, aes(x = Season, y = tau, color = Model, linetype = Model)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50", linewidth = 0.6) +
  geom_line(linewidth = 1.0) +
  geom_point(aes(shape = Sig), fill = "white", size = 3.4, stroke = 1.1) +
  scale_x_continuous(breaks = 2008:2019) +
  scale_y_continuous(limits = c(-0.05, 1.05), breaks = seq(0.0, 1.0, by = 0.2)) +
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = model_linetypes) +
  scale_shape_manual(values = sig_shapes) +
  labs(
    #title    = "Kendall's Rank Correlation: Official Standings vs. Unadjusted Model Rankings",
    #subtitle = "Solid markers indicate statistically significant rank correlation (p < 0.05); hollow markers indicate p >= 0.05",
    x        = "Season",
    y        = expression(bold(paste("Kendall's Rank Correlation (", tau, ")")))
  ) +
  theme_publication()

# ==============================================================================
# TABLE 2 DATA & PLOT: Inter-Season Ranking Stability Without Covariates
# (\label{tab:ranking_stability_combined})
# ==============================================================================
season_pairs <- c("2008-09", "2009-10", "2010-11", "2011-12", "2012-13",
                  "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19")

df_tab2 <- data.frame(
  Pair     = factor(season_pairs, levels = season_pairs),
  Off_tau  = c(0.000000, 0.000000, 0.357143, 0.333333, 0.111111, 0.214286, 0.071429, -0.142857, -0.285714, 0.285714, 0.214286),
  Off_p    = c(1.000000, 1.000000, 0.275099, 0.259518, 0.761414, 0.548413, 0.904861, 0.719544, 0.398760, 0.398760, 0.548413),
  BT_tau   = c(0.071429, 0.000000, 0.571429, 0.500000, 0.222222, 0.285714, 0.214286, 0.142857, -0.285714, 0.214286, -0.071429),
  BT_p     = c(0.904861, 1.000000, 0.061012, 0.075176, 0.476709, 0.398760, 0.548413, 0.719544, 0.398760, 0.548413, 0.904861),
  CPC_tau  = c(-0.071429, 0.428571, -0.071429, 0.222222, 0.500000, 0.000000, -0.214286, 0.357143, -0.142857, -0.214286, 0.285714),
  CPC_p    = c(0.904861, 0.178869, 0.904861, 0.476709, 0.075176, 1.000000, 0.548413, 0.275099, 0.719544, 0.548413, 0.398760)
) %>%
  pivot_longer(
    cols          = -Pair,
    names_to      = c("Model", ".value"),
    names_pattern = "(Off|BT|CPC)_(tau|p)"
  ) %>%
  mutate(
    Model = case_when(
      Model == "CPC" ~ "CPC Ranking",
      Model == "BT"  ~ "BT Ranking",
      Model == "Off" ~ "Official Ranking"
    ),
    Model     = factor(Model, levels = c("CPC Ranking", "BT Ranking", "Official Ranking")),
    Marg_Sig  = factor(ifelse(p < 0.10, "p < 0.10 ", "p >= 0.10 "),
                       levels = c("p < 0.10 ", "p >= 0.10 "))
  )

plot_tab2 <- ggplot(df_tab2, aes(x = Pair, y = tau, group = Model, color = Model, linetype = Model)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey60", linewidth = 0.6) +
  geom_line(linewidth = 0.95) +
  geom_point(aes(shape = Marg_Sig), fill = "white", size = 3.3, stroke = 1.1) +
  scale_y_continuous(limits = c(-0.35, 0.65), breaks = seq(-0.3, 0.6, by = 0.15)) +
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = model_linetypes) +
  scale_shape_manual(values = c("p < 0.10 " = 19, "p >= 0.10 " = 21)) +
  labs(
    #title    = "Season-to-Season Ranking Stability of Models Without Covariates",
    #subtitle = "Consecutive-season Kendall's tau across Official, BT, and CPC rankings (all p >= 0.05; solid markers denote p < 0.10)",
    x        = "Consecutive Season Pair",
    y        = expression(bold(paste("Kendall's Rank Correlation (", tau, ")")))
  ) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ==============================================================================
# TABLE 3 DATA & PLOT: Official Standing vs. Covariate-Adjusted PC Rankings
# (\label{tab:ranking_comparison_cov1})
# ==============================================================================
df_tab3 <- data.frame(
  Season  = 2008:2019,
  CPC_tau = c(0.571, 0.429, 0.500, 0.378, 0.611, 0.722, 0.857, 0.786, 0.571, 0.643, 0.000, 0.786),
  CPC_p   = c(0.061012, 0.178869, 0.108681, 0.155742, 0.024741, 0.005886, 0.001736, 0.005506, 0.061012, 0.031151, 1.000000, 0.005506),
  BT_tau  = c(0.643, 0.643, 0.714, 0.556, 0.889, 0.889, 0.786, 0.857, 0.429, 1.000, 0.714, 0.286),
  BT_p    = c(0.031151, 0.031151, 0.014137, 0.028609, 0.000243, 0.000243, 0.005506, 0.001736, 0.178869, 0.000050, 0.014137, 0.398760)
) %>%
  pivot_longer(
    cols          = -Season,
    names_to      = c("Model", ".value"),
    names_pattern = "(CPC|BT)_(tau|p)"
  ) %>%
  mutate(
    Model = ifelse(Model == "CPC", "CPC Ranking", "BT Ranking"),
    Model = factor(Model, levels = c("CPC Ranking", "BT Ranking")),
    Sig   = factor(ifelse(p < 0.05, "p < 0.05 ", "p >= 0.05 "),
                   levels = names(sig_shapes))
  )

plot_tab3 <- ggplot(df_tab3, aes(x = Season, y = tau, color = Model, linetype = Model)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50", linewidth = 0.6) +
  geom_line(linewidth = 1.0) +
  geom_point(aes(shape = Sig), fill = "white", size = 3.4, stroke = 1.1) +
  scale_x_continuous(breaks = 2008:2019) +
  scale_y_continuous(limits = c(-0.05, 1.05), breaks = seq(0.0, 1.0, by = 0.2)) +
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = model_linetypes) +
  scale_shape_manual(values = sig_shapes) +
  labs(
    #title    = "Kendall's Rank Correlation: Official Standings vs. Covariate-Adjusted PC Models",
    #subtitle = "Solid markers indicate statistically significant rank correlation (p < 0.05); hollow markers indicate p >= 0.05",
    x        = "Season",
    y        = expression(bold(paste("Kendall's Rank Correlation (", tau, ")")))
  ) +
  theme_publication()

# ==============================================================================
# TABLE 4 DATA & PLOT: Unadjusted vs. Covariate-Adjusted PC Rankings
# (\label{tab:ranking_comparison_cov})
# ==============================================================================
df_tab4 <- data.frame(
  Season  = 2008:2019,
  CPC_tau = c(0.929, 0.929, 0.429, 0.422, 0.389, 0.667, 0.786, 0.500, 0.643, 0.714, 0.500, 0.286),
  CPC_p   = c(0.000397, 0.000397, 0.178869, 0.108313, 0.180181, 0.012665, 0.005506, 0.108681, 0.031151, 0.014137, 0.108681, 0.398760),
  BT_tau  = c(0.929, 0.714, 0.786, 0.467, 0.944, 0.944, 0.786, 0.857, 0.857, 1.000, 0.643, 0.071),
  BT_p    = c(0.000397, 0.014137, 0.005506, 0.072550, 0.000050, 0.000050, 0.005506, 0.001736, 0.001736, 0.000050, 0.031151, 0.904861)
) %>%
  pivot_longer(
    cols          = -Season,
    names_to      = c("Model", ".value"),
    names_pattern = "(CPC|BT)_(tau|p)"
  ) %>%
  mutate(
    Model = ifelse(Model == "CPC", "CPC Ranking", "BT Ranking"),
    Model = factor(Model, levels = c("CPC Ranking", "BT Ranking")),
    Sig   = factor(ifelse(p < 0.05, "p < 0.05 ", "p >= 0.05 "),
                   levels = names(sig_shapes))
  )

plot_tab4 <- ggplot(df_tab4, aes(x = Season, y = tau, color = Model, linetype = Model)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50", linewidth = 0.6) +
  geom_line(linewidth = 1.0) +
  geom_point(aes(shape = Sig), fill = "white", size = 3.4, stroke = 1.1) +
  scale_x_continuous(breaks = 2008:2019) +
  scale_y_continuous(limits = c(0.0, 1.05), breaks = seq(0.0, 1.0, by = 0.2)) +
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = model_linetypes) +
  scale_shape_manual(values = sig_shapes) +
  labs(
    #title    = "Kendall's Rank Correlation: Unadjusted vs. Covariate-Adjusted PC Models",
    #subtitle = "Measures sensitivity of inferred hierarchies to covariate inclusion (solid: p < 0.05; hollow: p >= 0.05)",
    x        = "Season",
    y        = expression(bold(paste("Kendall's Rank Correlation (", tau, ")")))
  ) +
  theme_publication()

# ==============================================================================
# TABLE 5 DATA & PLOT: Cayley Distance Summary Under Resampling
# (\label{tab:combined_bootstrap_comparison})
# ==============================================================================
df_tab5 <- data.frame(
  Season          = 2008:2019,
  BT_NoRep_M    = c(2.563, 3.379, 2.992, 4.086, 3.082, 2.582, 1.400, 3.160, 2.723, 2.229, 3.556, 3.902),
  BT_NoRep_SD   = c(1.105, 1.294, 1.100, 1.396, 1.283, 1.218, 0.980, 1.203, 1.099, 1.116, 1.298, 1.256),
  BT_Rep_M      = c(3.529, 4.291, 3.841, 5.486, 4.450, 4.009, 2.625, 4.057, 3.909, 3.497, 4.529, 4.647),
  BT_Rep_SD     = c(1.195, 1.320, 1.252, 1.420, 1.417, 1.414, 1.228, 1.307, 1.230, 1.239, 1.310, 1.199),
  CPC_NoRep_M   = c(2.673, 2.034, 2.662, 4.703, 3.491, 3.443, 1.751, 1.751, 2.406, 2.253, 3.586, 2.810),
  CPC_NoRep_SD  = c(1.247, 1.343, 1.158, 1.507, 1.303, 1.316, 1.064, 0.989, 1.407, 1.093, 1.371, 1.614),
  CPC_Rep_M     = c(3.771, 3.434, 3.722, 5.940, 4.728, 4.446, 2.795, 2.739, 3.697, 3.379, 4.484, 4.065),
  CPC_Rep_SD    = c(1.209, 1.308, 1.235, 1.417, 1.350, 1.395, 1.199, 1.278, 1.312, 1.246, 1.292, 1.432)
) %>%
  pivot_longer(
    cols          = -Season,
    names_to      = c("Model", "Scheme", ".value"),
    names_pattern = "(BT|CPC)_(NoRep|Rep)_(M|SD)"
  ) %>%
  mutate(
    Model  = ifelse(Model == "CPC", "Cardinal Model", "Bradley-Terry"),
    Model  = factor(Model, levels = c("Cardinal Model", "Bradley-Terry")),
    Scheme = ifelse(Scheme == "NoRep", "Without Replacement (Subsampling)", "With Replacement (Bootstrap)"),
    Scheme = factor(Scheme, levels = c("Without Replacement (Subsampling)", "With Replacement (Bootstrap)"))
  )

pd_cayley <- position_dodge(width = 0.35)

plot_tab5 <- ggplot(df_tab5, aes(x = Season, y = M, color = Model, fill = Model,
                                 linetype = Model, shape = Model)) +
  geom_ribbon(aes(ymin = pmax(0, M - SD), ymax = M + SD),
              alpha = 0.12, color = NA, position = pd_cayley) +
  geom_errorbar(aes(ymin = pmax(0, M - SD), ymax = M + SD),
                width = 0.25, linewidth = 0.55, alpha = 0.75, position = pd_cayley) +
  geom_line(linewidth = 1.0, position = pd_cayley) +
  geom_point(size = 3.0, position = pd_cayley) +
  facet_wrap(~ Scheme, ncol = 2) +
  scale_x_continuous(breaks = seq(2008, 2019, by = 2)) +
  scale_y_continuous(limits = c(0, 7.6), breaks = seq(0, 7, by = 1)) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  scale_linetype_manual(values = model_linetypes) +
  scale_shape_manual(values = c("Cardinal Model" = 16, "Bradley-Terry" = 17)) +
  labs(
    #title    = "Cayley Distance Summary for BT and CPC Models Under Resampling",
    #subtitle = "Mean Cayley distance (+/- 1 SD error bars and shaded band) across 2008-2019 IPL seasons",
    x        = "Season",
    y        = "Cayley Distance (Mean +/- 1 SD)"
  ) +
  theme_publication()

# ==============================================================================
# OPTIONAL COMPOSITE FIGURE: Unified 3-Panel Kendall's Tau Comparison (Tables 1, 3, 4)
# ==============================================================================
p1_compact <- plot_tab1 + labs(title = "Official vs. Unadjusted PC Models", subtitle = NULL, x = NULL)
p3_compact <- plot_tab3 + labs(title = "Official vs. Covariate-Adjusted PC Models", subtitle = NULL, x = NULL)
p4_compact <- plot_tab4 + labs(title = "Unadjusted vs. Covariate-Adjusted PC Models", subtitle = NULL)

plot_kendall_unified <- (p1_compact / p3_compact / p4_compact) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag        = element_text(face = "bold", size = 12)
  )

# Print all plots
pdf("plot_tab1.pdf", width = 9.0, height = 5.0)
plot_tab1
dev.off()

pdf("plot_tab2.pdf", width = 9.0, height = 5.0)
plot_tab2
dev.off()

pdf("plot_tab3.pdf", width = 9.0, height = 5.0)
plot_tab3
dev.off()

pdf("plot_tab4.pdf", width = 9.0, height = 5.0)
plot_tab4
dev.off()

pdf("plot_tab5.pdf", width = 9.0, height = 5.0)
plot_tab5
dev.off()

pdf("plot_kendall_unified.pdf", width = 9.0, height = 10.0)
plot_kendall_unified
dev.off()

