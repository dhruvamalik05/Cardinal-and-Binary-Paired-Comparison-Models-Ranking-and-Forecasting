# ==============================================================================
# TABLE 5 DATA & PLOTS: Cayley Distance Summary Under Resampling
# (\label{tab:combined_bootstrap_comparison})
# ==============================================================================
library(ggplot2)
library(dplyr)
library(tidyr)

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
    # Keep the exact acronyms for the legend
    Model  = factor(Model, levels = c("CPC", "BT"))
  )

# Split data into two distinct dataframes for the subplots
df_norep <- df_tab5 %>% filter(Scheme == "NoRep")
df_rep   <- df_tab5 %>% filter(Scheme == "Rep")

# Updated mapping aesthetics for the acronyms
model_colors <- c("CPC" = "#00468B", "BT" = "#ED0000")
model_linetypes <- c("CPC" = "solid", "BT" = "dashed")
pd_cayley <- position_dodge(width = 0.35)

# ------------------------------------------------------------------------------
# PLOT A: Without Replacement (Subsampling)
# ------------------------------------------------------------------------------
plot_tab5a <- ggplot(df_norep, aes(x = Season, y = M, color = Model, fill = Model,
                                   linetype = Model, shape = Model)) +
  geom_ribbon(aes(ymin = pmax(0, M - SD), ymax = M + SD),
              alpha = 0.12, color = NA, position = pd_cayley) +
  geom_errorbar(aes(ymin = pmax(0, M - SD), ymax = M + SD),
                width = 0.25, linewidth = 0.55, alpha = 0.75, position = pd_cayley) +
  geom_line(linewidth = 1.0, position = pd_cayley) +
  geom_point(size = 3.0, position = pd_cayley) +
  scale_x_continuous(breaks = seq(2008, 2019, by = 2)) +
  scale_y_continuous(limits = c(0, 7.6), breaks = seq(0, 7, by = 1)) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  scale_linetype_manual(values = model_linetypes) +
  scale_shape_manual(values = c("CPC" = 16, "BT" = 17)) +
  labs(
    #title = "Without Replacement (Subsampling)",
    x     = "Season",
    y     = "Cayley Distance (Mean +/- 1 SD)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, hjust = 0),
    axis.title       = element_text(face = "bold", size = 12),
    axis.text        = element_text(color = "black", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 11, face = "bold"),
    legend.key.width = unit(1.3, "cm")
  )

# ------------------------------------------------------------------------------
# PLOT B: With Replacement (Bootstrap)
# ------------------------------------------------------------------------------
plot_tab5b <- ggplot(df_rep, aes(x = Season, y = M, color = Model, fill = Model,
                                 linetype = Model, shape = Model)) +
  geom_ribbon(aes(ymin = pmax(0, M - SD), ymax = M + SD),
              alpha = 0.12, color = NA, position = pd_cayley) +
  geom_errorbar(aes(ymin = pmax(0, M - SD), ymax = M + SD),
                width = 0.25, linewidth = 0.55, alpha = 0.75, position = pd_cayley) +
  geom_line(linewidth = 1.0, position = pd_cayley) +
  geom_point(size = 3.0, position = pd_cayley) +
  scale_x_continuous(breaks = seq(2008, 2019, by = 2)) +
  scale_y_continuous(limits = c(0, 7.6), breaks = seq(0, 7, by = 1)) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  scale_linetype_manual(values = model_linetypes) +
  scale_shape_manual(values = c("CPC" = 16, "BT" = 17)) +
  labs(
    #title = "With Replacement (Bootstrap)",
    x     = "Season",
    y     = "Cayley Distance (Mean +/- 1 SD)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, hjust = 0),
    axis.title       = element_text(face = "bold", size = 12),
    axis.text        = element_text(color = "black", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 11, face = "bold"),
    legend.key.width = unit(1.3, "cm")
  )



# Combine into a unified 1-row, 2-column figure
combined_figure <- (plot_tab5a + plot_tab5b) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.tag        = element_text(face = "bold", size = 13)
  )

print(combined_figure)

# Optional: Save the combined side-by-side figure
pdf("plot_tab5ab.pdf", width = 11.0, height = 5.0)
combined_figure
dev.off()


