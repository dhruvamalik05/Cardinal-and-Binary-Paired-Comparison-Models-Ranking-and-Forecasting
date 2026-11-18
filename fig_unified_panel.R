# ==============================================================================
# Publication-Ready Visualization Script: IPL Out-of-Sample Forecasting (2008-2019)
# Models: BT (Regularized Fisher-Weighted IRLS) vs. Cardinal Paired Comparison (CPC)
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)

# ------------------------------------------------------------------------------
# 1. Data Entry (Model-III: LTF, STM, HGA | Synchronized with Appendix Table 5)
# ------------------------------------------------------------------------------
seasons <- 2008:2019
thresholds <- c("0.00", "0.25", "0.50", "0.75")

card_acc <- list(
  "0.00" = c(0.517, 0.517, 0.467, 0.583, 0.486, 0.684, 0.556, 0.586, 0.700, 0.567, 0.433, 0.600),
  "0.25" = c(0.538, 0.520, 0.500, 0.542, 0.462, 0.714, 0.632, 0.636, 0.727, 0.611, 0.474, 0.542),
  "0.50" = c(0.800, 0.500, 0.429, 0.438, 0.556, 0.846, 0.643, 0.692, 0.733, 0.750, 0.500, 0.500),
  "0.75" = c(0.333, 0.385, 0.375, 0.375, 0.583, 1.000, 0.545, 0.667, 0.750, 0.600, 0.286, 0.333)
)

bt_acc <- list(
  "0.00" = c(0.621, 0.483, 0.433, 0.611, 0.649, 0.605, 0.630, 0.586, 0.567, 0.633, 0.333, 0.367),
  "0.25" = c(0.652, 0.500, 0.476, 0.545, 0.667, 0.750, 0.706, 0.500, 0.667, 0.600, 0.300, 0.385),
  "0.50" = c(0.765, 0.500, 0.500, 0.556, 0.500, 0.818, 0.643, 0.444, 0.800, 0.556, 0.200, 0.333),
  "0.75" = c(0.667, 0.600, 0.500, 0.750, 0.400, 1.000, 0.571, 0.333, 0.667, 0.333, 0.500, 0.000)
)

card_n <- list(
  "0.00" = c(29, 29, 30, 36, 37, 38, 27, 29, 30, 30, 30, 30),
  "0.25" = c(26, 25, 24, 24, 26, 28, 19, 22, 22, 18, 19, 24),
  "0.50" = c(10, 22, 14, 16, 18, 13, 14, 13, 15, 8, 14, 14),
  "0.75" = c(3, 13, 8, 8, 12, 7, 11, 12, 8, 5, 7, 6)
)

bt_n <- list(
  "0.00" = c(29, 29, 30, 36, 37, 38, 27, 29, 30, 30, 30, 30),
  "0.25" = c(23, 20, 21, 22, 21, 20, 17, 12, 18, 15, 20, 13),
  "0.50" = c(17, 10, 14, 9, 12, 11, 14, 9, 10, 9, 5, 3),
  "0.75" = c(12, 5, 6, 4, 5, 5, 7, 3, 3, 3, 2, 2)
)

# ------------------------------------------------------------------------------
# 2. Data Structuring & Summary Metrics
# ------------------------------------------------------------------------------
build_df <- function(acc_list, n_list, model_name) {
  df_list <- lapply(thresholds, function(k) {
    data.frame(
      Season    = seasons,
      Threshold = as.numeric(k),
      Accuracy  = acc_list[[k]],
      Matches   = n_list[[k]],
      Model     = model_name
    )
  })
  do.call(rbind, df_list)
}

df_full <- rbind(
  build_df(card_acc, card_n, "CPC"),
  build_df(bt_acc, bt_n, "BT")
) %>%
  mutate(Model = factor(Model, levels = c("CPC", "BT")))

df_summary <- df_full %>%
  group_by(Model, Threshold) %>%
  summarize(
    Mean_Acc     = mean(Accuracy),
    SD_Acc       = sd(Accuracy),
    SE_Acc       = sd(Accuracy) / sqrt(n()),
    Median_Acc   = median(Accuracy),
    IQR_Acc      = IQR(Accuracy),
    Mean_Matches = mean(Matches),
    SD_Matches   = sd(Matches),
    Min_Matches  = min(Matches),
    Max_Matches  = max(Matches),
    .groups      = "drop"
  )

# ------------------------------------------------------------------------------
# 3. Global Aesthetic & Theme Configuration
# ------------------------------------------------------------------------------
model_colors <- c("CPC" = "#00468B", "BT" = "#ED0000")
model_shapes <- c("CPC" = 16, "BT" = 17)
pd <- position_dodge(width = 0.03)

theme_publication <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 1, hjust = 0),
      plot.subtitle    = element_text(color = "grey30", size = base_size - 1, margin = margin(b = 8)),
      axis.title       = element_text(face = "bold", size = base_size),
      axis.text        = element_text(color = "black", size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.text      = element_text(size = base_size, face = "bold"),
      legend.key.width = unit(1.5, "cm"),
      plot.margin      = margin(10, 12, 10, 10)
    )
}

# ==============================================================================
# PLOT 1: The Accuracy Trade-off (Mean +/- 1 SE with Optimal Regime Highlight)
# ==============================================================================
plot_accuracy <- ggplot(df_summary, aes(x = Threshold, y = Mean_Acc, color = Model, shape = Model)) +
  annotate("rect", xmin = 0.44, xmax = 0.56, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.4) +
  annotate("text", x = 0.50, y = 0.672, label = "Optimal Regime\n(C = 0.50)",
           size = 3.3, fontface = "italic", color = "grey20") +
  geom_hline(yintercept = 0.50, linetype = "dotted", color = "grey40", linewidth = 0.7) +
  geom_errorbar(aes(ymin = Mean_Acc - SE_Acc, ymax = Mean_Acc + SE_Acc),
                width = 0.025, linewidth = 0.6, position = pd, alpha = 0.75) +
  geom_line(linewidth = 1.1, position = pd) +
  geom_point(size = 3.5, position = pd) +
  geom_text(aes(label = sprintf("%.1f%%", Mean_Acc * 100),
                vjust = ifelse(Model == "CPC", -1.2, 1.8)),
            position = pd, size = 3.3, fontface = "bold", show.legend = FALSE) +
  scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75),
                     labels = c("0.00\n(Baseline)", "0.25", "0.50", "0.75"),
                     expand = expansion(mult = 0.1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0.44, 0.69),
                     breaks = seq(0.48, 0.68, by = 0.04)) +
  scale_color_manual(values = model_colors) +
  scale_shape_manual(values = model_shapes) +
  labs(
    x = expression(bold(paste("Confidence Threshold Multiplier (", C, ")"))),
    y = "Mean Prediction Accuracy"
  ) +
  theme_publication()

# ==============================================================================
# PLOT 2: Sample Starvation (Mean Predicted Volume +/- 1 SD Ribbon)
# ==============================================================================
plot_volume <- ggplot(df_summary, aes(x = Threshold, y = Mean_Matches, color = Model, fill = Model, shape = Model)) +
  geom_ribbon(aes(ymin = Mean_Matches - SD_Matches, ymax = Mean_Matches + SD_Matches),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1, linetype = "dashed") +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%.1f", Mean_Matches),
                vjust = ifelse(Model == "CPC", -1.1, 1.8)),
            size = 3.3, fontface = "bold", show.legend = FALSE) +
  scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75),
                     labels = c("0.00\n(Baseline)", "0.25", "0.50", "0.75"),
                     expand = expansion(mult = 0.1)) +
  scale_y_continuous(limits = c(0, 38), breaks = seq(0, 35, by = 5)) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  scale_shape_manual(values = model_shapes) +
  labs(
    x = expression(bold(paste("Confidence Threshold Multiplier (", C, ")"))),
    y = "Predicted Matches per Season"
  ) +
  theme_publication()

# ==============================================================================
# PLOT 3: Cross-Season Dispersion (Boxplot + Raw Seasons + Mean vs. Median)
# ==============================================================================
plot_variance <- ggplot(df_full, aes(x = factor(Threshold, labels = c("0.00", "0.25", "0.50", "0.75")),
                                     y = Accuracy, fill = Model, color = Model)) +
  geom_hline(yintercept = 0.50, linetype = "dotted", color = "grey40", linewidth = 0.7) +
  geom_boxplot(width = 0.55, alpha = 0.35, outlier.shape = NA,
               position = position_dodge(width = 0.68), linewidth = 0.6) +
  geom_point(position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.68, seed = 42),
             size = 2, alpha = 0.75) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.2, fill = "white",
               stroke = 1.1, position = position_dodge(width = 0.68), show.legend = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0.00, 1.02),
                     breaks = seq(0.00, 1.00, by = 0.20)) +
  scale_fill_manual(values = model_colors) +
  scale_color_manual(values = model_colors) +
  labs(
    x = expression(bold(paste("Confidence Threshold Multiplier (", C, ")"))),
    y = "Seasonal Prediction Accuracy"
  ) +
  theme_publication()

# ==============================================================================
# 4. Composite Publication Figure
# ==============================================================================
combined_figure <- (plot_accuracy + plot_volume) / plot_variance +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") +
  plot_layout(guides = "collect", heights = c(1, 1.15)) &
  theme(
    legend.position = "bottom",
    plot.tag        = element_text(face = "bold", size = 13)
  )

print(combined_figure)

# Save high-resolution vector PDFs for LaTeX inclusion
# ggsave("fig_accuracy_tradeoff.pdf", plot_accuracy, width = 6.5, height = 4.8, device = cairo_pdf)
# ggsave("fig_sample_volume.pdf", plot_volume, width = 6.5, height = 4.8, device = cairo_pdf)
# ggsave("fig_variance_boxplot.pdf", plot_variance, width = 8.0, height = 5.2, device = cairo_pdf)
#
pdf("fig_unified_panel.pdf", combined_figure, width = 12.0, height = 9.0)
combined_figure
dev.off()
