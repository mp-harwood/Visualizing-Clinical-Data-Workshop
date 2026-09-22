# functions.R
#
# This file contains reusable plotting functions.
# Keeping plotting code here separates visualisation logic
# from the Shiny user interface and server logic.
#
# Each function reads its own data internally (rather than relying on
# global variables defined by whichever app.R sources this file). Shiny
# often evaluates app.R in its own private environment, so a plain
# top-level `adlbh <- ...` in app.R is not guaranteed to be visible to
# functions defined via source() here -- self-loading avoids that
# entirely, at the small cost of re-reading the xpt files on each call.

library(ggplot2)
library(dplyr)
library(crosstalk)

# Treatment-arm colors, shared across every plot in this template.
trt_colors <- c(
  "Placebo"              = "#7F7F7F",
  "Xanomeline Low Dose"  = "#0072B2",
  "Xanomeline High Dose" = "#E69F00"
)

# All treatment arms available in the sample data, in display order.
all_treatments <- names(trt_colors)


create_meanplot <- function(param = "EOS", treatments = all_treatments) {
  adlbh <- haven::read_xpt("data/adlbh.xpt")

  keep_visits <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")
  nudge_vals  <- c("Placebo" = -0.3, "Xanomeline Low Dose" = 0.0, "Xanomeline High Dose" = 0.3)
  xticks      <- c(0, 2, 4, 8, 12)

  chg_by_visit <- adlbh |>
    mutate(AVISIT = sub("^\\s+", "", AVISIT)) |>
    filter(PARAMCD == param) |>
    mutate(CHG = AVAL - BASE) |>
    filter(!is.na(TRTA), TRTA %in% treatments) |>
    filter(AVISIT %in% keep_visits) |>
    mutate(
      TRTA = factor(TRTA, levels = treatments),
      week_n = case_when(
        grepl("^\\s*baseline\\s*$", AVISIT, ignore.case = TRUE) ~ 0,
        grepl("^\\s*week\\b", AVISIT, ignore.case = TRUE) ~
          suppressWarnings(as.numeric(sub("^\\s*Week\\s*([0-9]+).*$", "\\1", AVISIT, ignore.case = TRUE))),
        TRUE ~ NA_real_
      )
    )

  param_label <- adlbh |> distinct(PARAMCD, PARAM) |> filter(PARAMCD == param) |> pull(PARAM)

  visit_summary <- chg_by_visit |>
    group_by(TRTA, week_n) |>
    summarise(n = n(), mean_chg = mean(CHG, na.rm = TRUE), sd_chg = sd(CHG, na.rm = TRUE), .groups = "drop") |>
    mutate(
      se_chg   = ifelse(is.na(sd_chg / sqrt(n)), 0, sd_chg / sqrt(n)),
      mean_chg = ifelse(week_n == 0, 0, mean_chg),
      se_chg   = ifelse(week_n == 0, 0, se_chg),
      nudge    = nudge_vals[as.character(TRTA)]
    ) |>
    arrange(week_n)

  ggplot(
    visit_summary,
    aes(x = ifelse(week_n == 0, week_n, week_n + nudge), y = mean_chg, color = TRTA, linetype = TRTA, group = TRTA)
  ) +
    geom_line(linewidth = 1, alpha = 0.7) +
    geom_point(size = 3, alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_chg - se_chg, ymax = mean_chg + se_chg), width = 0.5, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 0.6) +
    labs(
      x = "Weeks Since Baseline Measurement",
      y = paste0(param_label, "\nChange from Baseline (Mean \u00b1 SE)")
    ) +
    scale_color_manual(values = trt_colors[treatments]) +
    scale_x_continuous(breaks = xticks, expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, max(xticks) + 0.5), clip = "off") +
    theme_classic(base_size = 16) +
    theme(axis.title = element_text(face = "bold"), legend.position = "none")
}


create_box_plot <- function(
    param = "ALB",
    treatments = all_treatments,
    crosstalk_group = NULL
) {
  adlbc <- haven::read_xpt("data/adlbc.xpt")
  adsl  <- haven::read_xpt("data/adsl.xpt")

  keep_visits <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")

  lab_by_visit <- adlbc |>
    mutate(AVISIT = sub("^\\s+", "", AVISIT)) |>  # the pilot data pads AVISIT with leading spaces
    filter(PARAMCD == param, is.na(ANL01FL) | ANL01FL == "Y" | ABLFL == "Y") |>
    mutate(AVAL = suppressWarnings(as.numeric(AVAL))) |>
    filter(!is.na(AVAL)) |>
    left_join(adsl, by = "USUBJID", suffix = c(".adlb", ".adsl")) |>
    filter(!is.na(TRTA), TRTA %in% treatments) |>
    filter(AVISIT %in% keep_visits) |>
    mutate(
      TRTA   = factor(TRTA, levels = treatments),
      AVISIT = factor(AVISIT, levels = keep_visits)
    )

  # When a crosstalk group is supplied, wrap the data so clicking a point
  # highlights the matching subject's row(s) in a linked DT table. Points
  # share USUBJID with the table's key, so this works even though each
  # subject contributes multiple points here (one per visit/facet).
  plot_data <- if (!is.null(crosstalk_group)) {
    crosstalk::SharedData$new(lab_by_visit, key = ~USUBJID, group = crosstalk_group)
  } else {
    lab_by_visit
  }

  param_label <- adlbc |> distinct(PARAMCD, PARAM) |> filter(PARAMCD == param) |> pull(PARAM)

  ggplot(plot_data, aes(x = TRTA, y = AVAL)) +
    geom_boxplot(aes(color = TRTA), show.legend = FALSE, outlier.shape = NA) +
    geom_point(
      aes(color = TRTA),
      position = position_jitter(width = 0.2), alpha = 0.4, size = 1, show.legend = FALSE
    ) +
    stat_summary(
      fun = median, geom = "crossbar",
      aes(linetype = "Median", color = TRTA),
      width = 0.7, fatten = 0.5, linewidth = 1, show.legend = FALSE
    ) +
    geom_line(
      data = lab_by_visit,
      aes(x = as.numeric(TRTA), y = min(AVAL)),
      show.legend = TRUE
    ) +
    facet_wrap(~ AVISIT, nrow = 1) +
    labs(x = NULL, y = param_label[1]) +
    scale_color_manual(values = trt_colors[treatments]) +
    theme_classic(base_size = 16) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      axis.title       = element_text(face = "bold"),
      strip.background = element_blank(),
      legend.position  = "top",
      legend.key       = element_blank(),
      legend.background = element_blank(),
      legend.title     = element_blank()
    ) +
    guides(color = "none", linetype = guide_legend(override.aes = list(color = "black")))
}
