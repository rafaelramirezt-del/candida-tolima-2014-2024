# ============================================================
# Utility functions for Figure 3 (violin panels + stats)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(rstatix)
  library(ggpubr)
  library(grid)
})

make_violin_panel <- function(df, xvar, yvar,
                              order_levels, pal,
                              x_label, y_label,
                              recode_map = NULL,
                              add_sig_brackets = TRUE,
                              add_subtitle = FALSE) {
  # Optional recode (e.g., Spanish -> English, label standardization)
  if (!is.null(recode_map)) {
    df[[xvar]] <- dplyr::recode(df[[xvar]], !!!recode_map)
  }

  df[[xvar]] <- factor(df[[xvar]], levels = order_levels)
  df <- df |> dplyr::filter(!is.na(.data[[xvar]]))

  upper <- ceiling(max(df[[yvar]], na.rm = TRUE)) + 5

  quart <- df |>
    dplyr::group_by(.data[[xvar]]) |>
    dplyr::summarise(
      n   = dplyr::n(),
      q1  = stats::quantile(.data[[yvar]], 0.25, na.rm = TRUE),
      med = stats::quantile(.data[[yvar]], 0.50, na.rm = TRUE),
      q3  = stats::quantile(.data[[yvar]], 0.75, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      x       = as.numeric(.data[[xvar]]),
      q1_lab  = q1  + upper*0.06,
      med_lab = med + upper*0.06,
      q3_lab  = q3  + upper*0.08
    )

  # x labels with n
  lab_x <- paste0(
    levels(df[[xvar]]),
    " (n=",
    quart$n[match(levels(df[[xvar]]), quart[[xvar]])],
    ")"
  )

  # Kruskal + eta2 + Dunn
  fml <- stats::as.formula(paste0(yvar, " ~ ", xvar))
  kw  <- rstatix::kruskal_test(df, fml)
  eff <- rstatix::kruskal_effsize(df, fml) |>
    dplyr::mutate(
      magnitude = dplyr::case_when(
        effsize < 0.01 ~ "very small",
        effsize < 0.06 ~ "small",
        effsize < 0.14 ~ "medium",
        TRUE           ~ "large"
      )
    )

  dunn <- rstatix::dunn_test(df, fml, p.adjust.method = "bonferroni") |>
    dplyr::mutate(
      group1 = stringr::str_squish(as.character(group1)),
      group2 = stringr::str_squish(as.character(group2)),
      p.adj.signif = dplyr::case_when(
        p.adj < 0.001 ~ "***",
        p.adj < 0.01  ~ "**",
        p.adj < 0.05  ~ "*",
        TRUE          ~ "ns"
      )
    )

  sig_pairs <- dunn |> dplyr::filter(p.adj < 0.05)
  n_sig <- nrow(sig_pairs)

  # Brackets above violins
  y_max <- max(df[[yvar]], na.rm = TRUE)
  bracket_start <- y_max * 1.18
  bracket_step  <- y_max * 0.06

  top_lim <- if (add_sig_brackets && n_sig > 0) {
    bracket_start + (n_sig * bracket_step) + y_max*0.10
  } else {
    y_max * 1.20
  }

  if (add_sig_brackets && n_sig > 0) {
    sig_pairs$y.position <- bracket_start + (seq_len(n_sig) - 1) * bracket_step
  }

  y_breaks <- seq(0, ceiling(top_lim/10)*10, 10)
  seg_w <- 0.28

  p <- ggplot2::ggplot() +
    ggplot2::geom_jitter(
      data = df, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]]),
      width = 0.12, alpha = 0.25, size = 1, color = "black"
    ) +
    ggplot2::geom_violin(
      data = df, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], fill = .data[[xvar]]),
      width = 0.9, trim = FALSE, scale = "width",
      color = "black", alpha = 0.70
    ) +
    ggplot2::geom_segment(data = quart, ggplot2::aes(x = x-seg_w, xend = x+seg_w, y = q1,  yend = q1),  linewidth = 0.9) +
    ggplot2::geom_segment(data = quart, ggplot2::aes(x = x-seg_w, xend = x+seg_w, y = med, yend = med), linewidth = 1.4) +
    ggplot2::geom_segment(data = quart, ggplot2::aes(x = x-seg_w, xend = x+seg_w, y = q3,  yend = q3),  linewidth = 0.9) +
    ggplot2::geom_text(data = quart, ggplot2::aes(x = .data[[xvar]], y = q1_lab,  label = round(q1)),  size = 5) +
    ggplot2::geom_text(data = quart, ggplot2::aes(x = .data[[xvar]], y = med_lab, label = round(med)), size = 6, fontface = "bold") +
    ggplot2::geom_text(data = quart, ggplot2::aes(x = .data[[xvar]], y = q3_lab,  label = round(q3)),  size = 5) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::scale_x_discrete(labels = lab_x) +
    ggplot2::scale_y_continuous(
      limits = c(0, top_lim),
      breaks = y_breaks,
      expand = ggplot2::expansion(mult = c(0.06, 0.12))
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      subtitle = if (add_subtitle) {
        sprintf("H = %.2f, p = %.3g, eta² = %.3f (%s)",
                kw$statistic, kw$p, eff$effsize, eff$magnitude)
      } else NULL
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_family = "Arial") +
    ggplot2::theme(
      panel.grid        = ggplot2::element_blank(),
      axis.line.x       = ggplot2::element_line(color="black"),
      axis.line.y       = ggplot2::element_line(color="black"),
      axis.ticks        = ggplot2::element_line(color="black"),
      axis.ticks.length = grid::unit(5, "pt"),
      axis.text.x       = ggplot2::element_text(
        angle = 45, hjust = 1, size = 13,
        color = "black", margin = ggplot2::margin(t = 10)
      ),
      axis.text.y       = ggplot2::element_text(
        size = 12, color = "black",
        margin = ggplot2::margin(r = 10)
      ),
      axis.title.x      = ggplot2::element_text(size = 16, face="bold", color="black"),
      axis.title.y      = ggplot2::element_text(size = 16, face="bold", color="black"),
      plot.margin       = ggplot2::margin(25, 25, 80, 55),
      legend.position   = "none"
    )

  if (add_sig_brackets && n_sig > 0) {
    p <- p + ggpubr::stat_pvalue_manual(
      data         = sig_pairs,
      label        = "p.adj.signif",
      xmin         = "group1",
      xmax         = "group2",
      y.position   = "y.position",
      tip.length   = 0.01,
      bracket.size = 0.6,
      size         = 4.5,
      color        = "black"
    )
  }

  list(plot = p, quart = quart, kw = kw, dunn = dunn, eff = eff, sig_pairs = sig_pairs, df = df)
}
