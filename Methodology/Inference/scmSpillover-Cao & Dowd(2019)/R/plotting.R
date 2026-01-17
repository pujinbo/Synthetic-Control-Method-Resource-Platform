
#' Generate all plots for SCM analysis
#'
#' @param result Output from run_scm_spillover
#' @param start_year First year of treatment period
#' @param unit_name Name of treated unit (default "Treated Unit")
#' @param outcome_label Label for outcome variable (default "Outcome")
#' @param treatment_label Label for treatment (default "Treatment")
#' @param show_ci Logical, whether to show confidence bands (default TRUE)
#' @export
plot_all <- function(result,
                     start_year = NULL,
                     unit_name = "Treated Unit",
                     outcome_label = "Outcome",
                     treatment_label = "Treatment",
                     show_ci = TRUE) {



  # Automatically generate the year or period
  n_post <- length(result$spillover_effects)
  if (is.null(start_year)) {
    periods <- 1:n_post
    x_label <- "Post-treatment Period"
  } else {
    periods <- start_year:(start_year + n_post - 1)
    x_label <- "Year"
  }

  # Plot 1: Method Comparison
  df_compare <- data.frame(
    Period = rep(periods, 2),
    Effect = c(result$spillover_effects, result$vanilla_effects),
    Method = factor(rep(c("Spillover-Adjusted", "Standard SCM"), each = n_post))
  )

  p1 <- ggplot(df_compare, aes(x = Period, y = Effect, color = Method)) +
    geom_line(aes(linetype = Method), size = 1.2) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
    scale_color_manual(values = c("Spillover-Adjusted" = "#0072B2",
                                  "Standard SCM" = "#D55E00")) +
    labs(title = paste("Treatment Effects Comparison:", unit_name),
         subtitle = "Spillover-Adjusted vs Standard Synthetic Control",
         x = x_label,
         y = paste("Treatment effect on", outcome_label)) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Plot 2: Confidence interval (improved version)
  df_ci <- data.frame(
    Period = periods,
    Effect = result$spillover_effects,
    Lower = result$ci_lower,
    Upper = result$ci_upper
  )

  p2 <- ggplot(df_ci, aes(x = Period, y = Effect)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.6)

  # Add confidence band only if show_ci is TRUE
  if (show_ci) {
    p2 <- p2 + geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = "#0072B2")
  }

  p2 <- p2 +
    geom_line(color = "#0072B2", size = 1.2) +
    geom_point(size = 3, color = "#0072B2") +
    labs(title = paste("Spillover-Adjusted Treatment Effects:", unit_name),
         subtitle = ifelse(show_ci, "With 95% Confidence Intervals", "Point Estimates"),
         x = x_label,
         y = paste("Treatment effect on", outcome_label)) +
    theme_minimal()

  # Plot 3: Time series (actual vs synthetic)
  n_total <- ncol(result$data$Y0) + ncol(result$data$Y1)

  if (!is.null(start_year)) {
    n_pre <- ncol(result$data$Y0)
    periods_full <- (start_year - n_pre):(start_year + n_post - 1)
    treatment_time <- start_year - 0.5
  } else {
    periods_full <- 1:n_total
    treatment_time <- ncol(result$data$Y0) + 0.5
  }

  Y_all <- cbind(result$data$Y0, result$data$Y1)

  df_series <- data.frame(
    Period = rep(periods_full, 2),
    Value = c(
      Y_all[1, ],  # actual value
      c(result$data$Y0[1, ], result$synthetic)  # Synthetic control
    ),
    Type = factor(rep(c(paste("Actual", unit_name),
                        "Synthetic Control (Spillover-Adjusted)"),
                      each = n_total))
  )

  p3 <- ggplot(df_series, aes(x = Period, y = Value, color = Type)) +
    geom_line(size = 1) +
    geom_vline(xintercept = treatment_time, linetype = "dashed", alpha = 0.5) +
    annotate("text",
             x = treatment_time - 1,
             y = max(df_series$Value) * 0.95,
             label = paste(treatment_label, "->"),
             hjust = 1,
             size = 3.5) +
    scale_color_manual(values = c("#E69F00", "#0072B2")) +
    labs(title = paste("Actual vs Synthetic Control:", unit_name),
         x = x_label,
         y = outcome_label) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Print plots
  print(p1)
  cat("Showing plot 1 of 3...\n")
  invisible(readline("Press Enter for next plot..."))

  print(p2)
  cat("Showing plot 2 of 3...\n")
  invisible(readline("Press Enter for next plot..."))

  print(p3)
  cat("Showing plot 3 of 3.\n")

  # Return all chart objects
  invisible(list(p1 = p1, p2 = p2, p3 = p3))
}

#' Plot treatment effects only
#'
#' @param result Output from run_scm_spillover
#' @param start_year First year of treatment period
#' @param unit_name Name of treated unit (default "Treated Unit")
#' @param outcome_label Label for outcome variable (default "Outcome")
#' @param show_ci Logical, whether to show confidence bands (default TRUE)
#' @param show_vanilla Logical, whether to show vanilla SCM comparison (default FALSE)
#' @export
plot_effects <- function(result,
                         start_year = NULL,
                         unit_name = "Treated Unit",
                         outcome_label = "Outcome",
                         show_ci = TRUE,
                         show_vanilla = FALSE) {



  # Generate periods
  n_post <- length(result$spillover_effects)
  if (is.null(start_year)) {
    periods <- 1:n_post
    x_label <- "Post-treatment Period"
  } else {
    periods <- start_year:(start_year + n_post - 1)
    x_label <- "Year"
  }

  # Create base dataframe
  df <- data.frame(
    Period = periods,
    Effect = result$spillover_effects,
    Lower = result$ci_lower,
    Upper = result$ci_upper
  )

  # Start building the plot
  p <- ggplot(df, aes(x = Period, y = Effect)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.6)

  # Add confidence band if requested
  if (show_ci) {
    p <- p + geom_ribbon(aes(ymin = Lower, ymax = Upper),
                         alpha = 0.2, fill = "#0072B2")
  }

  # Add spillover-adjusted effects
  p <- p +
    geom_line(color = "#0072B2", size = 1.2) +
    geom_point(size = 3, color = "#0072B2")

  # Add vanilla SCM if requested
  if (show_vanilla) {
    df_vanilla <- data.frame(
      Period = periods,
      Effect = result$vanilla_effects
    )
    p <- p +
      geom_line(data = df_vanilla, aes(x = Period, y = Effect),
                color = "#D55E00", size = 1.2, linetype = "dashed") +
      geom_point(data = df_vanilla, aes(x = Period, y = Effect),
                 size = 2.5, color = "#D55E00")
  }

  # Add labels and theme
  subtitle <- paste0(
    ifelse(show_ci, "With 95% Confidence Intervals", "Point Estimates"),
    ifelse(show_vanilla, " (Dashed line: Standard SCM)", "")
  )

  p <- p +
    labs(title = paste("Spillover-Adjusted Treatment Effects:", unit_name),
         subtitle = subtitle,
         x = x_label,
         y = paste("Treatment effect on", outcome_label)) +
    theme_minimal()

  return(p)
}


#' Quick plot for treatment effects with confidence intervals
#'
#' @param result Output from run_scm_spillover
#' @param start_year First year of treatment period
#' @param unit_name Name of treated unit (default "Treated Unit")
#' @param outcome_label Label for outcome variable (default "Outcome")
#' @param show_ci Logical, whether to show confidence bands (default TRUE)
#' @param show_vanilla Logical, whether to show vanilla SCM comparison (default FALSE)
#' @return ggplot object (invisibly)
#' @export
qplot_effects <- function(result,
                          start_year = NULL,
                          unit_name = "Treated Unit",
                          outcome_label = "Outcome",
                          show_ci = TRUE,
                          show_vanilla = FALSE) {

  p <- plot_effects(result, start_year, unit_name, outcome_label, show_ci, show_vanilla)
  print(p)
  invisible(p)
}

#' Quick plot for method comparison (Spillover vs Vanilla)
#'
#' @param result Output from run_scm_spillover
#' @param start_year First year of treatment period
#' @param unit_name Name of treated unit (default "Treated Unit")
#' @param outcome_label Label for outcome variable (default "Outcome")
#' @return ggplot object (invisibly)
#' @export
qplot_compare <- function(result,
                          start_year = NULL,
                          unit_name = "Treated Unit",
                          outcome_label = "Outcome") {



  # Generate periods
  n_post <- length(result$spillover_effects)
  if (is.null(start_year)) {
    periods <- 1:n_post
    x_label <- "Post-treatment Period"
  } else {
    periods <- start_year:(start_year + n_post - 1)
    x_label <- "Year"
  }

  # Create dataframe
  df_compare <- data.frame(
    Period = rep(periods, 2),
    Effect = c(result$spillover_effects, result$vanilla_effects),
    Method = factor(rep(c("Spillover-Adjusted", "Standard SCM"), each = n_post))
  )

  p <- ggplot(df_compare, aes(x = Period, y = Effect, color = Method)) +
    geom_line(aes(linetype = Method), size = 1.2) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
    scale_color_manual(values = c("Spillover-Adjusted" = "#0072B2",
                                  "Standard SCM" = "#D55E00")) +
    labs(title = paste("Treatment Effects Comparison:", unit_name),
         subtitle = "Spillover-Adjusted vs Standard Synthetic Control",
         x = x_label,
         y = paste("Treatment effect on", outcome_label)) +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p)
  invisible(p)
}

#' Quick plot for actual vs synthetic control time series
#'
#' @param result Output from run_scm_spillover
#' @param start_year First year of treatment period
#' @param unit_name Name of treated unit (default "Treated Unit")
#' @param outcome_label Label for outcome variable (default "Outcome")
#' @param treatment_label Label for treatment (default "Treatment")
#' @return ggplot object (invisibly)
#' @export
qplot_series <- function(result,
                         start_year = NULL,
                         unit_name = "Treated Unit",
                         outcome_label = "Outcome",
                         treatment_label = "Treatment") {



  # Calculate periods
  n_post <- length(result$spillover_effects)
  n_total <- ncol(result$data$Y0) + ncol(result$data$Y1)

  if (!is.null(start_year)) {
    n_pre <- ncol(result$data$Y0)
    periods_full <- (start_year - n_pre):(start_year + n_post - 1)
    treatment_time <- start_year - 0.5
    x_label <- "Year"
  } else {
    periods_full <- 1:n_total
    treatment_time <- ncol(result$data$Y0) + 0.5
    x_label <- "Period"
  }

  # Combine data
  Y_all <- cbind(result$data$Y0, result$data$Y1)

  df_series <- data.frame(
    Period = rep(periods_full, 2),
    Value = c(
      Y_all[1, ],  # actual value
      c(result$data$Y0[1, ], result$synthetic)  # Synthetic control
    ),
    Type = factor(rep(c(paste("Actual", unit_name),
                        "Synthetic Control (Spillover-Adjusted)"),
                      each = n_total))
  )

  p <- ggplot(df_series, aes(x = Period, y = Value, color = Type)) +
    geom_line(size = 1) +
    geom_vline(xintercept = treatment_time, linetype = "dashed", alpha = 0.5) +
    annotate("text",
             x = treatment_time - 1,
             y = max(df_series$Value) * 0.95,
             label = paste(treatment_label, "->"),
             hjust = 1,
             size = 3.5) +
    scale_color_manual(values = c("#E69F00", "#0072B2")) +
    labs(title = paste("Actual vs Synthetic Control:", unit_name),
         x = x_label,
         y = outcome_label) +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p)
  invisible(p)
}

#' Quick plot with confidence intervals only
#'
#' @param result Output from run_scm_spillover
#' @param start_year First year of treatment period
#' @param unit_name Name of treated unit (default "Treated Unit")
#' @param outcome_label Label for outcome variable (default "Outcome")
#' @return ggplot object (invisibly)
#' @export
qplot_ci <- function(result,
                     start_year = NULL,
                     unit_name = "Treated Unit",
                     outcome_label = "Outcome") {

  p <- plot_effects(result, start_year, unit_name, outcome_label,
                    show_ci = TRUE, show_vanilla = FALSE)
  print(p)
  invisible(p)
}

#' Quick plot without confidence intervals
#'
#' @param result Output from run_scm_spillover
#' @param start_year First year of treatment period
#' @param unit_name Name of treated unit (default "Treated Unit")
#' @param outcome_label Label for outcome variable (default "Outcome")
#' @return ggplot object (invisibly)
#' @export
qplot_point <- function(result,
                        start_year = NULL,
                        unit_name = "Treated Unit",
                        outcome_label = "Outcome") {

  p <- plot_effects(result, start_year, unit_name, outcome_label,
                    show_ci = FALSE, show_vanilla = FALSE)
  print(p)
  invisible(p)
}

#' Quick plot all three main plots in sequence
#'
#' @param result Output from run_scm_spillover
#' @param start_year First year of treatment period
#' @param unit_name Name of treated unit (default "Treated Unit")
#' @param outcome_label Label for outcome variable (default "Outcome")
#' @param treatment_label Label for treatment (default "Treatment")
#' @param show_ci Logical, whether to show confidence bands (default TRUE)
#' @param pause Logical, whether to pause between plots (default TRUE)
#' @return List of ggplot objects (invisibly)
#' @export
qplot_all <- function(result,
                      start_year = NULL,
                      unit_name = "Treated Unit",
                      outcome_label = "Outcome",
                      treatment_label = "Treatment",
                      show_ci = TRUE,
                      pause = TRUE) {

  # Plot 1: Comparison
  cat("Plot 1 of 3: Method Comparison\n")
  p1 <- qplot_compare(result, start_year, unit_name, outcome_label)
  if (pause) {
    invisible(readline("Press Enter for next plot..."))
  }

  # Plot 2: Effects with/without CI
  cat("Plot 2 of 3: Treatment Effects\n")
  if (show_ci) {
    p2 <- qplot_ci(result, start_year, unit_name, outcome_label)
  } else {
    p2 <- qplot_point(result, start_year, unit_name, outcome_label)
  }
  if (pause) {
    invisible(readline("Press Enter for next plot..."))
  }

  # Plot 3: Time series
  cat("Plot 3 of 3: Time Series\n")
  p3 <- qplot_series(result, start_year, unit_name, outcome_label, treatment_label)

  invisible(list(comparison = p1, effects = p2, series = p3))
}

#' Save all plots to files
#'
#' @param result Output from run_scm_spillover
#' @param prefix File name prefix (default "scm")
#' @param path Directory to save plots (default current directory)
#' @param width Plot width in inches (default 10)
#' @param height Plot height in inches (default 6)
#' @param dpi Resolution (default 300)
#' @param ... Additional arguments passed to plotting functions
#' @return Character vector of saved file names (invisibly)
#' @export
save_all_plots <- function(result,
                           prefix = "scm",
                           path = ".",
                           width = 10,
                           height = 6,
                           dpi = 300,
                           ...) {



  # Create directory if it doesn't exist
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Create plots (without printing)
  p1 <- plot_effects(result, show_ci = TRUE, show_vanilla = FALSE, ...)
  p2 <- plot_effects(result, show_ci = FALSE, show_vanilla = FALSE, ...)
  p3 <- plot_effects(result, show_ci = TRUE, show_vanilla = TRUE, ...)

  # Create comparison and series plots
  p4 <- qplot_compare(result, ...)
  p5 <- qplot_series(result, ...)

  # Save files
  files <- c(
    file.path(path, paste0(prefix, "_effects_ci.png")),
    file.path(path, paste0(prefix, "_effects_point.png")),
    file.path(path, paste0(prefix, "_effects_comparison.png")),
    file.path(path, paste0(prefix, "_method_comparison.png")),
    file.path(path, paste0(prefix, "_time_series.png"))
  )

  ggsave(files[1], p1, width = width, height = height, dpi = dpi)
  ggsave(files[2], p2, width = width, height = height, dpi = dpi)
  ggsave(files[3], p3, width = width, height = height, dpi = dpi)
  ggsave(files[4], p4, width = width, height = height, dpi = dpi)
  ggsave(files[5], p5, width = width, height = height, dpi = dpi)

  cat("Saved plots to:\n")
  cat(paste("-", files), sep = "\n")

  invisible(files)
}
