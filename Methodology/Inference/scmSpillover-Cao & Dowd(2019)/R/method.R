#' Print method for scm_spillover objects
#' @param x An object of class scm_spillover
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @export
print.scm_spillover <- function(x, ...) {
  cat("\nSCM with Spillover Effects Results\n")
  cat("===================================\n")
  cat("Average spillover-adjusted effect:", round(mean(x$spillover_effects), 3), "\n")
  cat("Average standard SCM effect:", round(mean(x$vanilla_effects), 3), "\n")
  cat("Difference:", round(mean(x$spillover_effects - x$vanilla_effects), 3), "\n")
  cat("\nNumber of post-treatment periods:", length(x$spillover_effects), "\n")
  cat("Number of affected units:", ncol(x$data$A), "\n")
  invisible(x)
}

#' Summary method for scm_spillover objects
#' @param object An object of class scm_spillover
#' @param ... Additional arguments (not used)
#' @return Invisibly returns a data frame with detailed results
#' @export
summary.scm_spillover <- function(object, ...) {
  cat("\nDetailed SCM Analysis Summary\n")
  cat("==============================\n\n")

  results_df <- data.frame(
    Period = 1:length(object$spillover_effects),
    Spillover_Effect = round(object$spillover_effects, 3),
    Vanilla_Effect = round(object$vanilla_effects, 3),
    Difference = round(object$spillover_effects - object$vanilla_effects, 3),
    CI_Lower = round(object$ci_lower, 3),
    CI_Upper = round(object$ci_upper, 3)
  )

  print(results_df)

  cat("\n--- Statistical Summary ---\n")
  cat("Mean spillover effect:", round(mean(object$spillover_effects), 3), "\n")
  cat("Mean vanilla effect:", round(mean(object$vanilla_effects), 3), "\n")
  cat("SD of spillover effects:", round(stats::sd(object$spillover_effects), 3), "\n")

  invisible(results_df)
}

#' Plot method for scm_spillover objects
#' @param x An object of class scm_spillover
#' @param type Type of plot: "effects" or "all"
#' @param ... Additional arguments passed to plotting functions
#' @return A ggplot object (invisibly for type="all")
#' @export
plot.scm_spillover <- function(x, type = "effects", ...) {
  if (type == "effects") {
    plot_effects(x, ...)
  } else if (type == "all") {
    plot_all(x, ...)
  }
}
