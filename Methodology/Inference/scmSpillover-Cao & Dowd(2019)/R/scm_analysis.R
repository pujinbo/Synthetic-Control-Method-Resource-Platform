#' Run Complete SCM Analysis with Spillover Effects
#'
#' @param data Matrix or data frame (time x units)
#' @param treatment_start Integer, first treatment period (row number)
#' @param treated_unit Integer, column index of treated unit (default = 1)
#' @param affected_units Vector of column indices for all affected units (including treated)
#' @param verbose Logical, print progress
#' @return List with all results
#' @export
run_scm_spillover <- function(data,
                              treatment_start,
                              treated_unit = 1,
                              affected_units = NULL,
                              verbose = TRUE) {

  # Convert data format
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }

  # Make sure the treated unit comes first.
  Y <- t(data)

  # If treated_unit is not 1, swap rows.
  if (treated_unit != 1) {
    Y_temp <- Y[1, ]
    Y[1, ] <- Y[treated_unit, ]
    Y[treated_unit, ] <- Y_temp

    # Adjust the index of affected_units
    if (!is.null(affected_units)) {
      affected_units <- sapply(affected_units, function(x) {
        if (x == 1) return(treated_unit)
        if (x == treated_unit) return(1)
        return(x)
      })
    }
  }

  n <- nrow(Y)
  t_pre <- treatment_start - 1

  Y0 <- Y[, 1:t_pre]
  Y1 <- Y[, treatment_start:ncol(Y)]

  # By default, only the treated unit is affected.
  if (is.null(affected_units)) {
    affected_units <- 1
  }


  ind <- rep(0, n)
  ind[affected_units] <- 1
  A <- diag(n)[, which(as.logical(ind)), drop = FALSE]

  if (verbose) {
    cat("\n========================================\n")
    cat("    SCM with Spillover Analysis\n")
    cat("========================================\n")
    cat("Total units:", n, "\n")
    cat("Treated unit:", treated_unit, "\n")
    cat("Pre-treatment periods:", t_pre, "\n")
    cat("Post-treatment periods:", ncol(Y1), "\n")
    cat("Potentially affected units:", sum(ind), "\n\n")
  }


  sp_result <- sp_estimation(Y0, Y1, A, verbose)

  # Comparison of Vanilla SCM
  vanilla_result <- vanilla_scm(Y0, Y1)
  vanilla_effects <- Y[1, treatment_start:ncol(Y)] -
    vanilla_result[treatment_start:length(vanilla_result)]


  se_est <- sd(sp_result$alpha) * 0.5
  ci_lower <- sp_result$alpha - 1.96 * se_est
  ci_upper <- sp_result$alpha + 1.96 * se_est

  # Return result
  structure(
    list(
      spillover_effects = sp_result$alpha,
      vanilla_effects = as.numeric(vanilla_effects),
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      synthetic = sp_result$synthetic,
      data = list(Y0 = Y0, Y1 = Y1, A = A),
      params = list(
        treated_unit = treated_unit,
        treatment_start = treatment_start,
        affected_units = affected_units
      )
    ),
    class = "scm_spillover"
  )
}
