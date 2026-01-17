#' Basic SCM function for single unit
#'
#' @description Computes synthetic control weights for a single treated unit
#' @param Y N x T matrix where first row is treated unit
#' @param lambda Regularization parameter for numerical stability
#' @return List containing intercept (a) and weights (b)
#' @import limSolve
#' @export
scm <- function(Y, lambda = 1e-6) {
  # Y: N x T matrix, first row is treated unit
  # lambda: regularization parameter for numerical stability

  N <- nrow(Y)
  T <- ncol(Y)

  if (N < 2) {
    stop("Need at least 2 units (1 treated, 1 control)")
  }

  Y_treated <- Y[1, ]
  Y_untreated <- Y[-1, , drop = FALSE]

  # Demean
  Y_treated_mean <- mean(Y_treated)
  Y_untreated_mean <- rowMeans(Y_untreated)  # Mean for each control unit

  Y_demeaned <- Y_treated - Y_treated_mean
  X_demeaned <- t(Y_untreated) - rep(Y_untreated_mean, each = T)  # Demean correctly

  # Setup the constrained least squares problem
  A <- X_demeaned
  b <- Y_demeaned

  # Constraints: weights sum to 1 and are non-negative
  E <- matrix(1, nrow = 1, ncol = N-1)  # Equality constraint: sum = 1
  f <- 1
  G <- diag(N-1)  # Inequality constraints: w >= 0
  h <- rep(0, N-1)

  # Solve using limSolve
  tryCatch({
    sol <- lsei(A = A, B = b, E = E, F = f, G = G, H = h)
    b_hat <- sol$X
  }, error = function(e) {
    warning("SCM optimization failed, using equal weights")
    b_hat <- rep(1/(N-1), N-1)
  })

  # Calculate intercept
  weighted_control_mean <- sum(Y_untreated_mean * b_hat)
  a_hat <- Y_treated_mean - weighted_control_mean

  # Add zero for treated unit
  b_hat <- c(0, b_hat)

  return(list(a = a_hat, b = b_hat))
}

#' Batch SCM for all units
#'
#' @description Computes synthetic control weights for all units
#' @param Y N x T matrix of outcomes
#' @param verbose Print progress messages
#' @return List with intercepts (a) and weight matrix (B)
#' @export
scm_batch <- function(Y, verbose = FALSE) {
  # Y: N x T matrix
  # Returns: a_hat (Nx1), B_hat (NxN)

  N <- nrow(Y)
  T <- ncol(Y)

  a_hat <- rep(0, N)
  B_hat <- matrix(0, N, N)

  if (verbose) cat("Running SCM for", N, "units...\n")

  for (i in 1:N) {
    if (verbose && i %% 10 == 0) cat("  Processing unit", i, "/", N, "\n")

    # Create permuted matrix with unit i as treated
    Y_temp <- Y
    if (i != 1) {
      Y_temp[c(1, i), ] <- Y_temp[c(i, 1), ]
    }

    # Run SCM
    result <- scm(Y_temp)
    a_hat[i] <- result$a

    # Rearrange weights back to original order
    b_temp <- result$b
    b_rearranged <- rep(0, N)

    if (i == 1) {
      b_rearranged <- b_temp
    } else {
      # Unit i was swapped with unit 1
      b_rearranged[i] <- b_temp[1]  # Weight for treated (should be 0)
      b_rearranged[1] <- b_temp[i]  # Weight that unit 1 gets

      # Other weights
      if (N > 2) {
        other_idx_original <- setdiff(1:N, c(1, i))
        other_idx_temp <- setdiff(1:N, c(1, i))
        b_rearranged[other_idx_original] <- b_temp[other_idx_temp]
      }
    }

    B_hat[i, ] <- b_rearranged
  }

  return(list(a = a_hat, B = B_hat))
}

#' Safe matrix inversion with regularization
#'
#' @description Safely inverts a matrix using regularization if needed
#' @param M Matrix to invert
#' @param tol Tolerance for condition number check
#' @return Inverted matrix
#' @import MASS
#' @export
safe_solve <- function(M, tol = 1e-10) {
  # Check condition number
  if (rcond(M) < tol) {
    # Add regularization
    M_reg <- M + diag(nrow(M)) * tol

    # Try regularized version first
    tryCatch({
      return(solve(M_reg))
    }, error = function(e) {
      # Use pseudoinverse as last resort
      warning("Using pseudoinverse for singular matrix")
      return(ginv(M))
    })
  } else {
    return(solve(M))
  }
}

#' Standard SCM without spillovers
#'
#' @description Computes standard synthetic control for comparison
#' @param Y_pre N x T pre-treatment matrix
#' @param Y_post N x S post-treatment matrix
#' @return Synthetic control values for all periods
#' @export
vanilla_scm <- function(Y_pre, Y_post) {
  # Standard synthetic control without spillovers
  scm_result <- scm_batch(Y_pre)
  a_hat <- scm_result$a[1]
  b_hat <- scm_result$B[1, ]

  # Synthetic control for all periods
  Y_all <- cbind(Y_pre, Y_post)
  synthetic_control <- a_hat + b_hat %*% Y_all

  return(synthetic_control)
}
