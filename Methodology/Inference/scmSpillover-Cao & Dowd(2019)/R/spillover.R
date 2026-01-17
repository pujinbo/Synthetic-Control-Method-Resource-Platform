#' Spillover-adjusted SCM estimation
#'
#' @description Estimates treatment effects accounting for spillovers
#' @param Y_pre N x T pre-treatment matrix
#' @param Y_post N x S post-treatment matrix
#' @param A N x k spillover structure matrix
#' @param verbose Print progress messages
#' @return List containing treatment effects and other results
#' @export
sp_estimation <- function(Y_pre, Y_post, A, verbose = FALSE) {
  N <- nrow(Y_pre)
  S <- ncol(Y_post)
  k <- ncol(A)

  if (verbose) cat("Calculating synthetic control weights...\n")

  # Get synthetic control weights
  scm_result <- scm_batch(Y_pre, verbose = verbose)
  a_hat <- scm_result$a
  B_hat <- scm_result$B

  # Calculate matrices
  I_B <- diag(N) - B_hat
  M_hat <- t(I_B) %*% I_B + diag(N) * 1e-8

  # Check invertibility
  AMA <- t(A) %*% M_hat %*% A
  if (rcond(AMA) < 1e-10) {
    warning("A'MA is near-singular")
  }

  # Storage for results
  gamma_matrix <- matrix(0, k, S)  # k parameters x S periods
  alpha1_vec <- numeric(S)         # Treatment effects on California
  synthetic_control_sp <- numeric(S)

  # Estimate for each period
  for (s in 1:S) {
    if (verbose && s %% 3 == 0) cat("  Period", s, "/", S, "\n")

    Y_Ts <- Y_post[, s]

    # Calculate residual
    residual <- I_B %*% Y_Ts - a_hat

    # Solve for gamma (the k parameters)
    gamma_hat <- solve(AMA) %*% (t(A) %*% t(I_B) %*% residual)
    gamma_matrix[, s] <- as.vector(gamma_hat)

    # Calculate alpha for ALL units (N x 1)
    alpha_full <- A %*% gamma_hat

    # Extract California's effect (unit 1)
    alpha1_vec[s] <- alpha_full[1]

    # Synthetic control
    synthetic_control_sp[s] <- Y_Ts[1] - alpha1_vec[s]
  }

  return(list(
    alpha = alpha1_vec,           # California treatment effects
    gamma_all = gamma_matrix,     # All k parameters
    synthetic = synthetic_control_sp,
    a_hat = a_hat,
    B_hat = B_hat,
    M_hat = M_hat,
    A = A                         # Keep A for reference
  ))
}

#' Andrews test for treatment effects
#'
#' @description Tests significance of treatment effects
#' @param Y0 N x T pre-treatment matrix
#' @param Y1 N x 1 post-treatment vector (single period)
#' @param A N x k spillover structure matrix
#' @param C Constraint matrix (default tests first unit)
#' @param alpha_sig Significance level
#' @return List with estimates, p-values, and confidence intervals
#' @export
sp_andrews_te <- function(Y0, Y1, A, C = NULL, alpha_sig = 0.05) {
  # Y0: N x T pre-treatment
  # Y1: N x 1 post-treatment (single period)
  # A: N x k spillover structure
  # C: constraint matrix (default tests first unit)

  N <- nrow(Y0)
  T <- ncol(Y0)
  k <- ncol(A)

  # Default: test treatment effect on unit 1
  if (is.null(C)) {
    C <- matrix(c(1, rep(0, k-1)), nrow = 1)
  }

  # Ensure C is a matrix
  if (is.vector(C)) {
    C <- matrix(C, nrow = 1)
  }

  # Get synthetic control weights
  scm_result <- scm_batch(Y0)
  a_hat <- scm_result$a
  B_hat <- scm_result$B

  # Calculate matrices
  I_B <- diag(N) - B_hat
  M_hat <- t(I_B) %*% I_B + diag(N) * 1e-8

  # Estimate effects
  AMA <- t(A) %*% M_hat %*% A
  AMA_inv <- safe_solve(AMA)

  gamma_hat <- AMA_inv %*% (t(A) %*% t(I_B) %*% (I_B %*% Y1 - a_hat))
  alpha_hat <- A %*% gamma_hat

  # Test statistic
  Ca <- C %*% alpha_hat
  P <- as.numeric(t(Ca) %*% Ca)

  # G matrix for distribution
  G_hat <- A %*% AMA_inv %*% t(A) %*% t(I_B)

  # Compute test statistic for each pre-treatment period
  P_t <- rep(0, T)
  u_hat_vec <- rep(0, T)

  for (t in 1:T) {
    u_t <- Y0[, t] - (a_hat + B_hat %*% Y0[, t])
    CGu <- C %*% G_hat %*% u_t
    P_t[t] <- as.numeric(t(CGu) %*% CGu)
    u_hat_vec[t] <- as.numeric(CGu)
  }

  # P-value
  p_value <- mean(P <= P_t)

  # Confidence interval
  lb <- alpha_hat[1] + quantile(u_hat_vec, alpha_sig/2)
  ub <- alpha_hat[1] + quantile(u_hat_vec, 1 - alpha_sig/2)

  return(list(
    Estimates = as.vector(alpha_hat),
    pval = p_value,
    CI = c(lb, ub),
    test_stat = P
  ))
}

#' Test for spillover effects
#'
#' @description Tests whether spillover effects are significant
#' @param Y0 N x T pre-treatment matrix
#' @param Y1 N x 1 post-treatment vector
#' @param A N x k spillover structure matrix
#' @param alpha_sig Significance level
#' @return List with test results
#' @export
sp_andrews_spillover <- function(Y0, Y1, A, alpha_sig = 0.05) {
  # Test H0: no spillover effects (all spillover coefficients = 0)

  N <- nrow(Y0)
  T <- ncol(Y0)
  k <- ncol(A)

  if (k <= 1) {
    warning("No spillover units to test")
    return(list(reject = FALSE, p_value = 1, test_stat = 0))
  }

  # Construct C matrix to test spillover effects
  # We test whether alpha_2, ..., alpha_k are all zero
  C <- matrix(0, nrow = k-1, ncol = k)
  for (i in 1:(k-1)) {
    C[i, i+1] <- 1
  }
  d <- rep(0, k-1)

  # Get synthetic control weights
  scm_result <- scm_batch(Y0)
  a_hat <- scm_result$a
  B_hat <- scm_result$B

  # Calculate matrices
  I_B <- diag(N) - B_hat
  M_hat <- t(I_B) %*% I_B + diag(N) * 1e-8

  # Estimate effects
  AMA <- t(A) %*% M_hat %*% A
  AMA_inv <- safe_solve(AMA)

  gamma_hat <- AMA_inv %*% (t(A) %*% t(I_B) %*% (I_B %*% Y1 - a_hat))
  alpha_hat <- A %*% gamma_hat

  # Test statistic
  diff <- C %*% alpha_hat - d
  P <- as.numeric(t(diff) %*% diff)

  # G matrix
  G_hat <- A %*% AMA_inv %*% t(A) %*% t(I_B)

  # Null distribution
  P_t <- rep(0, T)
  for (t in 1:T) {
    u_t <- Y0[, t] - (a_hat + B_hat %*% Y0[, t])
    CGu <- C %*% G_hat %*% u_t
    P_t[t] <- as.numeric(t(CGu) %*% CGu)
  }

  # Test decision
  critical_value <- quantile(P_t, 1 - alpha_sig)
  reject <- P > critical_value

  # P-value
  p_value <- mean(P <= P_t)

  return(list(
    reject = as.logical(reject),
    p_value = p_value,
    test_stat = P,
    critical_value = critical_value,
    spillover_effects = alpha_hat[-1]  # Exclude treatment effect
  ))
}
