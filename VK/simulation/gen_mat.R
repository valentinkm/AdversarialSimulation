# Load necessary libraries
library(lavaan)
library(Matrix)

# Generate a set of matrices that serve as basis for the population model
gen_mat <- function(model_type, nfactors = 5, nvar.factor = 3, lambda = 0.70, 
                    beta_value = 0.1, psi.cor = 0.3, reliability = 0.80, 
                    rho = 0.80) {
  
  # 1. LAMBDA
  fac <- matrix(c(1, rep(lambda, times = (nvar.factor - 1L))), nvar.factor, 1L)
  LAMBDA <- bdiag(rep(list(fac), nfactors))
  
  if (model_type == "1.2") {
    # misspecification in the measurement part: cross-loadings
    i.cross <- (0:(nfactors-1)) * nvar.factor + ceiling(nvar.factor / 2)
    for (j in 1:ncol(LAMBDA)) {
      LAMBDA[i.cross[j], c(2:nfactors, 1)[j]] <- rho * lambda
    }
  }
  
  LAMBDA <- as.matrix(LAMBDA)
  
  # 2. BETA
  BETA <- matrix(0, nrow = nfactors, ncol = nfactors)
  BETA[3:5, 1] <- BETA[3:4, 2] <- BETA[5, 3] <- BETA[c(3, 5), 4] <- beta_value
  if (model_type == "1.4") {
    # misspecification in the structural part for model 1.4
    BETA[4, c(1, 2, 3)] <- 0.1
  } else if (model_type == "1.3") {
    # misspecification in the structural part for model 1.3
    BETA[4, 3] <- BETA[3, 4]
    BETA[3, 4] <- 0
  }
  BETA.model <- BETA  # Structural part to be fitted
  VAL <- BETA[BETA != 0]  # true values
  
  # 3. PSI
  PSI <- matrix(0, nrow = nfactors, ncol = nfactors)
  PSI[1, 1] <- PSI[2, 2] <- 1  # the exogenous latent variables
  RES <- (1 - beta_value^2)
  PSI[lav_matrix_diag_idx(nfactors)[-c(1:2)]] <- RES
  
  # Calculate Sigma_eta
  IB_inv <- solve(diag(nfactors) - BETA)
  Sigma_eta <- IB_inv %*% PSI %*% t(IB_inv)
  
  # 4. THETA
  tmp <- diag(LAMBDA %*% Sigma_eta %*% t(LAMBDA))
  theta_diag <- tmp / reliability - tmp
  stopifnot(all(theta_diag > 0))
  THETA <- matrix(0, nrow(LAMBDA), nrow(LAMBDA))
  diag(THETA) <- theta_diag
  
  if (model_type == "1.3") {
    pairs <- cbind(c(2, 5, 8, 11, 14), c(3, 6, 9, 12, 15))
    for (pair in 1:nrow(pairs)) {
      i <- pairs[pair, 1]
      j <- pairs[pair, 2]
      THETA[i, j] <- THETA[j, i] <- 0.6 * min(theta_diag[c(i, j)])
    }
  }
  
  MLIST <- list(lambda = LAMBDA, theta = THETA, psi = PSI, beta = BETA)
  
  return(MLIST)
}

# Utility function to get diagonal indices
lav_matrix_diag_idx <- function(n) {
  return(seq(1, n^2, by = n + 1))
}
