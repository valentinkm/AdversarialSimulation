lav_sam_gen_model <- function(nfactors = 3L, nvar.factor = 3L,
                              lambda = 0.70, PSI = NULL, BETA = NULL,
                              psi.cor = 0.3, reliability = 0.80, 
                              misspecification = 0L, rho=0.80) {

    # 1. LAMBDA
    fac <- matrix(c(1, rep(lambda, times = (nvar.factor - 1L))),
                   nvar.factor, 1L)
    LAMBDA <- lav_matrix_bdiag(rep(list(fac), nfactors))
    
    if (misspecification==1L) {
        # misspecification in the measurement part: cross-loadings
        i.cross <- (0:(nfactors-1))*nvar.factor+ceiling(nvar.factor/2)
        for (j in 1:ncol(LAMBDA)) {
            LAMBDA[i.cross[j],c(2:nfactors,1)[j]] <- rho*lambda
        }
    }
    
    # 2. PSI (in correlation metric)
    if(!is.null(PSI)) {
        stopifnot(nrow(PSI) == ncol(LAMBDA),
                  all((t(PSI) - PSI) == 0))
        if(!is.null(BETA)) {
            stopifnot(nrow(BETA) == ncol(LAMBDA),
                      all(diag(BETA) == 0))
            IB.inv <- solve(diag(nrow(BETA)) - BETA)
            VETA <- IB.inv %*% PSI %*% t(IB.inv)
        } else {
            VETA <- PSI
        }
    } else {
        PSI.cor <- matrix(psi.cor, nfactors, nfactors)
        diag(PSI.cor) <- 1L
        # convert to covariance matrix (not yet for now)
        PSI <- PSI.cor
        BETA <- NULL
        VETA <- PSI
    }

    # 3. THETA (depending on PSI, LAMBDA, and the required reliability)
    tmp <- diag(LAMBDA %*% VETA %*% t(LAMBDA))
    theta.diag <- tmp/reliability - tmp
    # no zero or negative theta values on the diagonal
    stopifnot(all(theta.diag > 0))
    THETA <- matrix(0, nrow(LAMBDA), nrow(LAMBDA))
    diag(THETA) <- theta.diag
    
    if (misspecification==2L) {
        # misspecification in the measurement part: 
        ## missing correlated indicator residuals
        i1.corr <- (0:(nfactors-1))*nvar.factor+ceiling(nvar.factor/2)
        i2.corr <- (1:nfactors)*nvar.factor
        for (t.i in 1:(nfactors-1)) {
            for (t.j in (t.i+1):nfactors) {
                # upper diagonal entries
                THETA[i1.corr[t.i],i1.corr[t.j]] <- min(diag(THETA))*rho
                THETA[i2.corr[t.i],i2.corr[t.j]] <- min(diag(THETA))*rho
            }
        }
        # return symmetric matrix
        THETA[lower.tri(THETA)] <- t(THETA)[lower.tri(THETA)]
    }

    if(is.null(BETA)) {
        MLIST <- list(lambda = LAMBDA, theta = THETA, psi = PSI)
    } else {
        MLIST <- list(lambda = LAMBDA, theta = THETA, psi = PSI, beta = BETA)
    }

    MLIST
}

