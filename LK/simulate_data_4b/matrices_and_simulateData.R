######Explanation######
#######################
#This script requires lav_sam_gen_model() and lav_syntax_mlist() to be run
#via their respective files.

#If true_model=1, the condition with 5 cross-loadings is generated.
#If true_model=2, the condition with 20 residual correlation should be generated

#If true_model=2, the misspecification strength is reduced

#######################
#######################


NFAC   <- 5L    # number of factors
FVAR   <- 3L    # number of indicators per factor
lambda <- 0.70  # lambda value
# reliability for all indicators
REL    <- 0.7
# same beta value for all regression coefficients
beta   <- c(0.1)

# f1 -> f3, f4, f5
# f2 -> f3, f4
# f3 -> f5
# f4 -> f3, f5
# var(f1) == var(f2) == 1

BETA <- matrix(0, NFAC, NFAC)
BETA[3:5,1] <- BETA[3:4,2] <- BETA[5,3] <- BETA[c(3,5),4] <- beta
VAL <- BETA[BETA!=0] # true values
BETA.model <- BETA   # structural part to be fitted

PSI <- matrix(0, NFAC, NFAC)
PSI[1,1] <- PSI[2,2] <- 1 # the exogenous latent variables
RES <- (1 - beta^2)
PSI[lav_matrix_diag_idx(NFAC)[-c(1:2)]] <- RES

#need: assumed_model=c(1:2)
#need: true_model=c(1:2)
#But both seperately. Here, starting with 1
assumed_model=1
true_model=1


# generate pop model matrices
MLIST <- lav_sam_gen_model(nfactors = NFAC, nvar.factor = FVAR, lambda = lambda,
                           PSI = PSI, BETA = BETA, reliability = REL, 
                           misspecification = true_model,
                           rho = ifelse(true_model==2L,0.6,0.9))

# Specify population model

pop.model <- lav_syntax_mlist(MLIST, include.values = TRUE)
cat(pop.model)

#Simulate Data
Data <- simulateData(pop.model, sample.nobs = 100)
