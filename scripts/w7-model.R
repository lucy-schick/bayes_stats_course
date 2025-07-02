source("header.R")

# Load data object -------------------------------------------------------------
data <- readRDS("output/w7/data.rds")

# Code model -------------------------------------------------------------------
# 1. Intercept-only model
# 2. Slowly build up model complexity yourself
sink("w7-demo.txt")
cat("model{
  # Priors
  beta_0 ~ dnorm(0, 2^-2)
 
  # Likelihood
  for (i in 1:nObs) {
    logit(prob[i]) <- beta_0
    germinated[i] ~ dbern(prob[i])
  }
}")
sink()

# Initial values ---------------------------------------------------------------
gen_inits <- function(data) {
  inits <- list()
  inits$beta_0 <- rnorm(1, 0, 2)
  inits
}

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("beta_0")

# MCMC tuning parameters -------------------------------------------------------
nc <- 3            # Number of MCMC chains
nt <- 1            # Thinning rate
nb <- 500 * nt     # Number of burn-iterations
ni <- 1000 * nt    # Total number of iterations

# Run JAGS model ---------------------------------------------------------------
mod <- jagsUI::jags(
  data = data, 
  inits = gen_inits,
  parameters.to.save = params_to_monitor, 
  model.file = "w7-demo.txt", 
  n.chains = nc, 
  n.thin = nt, 
  n.iter = ni, 
  n.burnin = nb
)

# Save model run ---------------------------------------------------------------
saveRDS(mod, "output/w7/mod.rds")

# Evaluate convergence ---------------------------------------------------------
# Check all Rhat < 1.1 and all n.eff > 150
MCMCvis::MCMCsummary(mod, params = params_to_monitor)

# Check for "grassy" trace plots
MCMCvis::MCMCtrace(mod, params = params_to_monitor, ind = TRUE, pdf = FALSE) 

# Model Selection --------------------------------------------------------------
# Look at parameter estimates
par(mfrow = c(1, 1)) 
MCMCvis::MCMCplot(mod, params = params_to_monitor)

# Look at effect sizes and s-values
print(mod)

# Exercises:
# 1. Using the intercept-only template provided above, fit a binary model to the
# seed germination dataset. Build the model up one effect at a time to include 
# plausible covariates, perform model selection, create prediction plots, and 
# do a sensitivity analysis. Create a list of the parameters you included in 
# your final model.
