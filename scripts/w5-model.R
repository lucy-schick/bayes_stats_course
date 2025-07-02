source("header.R")

# Load data object -------------------------------------------------------------
data <- readRDS("output/w5/data.rds")

# Code model -------------------------------------------------------------------
# 1. Add random effect of year to our model from last week
# 2. Add random interaction effect of site-year.
sink("w5-demo.txt")
cat("model{
  # Priors
  beta_0 ~ dnorm(0, 2^-2)
  beta_t ~ dnorm(0, 2^-2)
  
  beta_s[1] <- 0
  for (i in 2:nsite) {
    beta_s[i] ~ dnorm(0, 2^-2)
  }

  # Likelihood
  for (i in 1:nObs) {
    log(lambda[i]) <- beta_0 + beta_t * temp[i] + beta_s[site[i]]
    count[i] ~ dpois(lambda[i])
  }
}")
sink()

# Inits ------------------------------------------------------------------------
gen_inits <- function() {
  inits <- list()
  inits$beta_0 <- rnorm(1, 0, 2)
  inits$beta_t <- rnorm(1, 0, 2)
  inits$beta_s[2:data$nsite] <- rnorm(data$nsite - 1, 0, 2)
  return(inits)
}

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("beta_0", "beta_t", "beta_s")

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
  model.file = "w5-demo.txt", 
  n.chains = nc, 
  n.thin = nt, 
  n.iter = ni, 
  n.burnin = nb
)

# Save model run ---------------------------------------------------------------
saveRDS(mod, "output/w5/mod.rds")

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

# Homework:

# 1. Create prediction plots for:
# a. The effect of temperature on the expected count.
# b. The effect of site and year on the expected count.

# 2. Look at the estimated `sigma_y` and `sigma_s_y` coefficients, and explain 
# what the magnitudes of these standard deviations tell you about where 
# variation is coming from.
