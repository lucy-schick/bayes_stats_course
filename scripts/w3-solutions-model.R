source("header.R")

# Load data object -------------------------------------------------------------
data <- readRDS("output/w3/data.rds")

# Code model -------------------------------------------------------------------
sink("w3-demo.txt")
cat("model{
  # Priors
  beta_0 ~ dnorm(1, 3^-2)     # Prior for intercept
  beta_e ~ dnorm(0, 0.01^-2)  # Prior for slope effect of elev
  beta_l ~ dnorm(0, 1^-2)     # Prior for slope effect of lat
  sigma ~ dexp(0.25)          # Prior for standard deviation

  # Likelihood 
  for (i in 1:nObs) {                        
    mu[i] <- beta_0 + beta_e * elev[i] + beta_l * lat[i]
    temp_anomaly[i] ~ dnorm(mu[i], sigma^-2) 
  }
}")
sink()

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("beta_0", "beta_e", "beta_l", "sigma")

# MCMC tuning parameters -------------------------------------------------------
nc <- 3            # Number of MCMC chains
nt <- 5            # Thinning rate
nb <- 500 * nt     # Number of burn-iterations
ni <- 1000 * nt    # Total number of iterations

# Inits 
gen_inits <- function() {
  inits <- list()
  inits$beta_0 <- rnorm(1, 1, 3)
  inits$beta_e <- rnorm(1, 0, 0.01)
  inits$beta_l <- rnorm(1, 0, 1)
  inits$sigma <- rexp(1, 0.25)
  inits
}

# Run JAGS model ---------------------------------------------------------------
mod <- jagsUI::jags(
  data = data, 
  inits = gen_inits,
  parameters.to.save = params_to_monitor, 
  model.file = "w3-demo.txt", 
  n.chains = nc, 
  n.thin = nt, 
  n.iter = ni, 
  n.burnin = nb
)

# Save model run ---------------------------------------------------------------
saveRDS(mod, "output/w3/mod.rds")

# Evaluate convergence ---------------------------------------------------------
# Check all Rhat < 1.1 and all n.eff > 150
MCMCvis::MCMCsummary(mod, params = params_to_monitor)
# Check for "grassy" trace plots
MCMCvis::MCMCtrace(mod, params = params_to_monitor, ind = TRUE, pdf = FALSE) 

# Model Selection --------------------------------------------------------------
# Look at parameter estimates
par(mfrow = c(1, 1)) 
MCMCvis::MCMCplot(mod, params = params_to_monitor)

# Look at overlap0 and effect sizes
print(mod)

# Homework
# 1. Create two prediction plots to show the effects of elevation and latitude.

# Solution: see `w3-solutions-predictions.R`

# 2. Using the overlap0 column from the model output, decide whether or not to 
# keep the effects of elevation and/or latitude in the model. If you decide to 
# remove an effect, re-fit the model without that effect.

# Solution: 
# Both effects have overlap0 = FALSE. This suggests that their effects are 
# different from zero. The prediction plots also suggest that the effect size is 
# reasonably large on the scale of the response variable. Both effects should be
# retained in the model.
