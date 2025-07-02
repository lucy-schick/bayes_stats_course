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
  beta_seed_mass ~ dnorm(0, 2^-2)
  beta_herbicide[1] <- 0
  for (i in 2:nherbicide) {
    beta_herbicide[i] ~ dnorm(0, 2^-2)
  }
  beta_soil_type[1] <- 0
  for (i in 2:nsoil_type) {
    beta_soil_type[i] ~ dnorm(0, 2^-2)
  }
  sd_tray_id ~ dexp(1)
  for (i in 1:ntray_id) {
    beta_tray_id[i] ~ dnorm(0, sd_tray_id^-2)
  }
 
  # Likelihood
  for (i in 1:nObs) {
    logit(prob[i]) <- beta_0 + beta_seed_mass * seed_mass[i] + beta_herbicide[herbicide[i]] + beta_soil_type[soil_type[i]] + beta_tray_id[tray_id[i]]
    germinated[i] ~ dbern(prob[i])
  }
}")
sink()

# Initial values ---------------------------------------------------------------
gen_inits <- function() {
  inits <- list()
  inits$beta_0 <- rnorm(1, 0, 2)
  inits$beta_seed_mass <- rnorm(1, 0, 2)
  inits$beta_herbicide[2] <- rnorm(1, 0, 2)
  inits$beta_soil_type[2:data$nsoil_type] <- rnorm(data$nsoil_type - 1, 0, 2)
  inits$sd_tray_id <- rexp(1, 1)
  inits$beta_tray_id <- rnorm(data$ntray_id, 0, inits$sd_tray_id)
  inits
}

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("beta_0", "beta_seed_mass", 
                       "beta_herbicide", "beta_soil_type", 
                       "sd_tray_id", "beta_tray_id")

# MCMC tuning parameters -------------------------------------------------------
nc <- 3            # Number of MCMC chains
nt <- 60           # Thinning rate
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
# So many effects, we can filter to see what the minimum is.
conv <- MCMCvis::MCMCsummary(mod, params = params_to_monitor)
conv %>% arrange(n.eff) %>% slice_head(n = 5) # Shows 5 minimum n.eff
conv %>% arrange(desc(Rhat)) %>% slice_head(n = 5) # Shows 5 maximum Rhat

# Check for "grassy" trace plots
MCMCvis::MCMCtrace(
  mod, 
  params = params_to_monitor, 
  ind = TRUE, 
  pdf = TRUE, # puts trace plots into pdf
  open_pdf = FALSE, # don't open interactively
  filename = "w7-mod"
) 

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

# Solution: 
# Consider what covariates could causally affect germination:
# They all could plausibly affect germination, except for `planting_time`, which
# doesn't have much of a biological justification.

# Now add in covariates one at a time. 
# The order I've done it is: continuous effects (seed_mass, seed_depth), fixed
# effects (herbicide, soil_type, watering_freq), random effects(tray_id).

# After adding all of these effects, I've evaluated the directional certainty
# of the continuous effects, discrete fixed effects, and standard deviation of
# the random effect.

# Based on the `overlap0` column, I've decided to remove the effects of 
# `seed_depth` and `watering_freq`. For me, the remaining fixed effects have
# estimates that either don't overlap 0 or have the lower 95% CI of the standard
# deviation of their random effect ≥ 5% of the median estimate.

# See `w7-solutions-predictions.R` for predictions
# and `w7-solutions-sensitivity.R` for sensitivity
# Recall that goodness of fit is not informative for Bernoulli responses.
