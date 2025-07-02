source("header.R")

# Load data object -------------------------------------------------------------
data <- readRDS("output/w6/data.rds")

# Code model -------------------------------------------------------------------
# 1. Evaluate the goodness of fit of the current model (run this model, then
# run through the `w6-gof.R` script).
# 2. If needed, add in random effect of "id" (add in a column to your dataset 
# with the current row number).
# 3. Evaluate the sensitivity of the final model in the `w6-sensitivity.R`
# script.
sink("w6-demo.txt")
cat("model{
  # Priors
  beta_0 ~ dnorm(0, 2^-2)
  beta_temp ~ dnorm(0, 2^-2)
  
  beta_site[1] <- 0
  for (i in 2:nsite) {
    beta_site[i] ~ dnorm(0, 2^-2)
  }
  
  sd_year ~ dexp(1)                       # Standard deviation of the random effect
  for (i in 1:nyear) {                    # Iterate through the levels of the 'year' covariate
    beta_year[i] ~ dnorm(0, sd_year^-2)   # Specify the random effect
  }

  sd_site_year ~ dexp(1)
  for (i in 1:nsite) {
    for (j in 1:nyear) {
      beta_site_year[i, j] ~ dnorm(0, sd_site_year^-2)
    }
  }

  # Likelihood
  for (i in 1:nObs) {
    log(lambda[i]) <- beta_0 + beta_temp * temp[i] + beta_site[site[i]] + beta_year[year[i]] + beta_site_year[site[i], year[i]]
    count[i] ~ dpois(lambda[i])
  }
  
  # Goodness of Fit using the Chi-square statistic
  for (i in 1:nObs) {
    count_sim[i] ~ dpois(lambda[i])
    count_exp[i] <- lambda[i]
    x2_obs[i] <- pow((count[i] - count_exp[i]), 2) / count_exp[i]   # Observed data
    x2_sim[i] <- pow((count_sim[i] - count_exp[i]), 2) / count_exp[i]  # Ideal data
  }
  
  chi2_obs <- sum(x2_obs)
  chi2_sim <- sum(x2_sim)
  
}")
sink()

# Initial values ---------------------------------------------------------------

# Parameters to monitor --------------------------------------------------------
# Don't include beta_id - we don't care about its value!
params_to_monitor <- c("beta_0", "beta_temp", "beta_site", "beta_year", 
                       "sd_year", "beta_site_year", "sd_site_year",
                       "chi2_obs", "chi2_sim")

# MCMC tuning parameters -------------------------------------------------------
nc <- 3            # Number of MCMC chains
nt <- 50           # Thinning rate
nb <- 500 * nt     # Number of burn-iterations
ni <- 1000 * nt    # Total number of iterations

# Initial values ---------------------------------------------------------------
# Don't need inits for GoF components
gen_inits <- function() {
  inits <- list()
  inits$beta_0 <- rnorm(1, 0, 2)
  inits$beta_temp <- rnorm(1, 0, 2)
  inits$beta_site[2:data$nsite] <- rnorm(data$nsite - 1, 0, 2)
  inits$sd_year <- rexp(1, 1)
  inits$beta_year[1:data$nyear] <- rnorm(data$nyear, 0, inits$sd_year)
  inits$sd_site_year <- rexp(1, 1)
  inits$beta_site_year <- matrix(rnorm(data$nsite * data$nyear, 0, inits$sd_site_year), nrow = data$nsite, ncol = data$nyear)
  return(inits)
}

# Run JAGS model ---------------------------------------------------------------
mod <- jagsUI::jags(
  data = data, 
  inits = gen_inits,
  parameters.to.save = params_to_monitor, 
  model.file = "w6-demo.txt", 
  n.chains = nc, 
  n.thin = nt, 
  n.iter = ni, 
  n.burnin = nb
)

# Save model run ---------------------------------------------------------------
saveRDS(mod, "output/w6/mod.rds")

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
  filename = "w6-mod"
)

# Model Selection --------------------------------------------------------------
# Look at parameter estimates
# This is becoming rather large to deal with - better to look at our prediction 
# plots.
par(mfrow = c(1, 1)) 
MCMCvis::MCMCplot(mod, params = params_to_monitor)

# Look at effect sizes and s-values
print(mod)

# Homework ---------------------------------------------------------------------

# 1. As an exercise, change one of your original priors to something you would 
# consider "strong." Re-run the sensitivity analysis. Did the results suggest 
# sensitivity in that parameter? If not, try a stronger prior. Play around with 
# this for a while to gather some intuition on what constitutes a strong prior
# for this dataset. Optionally, you could also subset the data to see how fewer
# data points can lead to more sensitive models (fewer data mean the likelihood
# doesn't overwhelm the priors as much).
