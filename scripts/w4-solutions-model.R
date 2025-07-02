source("header.R")

# Load data object -------------------------------------------------------------
data <- readRDS("output/w4/data.rds")

# Code model ------------------------------------------------------------------- 
# Steps: 
# 1. Intercept-only 
# 2. Continuous fixed effect of temperature 
# 3. Discrete fixed effect of site
sink("w4-demo.txt")
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

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("beta_0", "beta_t", "beta_s")

# Inits ------------------------------------------------------------------------
gen_inits <- function() {
  inits <- list()
  inits$beta_0 <- rnorm(1, 0, 2)
  inits$beta_t <- rnorm(1, 0, 2)
  inits$beta_s[2:3] <- rnorm(2, 0, 2)
  return(inits)
}

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
  model.file = "w4-demo.txt", 
  n.chains = nc, 
  n.thin = nt, 
  n.iter = ni, 
  n.burnin = nb
)

# Save model run ---------------------------------------------------------------
saveRDS(mod, "output/w4/mod.rds")

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
# 1. Do sites B and C differ from site A based on the overlap0 column of the 
# model output?

# Solution: In my model output, both beta_s[2] (site B) and beta_s[3] (site C)
# have `FALSE` in the overlap0 column, which suggests that each differs from 
# site A.

# 2. In the `w4-data.R` script, change the base level of the site variable to 
# site C using this line before you generate the list of data:
# starfish$site <- factor(starfish$site, levels = c("Site C", "Site A", "Site B"))
# Refit the model, and compare the prediction plots of the effect of site.
# Did the parameter estimates change? What about the predictions?

# Solution: The parameter estimates for beta_s[2] and beta_s[3] did change!
# This reflects that we are now comparing sites A and B to site C.
# If you noticed, beta_s[2] in the new model (comparing site A to site C) should 
# be quite similar in magnitude to beta_s[3] in the original model (which 
# compared site C to site A), but with an opposite sign.

# The prediction plot should not have changed noticeably (aside from a small 
# amount of sampling variation). If it did, you were likely using the original
# starfish dataset, without the change to the `site` variable we made in the 
# data script. See the `w4-solutions-data.R` and `w4-solutions-predictions.R`
# scripts to see how to save the updated dataframe and make it available for the
# predictions.

# 3. Remove the `beta_site[1] <- 0` line and have the for loop for the 
# `beta_site` prior iterate through `1:nsite`. See what happens when you try to 
# fit the model. Can you remember what this problem is called?

# Solution: 
#  I didn't run into any issues fitting my model making these changes. However,
# if you compared the parameter estimates, you would have noticed that the 
# uncertainties of beta_0 and beta_s (all three levels) expanded greatly.
# This problem is called *identifiability*.
