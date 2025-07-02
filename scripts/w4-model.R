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
  
  # Likelihood
  for (i in 1:nObs) {
    log(lambda[i]) <- beta_0
    count[i] ~ dpois(lambda[i])
  }
}")
sink()

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("beta_0")

# Inits ------------------------------------------------------------------------
gen_inits <- function() {
  inits <- list()
  inits$beta_0 <- rnorm(1, 0, 2)
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

# Exercises:
# 1. Start by fitting the intercept-only model.

# 2. Next, add the continuous effect of temperature. Try adapting last week's 
# prediction code to produce a prediction plot for the effect of temperature on 
# the expected count. Hint: you'll need to add in a `log()` function to your for 
# loop. There is a  function in the `extras` package to do the same replacement 
# style of function as JAGS uses (i.e., this will work identically to JAGS: 
# log(x) <- 10).

# 3. Now add in the fixed effect of site into your model. I will show you how to
# do the predictions.

# Homework:
# 1. Do sites B and C differ from site A based on the overlap0 column of the 
# model output?

# 2. In the `w4-data.R` script, change the base level of the site variable to 
# site C using this line before you generate the list of data:
# starfish$site <- factor(starfish$site, levels = c("Site C", "Site A", "Site B"))
# Refit the model, and compare the prediction plots of the effect of site.
# Did the parameter estimates change? What about the predictions?

# 3. Remove the `beta_site[1] <- 0` line and have the for loop for the 
# `beta_site` prior iterate through `1:nsite`. See what happens when you try to 
# fit the model. Can you remember what this problem is called?
