source("header.R")

# Load data object -------------------------------------------------------------
data <- readRDS("output/w2/data.rds")

# Code model -------------------------------------------------------------------
sink("w2-demo.txt")
cat("model{ 
  # Priors
  beta ~ dnorm(5000, 1000^-2)
  sigma ~ dexp(1)
  
  # Likelihood 
  for (i in 1:nObs) {
    mu[i] <- beta
    mass[i] ~ dnorm(mu[i], sigma^-2)
  }
}")
sink()

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("beta", "sigma")

# Initial values ---------------------------------------------------------------
get_inits <- function(x) {
  # Setup list to hold initial values
  inits <- list()
  # Name each element of the list the same name as specified in the model
  inits$beta <- rnorm(n = 1, mean = 5000, sd = 1000) 
  inits$sigma <- rexp(n = 1, rate = 0.001)
  # Return the list from the function
  return(inits)
}

# MCMC tuning parameters -------------------------------------------------------
nc <- 3            # Number of MCMC chains
nt <- 2            # Thinning rate
nb <- 500 * nt     # Number of burn-iterations
ni <- 1000 * nt    # Total number of iterations

# Run JAGS model ---------------------------------------------------------------
mod <- jagsUI::jags(
  data = data, 
  inits = get_inits,
  parameters.to.save = params_to_monitor, 
  model.file = "w2-demo.txt", 
  n.chains = nc, 
  n.thin = nt, 
  n.iter = ni, 
  n.burnin = nb
)

# Save model run ---------------------------------------------------------------
saveRDS(mod, "output/w2/mod.RDS")

# Evaluate convergence ---------------------------------------------------------
params_to_evaluate <- c("beta", "sigma")

# Check Rhat < 1.1 and reasonable effective sample size for each parameter.
MCMCvis::MCMCsummary(mod, params = params_to_evaluate)
# Check for "grassy" trace plots
MCMCvis::MCMCtrace(mod, params = params_to_evaluate, ind = TRUE, pdf = FALSE) 

# Model Selection --------------------------------------------------------------
# Look at parameter estimates
par(mfrow = c(1, 1)) 
MCMCvis::MCMCplot(mod, params = params_to_monitor)

# My original estimates and 95% CIs.
# beta: 4204.12, 4116-4291
# sigma: 803, 745-865

# Exercises --------------------------------------------------------------------
# 1. Change the prior for `beta` to a Normal distribution to have a standard 
# deviation of 100 (option B from our exercise above) and re-run the model. 
# Do the parameter estimates change considerably? 
# My answer: beta: 4331, 4250-4411 (slightly higher estimate - closer to the prior mean)
# sigma: 813, 754-884 - not very different
# What about with a standard deviation of 10 (option A from above)?
# My answer: beta: 4978, 4959-4998 (this is much closer to the prior mean!)
# sigma: 1119, 1038-1207 - quite a bit higher! Reason: something else has to 
# account for the variability in the data!

# because beta distribution gets super constrained (standard deviation of 10), something else has to take up the variability, aka sigma 

# 2. Return the standard deviation for the `beta` prior back to 1000. 
# Now change the prior for `sigma` to an exponential distribution with a rate 
# of 1. Do the parameter estimates change considerably? 
# My answer: beta: 4202, 4151-4255 - this is quite similar to the original estimate.
# sigma: 508, 488-529 - quite a bit lower! Reason: prior for sigma is much narrower.

# iin this example, signma is the one that gets constrained. But beta does not baloon. 
