source("header.R")

# Load data object -------------------------------------------------------------
data <- readRDS("output/w2/data.rds")

# Code model -------------------------------------------------------------------
sink("w2-demo.txt")
cat("model{ 
  # Priors
  beta ~ dnorm(5000,1000^-2)
  sigma ~ dexp(0.001)
  
  # Likelihood 
  for (i in 1:nObs) {
   mu[i] <-  beta 
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
nt <- 1            # Thinning rate
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
# Check Rhat < 1.1 and reasonable effective sample size for each parameter.
MCMCvis::MCMCsummary(mod, params = params_to_monitor)
# Check for "grassy" trace plots
MCMCvis::MCMCtrace(mod, params = params_to_monitor, ind = TRUE, pdf = FALSE) 

# Model Selection --------------------------------------------------------------
# Look at parameter estimates
par(mfrow = c(1, 1)) 
MCMCvis::MCMCplot(mod, params = params_to_monitor)

# Exercises --------------------------------------------------------------------
# 1. Change the prior for `beta` to a Normal distribution to have a standard 
# deviation of 100 (option B from our exercise above) and re-run the model. 
# Do the parameter estimates change considerably? 
# What about with a standard deviation of 10 (option A from above)?



# 2. Return the standard deviation for the `beta` prior back to 1000. 
# Now change the prior for `sigma` to an exponential distribution with a rate 
# of 1. Do the parameter estimates change considerably? 
