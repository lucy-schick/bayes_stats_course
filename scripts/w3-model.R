source("header.R")

# Load data object -------------------------------------------------------------
data <- readRDS("output/w3/data.rds")

# Code model -------------------------------------------------------------------
# 1. Start with intercept-only model.
# 2. Add continuous effect of elevation.
# 3. Add continuous effect of latitude
sink("w3-demo.txt")
cat("model{
  # Priors

  # Likelihood 
  for (i in 1:nObs) {                        

  }
}")
sink()

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c()

# MCMC tuning parameters -------------------------------------------------------
nc <- 3            # Number of MCMC chains
nt <- 1            # Thinning rate
nb <- 500 * nt     # Number of burn-iterations
ni <- 1000 * nt    # Total number of iterations

# Run JAGS model ---------------------------------------------------------------
mod <- jagsUI::jags(
  data = data, 
  inits = NULL,
  parameters.to.save = params_to_monitor, 
  model.file = "w3-demo.txt", 
  n.chains = nc, 
  n.thin = nt, 
  n.iter = ni, 
  n.burnin = nb
)

# Save model run ---------------------------------------------------------------
saveRDS(mod, "output/w3/mod.RDS")

# Evaluate convergence ---------------------------------------------------------
# Check all Rhat < 1.1 and all n.eff > 150
MCMCvis::MCMCsummary(mod, params = params_to_monitor)
# Check for "grassy" trace plots
MCMCvis::MCMCtrace(mod, params = params_to_monitor, ind = TRUE, pdf = FALSE) 

# Model Selection --------------------------------------------------------------
# Look at parameter estimates
par(mfrow = c(1, 1)) 
MCMCvis::MCMCplot(mod, params = params_to_monitor)

# Look at overlap0 column and effect sizes
print(mod)
