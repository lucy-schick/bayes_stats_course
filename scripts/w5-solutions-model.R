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
  
  sd_y ~ dexp(1)
  for (i in 1:nyear) {
    beta_y[i] ~ dnorm(0, sd_y^-2)
  }
  
  sd_s_y ~ dexp(1)
  for (i in 1:nsite) {
    for (j in 1:nyear) {
      beta_s_y[i, j] ~ dnorm(0, sd_s_y^-2)
    }
  }

  # Likelihood
  for (i in 1:nObs) {
    log(lambda[i]) <- beta_0 + beta_t * temp[i] + beta_s[site[i]] + beta_y[year[i]] + beta_s_y[site[i], year[i]]
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
  inits$sd_y <- rexp(1, 1)
  inits$beta_y[1:data$nyear] <- rnorm(data$nyear, 0, inits$sd_y)
  inits$sd_s_y <- rexp(1, 1)
  inits$beta_s_y <- matrix(rnorm(data$nsite * data$nyear, 0, inits$sd_s_y), nrow = data$nsite, ncol = data$nyear)
  return(inits)
}

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("beta_0", "beta_t", "beta_s", "beta_y", "sd_y", "beta_s_y", "sd_s_y")

# MCMC tuning parameters -------------------------------------------------------
nc <- 3            # Number of MCMC chains
nt <- 50            # Thinning rate
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
# b. The effect of site and year on the expected count (including the interaction effect).

# Solution: see `w5-solutions-predictions.R`

# 2. Look at the estimated values of `sd_y` and `sd_s_y`, and explain 
# what the magnitudes of these standard deviation terms tell you about the  
# sources of variation in the expected count of starfish in a quadrat.

# Solution: My estimated values are:
# sd_y: 0.591 95% CI: (0.209, 1.144)
# sd_s_y: 0.463 95% CI: (0.170, 0.858)

# The estimated coefficients are quite similar, which suggests that both 
# inter-annual variation and site-year interaction effects are important 
# contributors to variability in starfish counts.
