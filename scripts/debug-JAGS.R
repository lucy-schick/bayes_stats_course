source("header.R")

# Simulate data ----------------------------------------------------------------
set.seed(101)

beta_0 <- 1
sd_site <- 1.4
nsite <- 10
beta_site <- rnorm(nsite, mean = 0, sd = sd_site)

nrep <- 3
site <- rep(1:nsite, each = nrep)
nObs <- nsite * nrep
prob <- vector(length = nObs)
logit(prob) <- beta_0 + beta_site[site]

y <- rbern(n = nObs, prob = prob)

# Prepare data for JAGS --------------------------------------------------------
data <- list(
  y = y, 
  site = site,
  nsite = nsite,
  nObs = nObs
)

# Model ------------------------------------------------------------------------
# Look here and below for ~ 13 issues.
sink("debug-JAGS.txt")
cat("model{
  # Priors
  beta_0 ~ dnorm(0 2)
  
  sd_site ~ dexp(1)
  for (i 1:nsite) {
    beta_site[i] ~ dnorm(1, sd_site^-2)
  }
 
  # Likelihood
  for (i in 1:nObs) {
    prob[i] <- beta0 + beta_site[site]
    y = dbern(prob[i])
  }
}")
sink()

# Initial values ---------------------------------------------------------------
gen_inits <- function() {
  inits <- list()
  inis$beta_0 <- rnorm(10, 0, 2)
  inits$sd_site <- rexp(1, 1)
  inits$beta_site <- rnorm(nsite, 0, inits$sd_site)
  inits
}

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("Beta_0", "beta_site")

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
  model.file = "debug-jags.txt", 
  n.chains = nc, 
  n.thin = nt, 
  n.iter = ni, 
  n.burnin = nb
)

print(mod)
