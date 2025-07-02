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
  beta_0 ~ dnorm(0, 2^-2) # needed comma and for the ^-2 adjustment for precision
  
  sd_site ~ dexp(1)
  for (i in 1:nsite) { # was missing 'in'
    beta_site[i] ~ dnorm(0, sd_site^-2) # prior should have mean = 0, not mean = 1
  }
 
  # Likelihood
  for (i in 1:nObs) {
    logit(prob[i]) <- beta_0 + beta_site[site[i]] # needed logit(), index for site[i], and beta_0 needed the underscore
    y[i] ~ dbern(prob[i]) # needed index for y[i] and `~` instead of `=`
  }
}")
sink()

# Initial values ---------------------------------------------------------------
gen_inits <- function() {
  inits <- list()
  inits$beta_0 <- rnorm(1, 0, 2) # needs just 1 value, not 10; inits was spelled wrong
  inits$sd_site <- rexp(1, 1)
  inits$beta_site <- rnorm(nsite, 0, inits$sd_site)
  inits
}

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("beta_0", "beta_site", "sd_site") # beta_0 lowercase, need to add sd_site

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
  model.file = "debug-JAGS.txt", # JAGS in capitals! See line 29!
  n.chains = nc, 
  n.thin = nt, 
  n.iter = ni, 
  n.burnin = nb
)

print(mod)
