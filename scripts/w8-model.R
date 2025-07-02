source("header.R")

# Load data object -------------------------------------------------------------
data <- readRDS("output/w8/data.rds")

# Code model -------------------------------------------------------------------
# 1. Intercept-only model
# 2. Slowly build up model complexity yourself
sink("w8-demo.txt")
cat("model{
  # Priors
  beta_0 ~ dnorm(0, 2^-2)
  
 sd_tray_id ~ dexp(1)
  for (i in 1:ntray_id) {
    beta_tray_id[i] ~ dnorm(0, sd_tray_id^-2)
  }
  
  beta_herbicide[1] <- 0
  for (i in 2:nherbicide){
    beta_herbicide[i] ~ dnorm(0, 2^-2)
  }
 
  # Likelihood
  for (i in 1:nObs) {
    logit(prob[i]) <- beta_0 + beta_herbicide[herbicide[i]] + beta_tray_id[tray_id[i]]
    germinated[i] ~ dbinom(prob[i], seeds[i])
  }
  
  # Goodness of Fit
  for (i in 1:nObs) {
    germ_sim[i] ~ dbin(prob[i], seeds[i])
    # expected value of binomial is prob * size
    germ_exp[i] <- prob[i] * seeds[i] + 0.001 # adding 0.001 to avoid dividing by 0
    x2_obs[i] <- pow((germinated[i] - germ_exp[i]), 2) / germ_exp[i]# Observed data
    x2_sim[i] <- pow((germ_sim[i] - germ_exp[i]), 2) / germ_exp[i]  # Ideal data
  }
  chi2_obs <- sum(x2_obs)
  chi2_sim <- sum(x2_sim)
}")
sink()

# Initial values ---------------------------------------------------------------
gen_inits <- function() {
  inits <- list()
  inits$beta_0 <- rnorm(1, 0, 2)
  inits$beta_herbicide[2] <- rnorm(1, 0, 2)
  inits$sd_tray_id <- rexp(1, 1)
  inits$beta_tray_id[1:data$ntray_id] <- rnorm(data$ntray_id, 0, inits$sd_tray_id)
  inits
}

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("beta_0", "chi2_obs", "chi2_sim", "beta_herbicide", "sd_tray_id", "beta_tray_id")

# MCMC tuning parameters -------------------------------------------------------
nc <- 3            # Number of MCMC chains
nt <- 10            # Thinning rate
nb <- 500 * nt     # Number of burn-iterations
ni <- 1000 * nt    # Total number of iterations

# Run JAGS model ---------------------------------------------------------------
mod <- jagsUI::jags(
  data = data, 
  inits = gen_inits,
  parameters.to.save = params_to_monitor, 
  model.file = "w8-demo.txt", 
  n.chains = nc, 
  n.thin = nt, 
  n.iter = ni, 
  n.burnin = nb
)

# Save model run ---------------------------------------------------------------
saveRDS(mod, "output/w8/mod.rds")

# Evaluate convergence ---------------------------------------------------------
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
  filename = "w8-mod"
) 

# Model Selection --------------------------------------------------------------
# Look at parameter estimates
# This is becoming rather large to deal with - better to look at our prediction 
# plots.
par(mfrow = c(1, 1)) 
MCMCvis::MCMCplot(mod, params = params_to_monitor)

# Look at effect sizes and s-values
print(mod)

