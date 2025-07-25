source("header.R")

# Load data object -------------------------------------------------------------
data <- readRDS("output/w8/data.rds")

# Code model -------------------------------------------------------------------
# Let's expand by a factor of 2.
sd_mult <- 2
# for normal distributions, we simply multiply the existing sd of the prior by 2
sens_norm(mean = 0, sd = 2, sd_mult = sd_mult)
# for exponential distributions, we divide the existing rate by 2.
sens_exp(rate = 1, sd_mult = sd_mult)

sink("w8-sens.txt")
cat("model{
  # Priors
  beta_0 ~ dnorm(0, 4^-2)
  beta_herbicide[1] <- 0
  for (i in 2:nherbicide) {
    beta_herbicide[i] ~ dnorm(0, 4^-2)
  } 
  sd_tray_id ~ dexp(0.5)
  for (i in 1:ntray_id) {
    beta_tray_id[i] ~ dnorm(0, sd_tray_id^-2)
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
  inits$beta_0 <- rnorm(1, 0, 4)
  inits$beta_herbicide[2] <- rnorm(1, 0, 4)
  inits$sd_tray_id <- rexp(1, 0.5)
  inits$beta_tray_id[1:data$ntray_id] <- rnorm(data$ntray_id, 0, inits$sd_tray_id)
  inits
}

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("beta_0", "beta_herbicide", "sd_tray_id", "beta_tray_id",
                       "chi2_obs", "chi2_sim")

# MCMC tuning parameters -------------------------------------------------------
nc <- 3            # Number of MCMC chains
nt <- 10           # Thinning rate
nb <- 500 * nt     # Number of burn-iterations
ni <- 1000 * nt    # Total number of iterations

# Run JAGS model ---------------------------------------------------------------
sens <- jagsUI::jags(
  data = data, 
  inits = gen_inits,
  parameters.to.save = params_to_monitor, 
  model.file = "w8-sens.txt", 
  n.chains = nc, 
  n.thin = nt, 
  n.iter = ni, 
  n.burnin = nb
)

# Save model run ---------------------------------------------------------------
saveRDS(sens, "output/w8/sens.rds")

# Evaluate convergence ---------------------------------------------------------
# Check all Rhat < 1.1 and all n.eff > 150
conv <- MCMCvis::MCMCsummary(sens, params = params_to_monitor)
conv %>% arrange(n.eff) %>% slice_head(n = 5) # Shows 5 minimum n.eff
conv %>% arrange(desc(Rhat)) %>% slice_head(n = 5) # Shows 5 maximum Rhat

# Check for "grassy" trace plots
MCMCvis::MCMCtrace(
  sens, 
  params = params_to_monitor, 
  ind = TRUE, # plots density plots too
  pdf = TRUE, # puts trace plots into pdf
  open_pdf = FALSE, # don't open pdf interactively
  filename = "w8-sens"
)

# Model Selection --------------------------------------------------------------
# Look at parameter estimates
par(mfrow = c(1, 1)) 
MCMCvis::MCMCplot(sens, params = params_to_monitor)

# Look at effect sizes and s-values
print(sens)

# Sensitivity Comparison -------------------------------------------------------
# Load in original model
mod <- readRDS("output/w8/mod.rds")

# Create tibble to compare
comp <- 
  tibble(
    term = c(rownames(mod$summary), rownames(sens$summary)),
    estimate = c(mod$summary[, "50%"], sens$summary[, "50%"]),
    lower = c(mod$summary[, "2.5%"], sens$summary[, "2.5%"]),
    upper = c(mod$summary[, "97.5%"], sens$summary[, "97.5%"]),
    model = rep(c("model", "sens"), each = nrow(mod$summary))
  ) %>% 
  arrange(term, model) %>% 
  filter(!str_detect(term, "deviance|chi2")) # don't need to look at these terms

# Compare graphically
# You may need to separate these into multiple plots to show the different 
# scales.
gp <- ggplot(comp) +
  geom_pointrange(
    aes(x = term, y = estimate, ymin = lower, ymax = upper, colour = model),
    position = position_dodge(width = 0.5)
  ) +
  guides(x = guide_axis(angle = 90)) +
  xlab("Term") +
  ylab("Estimate") +
  NULL

print(gp)

# Compare numerically
view(comp)
