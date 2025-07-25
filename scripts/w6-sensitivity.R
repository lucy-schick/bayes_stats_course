source("header.R")

# Load data object -------------------------------------------------------------
data <- readRDS("output/w6/data.rds")

# Code model -------------------------------------------------------------------
# The parameters we want to test for prior sensitivity are:
# beta_0
# beta_temp
# beta_site
# sd_year
# sd_site_year
# sd_id (observation-level random effect)

# Let's expand by a factor of 2.
sd_mult <- 2
# for normal distributions, we simply multiply the existing sd of the prior by 2
sens_norm(mean = 0, sd = 2, sd_mult = sd_mult)
# for exponential distributions, we divide the existing rate by 2.
sens_exp(rate = 1, sd_mult = sd_mult)

sink("w6-sens.txt")
# TODO: copy in the cat("model{}") line with your model in it from `w6-model.R`
sink()

# Initial values ---------------------------------------------------------------
# TODO: adjust the values in the inits function to reflect the new priors.
gen_inits <- function() {
  inits <- list()
  inits$beta_0 <- rnorm(1, 0, 2)
  inits$beta_temp <- rnorm(1, 0, 2)
  inits$beta_site[2:data$nsite] <- rnorm(data$nsite - 1, 0, 2)
  inits$sd_year <- rexp(1, 1)
  inits$beta_year[1:data$nyear] <- rnorm(data$nyear, 0, inits$sd_year)
  inits$sd_site_year <- rexp(1, 1)
  inits$beta_site_year <- matrix(rnorm(data$nsite * data$nyear, 0, inits$sd_site_year), nrow = data$nsite, ncol = data$nyear)
  inits$sd_id <- rexp(1, 1)
  inits$beta_id <- rnorm(data$nid, 0, inits$sd_id)
  return(inits)
}

# Parameters to monitor --------------------------------------------------------
params_to_monitor <- c("beta_0", "beta_temp", "beta_site", "beta_year", 
                       "sd_year", "beta_site_year", "sd_site_year", "sd_id",
                       "chi2_obs", "chi2_sim")

# MCMC tuning parameters -------------------------------------------------------
nc <- 3            # Number of MCMC chains
nt <- 50           # Thinning rate
nb <- 500 * nt     # Number of burn-iterations
ni <- 1000 * nt    # Total number of iterations

# Run JAGS model ---------------------------------------------------------------
sens <- jagsUI::jags(
  data = data, 
  inits = gen_inits,
  parameters.to.save = params_to_monitor, 
  model.file = "w6-sens.txt", 
  n.chains = nc, 
  n.thin = nt, 
  n.iter = ni, 
  n.burnin = nb
)

# Save model run ---------------------------------------------------------------
saveRDS(sens, "output/w6/sens.rds")

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
  filename = "w6-sens"
)

# Model Selection --------------------------------------------------------------
# Look at parameter estimates
par(mfrow = c(1, 1)) 
MCMCvis::MCMCplot(sens, params = params_to_monitor)

# Look at effect sizes and s-values
print(sens)

# Sensitivity Comparison -------------------------------------------------------
# Load in original model
mod <- readRDS("output/w6/mod.rds")

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
  filter(!str_detect(term, "chi|deviance")) # don't need to look at these terms

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
