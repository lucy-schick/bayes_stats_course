source("header.R")

# Load model object ------------------------------------------------------------
mod <- readRDS("output/w8/mod.rds")

# Calculate Bayesian p-value
# If bp is close to 0 or 1, it indicates that the model is a poor fit to the data.
bp <- mean(mod$sims.list$chi2_sim > mod$sims.list$chi2_obs)
print(paste0("The Bayesian p-value is ", bp))

df <- tibble(
  obs = mod$sims.list$chi2_obs,
  sim = mod$sims.list$chi2_sim
)

gp <- ggplot(df) +
  geom_point(aes(x = obs, y = sim), alpha = 0.3) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  expand_limits(x = 0, y = 0)

print(gp)
