source("header.R")

# Load model object ------------------------------------------------------------
mod <- readRDS("output/w5/mod.rds")

# Make predictions -------------------------------------------------------------
# Effect of temperature
post <- as.matrix(mod$samples)
x_pred <- seq(min(starfish$temp), max(starfish$temp), length.out = 30)
beta_0_draws <- post[, "beta_0"]
beta_s_draws <- post[, "beta_s[1]"] # Look at effect for Site A (first level)
beta_t_draws <- post[, "beta_t"]

n_draws <- length(beta_0_draws)
n_x <- length(x_pred)
pred_mat <- matrix(NA, nrow = n_draws, ncol = n_x) 

for (i in 1:n_x) {
  # Specify this equation exactly as in the model code
  # !! Except we'll exclude the random effects when not looking directly at that effect.
  log(pred_mat[, i]) <- beta_0_draws + beta_t_draws * x_pred[i] + beta_s_draws
}

df <- tibble(
  x_pred = x_pred,
  estimate = apply(pred_mat, 2, median),
  lower = apply(pred_mat, 2, quantile, 0.025),
  upper = apply(pred_mat, 2, quantile, 0.975)
)

gp <- ggplot(df) +
  aes(x = x_pred, y = estimate) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  xlab("Temperature (˚C)") +
  ylab("Count") +
  NULL

print(gp)

# Effect of site
post <- as.matrix(mod$samples)
x_pred <- unique(starfish$site)
beta_0_draws <- post[, "beta_0"]
beta_s_draws <- sapply(as.integer(x_pred), function(i) {post[, paste0("beta_s", "[", i, "]")]})
beta_t_draws <- post[, "beta_t"]

n_draws <- length(beta_0_draws)
n_x <- length(x_pred)
pred_mat <- matrix(NA, nrow = n_draws, ncol = n_x) 

for (i in 1:n_x) {
  # Specify this equation exactly as in the model code
  # !! Except we'll exclude the random effects when not looking directly at that effect.
  log(pred_mat[, i]) <- beta_0_draws + beta_t_draws * mean(starfish$temp) + beta_s_draws[, x_pred[i]]
}

df <- tibble(
  x_pred = x_pred,
  estimate = apply(pred_mat, 2, median),
  lower = apply(pred_mat, 2, quantile, 0.025),
  upper = apply(pred_mat, 2, quantile, 0.975)
)

gp <- ggplot(df) +
  aes(x = x_pred, y = estimate) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  xlab("Site") +
  ylab("Count") + 
  expand_limits(y = 0) +
  NULL

print(gp)

# Effect of year
post <- as.matrix(mod$samples)
x_pred <- unique(starfish$year)
beta_0_draws <- post[, "beta_0"]
beta_s_draws <- post[, "beta_s[1]"] # Look at effect for Site A (first level)
beta_t_draws <- post[, "beta_t"]
beta_y_draws <- sapply(as.integer(x_pred), function(i) {post[, paste0("beta_y", "[", i, "]")]})

n_draws <- length(post[, 1])
n_x <- length(x_pred)
pred_mat <- matrix(NA, nrow = n_draws, ncol = n_x) 

for (i in 1:n_x) {
  # Specify this equation exactly as in the model code
  # This time include the random effect, as we are looking explicitly at their
  # estimated values.
  log(pred_mat[, i]) <- beta_0_draws + beta_t_draws * mean(starfish$temp) + beta_s_draws + beta_y_draws[, x_pred[i]]
}

df <- tibble(
  x_pred = x_pred,
  estimate = apply(pred_mat, 2, median),
  lower = apply(pred_mat, 2, quantile, 0.025),
  upper = apply(pred_mat, 2, quantile, 0.975)
)

gp <- ggplot(df) +
  aes(x = x_pred, y = estimate) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  xlab("Year") +
  ylab("Count") +
  NULL

print(gp)

# Both: Interaction effect
post <- as.matrix(mod$samples)
x_pred <- expand.grid(site = unique(starfish$site), year = unique(starfish$year))
beta_0_draws <- post[, "beta_0"]
beta_s_draws <- sapply(as.integer(x_pred$site), function(i) {post[, paste0("beta_s", "[", i, "]")]})
beta_t_draws <- post[, "beta_t"]
beta_y_draws <- sapply(as.integer(x_pred$year), function(i) {post[, paste0("beta_y", "[", i, "]")]})
beta_s_y_draws <- mapply(
  function(i, j) {post[, paste0("beta_s_y[", i, ",", j, "]")]}, 
  i = as.integer(x_pred$site), 
  j = as.integer(x_pred$year)
)

n_draws <- length(post[, 1])
n_x <- nrow(x_pred)
pred_mat <- matrix(NA, nrow = n_draws, ncol = n_x)

for (i in 1:n_x) {
  # Specify this equation exactly as in the model code
  # This time include the random effects, as we are looking explicitly at their
  # estimated values.
  log(pred_mat[, i]) <- beta_0_draws + beta_t_draws * mean(starfish$temp) + beta_s_draws[, i] + beta_y_draws[, i] + beta_s_y_draws[, i]
}

df <- tibble(
  site = x_pred$site,
  year = x_pred$year,
  estimate = apply(pred_mat, 2, median),
  lower = apply(pred_mat, 2, quantile, 0.025),
  upper = apply(pred_mat, 2, quantile, 0.975)
)

gp <- ggplot(df) +
  aes(x = year, y = estimate) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  facet_grid(rows = vars(site)) +
  xlab("Year") +
  ylab("Count") +
  NULL

print(gp)
