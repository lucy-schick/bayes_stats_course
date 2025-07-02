source("header.R")

# Load model object ------------------------------------------------------------
mod <- readRDS("output/w3/mod.rds")
climate <- readRDS("output/w3/climate.rds")

# Make predictions -------------------------------------------------------------
# Effect of elevation
post <- as.matrix(mod$samples)
beta_0_draws <- post[, "beta_0"]
beta_e_draws <- post[, "beta_e"]
beta_l_draws <- post[, "beta_l"]
sigma_draws <- post[, "sigma"]

x_pred <- seq(min(climate$elev), max(climate$elev), length.out = 30)

n_draws <- length(beta_0_draws)
n_x <- length(x_pred)
pred_mat <- matrix(NA, nrow = n_draws, ncol = n_x) 

# Pick one of the following scales:
type <- "term" # Shows the uncertainty in the expected value only.
# type <- "response" # Adds residual variation to show uncertainty on the scale of the response

for (i in 1:n_x) {
  # Specify this equation exactly as in the model code
  pred_mat[, i] <- beta_0_draws + beta_e_draws * x_pred[i] + beta_l_draws * mean(climate$lat)
  if (type == "response") {
    noise <- rnorm(n = n_draws, mean = 0, sd = sigma_draws)
    pred_mat[, i] <- pred_mat[, i] + noise
  }
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
  geom_point(data = climate, aes(x = elev, y = temp_anomaly), alpha = 0.2) +
  xlab("Elevation (m)") +
  ylab("Temperature Anomaly (˚C)") +
  NULL

print(gp)

# Effect of latitude
x_pred <- seq(min(climate$lat), max(climate$lat), length.out = 30)

n_draws <- length(beta_0_draws)
n_x <- length(x_pred)
pred_mat <- matrix(NA, nrow = n_draws, ncol = n_x) 

# Pick one of the following scales:
type <- "term" # Shows the uncertainty in the expected value only.
# type <- "response" # Adds residual variation to show uncertainty on the scale of the response

for (i in 1:n_x) {
  # Specify this equation exactly as in the model code
  pred_mat[, i] <- beta_0_draws + beta_e_draws * mean(climate$elev) + beta_l_draws * x_pred[i]
  if (type == "response") {
    noise <- rnorm(n = n_draws, mean = 0, sd = sigma_draws)
    pred_mat[, i] <- pred_mat[, i] + noise
  }
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
  geom_point(data = climate, aes(x = lat, y = temp_anomaly), alpha = 0.2) +
  xlab("Latitude (˚N)") +
  ylab("Temperature Anomaly (˚C)") +
  NULL

print(gp)
