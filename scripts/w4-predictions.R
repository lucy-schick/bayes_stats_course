source("header.R")

# Load model object ------------------------------------------------------------
mod <- readRDS("output/w4/mod.rds")

# Make predictions -------------------------------------------------------------
## Poisson GLM with continuous effect of temperature

# TODO: try adapting the prediction code from last week to generate your own
# prediction plot! Focus on the term-level prediction. I will show you what you
# need to do to get Poisson response-level predictions.


## Poisson GLM with continuous effect of temperature and discrete fixed effect
# of site
if (FALSE) {
  # Effect of temperature
  post <- as.matrix(mod$samples)
  x_pred <- seq(min(starfish$temp), max(starfish$temp), length.out = 30)
  beta_0_draws <- post[, "beta_0"]
  beta_site_draws <- post[, "beta_s[1]"] # Reference site will be Site A.
  beta_temp_draws <- post[, "beta_t"]
  
  n_draws <- length(beta_0_draws)
  n_x <- length(x_pred)
  pred_mat <- matrix(NA, nrow = n_draws, ncol = n_x) 
  
  # Pick one of the following scales:
  type <- "term" # Shows the uncertainty in the expected value only.
  # type <- "response" # Adds residual variation to show uncertainty on the scale of the response
  
  for (i in 1:n_x) {
    # Specify this equation exactly as in the model code
    log(pred_mat[, i]) <- beta_0_draws + beta_temp_draws * x_pred[i] + beta_site_draws
    if (type == "response") {
      pred_mat[, i] <- rpois(n = n_draws, lambda = pred_mat[, 1])
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
    expand_limits(y = 0) +
    xlab("Temperature (˚C)") +
    ylab("Count") +
    NULL
  
  print(gp)
  
  # Effect of site
  post <- as.matrix(mod$samples)
  x_pred <- unique(starfish$site)
  beta_0_draws <- post[, "beta_0"]
  beta_site_draws <- sapply(as.integer(x_pred), function(i) {post[, paste0("beta_s", "[", i, "]")]})
  beta_temp_draws <- post[, "beta_t"]
  
  n_draws <- length(beta_0_draws)
  n_x <- length(x_pred)
  pred_mat <- matrix(NA, nrow = n_draws, ncol = n_x) 
  
  # Pick one of the following scales:
  type <- "term" # Shows the uncertainty in the expected value only.
  # type <- "response" # Adds residual variation to show uncertainty on the scale of the response
  for (i in 1:n_x) {
    # Specify this equation exactly as in the model code
    log(pred_mat[, i]) <- beta_0_draws + beta_temp_draws * mean(starfish$temp) + beta_site_draws[, x_pred[i]]
    if (type == "response") {
      pred_mat[, i] <- rpois(n = n_draws, lambda = pred_mat[, 1])
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
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    xlab("Site") +
    ylab("Count") + 
    expand_limits(y = 0) +
    NULL
  
  print(gp)
}
