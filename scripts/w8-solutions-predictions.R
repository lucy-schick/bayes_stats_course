source("header.R")

# Load model object ------------------------------------------------------------
mod <- readRDS("output/w8/mod.rds")
seeds <- readRDS("output/w8/seeds.rds")


# Make predictions -------------------------------------------------------------
# Effect of herbicide
post <- as.matrix(mod$samples)
x_pred <- unique(seeds$herbicide)
beta_0_draws <- post[, "beta_0"]
beta_herbicide_draws <- sapply(as.integer(x_pred), function(i) {post[, paste0("beta_herbicide", "[", i, "]")]})

n_draws <- length(beta_0_draws)
n_x <- length(x_pred)
pred_mat <- matrix(NA, nrow = n_draws, ncol = n_x) 

for (i in 1:n_x) {
  # Specify this equation exactly as in the model code
  # !! Except we'll exclude the random effects when not looking directly at that effect.
  logit(pred_mat[, i]) <- beta_0_draws + beta_herbicide_draws[, x_pred[i]]
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
  xlab("Herbicide Applied") +
  ylab("Probability of Germination") +
  NULL

print(gp)

# We could also look at the effect of tray id.
# It will show us the spread of the values of the random effect
post <- as.matrix(mod$samples)
x_pred <- unique(seeds$tray_id)
beta_0_draws <- post[, "beta_0"]
beta_herbicide_draws <- post[, "beta_herbicide[1]"]
beta_tray_id_draws <- sapply(as.integer(x_pred), function(i) {post[, paste0("beta_tray_id", "[", i, "]")]})

n_draws <- length(beta_0_draws)
n_x <- length(x_pred)
pred_mat <- matrix(NA, nrow = n_draws, ncol = n_x) 

for (i in 1:n_x) {
  # Specify this equation exactly as in the model code
  # !! Except we'll exclude the random effects when not looking directly at that effect.
  logit(pred_mat[, i]) <- beta_0_draws + beta_herbicide_draws + beta_tray_id_draws[, x_pred[i]]
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
  xlab("Tray ID") +
  ylab("Probability of Germination") +
  NULL

print(gp)
