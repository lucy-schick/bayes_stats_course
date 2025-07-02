source("header.R")

# Load model object ------------------------------------------------------------
mod <- readRDS("output/w7/mod.rds")

# Make predictions -------------------------------------------------------------
# Effect 
post <- as.matrix(mod$samples)
# TODO: replace x_pred with a sequence values for your chosen effect
x_pred <- 1
beta_0_draws <- post[, "beta_0"]
# TODO: add other draws in here

n_draws <- length(beta_0_draws)
n_x <- length(x_pred)
pred_mat <- matrix(NA, nrow = n_draws, ncol = n_x) 

for (i in 1:n_x) {
  # Specify this equation exactly as in the model code
  # !! Except we'll exclude the random effects when not looking directly at that effect.
  logit(pred_mat[, i]) <- beta_0_draws
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
  expand_limits(y = c(0, 1)) +
  xlab("") +
  ylab("Probability of Germination") +
  NULL

print(gp)
