# Clear environment ------------------------------------------------------------
rm(list = ls())

# Install and load course package ----------------------------------------------
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}

if (!requireNamespace("intro2jags")) {
  remotes::install_github("poissonconsulting/intro2jags")
}

library(intro2jags)
