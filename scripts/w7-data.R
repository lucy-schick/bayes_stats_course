source("header.R")

# Load data --------------------------------------------------------------------
data(seeds)

# Clean/tidy/explore data ------------------------------------------------------
str(seeds)
summary(seeds)

# Prepare data for JAGS --------------------------------------------------------
data <- list(
  nObs = nrow(seeds),
  germinated = seeds$germinated
)

# Save data object -------------------------------------------------------------
if (!dir.exists("output/w7")) {
  dir.create("output/w7", recursive = TRUE) 
}

saveRDS(data, file = "output/w7/data.rds")
