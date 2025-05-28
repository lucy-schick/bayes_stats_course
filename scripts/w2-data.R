source("header.R")

# Load data --------------------------------------------------------------------
data(penguins)

# Clean/tidy/explore data ------------------------------------------------------
str(penguins)
summary(penguins) # 2 NAs in body_mass_g

penguins %<>% drop_na(body_mass_g)

# Prepare data for JAGS --------------------------------------------------------
data <- list(
  nObs = nrow(penguins),
  mass = penguins$body_mass_g
)

# Save data object -------------------------------------------------------------
if (!dir.exists("output/w2")) {
  dir.create("output/w2", recursive = TRUE) 
}

saveRDS(data, file = "output/w2/data.rds")
