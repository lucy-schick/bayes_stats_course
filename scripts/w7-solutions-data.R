source("header.R")

# Load data --------------------------------------------------------------------
data(seeds)

# Clean/tidy/explore data ------------------------------------------------------
str(seeds)
summary(seeds)

# Prepare data for JAGS --------------------------------------------------------
data <- list(
  nObs = nrow(seeds),
  germinated = seeds$germinated,
  ntray_id = nlevels(seeds$tray_id),
  tray_id = as.integer(seeds$tray_id),
  nherbicide = nlevels(seeds$herbicide),
  herbicide = as.integer(seeds$herbicide),
  seed_mass = seeds$seed_mass,
  nwatering_freq = nlevels(seeds$watering_freq),
  watering_freq = as.integer(seeds$watering_freq),
  nsoil_type = nlevels(seeds$soil_type),
  soil_type = as.integer(seeds$soil_type),
  seed_depth = seeds$seed_depth
)

# Save data object -------------------------------------------------------------
if (!dir.exists("output/w7")) {
  dir.create("output/w7", recursive = TRUE) 
}

saveRDS(data, file = "output/w7/data.rds")
saveRDS(seeds, file = "output/w7/seeds.rds")
