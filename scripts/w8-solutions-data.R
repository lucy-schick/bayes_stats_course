source("header.R")

# Load data --------------------------------------------------------------------
data(seeds)

# Clean/tidy/explore data ------------------------------------------------------
str(seeds)
summary(seeds)

# Aggregate data ---------------------------------------------------------------
seeds <- 
  seeds %>%
  group_by(tray_id) %>% # Calculate the following summary by tray group
  mutate(
    # Calculate binomial values by tray
    seeds = n(), # size parameter
    germinated = sum(germinated), # number of sucesses (response)
    # Average continuous parameters across groups
    seed_mass = mean(seed_mass),
    seed_depth = mean(seed_depth)
  ) %>%
  # Remove individual tray cell factor covariates
  select(-soil_type, -planting_time, -watering_freq) %>%
  # Reduce to one row per tray
  distinct() %>%
  ungroup()

# Prepare data for JAGS --------------------------------------------------------
data <- list(
  nObs = nrow(seeds),
  germinated = seeds$germinated,
  seeds = as.numeric(seeds$seeds),
  herbicide = as.integer(seeds$herbicide),
  nherbicide = nlevels(seeds$herbicide),
  tray_id = as.integer(seeds$tray_id),
  ntray_id = nlevels(seeds$tray_id)
)

# Save data object -------------------------------------------------------------
if (!dir.exists("output/w8")) {
  dir.create("output/w8", recursive = TRUE) 
}

saveRDS(data, file = "output/w8/data.rds")
saveRDS(seeds, file = "output/w8/seeds.rds")
