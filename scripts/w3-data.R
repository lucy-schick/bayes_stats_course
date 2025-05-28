source("header.R")

# Load data --------------------------------------------------------------------
data(climate)

# Clean/tidy/explore data ------------------------------------------------------
str(climate)
summary(climate[c("temp_anomaly", "elev", "lat")])

gp <- ggplot(climate) +
  aes(x = temp_anomaly, y = elev) + 
  geom_point() +
  xlab("Elevation (m)") +
  ylab("Temperature Anomaly (˚C)") + 
  NULL

gp

gp <- ggplot(climate) +
  aes(x = temp_anomaly, y = lat) +
  geom_point() +
  xlab("Latitude (˚N)") +
  ylab("Temperature Anomaly (˚C)") + 
  NULL

gp

# Prepare data for JAGS --------------------------------------------------------
data <- list(
  nObs = nrow(climate),
  temp_anomaly = climate$temp_anomaly,
  elev = climate$elev,
  lat = climate$lat
)

# Save data object -------------------------------------------------------------
if (!dir.exists("output/w3")) {
  dir.create("output/w3", recursive = TRUE) 
}

saveRDS(climate, file = "output/w3/climate.rds")
saveRDS(data, file = "output/w3/data.rds")
