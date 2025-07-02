source("header.R")

# Load data --------------------------------------------------------------------
data(starfish)

# Clean/tidy/explore data ------------------------------------------------------
str(starfish)
summary(starfish[c("temp", "count")])

ggplot(starfish) +
  aes(x = temp, y = count) +
  geom_point() + 
  xlab("Temperature (˚C)") +
  ylab("Count") +
  NULL

# Prepare data for JAGS --------------------------------------------------------
data <- list(
  nObs = nrow(starfish),
  nsite = nlevels(starfish$site),
  temp = starfish$temp,
  count = starfish$count,
  site = as.integer(starfish$site)
)

# Save data object -------------------------------------------------------------
if (!dir.exists("output/w4")) {
  dir.create("output/w4", recursive = TRUE) 
}

saveRDS(data, file = "output/w4/data.rds")
