source("header.R")

# Load data --------------------------------------------------------------------
data(starfish)

# Clean/tidy/explore data ------------------------------------------------------
str(starfish)
summary(starfish[c("site", "year", "temp", "count")])

ggplot(starfish) +
  aes(x = site, y = count) +
  geom_point() +
  xlab("Site") +
  ylab("Count") +
  NULL

ggplot(starfish) +
  aes(x = year, y = count) +
  geom_point() +
  xlab("Year") +
  ylab("Count") +
  NULL

ggplot(starfish) +
  aes(x = temp, y = count) +
  geom_point() +
  xlab("Temperature (˚C)") +
  ylab("Count") +
  NULL

# Add ID column:
starfish %<>% mutate(id = factor(1:n()))

# Prepare data for JAGS --------------------------------------------------------
data <- list(
  nObs = nrow(starfish),
  nsite = nlevels(starfish$site),
  site = as.integer(starfish$site),
  nyear = nlevels(starfish$year),
  year = as.integer(starfish$year),
  nid = nlevels(starfish$id),
  id = as.integer(starfish$id),
  temp = starfish$temp,
  count = starfish$count
)

# Save data object -------------------------------------------------------------
if (!dir.exists("output/w6")) {
  dir.create("output/w6", recursive = TRUE) 
}

saveRDS(data, file = "output/w6/data.rds")
