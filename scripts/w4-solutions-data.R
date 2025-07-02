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

# Change levels of the site variable
message("Changed base level of the site variable to Site C")
starfish %<>% mutate(site = factor(site, levels = c("Site C", "Site A", "Site B")))

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
saveRDS(starfish, file = "output/w4/starfish.rds")
