library(tidyverse)
library(rworldmap)
library(gganimate)

# Define a function for adding zerio
add_zero <- 
  function (x, n) 
  {
    x <- as.character(x)
    adders <- n - nchar(x)
    adders <- ifelse(adders < 0, 0, adders)
    for (i in 1:length(x)) {
      if (!is.na(x[i])) {
        x[i] <- paste0(paste0(rep("0", adders[i]), collapse = ""), 
                       x[i], collapse = "")
      }
    }
    return(x)
  }

# Read data
world <- map_data(map="world")
hurricanes <- read_csv('data/Allstorms.ibtracs_wmo.v03r09.csv', skip = 1)
cnames <- names(hurricanes)
hurricanes <- read_csv('data/Allstorms.ibtracs_wmo.v03r09.csv', skip = 2)
names(hurricanes) <- cnames

# Process hurricanes data
hurricanes <-
  hurricanes %>%
  mutate(year = Season) %>%
  # Keep only those since 1917
  filter(year >= 1917) %>%
  mutate(date_time = ISO_time,
         lon = Longitude,
         lat = Latitude,
         id = Serial_Num,
         wind = `Wind(WMO)`,
         pressure = `Pres(WMO) Percentile`) %>%
  arrange(date_time) %>%
  # Keep out those which cross the longitudinal start-over
  group_by(id) %>%
  mutate(cross_over = any(lon < -100) & any(lon > 100)) %>%
  ungroup %>%
  filter(!cross_over) %>%
  # Keep only those in the Atlantic basin
  group_by(id) %>%
  filter(any(lon >-90) & any(lon < -10),
         any(lat > 10) & (any(lat < 40))) %>%
  # Get max wind
  mutate(max_wind = max(wind)) %>%
  ungroup %>%
  # Make ids simple
  mutate(id = as.numeric(factor(id)))

# Get an index
hurricanes$index <- 1:nrow(hurricanes)
hurricanes <- hurricanes %>%
  group_by(id) %>%
  mutate(max_index = max(index)) %>%
  ungroup

# Define ids of storms
ids <- sort(unique(hurricanes$id))
years <- sort(unique(hurricanes$year))
max_indices <- sort(unique(hurricanes$max_index))

# Define some colors
colors <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, 'Spectral'))(length(ids))
colors <- sample(colors, length(colors))
year_colors <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, 'Spectral'))(length(years))
year_colors <- sample(year_colors, length(year_colors))
wind_colors <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, 'Spectral'))(max(hurricanes$max_wind))
wind_colors <- rev(wind_colors)

# Map the world
map('world', 
    xlim = c(-90, -10),
    ylim = c(10, 40),
    fill = TRUE,
    col = adjustcolor('black', alpha.f = 0.6))


dir.create('pngs')
dir.create('pngs2')

lmi <- length(max_indices)
for (i in c(seq(10, round(lmi), 10),
            rep(max(lmi), 20))){
  message(paste0(i, ' of ', lmi))
  this_max <- max_indices[i]
  sub_data <- hurricanes %>%
    filter(max_index <= this_max)
  # Get each line separetely for each id
  sub_ids <- sort(unique(sub_data$id))
  file_name <- 
    paste0(add_zero(this_max, 15), '.png')
  png(filename = paste0('pngs2/', file_name),
      width = 600,
      height = 400)
  this_year <- max(sub_data$year)
  map('world', 
      xlim = c(-90, -10),
      ylim = c(10, 40),
      fill = TRUE,
      col = adjustcolor('black', alpha.f = 1))
  title(main = this_year)
  for (j in 1:length(sub_ids)){

    sub_sub_data <- sub_data %>%
      filter(id == sub_ids[j])
    lines(sub_sub_data$lon,
          sub_sub_data$lat,
          col = adjustcolor('darkred', alpha.f = 0.8))
  }
  dev.off()
}

