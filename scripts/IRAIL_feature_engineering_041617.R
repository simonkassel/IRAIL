# FEATURE ENGINEERING -----------------------------------------------------

# Clean, reshape and explore data
#   Simon Kassel
#   Created: 16 Apr 17

# load helper functions
source("https://raw.githubusercontent.com/simonkassel/IRAIL/master/scripts/IRAIL_helper_functions_032317.R")

# load packages
packages(c("plyr", "dplyr", "RCurl", "ggplot2","lubridate", "ggmap", "ggthemes", "chron"))

# global options
options(stringsAsFactors = FALSE)
options(scipen = "999")

# data
stations <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/stations.csv")
trips <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/trains_train.csv")
line_info <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/line_info.csv")
provinces <- readOGR("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/BE_provinces.geojson")

# CLEAN/JOIN DATA ------------------------------------------------------------

# station id field
stations$station <- stations$URI %>% 
  substr(., substrFunc(stations$URI), nchar(stations$URI))

# remove unnecessary vars
stations <- filter(stations, country.code == "be" & latitude > 50.37680)
stations <- stations %>% select(station, name, longitude, latitude, avg_stop_times)

# Join together
dat <- trips %>% 
  joinToTrips( ., "from") %>%
  joinToTrips( ., "to")

# NEW VARIABLES -----------------------------------------------------------

# Occupancy binary variables
dat$occ_binary <- ifelse(dat$occupancy == "high", 1, 0) %>% as.factor()

# date-time
dat$date_time <- paste(dat$date, dat$time, sep = " ")
dat$date_time <- parse_date_time(dat$date_time,  "Ymd HMS p")

# date as date obj
dat$date <- as.Date(dat$date)

# numeric time variables
dat$hour <- dat$date_time %>%
  as.character() %>%
  substr(12, 13) %>%
  as.numeric() 
dat$min <- dat$date_time %>%
  as.character() %>%
  substr(15, 16) %>%
  as.numeric()
dat$min_of_day <- (60 * dat$hour) + dat$min

# Day of week
dat$day_of_week <- weekdays(dat$date) %>%
  factor(levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
dat$weekend <- ifelse(dat$day_of_week == "Sunday" | dat$day_of_week == "Saturday", "Y", "N")

# Rush hour variables
dat$dist_morning_rush <- ifelse(abs(dat$hour - 8) < abs(dat$hour - 32), abs(dat$hour - 8), abs(dat$hour - 32)) 
dat$dist_evening_rush <- ifelse(abs(dat$hour - 18) < abs(dat$hour + 8), abs(dat$hour - 18), abs(dat$hour + 8)) 
dat$dist_rush <- ifelse(dat$dist_morning_rush < dat$dist_evening_rush, dat$dist_morning_rush, dat$dist_evening_rush)

# dummy variable of to/from brussels
dat$from_bruss <- ifelse(grepl("Brussel", dat$from.name), 1, 0) %>% as.factor()
dat$to_bruss <- ifelse(grepl("Brussel", dat$to.name), 1, 0) %>% as.factor()
dat$to_from_bruss <- ifelse(dat$from_bruss == 1 | dat$to_bruss == 1, 1, 0)

# vehicle line info
line_info$vehicle <- line_info$vehicle_id
dat <- left_join(dat, select(line_info, vehicle, vehicle_type, nr_of_stops))

# remove records without a corresponding vehicle line
dat <- na.omit(dat)


# PROVINCE FIXED EFFECTS --------------------------------------------------
stations_sp <- SpatialPointsDataFrame(cbind(stations$longitude, stations$latitude),
                                      data = stations, proj4string = provinces@proj4string)

swp <- cbind(stations, over(stations_sp, provinces))[, c("station", "NAME_2")]

names(swp) <- c("from.station", "from.prov")
dat <- left_join(dat, swp, by = "from.station")

names(swp) <- c("to.station", "to.prov")
dat <- left_join(dat, swp, by = "to.station")



# OUTPUT MODEL DATASET ----------------------------------------------------
write.csv(dat, "trip_data_cleaned.csv")


