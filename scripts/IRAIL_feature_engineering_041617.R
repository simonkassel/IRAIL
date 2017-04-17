# FEATURE ENGINEERING -----------------------------------------------------

# Clean, reshape and explore data
#   Simon Kassel
#   Created: 16 Apr 17

# load helper functions
source("https://raw.githubusercontent.com/simonkassel/IRAIL/master/scripts/IRAIL_helper_functions_032317.R")

# load packages
packages(c("plyr", "dplyr", "RCurl", "ggplot2","lubridate", "ggmap", "ggthemes", "chron",
           "dendextend"))

# global options
options(stringsAsFactors = FALSE)
options(scipen = "999")

# data
stations <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/stations.csv")
trips <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/trains_train.csv")
line_info <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/line_info.csv")
provinces <- readOGR("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/BE_provinces.geojson")
lbc <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/line_by_connection.csv")


# CLEAN STATION DATA ------------------------------------------------------
# station id field
stations$station <- stations$URI %>% 
  substr(., substrFunc(stations$URI), nchar(stations$URI))

# remove unnecessary vars
stations <- filter(stations, country.code == "be" & latitude > 50.37680)
stations <- stations %>% select(station, name, longitude, latitude, avg_stop_times)


# PROVINCE FIXED EFFECTS --------------------------------------------------
stations_sp <- SpatialPointsDataFrame(cbind(stations$longitude, stations$latitude),
                                      data = stations, proj4string = provinces@proj4string)

provinces@data$prov <- provinces@data$NAME_2

stations <- cbind(stations, over(stations_sp, provinces))[, c("station", "prov")] %>%
  left_join(stations, ., by = "station")


# NETWORK HIERARCHY -------------------------------------------------------
all_routes <- line_info$stopping_station_ids %>% as.character()
line_count <- data.frame(stations$station) 
names(line_count) <- "station"
line_count$count <- 0

for (i in line_count$station) {
  for (x in all_routes) {
    if (grepl(i, x)) {
      count_of_routes <- line_count[which(line_count$station == i), ]$count + 1
      line_count[which(line_count$station == i), ]$count <- count_of_routes 
    }
  }
}

stations <- left_join(stations, line_count)
stations <- findHubs(stations, 11)[,c("groups", "k", "maxcount")] %>% 
  cbind(stations, .)
stations$groups <- stations$groups %>% paste0("g", .) %>% as.factor()


stations$maj_groups <- stations[,c("count")] %>%
  dist(method = "euclidean") %>%
  hclust(method="ward.D") %>%
  cutree(5) %>%
  paste0("mg", .) %>%
  as.factor()

stations$major_hub <- ifelse(stations$maj_groups == "mg5", "Y", "N")
stations$non_hub <- ifelse(stations$maj_groups == "mg2", "Y", "N")

# CLEAN/JOIN DATA ------------------------------------------------------------

# join line to trips
trips <- plyr::join(trips, lbc[,c(2:3)], by = "connection", type = "left", match = "first") %>%
  na.omit()
trips$line <- paste0("l.", trips$line) %>% as.factor()

# Join together
dat <- trips %>% 
  joinToTrips( ., "from") %>%
  joinToTrips( ., "to")


# NEW VARIABLES -----------------------------------------------------------

# Occupancy binary variables
dat$occ_binary <- ifelse(dat$occupancy == "high", "H", "L") %>% as.factor()

# date-time
dat$date_time <- paste(dat$date, dat$time, sep = " ") %>%
  parse_date_time("Ymd HMS p") %>%
  round_date(unit = "hour")

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

# Rush hour variables
dat$dist_morning_rush <- ifelse(abs(dat$hour - 8) < abs(dat$hour - 32), abs(dat$hour - 8), abs(dat$hour - 32)) 
dat$dist_evening_rush <- ifelse(abs(dat$hour - 18) < abs(dat$hour + 8), abs(dat$hour - 18), abs(dat$hour + 8)) 
dat$dist_rush <- ifelse(dat$dist_morning_rush < dat$dist_evening_rush, dat$dist_morning_rush, dat$dist_evening_rush)

# dummy variable of to/from brussels
dat$from_bruss <- ifelse(grepl("Brussel", dat$from.name), "Y", "N") %>% as.factor()
dat$to_bruss <- ifelse(grepl("Brussel", dat$to.name), "Y", "N") %>% as.factor()
dat$to_from_bruss <- ifelse(dat$from_bruss == "Y" | dat$to_bruss == "Y", "Y", "N") %>% as.factor()

# vehicle line info
line_info$vehicle <- line_info$vehicle_id
dat <- left_join(dat, select(line_info, vehicle, vehicle_type, nr_of_stops))

# clean up the vehicle type a bit
dat$vehicle_type <- ifelse(dat$vehicle_type %in% c("ICE", "THA", "TRN"), "IC", dat$vehicle_type)

# WEATHER -----------------------------------------------------------------

weather_data <- c("july_1", "july_2", "aug_1", "aug_2", "sep_1", "sep_2", "oct_1", "oct_2")

dat <- ldply(weather_data, findWeather) %>%
  left_join(dat, .)
dat$weather_type <- dat$weather_type %>% paste0("wt.", .) %>% as.factor()


# OUTPUT MODEL DATASET ----------------------------------------------------

# Stations
write.csv(stations, "stations_cleaned.csv")

# Trips
allrows <- nrow(dat)
dat <- na.omit(dat)
remainingrows <- nrow(dat)

paste0("Removed ", allrows - remainingrows, " rows with NA values.") %>% print()

write.csv(dat, "trip_data_cleaned.csv")

dat_p <- dat %>% select(occ_binary, line, hour, day_of_week, dist_rush, to_from_bruss,
                        vehicle_type, nr_of_stops,
                        temperature, humidity, windspeed, visibility, weather_type,
                        from.avg_stop_times, from.prov, from.count, from.groups, from.maxcount, 
                        from.maj_groups, from.major_hub, from.non_hub,
                        to.avg_stop_times, to.prov, to.count, to.groups, to.maxcount, 
                        to.maj_groups, to.major_hub, to.non_hub)

remove(lbc, line_count, test_trips, all_routes, allrows, count_of_routes, i, remainingrows,
       weather_data, x, stations_sp, provinces)


