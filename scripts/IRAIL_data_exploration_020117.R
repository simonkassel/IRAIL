# INTRO -------------------------------------------------------------------
# Clean, reshape and explore data
#   Simon Kassel
#   Created: 1 Feb 17

# working directory
setwd("~/Dropbox/IRAIL/data")

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
test_trips <- read.csv("https://inclass.kaggle.com/blobs/download/forum-message-attachment-files/5802/trains_test.csv")
line_info <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/line_info.csv")

# CLEAN/JOIN DATA ------------------------------------------------------------

# generate a station code from URI field
substrFunc <- function(i){
  return(rev(regexpr("\\/[^\\/]*$", i)) + 1)
}

stations$station <- stations$URI %>% 
  substr(., substrFunc(stations$URI), nchar(stations$URI))

# Clean column names
fromToRename <- function(prefix){
  colnames(stations) <- gsub("alternative.", "", names(stations))
  colnames(stations) <- paste(prefix, names(stations), sep = ".")
  colnames(stations)[ncol(stations)] <- prefix 
  return(stations)
}

# Join in one dataset
joinToTrips <- function(tripDat, prefix){
  dat <- join(tripDat, fromToRename(prefix), by = prefix, type = "left", match = "all")
  return(dat)
}

# Join together
dat <- trips %>% 
  joinToTrips( ., "from") %>%
  joinToTrips( ., "to")

# NEW VARIABLES -----------------------------------------------------------

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

# Occupancy
dat$occupancy <- factor(dat$occupancy, levels = c("low", "medium", "high"))
dat$occ_num <- ifelse(dat$occupancy == "high", 3, ifelse(dat$occupancy == "medium", 2, 1))

# MAP THE STATIONS --------------------------------------------------------
# new station variables
stations$instudy <- ifelse(stations$station %in% unique(trips$to), "Y", "N") %>% factor(levels = c("Y", "N"))

ggplot(stations, aes(x = instudy)) + geom_bar(stat = "count") +
  theme(
    axis.ticks = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    axis.title = element_text(face = "italic")
  ) +
  ylab("Count") + xlab("Does the station have a measurement?") +
  ggtitle("Stations in and out of the sample")

# bounding box
get_Belgium_basemap <- function(){
  bbox <- c(min(stations$longitude), min(stations$latitude), 
            max(stations$longitude), max(stations$latitude))
  bm <- get_stamenmap(bbox = bbox, maptype = "toner-background")
}

# Get basemap
bm <- get_googlemap(center = c(mean(stations$longitude), mean(stations$latitude)), zoom = 7, color = "bw")

# Map stations in and out of training set
map.stations <- ggmap(bm) + 
  geom_point(data = stations, aes(x = longitude, y = latitude, color = instudy), size = .25) +
  theme_map() +
  scale_color_fivethirtyeight("Station in \nTraining Set?") +
  ggtitle("Belgian Train Stations") +
  theme( 
    legend.position = c(.05,.85),
    legend.direction = "horizontal",
    plot.title = element_text(face = "bold", hjust = "0.5", size = 14))
ggsave("IRAIL_stage1_mapping_stations.pdf", map.stations, device = "pdf", width = 8.5, height = 11, units = "in")

# TIME INTERVALS ----------------------------------------------------------

# cor b/w occupancy and weekend
weekday.cor.jitter <- ggplot(dat, aes(x = occupancy, y = weekend)) + geom_jitter() +
  theme_fivethirtyeight() + xlab("ridership") + ylab("weekend day?") +
  ggtitle("Correlation between ridership level and weekday/weekend") +
  theme(
    axis.title = element_text(face = "italic", colour = "grey50"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
ggsave("IRAIL_stage1_weekday-weekend_ridership_jitter.pdf", weekday.cor.jitter, device = "pdf", width = 11, height = 8.5, units = "in")

# trip segments by day of week
trip.segments.dow <- ggmap(bm) + 
  geom_segment(data = dat, aes(x = from.longitude, y = from.latitude, xend = to.longitude, yend = to.latitude,  
                   colour = occupancy), size = .5) +
  ggtitle("Train ridership by day-of-week") + 
  theme_fivethirtyeight() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank()
  ) +
  facet_wrap(~day_of_week, ncol = 4) 
ggsave("IRAIL_stage1_trip_segments_by_dow.pdf", trip.segments.dow, device = "pdf", width = 11, height = 8.5, units = "in")

# trip segments by hour of day
trip.segments.hour.of.day <- ggmap(bm) + 
  geom_segment(data = dat, aes(x = from.longitude, y = from.latitude, xend = to.longitude, yend = to.latitude,  
                               colour = occupancy), size = .5) +
  ggtitle("Train ridership by hour of the day") + 
  theme_fivethirtyeight() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank()
  ) +
  facet_wrap(~hour, ncol = 6) 
ggsave("IRAIL_stage1_trip_segments_by_hod.pdf", trip.segments.hour.of.day, device = "pdf", width = 11, height = 8.5, units = "in")

# Bar plot of observations by day of the week
day.barplot <- ggplot(dat, aes(x = day_of_week)) + 
  geom_bar(stat = "count", fill = "grey50") + 
  ylab("Count of samples") +
  ggtitle("Train usage samples by day of week") +
  theme(
    axis.title = element_text(face = "italic"),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    axis.ticks = element_blank()
    )
ggsave("IRAIL_stage1_bar_plot_samples_by_dow.pdf", day.barplot, device = "pdf", width = 11, height = 8.5, units = "in")

# Observations time series
daily.obs <- dat$date %>%
  table() %>%
  data.frame()

colnames(daily.obs) <- c("date", "obs")
daily.obs$date <- as.Date(daily.obs$date)

irail.collection.timeseries <- ggplot(daily.obs, aes(x = date, y = obs)) + 
  geom_line() + 
  geom_point(aes(colour = wday(date, label = TRUE))) +
  scale_color_discrete("Day of \nthe Week") +
  ggtitle("IRAIL traffic data collection time series") +
  ylab("# of measurements") + 
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.minor = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey90"),
    axis.title.y = element_text(face = "italic"),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24))
ggsave("IRAIL_stage1_data_collection_time_series.pdf", day.barplot, device = "pdf", width = 11, height = 8.5, units = "in")

ggplot(dat, aes(x = occupancy, fill = occupancy)) + geom_bar(stat = "count") +
  ggtitle("Dist. of training set occupancy levels") + xlab("") + ylab("Count")


# ADDITIONAL FEATURES -----------------------------------------------------
# Occupancy binary variables
dat$occ_binary <- ifelse(dat$occupancy == "high", 1, 0) %>% as.factor()

# Rush hour variables
dat$dist_morning_rush <- ifelse(abs(dat$hour - 8) < abs(dat$hour - 32), abs(dat$hour - 8), abs(dat$hour - 32)) 
dat$dist_evening_rush <- ifelse(abs(dat$hour - 18) < abs(dat$hour + 8), abs(dat$hour - 18), abs(dat$hour + 8)) 
dat$dist_rush <- ifelse(dat$dist_morning_rush < dat$dist_evening_rush, dat$dist_morning_rush, dat$dist_evening_rush)

# dummy variable of to/from brussels
dat$from_bruss <- ifelse(grepl("Brussel", dat$from.name), 1, 0) %>% as.factor()
dat$to_bruss <- ifelse(grepl("Brussel", dat$to.name), 1, 0) %>% as.factor()
dat$to_from_bruss <- ifelse(dat$from_bruss == 1 | dat$to_bruss == 1, 1, 0)

line_info$vehicle <- line_info$vehicle_id
dat <- left_join(dat, select(line_info, vehicle, vehicle_type, nr_of_stops))

# REMOVE ROWS -------------------------------------------------------------
# Just trips starting originitating in and ending up in the northern part of belgium
dat2 <- dat[which(dat$from.country.code == "be" & 
                    dat$to.country.code == "be" & 
                    dat$to.latitude > 50.37680), ]


# REGION FIXED EFFECTS ----------------------------------------------------

provinces <- readOGR("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/BE_provinces.geojson")

stations$station <- stations$name
stations_sp <- SpatialPointsDataFrame(cbind(stations$longitude, stations$latitude),
                                      data = stations, proj4string = provinces@proj4string)

swp <- cbind(stations, over(stations_sp, provinces))[, c("station", "NAME_2")]
swp$station <- as.numeric(swp$station)

names(swp) <- c("from.name", "from.prov")
dat3 <- left_join(dat2, swp, by = "from.name")

names(swp) <- c("to.name", "to.prov")
dat4 <- left_join(dat3, swp, by = "to.name")

dat4$to.prov <- as.factor(dat4$to.prov)
dat4$from.prov <- as.factor(dat4$from.prov)


# OUTPUT MODEL DATASET ----------------------------------------------------

write.csv(dat4, "trip_data_cleaned.csv")
