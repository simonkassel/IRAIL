# INTRO -------------------------------------------------------------------
# Explore dataset through visualization and summary statistics
#   Simon Kassel
#   Created: 1 Feb 17

# load helper functions
source("https://raw.githubusercontent.com/simonkassel/IRAIL/master/scripts/IRAIL_helper_functions_032317.R")

# load packages
packages(c("plyr", "dplyr", "ggplot2", "ggmap", "ggthemes", "chron"))

# global options
options(stringsAsFactors = FALSE)
options(scipen = "999")

# data
stations <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/stations.csv")
trips <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/trains_train.csv")
test_trips <- read.csv("https://inclass.kaggle.com/blobs/download/forum-message-attachment-files/5802/trains_test.csv")
line_info <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/line_info.csv")


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
  geom_point(data = stations, aes(x = longitude, y = latitude), size = .25) +
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


# NETOWRK HIERARCHY -------------------------------------------------------

# needs to be cleaned up
st$quint <- ntile(st$count, 10) 

bmc <- get_googlemap(center = c(mean(st$longitude), mean(st$latitude)), zoom = 8, color = "bw")


# Map stations in and out of training set
map.stations <- ggmap(bmc) + 
  geom_point(data = st, aes(x = longitude, y = latitude, color = count), size = 1) +
  theme_map() +
  #scale_color_brewer("Station in \nTraining Set?") +
  ggtitle("Belgian Train Stations") +
  theme( 
    legend.position = c(.05,.85),
    legend.direction = "horizontal",
    plot.title = element_text(face = "bold", hjust = "0.5", size = 14))
ggsave("IRAIL_stage1_mapping_stations.pdf", map.stations, device = "pdf", width = 8.5, height = 11, units = "in")


# mapping clusters
hubs <- findHubs(st, 5)
for (i in c(6:13)) {
  temp <- findHubs(st, i)
  hubs <- rbind(hubs, temp)
}

ggplot(hubs, aes(longitude, latitude, color = as.factor(groups))) + geom_point(size = 0.5) +
  geom_label(data = filter(hubs, maxcount == count), aes(label = name), size = 2) + 
  facet_wrap(~k, ncol = 3) + theme_void() + theme(legend.position = "none")


sorted <- arrange(st, desc(count))
sorted$count_rank <- c(1:nrow(sorted))

maxhubs <- head(sorted)
maxhubs$h <- "hubs = 5"
for (i in c(6:13)) {
  temp <- head(sorted, i)
  temp$h <- paste0("hubs = ", i)
  maxhubs <- rbind(maxhubs, temp)
}

ggmap(bmc) + 
  geom_point(data = maxhubs, aes(longitude, latitude, color = count), size = 2) + 
  facet_wrap(~h, ncol = 3) + theme_void() + theme(legend.position = "none")



ggplot(filter(hubs, maxcount == count & k == "k = 11"), aes(x = groups, y = count)) + 
  geom_bar(stat = "identity") + facet_wrap(~k)


