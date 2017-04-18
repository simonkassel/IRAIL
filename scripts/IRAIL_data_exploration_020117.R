# INTRO -------------------------------------------------------------------
# Explore dataset through visualization and summary statistics
#   Simon Kassel
#   Created: 1 Feb 17

# load helper functions
source("https://raw.githubusercontent.com/simonkassel/IRAIL/master/scripts/IRAIL_helper_functions_032317.R")

# load packages
packages(c("plyr", "dplyr", "ggplot2", "ggmap", "ggthemes", "chron", "tidyr", "reshape2"))

# global options
options(stringsAsFactors = TRUE)
options(scipen = "999")

# data
stations <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/stations_cleaned.csv")
dat <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/trip_data_clean.csv")
md <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/model_variables.csv")
md <- md[,-1]

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



# PREDICTOR VARIABLES -----------------------------------------------------

catv <- md[, !sapply(md, is.numeric)]
catv$occ_binary <- md$occ_binary

catv_tidy <- melt(catv, id.vars = "occ_binary", measure.vars = names(catv)[which(names(catv) != "occ_binary")])

ggplot(catv_tidy, aes(x = as.factor(occ_binary), 
                      fill = value)) + 
  geom_bar(position = "fill") + facet_wrap(~variable) + 
  labs(
    title = "Categorical predictors"
  ) +
  xlab("Train traffice level (0=low, 1=high)") +
  theme_minimal() + theme(
    legend.position = "none",
    axis.text.y = element_blank()
  )

conv <- md[, sapply(md, is.numeric)]
conv_tidy <- melt(conv, id.vars = "occ_binary", measure.vars = names(conv)[which(names(conv) != "occ_binary")])

ggplot(conv_tidy, aes(x = as.factor(occ_binary), 
                      y = value)) + 
  geom_boxplot() + facet_wrap(~variable, scales = "free") + 
  labs(
    title = "Continuous predictors"
  ) +
  xlab("Train traffic level (0=low, 1=high)") +
  theme_minimal() + theme(
    legend.position = "none",
    axis.text.y = element_blank()
  )

# NETOWRK HIERARCHY -------------------------------------------------------

###
mult_k <- ldply(c(5:13), function(x) {
  return(findHubs(stations, x))
})
mult_k$k <- factor(mult_k$k, levels(mult_k$k)[c(5:9, 1:4)])

ggplot(mult_k, aes(x = longitude, y = latitude, color = as.factor(groups))) + 
  geom_point() + geom_label(data = filter(mult_k, maxcount == count), aes(label = name), size = 2) +
  theme_void() + facet_wrap(~k, ncol = 3) + ggtitle("Spatial Clustering of Stations, different numbers (k) of clusters") +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )

stations$maj_groups <- as.factor(stations$maj_groups)
stations$maj_groups <- factor(stations$maj_groups, levels(stations$maj_groups)[c(2,1,3:5)])

leg_labels <- ddply(stations, ~maj_groups, summarise, name = paste0(min(count), " - ", max(count)))$name
pal <- c('#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494')

ggplot(stations, aes(x = longitude, y = latitude, color = maj_groups)) + 
  geom_point(size = 2) + 
  
  theme_void() + labs(title = "Belgian Rail Hierarchy", subtitle = "# of trains to come through each station") +
  scale_color_manual("Number of trains", values = pal, labels = leg_labels) + 
  geom_label(data = filter(stations, major_hub == "Y"), aes(label = name), size = 2) +
  theme(
    legend.position = "right",
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in")
  )



stations$maj_groups <- stations[,c("count")] %>%
  dist(method = "euclidean") %>%
  hclust(method="ward.D") %>%
  cutree(5) %>%
  paste0("mg", .) %>%
  as.factor()








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


