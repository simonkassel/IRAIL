# INTRO -------------------------------------------------------------------
# Test simple preliminary models
#   Simon Kassel
#   Created: 23 Mar 17

# load helper functions
source("https://raw.githubusercontent.com/simonkassel/IRAIL/master/scripts/IRAIL_helper_functions_032317.R")

# load packages
packages(c("caret", "dplyr", "ggplot2", "C50", "spatstat"))

# global options
options(stringsAsFactors = FALSE)
options(scipen = "999")

# data
dat2 <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/trip_data_cleaned.csv")
stations <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/stations.csv")


# INDEPENDENT VARIABLES ---------------------------------------------------

# partition data
set.seed(123)
inTrain <- createDataPartition(dat2$occ_binary, p = .75, list = FALSE) 

training <- dat2[inTrain, ]
testing <- dat2[-inTrain, ]

# logistic regression
glm_mod <- glm(occ_binary ~ day_of_week + nr_of_stops + 
                  to_bruss + from_bruss + hour + vehicle_type + 
                  dist_rush + to.avg_stop_times + from.avg_stop_times, 
                data = training)

for (i in unique(testing$vehicle_type)) {
  if (!i %in% unique(training$vehicle_type)) {
    testing <- testing[which(testing$vehicle_type != i), ]
  }
}

testing$glm_pred <- predict(glm_mod, testing)
threshold <- quantile(na.omit(testing$glm_pred), nrow(testing)/nrow(training)) %>% unname()
testing$glm_prediction <- ifelse(testing$glm_pred > threshold, 1, 0) 
testing$result <- ifelse(testing$glm_prediction == testing$occ_binary, 1, 0)
mean(na.omit(testing$result)) # 49% accuracy

# decision tree
dt_mod <- C5.0(
  training %>% select(day_of_week, nr_of_stops, to_bruss, 
                      from_bruss, hour, vehicle_type, 
                      dist_rush, to.avg_stop_times, 
                      from.avg_stop_times), 
  training$occ_binary %>% as.factor()
  )
summary(dt_mod)

testing$dt_pred <- predict(dt_mod, testing)
testing$dt_result <- testing$dt_pred == testing$occ_binary
testing$dt_result  
table(testing$dt_result)[2] / nrow(testing) # 68% accuracy


# STATION PCA -------------------------------------------------------------

# compute new variables for each station
# Measure dist to five nearest stations
be_window <- owin(xrange = range(stations$longitude), yrange = range(stations$latitude))

stations_pp <- ppp(stations$longitude, stations$latitude, window = be_window)

dist_table <- nncross(stations_pp, stations_pp, what = c("dist"), 
                      k = c(2:6), is.sorted.X = FALSE, is.sorted.Y = FALSE)
# variable in stations data frame
stations$dist_5_stations <- (dist_table$dist.2 + dist_table$dist.3 + 
    dist_table$dist.4 + dist_table$dist.5 + dist_table$dist.6) / 5


# count the number of vehicles that stop at each station
stations$num_trains <- 0
iter <- 0

for (x in row.names(stations)) {
  station_id <- stations[x, "station"]
  counter <- 0
  for (i in row.names(line_info)) {
    if (grepl(station_id, line_info[i, "stopping_station_ids"])) {
      counter <- counter + 1
    }
  }
  stations[x, "num_trains"] <- counter
  iter <- iter + 1
  print(paste("Completed [", iter, "] of ", nrow(stations), " stations.", sep = ""))
}







