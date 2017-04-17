# INTRO -------------------------------------------------------------------
# Test simple preliminary models
#   Simon Kassel
#   Created: 23 Mar 17

# load helper functions
source("https://raw.githubusercontent.com/simonkassel/IRAIL/master/scripts/IRAIL_helper_functions_032317.R")

# load packages
packages(c("caret", "dplyr", "ggplot2", "C50", "spatstat", "rgdal", "plyr"))

# global options
options(stringsAsFactors = FALSE)
options(scipen = "999")

# data
dat <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/trip_data_cleaned.csv")
stations <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/stations.csv")


# INDEPENDENT VARIABLES ---------------------------------------------------
# glm mod
mod1 <- logitMod(dat, c("day_of_week", "hour", "to.avg_stop_times", 
                         "nr_of_stops", "vehicle_type"), seed = 456)

# decision tree moded
dtMod1 <- dtMod(dat, c("day_of_week", "hour", "to.avg_stop_times", 
                     "nr_of_stops", "vehicle_type", "to.major_hub"), seed = 456)


# PREDICTING WITH FIXED EFFECTS -------------------------------------------

# with line fixed effect p-values
# glm mod
lfe_vars <- c("day_of_week", "hour", "to.avg_stop_times", 
              "nr_of_stops", "vehicle_type", "line")
mod2 <- logitMod(dat, lfe_vars, seed = 456)


# with province fixed effect p-values
pfe_vars <- c("day_of_week", "hour", "to.avg_stop_times", "nr_of_stops", 
              "vehicle_type", "to_bruss", "to.prov")

mod3 <- logitMod(dat, pfe_vars, seed = 456)


# VARIABLE INTERACTIONS ---------------------------------------------------

dat <- d[, c("line", "to.prov", "occ_binary")]

set.seed(456)
inTrain <- createDataPartition(dat$occ_binary, p = .75, list = FALSE) 
training <- dat[inTrain, ]
testing <- dat[-inTrain, ]

glm_mod <- glm(occ_binary ~ . + to.prov:line, data = training)

testing <- catVars(training, testing)

pred_probs <- predict(glm_mod, testing) %>% abs() %>% unname()
plot(roc(testing$occ_binary, pred_probs, direction="<"),
     col="yellow", lwd=3, main="ROC Curve")

if (is.null(threshold)) {
  threshold <- quantile(na.omit(pred_probs), 
                        mean(dat$occ_binary %>% as.numeric())) %>% 
    unname() %>% round(digits = 3)
  print(paste0("No threshold specified, using default calculated threshold of ", threshold, "."))
}

predictions <- ifelse(pred_probs > threshold, 1, 0)
confusionMatrix(predictions, testing$occ_binary) %>% print()


