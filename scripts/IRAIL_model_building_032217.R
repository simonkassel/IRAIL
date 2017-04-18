# INTRO -------------------------------------------------------------------
# Test simple preliminary models
#   Simon Kassel
#   Created: 23 Mar 17

# load helper functions
source("https://raw.githubusercontent.com/simonkassel/IRAIL/master/scripts/IRAIL_helper_functions_032317.R")

# load packages
packages(c("caret", "dplyr", "ggplot2", "C50", "spatstat", "rgdal", "plyr"))

# global options
options(stringsAsFactors = TRUE)
options(scipen = "999")

# data
md <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/model_variables.csv")
md <- md[,-1]



ggplot(md, aes(x = occ_binary, y = tf_avg_stop)) + geom_boxplot()
ggplot(md, aes(x = occ_binary, fill = to.groups)) + geom_bar(position = "fill")



set.seed(1234)
inTrain <- createDataPartition(md$occ_binary, p = .75, list = FALSE) 
training <- md[inTrain, ]
testing <- md[-inTrain, ]

dt_mod <- C5.0(training[,-1], training$occ_binary)


m <- glm(occ_binary ~ to_from_bruss + day_of_week + dist_rush + visibility +
           from.count + to.count + tf_avg_stop + to.Hainut + to.VlaamsBrabant +
          from.VlaamsBrabant + line7573 + line66 + line12 + windspeed + 
           vehicle_type * to.maj_groups + to.maj_groups + vehicle_type, 
         data = training, 
         family = binomial(logit), control = list(maxit = 50))

testing$pred <- predict(m, testing)
summary(m)
plot(roc(testing$occ_binary, testing$pred, direction="<"),
       col="yellow", lwd=3, main="ROC Curve")



ggplot(testing, aes(x = pred, y = occ_binary)) + geom_jitter(color = "blue") + 
  xlim(-1,2.5) + theme_fivethirtyeight()



t <- md[which(md$pred > 10 | md$pred < -10), ]$line %>% table() 
t[which(t>0)]

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


