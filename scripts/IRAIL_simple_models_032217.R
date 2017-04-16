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
dat2 <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/trip_data_cleaned.csv")
stations <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/stations.csv")


# INDEPENDENT VARIABLES ---------------------------------------------------

# glm mod
mod1 <- logitMod(dat2, c("day_of_week", "hour_fact", "to.avg_stop_times", 
                         "nr_of_stops", "vehicle_type", "to_bruss"), seed = 456)

# decision tree moded
dtMod1 <- dtMod(dat2, c("day_of_week", "hour_fact", "to.avg_stop_times", 
                        "nr_of_stops", "vehicle_type", "to_bruss"), seed = 456)


# PREDICTING WITH FIXED EFFECTS -------------------------------------------

ref0coef <- coef(summary(glm1)) %>% data.frame()
rownames(ref0coef)[1] <- "line0"

ref0coef$name <- substr(rownames(ref0coef), 5, nchar(rownames(ref0coef)))

head(line_fe)
dat5 <- left_join(dat4, ref0coef, by = "name")
dat5$hour_fact <- dat5$hour %>% as.factor()

# with line fixed effect p-values
# glm mod
lfe_vars <- c("day_of_week", "hour", "to.avg_stop_times", "nr_of_stops", "vehicle_type", "to_bruss", "line.x", "to.prov")
mod2 <- logitMod(dat5, lfe_vars, seed = 456)

#dtmod2 <- dtMod(dat5, lfe_vars, seed = 456)

# with province fixed effect p-values
dat6 <- left_join(dat5, line_to.prov, by = "to.prov")

pfe_vars <- c(lfe_vars, "to.P.value")
mod3 <- logitMod(dat6, pfe_vars, seed = 456)
dtmod4 <- dtMod(dat6, pfe_vars, seed = 456)


# VARIABLE INTERACTIONS ---------------------------------------------------

glminteract <- glm(occ_binary ~ line * from.prov, dat4, family = "binomial")

summary(glminteract)
