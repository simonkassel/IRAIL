# IRAIL PROJECT HELPER FUNCTIONS ------------------------------------------

# Simon Kassel


# FUNCTIONS ---------------------------------------------------------------

# checks for availability of packages, if not there it installs them and 
# then it attaches all packages
#   takes:
#     a vector of package names in quotation marks
#   returns:
#     nothing
#   side effects:
#     attaches packages
packages <- function(package_vector) {
  for (lib in package_vector) {
    if (!requireNamespace(lib, quietly = TRUE))
      install.packages(lib)
    suppressMessages(library(lib, character.only = TRUE))
    remove(lib)
  } 
}


# ensures that all levels of categorical variables present in test
# sets are also in training sets
#   takes:
#     a data frame training set (train)
#     a data frame test set (tes)
#   returns:
#     the test set
#   side effects:
#     attaches packages
catVars <- function(train, test) {
  cat_vars <- sapply(test, class)[sapply(test, class) %in% c("character", "factor")] %>% names()
  beginRows <- nrow(test) 
  for (v in cat_vars) {
    for (i in unique(test[ , v])) {
      if (!i %in% unique(train[ , v])) {
        test <- test[-which(test[, v] == i), ]
      }
    }
  }
  rowsRemoved <- beginRows - nrow(test)
  paste0("Removed [", rowsRemoved, "] rows from test set.") %>% print()
  return(test)
}


# run a logisitc regression on a dataset with 
#   takes:
#     a vector of iv field names (ivars)
#     a data frame (dat)
#     an integer seed value (seed)
#   returns:
#     a model
#   side effects:
#     prints confusion matrix
logitMod <- function(dat, ivars, seed, threshold = NULL) {
  
  packages(c("pROC", "caret"))
  
  dat <- dat[ , c("occ_binary", ivars)]

  set.seed(seed)
  inTrain <- createDataPartition(dat$occ_binary, p = .75, list = FALSE) 
  training <- dat[inTrain, ]
  testing <- dat[-inTrain, ]
  
  glm_mod <- glm(occ_binary ~ ., data = training)

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
  
  return(glm_mod)
}


# run a decision tree model on dataset with occ_binary, dv 
#   takes:
#     a vector of iv field names (ivars)
#     a data frame (dat)
#     an integer seed value (seed)
#   returns:
#     a model
#   side effects:
#     prints confusion matrix
dtMod <- function(dat, ivars, seed = 123) {
  
  packages(c("pROC", "caret", "C50"))
  
  dat <- dat[ , c("occ_binary", ivars)]
  
  set.seed(seed)
  inTrain <- createDataPartition(dat$occ_binary, p = .75, list = FALSE) 
  training <- dat[inTrain, ]
  testing <- dat[-inTrain, ]
  
  ts <- training[ , ivars]
  
  if(! is.data.frame(ts)) {
    ts <- as.data.frame(ts)
    names(ts) <- ivars
  }
  
  dt_mod <- C5.0(ts, training$occ_binary %>% as.factor())
  
  testing <- catVars(training, testing)
  
  dt_pred <- predict(dt_mod, testing)
  print(confusionMatrix(dt_pred, testing$occ_binary))
  
  return(dt_mod)
}

# generate a station code from URI field
#   takes:
#     a URI string
#   returns:
#     a station ID
substrFunc <- function(i){
  return(rev(regexpr("\\/[^\\/]*$", i)) + 1)
}

# Clean column names
#   takes:
#     a prefix and appends it to eveery column name in the stations dataset
#   returns:
#     the data frame with adjusted column names
fromToRename <- function(prefix){
  stations$temp <- stations$station
  colnames(stations) <- paste(prefix, names(stations), sep = ".")
  colnames(stations)[ncol(stations)] <- prefix 
  return(stations)
}

#----- Join in one dataset -----
#   takes: 
#     a data frame of trips and 
#     a prefix to add to each station variable
#   returns:
#     a trip dataset with stations to/from prefix to station variable
joinToTrips <- function(tripDat, prefix){
  dat <- join(tripDat, fromToRename(prefix), by = prefix, type = "inner", match = "all")
  return(dat)
}

#----- Find the hubs within clusters of stations -----
#   takes: 
#     a data frame with latitude, longitude and count variables (df)
#     a number of clusters to break the data up into (kvar)
#   returns:
#     a dataset of stations with their associated clusters and the max
#     count hub within that cluster
findHubs <- function(df, kvar) {
  
  d <- dist(df[,c("longitude", "latitude")], method = "euclidean")
  fit <- hclust(d, method="ward.D") 
  
  groups <- cutree(fit, k = kvar) 
  df$groups <- groups %>% unname() %>% as.factor()
  maxtab <- ddply(df, ~groups, summarise, maxcount = max(count))
  
  hubs <- filter(df, count %in% maxtab$maxcount)
  df$k <- paste0("k = ", kvar)
  df <- left_join(df, maxtab)
  return(df)
}


