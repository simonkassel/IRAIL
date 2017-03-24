# INTRO -------------------------------------------------------------------
# Query IRAIL API for route info
#   Simon Kassel
#   Created: 22 Mar 17

# working directory
setwd("~/Dropbox/IRAIL/data")

# load helper functions
source("https://raw.githubusercontent.com/simonkassel/IRAIL/master/scripts/IRAIL_helper_functions_032317.R")

# load packages
packages(c("plyr", "dplyr", "RCurl", "XML"))

# global options
options(stringsAsFactors = FALSE)
options(scipen = "999")


# FIND ROUTE INFO FROM IRAIL API ------------------------------------------
# dat$date_api <- paste(
#   substr(dat$date, 6, 7),
#   substr(dat$date, 9, 10),
#   substr(dat$date, 3, 4),
#   sep = ""
# )

# grabData <- function(x) {
#   return(c(dat$date_api[x], dat$vehicle[x]))
# }

grabData <- function(x) {
  return(dat$vehicle[x])
}

queries <- llply(as.list(1:nrow(dat)), grabData) %>% unique()

theList <- list()
iter <- 1

for (i in queries) {
  #vehicleNo <- i[2]
  #date <- i[1]
  vehicleNo <- i[1]
  
  # URL <- paste("https://api.irail.be/vehicle/?id=BE.NMBS.", vehicleNo, "&date=", date, sep = "")
  URL <- paste("https://api.irail.be/vehicle/?id=BE.NMBS.", vehicleNo, sep = "")
  
  xml_dat <- getURL(URL)
  
  if (nchar(xml_dat) == 0) {
    print(paste("Row number", iter, "has no result", sep = " "))
    theList[[iter]] <- NA
    break()
  }
  
  # if (nchar(xml_dat) == 0) {
  #   
  #   print("date didn't work, trying without")
  #   URL <- paste("https://api.irail.be/vehicle/?id=BE.NMBS.", vehicleNo, sep = "")
  #   xml_dat <- getURL(URL)
  #   
  #   if (nchar(xml_dat) == 0) {
  #     print(paste("Row number", iter, "has no result", sep = " "))
  #     theList[[iter]] <- NA
  #     break()
  #   }
  # }
    
  xlist <- xmlToList(xml_dat)
    
  stops <- xlist$stops
  station_vector <- c()
    
  for (ii in stops) {
    if (class(ii) == "list") {
      y <- ii$station$text
      station_vector <- c(station_vector, y)
    } else {
      print(ii)
    }
  }
  
  theList[[iter]] <- station_vector
  
  print(paste("completed ", "[", iter, "] of ", length(queries), " queries.", sep = ""))
  iter = iter + 1
}

names <- lapply(queries, function(x) {
  return(x[[1]])
}) %>% unlist()
alist <- setNames(theList, names) 

