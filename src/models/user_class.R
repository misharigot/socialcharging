# User classification model
library(readr)
library(ggplot2)
library(config)
library(purrr)
library(tidyr)
config <- config::get(file = "config.yml")
source(config$baseClean)
set.seed(100)

# Constants
minUserSessions <- 10
# 
# df <- read_csv2(config$scDataset, col_names = FALSE)
# df <- cleanSecondDf(df)

# Classification --------------------------------------------------------------------------------------------------

# Filter and select only relevant rows/columns
cleanDf <- function(df) {
  # Users ids that have sessions >= minUserSessions
  usersWithEnoughSessions <- df %>%
    group_by(user_id) %>%
    summarise(count = n()) %>%
    filter(count >= minUserSessions) %>%
    select(user_id)

  df %>%
  filter(!is.na(hours_elapsed),
         hours_elapsed > 0.00,
         !is.na(start_date),
         !is.na(end_date),
         user_id %in% usersWithEnoughSessions$user_id) %>%
  select(session_id, user_id, start_date, end_date, charged_kwh, hours_elapsed)
}

# Returns the bucket a POSIXct datetime belongs to
classifyTf <- function(datetime) {
  # datetime <- hour(datetime)
  if (datetime >= 12 & datetime < 18) {
    return("12-18")
  } else if (datetime >= 18 & datetime < 24) {
    return("18-00")
  } else if (datetime >= 0 & datetime < 6) {
    return("00-06")
  } else if (datetime >= 6 & datetime < 12) {
    return("06-12")
  }
}

# Boolean checks

isMorning <- function(x) {
  return(x == "06-12")
}

isAfternoon <- function(x) {
  return(x == "12-18")
}

isEvening <- function(x) {
  return(x == "18-00")
}

isNight <- function(x) {
  return(x == "00-06")
}

## Returns the classification a session belongs to
# stf: start_tf
# etf: end_tf
# hrs: hours_elapsed
getSessionClass <- function(stf, etf, hrs) {
  # stf: Morning
  if (isMorning(stf) & isMorning(etf) & hrs < 24) {
    return(1)
  } else if (isMorning(stf) & isAfternoon(etf) & hrs < 24) {
    return(2)
  } else if (isMorning(stf) & isEvening(etf) & hrs < 24) {
    return(3)
  } else if (isMorning(stf) & isNight(etf) & hrs < 24) {
    return(4)
  }

  # stf: Afternoon
  if (isAfternoon(stf) & isMorning(etf) & hrs < 24) {
    return(5)
  } else if (isAfternoon(stf) & isAfternoon(etf) & hrs < 24) {
    return(6)
  } else if (isAfternoon(stf) & isEvening(etf) & hrs < 24) {
    return(7)
  } else if (isAfternoon(stf) & isNight(etf) & hrs < 24) {
    return(8)
  }

  # stf: Evening
  if (isEvening(stf) & isMorning(etf) & hrs < 24) {
    return(9)
  } else if (isEvening(stf) & isAfternoon(etf) & hrs < 24) {
    return(10)
  } else if (isEvening(stf) & isEvening(etf) & hrs < 24) {
    return(11)
  } else if (isEvening(stf) & isNight(etf) & hrs < 24) {
    return(12)
  }

  # stf: Night
  if (isNight(stf) & isMorning(etf) & hrs < 24) {
    return(13)
  } else if (isNight(stf) & isAfternoon(etf) & hrs < 24) {
    return(14)
  } else if (isNight(stf) & isEvening(etf) & hrs < 24) {
    return(15)
  } else if (isNight(stf) & isNight(etf) & hrs < 24) {
    return(16)
  }
  return(-1) # Greater than 24 hours elapsed
}

# Classify sessions on timeframes
sessionClassificationDf <- function(cleanDf) {
  sessionClassifications <- cleanDf %>%
    mutate(start_date_hour = hour(start_date), end_date_hour = hour(end_date)) %>%
    mutate(start_tf = map(start_date_hour, classifyTf), end_tf = map(end_date_hour, classifyTf)) %>%
    mutate(start_tf = as.factor(unlist(start_tf)), end_tf = as.factor(unlist(end_tf))) %>%
    rowwise() %>%
    mutate(class = getSessionClass(start_tf, end_tf, hours_elapsed))
  sessionClassifications$class <- as.factor(sessionClassifications$class)
  return(sessionClassifications)
}

# Classify users by their most dominant timeframe
userClassificationDf <- function(sessionClassification) {
  userClassifications <- sessionClassification %>%
    count(user_id, class) %>%
    group_by(user_id) %>%
    slice(which.max(n))
}

# Summarise count for each timeframe

classDistributionDf <- function(sessionClassificationDf) {
  classDistribution <- sessionClassificationDf %>%
    count(user_id, class) %>%
    group_by(user_id) %>%
    slice(which.max(n)) %>%
    group_by(class) %>%
    summarise(n = n())
}

# Barchart: count for each timeframe
plotClassCount <- function(sessionClassificationDf) {
  ggplot(classDistributionDf(sessionClassificationDf),
         aes(x = class, y = n)) +
    geom_bar(stat = "identity") +
    geom_smooth() +
    labs(x = "class", y = "amount") +
    ggtitle("Amount total most dominant classes") +
    coord_flip() +
    theme_light()
}

plotClassCountShiny <- function(scData) {
  plotClassCount(sessionClassificationDf(cleanDf(scData)))
}

# Clustering for exploration purposes -----------------------------------------------------------------------------

# Returns a scree plot of the data (aim for centers under a 0.2 ratio)
screePlot <- function(data) {
  ratio_ss <- rep(0, 10)
  for (k in 1:10) {
    km_model <- kmeans(data, centers = k, nstart = 20)
    ratio_ss[k] <- km_model$tot.withinss / km_model$totss
  }
  plot(ratio_ss, type = "b", xlab = "k")
}

as.numeric.factor <- function(x) {
  if (x == "00-06") return(1)
  if (x == "06-12") return(2)
  if (x == "12-18") return(3)
  if (x == "18-00") return(4)
}

doClustering <- function() {
  cluster_tf <- sessionClassifications %>% select(start_tf, end_tf, hours_elapsed) %>% filter(hours_elapsed < 30)
  ctf <- sessionClassifications %>% filter(hours_elapsed < 30) %>% mutate(hr = hour(start_date))
  cluster_tf$start_tf <- map(cluster_tf$start_tf, as.numeric.factor)
  cluster_tf$end_tf <- map(cluster_tf$end_tf, as.numeric.factor)

  screePlot(cluster_tf)
  sc_km <- kmeans(cluster_tf, centers = 3, nstart = 20)

  plot(x = ctf$hr, y = ctf$hours_elapsed, col = sc_km$cluster,
       xlab = "start timeframe (hour)", ylab = "hours elapsed in session")
}
