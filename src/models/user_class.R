# User classification model
library(readr)
library(ggplot2)
library(config)
library(purrr)
library(tidyr)
config <- config::get(file = "config.yml")
source(config$baseClean)
set.seed(100)

df <- read_csv2(config$scDataset)
df <- cleanSecondDf(df)

avg_per_user <- df %>%
  # select(-car, -ev_provider, -corporate, -smart_charging, -evse_id, -address) %>%
  # select(charged_kwh, hours_elapsed) %>%
  na.omit(df) %>%
  group_by(user_id) %>%
  summarise(avg_kwh = mean(charged_kwh), avg_hrs = mean(hours_elapsed)) %>%
  select(2:3)

# Classification --------------------------------------------------------------------------------------------------

# Select a user's data
user_data <- df %>%
  filter(!is.na(hours_elapsed), hours_elapsed > 0.00, !is.na(start_date), !is.na(end_date)) %>%
  select(session_id, user_id, start_date, end_date, charged_kwh, hours_elapsed)

classify_tf <- function(time) {
  time <- hour(time)
  if (time > 12 & time <= 16) {
    return("12-16")
  } else if (time > 16 & time <= 20) {
    return("16-20")
  } else if (time == 0 | time > 20 & time <= 23) {
    return("20-00")
  } else if (time > 0 & time <= 4) {
    return("00-04")
  } else if (time > 4 & time <= 8) {
    return("04-08")
  } else if (time > 8 & time <= 12) {
    return("08-12")
  }
}

isAfternoon <- function(tf) {
  return(tf == "12-16" | tf == "16")
}

getSessionClass <- function(stf, etf, hrs) {
  if (stf == "16-20" & (etf == "08-12" | etf == "04-08") & hrs <= 20) {
    c <- "Overnight"
  } else if (stf == "20-00" & (etf == "08-12" | etf == "04-08") & hrs <= 16) {
    c <- "Late Overnight"
  } else if ((stf == "04-08" | stf == "08-12") & (etf == "12-16" | etf == "16-20" | etf == "20-00") & hrs <= 16) {
    c <- "Workday"
  } else if (stf == "04-08" & etf == "08-12" & hrs <= 8) {
    c <- "Morning"
  } else if (hrs >= 24) {
    c <- "Long"
  } else if (stf == etf | hrs <= 4) {
    c <- "Short"
  } else if (stf == "12-16" & etf == "16-20" & hrs <= 8) {
    c <- "Afternoon to Dinner"
  } else if ((stf == "12-16" | stf == "16-20") & etf == "20-00" & hrs <= 8) {
    c <- "Afternoon to Evening"
  } else if ((stf == "12-16" | stf == "16-20") & (etf == "04-08" | etf == "08-12") & hrs <= 8) {
    c <- "Afternoon to Morning"
  } else if ((stf == "20-00" | stf == "00-04") & (etf == "16-20" | etf == "20-00") & hrs < 24 & hrs > 4) {
    c <- "Night to Evening"
  } else if ((stf == "20-00" | stf == "00-04") & (etf == "04-08" | etf == "08-12") & hrs < 24 & hrs > 4) {
    c <- "Night to Morning"
  } else if (stf == "00-04" & etf == "12-16" & hrs < 24 & hrs > 4) {
    c <- "Night to Afternoon"
  } else {
    c <- "Unknown"
  }
  return(c)
}

# Assign timeframes columns to sessions
# TODO FIX this (only works with single user, not with entire data set)
tf_data <- user_data %>%
  mutate(start_tf = map(start_date, classify_tf), end_tf = map(end_date, classify_tf)) %>%
  mutate(start_tf = as.factor(unlist(start_tf)), end_tf = as.factor(unlist(end_tf))) %>%
  rowwise() %>%
  mutate(class = getSessionClass(start_tf, end_tf, hours_elapsed))

tf_data$class <- as.factor(tf_data$class)
summary(tf_data)

summary_tf <- tf_data %>%
  count(user_id, class) %>%
  group_by(user_id) %>%
  slice(which.max(n)) %>%
  group_by(class) %>%
  summarise(n = n())

plotBarTfSumm <- function() {
  p <- ggplot(summary_tf, aes(x = class, y = n)) +
    geom_bar(stat = "identity") +
    geom_smooth() +
    labs(x = "class", y = "amount") +
    ggtitle("Amount total most dominant classes") +
    coord_flip() +
    theme_light()
  return(p)
}

plotBarTfSumm()

# Count each timeframe
tf_summ <- tf_data %>% select(-session_id, -start_date, -end_date, -charged_kwh, -hours_elapsed) %>%
  gather(key, value, -user_id) %>%
  group_by(user_id, key, value) %>%
  tally %>%
  spread(value, n, fill = 0)

# Clustering ------------------------------------------------------------------------------------------------------

# Returns a scree plot of the data (aim for centers under a 0.2 ratio)
scree_plot <- function(data) {
  ratio_ss <- rep(0, 7)
  for (k in 1:10) {
    km_model <- kmeans(data, centers = k, nstart = 20)
    ratio_ss[k] <- km_model$tot.withinss / km_model$totss
  }
  plot(ratio_ss, type = "b", xlab = "k")
}

as.numeric.factor <- function(x) {
  if (x == "00-04") return(1)
  if (x == "04-08") return(2)
  if (x == "08-12") return(3)
  if (x == "12-16") return(4)
  if (x == "16-20") return(5)
  if (x == "20-00") return(6)
}
cluster_tf <- tf_data %>% select(start_tf, end_tf, hours_elapsed) %>% filter(hours_elapsed < 30)
cluster_tf$start_tf <- map(cluster_tf$start_tf, as.numeric.factor)
cluster_tf$end_tf <- map(cluster_tf$end_tf, as.numeric.factor)

scree_plot(cluster_tf)

# sc_km <- kmeans(tf_data, centers = 5, nstart = 20)

# plot(y = sc_data$charged_kwh, x = sc_data$hours_elapsed, col = sc_km$cluster)
