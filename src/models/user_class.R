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

# sc_km <- kmeans(sc_data, centers = 5, nstart = 20)

# plot(y = sc_data$charged_kwh, x = sc_data$hours_elapsed, col = sc_km$cluster)

# Classification --------------------------------------------------------------------------------------------------

# Select a user's data
user_data <- df %>%
  filter(!is.na(hours_elapsed), hours_elapsed > 0.00, !is.na(start_date), !is.na(end_datesumm)) %>%
  select(session_id, user_id, start_date, end_date, charged_kwh, hours_elapsed)

classify_tf <- function(time) {
  time <- hour(time)
  if (time > 12 & time <= 16) {
    return("12-16")
  } else if (time > 16 & time <= 20) {
    return("16-20")
  } else if (time > 20 & time <= 0) {
    return("20-00")
  } else if (time > 0 & time <= 4) {
    return("00-04")
  } else if (time > 4 & time <= 8) {
    return("04-08")
  } else if (time > 8 & time <= 12) {
    return("08-12")
  }
}

# Assign timeframes columns to sessions
# TODO FIX this (only works with single user, not with entire data set)
tf_data <- user_data %>%
  mutate(start_tf = map(start_date, classify_tf), end_tf = map(end_date, classify_tf)) %>%
  mutate(start_tf = as.factor(unlist(start_tf)), end_tf = as.factor(unlist(end_tf)))

# Count each timeframe
tf_summ <- tf_data %>% select(-session_id, -start_date, -end_date, -charged_kwh, -hours_elapsed) %>%
  gather(key, value, -user_id) %>%
  group_by(user_id, key, value) %>%
  tally %>%
  spread(value, n, fill = 0)

# Get the dominant timeframe for startdate and enddate
getDominantClass <- function(tf_summ_row) {
  type <- tf_summ_row[, 2]
  timeframes <- tf_summ_row[, 3:length(tf_summ_row)]
  dom_val <- 0;
  count <- 0
  for (tf in timeframes) {
    count <- count + 1
    if (tf > dom_val) {
      dom_val = tf
    }
  }
  return(c(type = type, dominator = colnames(tf_summ_row)[count]))
}

getUserClassification <- function()

user_classifier <- function(x) {
  prediction <- rep(NA, length(x))
  prediction[x$start_tf == as.factor("")]
}
