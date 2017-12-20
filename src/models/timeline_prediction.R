library(MASS)
library(purrr)
library(tidyr)
library(readr)
library(dplyr)

config <- config::get(file = "config.yml")
source(config$baseClean)

minUserSessions <- 5

# Data cleaning -----------------------------------------------------------
cleanDf <- function(df) {
  usersWithEnoughSessions <- df %>%
    group_by(user_id) %>%
    summarise(count = n()) %>%
    filter(count >= minUserSessions) %>%
    dplyr::select(user_id)
  
  df$day <-
    as.numeric(strftime(as.Date(df$start_date), format = "%u"))
  
  df <- df %>%
    filter(
      !is.na(hours_elapsed),
      charged_kwh > 0.00,
      hours_elapsed > 0.01, hours_elapsed < 48,
      !is.na(start_date),!is.na(end_date),
      user_id %in% usersWithEnoughSessions$user_id
    ) %>%
    filter(!is.na(start_date),
           !is.na(hours_elapsed),
           !is.na(charged_kwh)) %>%
    mutate(
      start_date_hour = round(hour(start_date))) %>%
    dplyr::select(session_id,
           user_id,
           day,
           start_date,
           start_date_hour,
           hours_elapsed,
           charged_kwh
           )
  
  return(df)
}

# Functions ---------------------------------------------------------------
logChargedKwh <- function(kwh) {
  return(log(kwh))
}

# Clustering --------------------------------------------------------------
setClusters <- function(df) {
  df <- cleanDf(df)
  
  df_start_sessions <- df %>%
    dplyr::select(session_id, user_id, day, start_date_hour)
  
  df_hours_elapsed <- df %>%
    dplyr::select(session_id, user_id, day, hours_elapsed)
  
  df_charged_kwh <- df %>%
    mutate(log_charged_kwh = as.numeric(map(charged_kwh, logChargedKwh))) %>%
    dplyr::select(session_id, user_id, day, log_charged_kwh)
  
  d1 <- dist(df_start_sessions[, 4])
  hc_start_sessions <- hclust(d1)
  
  d2 <- dist(df_hours_elapsed[, 4], method = "euclidean")
  hc_hours_elapsed <- hclust(d2)
  
  d3 <- dist(df_charged_kwh[, 4])
  hc_kwh <- hclust(d3)
  
  df <- as.data.frame(
    cbind(
      start_hour_cluster=cutree(hc_start_sessions, k=8),
      hours_elapsed_cluster=cutree(hc_hours_elapsed, k=8),
      charged_kwh_cluster=cutree(hc_kwh, k=8),
      sessionId=df$session_id,
      userId=df$user_id,
      day=df$day,
      start_hour=df_start_sessions$start_date_hour,
      hours_elapsed=df_hours_elapsed$hours_elapsed,
      charged_kwh=df_charged_kwh$log_charged_kwh
        )
      )
  
  return(df)
}

# Test envoirement --------------------------------------------------------
df <- read_csv2(config$scDataset, col_names = FALSE)
df <- cleanDataframe(df)
clusters <- setClusters(df)

fit_start_hour <- qda(start_hour_cluster ~
                        start_hour + hours_elapsed +
                        charged_kwh,
                      data = clusters,
                      CV=TRUE)
fit_hours_elapsed <- qda(hours_elapsed_cluster ~
                           start_hour + hours_elapsed +
                           charged_kwh,
                         data = clusters,
                         CV=TRUE)
fit_charged_kwh <- qda(charged_kwh_cluster ~
                         start_hour + hours_elapsed +
                         charged_kwh,
                       data = clusters,
                       CV=TRUE)

ct1 <- table(clusters$start_hour_cluster, fit_start_hour$class)
ct2 <- table(clusters$hours_elapsed_cluster, fit_hours_elapsed$class)
ct3 <- table(clusters$charged_kwh_cluster, fit_charged_kwh$class)

diag(prop.table(ct1, 1))
diag(prop.table(ct2, 1))
diag(prop.table(ct3, 1))

hist(clusters$start_hour_cluster)
hist(clusters$hours_elapsed_cluster)
hist(clusters$charged_kwh_cluster)

# TODO
# - Clusters
# - Fix kwh clusters






