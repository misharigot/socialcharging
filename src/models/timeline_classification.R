# Classification

library(purrr)
library(tidyr)
library(readr)
library(dplyr)

config <- config::get(file = "config.yml")
source(config$baseClean)

minUserSessions <- 5

cleanDf <- function(df) {
  # Users id's that have sessions >= minUserSessions
  usersWithEnoughSessions <- df %>%
    group_by(user_id) %>%
    summarise(count = n()) %>%
    filter(count >= minUserSessions) %>%
    select(user_id)
  
  df$day <-
    as.numeric(strftime(as.Date(df$start_date), format = "%u"))
  
  df <- df %>%
    filter(
      !is.na(hours_elapsed),
      hours_elapsed > 0.01, hours_elapsed < 48,
      !is.na(start_date),!is.na(end_date),
      user_id %in% usersWithEnoughSessions$user_id
    ) %>%
    filter(!is.na(start_date),
           !is.na(hours_elapsed),
           !is.na(charged_kwh)) %>%
    mutate(
      start_date_hour = round(hour(start_date))) %>%
    select(session_id,
           user_id,
           day,
           start_date,
           start_date_hour,
           hours_elapsed,
           charged_kwh)
  
  return(df)
}

getClustersOfUsers <- function(clusterDf) {
  
  tfCluster <- clusterDf %>%
    count(userId, start_hour_cluster) %>%
    group_by(userId) %>%
    slice(which.max(n))
  
  heCluster <- clusterDf %>%
    count(userId, hours_elapsed_cluster) %>%
    group_by(userId) %>%
    slice(which.max(n))
  
  kwhCluster <- clusterDf %>%
    count(userId, charged_kwh_cluster) %>%
    group_by(userId) %>%
    slice(which.max(n))
  
  mergedDf <-
    base::merge(tfCluster, heCluster, by = "userId")
  mergedDf <-
    base::merge(mergedDf, kwhCluster, by = "userId")
  mergedDf$n.x <- NULL
  mergedDf$n.y <- NULL
  mergedDf$n <- NULL
  
  return(mergedDf)
}

setClusters <- function(df) {
  df <- cleanDf(df)
  
  df_start_sessions <- df %>%
    select(session_id, user_id, day, start_date_hour)
  
  df_hours_elapsed <- df %>%
    select(session_id, user_id, day, hours_elapsed)
  
  df_charged_kwh <- df %>%
    select(session_id, user_id, day, charged_kwh)
  
  d1 <- dist(df_start_sessions[, 4]) # '?dist' for details
  hc_start_sessions <- hclust(d1)
  
  d2 <- dist(df_hours_elapsed[, 4], method = "euclidean") # '?dist' for details
  hc_hours_elapsed <- hclust(d2)
  
  d3 <- dist(df_charged_kwh[, 4]) # '?dist' for details
  hc_kwh <- hclust(d3)
  
  df <- as.data.frame(
    cbind(
      start_hour_cluster=cutree(hc_start_sessions, k=8),
      hours_elapsed_cluster=cutree(hc_hours_elapsed, k=8),
      charged_kwh_cluster=cutree(hc_kwh, k=8),
      sessionId=df_start_sessions$session_id,
      userId=df_start_sessions$user_id,
      day=df_start_sessions$day,
      start_hour=df_start_sessions$start_date_hour

        )
      )
  
  return(df)
}

df <- read_csv2(config$scDataset, col_names = FALSE)
df <- cleanDataframe(df)
clusters <- setClusters(df)
userClusters <- getClustersOfUsers(clusters)
