# User classification model
library(readr)
library(ggplot2)
library(config)
library(purrr)
library(tidyr)
library(clValid)
library(plotly)

config <- config::get(file = "config.yml")
source(config$baseClean)
set.seed(100)

# Constants
df <- read_csv2(config$scDataset, col_names = FALSE)
df <- cleanSecondDf(df)

df <- df %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(charged_kwh), !is.na(hours_elapsed))

split_str_by_index <- function(target, index) {
  index <- sort(index)
  substr(rep(target, length(index) + 1),
         start = c(1, index),
         stop = c(index - 1, nchar(target)))
}

interleave <- function(v1, v2) {
  ord1 <- 2 * (1:length(v1)) - 1
  ord2 <- 2 * (1:length(v2))
  c(v1, v2)[order(c(ord1, ord2))]
}

df$latitude <- sapply(df$latitude, function(x){
  insert <- "."[order(3)]
  index <- sort(3)
  paste(interleave(split_str_by_index(x, 3), "."), collapse = "")
})

df$longitude <- sapply(df$longitude, function(x){
  insert <- "."[order(2)]
  index <- sort(2)
  paste(interleave(split_str_by_index(x, 2), "."), collapse = "")
})

df$latitude <- as.numeric(df$latitude)
df$longitude <- as.numeric(df$longitude)

totalHours <- interval(min(df$start_date), max(df$end_date)) / 3600

# Advanced data cleaning ----------------------------------------------------------
cleanMapData <- function() {
  df <- df %>%
    group_by(longitude, latitude) %>%
    summarise(address = first(address),
              outlets = first(outlets),
              total_sessions = n(),
              total_users = n_distinct(user_id),
              total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed))
  
  df$total_sessions <- as.numeric(df$total_sessions)
  df$total_charged <- as.numeric(df$total_charged)
  
  return(df)
}

cleanedDf <- cleanMapData()

clusterDf <- cleanedDf %>%
  select(total_charged, total_hours_elapsed, total_sessions)

# Clustering --------------------------------------------------------------------------------------------------
charging_km <- kmeans(clusterDf, 5, nstart = 20)

# Performance measures ----------------------------------------------------------------------------------------
# Dunn's Index
dunn_km <- dunn(clusters = charging_km$cluster, Data = clusterDf)
dunn_km

# Scree Plot
ratio_ss <- rep(0, 15)

for (k in 1:10) {
  charging_km_test <- kmeans(clusterDf, k, nstart = 20)
  
  ratio_ss[k] <- charging_km_test$tot.withinss / charging_km_test$totss
}

plot(ratio_ss, type = "b", xlab = "k")

# Plotering ---------------------------------------------------------------------------------------------------
plot(total_charged ~ total_hours_elapsed, data = clusterDf, col = charging_km$cluster)

p <- plot_ly(clusterDf, x = ~total_hours_elapsed, y = ~total_charged,
             z = ~total_sessions, color = charging_km$cluster, showscale = TRUE,
             hoverinfo = 'text',
             text = ~paste('</br> Hours elapsed: ', total_hours_elapsed,
                           '</br> Charged kWh: ', total_charged,
                           '</br> Sessions: ', total_sessions)) %>%
  hide_colorbar() %>%
  layout(scene = list(xaxis = list(title = 'total hours elapsed'),
                      yaxis = list(title = 'total charged kwh'),
                      zaxis = list(title = 'total sessions')))
p
