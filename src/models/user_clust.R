library(ggplot2)
library(config)
library(dplyr)
library(plotly)
library(clValid)
config <- config::get(file = "config.yml")
source(config$baseClean)
set.seed(100)

df <- read_csv2(config$scDataset, col_names = FALSE)
df <- cleanSecondDf(df)

# Data cleaning -----------------------------------------------------------------------------------------------
cleanDataChargedKwh <- function() {
  df <- df %>%
    filter(!is.na(hours_elapsed), !is.na(charged_kwh)) %>%
    group_by(user_id) %>%
    summarise(total_charge_sessions = n(),
              total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed))
  
  return(df)
}

cleanDataTime <- function(){
  df <- df %>%
    filter(!is.na(start_date), !is.na(end_date), !is.na(hours_elapsed)) %>%
    group_by(user_id) %>%
    summarise(total_charge_sessions = n(),
              avg_hours_elapsed = sum(hours_elapsed)/total_charge_sessions,
              start_time = mean(as.numeric(format(round(start_date, "hours"), format='%H'))),
              end_time = mean(as.numeric(format(round(end_date, "hours"), format='%H'))))
  
  return(df)
}

cleanedDfCharged <- cleanDataChargedKwh()
cleanedDfTime <- cleanDataTime()

# select columns needed for clustering ------------------------------------------------------------------------
clusteredDfCharged <- cleanedDfCharged %>%
  select(total_charge_sessions, total_charged, total_hours_elapsed)

rownames(clusteredDfCharged) <- cleanedDfCharged$user_id

clusteredDfTime <- cleanedDfTime %>%
  select(avg_hours_elapsed, start_time, end_time)

rownames(clusteredDfTime) <- clusteredDfTime$user_id

# Clustering --------------------------------------------------------------------------------------------------
clusteredCharged_km <- kmeans(clusteredDfCharged, 5, nstart = 20)

clusteredTime_km <- kmeans(clusteredDfTime, 5, nstart = 20)

# Dunn's Index ------------------------------------------------------------------------------------------------
dunn_kmCharged <- dunn(clusters = clusteredCharged_km$cluster, Data = clusteredDfCharged)

dunn_kmTime <- dunn(clusters = clustered_kmTime$cluster, Data = clusteredDfTime)

# Screeplot ---------------------------------------------------------------------------------------------------
screePlotCharged <- function() {
  
  ratio_ss <- rep(0, 10)
  
  for (k in 1:10) {
    user_charging_km <- kmeans(clusteredDfCharged, k, nstart = 20)
    ratio_ss[k] <- user_charging_km$tot.withinss / user_charging_km$totss
  }
  
  plot(ratio_ss, type = "b", xlab = "k")
  
}

screePlotTime <- function() {
  
  ratio_ss <- rep(0, 10)
  
  for (k in 1:10) {
    user_time_km <- kmeans(clusteredDfTime, k, nstart = 20)
    ratio_ss[k] <- user_time_km$tot.withinss / user_time_km$totss
  }
  
  plot(ratio_ss, type = "b", xlab = "k")
  
}

# Plots -------------------------------------------------------------------------------------------------------
plotClusterDataCharged <- function() {
  p <- plot_ly(clusteredDfCharged, x = ~total_hours_elapsed, y = ~total_charged,
               z = ~total_charge_sessions, color = clusteredCharged_km$cluster, showscale = TRUE,
               hoverinfo = 'text',
               text = ~paste('</br> Hours elapsed: ', total_hours_elapsed,
                             '</br> Charged kWh: ', total_charged,
                             '</br> Sessions: ', total_charge_sessions,
                             '</br> User id: ', rownames(clusteredDfCharged))) %>%
    hide_colorbar() %>%
    layout(scene = list(xaxis = list(title = 'total hours elapsed'),
                        yaxis = list(title = 'total charged kwh'),
                        zaxis = list(title = 'total sessions')))
  return(p)
}

plotClusterDataTime <- function() {
  p <- plot_ly(clusteredDfTime, x = ~start_time, y = ~end_time,
               z = ~avg_hours_elapsed, color = clusteredTime_km$cluster, showscale = TRUE,
               hoverinfo = 'text',
               text = ~paste('</br> User id: ', rownames(clusteredDfTime))) %>%
    hide_colorbar() %>%
    layout(scene = list(xaxis = list(title = 'Avg start time'),
                        yaxis = list(title = 'Avg end time'),
                        zaxis = list(title = 'Avg hours elapsed')))
  return(p)
}

#Calls -------------------------------------------------------------------------------------------------------- 
plotClusterDataCharged()
plotClusterDataTime()
screePlotCharged()
screePlotTime()
