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
cleanDataChargedKwh <- function(data) {
  myDf <- data %>%
    filter(!is.na(hours_elapsed), !is.na(charged_kwh)) %>%
    group_by(user_id) %>%
    summarise(total_charge_sessions = n(),
              total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed)) %>%
    select(total_charge_sessions, total_charged, total_hours_elapsed)
  
  rownames(myDf) <- myDf$user_id
  return(myDf)
}

# Todo: change logic
cleanDataTime <- function(data) {
  myDf <- data %>%
    filter(!is.na(start_date), !is.na(end_date), !is.na(hours_elapsed)) %>%
    group_by(user_id) %>%
    summarise(total_charge_sessions = n(),
              avg_hours_elapsed = sum(hours_elapsed) / total_charge_sessions,
              start_time = mean(as.numeric(format(round(start_date, "hours"), format = "%H"))),
              end_time = mean(as.numeric(format(round(end_date, "hours"), format = "%H")))) %>%
    select(avg_hours_elapsed, start_time, end_time)
  
  rownames(myDf) <- myDf$user_id
  return(myDf)
}

cleanDfCharged <- cleanDataChargedKwh(df)
cleanDfTime <- cleanDataTime(df)

# Kmeans Clustering -------------------------------------------------------------------------------------------

clusteredChargedKm <- kmeans(cleanDfCharged, 5, nstart = 20)
clusteredTimeKm <- kmeans(cleanDfTime, 5, nstart = 20)

# Dunn's Index ------------------------------------------------------------------------------------------------

dunnKmCharged <- dunn(clusters = clusteredChargedKm$cluster, Data = cleanDfCharged)

dunnKmTime <- dunn(clusters = clusteredTimeKm$cluster, Data = cleanDfTime)

# Screeplot ---------------------------------------------------------------------------------------------------

screePlotCharged <- function() { 
  ratioSs <- rep(0, 10)
  
  for (k in 1:10) {
    userChargingKm <- kmeans(cleanDfCharged, k, nstart = 20)
    ratioSs[k] <- userChargingKm$tot.withinss / userChargingKm$totss
  }
  
  plot(ratioSs, type = "b", xlab = "k")
  
}

screePlotTime <- function() {
  ratioSs <- rep(0, 10)
  
  for (k in 1:10) {
    userTimeKm <- kmeans(cleanDfTime, k, nstart = 20)
    ratioSs[k] <- userTimeKm$tot.withinss / userTimeKm$totss
  }
  
  plot(ratioSs, type = "b", xlab = "k")
  
}

# Plots -------------------------------------------------------------------------------------------------------

plotClusterDataCharged <- function() {
  p <- plot_ly(cleanDfCharged, x = ~total_hours_elapsed, y = ~total_charged,
               z = ~total_charge_sessions, color = clusteredChargedKm$cluster, showscale = TRUE,
               hoverinfo = "text",
               text = ~paste("</br> Hours elapsed: ", total_hours_elapsed,
                             "</br> Charged kWh: ", total_charged,
                             "</br> Sessions: ", total_charge_sessions,
                             "</br> User id: ", rownames(cleanDfCharged))) %>%
    hide_colorbar() %>%
    layout(scene = list(xaxis = list(title = "total hours elapsed"),
                        yaxis = list(title = "total charged kwh"),
                        zaxis = list(title = "total sessions")))
  return(p)
}

plotClusterDataTime <- function() {
  p <- plot_ly(cleanDfTime, x = ~start_time, y = ~end_time,
               z = ~avg_hours_elapsed, color = clusteredTimeKm$cluster, showscale = TRUE,
               hoverinfo = "text",
               text = ~paste("</br> User id: ", rownames(cleanDfTime))) %>%
    hide_colorbar() %>%
    layout(scene = list(xaxis = list(title = "Avg start time"),
                        yaxis = list(title = "Avg end time"),
                        zaxis = list(title = "Avg hours elapsed")))
  return(p)
}

# Calls -------------------------------------------------------------------------------------------------------- 

plotClusterDataCharged()
plotClusterDataTime()
screePlotCharged()
screePlotTime()
