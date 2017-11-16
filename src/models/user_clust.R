library(ggplot2)
library(config)
library(dplyr)
library(plotly)
library(clValid)
config <- config::get(file = "config.yml")
source(config$baseClean)
source("src/models/user_class.R")
set.seed(100)

df <- read_csv2(config$scDataset, col_names = FALSE)
df <- cleanSecondDf(df)



# Data cleaning -----------------------------------------------------------------------------------------------
cleanDataChargedKwh <- function(data) {
  data %>%
    filter(!is.na(hours_elapsed), !is.na(charged_kwh)) %>%
    group_by(user_id) %>%
    summarise(total_charge_sessions = n(),
              total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed)) %>%
    select(total_charge_sessions, total_charged, total_hours_elapsed)
}

cleanDataTime <- function(data) {
  data <- data %>% 
    filter(!is.na(start_date), !is.na(end_date), !is.na(hours_elapsed), hours_elapsed < 56, !is.na(charged_kwh)) %>%
    select(charged_kwh, hours_elapsed, start_date_hour)
}



# Kmeans Clustering -------------------------------------------------------------------------------------------


# Dunn's Index ------------------------------------------------------------------------------------------------

# dunnKmCharged <- dunn(clusters = clusteredChargedKm$cluster, Data = cleanDfCharged)
# dunnKmTime <- dunn(clusters = clusteredTimeKm$cluster, Data = cleanDfTime)

# Screeplot ---------------------------------------------------------------------------------------------------

screePlot <- function(data) { 
  ratioSs <- rep(0, 10)
  for (k in 1:10) {
    kmeansObj <- kmeans(data, k, nstart = 20)
    ratioSs[k] <- kmeansObj$tot.withinss / kmeansObj$totss
  }
  plot(ratioSs, type = "b", xlab = "k")
  print(ratioSs)
}

dunnPlot <- function(data) {
  dunn <- rep(0, 10)
  for (k in 2:10) {
    kmeansObj <- kmeans(data, k, nstart = 20)
    dunn[k] <- dunn(clusters = kmeansObj$cluster, Data = data)
  }
  plot(dunn, type = "b", xlab = "k")
  print(dunn)
}

# Plots -------------------------------------------------------------------------------------------------------

plotClusterDataCharged <- function(data, kmeans) {
  plot_ly(data, x = ~total_hours_elapsed, y = ~total_charged,
          z = ~total_charge_sessions, color = kmeans$cluster, showscale = TRUE,
          hoverinfo = "text",
          text = ~paste("</br> Hours elapsed: ", total_hours_elapsed,
                        "</br> Charged kWh: ", total_charged,
                        "</br> Sessions: ", total_charge_sessions,
                        "</br> User id: ", rownames(data))) %>%
    hide_colorbar() %>%
    layout(scene = list(xaxis = list(title = "Total hours elapsed"),
                        yaxis = list(title = "Total charged kwh"),
                        zaxis = list(title = "Total sessions")))
}

plotClusterDataTime <- function(data, kmeans) {
  plot_ly(data, x = ~charged_kwh, y = ~hours_elapsed,
          z = ~start_date_hour, color = kmeans$cluster, showscale = TRUE,
          hoverinfo = "text",
          text = ~paste("</br> User id: ", rownames(data))) %>%
    hide_colorbar() %>%
    layout(scene = list(xaxis = list(title = "charged_kwh"),
                        yaxis = list(title = "hours_elapsed"),
                        zaxis = list(title = "start_date_hour")))
}

# Calls -------------------------------------------------------------------------------------------------------- 

# Fucked up #1
plotUserCluster1 <- function(scData) {
  cleanDfCharged <- cleanDataChargedKwh(scData)
  clusteredChargedKm <- kmeans(cleanDfCharged, 6, nstart = 20)
  plotClusterDataCharged(cleanDfCharged, clusteredChargedKm)
}

# Fucked up #2
plotUserCluster2 <- function(scData) {
  abc <- df %>% filter(!is.na(start_date), !is.na(end_date)) %>% sessionClassificationDf()
  cleanDfTime <- cleanDataTime(abc)
  clusteredTimeKm <- kmeans(cleanDfTime, 4, nstart = 20)
  plotClusterDataTime(cleanDfTime, clusteredTimeKm)
}

# screePlot(cleanDfCharged)
# screePlot(cleanDfTime)
# dunnPlot(cleanDfCharged)
# dunnPlot(cleanDfTime)
# dunn(clusters = clusteredTimeKm$cluster, Data = cleanDfTime)

