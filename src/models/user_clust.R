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

abc <- df %>% filter(!is.na(start_date), !is.na(end_date)) %>% sessionClassificationDf()

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

# Todo: change logic
cleanDataTime <- function(data) {
  topUsersClasses <- userClassificationDf(data)
  
  summary <- data %>%
    filter(!is.na(start_date), !is.na(end_date), !is.na(hours_elapsed), hours_elapsed < 56) %>%
    group_by(user_id) %>%
    summarise(avg_hours_elapsed = mean(hours_elapsed),
              # start_time = mean(as.numeric(format(round(start_date, "hours"), format = "%H"))),
              # end_time = mean(as.numeric(format(round(end_date, "hours"), format = "%H"))),
              n_diff_stations = n_distinct(address)) %>%
    select(avg_hours_elapsed, n_diff_stations)
  print(str(summary))
  print(str(topUsersClasses))
  print(summary(summary))
  print(summary(topUsersClasses))
  
  
  summary$class <- map(topUsersClasses, function(x) {
    print(x["user_id"])
    print("en die andere:")
    print(summary$user_id)
    if (x["user_id"] == summary$user_id) {
      summary$top_class <- x["class"]
    }
  })
}

cleanDfCharged <- cleanDataChargedKwh(df)
cleanDfTime <- cleanDataTime(abc)

# Kmeans Clustering -------------------------------------------------------------------------------------------

clusteredChargedKm <- kmeans(cleanDfCharged, 5, nstart = 20)
clusteredTimeKm <- kmeans(cleanDfTime, 5, nstart = 20)

# Dunn's Index ------------------------------------------------------------------------------------------------

dunnKmCharged <- dunn(clusters = clusteredChargedKm$cluster, Data = cleanDfCharged)
dunnKmTime <- dunn(clusters = clusteredTimeKm$cluster, Data = cleanDfTime)

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

# Plots -------------------------------------------------------------------------------------------------------

plotClusterDataCharged <- function(data, kmeans) {
  plot_ly(data, x = ~total_hours_elapsed, y = ~total_charged,
          z = ~total_charge_sessions, color = kmeans$cluster, showscale = TRUE,
          hoverinfo = "text",
          text = ~paste("</br> Hours elapsed: ", total_hours_elapsed,
                        "</br> Charged kWh: ", total_charged,
                        "</br> Sessions: ", total_charge_sessions,
                        "</br> User id: ", rownames(cleanDfCharged))) %>%
    hide_colorbar() %>%
    layout(scene = list(xaxis = list(title = "Total hours elapsed"),
                        yaxis = list(title = "Total charged kwh"),
                        zaxis = list(title = "Total sessions")))
}

plotClusterDataTime <- function(data, kmeans) {
  plot_ly(data, x = ~start_time, y = ~n_diff_stations,
          z = ~avg_hours_elapsed, color = kmeans$cluster, showscale = TRUE,
          hoverinfo = "text",
          text = ~paste("</br> User id: ", rownames(cleanDfTime))) %>%
    hide_colorbar() %>%
    layout(scene = list(xaxis = list(title = "Avg start time"),
                        yaxis = list(title = "N Diff stations"),
                        zaxis = list(title = "Avg hours elapsed")))
}

# Calls -------------------------------------------------------------------------------------------------------- 

plotClusterDataCharged(cleanDfCharged, clusteredChargedKm)
plotClusterDataTime(cleanDfTime, clusteredTimeKm)
screePlot(cleanDfCharged)
screePlot(cleanDfTime)
