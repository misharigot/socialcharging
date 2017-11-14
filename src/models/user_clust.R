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
cleanData <- function() {
  df <- df %>%
    filter(!is.na(hours_elapsed), !is.na(charged_kwh)) %>%
    group_by(user_id) %>%
    summarise(total_charge_sessions = n(),
              total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed))
  
  return(df)
}

cleanedDf <- cleanData()  

# select columns needed for clustering ------------------------------------------------------------------------
clusteredDf <- cleanedDf %>%
  select(total_charge_sessions, total_charged, total_hours_elapsed)

# Clustering --------------------------------------------------------------------------------------------------
clustered_km <- kmeans(clusteredDf, 5, nstart = 20)

# Tables ------------------------------------------------------------------------------------------------------
table(clustered_km, cleanedDf$total_charged)

# Plots -------------------------------------------------------------------------------------------------------
plotClusterData <- function() {
  plot(total_charged ~ total_hours_elapsed, data = clusteredDf, col = clustered_km$cluster)

    p <- plot_ly(clusteredDf, x = ~total_hours_elapsed, y = ~total_charged,
                 z = ~total_charge_sessions, color = clustered_km$cluster, showscale = TRUE,
                 hoverinfo = 'text',
                 text = ~paste('</br> Hours elapsed: ', total_hours_elapsed,
                               '</br> Charged kWh: ', total_charged,
                               '</br> Sessions: ', total_charge_sessions)) %>%
      hide_colorbar() %>%
      layout(scene = list(xaxis = list(title = 'total hours elapsed'),
                                       yaxis = list(title = 'total charged kwh'),
                                       zaxis = list(title = 'total sessions')))
  return(p)
}

plotClusterData()

# Dunn's Index ------------------------------------------------------------------------------------------------
dunn_km <- dunn(clusters = clustered_km$cluster, Data = clusteredDf)
dunn_km

# Screeplot ---------------------------------------------------------------------------------------------------
ratio_ss <- rep(0, 10)

for (k in 1:15) {
  user_charging_km <- kmeans(clusteredDf, k, nstart = 20)
  ratio_ss[k] <- user_charging_km$tot.withinss / user_charging_km$totss
}

plot(ratio_ss, type = "b", xlab = "k")
ratio_ss
