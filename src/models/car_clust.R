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

cleanDataCarKwh <- function(data) {
  myDf <- data %>%
    filter(!is.na(hours_elapsed), !is.na(charged_kwh), !is.na(car)) %>%
    group_by(car) %>%
    summarise(total_charge_sessions = n(),
              total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed)) %>%
    select(total_charge_sessions, total_charged, total_hours_elapsed)
  
  rownames(myDf) <- myDf$car
  return(myDf)
}

cleanDfCar <- cleanDataCarKwh(df)

# Hierarchical Clustering -------------------------------------------------------------------------------------

clusteredCarSc <- as.data.frame(scale(cleanDfCar))
clusteredCarKm <- kmeans(clusteredCarSc, 5, nstart = 20)
clusteredCarDist <- dist(clusteredCarSc, method = "euclidean")
clusteredCarSingle <- hclust(clusteredCarDist, method = "single")
clusteredCarMembSingle <- cutree(clusteredCarSingle, k = 5)

clusteredCarComplete <- hclust(clusteredCarDist, method = "complete")
clusteredCarMembComplete <- cutree(clusteredCarComplete, k = 5)

# Dunn's Index ------------------------------------------------------------------------------------------------

dunnKmCar <- dunn(clusters = clusteredCarKm$cluster, Data = clusteredCarSc)
dunnKmCar

dunnSingleCar <- dunn(clusters = clusteredCarMembSingle, Data = clusteredCarSc)
dunnSingleCar

dunnCompleteCar <- dunn(clusters = clusteredCarMembComplete, Data = clusteredCarSc)
dunnCompleteCar

# Plots -------------------------------------------------------------------------------------------------------
plot(clusteredCarSingle)
rect.hclust(clusteredCarSingle, k = 5, border = 2:6)

plot(clusteredCarComplete)
rect.hclust(clusteredCarComplete, k = 5, border = 2:6)
