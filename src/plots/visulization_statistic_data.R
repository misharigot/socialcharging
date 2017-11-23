# 91 visulization statistic data
library(config)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(plotly)

config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset, col_names = FALSE)
df <- cleanDataframe(df)

# Return the df filtered by user input
applyFilters <- function(df, filters) {
  df <- read_csv2(config$scDataset, col_names = FALSE)
  df <- cleanDataframe(df)
  if (filters[["corruptDate"]]) {
    df <- subset(df, (!(start_date == end_date) ) | is.na(charged_kwh))
    df <- subset(df, hours_elapsed > 0)
  }
  
  if (filters[["zeroCharged"]]) {
    df <- subset(df, charged_kwh > 0 | is.na(charged_kwh))
  }
  
  if (filters[["normalHoursElapsed"]]) {
    df <- subset(df, hours_elapsed < 100)
  }
  
  if (filters[["session"]]) {
    count <- df %>% 
      group_by(user_id) %>% 
      summarise(sessionNum = n_distinct(session_id))
    
    df <- base::merge(df, count, by = "user_id", dropDups = FALSE)
    
    df <- df %>% 
      filter(sessionNum >= 10)
  }

  valuesNA <- filters[["valuesNA"]]

  if (!is.null(valuesNA)) {
    if (length(valuesNA) > 0) {
      for (i in 1:length(valuesNA)) {
        df <- df %>% 
          filter(!is.na(df[,valuesNA[i]]))
      }
    }
  }
  return(df)
}

# Create the data frame that the barchart plot expects
createResultData <- function(df, originalNRow) {
  statuses <- c("usable", "unusable")
  data <- c("data", "data")
  
  rowCount <- rep(0, 2)
  rowCount[1] <- nrow(df)
  rowCount[2] <- originalNRow - nrow(df)
  
  percentage <- rep(0,2)
  percentage[1] <- paste0(round(rowCount[1] / originalNRow * 100, digits = 2), '%')
  percentage[2] <- paste0(round(rowCount[2] / originalNRow * 100, digits = 2), '%')
  
  resultDf <- data.frame(type = data, status = statuses, count = rowCount, percentage = percentage)
  return(resultDf)
}

# Create the barchart plot
dataStatusPlot <- function(dataTable) {
  p <- ggplot(dataTable, aes(x = type, y = count, fill = status)) +
    geom_bar(stat = "identity", position = "stack", width = 0.2) +
    theme_void() +
    geom_text(aes(label = percentage),
              position = position_stack(vjust = 0.5),
              size = 10) +
    guides( fill = guide_legend(title = "Status")) +
    theme( legend.justification = c(1, 0), legend.position = c(1, 0), legend.text = element_text(size = 18),
           legend.title = element_text(size = 20)) +
    ggtitle("Usable data ratio") +
    theme(plot.title = element_text( hjust = 0.5, size = 20))
  return(p)
}

showDataStatusPlot <- function(scData, filters){
  filteredDf <- applyFilters(scData, filters)
  resultData <- createResultData(filteredDf, nrow(scData))
  dataStatusPlot(resultData)
}

getFilteredDataCount <- function(scData, filters) {
  createResultData(applyFilters(scData, filters), nrow(scData))$count[1]
}
