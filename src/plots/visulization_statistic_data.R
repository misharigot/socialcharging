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
a <- c("hello" = "goodbye")
# Return the df filtered by user input
applyFilters <- function(df, filters) {
  if (filters[["corruptDate"]]) {
    df <- df %>% 
      filter(!(start_date == end_date))
  }
  
  if (filters[["session"]]) {
    count <- df %>% 
      group_by(user_id) %>% 
      summarise(sessionNum = n_distinct(session_id))
    
    df <- base::merge(df, count, by = "user_id", dropDups = FALSE)
    
    df <- df %>% 
      filter(sessionNum >= 10)
  }
  print(filters)
  valuesNA <- filters[["valuesNA"]]
  print(valuesNA)
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
              position = position_stack(vjust = 0.5)) +
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

# showDataStatusPlot(df,columName,corrupt,session)
# session<- TRUE
# aa <- df %>% 
#   group_by(user_id) %>% 
#   summarise(n_distinct(session_id))
# 
# 
# aaa <- data.frame(name = c("apple","bpple","apple","bpple"),num = c(3,4,3,4))
# bbb <- data.frame(name = c("apple","bpple"),count = c(1,2))
# dMerge(aaa,bbb, by = 'name',dropDups = FALSE)
# 
# 
# count <- df %>% 
#   group_by(user_id) %>% 
#   summarise(sessionNum = n_distinct(session_id))
# 
# df <- dMerge(df,count,by = 'user_id',dropDups = FALSE)
# 
# df <- df %>% 
#   filter(sessionNum >= 10)













# #user session
# aaaa <- df %>%
#   filter(!is.na(charged_kwh),!is.na(hours_elapsed)) %>%
#   group_by(user_id) %>%
#   summarise(number = n_distinct(session_id))
# userPlot <- ggplot(aaaa, aes(x=user_id, y=number)) +
#   geom_bar(stat ="identity")
# userPlot
# 
# 
# 
# # test
# 
# source("src/models/user_class.R")
# testdf <- sessionClassificationDf(cleanDf(df))
# testdf <- testdf %>%
#   filter(!is.na(charged_kwh)) %>%
#   group_by(user_id) %>%
#   summarise(count = n_distinct(class))
# 
# classPlot <- ggplot(testdf,aes(x=user_id, y= count)) +
#   geom_bar(stat = "identity")+
#   ylim(0,25) +
#   geom_smooth()
# classPlot
# 
# testdf$user_id <- as.character(testdf$user_id)
# class(testdf$user_id)
# 
# 


# df <- read_csv2(config$scDataset, col_names = FALSE)
# df <- cleanDataframe(df)
# 
# nameve <- names(df)
# unuseable <- rep(0,17)
# usable <-rep(0,17)
#  
# for(k in 1:17){
#   unuseable[k]<- sum(is.na(df[,k]))
#   usable[k] <- nrow(df) - unuseable[k]
# }
# 
# show <- data.frame(column = nameve, usable = usable,unuseable= unuseable)