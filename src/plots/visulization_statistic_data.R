# 91 visulization statistic data
library(config)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(plotly)

config <- config::get(file = "config.yml")
source(config$baseClean)

# df <- read_csv2(config$scDataset, col_names = FALSE)
# df <- cleanDataframe(df)

# Returns a list with 2 spaces: 1. df filtered by user input and 2. the amount of rows unfiltered
applyFilters <- function(df, columnName, corrupt, session) {
  totalNRow <- nrow(df)
  
  if (corrupt) {
    df <- df %>% 
      filter(!(start_date == end_date))
  }
  
  if (session) {
    count <- df %>% 
      group_by(user_id) %>% 
      summarise(sessionNum = n_distinct(session_id))
    
    df <- base::merge(df, count, by = "user_id", dropDups = FALSE)
    
    df <- df %>% 
      filter(sessionNum >= 10)
  }
  
  if (length(columnName) > 0) {
    for (i in 1:length(columnName)) {
      df <- df %>% 
        filter(!is.na(df[,columnName[i]]))
    }
  }
  resultList <- list(df, totalNRow)
  return(resultList)
}

# Create the table that the barchart plot expects
createResultTable <- function(resultList) {
  df <- resultList[[1]]
  totalNRow <- resultList[[2]]
  
  dataStat <- c("useable", "unuseable")
  data <- c("data", "data")
  num <- rep(0, 2)
  num[1] <- nrow(df)
  num[2] <- totalNRow - nrow(df)
  percentage <- rep(0,2)
  percentage[1] <- paste0(round(num[1] / totalNRow * 100, digits = 2), '%')
  percentage[2] <- paste0(round(num[2] / totalNRow * 100, digits = 2), '%')
  dataTable <- data.frame(type = data, status = dataStat, count = num, percentage = percentage)
  return(dataTable)
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
    ggtitle("Our Dataset Status") +
    theme(plot.title = element_text( hjust = 0.5, size = 20))
  # p <- ggplotly(p)
  return(p)
}

showDataStatusPlot <- function(scData, columName = c(), corrupt = FALSE, session = FALSE){
  dataStatusPlot(createResultTable(applyFilters(scData, columName, corrupt, session)))
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