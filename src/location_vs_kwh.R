# Smart charging related to kWh
library(ggplot2)
library(config)
library(readr)
library(dplyr)

config <- config::get(file = "../config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

CreateDataForMapPlot <- function() {

  df <- df %>%
    filter(!is.na(latitude),!is.na(longitude), !is.na(charged_kwh))
  
  split_str_by_index <- function(target, index) {
    index <- sort(index)
    substr(rep(target, length(index) + 1),
           start = c(1, index),
           stop = c(index -1, nchar(target)))
  }
  
  interleave <- function(v1,v2)
  {
    ord1 <- 2*(1:length(v1))-1
    ord2 <- 2*(1:length(v2))
    c(v1,v2)[order(c(ord1,ord2))]
  }
  
  df$latitude <- sapply(df$latitude, function(x){
    insert <- "."[order(3)]
    index <- sort(3)
    paste(interleave(split_str_by_index(x, 3), "."), collapse="")
  })
  
  df$longitude <- sapply(df$longitude, function(x){
    insert <- "."[order(2)]
    index <- sort(2)
    paste(interleave(split_str_by_index(x, 2), "."), collapse="")
  })

  df <- df %>%
    group_by(longitude, latitude) %>%
    summarise(total = sum(charged_kwh))
  
  df$latitude <- as.numeric(df$latitude)
  df$longitude <- as.numeric(df$longitude)
  
  return(df)
}


