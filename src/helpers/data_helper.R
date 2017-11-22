# Writes a csv to data folder with predictions
library(ggplot2)
library(config)
library(readr)
library(lubridate)
library(tidyr)
library(purrr)
library(corrplot)

config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)
source("./src/models/regression_user_class.R")

df <- read_csv2(config$scDataset, col_names = FALSE)
df <- cleanSecondDf(df)

# Creates a csv file with predictions based on user classification 
cleanDf <- prepareDataForLM(df, 0)
sessionsIdsWithPreds <- createLinearModelData(cleanDf)
result <- base::merge(cleanDf, sessionsIdsWithPreds, by = "session_id")

write.csv(result, config$dataFolder, row.names = TRUE)



