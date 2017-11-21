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
source("./src/models/regression_profile.R")

df <- read_csv2(config$scDataset, col_names = FALSE)

df <- cleanSecondDf(df)
cleanDf <- prepareDataForLM(df, 0)
idsWithPreds <- createLinearModelData(cleanDf, FALSE)
result <- base::merge(cleanDf, idsWithPreds, by = "session_id")
