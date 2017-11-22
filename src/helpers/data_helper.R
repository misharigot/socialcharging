# Writes a csv to data folder with predictions
library(config)

config <- config::get(file = "config.yml")
source(config$baseClean)
source(config$multiplotHelper)
source("./src/models/regression_user_class.R")
source("./src/models/regression_station_class.R")

df <- read_csv2(config$scDataset, col_names = FALSE)
df <- cleanSecondDf(df)

# Creates a dataframe with predictions based on user classification 
cleanDf <- prepareDataForUserPred(df)
sessionsIdsWithPreds <- createLinearModelDataUser(cleanDf)
result <- base::merge(cleanDf, sessionsIdsWithPreds, by = "session_id")

# Creates a dataframe with predictions based on station classification 
cleanDf <- prepareDataForStationPred(result)
sessionsIdsWithPreds <- createLinearModelDataStation(cleanDf)
result <- base::merge(cleanDf, sessionsIdsWithPreds, by = "session_id")

# Writes dataframe to csv file
result <- as.data.frame(lapply(result, unlist))
write.csv(result, config$dataFolder)



