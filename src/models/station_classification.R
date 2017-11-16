#68-station classification
#classify the station by how important they are 
#using the information about shiny map 
library(RColorBrewer)
library(rpart.plot)
library(rattle)
library(caret)
library(rpart)
library(ggplot2)
library(config)
library(readr)
library(e1071)


config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanSecondDf(df)
set.seed(1000)


# trim data ----------------------------------------------------------

cleanDataForStation <- function(x) {

  df <- df %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(charged_kwh), !is.na(hours_elapsed))
  totalHours <- interval(min(df$start_date), max(df$end_date)) / 3600
  df <- df %>%
    group_by(longitude, latitude) %>%
    summarise(address = first(address),
              outlets = first(outlets),
              total_sessions = n(),
              total_users = n_distinct(user_id),
              total_charged = sum(charged_kwh),
              total_hours_elapsed = sum(hours_elapsed)
             )
}
df <- cleanDataForStation(df)
# classification ----------------------------------------------------------

# using five number summary give the station a score (high or low)
givePoint <- function(x) {
  kwhSummary <- summary(x)
  med <- kwhSummary[3]
  ifelse (x > med, "H", "L")
}

#make a station table
stationPointTable <- function(x) {
  df$charging <- givePoint(df$total_charged)
  df$occupation <- givePoint(df$total_hours_elapsed)
  df$user_amount <- givePoint(df$total_users)
  df <- df %>%  select(latitude, address, occupation, user_amount, charging)
}
stationDf <- stationPointTable(df)

# classify the station based on the scores of the features
stationDf$stationClass <- paste(stationDf$occupation, stationDf$user_amount, stationDf$charging, sep = "")
class(stationDf$stationClass)
changeName <- function(x) {
  ifelse (x == "HHH", "LadyOfTheEvening",
          ifelse ( x == "HHL", "ParkingSpace",
                   ifelse (x == "HLL", "MarriedToThisStation",
                           ifelse (x == "HLH", "LateNightCharging",
                                   ifelse(x == "LHH", "WorkerBee",
                                          ifelse(x == "LLH", "PowerBank",
                                                 ifelse(x == "LLL", "ForeverAlone", "HitandRun")
                                                 )
                                          )
                                   )
                           )
                   )
  )
}
stationDf$stationClass <- changeName(stationDf$stationClass)
df$stationClass <- stationDf$stationClass



# distribution ------------------------------------------------------------

# show the distribution of the station class
stationClassDis <- function(){
  stationDf %>%
  group_by(stationClass) %>%
  summarise(num = n()) %>%
  mutate(stationClass = factor(stationClass, levels = stationClass[order(num)]))  
}
showDistribution <- function(){
  ggplot(stationClassDis(), aes(x = stationClass, y = num)) +
    geom_bar(position = "dodge", stat = "identity", fill = "#66bb6a") +
    coord_flip() +
    ggtitle("Show the station Class number")
}
showDistribution()



# tree --------------------------------------------------------------------

#divide data
intrain <- createDataPartition(y = df$stationClass, p = 0.7, list = FALSE)
train <- df[intrain, ]
test <- df[-intrain, ]


#make tree
rpartmod <- rpart(stationClass ~ total_hours_elapsed + total_users + total_charged, data = train, method = "class")
plot(rpartmod)
text(rpartmod)

#pruning
printcp(rpartmod)
plotcp(rpartmod)

ptree <- prune(rpartmod, cp = rpartmod$cptable[which.min(rpartmod$cptable[, "xerror"]), "CP"])
plot(ptree)
text(ptree)

#check accuracy by using test dataset
rpartpred <- predict(ptree, test, type = "class")
confusionMatrix(rpartpred, test$stationClass)

# fancy plot
fancyRpartPlot(ptree, tweak = 1.5)

#predict buy using input data
predictClass <- function(totHourElapsed, totUser, totCharged ) {
  pframe <- data.frame(total_hours_elapsed = totHourElapsed, total_users = totUser, total_charged = totCharged)
  test <- predict(ptree, pframe, type = "class")
  return(test)
}

