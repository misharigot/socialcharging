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
              total_hours_elapsed = sum(hours_elapsed),
              total_effective_charging = sum(effective_charging_hours)) %>%
    mutate(efficiency_score = round((total_effective_charging / total_hours_elapsed) * 100 + 10, digits = 0),
           popularity_score = round(((total_hours_elapsed / as.numeric(totalHours))
                                     / outlets) * 100 + 10, digits = 0))
}
df <- cleanDataForStation(df)
# classification ----------------------------------------------------------

# using five number summary gice a point
givePoint <- function(x) {
  kwhSummary <- summary(x)
  fst <- kwhSummary[2]
  med <- kwhSummary[3]
  trd <- kwhSummary[5]
  ifelse (x > trd, 4,
          ifelse (x > med, 3,
                  ifelse (x > fst, 2, 1)
          )
  )
}

# sum the results and classify the station A to D
classifyStation <- function(){
  result <- givePoint(df$total_charged) + givePoint(df$total_hours_elapsed) +
    givePoint(df$efficiency_score) + givePoint(df$total_users)
  stationSummary <- summary(result)
  fst <- stationSummary[2]
  med <- stationSummary[3]
  trd <- stationSummary[5]
  ifelse (result > trd, "A",
         ifelse (result > med, "B",
                ifelse (result > fst, "C", "D")
         )
  )
}
df$stationClass <- classifyStation()


#divide data
intrain <- createDataPartition(y = df$stationClass, p = 0.7, list = FALSE)
train <- df[intrain, ]
test <- df[-intrain, ]

#make data frame to show station point and class
stationPointTable <- function(x) {
  df$chargePoint <- givePoint(df$total_charged)
  df$occuPoint <- givePoint(df$total_hours_elapsed)
  df$effPoint <- givePoint(df$efficiency_score)
  df$userPoint <- givePoint(df$total_users)
  df <- df %>%  select(latitude, address, chargePoint, occuPoint, effPoint, userPoint, stationClass)
}

stationDf <- stationPointTable(df)


# tree --------------------------------------------------------------------

#make tree
rpartmod <- rpart(stationClass ~ total_charged + total_hours_elapsed + efficiency_score + total_users, 
                data = train, method = "class")
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
predictClass <- function(totCharged, totHourElapsed, effScore, totUser) {
  pframe <- data.frame(total_charged = totCharged, total_hours_elapsed = totHourElapsed, efficiency_score = effScore, 
                       total_users = totUser)
  test <- predict(ptree, pframe, type = "class")
  return(test)
}