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


intrain <- createDataPartition(y=df$stationClass, p = 0.7, list = FALSE)
train <- df[intrain, ]
test <- df[-intrain, ]

# classification ----------------------------------------------------------


#charged kwh 
classifyKwh <- function(x){
  kwhSummary <- summary(df$total_charged)
  fst <- kwhSummary[2]
  med <- kwhSummary[3]
  trd <- kwhSummary[5]
  ifelse(x > trd,4,
         ifelse(x>med,3,
                ifelse(x>fst,2,1)
                       )
         )
}

# occupation 
classifyOccu <- function(x){
  occuSummary <- summary(df$total_hours_elapsed)
  fst <- occuSummary[2]
  med <- occuSummary[3]
  trd <- occuSummary[5]
  ifelse(x > trd,4,
         ifelse(x>med,3,
                ifelse(x>fst,2,1)
         )
  )
}

# efficiency
classifyEff <- function(x){
  effSummary <- summary(df$efficiency_score)
  fst <- effSummary[2]
  med <- effSummary[3]
  trd <- effSummary[5]
  ifelse(x > trd,4,
         ifelse(x>med,3,
                ifelse(x>fst,2,1)
         )
  )
}

# user
classifyUser <- function(x){
  userSummary <- summary(df$total_users)
  fst <- userSummary[2]
  med <- userSummary[3]
  trd <- userSummary[5]
  ifelse(x > trd,4,
         ifelse(x>med,3,
                ifelse(x>fst,2,1)
         )
  )
}



# sum the results
#kwhS = kwh score per station 
#occuS = occupation score per station
#effS = efficiency scroe per sation
#userS = user score per station

classifyStation <- function(){
  result <- classifyKwh(df$total_charged) + classifyOccu(df$total_hours_elapsed) +
    classifyEff(df$efficiency_score) + classifyUser(df$total_users)
  stationSummary <- summary(result)
  fst <- stationSummary[2]
  med <- stationSummary[3]
  trd <- stationSummary[5]
  ifelse(result > trd,"A",
         ifelse(result>med,"B",
                ifelse(result>fst,"C","D")
         )
  )
}

df$stationClass <- classifyStation()
###

##

# tree --------------------------------------------------------------------

#make tree
rpartmod <- rpart(stationClass ~ total_charged + total_hours_elapsed + 
                    efficiency_score + total_users, data = train, method = "class")
plot(rpartmod)
text(rpartmod)

#pruning
printcp(rpartmod)
plotcp(rpartmod)

ptree <- prune(rpartmod, cp= rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]),"CP"])
plot(ptree)
text(ptree)

#check accuracy by using test dataset
rpartpred <- predict(ptree, test, type ="class")
confusionMatrix(rpartpred,test$stationClass)


# fancy plot
fancyRpartPlot(ptree,tweak=2)
