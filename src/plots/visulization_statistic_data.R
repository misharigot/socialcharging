# 91 visulization statistic data

library(config)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(plotly)


config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanSecondDf(df)

makeTable <- function(testDf){
  name <- c("total","useable")
  xname <- rep(name,16)
  num <- c(1:16)
  snum <- rep(num,each=2)
  useable <- rep(0,32)
  for(k in 1:16){
    useable[k*2-1] <- nrow(testDf)
    useable[k*2] <- useable[k*2-1] - sum(is.na(testDf[,k]))
  }
  result <- data.frame(x = xname, y = useable,stationNum= snum)
  return(result)
}


filterColums <- function(result,selectNum){
  aa <-result %>% 
    filter(stationNum == selectNum)
  
  p <- ggplot(data=aa, aes(x= x, y=y)) +
    geom_line(stat = 'summary', fun.y=sum) +
    stat_summary(fun.y =sum,geom ="line") +
    geom_point() 
  
  p <- ggplotly(p)
  return(p)
}

staticPlot <- function(scData,selectNum){
  filterColums(makeTable(scData),selectNum)
}

staticPlot(df,15)



# nameve <- names(df)
# unuseable <- rep(0,16)
# usable <-rep(0,16)
# 
# for(k in 1:16){
#   unuseable[k]<- sum(is.na(df[,k]))
#   usable[k] <- nrow(df) - unuseable[k]
# }
# 
# show <- data.frame(column = nameve, usable = usable,unuseable= unuseable)




















# miss going --------------------------------------------------------------


makePivotTable <-function(df){
  testDf <- df
  testDf <- testDf %>% 
    filter(!is.na(charged_kwh)) %>% 
    mutate(start_date = sapply(start_date,function(x) { substr(x,1,7)}))
  pivotTable <- dcast(testDf,user_id~start_date,value.var = "charged_kwh",sum)
  return(pivotTable)
}

selectUser <- function(pivotTable,x){
  a <- x
  selectUser <- pivotTable %>% 
    filter(user_id == a) %>% 
    mutate(user_id = as.character(user_id))
  changeStructure <- gather(selectUser,year,kwh,-user_id)
  return(changeStructure)
}

makePlot <- function(changeStructure){
  p <- ggplot(data=changeStructure, aes(x= year, y=kwh)) +
    geom_line(stat = 'summary', fun.y=sum) +
    stat_summary(fun.y =sum,geom ="line") +
    geom_point() 
  
  p <- ggplotly(p)
  return(p)
}

pivotTable <- makePivotTable(df)
changeStructure <- selectUser(pivotTable,2259)
makePlot(changeStructure)


finalplot <- function(scData,text){
  makePlot(selectUser(makePivotTable(scData),text))
}

finalplot(df,2259)

# p <- ggplot(data=changeStructure, aes(x= year, y=kwh)) +
#   geom_line(stat = 'summary', fun.y=sum) +
#   stat_summary(fun.y =sum,geom ="line") +
#   geom_point()
# 
# p <- ggplotly(p)
# p

sary <- summary(pivotTable)
head(sary)
sary[4, ]





ss <- gather(sary[4,],year,mean,-user_id)

dim(sary)
head(sary)
class(sary[4, ])
sary[4]
