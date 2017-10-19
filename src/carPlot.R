# think about plot per car 

library(ggplot2)
library(config)
library(readr)

config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)


# table -------------------------------------------------------------------


# try to figure out the percentage
kindofcar <- function(){
  df %>% 
  group_by(car) %>% 
  summarise(number = n()) %>% 
  mutate(per = number / sum(number), car = factor(car, levels = car[order(per)]))
}

# it show the totalchaing per car
totalChargingPerCar <- function(){
  df %>% 
  group_by(car) %>% 
  summarise(totalCharged = sum(charged_kwh,na.rm=TRUE)) %>% 
  filter(totalCharged > 0.00) %>% 
  mutate(car = factor(car, levels = car[order(totalCharged)]))
}

# plot function -----------------------------------------------------------

# plot percentage per car

plotPersantagePerCar <- function(){
  ggplot(kindofcar(),aes(x=car,y=per)) +
  geom_bar(position= "dodge",stat= "identity") +
  coord_flip() +
  labs(x="car",y="percentage")+
  ggtitle("Percentage per car")
}



# plot charging rank by car

plotTotaltotalChargingPerCar <- function(){
  ggplot(totalChargingPerCar(),aes(x=car,y=totalCharged))+
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  labs(x = "car", y="Total Charging") +
  ggtitle("Total Charging per car")
}


# calls -------------------------------------------------------------------

plotPersantagePerCar()
plotTotaltotalChargingPerCar()

