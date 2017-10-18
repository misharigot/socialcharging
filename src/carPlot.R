# think about plot per car 

library(ggplot2)
library(config)
library(readr)

config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)


# table -------------------------------------------------------------------


# try to figure out the persentage
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
# each car's gap between effective and real charging per hour

realtotalChargingPerCar<- function(){
  df %>%
  filter(!is.na(charged_kwh),!is.na(hours_elapsed),!is.na(kw_charge_point_speed),!is.na(car)) %>%
  group_by(car) %>% 
  summarise(totalChargedKwh = sum(charged_kwh, na.rm = TRUE), totalHoursElapsed = sum(hours_elapsed,na.rm = TRUE)) %>% 
  mutate(realChargingPerHour = totalChargedKwh/totalHoursElapsed, car = factor(car,levels = car[order(realChargingPerHour)]))
}

aa<-realtotalChargingPerCar()

# plot function -----------------------------------------------------------

# plot persantage per car

plotPersantagePerCar <- function(){
  ggplot(kindofcar(),aes(x=car,y=per)) +
  geom_bar(position= "dodge",stat= "identity") +
  coord_flip() +
  labs(x="car",y="persentage")+
  ggtitle("Persantage per car")
}



# plot charging rank by car

plotTotaltotalChargingPerCar <- function(){
  ggplot(totalChargingPerCar(),aes(x=car,y=totalCharged))+
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  labs(x = "car", y="Total Charging") +
  ggtitle("Total Charging by car")
}


# plot list realCharging Per car

plotRealtotalChargingPerCar <- function(){
  ggplot(realtotalChargingPerCar(),aes(x=car,y=realChargingPerHour)) +
  geom_bar(position="dodge",stat ="identity")+
  labs(x= "Real Charging per Hour", y= "Car") +
  ggtitle("Real Charging Per Car") +
  coord_flip()
}

# calls -------------------------------------------------------------------

plotPersantagePerCar()
plotTotaltotalChargingPerCar()
plotRealtotalChargingPerCar()
