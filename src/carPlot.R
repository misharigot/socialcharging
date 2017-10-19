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
averageChargingPerCar <- function(){
  df %>% 
    group_by(car) %>% 
    filter(!is.na(charged_kwh)) %>% 
    summarise(number = n(), totalCharged = sum(charged_kwh)) %>% 
    filter(totalCharged > 0.00) %>% 
    mutate(chargingpercar = totalCharged/number)%>% 
    mutate(car = factor(car, levels = car[order(chargingpercar)]))  
}

# plot function -----------------------------------------------------------

# plot percentage per car

plotPersantagePerCar <- function(){
  ggplot(kindofcar(),aes(x=car,y=per)) +
  geom_bar(position= "dodge",stat= "identity",fill ="#66bb6a") +
  coord_flip() +
  labs(x="car",y="percentage")+
  ggtitle("Percentage per car")
}



# plot average charged kwh per car in january

plotAverageChargedKwhPerCar <- function(){
  ggplot(averageChargingPerCar(),aes(x=car,y=chargingpercar))+
    geom_bar(position = "dodge", stat = "identity",fill ="#66bb6a") +
    coord_flip() +
    labs(x = "car", y="Total Charging") +
    ggtitle("average Charged kwh per car in January")
}


# calls -------------------------------------------------------------------

plotPersantagePerCar()
plotAverageChargedKwhPerCar()

