# think about plot per car 

library(ggplot2)
library(config)
library(readr)

config <- config::get(file = "../config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)


# table -------------------------------------------------------------------



# try to figure out the persentage
devidedbycar <- df %>% 
  group_by(car) %>% 
  summarise(number = n()) %>% 
  arrange(desc(number)) %>% 
  mutate(per = number / sum(number))

devidedbycar$label <- scales::percent(devidedbycar$per)


# it s the rank which car is most charged 
chargingbycar <- df %>% 
  group_by(car) %>% 
  summarise(totalCharged = sum(charged_kwh,na.rm=TRUE)) %>% 
  arrange(desc(totalCharged)) %>% 
  filter(totalCharged > 0.00)

# each car's gap between effective and real charging per hour

carGapEffandReal <- df %>% 
  group_by(car) %>% 
  filter(!is.na(charged_kwh),!is.na(hours_elapsed),!is.na(kw_charge_point_speed),!is.na(car)) %>% 
  summarise(totalKwChargepSpeed = sum(kw_charge_point_speed),totalChargedKwh = sum(charged_kwh), totalHoursElapsed = sum(hours_elapsed)) %>% 
  mutate(realChargingPerHour = totalChargedKwh/totalHoursElapsed, effectiveChargingHour = totalChargedKwh/totalKwChargepSpeed,
         gapRealEffective = realChargingPerHour - effectiveChargingHour) %>% 
  arrange(gapRealEffective) 


# make 2 kinds of datasets

effectiveChargingHour <- df %>% 
  group_by(car) %>% 
  filter(!is.na(charged_kwh),!is.na(hours_elapsed),!is.na(kw_charge_point_speed),!is.na(car)) %>% 
  summarise(totalchargespeed = sum(kw_charge_point_speed),totalcharge = sum(charged_kwh)) %>% 
  mutate(charginghour = totalcharge/totalchargespeed , factor = 0)%>% 
  select(car,charginghour,factor)

realchargingHour <- df %>%
  group_by(car) %>% 
  filter(!is.na(charged_kwh),!is.na(hours_elapsed),!is.na(kw_charge_point_speed),!is.na(car)) %>% 
  summarise(totalcharge = sum(charged_kwh),totalhour = sum(hours_elapsed)) %>% 
  mutate(charginghour = totalcharge/totalhour,factor = 1) %>% 
  select(car,charginghour,factor)

total <- rbind(effectiveChargingHour,realchargingHour)

plotresult <- ggplot(total,aes(x=car,y=charginghour,fill=factor(factor)))+
  geom_bar(position ="dodge",stat="identity")+
  coord_flip() +
  scale_x_discrete(
    limits = c(
      "Tesla Model S 90","Tesla Model S 85","Tesla Model S 70","BMW i3","Renault Zoe Q210",
      "BMW 330E","Volvo V60 D6 ADW","Volvo V60 D5 ADW","Unknown","Opel Ampera","Audi E-Tron Plug-in Hybrid",
      "BMW 225xe XE","Volkswagen Passat GTE","Mercedes-Benz C350e","Volkswagen E-Golf","Volvo XC90 PHEV",
      "Volkswagen Golf GTE","Mitsubishi Outlander PHEV","Toyota Prius Plug-in","Ford C-Max Energi","Chevrolet Volt",
      "Mercedes-Benz B250e","Nissan Leaf model 30 kWh","Renault Kangoo","Nissan Leaf model 24 kWh","Mercedes-Benz Vito E-Cell",
      "Mercedes-Benz A-klasse - E"),
    labels = c(
      "Tesla Model S 90","Tesla Model S 85","Tesla Model S 70","BMW i3","Renault Zoe Q210",
      "BMW 330E","Volvo V60 D6 ADW","Volvo V60 D5 ADW","Unknown","Opel Ampera","Audi E-Tron Plug-in Hybrid",
      "BMW 225xe XE","Volkswagen Passat GTE","Mercedes-Benz C350e","Volkswagen E-Golf","Volvo XC90 PHEV","Volkswagen Golf GTE",
      "Mitsubishi Outlander PHEV","Toyota Prius Plug-in","Ford C-Max Energi","Chevrolet Volt","Mercedes-Benz B250e",
      "Nissan Leaf model 30 kWh","Renault Kangoo","Nissan Leaf model 24 kWh","Mercedes-Benz Vito E-Cell","Mercedes-Benz A-klasse - E")
  )


plotresult


# class(as.data.frame(effectiveChargingHour))
# class(as.data.frame(realchargingHour))
# my<- merge(effectiveChargingHour, realchargingHour, by = intersect(names(effectiveChargingHour), names(realchargingHour)))
# fuckingDataframe <- as.data.frame(my)

temp <- rbind(effectiveChargingHour, realchargingHour)
# plot function -----------------------------------------------------------

# using pie chart and show the percentage (i want to show just 5 and the other just sum it )
bp <- ggplot(devidedbycar, aes(x="",y=per,fill=car)) +
  geom_bar(width = 1, stat= "identity") +
  coord_polar("y", start=0 ) +
  theme_void() +
  geom_text(aes(x=1, y=cumsum(per) - per/2, label=label ))

bp

# plot charging rank by car
chargingbycar$car <-factor(chargingbycar$car, levels = chargingbycar$car[order(chargingbycar$totalCharged)])

plotTotalChargingPerCar <- ggplot(chargingbycar,aes(x=car,y=totalCharged))+
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "car", y="Total Charging") +
  ggtitle("Total Charging by car")
  coord_flip()

plotTotalChargingPerCar

# plot persantage per car
devidedbycar$car <-factor(devidedbycar$car, levels = devidedbycar$car[order(devidedbycar$per)])

plotPersantagePerCar <- ggplot(devidedbycar,aes(x=car,y=per)) +
  geom_bar(position= "dodge",stat= "identity") +
  coord_flip() +
  labs(x="car",y="persentage")+
  ggtitle("Persantage per car")

plotPersantagePerCar



# plot list realCharging Per car
carGapEffandReal$car <-factor(carGapEffandReal$car,levels = carGapEffandReal$car[order(carGapEffandReal$realChargingPerHour)])

plotRealChargingPerCar <- ggplot(carGapEffandReal,aes(x=car,y=realChargingPerHour)) +
  geom_bar(position="dodge",stat ="identity")+
  labs(x= "Real Charging per Hour", y= "Car") +
  ggtitle("Real Charging Per Car") +
  coord_flip()

plotRealChargingPerCar
