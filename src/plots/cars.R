# think about plot per car 
library(ggplot2)
library(config)
library(readr)

# Table functions ---------------------------------------------------------

# Try to figure out the percentage
kindOfCar <- function(scData){
  scData %>%
  group_by(car) %>%
  summarise(number = n()) %>%
  mutate(per = number / sum(number), car = factor(car, levels = car[order(per)]))
}

# It shows the total kWh charged per car
averageChargingPerCar <- function(scData){
  scData %>%
    group_by(car) %>%
    filter(!is.na(charged_kwh)) %>%
    summarise(number = n(), totalCharged = sum(charged_kwh)) %>%
    filter(totalCharged > 0.00) %>%
    mutate(chargingpercar = totalCharged / number) %>%
    mutate(car = factor(car, levels = car[order(chargingpercar)]))
}

# Plot functions ----------------------------------------------------------

# Plot the total car percentages
plotPercentagePerCar <- function(scData){
  ggplot(kindOfCar(scData), aes(x = car, y = per)) +
  geom_bar(position = "dodge", stat = "identity", fill = "#66bb6a") +
  coord_flip() +
  labs(x = "car", y = "percentage") +
  ggtitle("Percentage per car")
}

# Plot average charged kwh per car
plotAverageChargedKwhPerCar <- function(scData){
  ggplot(averageChargingPerCar(scData), aes(x = car, y = chargingpercar)) +
    geom_bar(position = "dodge", stat = "identity", fill = "#66bb6a") +
    coord_flip() +
    labs(x = "car", y = "Total Charging") +
    ggtitle("Average Charged kwh per car")
}

# Calls -------------------------------------------------------------------

# plotPercentagePerCar()
# plotAverageChargedKwhPerCar()
