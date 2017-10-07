# This script does some general cleaning on the social charging csv.
library(readr)
library(dplyr)
library(lubridate)

cleanDataframe <- function(df) {
  df <- df %>% 
  rename(charged_kwh = `charged_kwh,`, kw_charge_point_speed = `kw  charge point speed)`) %>% 
  mutate(start_date = dmy_hm(start_date), 
         end_date = dmy_hm(end_date), 
         charged_kwh = as.numeric(charged_kwh))
  }
