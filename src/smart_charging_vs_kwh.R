# smart charging related to kwh
library(ggplot2)
library(config)
library(readr)
config <- config::get()
source(config$baseClean)

df <- read_csv2(config$scDataset)
df <- cleanDataframe(df)

df <- df %>%
  filter(!is.na(end_date), !is.na(charged_kwh))

# checking the amount of usages on the smart- and non smart chargers
barplot <- ggplot(data=df,aes(x=smart_charging)) + geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=-1)
barplot + labs(x = "smart charging")

# subsetting the smart- and non smart chargers
df_is_smart <- filter(df, df$smart_charging == 'Yes')
df_not_smart <- filter(df, df$smart_charging == 'No')

# plot 1 
plotSmartChargers1 <- ggplot(df_is_smart, aes(y = charged_kwh, x = hours_elapsed)) + geom_point(alpha = 0.3) + geom_smooth()
plotSmartChargers1 + labs(x = "session time in hours", y = "kWh charged")

# plot 1.1
plotSmartChargers2 <- ggplot(df_is_smart, aes(y = effective_charging_hours, x = hours_elapsed)) + geom_point(alpha = 0.3)
plotSmartChargers2 + labs(x = "effective charging time in hours", y = "elapsed time in hours")

# plot 2
plotNonSmartChargers1 <- ggplot(df_not_smart, aes(y = charged_kwh, x = hours_elapsed)) + geom_point(alpha = 0.3) + geom_smooth()
plotNonSmartChargers1 + labs(x = "session time in hours", y = "kWh charged")

# plot 2.1
plotNonSmartChargers2 <-  ggplot(df_not_smart, aes(y = effective_charging_hours, x = hours_elapsed)) + geom_point(alpha = 0.3) 
plotNonSmartChargers2 + labs(x = "effective charging time in hours", y = "elapsed time in hours")

