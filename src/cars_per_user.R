library(ggplot2)
library(config)
library(readr)
library(dplyr)
config <- config::get(file = "config.yml")
source(config$baseClean)

df <- read_csv2(config$scDataset, col_names = FALSE)
df <- cleanDataframe(df)

# Table functions ---------------------------------------------------------

# Returns a table that counts the amount of user per car
getCarAmount <- function() {
  df %>%
    group_by(car) %>%
    filter(!is.na(car)) %>%
    summarise(count = n())
}

# Returns a table that shows what user uses what car
getCarPerUser <- function() {
  df %>%
    select(user_id, car) %>%
    group_by(user_id) %>%
    arrange(df, desc(user_id), .by_group = TRUE) %>%
    slice(1L)
}

# Plot functions ----------------------------------------------------------

# Returns a plot that shows how many users use a type of car
plotCarAmount <- function() {
  p <- ggplot(getCarAmount(), aes(x = car, y = count)) +
    geom_bar(stat = "identity") +
    geom_smooth() +
    labs(x = "Cars", y = "User amount") +
    ggtitle("Amount of users for each car") +
    coord_flip() +
    theme_light()
  return(p)
}

# Calls -------------------------------------------------------------------

carPerUser <- getCarPerUser()
carSummarise <- getCarAmount()
plotCarAmount()
