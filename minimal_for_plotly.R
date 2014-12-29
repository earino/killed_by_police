library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
py <- plotly()

data <- read.csv("data//cleaned.csv")
data$date <- ymd(as.character(data$date))
data <- data %>% filter(date > ymd("2014/01/01"))

data$region = state.region[match(data$state, state.name)]

ggplot(data, aes(x=date)) + 
  geom_histogram(binwidth=2563200, color="black", fill="white") + 
  facet_wrap(~ region) + 
  ggtitle("Deaths by Police Across Time by Region") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

py$ggplotly()
