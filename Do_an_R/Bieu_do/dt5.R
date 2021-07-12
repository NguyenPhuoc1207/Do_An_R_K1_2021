#cai thu 20
library(lubridate)
library(plotly)
library(tidyverse)
covid <- read_csv("https://raw.githubusercontent.com/laxmimerit/Covid-19-Preprocessed-Dataset/master/preprocessed/country_daywise.csv")
head(covid)
dim(covid)
covid$Date <- ymd(covid$Date)
head(covid)
tail(covid)
# Bieu do thu 20 line plot
daywise <- country_daywise
daywise$Date <- ymd(daywise$Date)
daywise <- arrange(daywise, Date)
head(daywise)
plot_ly(daywise, x=~Date, y=~Confirmed, type='scatter', mode='lines')
names(daywise)
fig <- plot_ly(daywise, x=~Date)
fig <- fig %>% add_trace(y=~Confirmed,
                         name='Confirmed')
fig <- fig %>% add_trace(y=~Recovered,
                         name= 'Recovered')
fig <- fig %>% add_trace(y=~Deaths,
                         name='Deaths')
fig
# bi???u d??? th??? 14 S
