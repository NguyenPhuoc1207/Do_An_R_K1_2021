library(lubridate)
library(plotly)
library(tidyverse)
covid <- read_csv("https://raw.githubusercontent.com/laxmimerit/Covid-19-Preprocessed-Dataset/master/preprocessed/country_daywise.csv")
head(covid)
dim(covid)
covid$Date <- ymd(covid$Date)
head(covid)
tail(covid)
# Bieu do thu 21 line plot
daywise <- country_daywise
daywise$Date <- ymd(daywise$Date)
daywise <- arrange(daywise, Date)
head(daywise)
plot_ly(daywise, x=~Date, y=~Confirmed, type='scatter', mode='lines')
names(daywise)
# Bieu do thu 22 Bar chart
head(covid)
tail(covid)
latest <- covid %>% filter(Date == max(Date)) %>% arrange(desc(Confirmed))
top10 <- latest %>% slice(1:10)
top10
summary(top10)
plot_ly(top10, x=~Country, y=~Confirmed, type = 'bar', name = 'Confirmed Cases')
factor(top10$Country, level =c(as.character(top10$Country)))
top10$Country <- factor(top10$Country, levels = c(as.character(top10$Country)))


plot_ly(top10, x=~Country, y=~Confirmed, type ='bar', name='Confirmed Cases')
#thêm chú thích và giá tr??? vào
values <- as.character(top10$Confirmed)
plot_ly(top10, x=~Country, y=~Confirmed, type ='bar', name='Confirmed Cases',
        text= values, textposition='auto', marker = list( line = list(color ='magenta', width=0)))
rainbow(n=10)
plot_ly(top10, x=~Country, y=~Confirmed, type ='bar', name='Confirmed Cases',
        text= values, textposition='auto', marker = list(color =heat.colors(n=10)))
#Bi???u d??? 23 subplots
us <- covid %>% filter(Country == 'US') %>% arrange(Date)
head(us)
tail(us)
#type='scatter', mode='lines'
dl1 <- plot_ly(us, x = ~Date, y=~Confirmed, name = 'Confirmed Cases')
dl2 <- plot_ly(us, x = ~Date, y=~Recovered, name ='Recovered Cases')
dl3 <- plot_ly(us, x = ~Date, y=~Deaths, name ='Deaths Cases')
subplot(dl1,dl2,dl3, nrows = 2, shareX = FALSE)
pl1 <- plot_ly(daywise, x~Date, mode = 'bar')
pl1 <- plot_ly(daywise, x=~Date, y=~Confirmed,type='bar', name='Confermed Cases')
pl1 <- pl1 %>% add_trace( y=~Recovered,type='bar',name ='Recovered')
pl1 <- pl1 %>% add_trace( y=~Deaths,type='bar',name ='Deaths')
pl1
# Bi???u d??? 24 pie                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
plot_ly(data=top10, x= ~Country, values=~Confirmed, type= 'pie', textinfo='label +percent')
pc <-plot_ly(top10, labels=~Country, values=~Deaths, type= 'pie', texinfo='label + percent')
pc
#Scatt
fig<-plot_ly(data=top10, x=~Confirmed, y~Deaths)
fig<-fig %>% add_trace(type='scatter', mode= 'markers', color=heat.colors(n=10), size=~Confirmed,
        marker= list(size=~Deaths/4000))
plot_ly(top10, labels=~Country, values=~Deaths, type= 'pie', texinfo='label + percent')
pc



#---------------------------
#24 bieu do scatter
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
#Load Data
url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/06-29-2021.csv"
df <- read.csv(url, header = TRUE)
#Nhom du lieu
country <- df %>% group_by(Country_Region)
country <- country %>% summarise(Recovered=sum(Recovered),
                                 Confirmed=sum(Confirmed),
                                 Deaths=sum(Deaths))
#Sort
country <- country[order(-country$Confirmed),]
country<-country[-c(1,2,3,4,7,11),]
country<-country<-country[1:10,]

data <- country
fig <- plot_ly(data, x = ~Confirmed, y = ~Recovered)
fig <- fig %>% add_trace(x = ~Confirmed, y = ~Recovered, text = ~Country_Region, type = 'scatter', color='red', name='dots')
fig <- fig %>% add_trace(type = 'scatter', mode='text',text = ~Country_Region, name='text')
fig <- fig %>% layout(title = 'Recovered - Comfirmed by Country (Scatter Plot)',
                      xaxis = list(showgrid = TRUE),
                      yaxis = list(showgrid = TRUE))
fig

