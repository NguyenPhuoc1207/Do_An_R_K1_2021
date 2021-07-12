library(readxl)
data.covid <- COVID_19

names(data.covid)

country = data.covid$countriesAndTerritories
unique(country)

data.india = data.covid[country == "India", ]
data.us = data.covid[country == "United_States_of_America", ]

dim(data.india)
dim(data.us)


library(lubridate)

dates.india = make_date(year = data.india$year,
                        month = data.india$month,
                        day = data.india$day)

dates.us = make_date(year = data.us$year,
                     month = data.us$month,
                     day = data.us$day)

cases.india = data.india$cases
cases.us = data.us$cases

ignore.date = which(dates.us == setdiff(dates.us, dates.india))

data.us = data.us[-ignore.date, ]
dates.us = dates.us[-ignore.date]
cases.us = cases.us[-ignore.date]

dim(data.india)
dim(data.us)

covid.table = data.frame(dates = dates.india,
                         cases.india = cases.india,
                         cases.us = cases.us)

range(covid.table$dates)

ignore.cases = which((year(covid.table$dates) == 2019)|(month(covid.table$dates) "REPLACE BY LESS THAN SYMBOL" 3))

covid.table = covid.table[-ignore.cases, ]

head(covid.table)
tail(covid.table)
library(ggplot2)
#28
plott1 <- ggplot(covid.table) +
  geom_line(aes(x = dates, y = cases.india / 1e3))
#ggsave(p, filename = "gganimate_single_ts1.pdf", height = 12, width = 18)
plott1
plott2 <- ggplot(covid.table) +
  geom_line(aes(x = dates, y = cases.india / 1e3), color = "darkgreen", size = 2)
#ggsave(p, filename = "gganimate_single_ts2.pdf", height = 12, width = 18)
plott2
plott3 <- ggplot(covid.table) +
  geom_line(aes(x = dates, y = cases.india / 1e3), color = "darkgreen", size = 2) +
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40))
#ggsave(p, filename = "gganimate_single_ts3.pdf", height = 12, width = 18)
plott3
plott4 <- ggplot(covid.table) +
  geom_line(aes(x = dates, y = cases.india / 1e3), color = "darkgreen", size = 2) +
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40)) +
  xlab(NULL) + ylab("Daily Covid-19 cases (in thousands)")
#ggsave(p, filename = "gganimate_single_ts4.pdf", height = 12, width = 18)
plott4
plott5 <- ggplot(covid.table) +
  geom_line(aes(x = dates, y = cases.india / 1e3), color = "darkgreen", size = 2) +
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40)) +
  xlab(NULL) + ylab("Daily Covid-19 cases (in thousands)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b")
#ggsave(p0, filename = "gganimate_single_ts5.pdf", height = 12, width = 18)
plott5
library(gganimate)

plott6 <- plott6 + transition_reveal(dates) +
  labs(title = 'Date: {frame_along}') +
  theme(plot.title = element_text(hjust = 0.5, size = 40))
plott6
library(av)

b = animate(p, duration = 20, fps = 20, height = 1080, width = 1920,
            renderer = av_renderer())
b
#anim_save("covid_india.mp4", b)
#29 So sanh so ca nhiem o An DO so voi My
pt = ggplot(covid.table) +
  geom_line(aes(x = dates, y = cases.india / 1e3)) +
  geom_line(aes(x = dates, y = cases.us / 1e3))
pt
#ggsave(p, filename = "gganimate_mult_ts1.pdf", height = 12, width = 18)

colors = c("???n D???", "M???")

pt1 = ggplot(covid.table) +
  geom_line(aes(x = dates, y = cases.india / 1e3, color = colors[1]), size = 2) +
  geom_line(aes(x = dates, y = cases.us / 1e3, color = colors[2]), size = 2)
#ggsave(p, filename = "gganimate_mult_ts2.pdf", height = 12, width = 18)
pt1
pt2 = ggplot(covid.table) +
  geom_line(aes(x = dates, y = cases.india / 1e3, color = colors[1]), size = 2) +
  geom_line(aes(x = dates, y = cases.us / 1e3, color = colors[2]), size = 2) +
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40))
#ggsave(p, filename = "gganimate_mult_ts3.pdf", height = 12, width = 18)
pt2
pt3 = ggplot(covid.table) +
  geom_line(aes(x = dates, y = cases.india / 1e3, color = colors[1]), size = 2) +
  geom_line(aes(x = dates, y = cases.us / 1e3, color = colors[2]), size = 2) +
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40)) +
  xlab(NULL) + ylab("Daily Covid-19 cases (in thousands)")
#ggsave(p, filename = "gganimate_mult_ts4.pdf", height = 12, width = 18)
pt3
pt02 = ggplot(covid.table) +
  geom_line(aes(x = dates, y = cases.india / 1e3, color = colors[1]), size = 2) +
  geom_line(aes(x = dates, y = cases.us / 1e3, color = colors[2]), size = 2) +
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40)) +
  xlab(NULL) + ylab("Daily Covid-19 cases (in thousands)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b")
#ggsave(p0, filename = "gganimate_mult_ts5.pdf", height = 12, width = 18)
pt02
pt03 = pt3 + theme(legend.text=element_text(size=40),
               legend.title = element_text(size=40, hjust = 0.5),
               legend.key.width = unit(3,"cm"),
               legend.position = c(0.2, 0.8))
#ggsave(p, filename = "gganimate_mult_ts6.pdf", height = 12, width = 18)
pt03
pt04 = pt02 + theme(legend.text=element_text(size=40),
                legend.title = element_text(size=40, hjust = 0.5),
                legend.key.width = unit(3,"cm"),
                legend.position = c(0.2, 0.8)) +
  scale_color_identity(breaks = colors,
                       guide = "legend",
                       name = "Country",
                       labels = c("India", "US"))
#ggsave(p1, filename = "gganimate_mult_ts7.pdf", height = 12, width = 18)
pt04
pt05 = pt04 + transition_reveal(dates) +
  labs(title = 'Date: {frame_along}') +
  theme(plot.title = element_text(hjust = 0.5, size = 40)) +
  shadow_mark(size = 1, color = 'grey')
pt05
library(gganimate)

b = animate(p, duration = 20, fps = 20, height = 1080, width = 1920,
            renderer = av_renderer())
anim_save("covid_both.mp4", b)

