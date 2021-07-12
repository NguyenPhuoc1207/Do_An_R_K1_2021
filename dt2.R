library(tidyverse)
library(lubridate)

theme_set(theme_minimal())

covid19_raw <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

covid19 <- covid19_raw %>%
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long),
               names_to = "date",
               values_to = "confirmed_n"
  ) %>%
  select(-c(Lat, Long)) %>%
  rename(
    province_state = `Province/State`,
    country_region = `Country/Region`
  ) %>%
  mutate(date = mdy(date)) %>%
  group_by(country_region, date) %>%
  summarise(confirmed_n = sum(confirmed_n)) %>%
  ungroup()

covid19 <- covid19 %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(new_cases_n = confirmed_n - lag(confirmed_n, default = 0)) %>%
  ungroup()
# Cái thu 7 ve bieu do geom_line cua US
covid19 %>%
  filter(country_region == "US") %>%
  ggplot(aes(x = date, y = new_cases_n)) +
  geom_line() +
  scale_x_date(date_breaks = "6 weeks", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Date", y = "New cases",
    title = "New confirmed COVID-19 cases in the United States"
  )
# cái thu 8 ve bieu do geom_line cuaa VietNam
covid19 %>%
  filter(country_region == "Vietnam") %>%
  ggplot(aes(x = date, y = new_cases_n)) +
  geom_line() +
  scale_x_date(date_breaks = "6 weeks", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Date", y = "New cases",
    title = "New confirmed COVID-19 cases in  VietNam"
  )
# cái thu 9 ve bieu do cua nuoc Japan

covid19 %>%
  filter(country_region == "Japan") %>%
  ggplot(aes(x = date, y = new_cases_n)) +
  geom_line() +
  scale_x_date(date_breaks = "6 weeks", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Date", y = "New cases",
    title = "New confirmed COVID-19 cases in Janpan"
  )
# cái thu 10 bieu do truong hop các nuoc trên the gioi
covid19 %>%
  filter(country_region %in% c("US","Japan","Australia","Vietnam")) %>%
  ggplot(aes(x = date, y = new_cases_n, color = country_region)) +
  geom_line(show.legend = FALSE) +
  scale_x_date(date_breaks = "6 weeks", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~country_region, ncol = 1, scales = "free_y") +
  labs(
    x = "Date", y = "New cases",
    title = "New confirmed COVID-19 cases in Australia & United States & VietNam & Japan"
  )


covid19_raw <- covid19_raw[,1:150]

