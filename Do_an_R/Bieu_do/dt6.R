library(tidyverse)
library(lubridate)

theme_set(theme_minimal())
covid_global_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/04c9ce883a9f29704d463f00f5d3a82f019b01e4/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

covid_global_cases <- covid_global_cases %>%
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = "date", values_to = "confirmed_cases_n") %>%
  rename(
    province_state = `Province/State`,
    country_region = `Country/Region`,
    latitude = Lat,
    longitud = Long
  )

covid_global_cases <- covid_global_cases %>%
  mutate(date = mdy(date))

covid_global_cases <- covid_global_cases %>%
  group_by(country_region, date) %>%
  summarise(confirmed_cases_n = sum(confirmed_cases_n)) %>%
  ungroup()
# Bieuu do thu 26 ve geom_line cua nuoc Mi
covid_global_cases %>%
  filter(country_region == "US") %>%
  ggplot(aes(x = date, y = confirmed_cases_n)) +
  geom_line() +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = "Confirmed cases (n)", title = "Number of confirmed COVID-19 cases in the United States in 2020")
# Bieu do thu 27 ve geom_line so sánh giua nuoc My và nuoc Úc
covid_global_cases %>%
  filter(country_region %in% c("US", "Australia")) %>%
  ggplot(aes(x = date, y = confirmed_cases_n, color = country_region)) +
  geom_line(show.legend = FALSE) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = "Confirmed cases (n)", title = "Number of confirmed COVID-19 cases in 2020") +
  facet_wrap(~country_region, scales = "free", ncol = 1)
