# Content: load library, read data, create global object used by UI and Server.

# Load library
library(shiny)
library(htmlwidgets)
library(htmltools)
library(plotly)
library(lubridate)
library(wordcloud)
library(RColorBrewer)
library(tidyverse)
library(shinyBS)

# Read data
rs_data <- read_csv("rs.csv") |>
  select(time_period, traveller_cat, duration_stay, reason_journey, observation_value)

sc_data <- read_csv("sc.csv") |>
  select(time_period, traveller_cat, country_res_name, observation_value)

label_geo <- read_csv("label_geo.csv")

country_lookup <- read_csv("country_lookup.csv") |>
  rename(country_res_name = country) |>
  rename(iso3 = iso3c)

sc_data <- sc_data |>
  left_join(country_lookup, by = "country_res_name")

# Data formatting and splitting for the filters and visualisations
rs_data$year <- as.integer(substr(rs_data$time_period, 1, 4))
rs_data$month <- as.integer(substr(rs_data$time_period, 6, 7))
rs_data$period <- sprintf("%d-%02d", rs_data$year, rs_data$month)

sc_data$year <- as.integer(substr(sc_data$time_period, 1, 4))
sc_data$month <- as.integer(substr(sc_data$time_period, 6, 7))
sc_data$period <- sprintf("%d-%02d", sc_data$year, sc_data$month)

month_seq <- seq.Date(
  from = as.Date("2018-01-01"),
  to = max(as.Date(paste0(sc_data$year, "-", sprintf("%02d", sc_data$month), "-01")), na.rm = TRUE),
  by = "month"
)
