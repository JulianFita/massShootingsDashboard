# Load packages
library(tidyverse)
library(janitor)
library(lubridate)
library(highcharter)
library(shiny)
library(shinydashboard)

# Load datasets
massShooting2018 <- read.csv('shootings_2018.csv')
massShooting2019 <- read.csv('shootings_2019.csv')
massShooting2020 <- read.csv('shootings_2020.csv')
massShooting2021 <- read.csv('shootings_2021.csv')
massShooting2022 <- read.csv('shootings_2022.csv')

# Merge datasets
massShootings <- rbind(massShooting2018,
                       massShooting2019,
                       massShooting2020,
                       massShooting2021,
                       massShooting2022)
# Clean
massShootings.clean <- massShootings %>%
  clean_names() %>%
  mutate(date = dmy(date))

# Have any duplicated or NA value ?
anyDuplicated(massShootings.clean)
sum(is.na(massShootings.clean))

massShootings.order <- massShootings.clean %>%
  group_by(date, state) %>%
  summarise(dead = sum(dead),
            injured = sum(injured),
            total = sum(total),
            description, .groups = 'drop')

# Select Input
years <- massShootings.order %>%
  sample_frac(1) %>%
  select(date) %>%
  mutate(date = year(date)) %>%
  arrange(date)

# Plot theme
hc_my_theme <- hc_theme_merge(hc_theme_flatdark(),
                              hc_theme(chart = list(backgroundColor = '#242f39'),
                                       subtitle = list(style = list(color = '#a7a5a5'))))                                       
                                       
