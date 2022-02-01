library(shiny)
library(shinybird)
library(shinydashboard)
library(dplyr)
library(stringr)
library(purrr)
library(highcharter)

pokemon <- readRDS("pokemon.rds")

dstype <- pokemon %>%
  count(type_1, color_1) %>%
  ungroup() %>%
  mutate(type_1 = str_to_title(type_1)) %>%
  arrange(desc(n))

dtm <- pokemon %>%
  group_by(type_1, color_1) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate_if(is.character, str_to_title)
