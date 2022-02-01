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

dstype2 <- pokemon %>%
  count(type_2, color_2) %>%
  ungroup() %>%
  mutate(type_2 = str_to_title(type_2)) %>%
  arrange(desc(n)) %>%
  mutate(
    type_2 = case_when(
      is.na(type_2) ~ "Unspecified",
      TRUE ~ type_2
    ),
    color_2 = case_when(
      is.na(color_2) ~ "#333333",
      TRUE ~ color_2
    )
  )

dtm <- pokemon %>%
  group_by(type_1, color_1) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate_if(is.character, str_to_title)
