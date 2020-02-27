library(shiny)
library(shinysuit)
library(highcharter)
library(dplyr)
# library(tidyr)
library(readr)

decoration_color <- "#2e96a5"
background_color <- "#ededed"

datasaurus <- read_delim("datasaurus.tsv", delim = "\t")
