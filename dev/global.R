library(shiny)
library(shinysuit)
library(highcharter)
library(dplyr)
# library(tidyr)
library(readr)

color <- "foobar"

datasaurus <- read_delim("datasaurus.tsv", delim = "\t")
