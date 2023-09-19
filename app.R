library(leaflet)
library(tidyverse)
library(DT)
library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(openxlsx)
library(shinyjs)
library(shinythemes)
library(shinyalert)
library(terra)

source('ui.R')
source('server.R')

shinyApp(
  ui = ui,
  server = server
)