\name{Shiny App for Vehicle History: Documentation}
\alias{Shiny App for Vehicle History: Documentation}

\title{Vehicle History}

\description{
The Vehicle History Shiny App is a web application designed to analyze and visualize GPS data for vehicles. 
This documentation provides an overview of the app's features, functionality, and usage instructions.
}

\details{
\strong{Features:}

\enumerate{
  \item Custom Data Upload
  \item Vehicle and Date Selection
  \item Distance and Average Speed by Date
  \item Map and Table View
}

\strong{Usage Instructions:}

\enumerate{
  \item Data Upload: Select any *.csv or *.xlsx file. Ensure that the file contains the necessary columns: "VEHICLE," "GPSDateTime," "X," "Y," "Distance," and "timesecs"
  \item Vehicle Selection: Select a vehicle to view vehicle history.
  \item Date Selection: Select the date of which the history should be shown.
  \item View Results: Press the 'Submit' button in order view the the results.
  \item Map View: Shows a map with points where GPS data was received. Toggle 'Route' in order to view a line representing the route travelled by the vehicle.
  \item Table View: Shows the data filtered by the selected vehicle and date.
}

\strong{Notes:}

\itemize{
  \item If the vehicle you're looking for isn't one of the choices, please ensure that the uploaded data contains that vehicle.
  \item An average speed threshold of 35 km/h has been set. When a vehicle has exceeded this threshold, a warning will be shown.
}

\strong{Dependencies:}

The Vehicle History Shiny App relies on the following R packages and libraries:

\itemize{
  \item leaflet
  \item tidyverse
  \item DT
  \item shiny
  \item shinydashboard
  \item readxl
  \item openxlsx
  \item shinyjs
  \item shinythemes
  \item shinyalert
  \item terra
}

\strong{Development and Maintenance:}

The app's source code is divided into three files:

\itemize{
  \item \code{app.R}: Main application script that combines the UI and server components.
  \item \code{ui.R}: User interface definition.
  \item \code{server.R}: Server-side logic and data processing.
}

\strong{Contact:}

For questions, issues, or support related to the Vehicle History Shiny App, please contact the developer at karla.oneale@opsi.co.za.
}

@keywords shiny app documentation

@name VehicleHistoryShinyAppDocumentation
@docType package

@author Karla O'Neale
