ui <- shinyUI({
  
  header <- dashboardHeader(
    title = "Vehicle History"
  )
  
  sidebar <- dashboardSidebar(
    useShinyjs(),
    
    shinyDashboardThemes("grey_dark"),
    
    fileInput('gps_file','Upload Data:', multiple = FALSE, accept = c('.csv', '.xlsx')),
    uiOutput('vehicles'),
    uiOutput('dates'),
    div(
      uiOutput('submit'),
      align = "center"
    ),
    div(class="textOutput",
      textOutput('hint')
    )
    
    
  )
  
  body <- dashboardBody(
    id = "body_content",
                        
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css",  href = "style.css")
      ),
      fluidRow(
        valueBoxOutput("distance", width = 6),
        valueBoxOutput("speed", width = 6)
      ),
      fluidRow(
        tabBox(id = "plot_box",
            tabPanel("Map View",leafletOutput("plot")),
            tabPanel("Table View",
               DT::dataTableOutput("table")),
               width = 12
        )
      )
  )
  
  ui <- dashboardPage(header, sidebar, body)
  
})