#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Authors: Gavin P. Wagner (gavin.wagner@rutgers.edu), with contributions from Renée F. Brown (rfbrown@unm.edu)
#

#### Clear environment ####
rm(list=ls(all=TRUE))


#### Load Required Libraries ####
library(shiny)
library(tidyverse)
library(zoo)
library(EDIutils)
library(plotly)
library(clifro)

# Define UI for application
ui <- fluidPage(
  
  # Application Theme
  theme = bslib::bs_theme(
    bootswatch = "superhero"),
  # Application Title
  titlePanel(title = div(img(src = "https://mcm.lternet.edu/sites/default/files/MCM_bigger_better_white.png", height="5%", width = "5%"), "ClimEx Data Viewer")),
  helpText("McMurdo Dry Valleys LTER Climate Extremes Data Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "input.met",
                  label = "Meteorological Station",
                  choices = list("Valley Floor Sites" = c("Explorers Cove",
                                                          "Lake Fryxell",
                                                          "Lake Hoare",
                                                          "Lake Bonney",
                                                          "Lake Brownworth",
                                                          "Lake Vanda",
                                                          "Lake Vida",
                                                          "Miers Valley"),
                                 "Glacial Sites" = c("Commonwealth Glacier",
                                                     "Canada Glacier",
                                                     "Howard Glacier",
                                                     "Taylor Glacier"),
                                 "High Altitude Sites" = c("Friis Hills",
                                                           "Mount Fleming"))),
      radioButtons(inputId = "input.plotType", "Plot Type",
                   choices = c("Standard",
                               "Historical Comparison")),
      selectInput(inputId = "input.variable", "Variable of Interest", 
                  choices = NULL),
      selectInput(inputId = "input.timescale", "Timescale",
                  choices = c("Daily",
                              "Monthly",
                              "Seasonal")),
      uiOutput("timescaleUI"),
      tabsetPanel(id = "plotSpecs",
                  type = "hidden",
                  tabPanel(title = "Standard"),
                  tabPanel(title = "Historical Comparison",
                           selectInput(inputId = "input.chosenYear", "Choose Year to Plot",
                                       choices = NULL),
                           textOutput("sigma")),
                  tabPanel(title = "Wind Rose",
                           textOutput("windDataInfo"))),
      textOutput("zoom")
    ),
    
    
    # Show the tabs
    mainPanel(
      tabsetPanel(
        id = "mainTabs",
        type = "tabs",
        tabPanel("Plot",
                 tabsetPanel(
                   id = "plotTabs",
                   type = "hidden",
                   tabPanel(
                     title = "Standard",
                     plotlyOutput("standardPlot"),
                     textOutput("standardPlotText")
                   ),
                   tabPanel(
                     title = "Historical Comparison",
                     plotlyOutput("historicalPlot"),
                     textOutput("historicalPlotText")
                   ),
                   tabPanel(
                     title = "Wind Rose",
                     plotOutput("windRosePlot"),
                     textOutput("windRosePlotText")
                   )
                 )
        )
      )
      
      
    )
  ),
  # Footer
  hr(),
  p("Funding for this work was provided by several grants from the National Science Foundation to the McMurdo Dry Valleys Long Term Ecological Research",  
    "(MCM LTER) program, most recently #OPP-1637708 and #OPP-2224760. ",
    "Data are available in the ", a(href="https://mcm.lternet.edu/meteorology-data-sets", "MCM LTER Data Catalog."),
    "Source code is available on ", a(href="https://github.com/mcmlter/MDV-ClimEx", "GitHub."), align="left", style = "font-size:11px; color = white")
)



# Define server logic
server <- function(input, output) {
  
  
  #### Parameter Info Function ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  getParameterInfo <- function(metID) {
    scope = "knb-lter-mcm"
    metInfo <- data.frame(
      met = c("Canada Glacier",
              "Commonwealth Glacier",
              "Explorers Cove",
              "Friis Hills",
              "Howard Glacier",
              "Lake Bonney",
              "Lake Brownworth",
              "Lake Fryxell",
              "Lake Hoare",
              "Lake Vanda",
              "Lake Vida",
              "Miers Valley",
              "Mount Fleming",
              "Taylor Glacier"),
      metAbv = c("CAAM",
                 "COHM",
                 "EXEM",
                 "FRSM",
                 "HODM",
                 "BOYM",
                 "BRHM",
                 "FRLM",
                 "HOEM",
                 "VAAM",
                 "VIAM",
                 "MISM",
                 "FLMM",
                 "TARM"),
      identifier = c(
        "7006",
        "7007",
        "7008",
        "7017",
        "7012",
        "7003",
        "7005",
        "7010",
        "7011",
        "7015",
        "7016",
        "7020",
        "7018",
        "7013")
    )
    metInfo <- metInfo %>%
      filter(metAbv == metID) %>%
      mutate(scope = scope, .before = "identifier") %>%
      mutate(revision = list_data_package_revisions(scope,
                                                    identifier,
                                                    filter = "newest"),
             parameterID = paste(scope, identifier, revision, sep = '.'))
    
    return(metInfo)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Table of Full Met Station Names and their Corresponding 4-letter Abbreviations
  metMatchTable <- data.frame(mets = c("Canada Glacier",
                                       "Commonwealth Glacier",
                                       "Explorers Cove",
                                       "Friis Hills",
                                       "Howard Glacier",
                                       "Lake Bonney",
                                       "Lake Brownworth",
                                       "Lake Fryxell",
                                       "Lake Hoare",
                                       "Lake Vanda",
                                       "Lake Vida",
                                       "Miers Valley",
                                       "Mount Fleming",
                                       "Taylor Glacier"),
                              metAbvs = c("CAAM",
                                          "COHM",
                                          "EXEM",
                                          "FRSM",
                                          "HODM",
                                          "BOYM",
                                          "BRHM",
                                          "FRLM",
                                          "HOEM",
                                          "VAAM",
                                          "VIAM",
                                          "MISM",
                                          "FLMM",
                                          "TARM"))
  
  # Table of possible variables, their laynames, and their laynames with units
  varMatchTable <- data.frame(vars = c("airtemp_1m_degc",
                                       "airtemp_3m_degc",
                                       "swradin_wm2",
                                       "swradout_wm2",
                                       "rhh2o_3m_pct",
                                       "wspd_ms",
                                       "wdir_deg",
                                       "lwradin2_wm2",
                                       "lwradout2_wm2",
                                       "par_umolm2s1",
                                       "soiltemp1_0cm_degc",
                                       "soiltemp1_5cm_degc", # Use soiltemp1 or soiltemp 2?
                                       "soiltemp1_10cm_degc",
                                       "bpress_mb"),
                              namesPlain = c("Air Temperature at 1m",
                                             "Air Temperature at 3m",
                                             "Incoming Shortwave Radiation",
                                             "Outgoing Shortwave Radiation",
                                             "Relative Humidity at 3m",
                                             "Wind Speed",
                                             "Wind Direction",
                                             "Incoming Longwave Radiation",
                                             "Outgoing Longwave Radiation",
                                             "Photosynthetically Active Radiation",
                                             "Soil Temperature at 0cm Depth",
                                             "Soil Temperature at 5cm Depth",
                                             "Soil Temperature at 10cm Depth",
                                             "Atmospheric Pressure"),
                              namesUnits = c("Air Temperature (°C) at 1m",
                                             "Air Temperature (°C) at 3m",
                                             "Incoming Shortwave Radiation (W/m^2)",
                                             "Outgoing Shortwave Radiation (W/m^2)",
                                             "Relative Humidity (%) at 3m",
                                             "Wind Speed (m/s)",
                                             "Wind Direction (° from north)",
                                             "Incoming Longwave Radiation (W/m^2)",
                                             "Outgoing Longwave Radiation (W/m^2)	",
                                             "Photosynthetically Active Radiation (W/m^2)",
                                             "Soil Temperature at 0cm Depth (°C)",
                                             "Soil Temperature at 5cm Depth (°C)",
                                             "Soil Temperature at 10cm Depth (°C)",
                                             "Atmospheric Pressure (mb)")
                              
  )
  
  # Replace units with LaTeX format for plotly
  varMatchTable$namesUnits <- sapply(varMatchTable$namesUnits, function(name) {
    gsub("(W/m\\^2)", "W*m<sup>-2</sup>", name)
  })
  
  
  # Get variables contained in the file of the chosen parameter suite of the chosen met station
  
  # Read in the base data CSV of the corresponding met-parameter combination
  baseData <- reactive({
    met <- metMatchTable %>%  # Get met abbreviation from full name input's corresponding row in the match table
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    var <- varMatchTable %>% 
      filter(names == input$input.variable) %>% 
      pull(params)
    timescale <- input$input.timescale
    baseData <- read_csv(file = str_glue("../data/met/{met}/{met}.{timescale}.csv"))
    histAvgData <- read_csv(file = str_glue("../data/met/{met}/{met}.{timescale}HistAvg.csv"))
    histSDData <- read_csv(file = str_glue("../data/met/{met}/{met}.{timescale}HistSD.csv"))
    return(data)
  })
  
  # Read in the historical average data CSV of the corresponding met-parameter combination
  histAvgData <- reactive({
    met <- metMatchTable %>%  # Get met abbreviation from full name input's corresponding row in the match table
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    var <- varMatchTable %>% 
      filter(names == input$input.variable) %>% 
      pull(params)
    timescale <- input$input.timescale
    data <- read_csv(file = str_glue("../data/met/{met}/{met}.{timescale}HistAvg.csv"))
    return(data)
  })
  
  # Read in the historical standard deviation CSV of the corresponding met-parameter combination
  histSDData <- reactive({
    met <- metMatchTable %>%  # Get met abbreviation from full name input's corresponding row in the match table
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    var <- varMatchTable %>% 
      filter(names == input$input.variable) %>% 
      pull(params)
    timescale <- input$input.timescale
    data <- read_csv(file = str_glue("../data/met/{met}/{met}.{timescale}HistSD.csv"))
    return(data)
  })
  
  # Get the plain laynames of the columns containing meteorological variables (the only numeric columns) from the base data
  varNames <- reactive({
    a <- baseData() %>% 
      select(where(is.double)) %>% 
      colnames()
    b <- varMatchTable %>% 
      filter(vars %in% a) %>% 
      pull(namesPlain)
    return(b)
  })
  
  # Get the unit-containing laynames of the chosen input variable
  varUnits <- reactive({
    a <- varMatchTable %>% 
      filter(namesPlain == input$input.variable) %>% 
      pull(namesUnits)
    return(a)
  })
  
  # Get the abbreviation of the chosen input variable
  varAbv <- reactive({
    a <- varMatchTable %>% 
      filter(namesPlain == input$input.variable) %>% 
      pull(vars)
    return(a)
  })
  
  #### Event Observations ####
  
  # Update choices when a new met station is selected
  observeEvent(input$input.met, {
    updateSelectInput(inputId = "input.variable", 
                      choices = varNames()) # Update the choices of variables using paramVars()
    
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
