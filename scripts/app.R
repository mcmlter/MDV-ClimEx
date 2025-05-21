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

#### Create cache directory if it does not exist ####
if (!file.exists("app_cache")) {
  dir.create(file.path("app_cache"))
}

#### Load Required Libraries ####
library(shiny)
library(tidyverse)
library(zoo)
library(EDIutils)
library(plotly)
library(openair)
library(shinyjs)

# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  # Application Theme
  theme = bslib::bs_theme(
    bootswatch = "darkly"),
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
      selectInput(inputId = "input.variable", "Variable of Interest", 
                  choices = NULL),
      radioButtons(inputId = "input.plotType", "Plot Type",
                   choices = c("Standard",
                               "Historical Comparison")),
      div(id = "timescaleDropdown",
          selectInput(inputId = "input.timescale", "Timescale",
                      choices = c("Daily", "Monthly", "Seasonal"))),
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
    "Data are available in the ", a(href="https://mcm.lternet.edu/meteorology-data-sets", "Environmental Data Initiative."),
    "Source code is available on ", a(href="https://github.com/mcmlter/MDV-ClimEx", "GitHub."), align="left", style = "font-size:11px; color = white")
)



#### Define server logic ####
server <- function(input, output, session) {
  
  
  ##### Functions #####
  ###### Function to get info about the chosen met-variable combination ######
  getmetVarInfo <- function(metID) {
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
  
  #### Match Tables ####
  
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
  varMatchTable <- data.frame(vars = c("airtemp_3m_degc",
                                       "bpress_mb",
                                       "rhh2o_3m_pct",
                                       "par_umolm2s1",
                                       "swradin_wm2",
                                       "swradout_wm2",
                                       "lwradin2_wm2",
                                       "lwradout2_wm2",
                                       "soiltemp1_0cm_degc",
                                       "soiltemp1_5cm_degc",
                                       "soiltemp1_10cm_degc",
                                       "wspd_ms",
                                       "wdir_deg"),
                              namesPlain = c("Air Temperature",
                                             "Atmospheric Pressure",
                                             "Relative Humidity",
                                             "Photosynthetically Active Radiation",
                                             "Incoming Shortwave Radiation",
                                             "Outgoing Shortwave Radiation",
                                             "Incoming Longwave Radiation",
                                             "Outgoing Longwave Radiation",
                                             "Soil Temperature at 0 cm",
                                             "Soil Temperature at 5 cm",
                                             "Soil Temperature at 10 cm",
                                             "Wind Speed",
                                             "Wind Direction"),
                              namesUnits = c("Air Temperature (°C)",
                                             "Atmospheric Pressure (mb)",
                                             "Relative Humidity (%)",
                                             "Photosynthetically Active Radiation (μmol/m^2/s^1)",
                                             "Incoming Shortwave Radiation (W/m^2)",
                                             "Outgoing Shortwave Radiation (W/m^2)",
                                             "Incoming Longwave Radiation (W/m^2)",
                                             "Outgoing Longwave Radiation (W/m^2)	",
                                             "Soil Temperature @ 0 cm (°C)",
                                             "Soil Temperature @ 5 cm (°C)",
                                             "Soil Temperature @ 10 cm (°C)",
                                             "Wind Speed (m/s)",
                                             "Wind Direction (° from north)"
                                             )
                              
  )
  
  # Replace units with LaTeX format for plotly
  varMatchTable$namesUnits <- sapply(varMatchTable$namesUnits, function(name) {
    gsub("(W/m\\^2)", "W*m<sup>-2</sup>", name)
    gsub("(m\\^3/m\\^3)", "m<sup>3</sup>/m<sup>3</sup>", name)
  })
  
  ##### Reactive Functions #####
  # Get variables contained in the file of the chosen met station
  
  # Read in the base data CSV of the corresponding met-variable combination
  baseData <- reactive({
    met <- metMatchTable %>%  # Get met abbreviation from full name input's corresponding row in the match table
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    timescale <- input$input.timescale
    data <- read_csv(file = str_glue("../data/met/{met}/{met}.{timescale}.csv"), show_col_types = FALSE)
    return(data)
  })
  
  # Read in the historical average data CSV of the corresponding met-variable combination
  histAvgData <- reactive({
    met <- metMatchTable %>%  # Get met abbreviation from full name input's corresponding row in the match table
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    timescale <- input$input.timescale
    data <- read_csv(file = str_glue("../data/met/{met}/{met}.{timescale}HistAvg.csv"), show_col_types = FALSE)
    return(data)
  })
  
  # Read in the historical standard deviation CSV of the corresponding met-variable combination
  histSDData <- reactive({
    met <- metMatchTable %>%  # Get met abbreviation from full name input's corresponding row in the match table
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    timescale <- input$input.timescale
    data <- read_csv(file = str_glue("../data/met/{met}/{met}.{timescale}HistSD.csv"), show_col_types = FALSE)
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
    req(input$input.variable)
    
    a <- varMatchTable %>% 
      filter(namesPlain == input$input.variable) %>% 
      pull(vars)
    return(a)
  })
  
  # Get unique years contained in the chosen dataset
  uniqYears <- reactive({
    
    # Pull Data
    data <- baseData()
    
    # Wait until data is loaded to proceed
    req(nrow(data) > 0)
    
    # Get Unique Years
    years <- unique(data$year)
    
    return(years)
  })


  
  ##### Event Observations #####
  
  # Update choices when a new met station + timescale is selected
  observeEvent(input$input.met, {
    updateSelectInput(inputId = "input.variable", 
                      choices = varNames()) # Update the choices of variables using paramVars()
    
  })
  
  # Update the radio buttons to include Wind Rose based on the selected variable
  observeEvent(input$input.variable, {
    if (input$input.variable %in% c("Wind Direction", "Wind Speed")) {
      updateRadioButtons(session, "input.plotType",
                         choices = c("Standard", "Historical Comparison", "Wind Rose"))
    } else {
      updateRadioButtons(session, "input.plotType",
                         choices = c("Standard", "Historical Comparison"))
    }
  })
  
  # Switch plot display tab depending on Plot Type Input AND Display historical comparison plot specifications if this type of plot is chosen
  # Observe the plot type input selection
  observeEvent(input$input.plotType, {
    
    # Display the plot tab that corresponds to the chosen plot type 
    updateTabsetPanel(inputId = "plotTabs", selected = input$input.plotType)
    
    # Display the additional selection dropdowns for the chosen plot type
    updateTabsetPanel(inputId = "plotSpecs", selected = input$input.plotType)
  })
  
  
  # Update the choices in the "Choose Year to Plot" dropdown based on what years are available in the dataset
  # Observe the plot type input selection
  observeEvent(input$input.plotType, {
    
    # Populate select input with unique years in chosen data
    updateSelectInput(inputId = "input.chosenYear",
                      choices = uniqYears())
  })
  
  # Hide or show the timescale dropdown based on the plot type
  observeEvent(input$input.plotType, {
    if (input$input.plotType == "Wind Rose") {
      updateSelectInput(session, "input.timescale", selected = "Daily")
      hide("timescaleDropdown")  # Hide the timescale dropdown
    } else {
      show("timescaleDropdown")  # Show the timescale dropdown
    }
  })

  
  ##### Render Plots #####
  
  ###### Standard Plot ######
  output$standardPlot <- renderPlotly({
    
    # Pull data
    data <- baseData() 
    
    # Wait until data is loaded to proceed
    req(nrow(data) > 0)
    
    # Store variable abbreviation
    varAbv <- varAbv()
    
    # Store the chosen timescale and the order of the time series. The order is needed to prevent alphabetical sorting of x axis values for yearmonth and yearSeason
    
    # If "Daily" timescale is chosen, the pertinent time column is "date_time".
    if(input$input.timescale == "Daily") {
      timescale <- "date_time"
      timeSeries <- "Daily"
      
      # If "Monthly" timescale is chosen, the pertinent time column is "yearmonth".
    } else if(input$input.timescale == "Monthly") {
      timescale <- "yearmonth"
      timeSeries <- "Monthly"
      orderArray <-  data$yearmonth
      
      # If "Seasonal" timescale is chosen, the pertinent time column is "yearSeason".
    } else if(input$input.timescale == "Seasonal") {
      timescale <- "yearseason"
      timeSeries <- "Seasonal"
      orderArray <-  data$yearseason
    }
    
    # Store variable actual name, minus the unit
    varTitle <-  input$input.variable
    
    # Store variable actual name, with the unit, for the axes
    varAxis <- varUnits()
    
    # Store met name
    metName <- input$input.met
    
    # Create title from varName and metName
    title <- str_glue("{timeSeries} {varTitle} at {metName}")
    
    # Create Plot
    # If daily timsecale is chosen, normal x axes formatting works.
    if(input$input.timescale == "Daily") {
      
      plot_ly() %>% 
        
        # Add a line trace for the chosen variable over the chosen timescale 
        add_trace(x = data[[timescale]],
                  y = data[[varAbv()]],
                  type = "scatter",
                  mode = "lines",
                  name = varAbv,
                  hovertemplate = '%{x}: %{y}') %>% 
        layout(
          title = title,
          xaxis = list(title = "Date"),
          yaxis = list(title = varAxis),
          margin = list(l = 60, 
                        r = 50, 
                        t = 50, 
                        b = 70)
        )
      
      # If monthly or seasonal timescales are chosen, categorical x axes formatting is needed
    } else {
      
      plot_ly() %>% 
        
        # Add a line trace for the chosen variable over the chosen timescale 
        add_trace(x = data[[timescale]],
                  y = data[[varAbv()]],
                  type = "scatter",
                  mode = "lines",
                  name = varAbv,
                  hovertemplate = '%{x}: %{y}') %>% 
        layout(
          title = title,
          xaxis = list(title = "Date",
                       type = "category",
                       categoryorder = "array",
                       categoryarray = orderArray,
                       nticks = 6),
          yaxis = list(title = varAxis),
          margin = list(l = 60, 
                        r = 50, 
                        t = 50, 
                        b = 70)
        )
    }
    
  })
  
  # Render Helper Text
  output$standardPlotText <- renderText({
    "Navigate, download, or reset the plot using the tools in the upper righthand corner. Click and drag to zoom in."
  })
  
  ###### Historical Comparison Plot ######
  
  output$historicalPlot <- renderPlotly({
    
    # Pull standard data
    data <- baseData() 
    
    # Wait until standard data is loaded to proceed
    req(nrow(data) > 0)
    
    # Pull historical average data
    histAvgData <- histAvgData()
    
    # Wait until historical average data is loaded to proceed
    req(nrow(histAvgData) > 0)
    
    # Pull in historical standard deviation data
    histSDData <- histSDData()
    
    # Wait until historical standard deviation data is loaded to proceed
    req(nrow(histSDData) > 0)
    
    
    # Store variable abbreviation
    varAbv <- varAbv()
    
    # Store variable actual name, minus the unit
    varTitle <-  input$input.variable
    
    # Store variable actual name, with the unit, for the axes
    varAxis <- varUnits()
    
    # Store met name
    metName <- input$input.met
    
    # Store number of years included
    nVal <- length(uniqYears())
    
    # Store chosen comparison year
    chosenYear <- as.character(input$input.chosenYear)
    
    # Create title from varName and metName
    title <- str_glue("{varTitle} at {metName} during {chosenYear} vs Averages")
    
    # Prepare data for plotting depending on user inputs
    
    # If "Daily" timescale is chosen, the pertinent time column is the corresponding date in 2020, "fakedate".
    if(input$input.timescale == "Daily") {
      
      # Set Timescale
      timescale <- "monthday"
      
      # Widen data to accommodate year-by-year plotting
      dataMatrix <- data %>% 
        arrange(date_time) %>% 
        pivot_wider(id_cols = c("fakedate", "monthday"), names_from = "year", values_from = varAbv) %>% 
        arrange(fakedate)
      
      # Store x axis title
      xAxisTitle <- "Day of the Year"
      
      # Store order of x axis values. In this case, it is the month and day.
      orderArray <- dataMatrix$monthday
      
      # Arrange the historical average and standard deviation datasets in calendar order (need to do this because monthday is a character and the default is to go in alphabetical order)
      histAvgData <- histAvgData %>% 
        arrange(match(monthday, orderArray))
      
      histSDData <- histSDData %>% 
        arrange(match(monthday, orderArray))
      
      # If "Monthly" timescale is chosen, the pertinent time column is the month abbreviation, "monthAbb".
    } else if(input$input.timescale == "Monthly") {
      
      # Set timescale
      timescale <- "monthAbb"
      
      # Widen data to accommodate year-by-year plotting
      dataMatrix <- data %>% 
        arrange(year) %>% 
        pivot_wider(id_cols = "month", names_from = "year", values_from = varAbv) %>% 
        arrange(month) %>% 
        mutate(monthAbb = month.abb[month], .after = month)
      
      # Store x axis title
      xAxisTitle <- "Month of the Year"
      
      # Store order of x axis values. In this case, it is the month abbreviation
      orderArray <- month.abb
      
      # Create month abbreviation column in histAvgs (PLACE IN metdatafetch.R LATER)
      histAvgData <- histAvgData%>% 
        mutate(monthAbb = month.abb[month], .after = month)
      
      # If "Seasonal" timescale is chosen, the pertinent time column is the name of the season, "season".
    } else if(input$input.timescale == "Seasonal") {
      
      # Set timescale
      timescale <- "season"
      
      # Store desired order of the seasons
      seasonOrder <- c("Winter", "Spring", "Summer", "Autumn")
      
      # Widen data to accommodate year-by-year plotting
      dataMatrix <- data %>% 
        arrange(year) %>% 
        pivot_wider(id_cols = "season", names_from = "year", values_from = varAbv) %>% 
        arrange(match(season, seasonOrder))
      
      # Store x axis title
      xAxisTitle <- "Austral Season of the Year"
      
      # Store order of x axis values. In this case, it is the order of the seasons. 
      orderArray <- seasonOrder
      
      # Arrange histAvgs entries in the following order: Winter, Spring, Summer, Autumn
      histAvgData <- histAvgData %>% 
        arrange(match(season, seasonOrder))
    }
    
    # Wait until dataMatrix, histAvgData, and histSDData are loaded to proceed
    req(nrow(dataMatrix) > 0)
    req(nrow(histAvgData) > 0)
    req(nrow(histSDData) > 0)
    
    # Create Plot
    plot_ly() %>%
      
      # Add a line trace for 3σ above historical average
      add_trace(x = histAvgData[[timescale]],
                y = histAvgData[[varAbv]] + 3*histSDData[[varAbv]],
                type = "scatter", 
                mode = "lines",
                line = list(color = 'transparent'),
                name = "3σ above historical average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 3σ below historical average
      add_trace(x = histAvgData[[timescale]],
                y = histAvgData[[varAbv]] - 3*histSDData[[varAbv]],
                type = "scatter", 
                mode = "lines", 
                fill = "tonexty", fillcolor='rgba(0,100,120,0.2)', line = list(color = 'transparent'),
                name = "3σ below historical average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 2σ above historical average
      add_trace(x = histAvgData[[timescale]],
                y = histAvgData[[varAbv]] + 2*histSDData[[varAbv]],
                type = "scatter", 
                mode = "lines",
                line = list(color = 'transparent'),
                name = "2σ above historical average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 2σ below historical average
      add_trace(x = histAvgData[[timescale]],
                y = histAvgData[[varAbv]] - 2*histSDData[[varAbv]],
                type = "scatter", 
                mode = "lines", 
                fill = "tonexty", fillcolor='rgba(0,100,100,0.2)', line = list(color = 'transparent'),
                name = "2σ below historical average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 1σ above historical average
      add_trace(x = histAvgData[[timescale]],
                y = histAvgData[[varAbv]] + histSDData[[varAbv]],
                type = "scatter", 
                mode = "lines", 
                line = list(color = 'transparent'),
                name = "1σ above historical average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 1σ below historical average
      add_trace(x = histAvgData[[timescale]],
                y = histAvgData[[varAbv]] - histSDData[[varAbv]],
                type = "scatter", 
                mode = "lines", 
                fill = "tonexty", fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                name = "1σ below historical average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for the historical average of the chosen variable over the chosen timescale
      add_trace(x = histAvgData[[timescale]],
                y = histAvgData[[varAbv]],
                type = "scatter", 
                line = list(color="black"),
                mode = "lines", 
                name = str_glue("Historical Average \n(n = {nVal} years)"),
                hovertemplate = '%{y}') %>% 
      
      # Add a line trace for the chosen variable over the chosen timescale during the year of choice
      add_trace(x = dataMatrix[[timescale]],
                y = dataMatrix[[chosenYear]],
                type = "scatter", 
                text = round(
                  (dataMatrix[[chosenYear]] - histAvgData[[varAbv]])
                  /histSDData[[varAbv]], 2),
                mode = "lines",
                line = list(color="brown"),
                name = chosenYear,
                hovertemplate = '%{y}, %{text}σ from the historical average') %>% 
      
      # Set plot, xaxis, and yaxis titles. Show legend. Create hover popup.
      layout(title = title,
             xaxis = list(title = xAxisTitle,
                          type = "category",
                          categoryorder = "array",
                          categoryarray = orderArray),
             yaxis = list(title = varAxis),
             legend = list(text = "Chosen Year"),
             margin = list(l = 70, 
                           r = 50, 
                           t = 50, 
                           b = 70),
             hovermode = "x unified"
      )
    
  })
  
  # Render helper text
  output$historicalPlotText <- renderText({
    "The historical plot compares the selected year's data to the historical average. Shaded areas denote ±1, ±2, and ±3 
    standard deviations (σ) from the mean. Navigate, download, or reset the plot using the tools in the upper righthand 
    corner. Click and drag to zoom in."
  })
  
  ###### Wind Rose Plot ######
  output$windRosePlot <- renderPlot({
    
    # Store data
    data <- baseData()
    
    # Ensure data is loaded and contains required columns
    req(nrow(data) > 0, "wspd_ms" %in% colnames(data), "wdir_deg" %in% colnames(data))
    
    # Store variable abbreviation
    varAbv <- varAbv()
    
    # Store variable actual name
    varName <- input$input.varName
    
    # Store met name
    metName <- input$input.met
    
    # Create title from varName and metName
    title <- str_glue("Wind Speed and Direction at {metName}")
    
    # Create Wind Rose Plot
    windRose(
      data,
      ws = "wspd_ms",
      wd = "wdir_deg",
      paddle = F,
      border = T,
      main = title
      )
    
  })
  
  # Render helper text
  output$windRosePlotText <- renderText({
    "The wind rose plot is presented as an aggregate of the entire record. Unlike other plot types, it does not allow for user interaction."
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
