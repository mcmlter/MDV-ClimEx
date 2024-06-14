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
      selectInput(inputId = "input.param", "Parameter Suite of Interest", 
                  choices = NULL),
      selectInput(inputId = "input.variable", "Variable of Interest", 
                  choices = NULL),
      selectInput(inputId = "input.timescale", "Timescale",
                  choices = c("Daily",
                              "Monthly",
                              "Seasonal")),
      radioButtons(inputId = "input.plotType", "Plot Type",
                  choices = c("Standard",
                              "Historical Comparison")),
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
                     plotlyOutput("standardPlot")
                   ),
                   tabPanel(
                     title = "Historical Comparison",
                     plotlyOutput("historicalPlot")
                   ),
                   tabPanel(
                     title = "Wind Rose",
                     plotOutput("windRosePlot")
                   )
                 )
        )
      )
      
            
    )
  ),
  # Footer
  hr(),
  p("Funding for this work was provided by several grants from the National Science Foundation to the McMurdo Dry Valleys Long Term Ecological Research (", 
    a(href="https://mcmlter.org/", "MCM LTER"), ") program, most recently #OPP-1637708 and #OPP-2224760. Source code is available on ",
    a(href="https://github.com/mcmlter/MDV-ClimEx", "GitHub"), ".", align="left", style = "font-size:11px; color = white")
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
  
  # Table of possible parameter suites and their laynames
  paramMatchTable <- data.frame(params = c("AIRT",
                                           "PRESSTA",
                                           "RH",
                                           "RADN",
                                           "WIND",
                                           "SOILT"),
                                names = c("Air Temperature",
                                          "Barometric Pressure",
                                          "Relative Humidity",
                                          "Solar Radiation",
                                          "Wind Direction and Speed",
                                          "Soil Temperature"))
  
  # Table of possible variables and their laynames 
  varMatchTable <- data.frame(vars = c("airt2m",
                                       "airt1m",
                                       "airt3m",
                                       "swradin",
                                       "swradout",
                                       "netrad",
                                       "rh2m",
                                       "rh1m",
                                       "rh3m",
                                       "wdir",
                                       "wspd",
                                       "lwradin2",
                                       "lwradout2",
                                       "rh",
                                       "par",
                                       "soilt0cm",
                                       "soilt5cm",
                                       "soilt10cm",
                                       "pressta"),
                           names = c("Air Temperature (°C) at 2m",
                                     "Air Temperature (°C) at 1m",
                                     "Air Temperature (°C) at 3m",
                                     "Incoming Shortwave Radiation (W/m^2)",
                                     "Outgoing Shortwave Radiation (W/m^2)",
                                     "Net Radiation (W/m^2)",
                                     "Relative Humidity (%) at 2m",
                                     "Relative Humidity (%) at 1m",
                                     "Relative Humidity (%) at 3m",
                                     "Wind Direction (° from north)",
                                     "Wind Speed (m/s)",
                                     "Incoming Longwave Radiation (W/m^2)",
                                     "Outgoing Longwave Radiation (W/m^2)	",
                                     "Relative Humidity (%)",
                                     "Photosynthetically Active Radiation (W/m^2)",
                                     "Soil Temperature at 0cm Depth (°C)",
                                     "Soil Temperature at 5cm Depth (°C)",
                                     "Soil Temperature at 10cm Depth (°C)",
                                     "Atmospheric Pressure (mb)")
  )
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #### Reactive Functions #### 
  
  # Get the list of parameter suites available from the chosen met station
  metParams <- reactive({
    a <- metMatchTable %>%  # Get met abbreviation from full name input's corresponding row in the match table
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    b <- word(read_data_entity_names(getParameterInfo(a)$parameterID)[,"entityName"], 2, sep = "_") # Read parameter IDs in a specified met
    c <- paramMatchTable %>% # Match the parameter IDs to layperson names
      filter(params %in% b,
             params != "WVAPD",
             params != "SOILM",
             params != "ONYXT",
             params != "PPT",
             params != "ICET") %>% 
      pull(names)
    return(c) # Return the names of parameter suites available from the input met
  })
  
  # Get variables contained in the file of the chosen parameter suite of the chosen met station
  
  # Read in the CSV of the corresponding met-parameter combination
  paramData <- reactive({
    met <- metMatchTable %>%  # Get met abbreviation from full name input's corresponding row in the match table
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    param <- paramMatchTable %>% 
      filter(names == input$input.param) %>% 
      pull(params)
    data <- read_csv(file = str_glue("../data/{met}/{met}_{param}/{met}_{param}.csv"))
    return(data)
  })
  
  # Get the laynames of the columns containing meteorological variables (the only numeric columns) from the rawData
  paramVars <- reactive({
    a <- paramData() %>% 
      select(where(is.double)) %>% 
      colnames()
    b <- varMatchTable %>% 
      filter(vars %in% a) %>% 
      pull(names)
    return(b)
  })
  
  # Get the abbreviation of the chosen input variable
  varAbv <- reactive({
    a <- varMatchTable %>% 
      filter(names == input$input.variable) %>% 
      pull(vars)
    return(a)
  })

  #### Event Observations ####
  
  # Update choices when a new met station is selected
  observeEvent(input$input.met, {
    updateSelectInput(inputId = "input.param", 
                      choices = metParams()) # Update the choices of available parameter suites using metParams()
    updateSelectInput(inputId = "input.variable", 
                      choices = paramVars()) # Update the choices of variables using paramVars()
    
  })
  

  # Update choices when a new parameter suite is selected
  observeEvent(input$input.param, {
    updateSelectInput(inputId = "input.variable", 
                      choices = paramVars())
  })
  
  
  # Update plot types when a variable is chosen
  
  observeEvent(input$input.variable, {
    # If wind direction or speed are selected, update the select plot type input to include "Wind Rose"
    if (input$input.variable == "Wind Direction (° from north)" | input$input.variable == "Wind Speed (m/s)") {
      updateRadioButtons(inputId = "input.plotType", 
                        choices = c("Standard",
                                    "Historical Comparison",
                                    "Wind Rose"))
    } else {
      # Otherwise, only show "Standard" and "Historical Comparison" plotting options
      updateRadioButtons(inputId = "input.plotType",
                        choices = c("Standard",
                                    "Historical Comparison"))
    }
  })
  
  
  # Pull in the cleaned data from the chosen meteorological station, parameter suite, variable for the standard plot
  
  standardData <- reactive({
    
    # Store chosen inputs as how they appear in the file name
    
    # Meteorological Station abbreviation
    met <- metMatchTable %>%
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    
    # Parameter suite abbreviation
    param <- paramMatchTable %>% 
      filter(names == input$input.param) %>% 
      pull(params)
    
    # Variable abbreviation
    varAbv <- varMatchTable %>% 
      filter(names == input$input.variable) %>% 
      pull(vars)
    
    # Choose which cleaned data to pull based on specified meteorological station, parameter suite, variable, and timescale for the comparison plot
    if (input$input.timescale == "Seasonal") {
      filePath <- str_glue("../data/{met}/{met}_{param}/{varAbv}/{met}.{varAbv}.seasonal.csv")
      req(file.exists(filePath))
      data <- read_csv(filePath)
    } else if (input$input.timescale == "Monthly") {
      filePath <- str_glue("../data/{met}/{met}_{param}/{varAbv}/{met}.{varAbv}.monthly.csv")
      req(file.exists(filePath))
      data <- read_csv(filePath)
    } else if (input$input.timescale == "Daily") {
      filePath <- str_glue("../data/{met}/{met}_{param}/{varAbv}/{met}.{varAbv}.daily.csv")
      req(file.exists(filePath))
      data <- read_csv(filePath)
    }
    
    return(data)
    
  })
  
  
  # Pull in the historical average data from the chosen meteorological station, parameter suite, variable, and timescale
  
  histData <- reactive({
    
    # Store chosen inputs as how they appear in the file name
    
    # Meteorological Station abbreviation
    met <- metMatchTable %>%
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    
    # Parameter Suite abbreviation
    param <- paramMatchTable %>% 
      filter(names == input$input.param) %>% 
      pull(params)
    
    # Variable abbreviation
    varAbv <- varMatchTable %>% 
      filter(names == input$input.variable) %>% 
      pull(vars)
    
    # Choose which cleaned data to pull based on specified meteorological station, parameter suite, variable, and timescale
    if (input$input.timescale == "Seasonal") {
      filePath <- str_glue("../data/{met}/{met}_{param}/{varAbv}/{met}.{varAbv}.seasonalHist.csv")
      req(file.exists(filePath))
      histData <- read_csv(filePath) 
    } else if (input$input.timescale == "Monthly") {
      filePath <- str_glue("../data/{met}/{met}_{param}/{varAbv}/{met}.{varAbv}.monthlyHist.csv")
      req(file.exists(filePath))
      histData <- read_csv(filePath)
    } else if (input$input.timescale == "Daily") {
      filePath <- str_glue("../data/{met}/{met}_{param}/{varAbv}/{met}.{varAbv}.dailyHist.csv")
      req(file.exists(filePath))
      histData <- read_csv(filePath)
    }
    return(histData)
    
  })
  
  # Get unique years contained in the chosen Standard dataset
  uniqYears <- reactive({
    
    # Pull Data
    data <- standardData()
    
    # Wait until data is loaded to proceed
    req(nrow(data) > 0)
    
    # Get Unique Years
    years <- unique(data$year)
    
    return(years)
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
  
  
  # Create reactive function to pull wind direction and wind speed data together
  windRoseData <- reactive({
    
    # Store chosen inputs as how they appear in the file name
    
    # Meteorological Station abbreviation
    met <- metMatchTable %>%
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    
    # Parameter Suite abbreviation (Always "WIND")
    param <- "WIND"
    
    # Variable abbreviations (Always "wspd" and "wdir")
    varSpd <- "wspd"
    varDir <- "wdir"
    
    spdData <- read_csv(str_glue("../data/{met}/{met}_{param}/{varSpd}/{met}.{varSpd}.daily.csv"))
    dirData <- read_csv(str_glue("../data/{met}/{met}_{param}/{varDir}/{met}.{varDir}.daily.csv"))
    
    # Join speed and direction data by date_time column
    data <- spdData %>% 
      select(date_time, wspd) %>% 
      right_join(dirData)
    
    # Return modified data frame
    return(data)
    
  })
  
  # Render Standard Plot of Chosen Data
  output$standardPlot <- renderPlotly({
    
    # Pull data
    data <- standardData() 
    
    # Wait until data is loaded to proceed
    req(nrow(data) > 0)
    
    # Store variable abbreviation
    varAbv <- varAbv()
    
    # Store the chosen timescale and the order of the time series. The order is needed to prevent alphabetical sorting of x axis values for yearmonth and yearSeason
    
    # If "Daily" timescale is chosen, the pertinent time column is "date_time".
    if(input$input.timescale == "Daily") {
      timescale <- "date_time"
      timeSeries <- "Daily"
      orderArray <-  data$date_time
      
      # If "Monthly" timescale is chosen, the pertinent time column is "yearmonth".
    } else if(input$input.timescale == "Monthly") {
      timescale <- "yearmonth"
      timeSeries <- "Monthly"
      orderArray <- data$yearmonth
      
      # If "Seasonal" timescale is chosen, the pertinent time column is "yearSeason".
    } else if(input$input.timescale == "Seasonal") {
      timescale <- "yearseason"
      timeSeries <- "Seasonal"
      orderArray <- data$yearseason
    }
    
    # Store variable actual name
    varName <- input$input.variable
    
    # Store met name
    metName <- input$input.met
    
    
    # Create title from varName and metName
    title <- str_glue("{timeSeries} {varName} at {metName}")
    
    # Create Plot
    plot_ly() %>% 
      
      # Add a line trace for the chosen variable over the chosen timescale 
      add_trace(x = data[[timescale]],
                y = data[[varAbv()]],
                type = "scatter",
                mode = "lines",
                name = varAbv,
                hovertemplate = '%{x}: %{y}') %>% 
      
      # Set plot, x-axis, and y-axis titles
      layout(
        title = title,
        xaxis = list(title = "Date",
                     type = "category",
                     categoryorder = "array",
                     categoryarray = orderArray,
                     nticks = 6),
        yaxis = list(title = varName),
        margin = list(l = 60, 
                      r = 50, 
                      t = 50, 
                      b = 70)
      )
  })
  
  
  # Render Historical Comparison Plot, comparing daily, seasonal, or monthly data with historical averages
  output$historicalPlot <- renderPlotly({
  
    # Pull standard data
    data <- standardData() 
    
    # Wait until standard data is loaded to proceed
    req(nrow(data) > 0)
    
    # Pull historical average data
    histAvgs <- histData()
    
    # Wait until historical data is loaded to proceed
    req(nrow(histAvgs) > 0)
    
    # Store variable abbreviation
    varAbv <- varAbv()
    
    # Store variable actual name
    varName <- input$input.variable
    
    # Store met name
    metName <- input$input.met
    
    # Store number of years included
    nVal <- length(uniqYears())
    
    # Store chosen comparison year
    chosenYear <- as.character(input$input.chosenYear)
    
    # Create title from varName and metName
    title <- str_glue("{varName} at {metName} during {chosenYear} vs Averages")
    
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
      
      # Arrange the historical average dataset in calendar order (need to do this because monthday is a character and the default is to go in alphabetical order)
      histAvgs <- histAvgs %>% 
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
      histAvgs <- histAvgs%>% 
        mutate(monthAbb = month.abb[month], .before = histAvg)
      
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
      histAvgs <- histAvgs %>% 
        arrange(match(season, seasonOrder))
    }
   
    # Wait until dataMatrix and histAvgs are loaded to proceed
    req(nrow(dataMatrix) > 0)
    req(nrow(histAvgs) > 0)
    
    # Create Plot
    plot_ly() %>%
      
      # Add a line trace for 3σ above historical average
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg + 3*histAvgs$sdVal,
                type = "scatter", 
                mode = "lines",
                line = list(color = 'transparent'),
                name = "3σ above historical average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 3σ below historical average
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg - 3*histAvgs$sdVal,
                type = "scatter", 
                mode = "lines", 
                fill = "tonexty", fillcolor='rgba(0,100,120,0.2)', line = list(color = 'transparent'),
                name = "3σ below historical average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 2σ above historical average
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg + 2*histAvgs$sdVal,
                type = "scatter", 
                mode = "lines",
                line = list(color = 'transparent'),
                name = "2σ above historical average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 2 σ below historical average
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg - 2*histAvgs$sdVal,
                type = "scatter", 
                mode = "lines", 
                fill = "tonexty", fillcolor='rgba(0,100,100,0.2)', line = list(color = 'transparent'),
                name = "2σ below historical average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 1 σ above historical average
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg + histAvgs$sdVal,
                type = "scatter", 
                mode = "lines", 
                line = list(color = 'transparent'),
                name = "1σ above historical average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 1σ below historical average
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg - histAvgs$sdVal,
                type = "scatter", 
                mode = "lines", 
                fill = "tonexty", fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                name = "1σ below historical average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for the historical average of the chosen variable over the chosen timescale
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg,
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
                  (dataMatrix[[chosenYear]] - histAvgs[["histAvg"]])
                  /histAvgs[["sdVal"]], 2),
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
             yaxis = list(title = varName),
             legend = list(text = "Chosen Year"),
             margin = list(l = 70, 
                           r = 50, 
                           t = 50, 
                           b = 70),
             hovermode = "x unified"
      )
    
  })
  
  # Render sigma definition
  
  output$sigma <- renderText({
    "σ is the lowercase Greek letter sigma, which stands for standard deviation."
  })

  
  # Render Wind Rose Plot
  
  output$windRosePlot <- renderPlot({
    
    # Store data
    data <- windRoseData()
    
    # Wait until data is loaded to proceed
    req(nrow(data) > 0)
    
    # Store variable abbreviation
    varAbv <- varAbv()
    
    # Store variable actual name
    varName <- input$input.varName
    
    # Store met name
    metName <- input$input.met
    
    
    # Create title from varName and metName
    title <- str_glue("Wind Speed and Direction at {metName}")
    
    # Create Wind Rose Plot
    windrose(
      speed = data$wspd,
      direction = data$wdir,
      col_pal = "YlGnBu",
      legend_title = "Wind Speed (m/s)")+
      theme_bw()+
      labs(title = title,
           y = "Proportion",
           x = NULL)+
      theme(plot.title = element_text(size = 16, hjust = 0.5))
    
    
  })

  # Render wind data info when wind rose is selected
  
  output$windDataInfo <- renderText(
    "The wind rose is presented as an aggregate of the entire existing record of wind speed and direction data at the chosen met station."
  )
  
  # Render "Use the tools in the top right corner of the plot to navigate, download, or revert plot. Click and drag to zoom in on data."
  
  output$zoom <- renderText(
    "Use the tools in the top right corner of the plot to navigate, download, or revert plot. Click and drag to zoom in on data."
  )
  
  
  
  
  
  # Test Outputs

  
  
  
   # Note extent of climate record to indicate what the n is
  
 # Use "if" statements to visualize different plots based on user input
}

# Run the application 
shinyApp(ui = ui, server = server)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



