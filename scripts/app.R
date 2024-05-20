#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
  titlePanel(title = div(img(src = "https://mcm.lternet.edu/sites/default/files/MCM_white_logo60x50.png", height="5%", width = "5%"), 
                         "MCM-LTER data viewer")),
  helpText("McMurdo Dry Valleys Long-Term Ecological Research"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "input.met",
                  label = "Choose Met Station",
                  choices = list("Terrestrial Sites" = c("Explorers Cove",
                                                         "Lake Fryxell",
                                                         "Lake Hoare",
                                                         "Lake Bonney",
                                                         "Lake Brownworth",
                                                         "Lake Vanda",
                                                         "Lake Vida",
                                                         "Miers Valley"),
                                 "Glacial Sites" = c("Taylor Glacier",
                                                     "Canada Glacier",
                                                     "Commonwealth Glacier",
                                                     "Howard Glacier"),
                                 "High Altitude Sites" = c("Friis Hills",
                                                           "Mt. Fleming")),
                  selected = "Lake Bonney"),
      selectInput(inputId = "input.pack", "Package of Interest", 
                  choices = NULL, 
                  selected = "Air Temperature"),
      selectInput(inputId = "input.variable", "Variable of Interest", 
                  choices = NULL, 
                  selected = "Air Temperature (°C) at Three Meters"),
      selectInput(inputId = "input.timescale", "Timescale",
                  choices = c("Daily",
                              "Monthly",
                              "Seasonally"),
                  selected = "Daily"),
      radioButtons(inputId = "input.plotType", "Plot Type",
                  choices = c("Standard",
                              "Historical Comparison")),
      tabsetPanel(id = "plotSpecs",
                  type = "hidden",
                  tabPanel(title = "Standard"),
                  tabPanel(title = "Historical Comparison",
                           selectInput(inputId = "input.chosenYear", "Choose Year to Plot",
                                       choices = NULL)),
                  tabPanel(title = "Wind Rose",
                           textOutput("windDataInfo"))
      )
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
  p("This material is based upon work supported by the National Science Foundation under Cooperative Agreement #OPP-2224760 and  NSF OPP-1637708, MCM LTER. Any opinions, findings, conclusions, or recommendations expressed in the material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.", 
    align="left", style = "font-size:11px; color = white")
)




# Define server logic
server <- function(input, output) {
  
  
  
  #### Package Info Function ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  getPackageInfo <- function(metID) {
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
              "Mt. Fleming",
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
             packageID = paste(scope, identifier, revision, sep = '.'))
    
    
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
                                      "Mt. Fleming",
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
  
  # Table of possible packages and their laynames
  packMatchTable <- data.frame(packs = c("AIRT",
                                         "AIRT3ASP",
                                         "RADN",
                                         "SURFTEMP",
                                         "WIND",
                                         "ICET",
                                         "PRESSTA",
                                         "ICESURFCHANGE",
                                         "RH",
                                         "WVAPD",
                                         "PPT",
                                         "SOILM",
                                         "SOILT",
                                         "SNOWHT",
                                         "SURFCHANGE",
                                         "SURF",
                                         "UV",
                                         "ONYXT",
                                         "DEPTH",
                                         "ICESURF"),
                               names = c("Air Temperature",
                                         "Aspirated Air Temperature",
                                         "Solar Radiation",
                                         "Ice Surface Temp.",
                                         "Wind Direction and Speed",
                                         "Ice Temperature",
                                         "Barometric Pressure",
                                         "Ice Surface Change",
                                         "Relative Humidity",
                                         "Water Vapor Density",
                                         "Precipitation",
                                         "Soil Moisture",
                                         "Soil Temperature",
                                         "Snow Height",
                                         "Surface Elev. Change",
                                         "Surface Elevation Change",
                                         "Ultraviolet Radiation",
                                         "Onyx River Water Temperature",
                                         "Ice Surface Change",
                                         "Ice Surface Temperature"))
  
  # Table of possible variables and their laynames 
  varMatchTable <- data.frame(vars = c("airt2m",
                                       "airt1m",
                                       "airt3m",
                                       "airt3asp",
                                       "swradin",
                                       "swradout",
                                       "thmir",
                                       "netrad",
                                       "rh2m",
                                       "rh1m",
                                       "rh3m",
                                       "surftemp",
                                       "wdir",
                                       "wdirstd",
                                       "wspd",
                                       "wspdmax",
                                       "wspdmin",
                                       "airtmax",
                                       "airtmin",
                                       "icesurfchange_cm",
                                       "dist_to_surface",
                                       "lwradin",
                                       "lwradin2",
                                       "lwradout",
                                       "lwradout2",
                                       "rh",
                                       "airtdel3m1",
                                       "ppt",
                                       "par",
                                       "soilm",
                                       "soilt",
                                       "soilt0cm",
                                       "soilt5cm",
                                       "soilt10cm",
                                       "pressta",
                                       "snowht",
                                       "uva",
                                       "uvb",
                                       "surfchange_cm",
                                       "surfelevchange",
                                       "uv_avg",
                                       "uv_max",
                                       "uv_min",
                                       "voltage",
                                       "surf_change",
                                       "ONYXT",
                                       "WVAPD",
                                       "airt1_3m"),
                           names = c("Air Temperature (°C) at Two Meters",
                                     "Air Temperature (°C) at One Meter",
                                     "Air Temperature (°C) at Three Meters",
                                     "Aspirated Air Temperature (°C) at 3 Meters",
                                     "Incoming Short Wave Radiation (W/m^2)",
                                     "Outgoing Short Wave Radiation (W/m^2)",
                                     "Thermal infrared surface temperature (°C)",
                                     "Net Radiation (W/m^2)",
                                     "Relative Humidity (%) at 2 meters",
                                     "Relative Humidity (%) at 1 meters",
                                     "Relative Humidity (%) at 3 meters",
                                     "Ice Surface Temperature (°C)",
                                     "Wind Direction (° from north)",
                                     "Standard deviation of wind direction (° from north)",
                                     "Wind Speed (m/s)",
                                     "Maximum Wind Speed (m/s)",
                                     "Minimum Wind Speed (m/s)",
                                     "Maximum Air Temperature (°C)",
                                     "Minimum Air Temperature (°C)",
                                     "Ice Surface Change (cm)",
                                     "Distance from Sensor to Surface (m)",
                                     "Incoming Longwave Radiation (W/m^2)",
                                     "Incoming Longwave Radiation Method 2 (W/m^2)",
                                     "Outgoing Longwave Radiation (W/m^2)	",
                                     "Outgoing Longwave Radiation Method 2 (W/m^2)",
                                     "Relative Humidity (%)",
                                     "Difference Between 3m and 1m Air Temperature (°C)",
                                     "Unevaporated Precipitation (mm)",
                                     "Photosynthetically Active Radiation (W/m^2)",
                                     "Soil Moisture (%)",
                                     "Soil Temperature (°C)",
                                     "Soil Temperature at 0 cm Depth (°C)",
                                     "Soil Temperature at 5 cm Depth (°C)",
                                     "Soil Temperature at 10 cm Depth (°C)",
                                     "Atmospheric Pressure (mb)",
                                     "Snow Height (cm)",
                                     "Incoming Ultraviolet-A Radiation (W/m^2)",
                                     "Incoming Ultraviolet-A Radiation (W/m^2)",
                                     "Surface Height Change (cm)",
                                     "Surface Height Change (cm)",
                                     "Average UV (W/m^2)",
                                     "Maximum UV (W/m^2)",
                                     "Minimum UV (W/m^2)",
                                     "Voltage (V)",
                                     "Surface Height Change (cm)",
                                     "Onyx River Temperature (°C)",
                                     "Water Vapor Density (g/m^3)",
                                     "3m-1m Air Temperature Differential (°C)")
  )
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Get the list of packages available from the chosen met station
  metPacks <- reactive({
    a <- metMatchTable %>%  # Get met abbreviation from full name input's corresponding row in the match table
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    b <- word(read_data_entity_names(getPackageInfo(a)$packageID)[,"entityName"], 2, sep = "_") # Read package IDs in a specified met
    c <- packMatchTable %>% # Match the package IDs to layperson names
      filter(packs %in% b,
             packs != "WVAPD",
             packs != "ONYXT",
             packs != "PPT",
             packs != "ICET") %>% 
      pull(names)
    return(c) # Return the names of packages available from the input met
  })

  
  # Use metPacks() to update options in the Parameter of Choice Dropdown
  observeEvent(input$input.met, {
    updateSelectInput(inputId = "input.pack", choices = metPacks()) # Whenever the input met changes, update the choices of available packages using metPacks()
  })
  

  # Get variables contained in the file of the chosen parameter of the chosen met station
  
  # Read in the CSV of the corresponding met-package combination
  packData <- reactive({
    met <- metMatchTable %>%  # Get met abbreviation from full name input's corresponding row in the match table
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    pack <- packMatchTable %>% 
      filter(names == input$input.pack) %>% 
      pull(packs)
    data <- read_csv(file = str_glue("../data/met/{met}/{met}_{pack}/{met}_{pack}.csv"))
    return(data)
  })
  
  # Get the laynames of the columns containing meteorological variables (the only numeric columns) from the rawData
  packVars <- reactive({
    a <- packData() %>% 
      select(where(is.double)) %>% 
      colnames()
    b <- varMatchTable %>% 
      filter(vars %in% a) %>% 
      pull(names)
    return(b)
  })
  
  # Use varMatchTable and packVars to update options in the "Variable of Choice" dropdown with layperson terms for the available variables
  
  observeEvent(input$input.pack, {
    updateSelectInput(inputId = "input.variable", 
                      choices = packVars())
  })
  
  
  # Get the abbreviation of the chosen input variable
  varAbv <- reactive({
    a <- varMatchTable %>% 
         filter(names == input$input.variable) %>% 
         pull(vars)
    return(a)
  })
  
  # Use the chosen variable to update options in the "Plot Type" dropdown
  
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
  
  
  # Pull in the cleaned data from the chosen met, package, variable for the standard plot
  
  standardData <- reactive({
    
    # Store chosen inputs as how they appear in the file name
    
    # Met abbreviation
    met <- metMatchTable %>%
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    
    # Pack abbreviation
    pack <- packMatchTable %>% 
      filter(names == input$input.pack) %>% 
      pull(packs)
    
    # Variable abbreviation
    varAbv <- varMatchTable %>% 
      filter(names == input$input.variable) %>% 
      pull(vars)
    
    # Choose which cleaned data to pull based on specified met, pack, variable, and timescale for the comparison plot
    if (input$input.timescale == "Seasonally") {
      data <- read_csv(str_glue("../data/met/{met}/{met}_{pack}/{varAbv}/{met}.{varAbv}.seasonal.csv"))
    } else if (input$input.timescale == "Monthly") {
      data <- read_csv(str_glue("../data/met/{met}/{met}_{pack}/{varAbv}/{met}.{varAbv}.monthly.csv"))
    } else if (input$input.timescale == "Daily") {
      data <- read_csv(str_glue("../data/met/{met}/{met}_{pack}/{varAbv}/{met}.{varAbv}.daily.csv"))
    }
    
    return(data)
    
  })
  
  
  # Pull in the historical average data from the chosen met, package, variable, and timescale
  
  histData <- reactive({
    
    # Store chosen inputs as how they appear in the file name
    
    # Met abbreviation
    met <- metMatchTable %>%
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    
    # Pack abbreviation
    pack <- packMatchTable %>% 
      filter(names == input$input.pack) %>% 
      pull(packs)
    
    # Variable abbreviation
    varAbv <- varMatchTable %>% 
      filter(names == input$input.variable) %>% 
      pull(vars)
    
    # Choose which cleaned data to pull based on specified met, pack, variable, and timescale
    if (input$input.timescale == "Seasonally") {
      histData <- read_csv(str_glue("../data/met/{met}/{met}_{pack}/{varAbv}/{met}.{varAbv}.seasonalHist.csv")) 
    } else if (input$input.timescale == "Monthly") {
      histData <- read_csv(str_glue("../data/met/{met}/{met}_{pack}/{varAbv}/{met}.{varAbv}.monthlyHist.csv"))
    } else if (input$input.timescale == "Daily") {
      histData <- read_csv(str_glue("../data/met/{met}/{met}_{pack}/{varAbv}/{met}.{varAbv}.dailyHist.csv"))
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
    
    # Met abbreviation
    met <- metMatchTable %>%
      filter(mets == input$input.met) %>% 
      pull(metAbvs)
    
    # Pack abbreviation (Always "WIND")
    pack <- "WIND"
    
    # Variable abbreviations (Always "wspd" and "wdir")
    varSpd <- "wspd"
    varDir <- "wdir"
    
    spdData <- read_csv(str_glue("../data/met/{met}/{met}_{pack}/{varSpd}/{met}.{varSpd}.daily.csv"))
    dirData <- read_csv(str_glue("../data/met/{met}/{met}_{pack}/{varDir}/{met}.{varDir}.daily.csv"))
    
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
      
      # If "Seasonally" timescale is chosen, the pertinent time column is "yearSeason".
    } else if(input$input.timescale == "Seasonally") {
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
      
      # If "Seasonally" timescale is chosen, the pertinent time column is the name of the season, "season".
    } else if(input$input.timescale == "Seasonally") {
      
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
      
      # Add a line trace for 3 SD above historical average
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg + 3*histAvgs$sdVal,
                type = "scatter", 
                mode = "lines",
                line = list(color = 'transparent'),
                name = "3 SD Above Historical Average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 3 SD below historical average
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg - 3*histAvgs$sdVal,
                type = "scatter", 
                mode = "lines", 
                fill = "tonexty", fillcolor='rgba(0,100,120,0.2)', line = list(color = 'transparent'),
                name = "3 SD Below Historical Average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 2 SD above historical average
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg + 2*histAvgs$sdVal,
                type = "scatter", 
                mode = "lines",
                line = list(color = 'transparent'),
                name = "2 SD Above Historical Average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 2 SD below historical average
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg - 2*histAvgs$sdVal,
                type = "scatter", 
                mode = "lines", 
                fill = "tonexty", fillcolor='rgba(0,100,100,0.2)', line = list(color = 'transparent'),
                name = "2 SD Below Historical Average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 1 SD above historical average
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg + histAvgs$sdVal,
                type = "scatter", 
                mode = "lines", 
                line = list(color = 'transparent'),
                name = "1 SD Above Historical Average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for 1 SD below historical average
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg - histAvgs$sdVal,
                type = "scatter", 
                mode = "lines", 
                fill = "tonexty", fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                name = "1 SD Below Historical Average",
                showlegend = FALSE,
                hovertemplate = '%{y}') %>%
      
      # Add a line trace for the historical average of the chosen variable over the chosen timescale
      add_trace(x = histAvgs[[timescale]],
                y = histAvgs$histAvg,
                type = "scatter", 
                line = list(color="black"),
                mode = "lines", 
                name = "Historical Average",
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
                hovertemplate = '%{y}, %{text} SDs from the historical average') %>% 
      
     
      
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
  
  # Render "map coming soon" text in map tab
  
  output$comingSoon <- renderText(
    "Map Coming Soon!"
  )
  
  
  
  # Test Outputs

  
  
  
   # Note extent of climate record to indicate what the n is
  
 # Use "if" statements to visualize different plots based on user input
}

# Run the application 
shinyApp(ui = ui, server = server)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



