#### PURPOSE: PART 1 ####
# The first part of this script compiles the most recent unique identifiers for high frequency meteorological data 
# collected by met stations at the McMurdo Dry Valleys Long Term Ecological Research project. The met data is housed 
# at the Environmental Data Initiative repository, and can be accessed using the EDIutils package. In order to pull 
# the most recent data, the most recent unique entityID-paramID string for each station-parameter combination must 
# be accessible. 
#
# The result of this script is a csv file that specifies the most recent entity IDs for each station 
# and the names for the meteorological variables stored within. Later, when data is fetched from EDI, this 
# csv will be referenced. A function, getEntityInfo is also created that outputs the latest entity ID and data 
# available for a given EDI entity.


#### PURPOSE: PART 2 ####
# The second part of this script compiles all high frequency met station data from the McMurdo Dry Valleys Long Term 
# Ecological Research project. The result is a separate CSV file for each met station, each of which contains the 
# meteorological variables measured by each station.


# Authors: Gavin P. Wagner (gavin.wagner@rutgers.edu), with contributions from Renée F. Brown (rfbrown@unm.edu)
# Start Date: 10/17/2023


#### Clear environment ####
rm(list=ls(all=TRUE))


#### Load Required Libraries ####
library(tidyverse)
library(EDIutils)
library(zoo)


#### PART 1 ####

##### Entity IDs #####
# Entity IDs are strings with the form "scope.identifier.revision"
# The scope for this project is "knb-lter-mcm"
# The identifiers for this project vary by met station
# The revision indicates a version of the data entity. We want the most recent data entity.

# This function, getEntityInfo(), procures information about a data entity in EDI.
# The input is the 4-letter identifier of the met station of interest. For example, "HOEM" for Lake Hoare
# The output is a dataframe with 6 columns:
# met: the met station name
# metAbv: the four-letter met station ID
# scope: The scope of the EDI project
# identifier: The EDI identifier of the entity of interest within the scope
# revision: The latest revision of the data entity
# entityID: the entity ID of the data entity, which can be used in the read_data_entity() function to procure data

#Begin Entity Info Function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getEntityInfo <- function(metID) {
  scope = "knb-lter-mcm"
  metInfo <- data.frame(
    met = c(
      "Canada Glacier",
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
    metAbv = c(
      "CAAM",
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
  if (!(metID %in% metInfo$metAbv)) {
    return("Invalid Met ID. Please input the four-letter MET identifier, for example 'HOEM' for Lake Hoare")
  }
  
  # Grab info about the met station specified.
  metInfo <- metInfo %>%
    filter(metAbv == metID) %>%
    mutate(scope = scope, .before = "identifier") %>%
    mutate(revision = list_data_package_revisions(scope,
                                                  identifier,
                                                  filter = "newest"),
           entityID = paste(scope, identifier, revision, sep = '.'))
  
  
  return(metInfo)
  
  
}

# End Entity Info Function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### PART 2 ####

##### Fetching Met Data #####

# Begin advanced data pull function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
advDataPull <- function(metAbv) {
  
  # First pull pertinent info about the specified met
  # Get general entity info
  entityInfo <- getEntityInfo(metAbv)
  
  # Create data directory if there is not already one
  if (!file.exists("data")) {
    dir.create(file.path("data"))
  }
  
  # Create subdirectory for met Station if there is not already one
  if (!file.exists(str_glue("data/{metAbv}"))) {
    dir.create(file.path("data", metAbv))
  }
  
  # Get the included parameter suites and their entityIDs
  params <- read_data_entity_names(entityInfo$entityID)
  
  # Get the names of the parameter suites to be included in the application
  # Include exceptions for CAAM (PRESSTA) and FRSM (RADN)
  if (metAbv == "CAAM") {
    paramNames <- params %>% 
      select(entityName) %>% 
      mutate(entityName = gsub(".*_", "", entityName)) %>% # Subset everything after the underscore
      filter(entityName %in% c("AIRT", "RADN", "WIND", "RH")) %>% 
      pull(entityName)
  } else if (metAbv == "FRSM") {
    paramNames <- params %>% 
      select(entityName) %>% 
      mutate(entityName = gsub(".*_", "", entityName)) %>% # Subset everything after the underscore
      filter(entityName %in% c("AIRT", "WIND", "PRESSTA", "RH")) %>% 
      pull(entityName)
  } else {
    paramNames <- params %>% 
      select(entityName) %>% 
      mutate(entityName = gsub(".*_", "", entityName)) %>% # Subset everything after the underscore
      filter(entityName %in% c("AIRT", "RADN", "WIND", "PRESSTA", "RH", "SOILT")) %>% 
      pull(entityName)
  }
  
  # Cycle through each entity and create subdirectories for each met-param combo if they don't already exist in the parent met directory
  for (param in paramNames) {
    subDir <- str_glue("data/{metAbv}/{metAbv}_{param}")
    if (!file.exists(subDir)) {
      dir.create(file.path(subDir))
    }
  }
  
  # Get the latest revision of the met station's data entity
  latestRevision <- data.frame(met = metAbv,
                               rev = list_data_package_revisions(entityInfo$scope, entityInfo$identifier, filter = "newest"))
  
  # Store the location where the revision file should be
  revFile <- str_glue("data/{metAbv}/{metAbv}latestRevision.csv") 
  
  #  Check if revision file exists.
  if (file.exists(revFile)) {
    # If yes, pull in the stored revision.
    storedRevision <- read_csv(revFile)
  } else {
    # If not, store the the latest revision in the met's directory. Create a dummy stored revision
    write_csv(latestRevision, revFile)
    storedRevision <- data.frame(met = metAbv,
                                 rev = "dummy")
  }
  
  # Only run if the latest revision doesn't match the stored data's revision
  if (storedRevision$rev != latestRevision$rev) {
    
    # Store the latest revision so we have it in the future
    write_csv(latestRevision, revFile) # Need to use write.csv here to write a csv from a single character vector
    
    # Cycle through the parameter suites contained in each met station's data entity, 
    # Write the parameter data to a csv that will take the name met_param.csv
    for (param in paramNames) {
        
      # Read in the entity's data
      rawData <- read_data_entity(entityInfo$entityID, 
                                  params[params$entityName == str_glue("{metAbv}_{param}"), "entityId"])
      
      # Read raw parameter data, only including pertinent variables
      # Note that wspd needs to come before wdir
      paramData <- read_csv(file = rawData) %>% 
        select(any_of(c("date_time",
                        "airt2m",
                        "airt1m",
                        "airt3m",
                        "swradin",
                        "swradout",
                        "rh2m",
                        "rh1m",
                        "rh3m",
                        "wspd",
                        "wdir",
                        "lwradin2",
                        "lwradout2",
                        "rh",
                        "par",
                        "soilt0cm",
                        "soilt5cm",
                        "soilt10cm",
                        "pressta")))
      
      # Store the file path for the met_param data
      met_paramFile <- str_glue("data/{metAbv}/{metAbv}_{param}/{metAbv}_{param}.csv")
      
      # Write the complete raw parameter data to a csv that will take the name met_param.csv
      write_csv(paramData, met_paramFile)
      
      # Read and store the numeric columns (variables of interest) contained in the parameter data. 
      # The variables of interest will be the only columns with numeric type.
      numericCols <- paramData %>% 
        select(where(is.numeric))
      variables <- colnames(numericCols)

      # Cycle through each variables contained in the parameter dataset
      for (var in variables) {

        # Create directories for each met-param-variable combo if they don't already exist in the parent met-param directory
        varDir <- str_glue("data/{metAbv}/{metAbv}_{param}/{var}")
        if (!file.exists(varDir)) {
          dir.create(file.path(varDir))
        } else {
          print("Subdirectory Exists")
        }
        
        # Select the data pertinent to the current variable, parse the date time column.
        # Wind direction relies on wind speed, so handle separately
        if (var == "wdir") { 
          varData <- paramData %>% 
            select(date_time, wdir, wspd) %>% 
            mutate(date_time = date(mdy_hm(date_time)))
        } else {
          varData <- paramData %>% 
            select(date_time, all_of(var)) %>% 
            mutate(date_time = date(mdy_hm(date_time)))
        }
 
        # Remove days that have more than 10% of the data missing
        varData <- varData %>%
          group_by(date_time) %>%
          summarise(naCount = sum(is.na(.data[[var]])),
                    entryCount = n(),
                    naProp = naCount/entryCount) %>%
          select(date_time, naProp) %>%
          right_join(varData, by = "date_time") %>%
          filter(naProp < 0.1) %>%
          select(!naProp)
        
        # Create daily summaries of the mean value of the variable over the day
        
        # Handle wind direction calculations separately
        if (var == "wdir") {
          # Filter for rows with non-missing wspd values
          varData <- varData %>%
            filter(!is.na(.data$wspd))
          
          # Calculate resultant vector average wind direction according to Grange 2014
          varData <- varData %>%
            group_by(date_time) %>%
            summarize(
                wdir_u = mean(-wspd * sin(2 * pi * wdir/360), na.rm = TRUE),
                wdir_v = mean(-wspd * cos(2 * pi * wdir/360), na.rm = TRUE)) %>%
            mutate(summaryVal = (atan2(wdir_u, wdir_v) * 360/2/pi) + 180)
        } else {
          varData <- varData %>%
            group_by(date_time) %>%
            summarize(summaryVal = mean(.data[[var]], na.rm = TRUE))
        }
        
        # Get data range
        startDate <- floor_date(head(varData$date_time,1), unit = "day")
        endDate <- floor_date(tail(varData$date_time,1), unit = "day")
        
        # Create sequence of all possible days in data range
        date_seq <- seq(startDate, endDate, by = "days")
        
        # Make this sequence the date column in a template data frame
        template_df <- data.frame(date_time = date(date_seq))
        
        # Backfill missing dates with NA's by merging the template to the raw data. Add columns for year, yearmonth, day of year, and season. Arrange columns
        # Season definitions provided by Obryk et al. 2020
        varData <- full_join(template_df, varData)%>%
          mutate(year = year(date_time),
                 month = month(date_time),
                 yearmonth = as.yearmon(date_time),
                 season = case_when(month %in% c(11, 12, 1, 2) ~ "Summer",     # Nov-Feb
                                    month %in% c(3) ~ "Autumn",                # Mar
                                    month %in% c(4, 5, 6, 7, 8, 9) ~ "Winter", # Apr-Sep
                                    month %in% c(10) ~ "Spring"),              # Oct
                 .after = "date_time") %>% 
          select(date_time, year, month, yearmonth, season, summaryVal) %>% 
          arrange(date_time)
        
        # Create "fakedate" column, a column with the dates correct except the year is 2020. Helps plot multiple years on the same x axis.
        varData <- varData %>%
          mutate(fakedate = update(date_time, year = 2020),
                 .after = "date_time")
        
        # Create special plotting columns for x axis labeling
        varData <- varData %>% 
          
          # Create "monthday" column, a column with the month abbreviation and the day of the month for each entry
          mutate(monthday = str_c(month.abb[month], day(date_time), sep = " "),
                 .before = "season") %>% 
          
          # Create "yearseason" column, a column with the season name and the year for each entry
          mutate(yearseason = str_c(season, year, sep = " "),
                 .after = "season")
        
        ###### Daily Data ######
      
        # Pull pertinent columns, add fakedate column
        dailyData <- varData %>% 
          select(date_time, fakedate, year, monthday, summaryVal)
        
        # Rename the summaryVal column as the variable of the current loop
        colnames(dailyData)[colnames(dailyData)=="summaryVal"] <- var
        
        # Store full paramData data frame (already daily-averaged) as variable.daily.csv
        write_csv(dailyData, str_glue("{varDir}/{metAbv}.{var}.daily.csv"))
        
        ###### Monthly Data ######
        
        # Aggregate over the month
        monthlyData <- varData %>%
          group_by(year, month, yearmonth) %>%
          summarise(summaryVal = mean(summaryVal, na.rm = TRUE)) 
        
        # Rename the summaryVal column as the variable of the current loop
        colnames(monthlyData)[colnames(monthlyData)=="summaryVal"] <- var
        
        # Store monthly-averaged data frame as variable.monthly.csv
        write_csv(monthlyData, str_glue("{varDir}/{metAbv}.{var}.monthly.csv"))
        
        ###### Seasonal Data ######
        
        # Aggregate over the season
        seasonalData <- varData %>% 
          group_by(year, season, yearseason) %>% 
          summarise(summaryVal = mean(summaryVal, na.rm = TRUE))
        
        # Rename the summaryVal column as the variable of the current loop
        colnames(seasonalData)[colnames(seasonalData)=="summaryVal"] <- var
        
        # Store seasonally-averaged data frame as variable.seasonal.csv
        write_csv(seasonalData, str_glue("{varDir}/{metAbv}.{var}.seasonal.csv"))
        
        ###### Historical Averages ######
        
        # Remove years that have more than 10% data missing.
        varData_cleaned <- varData %>%
          mutate(year = year(date_time),
                 isNA = is.na(summaryVal)) %>%
          group_by(year) %>%
          summarize(naCount = sum(isNA),
                    entryCount = n(),
                    naProp = naCount/entryCount) %>%
          select(year, naProp) %>%
          right_join(varData, by = "year") %>%
          filter(naProp < 0.1) %>%
          select(!naProp) %>%
          arrange(date_time)
        
        # Remove months that have more than 10% data missing (There are no months with more than 10% data missing in the lake hoare airt3m data)
        # Reformat Column order
        varData_cleaned <- varData_cleaned %>%
          mutate(isNA = is.na(summaryVal)) %>%
          group_by(month) %>%
          summarise(naCount = sum(isNA),
                    entryCount = n(),
                    naProp = naCount/entryCount) %>%
          select(month, naProp) %>%
          right_join(varData, by = "month") %>%
          filter(naProp < 0.1) %>%
          select(!naProp) %>%
          select(date_time, year, month, yearmonth, monthday, season, yearseason, summaryVal) %>%
          arrange(date_time)
          
        # Daily Historical Averages
        dailyHist <- varData_cleaned %>%
          group_by(monthday) %>% 
          summarise(histAvg = mean(summaryVal, na.rm = TRUE),
                    sdVal = sd(summaryVal, na.rm = TRUE))
        
        # Rename the summaryVal column as the variable of the current loop
        colnames(dailyHist)[colnames(dailyHist)=="summaryVal"] <- var
        
        # Store seasonally-averaged data frame as variable.seasonal.csv
        write_csv(dailyHist, str_glue("{varDir}/{metAbv}.{var}.dailyHist.csv"))
        
        # Monthly Historical Averages
        monthlyHist <- varData_cleaned %>% 
          group_by(month) %>% 
          summarise(histAvg = mean(summaryVal, na.rm = TRUE),
                    sdVal = sd(summaryVal, na.rm = TRUE))
        
        # Rename the summaryVal column as the variable of the current loop
        colnames(monthlyHist)[colnames(monthlyHist)=="summaryVal"] <- var
        
        # Store seasonally-averaged data frame as variable.seasonal.csv
        write_csv(monthlyHist, str_glue("{varDir}/{metAbv}.{var}.monthlyHist.csv"))
        
        # Seasonal Historical Averages
        seasonalHist <- varData_cleaned %>% 
          group_by(season) %>% 
          summarise(histAvg = mean(summaryVal, na.rm = TRUE),
                    sdVal = sd(summaryVal, na.rm = TRUE))
        
        # Rename the summaryVal column as the variable of the current loop
        colnames(seasonalHist)[colnames(seasonalHist)=="summaryVal"] <- var
        
        # Store seasonally-averaged data frame as variable.seasonal.csv
        write_csv(seasonalHist, str_glue("{varDir}/{metAbv}.{var}.seasonalHist.csv"))
      
      }
    }
  }
}


##### Daily Iterative Data Check/Pull Protocol #####

# Check each data entity to make sure if it is the latest. If not the latest, pull the latest data entity from EDI. To be run once daily. 


# Series of all possible Met Station 4-letter abbreviations
metAbvs = c(
  #"CAAM",
  #"COHM",
  "EXEM",
  "FRSM"#,
  #"HODM",
  #"BOYM",
  #"BRHM",
  #"FRLM",
  #"HOEM",
  #"VAAM",
  #"VIAM",
  #"MISM",
  #"FLMM",
  #"TARM"
  )

# Run advDataPull for all met stations in metAbvs
for (met in metAbvs) {
  advDataPull(met)
}



