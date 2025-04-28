#### PURPOSE: PART 1 ####
# The first part of this script compiles the most recent unique identifiers for high frequency meteorological data collected by met stations at the McMurdo Dry Valleys Long Term Ecological Research project. The met data is housed at the Environmental Data Initiative repository, and can be accessed using the EDIutils package. In order to pull the most recent data, the most recent unique entityID-paramID string for each station-parameter combination must be accessible. 

# The result of this script is a csv file that specifies the most recent entity IDs for each station and the names for the meteorological variables stored within. Later, when data is fetched from EDI, this csv will be referenced. A function, getEntityInfo is also created that outputs the latest entity ID and data available for a given EDI entity.


#### PURPOSE: PART 2 ####
# The second part of this script compiles all high frequency met station data from the McMurdo Dry Valleys Long Term Ecological Research project. The result is a separate CSV file for each met station, each of which contains the meteorological variables measured by each station.


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
           packageID = paste(scope, identifier, revision, sep = '.'))
  
  
  return(metInfo)
  
  
}

# End Entity Info Function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### PART 2 ####

##### Fetching Met Data #####

# Test Met

metAbv <- "HOEM"

# Begin advanced data pull function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
advDataPull <- function(metAbv) {
  
  ###### Reading Data ######
  
  # First pull pertinent info about the specified met
  # Get general entity info
  entityInfo <- getEntityInfo(metAbv)
  
  # Create data directory if there is not already one
  if (!file.exists("data")) {
    dir.create(file.path("data"))
  }
  
  # Create data/met directory if there is not already one
  if (!file.exists("data/met")) {
    dir.create(file.path("data/met"))
  }
  
  # Create subdirectory for met Station if there is not already one
  if (!file.exists(str_glue("data/met/{metAbv}"))) {
    dir.create(file.path("data/met", metAbv))
  }
  
  # Get the latest revision of the met station's data entity
  latestRevision <- data.frame(met = metAbv,
                               rev = list_data_package_revisions(entityInfo$scope, entityInfo$identifier, filter = "newest"))
  
  # Store the location where the revision file should be
  revFile <- str_glue("data/met/{metAbv}/{metAbv}latestRevision.csv") 
  
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
    write_csv(latestRevision, revFile) # Need to use write.csv here to write a csv from a single character vector?
    
    # Get the entityID from the daily dataset
    entityId <- read_data_entity_names(entityInfo$packageID) %>% 
      filter(grepl("daily", entityName, ignore.case = TRUE)) %>%
      pull(entityId)
    
    # Import daily dataset from specified met station
    rawData <- read_data_entity(entityInfo$packageID, 
                                entityId)
    
    # Read raw dataset into a data frame containing all of the variables
    data <- read_csv(file = rawData)
    
    
    ###### Processing Data ######
    
    # Add year, month, yearmonth, and season columns.
    # Season definitions provided by Obryk et al. 2020
    data <- data %>% 
      mutate(year = year(date_time),
             month = month(date_time),
             yearmonth = as.yearmon(date_time),
             season = case_when(month %in% c(11, 12, 1, 2) ~ "Summer",     # Nov-Feb
                                month %in% c(3) ~ "Autumn",                # Mar
                                month %in% c(4, 5, 6, 7, 8, 9) ~ "Winter", # Apr-Sep
                                month %in% c(10) ~ "Spring"),              # Oct
             .after = "date_time")
    
    # Create "fakedate" column, a column with the dates correct except the year is 2020. Helps plot multiple years on the same x axis.
    data <- data %>%
      mutate(fakedate = update(date_time, year = 2020),
             .after = "date_time")
    
    # Create special plotting columns for x axis labeling
    data <- data %>% 
      
      # Create "monthday" column, a column with the month abbreviation and the day of the month for each entry
      mutate(monthday = str_c(month.abb[month], day(date_time), sep = " "),
             .before = "season") %>% 
      
      # Create "yearseason" column, a column with the season name and the year for each entry
      mutate(yearseason = str_c(season, year, sep = " "),
             .after = "season")
    
    # Store full daily dataset as metAbv.daily.csv
    write_csv(data, str_glue("data/met/{metAbv}/{metAbv}.Daily.csv"))
    
    ####### Monthly Data #######
    
    # Aggregate over the month
    monthlyData <- data %>%
      group_by(year, month, yearmonth) %>%
      summarise(across(all_of(colnames(data)[11:ncol(data)]), ~mean(.x, na.rm = TRUE))) # numerical data starts at column 11
    
    # Replace a NaN values with NA
    monthlyData <- monthlyData %>%
      mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))
    
    # Store monthly-averaged data frame as metAbv.monthly.csv
    write_csv(monthlyData, str_glue("data/met/{metAbv}/{metAbv}.Monthly.csv"))
    
    ####### Seasonal Data #######
    
    # Aggregate over the season
    seasonalData <- data %>% 
      group_by(year, season, yearseason) %>% 
      summarise(across(all_of(colnames(data)[11:ncol(data)]), ~mean(.x, na.rm = TRUE))) # numerical data starts at column 11
    
    # Replace a NaN values with NA
    seasonalData <- seasonalData %>%
      mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))
    
    # Store seasonally-averaged data frame as metAbv.seasonal.csv
    write_csv(seasonalData, str_glue("data/met/{metAbv}/{metAbv}.Seasonal.csv"))
    
    ####### Historical Averages and Standard Deviations #######
    
    # Process and Store Daily Historical Averages as met.dailyHistAvg.csv
    dailyHistAvg <- data %>%
      group_by(monthday) %>% 
      summarise(across(all_of(colnames(data)[11:ncol(data)]), ~mean(.x, na.rm = TRUE)))
    write_csv(dailyHistAvg, str_glue("data/met/{metAbv}/{metAbv}.DailyHistAvg.csv"))
    
    # Process and Store Daily Standard Deviations as met.dailyHistSD.csv
    dailyHistSD <- data %>%
      group_by(monthday) %>% 
      summarise(across(all_of(colnames(data)[11:ncol(data)]), ~sd(.x, na.rm = TRUE)))
    write_csv(dailyHistSD, str_glue("data/met/{metAbv}/{metAbv}.DailyHistSD.csv"))
    
    # Process and Store Monthly Historical Averages as met.monthlyHistAvg.csv
    monthlyHistAvg <- data %>%
      group_by(month) %>% 
      summarise(across(all_of(colnames(data)[11:ncol(data)]), ~mean(.x, na.rm = TRUE)))
    write_csv(monthlyHistAvg, str_glue("data/met/{metAbv}/{metAbv}.MonthlyHistAvg.csv"))
    
    # Process and Store Monthly Standard Deviations as met.monthlyHistSD.csv
    monthlyHistSD <- data %>%
      group_by(month) %>% 
      summarise(across(all_of(colnames(data)[11:ncol(data)]), ~sd(.x, na.rm = TRUE)))
    write_csv(monthlyHistSD, str_glue("data/met/{metAbv}/{metAbv}.MonthlyHistSD.csv"))
    
    # Process and Store Seasonal Historical Averages as met.monthlyHistAvg.csv
    seasonalHistAvg <- data %>%
      group_by(season) %>% 
      summarise(across(all_of(colnames(data)[11:ncol(data)]), ~mean(.x, na.rm = TRUE)))
    write_csv(seasonalHistAvg, str_glue("data/met/{metAbv}/{metAbv}.SeasonalHistAvg.csv"))
    
    # Process and Store Seasonal Standard Deviations as met.monthlyHistSD.csv
    seasonalHistSD <- data %>%
      group_by(season) %>% 
      summarise(across(all_of(colnames(data)[11:ncol(data)]), ~sd(.x, na.rm = TRUE)))
    write_csv(seasonalHistSD, str_glue("data/met/{metAbv}/{metAbv}.SeasonalHistSD.csv"))
    
  }
}

##### Daily Iterative Data Check/Pull Protocol #####

# Check each data entity to make sure if it is the latest. If not the latest, pull the latest data entity from EDI. To be run once daily. 


# Series of all possible Met Station 4-letter abbreviations
metAbvs = c(
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
  "TARM"
)

# Run advDataPull for all met stations in metAbvs
for (met in metAbvs) {
  advDataPull(met)
}
