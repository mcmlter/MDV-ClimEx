#### PURPOSE: PART 1 ####
# The first part of this script compiles the most recent unique identifiers for high frequency meteorological data collected by met stations at the McMurdo Dry Valleys Long Term Ecological Research site. The met data is housed at the Environmental Data Initiative Repository, and can be accessed using the EDIutils package. In order to pull the most recent data, the most recent unique packageID-entityID string for each station-parameter combination must be accessible.
#The result of this script is a csv file that specifies the most recent package IDs for each station and the entity names for the meteorological parameters stored within. Later, when data is fetched from EDI, this csv will be referenced.
# A function, getPackageInfo is also created that outputs the latest package ID of and data entities within a given EDI package


#### PURPOSE: PART 2 ####
# The second part of this script compiles all high frequency met station data from the McMurdo Dry Valleys Long Term Ecological Research Station. The result is a separate CSV file for each met station, each of which contains the meteorological parameters measured by each station
# Don't need to make a big CSV of EDI data. Can write a function where the parameter and met of interest is specified.


# Author: Gavin Wagner

# Start Date: 10/17/2023
# Finalized: 3/19/2024



#### Loading Required Packages ####
library(tidyverse)
library(EDIutils)
library(zoo)


#### Set the Main Working Directory Here ####

# This will be the directory where a folder "metData" will be created and populated with the met data requested
mainDir <- getwd()

# Set initial working directory as main directory
setwd(mainDir)

#### PART 1 ####

##### Package IDs #####
# Package IDs are strings with the form "scope.identifier.revision"
# The scope for this project is "knb-lter-mcm"
# The identifiers for this project vary by met station
# The revision indicates a version of the data package. We want the most recent data package.

# This function, getPackageInfo(), procures information about a data package on EDI.
# The input is the 4-letter identifier of the met station of interest. For example, "HOEM" for Lake Hoare
# The output is a dataframe with 6 columns:
# met: the met station name
# metAbv: the four-letter met station ID
# scope: The scope of the EDI project
# identifier: The EDI identifier of the package of interest within the scope
# revision: The latest revision of the data package
# packageID: the package ID of the data package, which can be used in the read_data_entity() function to procure data

#Begin Package Info Function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getPackageInfo <- function(metID) {
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

# End Package Info Function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### PART 2 ####

##### Fetching Met Data #####

# # Test Value
# met <- "BOYM"

# Begin advanced data pull function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
advDataPull <- function(met) {
  
  # First pull pertinent info about the specified met
  # Get general package info
  packageInfo <- getPackageInfo(met)
  
  # Store met abbreviation
  metAbv <- packageInfo$metAbv
  
  # Create subdirectory for metData if there is not already one
  if (!file.exists("../data/met")) {
    dir.create(file.path("../data/met"))
  }
  
  # Create subdirectory for met Station if there is not already one
  if (!file.exists(str_glue("../data/met/{metAbv}"))) {
    dir.create(file.path("../data/met", metAbv))
  }
  
  # Get the included parameters and their entityIDs
  entities <- read_data_entity_names(packageInfo$packageID)
  
  # Get the names of the entities included, excluding WVAPD, ONYXT, ICET because these files have different structures. Exclude PPT because precipitation data is incomplete.
  entityNames <- entities$entityName[!str_detect(entities$entityName, "WVAPD") & !str_detect(entities$entityName, "ONYXT") & !str_detect(entities$entityName, "ICET") & !str_detect(entities$entityName, "PPT")]
  
  # Cycle through each entity and create subdirectories for each met-entity combo if they don't already exist in the parent met directory
  for (entity in entityNames) {
    subDir <- str_glue("../data/met/{metAbv}/{entity}")
    if (!file.exists(subDir)) {
      dir.create(file.path(subDir))
    }
  }
  
  # Get the latest revision of the met station's data package
  latestRevision <- data.frame(met = metAbv,
                               rev = list_data_package_revisions(packageInfo$scope, packageInfo$identifier, filter = "newest"))
    
  
  # Store the location where the revision file should be
  revFile <-str_glue("../data/met/{metAbv}/{met}latestRevision.csv") 
  
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
  
  # # Test value
  # entity <- "BOYM_AIRT"
  
  # Only run if the latest revision doesn't match the stored data's revision
  if (storedRevision$rev != latestRevision$rev) {
    
    # Store the latest revision so we have it in the future
    write_csv(latestRevision, revFile) # Need to use write.csv here to write a csv from a single character vector
    
    # Cycle through the entities contained in the met's data package, # Write the entity data to a csv that will take the name met.entity.csv
    for (entity in entityNames) {
        
      # Read in the entity's data
      rawData <- read_data_entity(packageInfo$packageID, entities[entities$entityName == str_glue("{entity}"), "entityId"])
      entityData <- read_csv(file = rawData)
      
      # Store the file path for the met_entity data
      met_entityFile <- str_glue("../data/met/{metAbv}/{entity}/{entity}.csv")
      
      # Write the complete raw entity data to a csv that will take the name met_entity.csv
      write_csv(entityData, met_entityFile)
      
      # Read and store the numeric columns (variables of interest) contained in the entity data. The variables of interest will be the only columns with numeric type.
      numericCols <- entityData %>% 
        select(where(is.numeric))
      variables <- colnames(numericCols)
      
      # # Test Value
      # var <- "airt3m"
      
      # Cycle through each variables contained in the entity data set:
      for (var in variables) {
        # Create directories for each met-entity-variable combo if they don't already exist in the parent met-entity directory
        varDir <- str_glue("../data/met/{metAbv}/{entity}/{var}")
        if (!file.exists(varDir)) {
          dir.create(file.path(varDir))
        } else {
          print("Sub Directory Exists")
        }
        
        # Select the data pertinent to the current variable
        varData <- entityData %>% 
          select(date_time, all_of(var)) %>% 
          mutate(date_time = date(mdy_hm(date_time))) %>% 
          group_by(date_time) %>% 
          summarize(meanVal = mean(.data[[var]])) # Can't name the summary column with var value of current loop
        
        # Get data range
        startDate <- floor_date(head(varData$date_time,1), unit = "day")
        endDate <- floor_date(tail(varData$date_time,1), unit = "day")
        
        # Create sequence of all possible days in data range
        date_seq <- seq(startDate, endDate, by = "days")
        
        # Make this sequence the date column in a template data frame
        template_df <- data.frame(date_time = date(date_seq))
        
        # Backfill missing dates by merging the template to the raw data. Add columns for year, yearmonth, day of year, and season. Arrange columns
        varData <- full_join(template_df, varData)%>%
          mutate(year = year(date_time),
                 month = month(date_time),
                 yearmonth = as.yearmon(date_time),
                 season = case_when(month %in% c(11, 12, 1, 2) ~ "Summer", # Definition of Austral Summer provided by Obryk et al. (2020)
                                    month %in% c(3, 4, 5) ~ "Autumn", # Perhaps just 3
                                    month %in% c(6, 7, 8) ~ "Winter", # Perhaps 4,5,6,7,8,9
                                    month %in% c(9, 10) ~ "Spring"), # Perhaps just 10
                 .after = "date_time") %>% 
          select(date_time, year, month, yearmonth, season, meanVal) %>% 
          arrange(date_time)
        
        # Create "fakedate" column, a column with the dates correct except the year is 2020. Helps plot multiple years on the same x axis.
        varData <- varData %>%
          mutate(fakedate = update(date_time, year = 2020))
        
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
          select(date_time, fakedate, year, monthday, meanVal)
        
        # Rename the meanVal column as the variable of the current loop
        colnames(dailyData)[colnames(dailyData)=="meanVal"] <- var
        
        # Store full entityData data frame (already daily-averaged) as variable.daily.csv
        write_csv(dailyData, str_glue("{varDir}/{metAbv}.{var}.daily.csv"))
        
        
        ###### Monthly Data ######
        
        # Aggregate over the month
        monthlyData <- varData %>% 
          group_by(year, month, yearmonth) %>% 
          summarise(meanVal = mean(meanVal, na.rm = TRUE))
          
        # Rename the meanVal column as the variable of the current loop
        colnames(monthlyData)[colnames(monthlyData)=="meanVal"] <- var
        
        # Store monthly-averaged data frame as variable.monthly.csv
        write_csv(monthlyData, str_glue("{varDir}/{metAbv}.{var}.monthly.csv"))
        

        ###### Seasonal Data ######
        
        # Aggregate over the season
        seasonalData <- varData %>% 
          group_by(year, season, yearseason) %>% 
          summarise(meanVal = mean(meanVal, na.rm = TRUE))
        
        # Rename the meanVal column as the variable of the current loop
        colnames(seasonalData)[colnames(seasonalData)=="meanVal"] <- var
        
        # Store seasonally-averaged data frame as variable.seasonal.csv
        write_csv(seasonalData, str_glue("{varDir}/{metAbv}.{var}.seasonal.csv"))
        
        
        ###### Historical Averages ######
        
        # Remove years that have more than 10% data missing.
        varData_cleaned <- varData %>%
          mutate(year = year(date_time),
                 isNA = is.na(meanVal)) %>%
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
          mutate(isNA = is.na(meanVal)) %>%
          group_by(month) %>%
          summarise(naCount = sum(isNA),
                    entryCount = n(),
                    naProp = naCount/entryCount) %>%
          select(month, naProp) %>%
          right_join(varData, by = "month") %>%
          filter(naProp < 0.1) %>%
          select(!naProp) %>% 
          select(date_time, year, month, yearmonth, monthday, season, yearseason, meanVal) %>% 
          arrange(date_time)
          
          
        # Daily Historical Averages
        dailyHist <- varData_cleaned %>%
          group_by(monthday) %>% 
          summarise(histAvg = mean(meanVal, na.rm = TRUE),
                    sdVal = sd(meanVal, na.rm = TRUE))
        
        # Rename the meanVal column as the variable of the current loop
        colnames(dailyHist)[colnames(dailyHist)=="meanVal"] <- var
        
        # Store seasonally-averaged data frame as variable.seasonal.csv
        write_csv(dailyHist, str_glue("{varDir}/{metAbv}.{var}.dailyHist.csv"))
        
        
        # Monthly Historical Averages
        monthlyHist <- varData_cleaned %>% 
          group_by(month) %>% 
          summarise(histAvg = mean(meanVal, na.rm = TRUE),
                    sdVal = sd(meanVal, na.rm = TRUE))
        
        # Rename the meanVal column as the variable of the current loop
        colnames(monthlyHist)[colnames(monthlyHist)=="meanVal"] <- var
        
        # Store seasonally-averaged data frame as variable.seasonal.csv
        write_csv(monthlyHist, str_glue("{varDir}/{metAbv}.{var}.monthlyHist.csv"))
        
        
        # Seasonal Historical Averages
        seasonalHist <- varData_cleaned %>% 
          group_by(season) %>% 
          summarise(histAvg = mean(meanVal, na.rm = TRUE),
                    sdVal = sd(meanVal, na.rm = TRUE))
        
        # Rename the meanVal column as the variable of the current loop
        colnames(seasonalHist)[colnames(seasonalHist)=="meanVal"] <- var
        
        # Store seasonally-averaged data frame as variable.seasonal.csv
        write_csv(seasonalHist, str_glue("{varDir}/{metAbv}.{var}.seasonalHist.csv"))
        
      }
    }
    
  }
  
  
}


##### Daily Iterative Data Check/Pull Protocol #####

# Check each data package to make sure if it is the latest. If not the latest, pull the latest data package from EDI. To be run once daily. 


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







