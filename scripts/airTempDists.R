# This script creates air temperature distributions from meteorological data in the McMurdo Dry Valleys of Antarctica

library(tidyverse)

# Read Met Data from Lake Hoare
data <- read_csv("data/met/HOEM/HOEM.Daily.CSV")

# Make histogram of air temperature at 3 meters over the whole record

HOEMPlot <- ggplot(data, aes(airtemp_3m_degc))+
  geom_histogram()+ 
  labs(title = "Air Temperature Distribution at Lake Hoare Met Station")

HOEMPlot

# Read Met Data from Lake Fryxell
data <- read_csv("data/met/FRLM/FRLM.Daily.CSV")

# Make histogram of air temperature at 3 meters over the whole record

FRYXPlot <- ggplot(data, aes(airtemp_3m_degc))+
  geom_histogram()+ 
  labs(title = "Air Temperature Distribution at Lake Fryxell Met Station")

FRYXPlot


# Read Met Data from Canada Glacier Met
data <- read_csv("data/met/CAAM/CAAM.Daily.CSV")

# Make histogram of air temperature at 3 meters over the whole record

CAAMPlot <- ggplot(data, aes(airtemp_3m_degc))+
  geom_histogram()+ 
  labs(title = "Air Temperature Distribution at Canada Glacier Met Station")

CAAMPlot 
