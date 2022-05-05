library(tidyverse)

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv')

glimpse(inspections)

names <- c('ID', 'DBAname', 'AKAname', 'License', 'FacilityType', 'Risk', 'Address', 'City',
           'City', 'State', 'Zip', 'InspectionDate' 'InspectionType', 'Result', 'Violations',
           'Latitute', 'Longitue', 'Location')

c02c