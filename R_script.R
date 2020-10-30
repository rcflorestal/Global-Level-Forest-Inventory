# Required packages
library(dplyr)
library(tidyr)
library(stringr)

# Load The List of Endangered Species, according to the Ordinance of the Brazilian Ministry of Environment (Portaria MMA 443/2014)
port <- read.csv2('C:/Robson/home_office/Global-Level-Forest-Inventory/Data/attachment_443.csv', header = T)

# Show Ordinance of the Brazilian Ministry of Environment 
head(port)

# load the forest inventory sheet
poa2 <- read.csv2('C:/Robson/home_office/Global-Level-Forest-Inventory/Data/forest_inventory.csv', 
                  header = T, sep = ',', as.is = T)

# Show variables of forest inventory
head(poa2)
