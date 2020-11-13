# Required packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#---------------------------Read data Sets-------------------------------------#
# Read The List of Endangered Species, according to the Ordinance of the 
# Brazilian Ministry of Environment (Portaria MMA 443/2014)
port <- read.csv2(
  'C:/Robson/home_office/Global-Level-Forest-Inventory/Data/attachment_443.csv',
                  header = T, comment.char = "")

# Show Ordinance of the Brazilian Ministry of Environment 
head(port)

# Read the forest inventory sheet
poa2 <- read.csv2(
  'C:/Robson/home_office/Global-Level-Forest-Inventory/Data/forest_inventory.csv', 
                  header = T, sep = ',', as.is = T, comment.char = "")

# Show variables of forest inventory
head(poa2)

# Read Infrastructure Sheet
infra <- read.csv2(
  'C:/Robson/home_office/Global-Level-Forest-Inventory/Data/poa2_infra.csv',
                   header = T, comment.char = "")

# Show variables of infrastructure
head(infra)

#------------------------------Summary Statics---------------------------------#
# Summary Statics
summary(poa2)

#-----------------Create Diameter at breast height Classes (DBH)---------------#
classe <- vector()
classe2 <- vector()

# Define the limits of DBH
classeMin <- 40  # Minimum diameter
classeMax <- 300 # Maximum diameter
classeAmp <- 10  # Range of diameter class
nClasses <- (classeMax - classeMin)/classeAmp # Number of diameter classes

# Looping to DBH
d <- classeMin 

  
for (j in 1:nClasses) {
        d = d + classeAmp   ## Upper Class Limit
        li = d - classeAmp  ## Lower Class Limit
        sup = d
        class = (li + sup)/2
        classe[which(poa2$DAP_cm >= li & poa2$DAP_cm < sup)] = paste(class,
                                                                     sep="")
        classe2[which(poa2$DAP_cm >= li & poa2$DAP_cm < sup)] = paste(li, 
                                                                      "-", 
                                                                      sup, 
                                                                      sep="")
}

# Define a limit maximums of class
maxClasse <- 150
classe2[which(as.numeric(classe) > maxClasse)] = paste(">", maxClasse, sep = "")
classe[which(as.numeric(classe) > maxClasse)] = paste(">", maxClasse, sep = "")

# Remove the object j created on looping  
rm(j)

# Convert vectors to factors
poa2$classe = as.factor(classe)
poa2$classe2 = as.factor(classe2)

rm(d, li, sup, classe)

# Display levels of class
levels(poa2$classe)
levels(poa2$classe2)

# Sort Class Levels
# poa2$classe <- ordered(poa2_port$classe, levels=c("45", "55", "65", "75", 
#                                                 "85", "95", "105", "115",  
#                                                 "125",  "135",  "145",  "155",  
#                                                 "165", "175",  "185",  "195", 
#                                                 ">200"))

poa2$classe2 <- ordered(poa2$classe2, levels=c("40-50", "50-60",  "60-70",  
                                               "70-80", "80-90", "90-100", 
                                               "100-110", "110-120", "120-130",
                                               "130-140", "140-150", ">150"))  

# Plot Diameter Class Distribution for all tree species
barplot(table(poa2$classe2), 
        ylab='Nº of Trees', 
        xlab='Diameter at Breast Height Classes de DAP (DBH)',
        axis.lty=1)

# Save the FI with diameter class
write.csv2(
        poa2, 
        file = 'C:/Robson/home_office/Global-Level-Forest-Inventory/IF_umf2_upa2_CLASS.csv', 
        na='NA',
        row.names = FALSE)

#--------------Join the FI with the list of endangered species-----------------#
poa2_port <- poa2 %>%
        full_join(port, by = 'Nome_Cientifico') %>% ## endangered species
        full_join(infra, by = 'UT') %>%             ## infrastructure
        select(-X.y)
 
# Save FI only endangered species      
write.csv2(
        poa2, 
        file = 'C:/Robson/home_office/Global-Level-Forest-Inventory/if_endangered.csv', 
        na='NA',
        row.names = FALSE)


