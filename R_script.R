# Required packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#---------------------------Read data Sets-------------------------------------#
# Read The List of Endangered Species, according to the Ordinance of the       #
# Brazilian Ministry of Environment (Portaria MMA 443/2014)                    #
#------------------------------------------------------------------------------#

port <- read.csv2(
  'C:/Robson/home_office/Global-Level-Forest-Inventory/Data/attachment_443.csv',
                  header = T, comment.char = "")

# Show Ordinance of the Brazilian Ministry of Environment 
head(port)

# Read the forest inventory sheet
FI <- read.csv2(
  'C:/Robson/home_office/Global-Level-Forest-Inventory/Data/forest_inventory.csv', 
                  header = T, sep = ',', as.is = T, comment.char = "")

# Show variables of forest inventory
head(FI)

# Read Infrastructure Sheet
infra <- read.csv2(
  'C:/Robson/home_office/Global-Level-Forest-Inventory/Data/FI_infra.csv',
                   header = T, comment.char = "")

# Show variables of infrastructure
head(infra)

#------------------------------Summary Statics---------------------------------#
# Summary Statics
summary(FI)

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
        classe[which(FI$DAP_cm >= li & FI$DAP_cm < sup)] = paste(class,
                                                                     sep="")
        classe2[which(FI$DAP_cm >= li & FI$DAP_cm < sup)] = paste(li, 
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
FI$classe = as.factor(classe)
FI$classe2 = as.factor(classe2)

rm(d, li, sup, classe)

# Display levels of class
levels(FI$classe)
levels(FI$classe2)

# Sort Class Levels
# FI$classe <- ordered(FI_port$classe, levels=c("45", "55", "65", "75", 
#                                                 "85", "95", "105", "115",  
#                                                 "125",  "135",  "145",  "155",  
#                                                 "165", "175",  "185",  "195", 
#                                                 ">200"))

FI$classe2 <- ordered(FI$classe2, levels=c("40-50", "50-60",  "60-70",  
                                               "70-80", "80-90", "90-100", 
                                               "100-110", "110-120", "120-130",
                                               "130-140", "140-150", ">150"))  

# Plot Diameter Class Distribution for all tree species
barplot(table(FI$classe2), 
        ylab='Nº of Trees', 
        xlab='Diameter at Breast Height Classes de DAP (DBH)',
        axis.lty=1)

# Save the FI with diameter class
write.csv2(
        FI, 
        file = 'C:/Robson/home_office/Global-Level-Forest-Inventory/IF_umf2_upa2_CLASS.csv', 
        na='NA',
        row.names = FALSE)

#--------------Join the FI with the list of endangered species-----------------#
FI_port <- FI %>%
        full_join(port, by = 'Nome_Cientifico') %>% ## endangered species
        full_join(infra, by = 'UT') %>%             ## infrastructure
        select(-X.y)
 
# Save FI only endangered species      
write.csv2(
        FI_port, 
        file = 'C:/Robson/home_office/Global-Level-Forest-Inventory/if_endangered.csv', 
        na='NA',
        row.names = FALSE)

#-----------------------Total Volume and Basal Area----------------------------#
# Total average volume
sapply(split(FI_port$vol_m3, FI_port$UT), mean)

# Sum of total volume
sapply(split(FI_port$vol_m3, FI_port$UT), sum)

# Sum of total basal area
sapply(split(FI_port$G, FI_port$UT), sum)

#----------------------Cutting Volume and Basal Area---------------------------#
cutVol <- FI_port[which(FI_port$Destinacao == "Abate"), ]
sapply(split(cutVol$vol_m3, cutVol$UT), mean)
sapply(split(cutVol$vol_m3, cutVol$UT), sum)
sapply(split(cutVol$G, cutVol$UT), sum)

#----------------------------Basal Area by DBH---------------------------------#
G_DBH <- aggregate.data.frame(FI_port$G, list(FI_port$classe2), sum) 
names_G <- c("DBH", "Basal_Area")
colnames(G_DBH) <- names_G
G_DBH

#---------------------Filter Trees Suitable for Cutting------------------------#
FI_filter <- FI_port %>%
        filter(DAP_cm >= 50 & QF <= 2)
head(FI_filter)  

