# Close all previously created figures
graphics.off()

# Load packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(hrbrthemes)
library(ggalt)

#---------------------------Read data Sets-------------------------------------#
# Read The List of Endangered Species, according to the Ordinance of the       #
# Brazilian Ministry of Environment (Portaria MMA 443/2014)                    #
#------------------------------------------------------------------------------#

port <- read.csv2(
  'C:/Robson/home_office/Global-Level-Forest-Inventory/Data/attachment_443.csv',
                  header = TRUE,
                  skip = 1)

# Show Ordinance of the Brazilian Ministry of Environment 
as_tibble(port)

# Read the forest inventory sheet
FI <- read.csv2(
  'C:/Robson/home_office/Global-Level-Forest-Inventory/Data/IFpoa2.csv', 
                  header = TRUE)

# Show variables of forest inventory
as_tibble(FI)

# Read Infrastructure Sheet
infra <- read.csv2(
  'C:/Robson/home_office/Global-Level-Forest-Inventory/Data/poa2_infra.csv',
                   header = T)

# Show variables of infrastructure
as_tibble(infra)

#------------------------------Summary Statics---------------------------------#
# Summary Statics
summary(FI)

#---------------------------Kernel density plot--------------------------------#
#
# Kernel density plot to view the distribution of DBH variable.
#
# Automatically defined window:
plot(density(FI$DBH), # returns the density data
             ylab = 'Density', 
             main = 'Diameter at Breast Height - FLONA Caxiuanã UMF 2 UPA 2')

# Manually defined Windows (2.5, 4, e 6)
plot(density(FI$DBH,
             bw = 2.5,
             na.rm = TRUE),
     ylab='Density', 
     xlab = 'DBH (cm)',
     main = ' Kernel Density Estimates of Diameter at Breast Height \n in FLONA Caxiuanã UMF 2 UPA 2', 
     col='red')
lines(density(FI$DBH, 
              bw = 4),
      col='green')
lines(density(FI$DBH,
              bw = 6), 
      col='blue')
legend('topright', 
       legend=c('red: bw = 2.5',
                'green: bw = 4',
                'blue: bw = 6'))



#-----------------Create Diameter at breast height Classes (DBH)---------------#
class <- vector()
class2 <- vector()

# Define the limits of DBH
classMin <- 40  # Minimum diameter
classMax <- 300 # Maximum diameter
classAmp <- 10  # Range of diameter class
nClasses <- (classMax - classMin) / classAmp # Number of diameter classes

# Looping to DBH
d <- classMin 

for(j in 1:nClasses){
        d = d + classAmp   ## Upper Class Limit
        li = d - classAmp  ## Lower Class Limit
        sup = d
        classL = (li + sup) / 2
        class[which(FI$DBH >= li & FI$DBH < sup)] = paste(classL, sep="")
        class2[which(FI$DBH >= li & FI$DBH < sup)] = paste(li, "-", sup, sep="")
}

# Define a limit maximums of class
maxClass <- 150
class2[which(as.numeric(class) > maxClass)] = paste(">", maxClass, sep = "")
class[which(as.numeric(class) > maxClass)] = paste(">", maxClass, sep = "")

# Remove the object j created on looping  
rm(j)

# Convert vectors to factors
FI$class = as.factor(class)
FI$class2 = as.factor(class2)

rm(d, li, sup, class)

# Display levels of class
levels(FI$class)
levels(FI$class2)

# Sort Class Levels
# FI$class <- ordered(FI_port$class,
                      # levels=c("45", "55", "65", "75", "85", "95", "105", "115", 
                      #          "125",  "135",  "145",  "155", "165", "175", 
                      #          "185",  "195", ">200"))

FI$class2 <- ordered(FI$class2, 
                     levels = c("40-50", "50-60",  "60-70", "70-80", "80-90", 
                                "90-100", "100-110", "110-120", "120-130",
                                "130-140", "140-150", ">150"))  

# Plot Diameter Class Distribution for all tree species
barplot(table(FI$class2), 
        ylab='Nº of Trees', 
        xlab='Diameter at Breast Height (DBH)',
        axis.lty=1)

# Save the FI with diameter class
write.csv2(
        FI, 
        file = 'C:/Robson/home_office/Global-Level-Forest-Inventory/IF_umf2_upa2_CLASS.csv', 
        na='NA',
        row.names = FALSE)

#--------------Join the FI with the list of endangered species-----------------#
FI_port <- FI %>%
        left_join(port, by = 'Scientific_Name') %>% ## endangered species
        full_join(infra, by = 'UT')                 ## infrastructure
        

# NA values replaced by "Nao Protegida"
FI_port$Status[which(is.na(FI_port$Status))] <- "Nao Protegida"
as_tibble(FI_port)

# Save FI only endangered species      
write.csv2(
        FI_port, 
        file = 'C:/Robson/home_office/Global-Level-Forest-Inventory/if_endangered.csv', 
        na='NA',
        row.names = FALSE)

#-----------------------Total Volume and Basal Area----------------------------#
# Total average volume
sapply(split(FI_port$vol, FI_port$UT), mean)

# Sum of total volume
sapply(split(FI_port$vol, FI_port$UT), sum)

# Sum of total basal area
sapply(split(FI_port$G, FI_port$UT), sum)

#----------------------Cutting Volume and Basal Area---------------------------#
cutVol <- FI_port[which(FI_port$Destination == "Abate"), ]
tapply(cutVol$vol, cutVol$UT, mean)
sapply(split(cutVol$vol, cutVol$UT), sum)
sapply(split(cutVol$G, cutVol$UT), sum)

#----------------------------Basal Area by DBH---------------------------------#
G_DBH <- aggregate.data.frame(FI_port$G, list(FI_port$class2), sum) 
names_G <- c("DBH", "Basal_Area")
colnames(G_DBH) <- names_G
G_DBH

#---------------------Filter Trees Suitable for Cutting------------------------#
FI_filter <- FI_port %>%
        filter(DBH >= 50 & QF <= 2)
as_tibble(FI_filter)  

#-------------------------Plot Basal Area by DBH-------------------------------#
FI_filter %>%
        select(class2, G) %>%
        filter(!is.na(class2)) %>%
        group_by(class2) %>%
        summarize(sum_G = sum(G)) %>%
        ggplot(aes(x = class2, 
                   y = sum_G,
                   fill = sum_G)) +
        geom_bar(stat = "identity",
                 position = position_dodge(0.8)) +
        theme(plot.title = element_text(color = "black",
                                        size = 14,
                                        face = "bold",
                                        hjust = 0.5),
              plot.subtitle = element_text(color = "black",
                                           hjust = 0.5),
              axis.title.x = element_text(color = "black",
                                          size = 13),
              axis.title.y = element_text(color = "black",
                                          size = 13)) +
        labs(title = "Basal Area per DBH",
             subtitle = "UPA 2 UMF 2 FLONA Caxiuanã",
             x = "Diamter at Breast Height (DBH - cm)",
             y = "Basal Area (m²)",
             caption = "Source: Florest Inventory POA 2020 Benevides Madeiras LTDA.") +
        theme(legend.position = c(0.9, 0.7))
  
