##----------------------------------------------------------------------------##
##                          Forest Inventory Analysis                         ##
##----------------------------------------------------------------------------##
#'
#'@author Robson Cruz
#'
#'@description Global Forest Inventory Analysis
#'
#'@param plot, species names, scientific names, DBH, Basal Area (G), 
#'stem height (H), .....
#'
#'@return 
#'Statistic analysis of global forest inventory, 
#'plots and tables.


# Load packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(hrbrthemes)
library(ggalt)
library(DT)

#---------------------------Read data Sets-------------------------------------#
# Read The List of Endangered Species, according to the Ordinance of the       #
# Brazilian Ministry of Environment (Portaria MMA 443/2014)                    #
#------------------------------------------------------------------------------#
#
port <- read.csv2(
  'D:/Robson/home_office/Global-Level-Forest-Inventory/Data/attachment_443.csv',
                  header = TRUE,
                  skip = 1)

# Show Ordinance of the Brazilian Ministry of Environment 
as_tibble(port)

# Read the forest inventory sheet
FI <- read.csv2(
  'D:/Robson/home_office/Global-Level-Forest-Inventory/Data/IFpoa2.csv', 
                  header = TRUE)

# Show variables of forest inventory
as_tibble(FI)

# Read Infrastructure Sheet
infra <- read.csv2(
  'D:/Robson/home_office/Global-Level-Forest-Inventory/Data/poa2_infra.csv',
                   header = T)

# Show variables of infrastructure
as_tibble(infra)

#------------------------------Summary Statics---------------------------------#
# Summary Statics
summary(FI[c(1:3, 7:11)])

#---------------------------Kernel density plot--------------------------------#
#
# Kernel density plot to view the distribution of DBH variable.
#
# Automatically defined window:
# plot(density(FI$DBH), # returns the density data
#              ylab = 'Density', 
#              main = 'Diameter at Breast Height - FLONA Caxiuanã UMF 2 UPA 2')

# Manually defined Windows (2.5, 4, e 6)
plot(density(FI$DBH,
             bw = 2.5,
             na.rm = TRUE),
     ylab='Density', 
     xlab = 'DBH (cm)',
     main = 'Kernel Density Estimates of Diameter at Breast Height \n in FLONA Caxiuanã UMF 2 UPA 2', 
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

png("D:/Robson/home_office/Global-Level-Forest-Inventory/output/densityKernelDBH.png",
    width = 640,
    height = 480,
    units = "px")

dev.off()

#-----------------Create Diameter at breast height Classes (DBH)---------------#
class <- vector()   ## Sets the class center vector
class2 <- vector()  ## Sets the class range vector

# Define the limits of DBH
classMin <- 40  # Sets Minimum diameter
classMax <- 300 # Sets Maximum diameter
classAmp <- 10  # Sets Class width
nClasses <- (classMax - classMin) / classAmp # Number of diameter classes

# Looping to DBH
d <- classMin 

for(j in 1:nClasses){
        d = d + classAmp   ## Upper Class Limits
        li = d - classAmp  ## Lower Class Limits
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
par(las = 3,
    mar = c(6.5, 5.6, 1.6, 1.6),
    mgp = c(3.8, 0.6, 0))

barplot(table(FI$class2), 
        ylab = 'Nº  of  Trees', 
        xlab = 'Class of Diameter at Breast Height (DBH) in cm.',
        axis.lty = 1,
        axes = TRUE)

png("D:/Robson/home_office/Global-Level-Forest-Inventory/output/distributionDBH.png",
    width = 640,
    height = 480,
    units = "px")

dev.off()

# Save the FI with diameter class
write.csv2(
        FI, 
        file = 'D:/Robson/home_office/Global-Level-Forest-Inventory/output/IF_umf2_upa2_CLASS.csv', 
        na='NA',
        row.names = FALSE)

#--------------Join the FI with the list of endangered species-----------------#
FI_port <- FI %>%
        left_join(port, by = 'Scientific_Name') %>% ## endangered species
        left_join(infra, by = 'UT')                 ## infrastructure
        

# NA values replaced by "Nao Protegida"
FI_port$Status[which(is.na(FI_port$Status))] <- "Nao Protegida"

FI_port <- FI_port %>%
  select(-note)

as_tibble(FI_port)

# Save FI only endangered species      
write.csv2(
        FI_port, 
        file = 'D:/Robson/home_office/Global-Level-Forest-Inventory/output/if_endangered.csv', 
        na = 'NA',
        row.names = FALSE)

#-------------------------------List of species-------------------------------##
list_Species <- FI_port %>%
            select(Name, Scientific_Name, Status, Destination) %>%
            group_by(Scientific_Name) %>%
            mutate(Abate = sum(Destination == "Abate"),
                   Remanescentes = sum(Destination == "Remanescente"),
                   Total = Abate + Remanescentes) %>%
            distinct(Scientific_Name, .keep_all = TRUE)

as_tibble(list_Species)

## Save the list of species
write.csv2(list_Species,
           "D:/Robson/home_office/Global-Level-Forest-Inventory/output/listSpecies.csv",
           row.names = TRUE)
#---------------------------List of species to be cut-------------------------##
autex <- FI_port %>%
        select(Name, Scientific_Name, Status, Destination, vol) %>%
        group_by(Scientific_Name) %>%
        mutate(count_cut = sum(Destination == "Abate"),
               count_Rem = sum(Destination == "Remanescente"),
               count_total = count_cut + count_Rem,
               vol_total = sum(vol)) %>%
        group_by(Scientific_Name, Destination == "Abate") %>%
        filter(count_cut >= 1) %>%
        mutate(cut_vol = sum(vol)) %>%
        ungroup() %>%
        mutate(cut_rem = vol_total - cut_vol) %>%
        ungroup() %>%
        distinct(Scientific_Name, .keep_all = TRUE) %>%
        select(-c(5,10))

as_tibble(autex)

## Save the List of species to be cut
write.csv2(autex,
           "D:/Robson/home_office/Global-Level-Forest-Inventory/output/autex.csv",
           row.names = TRUE)

#-----------------------Total Volume and Basal Area----------------------------#
FI_port %>%
  group_by(UT) %>%
  summarize(vol_sum = sum(vol),
            vol_avg = mean(vol),
            G_sum = sum(G),
            G_avg = mean(G))




#----------------------Cutting Volume and Basal Area---------------------------#
cutVol <- FI_port[which(FI_port$Destination == "Abate"), ]
tapply(cutVol$vol, cutVol$UT, mean)
sapply(split(cutVol$vol, cutVol$UT), sum)
sapply(split(cutVol$G, cutVol$UT), sum)

#----------------------------Basal Area by DBH---------------------------------#
G_DBH <- aggregate.data.frame(FI_port$G, list(FI_port$class2), sum) 
names_G <- c("DBH", "Basal_Area")
colnames(G_DBH) <- names_G
datatable(G_DBH, rownames = FALSE,
          options = list(pageLength = 12, autoWidth = FALSE),
          colnames = c("DBH (cm)", "Basal Area (m2)"))

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
        # scale_y_continuous(limits = 2000)) +
        theme(legend.position = c(0.9, 0.7))

png("D:/Robson/home_office/Global-Level-Forest-Inventory/output/basalAreaDBH.png",
    width = 640, 
    height = 480,
    units = "px")  

dev.off()

#-------------------------Plot Destination by DBH------------------------------#

#--------------------------Criterion 3 to 4 trees------------------------------#
crit_3.4 <- FI_filter %>%
        select(UT, Scientific_Name, Destination, Status, AEM) %>%
        group_by(UT, Scientific_Name) %>%
        mutate(Abate = sum(Destination == "Abate"), 
               Remanescentes = sum(Destination == "Remanescente"),
               Total = Abate + Remanescentes,
               Crit = if_else(Status == "Nao Protegida", 
                              ceiling(3*AEM/100), 
                              ceiling(4*AEM/100)),
               Analysis = if_else(Remanescentes >= Crit | Abate == 0,
                                  "Atende", 
                                  "Nao Atende")) %>%
        distinct(AEM, .keep_all = TRUE) %>%
        select(-c(AEM,Destination))
        
as_tibble(crit_3.4)

write.csv2(crit_3.4, 
           "C:/Robson/home_office/Global-Level-Forest-Inventory/output/crit_3_4_trees.csv",
           row.names = FALSE)

datatable(head(crit_3.4), 
          filter = 'top', options = list(pageLength = 10, 
                                         autoWidth = TRUE),
          callback = JS('table.page("next").draw(false);'))


#--------------------------Criterion 10 to 15 percent--------------------------#
crit_10.15 <- FI_filter %>%
        select(Scientific_Name, Destination, Status, AEM) %>%
        group_by(Scientific_Name) %>%
        mutate(Abate = sum(Destination == "Abate"), 
               Remanescentes = sum(Destination == "Remanescente"),
               Total = Abate + Remanescentes,
               PercRem = round(Remanescentes / Total * 100),
               Crit = if_else(Status == "Nao Protegida", (10), (15)),
               Analysis = if_else(PercRem >= Crit | Abate == 0,
                            "Atende", 
                            "Nao Atende")) %>%
        distinct(Scientific_Name, .keep_all = TRUE) %>%
        select(-c(AEM,Destination)) %>%
        filter(Abate != 0)

write.csv2(crit_10.15, 
           "C:/Robson/home_office/Global-Level-Forest-Inventory/output/crit_10_15_trees.csv",
           row.names = FALSE)

datatable(head(crit_10.15), class = 'cell-border stripe')
