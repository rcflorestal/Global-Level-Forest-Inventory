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


# Close all previously created figures
graphics.off()

# Load packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(hrbrthemes)
library(ggalt)
library(DT)
library(jpeg)
library(lattice)
library(plotly)

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
FI <- read.table(
  'D:/Robson/home_office/Global-Level-Forest-Inventory/Data/IFpoa2.csv', 
                  header = TRUE, dec = ",")

# Show variables of forest inventory
as_tibble(FI)

# Read Infrastructure Sheet
infra <- read.csv2(
  'D:/Robson/home_office/Global-Level-Forest-Inventory/Data/poa2_infra.csv',
                   header = TRUE)

# Show variables of infrastructure
as_tibble(infra)

#------------------------------Exploratory Data--------------------------------#
summary(FI[c(1:3, 7:11)])  ## Summary Statics

dhb <- transform(FI, UT = factor(UT))

#png("densityKernelDBH.png")

boxplot(DBH ~ UT, dhb, xlab = "Plot")

boxplot(H ~ UT, dhb, xlab = "Plot")

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
     ylab = 'Density', 
     xlab = 'DBH (cm)',
     main = 'Kernel Density Estimates of Diameter at Breast Height \n in FLONA Caxiuanã UMF 2 UPA 2', 
     col = 'red')
lines(density(FI$DBH, 
              bw = 4),
      col = 'green')
lines(density(FI$DBH,
              bw = 6), 
      col='blue')
legend(x = 250, y = 0.03, 
       legend=c('BW:',
                'red: 2.5',
                'green: 4.0',
                'blue: 6.0'))

#dev.off()

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
            mutate(Abate = sum(Destination == "Cut"),
                   Remanescentes = sum(Destination == "Remaining"),
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
        mutate(count_cut = sum(Destination == "Cut"),
               count_Rem = sum(Destination == "Remaining"),
               count_total = count_cut + count_Rem,
               vol_total = sum(vol)) %>%
        group_by(Scientific_Name, Destination == "Cut") %>%
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
FI_vol <- FI_port %>%
  group_by(UT) %>%
  summarize(vol_sum = sum(vol),
            vol_avg = mean(vol),
            G_sum = sum(G),
            G_avg = mean(G))

#----------------------Cutting Volume and Basal Area---------------------------#
cutVol <- FI_port[which(FI_port$Destination == "Cut"), ]
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
                                          size = 13),
              plot.caption = element_text(color = "blue", face = "italic"),
              legend.position = c(0.9, 0.7)) +
        labs(title = "Basal Area per DBH",
             subtitle = "UPA 2 UMF 2 FLONA Caxiuanã",
             x = "Diamter at Breast Height (DBH - cm)",
             y = "Basal Area (m²)",
             caption = "Source: Florest Inventory POA 2020 Benevides Madeiras LTDA.",
             fill = "Basal Area")
        

## Basal Area per plot
FI_port %>%
        select(UT, G) %>%
        summarize(G_avg = sum(G)/14)  ## Get total mean basal area

FI_port %>%
        select(UT, G) %>%
        group_by(UT) %>%
        summarize(sum_G = sum(G)) %>%
        ggplot(aes(x = as.factor(UT), y = sum_G, fill = sum_G > 597.2242)) +
        geom_bar(position = "dodge",
                 stat = "identity") +
        scale_fill_manual(name = 'Basal Area:',
                    values = c("#778899", "#4682B4"),
                    labels = c('TRUE' = 'Above Average', 
                               'FALSE' = 'Below Average')) +
        theme(plot.title = element_text(color = "black",
                                        size = 14,
                                        face = "bold",
                                        hjust = 0.5),
              legend.position = "bottom") +
        labs(title = "Basal Area per Plot",
             y = "Basal Area (m²)",
             x = "Plot")
    

#-----------------------Plot Select cut trees per DBH-------------------------##
dest <- FI_filter %>%
  select(Scientific_Name, DBH, Destination, class2) %>%
  filter(!is.na(class2)) %>%
  group_by(class2, Destination) %>%
  summarize(N = n())

total <- FI_filter %>%
  count(class2, name = 'N') %>%
  mutate(Destination = 'Total')

sel <- bind_rows(dest, total)

sel %>%
        ggplot(aes(x = class2, y = N, fill = Destination)) +
                geom_bar(stat = 'identity', position = 'dodge') +
                theme(
                  plot.title = element_text(color = "black", 
                                            size = 14, 
                                            face = "bold", 
                                            hjust = 0.5),
                  plot.subtitle = element_text(color = "black", hjust = 0.5),
                  axis.title.x = element_text(color = "black", size = 14),
                  axis.title.y = element_text(color = "black", size = 14),
                  plot.caption = element_text(color = "blue", face = "italic")) +
                labs(title = "Trees Selected for cutting in UPA 2 UMF 2 FLONA Caxiuanã", 
                     x = 'DHB (cm)', 
                     y = 'Number of Trees',
                     caption = 'Source: Florest Inventory POA 2020 Benevides Madeiras LTDA.') +
                theme(legend.position = c(0.9,0.8))

        
#--------------------------Criterion 3 to 4 trees------------------------------#
crit_3.4 <- FI_filter %>%
        select(UT, Scientific_Name, Destination, Status, AEM) %>%
        group_by(UT, Scientific_Name) %>%
        mutate(Cut = sum(Destination == "Cut"), 
               Remaining = sum(Destination == "Remaining"),
               Total = Cut + Remaining,
               Crit = if_else(Status == "Nao Protegida", 
                              ceiling(3*AEM/100), 
                              ceiling(4*AEM/100)),
               Analysis = if_else(Remaining >= Crit | Cut == 0,
                                  "Atende", 
                                  "Nao Atende")) %>%
        distinct(AEM, .keep_all = TRUE) %>%
        select(-c(AEM,Destination))
        
as_tibble(crit_3.4)

write.csv2(crit_3.4, 
           "D:/Robson/home_office/Global-Level-Forest-Inventory/output/crit_3_4_trees.csv",
           row.names = FALSE)

datatable(head(crit_3.4), 
          filter = 'top', options = list(pageLength = 10, 
                                         autoWidth = TRUE),
          callback = JS('table.page("next").draw(false);'))

  
#--------------------------Criterion 10 to 15 percent--------------------------#
crit_10.15 <- FI_filter %>%
        select(Scientific_Name, Destination, Status, AEM) %>%
        group_by(Scientific_Name) %>%
        mutate(Cut = sum(Destination == "Cut"), 
               Remaining = sum(Destination == "Remaining"),
               Total = Cut + Remaining,
               PercRem = round(Remaining / Total * 100),
               Crit = if_else(Status == "Nao Protegida", (10), (15)),
               Analysis = if_else(PercRem >= Crit | Cut == 0,
                            "Atende", 
                            "Nao Atende")) %>%
        distinct(Scientific_Name, .keep_all = TRUE) %>%
        select(-c(AEM,Destination)) %>%
        filter(Cut != 0)

as_tibble(crit_10.15)

write.csv2(crit_10.15, 
           "D:/Robson/home_office/Global-Level-Forest-Inventory/output/crit_10_15_trees.csv",
           row.names = FALSE)


## Table 10 - 15 %
datatable(head(crit_10.15), class = 'cell-border stripe')

## Plot 10 - 15%
ggplot(crit_10.15, aes(x = PercRem,
                       xend = Crit, 
                       y = Scientific_Name, 
                       group = Scientific_Name)) + 
  geom_dumbbell(color = "#a3c4dc", 
                size = 0.85) + 
  scale_x_continuous(breaks =  seq(0,100,5)) +
  #xlim(10, 90) +
  labs(x = 'Percentage of Remaining Trees', 
       y = NULL, 
       title = "PERCENTAGE OF REMAIING TREES IN UPA 2 UMF 2", 
       subtitle = "POA 2020 - Haverst 2020-2021 - UMF 2 FLONA Caxiuanã", 
       caption = "Source: Florest Inventory of POA 2020 Benevides Madeiras LTDA.") +
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        plot.caption = element_text(color = "blue", face = "italic"),
        plot.background = element_rect(fill = "#f7f7f7"), 
        panel.background = element_rect(fill = "#D8D8D8"), #f7f7f7
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(),
        axis.ticks = element_blank(),
        legend.position = "top",
        panel.border = element_blank())

## Plot vol ~ UT and Status
xyplot(vol ~ UT | Status, data = FI_port, 
       main = "Volume as a function of plot and Ecological Status",
       xlab = "Plot")

## Factors
FI_port$QF <- as.factor(FI_port$QF)

## Density Plot by Stem Quality and DBH
densityplot(~ DBH | QF, data = FI_port, 
            main = "Density Plot by Stem Quality and DBH",
            xlab = "DBH (cm)")

## Density Plot by Stem Quality and Height
densityplot(~ H | QF, data = FI_port, 
            main = "Density Plot by Stem Quality and Height",
            xlab = "Height (m)")

## Density Plot by Stem Quality and Basal Area
densityplot(~ G | QF, data = FI_port, 
            main = "Density Plot by Stem Quality and Basal Area",
            xlab = "Basal Area (m2)")


# xyplot(G ~ QF | Status*Destination,
#        data = FI_port, strip = TRUE, pch = 10)

## Map
map <- FI_filter %>%
        filter(UT == 1) %>%
        mutate(Status = str_replace(Status, "^Em Perigo", "Endangered")) %>%
        mutate(Status = str_replace(Status, "^Nao Protegida", "Not Endangered")) %>%
        mutate(Status = str_replace(Status, "^Vulneravel", "Vulnerable ")) %>%
        ggplot() +
        geom_point(aes(x = East, y = North, 
                       color = Destination,
                       text = paste(
                               'Scientific_Name:',Scientific_Name,'<br>',
                                 'Volume(m3):', vol, '<br>',
                               'Status:',Status))) +
        theme(plot.title = element_text(hjust = 0.5),
              plot.caption = element_text(face = "italic", size = 7)) +
        labs(title = "Selection of Trees in plot 1.",
             caption = "DATUM SIRGAS2000, MC-51, UTM Zone 22.") +
        guides(y = guide_axis(angle = 90)) +
        theme_bw()
#map

ggplotly(map)
