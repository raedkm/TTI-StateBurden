#########################################################
# Leaflet_Maps_03
# Author : Raed K Alotaibi
# Family: Leaflet Map
# Part : 02 - Leaflet function 
# Purpose : To create a function that takes two argument (Pollutant) and returns a leaflet map
# with two layers (2000, 2010) (the color scale here is standarised across pollutants)
##########################################################

# Loading packages
library(purrr)
library(readr)
library(data.table)
library(dplyr)
library(tidyr)  
library(ggplot2)
library(stringi)
library(stringr)
library(RColorBrewer)
library(qwraps2)
library(xlsx)
library(leaflet)
library(tigris)
library(Hmisc)
library(htmltools)
library(htmlwidgets)

#display.brewer.all()
#setwd("C:/Users/R-Alotaibi/OneDrive - Texas A&M Transportation Institute/Data sets final")

# loading the county data set
load("Data files/county_wide.Rda")
load("Data files/counties_shp.Rda")



# Creating the leaf_map function with one argument (pollutant)
leaf_map <- function(pollutant) {
  
  
  # Step 1: Creating colors palettes
  
  # Brewing colors
  rc1 <- colorRampPalette(colors = c("#41980a", "#ffe950" ), space = "Lab")(7)
  rc2 <- colorRampPalette(colors = c("#ffe950", "#ff4300" ), space = "Lab")(7)
  rc3 <- colorRampPalette(colors = c("#ff4300", "#8d1111" ), space = "Lab")(15)
  rc4 <- colorRampPalette(colors = c("#8d1111", "#450606" ), space = "Lab")(8)
  rc5 <- colorRampPalette(colors = c("#450606", "#000000" ), space = "Lab")(8)
  
  rampcols <- c(rc1, rc2, rc3, rc4, rc5)
  previewColors(colorNumeric(palette = rampcols , domain = NULL), values = 0:70)
  
  # Assigning brewed colors to pallette
  AF_pal <- colorNumeric(rampcols, domain = county_wide$AF*100)
  
  
  # Step 2: Subsetting pollutant
  county_wide_2 <- county_wide %>% filter(Pollutant == pollutant) %>% na.omit()
  county_2010 <- county_wide_2 %>% filter(year == 2010) 
  
  
  # Step 3: Joining shape files with data frame
  counties_2010 <- counties
  
  counties_2010@data <- left_join(counties@data,county_2010, by = "GEOID")
  
  
  
  # Step 4: Creating hover labels
  labels_2010 <- sprintf(
    "<strong>%s</strong><strong>, %s</strong> <br/>
    Population of children  %g<br/>
    Attributable case %g<br/>
    Percent of all cases %g<br/>
    Mean %s  (ug/m<sup>3</sup>) %g",
    counties_2010@data$NAME,
    counties_2000@data$state_name,
    counties_2010@data$total_children, 
    round(counties_2010@data$AC), 
    round(counties_2010@data$AF*100),
    pollutant,
    round(counties_2010@data$conc, digits = 1)) %>% 
    lapply(htmltools::HTML)
  
  # Step 5: creating legend and title
  label_legend <- HTML("Percentage")
  label_title <- sprintf('<p><strong><b>Burden of Childhood Asthma Due to %s</b>', pollutant )
  
  
  #Step 6: Creating the Map
  interactiveMap <- counties %>%
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 9)) %>% 
    setMaxBounds( -60 , 25 , -130 , 50 ) %>% 
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addPolygons(data = counties_2010,
                group = "2010",
                color = "White", weight = 0.15,  smoothFactor = 0.2,
                opacity = 1, fillOpacity = 0.9, 
                fillColor = ~AF_pal(AF*100),
                highlightOptions = highlightOptions(color = "White", weight = 2, bringToFront = TRUE),
                label = ~labels_2010) %>%
    addLegend(group = "Percentage legend",
              pal = AF_pal, values = ~county_wide$AF*100,
              labFormat = labelFormat(suffix = "%", between = "- "), #transform = function(x) round(x*100)),
              title = label_legend, position = "bottomright") %>% 
    addControl(label_title, position = "bottomleft")

  
  return(interactiveMap)
}





# Running the leaf_map function

NO2_map <- leaf_map('NO2')
NO2_map



# Saving the Maps
saveWidget(NO2_map, file = "C:/Users/R-Alotaibi/Desktop/BoD/Interactive_Maps/NO2_leaf_b.html")




