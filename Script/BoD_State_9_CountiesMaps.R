#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part    : (09) Creating Interactive Maps for counties
#Purpose : Creating Interactive Maps for countiess
#Created by Raed Alotaibi
#Date Created : Aug-6-2019
#Last Modified: Aug-8-2019
#---------------------------------------------#



# Installing packages -----------------------------------------------------

# install.packages("purrr")
# install.packages("RColorBrewer")
# install.packages("leaflet")
# install.packages("Hmisc")
# install.packages("htmlwidgets")
# install.packages("sf")
# install.packages("rmapshaper")
# install.packages("tigris")


#loading libraries
library(purrr)
library(RColorBrewer)
library(leaflet)
library(Hmisc)
library(htmltools)
library(htmlwidgets)
library(sf)
library(rmapshaper)
library(tigris)


# Preparing county data set -------------------------------------------------

#County Names and FIPS code
CountyFIPS_path <- "Data//Shape//nhgis0047_csv//nhgis0047_ds172_2010_county.csv"
CountyFIPS_var <- c("GISJOIN", "STATE", "COUNTY")

#CountyFIPS <- fread(CountyFIPS_path, select = CountyFIPS_var)
CountyFIPS <- fread(CountyFIPS_path)


# County Burden
burden_county <- burden %>% 
  group_by(STATE, COUNTY) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES) %>% 
  as_tibble() %>% 
  left_join(CountyFIPS, by = c("STATE", "COUNTY"))


# Loading shape files -----------------------------------------------------


# County shape file

CountyShape_path <- "Data//Shape//US_county_2010_simple//US_county_2010.shp"

CountyShape <- st_read(CountyShape_path)



# Mergin burden file with shape file --------------------------------------

burden_CountyShape <- CountyShape %>% 
  right_join(burden_county, by = "GISJOIN") 



# Creating functions ------------------------------------------------------


#creating max function

my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)



# Brewing colors ----------------------------------------------------------

# Step 1: Creating colors palettes

# Brewing colors

rc1 <- colorRampPalette(colors = c("#41980a", "#ffe950" ), space = "Lab")(15)
rc2 <- colorRampPalette(colors = c("#ffe950", "#ff4300" ), space = "Lab")(10)
rc3 <- colorRampPalette(colors = c("#ff4300", "#8d1111" ), space = "Lab")(7)
rc4 <- colorRampPalette(colors = c("#8d1111", "#450606" ), space = "Lab")(8)
rc5 <- colorRampPalette(colors = c("#450606", "#000000" ), space = "Lab")(5)

rampcols <- c(rc1, rc2, rc3, rc4)
previewColors(colorNumeric(palette = rampcols , domain = NULL), values = 0:my.max(burden_CountyShape$NO2))

# Assigning brewed colors to pallette
NO2_pal <- colorNumeric(rampcols, domain = burden_CountyShape$NO2)


# Define common CRS for county shape files
burden_CountyShape <- st_transform(burden_CountyShape, 4326)

# Step 4: Creating hover labels

labels_2010 <- sprintf(
  "<strong>%s</strong><strong>, %s</strong> <br/>
    Population of children  %g<br/>
    Attributable case %g<br/>
    Percent of all cases %g<br/>
    Mean NO<sub>2</sub>  (ug/m<sup>3</sup>) %g",
  burden_CountyShape$NAME10,
  burden_CountyShape$STATE,
  burden_CountyShape$CHILDREN, 
  round(burden_CountyShape$AC), 
  round(burden_CountyShape$AF*100),
  round(burden_CountyShape$NO2, digits = 1)) %>% 
  lapply(htmltools::HTML)


# Step 5: creating legend and title
label_legend <- HTML("Percentage")
label_title <- sprintf('<p><strong><b>Burden of Childhood Asthma Due to NO<sub>2</sub> in 2010 </b>' )


#Step 6: Creating the Map
interactiveMap <- burden_CountyShape %>% 
  leaflet(options = leafletOptions( minZoom = 4, maxZoom = 9)) %>% 
  setMaxBounds( -60 , 25 , -130 , 50 ) %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(
              color = "White", weight = 0.15,  smoothFactor = 0.2,
              opacity = 1, fillOpacity = 0.9, 
              fillColor = ~NO2_pal(AF*100),
              highlightOptions = highlightOptions(color = "White", weight = 2, bringToFront = TRUE),
              label = ~labels_2010) %>%
  addLegend(group = "Percentage legend",
            pal = NO2_pal, values = ~burden_CountyShape$AF*100,
            labFormat = labelFormat(suffix = "%", between = "- "), #transform = function(x) round(x*100)),
            title = label_legend, position = "bottomright") %>% 
  addControl(label_title, position = "bottomleft")


# Saving the Maps
saveWidget(interactiveMap, file = "StateSpecific_Counties_leaf.html", selfcontained = F)




