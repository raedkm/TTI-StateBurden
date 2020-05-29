#---------------------------------------------#
#Project : State specific Burden of diabetes due to TRAP - 2019
#Part : (03) Creating county maps for burden estimates
#Created by Raed Alotaibi
#Date Created: May-28-2020
#Last Modified: May-28-2020
#---------------------------------------------#



# Install packages --------------------------------------------------------

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", 
                   "rnaturalearthdata", "maps"))

# Loading packages -------------------------------------------------------
library("ggplot2")
library("sf")
library("ggspatial")
library("rnaturalearth")
library("rnaturalearthdata")
library("maps")
library("tools")


# Setting theme -----------------------------------------------------------
theme_set(theme_bw())





# getting country data
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)





# Getting state data ------------------------------------------------------
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
states <- cbind(states, st_coordinates(st_centroid(states)))

#converting to title case
states$ID <- toTitleCase(states$ID)
head(states)





# Drawing maps ------------------------------------------------------------

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_text(data = states, aes(X, Y, label = ID), size = 5) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)





ggplot(data = world) +
  geom_sf(fill = "antiquewhite1") +
  geom_sf(data = counties, aes(fill = area)) +
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
  geom_sf(data = flcities) +
  geom_text_repel(data = flcities, aes(x = lng, y = lat, label = city), 
                  fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  geom_label(data = states, aes(X, Y, label = ID), size = 5, fontface = "bold", 
             nudge_y = states$nudge_y) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Observation Sites", subtitle = "(2 sites in Palm Beach County, Florida)") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))