#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part    : (08) Creating Interactive Tables for the 500 Cities
#Purpose : Creating Interactive Tables for the 500 Cities
#Created by Raed Alotaibi
#Date Created : Aug-6-2019
#Last Modified: Aug-8-2019
#---------------------------------------------#





#Reading cities
cities_path <- "Data\\Cities.csv"
cities_var <- c("PlaceFIPS", "PlaceName")

cities <-  fread(cities_path, data.table = F, verbose = T, select = cities_var) %>%
  mutate(PLACEA = stri_sub(PlaceFIPS, -5, -1)) %>%
  mutate(PlaceFIPS = str_pad(PlaceFIPS, 7, pad = "0")) %>%
  mutate(FIPS = stri_sub(PlaceFIPS, 1,2)) %>%
  as_tibble()


# Preparing data set
burden_cities <- burden %>% 
  group_by(STATE, PlaceFIPS) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = round(mean(NO2), digits = 2),
            CASES = round(sum(CASES), digits = 0),
            AC = round(sum(AC),  digits = 0) ,
            AF = round((AC/CASES), digits = 2)) %>% 
  as_tibble() %>% 
  left_join(cities, by = "PlaceFIPS")  %>% 
  filter(!is.na(PlaceName)) %>% 
  select(PlaceFIPS, STATE, PlaceName,  TOTAL, CHILDREN, CASES, AC, AF, NO2 ) %>% 
  rename(`Place FIPS` = PlaceFIPS, 
         State = STATE,
         City = PlaceName,
         `Total Population` = TOTAL,
         `Total Children` = CHILDREN,
         `Incident Cases` = CASES,
         `Attributable Cases` = AC,
         `Attributable Fraction` = AF,
         `Mean NO2 Concentration (ug/m3)` = NO2)
  



# Interactive table
table_500 <- datatable(burden_cities, filter = 'top', 
          options = list(pageLength = 50, autoWidth = TRUE,  columnDefs = list(list(className = 'dt-center', targets = "_all"))), 
          class = 'cell-border stripe',
          editable = F,
          rownames = FALSE) 


class(table_500)

htmlwidgets::saveWidget(widget = table_500, libdir = "Results/Interactive/Cities/", file = "Cities500.html")

 