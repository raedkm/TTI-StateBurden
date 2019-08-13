#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part    : (01) Preparing data sets 
#Purpose : Read in census data, income data, NO2 conc, incidence rate (state-specifi), and prevelance rate (state, specific)
#         Followed by joining the data sets and replacing missing IR/PRV with weighted averages
#Created by Raed Alotaibi
#Date Created: March-12-2019
#Last Modified: Aug-8-2019
#---------------------------------------------#
 



# Increasing memory allocation --------------------------------------------

memory.size()
memory.limit(3210241024*1024)

# to change the allocation size change the first two numbers on the left >> memory.limit(XX10241024*1024)


# Installing packages -----------------------------------------------------

# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("data.table")
# install.packages("readxl")
# install.packages("stringr")
# install.packages("stringi")
# install.packages("DT")
# install.packages("htmltools")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("forcats")






# Loading packagesa -------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(stringr)
library(stringi)
library(DT)
library(htmltools)
library(ggplot2)
library(scales)
library(forcats)

# Loading census data -----------------------------------------------------


#Year 2010 data

census2010_path <- "Data\\Census\\nhgis0043_ds172_2010_block.csv"

census2010_var <- c("GISJOIN", "YEAR", "STATE", "STATEA", "H7V001", 
                    "H7W003", "H7W004", "H7W005", "H7W006",      
                    "H76003", "H76004", "H76005", "H76006", 
                    "H76027", "H76028", "H76029", "H76030", 
                    "PLACEA", "ZCTA5A", "SDELMA", "SDSECA", "SDUNIA", "SABINSA",
                    "COUNTY", "COUNTYA") #For information on what the variables are, please refer to the codebook


# Making new variable of total children count "CHILDREN" and renaming variables
census2010 <- fread(file = census2010_path,data.table = F, stringsAsFactors = F, verbose = F, select = census2010_var) %>% 
  filter(H7V001 > 0) %>% 
  mutate(CHILDREN = H76003 + H76004 + H76005 +H76006 +
           H76027 + H76028 + H76029 + H76030) %>% 
  mutate(FIPS = stri_sub(GISJOIN, 2,3)) %>%
  mutate(PLACEA = str_pad(PLACEA, 5, pad = "0")) %>%
  mutate(PlaceFIPS = paste0(FIPS, PLACEA)) %>% 
  mutate(URBAN = case_when(
    H7W003 > 0 ~ "Urbanized area", 
    H7W004 > 0 ~ "Urban cluster", 
    H7W005 > 0 ~ "Rural", 
    H7W006 > 0 ~ "Not defined",
    TRUE ~ "Not defined" 
  )) %>% 
  rename(TOTAL = H7V001) %>% 
  select(GISJOIN, YEAR, FIPS, STATEA, COUNTYA, PlaceFIPS, PLACEA, ZCTA5A, SDELMA, SDSECA, SDUNIA, SABINSA,  STATE, COUNTY, URBAN, TOTAL,  CHILDREN) %>% 
  as_tibble()




# Loading median houshold income -------------------------------------

#Year 2010 data

income_2010_path <- "Data\\Census\\nhgis0043_ds176_20105_2010_blck_grp.csv"

income_2010_var <- c("GISJOIN", "JOIE001")

income_2010 <- fread(file = income_2010_path, data.table = F, stringsAsFactors = F, verbose = F,  select = income_2010_var) %>% 
  rename(GISJOIN_i = GISJOIN,
         M_INCOME = JOIE001) %>%
  mutate(INCOME = case_when(
           M_INCOME < 20000 ~ "<$20,000", 
           between(M_INCOME, 20000, 34999) ~ "$20,000 to <$35,000", 
           between(M_INCOME, 35000, 49999) ~ "$35,000 to <$50,000", 
           between(M_INCOME, 50000, 74999) ~ "$50,000 to <$75,000", 
           M_INCOME >= 75000  ~ ">=$75,000", 
           TRUE ~ "Not defined")) %>% 
  as_tibble()  





# Loading NO2 data --------------------------------------------------------

#Year 2010 data
NO2_2010_path <-"Data\\Pollutant\\NO2_2010.csv"
NO2_2010_var <- c("GISJOIN", "Y2010")

NO2_2010 <- fread(NO2_2010_path, data.table = F, stringsAsFactors = F,  verbose = F, select = NO2_2010_var) %>% 
  mutate(NO2 = Y2010*1.88) %>% 
  select(GISJOIN, NO2) %>% 
  as_tibble()






# Loading state-specific asthma IR data -----------------------------------

path_inc <- "Data/Asthma/Asthma_IR_ver2.xlsx"

inc <- read_excel(path_inc, sheet = "Aggregate") %>% 
  mutate(IR = as.double(`IR per 1000`/1000)) %>% 
  mutate(FIPS = str_pad(FIPS, 2, pad = "0")) %>% 
  select(FIPS, IR)


# Estimating weighted IR
IR <- read_excel(path_inc, sheet = "Aggregate") %>%   
  summarise(IR = (sum(`<12_month`/YEARS)/sum(At_risk/YEARS)))

# Assiggning weighted IR after manually dividing by the number of years states participated in.
weighted_IR <- IR[[1]]





# Loading state-specific asthma  PRV rate ---------------------------------

path_prv <- "Data/Asthma/Asthma_PRV_ver2.xlsx"

prv <- read_excel(path_prv, sheet = "Aggregate") %>% 
  mutate(PRV = as.double(`PRV per 100`/100)) %>% 
  mutate(FIPS = str_pad(FIPS, 2, pad = "0")) %>% 
  select(FIPS, PRV)


# Estimating weighted PRV
PRV <- read_excel(path_prv, sheet = "Aggregate") %>%   
  summarise(PRV = sum(EVER/YEARS)/sum(SAMPLE/YEARS))

weighted_PRV <- PRV[[1]] 



# Joining Data Sets -------------------------------------------------------

# Joining census, income, NO2, asthma incidence and asthma prevalance data sets
join <- census2010 %>% 
            mutate(GISJOIN_i = substr(GISJOIN, 1, 15)) %>% 
            left_join(NO2_2010, by = "GISJOIN") %>%
            left_join(income_2010, by = "GISJOIN_i") %>% 
            left_join(inc, by = "FIPS") %>% 
            left_join(prv, by = "FIPS") %>% 
            replace_na(list(IR = weighted_IR, PRV = weighted_PRV)) %>% 
            select(-GISJOIN_i)

rm(census2010)
rm(NO2_2010)
rm(income_2010)

# Burden Modeling ---------------------------------------------------------

## Estimating (RR of new exposure: RRnew, Attributable fraction; AF, Attributable cases; AC with lower and upper limits)

crf <- 1.05
unit_inc <- 4

burden <- join %>% 
  mutate(CASES = (CHILDREN - (CHILDREN * PRV)) * IR) %>% 
  mutate(RRnew = exp((log(crf)/unit_inc)*NO2)) %>% 
  mutate(AF = (RRnew - 1)/(RRnew)) %>% 
  mutate(AC = AF*CASES) 

rm(join)



  
  
