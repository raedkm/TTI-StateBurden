#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part    : (04) Preparing original BoD estimates 
#Purpose : Read in census data, income data, NO2 conc, incidence rate (original study), and prevelance rate (original study)
#         Followed by joining the data sets 
#Created by Raed Alotaibi
#Date Created: May-06-2019
#Last Modified: Aug-8-2019
#---------------------------------------------#





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





# Joining Data Sets -------------------------------------------------------


# Joining census, income, NO2, asthma incidence and asthma prevalance data sets
join <- census2010 %>% 
  mutate(GISJOIN_i = substr(GISJOIN, 1, 15)) %>% 
  left_join(NO2_2010, by = "GISJOIN") %>%
  left_join(income_2010, by = "GISJOIN_i") %>% 
  mutate(IR = 0.0125, PRV = 0.137) %>% 
  select(-GISJOIN_i)

rm(census2010)
rm(NO2_2010)
rm(income_2010)




# Burden Modeling ---------------------------------------------------------

## Estimating (RR of new exposure: RRnew, Attributable fraction; AF, Attributable cases; AC with lower and upper limits)

crf <- 1.05
unit_inc <- 4

burden_c <- join %>%
  mutate(CASES = (CHILDREN - (CHILDREN * PRV)) * IR) %>% 
  mutate(RRnew = exp((log(crf)/unit_inc)*NO2)) %>% 
  mutate(AF = (RRnew - 1)/(RRnew)) %>% 
  mutate(AC = AF*CASES) 


rm(join)


# Summary estimates -------------------------------------------------------


BLOCK <- burden_c %>%
  count()

BLOCK_U <- burden_c %>% 
  group_by(URBAN) %>% 
  count()

CHILDREN <- burden_c %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            CASES = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/CASES * 100), 1)) 


CHILDREN_URBAN <- burden_c %>% 
  group_by(URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            CASES = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/CASES * 100), 1)) 


CHILDREN_INCOME <- burden_c %>% 
  group_by(INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            CASES = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/CASES * 100), 1))



# Results tables ----------------------------------------------------------


Table_1 <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME, CHILDREN, CASES, AC, AF) %>% as.data.frame()


POLLUT <- burden_c %>% 
  select(NO2) %>% 
  summary() %>% as.data.frame() 



# Printing to excel -------------------------------------------------------
xlsx::write.xlsx(Table_1, "Results/Tables/Table_c1.xlsx", row.names = F)
xlsx::write.xlsx(POLLUT, "Results/Tables/Table_c2.xlsx", row.names = F)


# Result ----------------------------------------------------------------


# (1) Total

burden_c_1 <- burden_c %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))

# (2) State 

burden_c_2 <- burden_c %>% 
  group_by(STATE) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))


# (3) Urban status

burden_c_3 <- burden_c %>% 
  group_by(URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))


# (4) Income status

burden_c_4 <- burden_c %>% 
  group_by(INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))



# (5) State/Urban status

burden_c_5 <- burden_c %>% 
  group_by(STATE, URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))



# (6) State/Income status

burden_c_6 <- burden_c %>% 
  group_by(STATE, INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))


# (7) Urban/State status

burden_c_7 <- burden_c %>% 
  group_by(URBAN, STATE) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))


# (8) Income/State status

burden_c_8 <- burden_c %>% 
  group_by(INCOME, STATE) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))



# (9) Urban/Income status

burden_c_9 <- burden_c %>% 
  group_by(URBAN, INCOME ) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))



# (10) Income/Urban status

burden_c_10 <- burden_c %>% 
  group_by(INCOME, URBAN ) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))



# (11) State/Income/Urban status

burden_c_11 <- burden_c %>% 
  group_by(STATE, INCOME, URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))



# (12) State/Urban/Income status

burden_c_12 <- burden_c %>% 
  group_by(STATE, URBAN, INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))


# Printing ----------------------------------------------------------------

burden_c_list <- list(burden_c_1, burden_c_2, burden_c_3, burden_c_4, burden_c_5, burden_c_6, burden_c_7, burden_c_8, burden_c_9, burden_c_10, burden_c_11, burden_c_12 )
sheet <- c("Total", "By State", "By Urban", "By Income", 
           "By State|Urban", "By State|Income", "By Urban|State", "By Income|State", "By Urban|Income", "By Income|Urban", "By State|Income|Urban", "By State|Urban|Income")

for(i in 1:length(sheet)) {
  print(i)
  print(sheet[i])
  # Printing Aggregated data to Excel
  xlsx::write.xlsx(burden_c_list[i], "Results/Tables/burden_by_state_c.xlsx", sheetName = sheet[i], showNA=F, append = T, row.names = F)
  
}









