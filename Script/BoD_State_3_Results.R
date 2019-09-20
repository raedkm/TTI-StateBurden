#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part : (03) Printing Results tables
#Purpose: Conduct the burden modeling and estimate results by state/urban/income
#Created by Raed Alotaibi
#Date Created: April-22-2019
#Last Modified: Aug-8-2019
#---------------------------------------------#



# Summary estimates -------------------------------------------------------


BLOCK <- burden %>%
  count()

BLOCK_U <- burden %>% 
  group_by(URBAN) %>% 
  count()


TOTAL <- burden %>% 
  summarise(TOTAL = sum(TOTAL))


CHILDREN <- burden %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            CASES = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/CASES * 100), 1)) 
  

CHILDREN_URBAN <- burden %>% 
  group_by(URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            CASES = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/CASES * 100), 1)) 


CHILDREN_INCOME <- burden %>% 
  group_by(INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            CASES = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/CASES * 100), 1))



# Results tables ----------------------------------------------------------


table_1 <- bind_rows(TOTAL, CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME, TOTAL, CHILDREN, CASES, AC, AF) %>% as.data.frame()

table_2 <- bind_rows(BLOCK, BLOCK_U, TOTAL, CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME, n, TOTAL, CHILDREN) %>% 
  rename(BLOCK_n = n) %>% as.data.frame()
  
table_3 <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME,CASES) %>% as.data.frame()

table_4 <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME,AC, AF) %>% as.data.frame()


table_p1 <- burden %>% 
  select(NO2) %>% 
  summarise( Mean = mean(NO2),
             Min = min(NO2), 
             first = quantile(NO2, .25),
             Median = median(NO2),
             third = quantile(NO2, .75),
             Max = max(NO2)) %>% as.data.frame() 


table_p2 <- burden %>% 
  group_by(URBAN) %>% 
  select(NO2) %>% 
  summarise( Mean = mean(NO2),
             Min = min(NO2), 
             first = quantile(NO2, .25),
             Median = median(NO2),
             third = quantile(NO2, .75),
             Max = max(NO2)) %>% as.data.frame() 


table_p3 <- burden %>% 
  group_by(INCOME) %>% 
  select(NO2) %>% 
  summarise( Mean = mean(NO2),
             Min = min(NO2), 
             first = quantile(NO2, .25),
             Median = median(NO2),
             third = quantile(NO2, .75),
             Max = max(NO2)) %>% as.data.frame() 

table_5 <- table_p1 %>% 
  full_join(table_p2) %>% 
  full_join(table_p3) %>% 
  select(URBAN, INCOME, Mean, Min, first, Median, third, Max)

table_6 <- burden %>% 
  group_by(STATE) %>% 
  select(NO2) %>% 
  summarise( Mean = mean(NO2),
             Min = min(NO2), 
             first = quantile(NO2, .25),
             Median = median(NO2),
             third = quantile(NO2, .75),
             Max = max(NO2)) %>% as.data.frame() 

# Printing to excel -------------------------------------------------------

xlsx::write.xlsx(table_1, "Output/Tables/Table_1.xlsx", row.names = F)
xlsx::write.xlsx(table_2, "Output/Tables/Table_2.xlsx", row.names = F)
xlsx::write.xlsx(table_3, "Output/Tables/Table_3.xlsx", row.names = F)
xlsx::write.xlsx(table_4, "Output/Tables/Table_4.xlsx", row.names = F)
xlsx::write.xlsx(table_5, "Output/Tables/Table_5.xlsx", row.names = F)
xlsx::write.xlsx(table_6, "Output/Tables/Table_6.xlsx", row.names = F)


