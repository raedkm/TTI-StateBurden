#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part    : (05) Creating final data sets 
#Purpose : Creat finale data sets at multiple geographical levels
#Created by Raed Alotaibi
#Date Created : Aug-5-2019
#Last Modified: Aug-8-2019
#---------------------------------------------#




# Creating teh data sets --------------------------------------------------


burden_county <- burden %>% 
  group_by(STATE, COUNTY) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES)


burden_place <- burden %>% 
  group_by(STATE, PLACEA) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES)


burden_zip <- burden %>% 
  group_by(STATE, ZCTA5A) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES)


burden_elementry <- burden %>% 
  filter(SDELMA != 99999) %>% 
  group_by(STATE, SDELMA) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES)

burden_secondary <- burden %>% 
  filter(SDSECA != 99999) %>%
  group_by(STATE, SDSECA) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES)

burden_unified <- burden %>% 
  filter(SDUNIA != 99999) %>%
  group_by(STATE, SDUNIA) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES)



# Writing the data sets ---------------------------------------------------

write_csv(burden, path = "Output_Data/burden_all.csv")  
write_csv(burden_county ,path = "Output_Data/burden_county.csv")
write_csv(burden_place ,path = "Output_Data/burden_place.csv")
write_csv(burden_zip ,path = "Output_Data/burden_zip.csv")
write_csv(burden_elementry ,path = "Output_Data/burden_elementry.csv")
write_csv(burden_secondary ,path = "Output_Data/burden_secondary.csv")
write_csv(burden_unified ,path = "Output_Data/burden_unified.csv")
