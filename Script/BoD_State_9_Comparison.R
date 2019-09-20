#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part    : (09) Comparison tables 
#Purpose : Read in census data, income data, NO2 conc, incidence rate (original study), and prevelance rate (original study)
#         Followed by joining the data sets 
#Created by Raed Alotaibi
#Date Created: May-07-2019
#Last Modified: Aug-8-2019
#---------------------------------------------#


# Defning function {myspread} --------------------------------------------

myspread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}


# Burden estimate ---------------------------------------------------------


burden_c <- burden_c %>% 
  mutate(type = "origin")

burden <- burden %>% 
  mutate(type = "state")

burden_join <- rbind(burden_c, burden)


burden_join %>% 
  group_by(type) %>% 
  summarise(CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES ) %>% 
  myspread(type, c(CASES, AC, AF))  
  


# Comparing overall  --------------------------------------------------

 #overall
 compare_1 <-  burden_join %>% 
   group_by(type) %>% 
   summarise(CASES = sum(CASES), AC = sum(AC))%>% 
   mutate(AF = AC/CASES ) %>% 
   myspread(type, c(CASES, AC, AF)) %>% 
   mutate(CASES_D = state_CASES - origin_CASES, CASES_P = (state_CASES - origin_CASES)/ origin_CASES ,
          AC_D = state_AC - origin_AC, AC_P = (state_AC - origin_AC) / origin_AC, 
          AF_D = state_AF - origin_AF, AF_P = (state_AF - origin_AF) / origin_AF) 
 
 #Urban
 compare_2 <-  burden_join %>% 
   group_by(URBAN,type) %>% 
   summarise(CASES = sum(CASES), AC = sum(AC))%>% 
   mutate(AF = AC/CASES ) %>% 
   myspread(type, c(CASES, AC, AF)) %>% 
   mutate(CASES_D = state_CASES - origin_CASES, CASES_P = (state_CASES - origin_CASES)/ origin_CASES ,
          AC_D = state_AC - origin_AC, AC_P = (state_AC - origin_AC) / origin_AC, 
          AF_D = state_AF - origin_AF, AF_P = (state_AF - origin_AF) / origin_AF) 

 
 #Income
 compare_3 <-  burden_join %>% 
   group_by(INCOME, type) %>% 
   summarise(CASES = sum(CASES), AC = sum(AC)) %>% 
   mutate(AF = AC/CASES ) %>% 
   myspread(type, c(CASES, AC, AF)) %>% 
   mutate(CASES_D = state_CASES - origin_CASES, CASES_P = (state_CASES - origin_CASES)/ origin_CASES,
          AC_D = state_AC - origin_AC, AC_P = (state_AC - origin_AC) / origin_AC, 
          AF_D = state_AF - origin_AF, AF_P = (state_AF - origin_AF) / origin_AF) 
 
 #Joining tables
 compare <- compare_1 %>% 
   full_join(compare_2) %>% 
   full_join(compare_3) %>% 
   select(URBAN,INCOME, state_CASES, origin_CASES, CASES_D, CASES_P, 
          state_AC, origin_AC, AC_D, AC_P, 
          state_AF, origin_AF, AF_D, AF_P) %>% 
   as.data.frame()
 
# Comparing state ---------------------------------------------------------


 #overall
 compare_state <-  burden_join %>% 
   group_by(type, STATE) %>% 
   summarise(CASES = sum(CASES), AC = sum(AC))%>% 
   mutate(AF = AC/CASES ) %>% 
   myspread(type, c(CASES, AC, AF)) %>% 
   mutate(CASES_D = state_CASES - origin_CASES, CASES_P = (state_CASES - origin_CASES)/ origin_CASES ,
          AC_D = state_AC - origin_AC, AC_P = (state_AC - origin_AC) / origin_AC, 
          AF_D = state_AF - origin_AF, AF_P = (state_AF - origin_AF) / origin_AF) %>% 
   select(STATE, state_CASES, origin_CASES, CASES_D, CASES_P, 
          state_AC, origin_AC, AC_D, AC_P, 
          state_AF, origin_AF, AF_D, AF_P) %>% 
   as.data.frame()

 

# Printing comparison tables ----------------------------------------------

 xlsx::write.xlsx(compare, "Output/Tables/Table_7.xlsx", row.names = F)
 xlsx::write.xlsx(compare_state, "Output/Tables/Table_8.xlsx", row.names = F) 
