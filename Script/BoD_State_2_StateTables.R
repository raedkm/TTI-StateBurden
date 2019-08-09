#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part : (02) Printing burden excel sheet
#Purpose: Conduct the burden modeling and estimate results by state/urban/income
#Created by Raed Alotaibi
#Date Created: March-12-2019
#Last Modified: Aug-8-2019
#---------------------------------------------#





# Result ----------------------------------------------------------------


# (1) Total

burden_1 <- burden %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))

# (2) State 

burden_2 <- burden %>% 
  group_by(STATE) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))


# (3) Urban status

burden_3 <- burden %>% 
  group_by(URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))


# (4) Income status

burden_4 <- burden %>% 
  group_by(INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))



# (5) State/Urban status

burden_5 <- burden %>% 
  group_by(STATE, URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))



# (6) State/Income status

burden_6 <- burden %>% 
  group_by(STATE, INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))


# (7) Urban/State status

burden_7 <- burden %>% 
  group_by(URBAN, STATE) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))


# (8) Income/State status

burden_8 <- burden %>% 
  group_by(INCOME, STATE) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))



# (9) Urban/Income status

burden_9 <- burden %>% 
  group_by(URBAN, INCOME ) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))



# (10) Income/Urban status

burden_10 <- burden %>% 
  group_by(INCOME, URBAN ) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))



# (11) State/Income/Urban status

burden_11 <- burden %>% 
  group_by(STATE, INCOME, URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))



# (12) State/Urban/Income status

burden_12 <- burden %>% 
  group_by(STATE, URBAN, INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, 0), AC = round(AC, 0), AF = round(AF, 1))


# Printing ----------------------------------------------------------------

burden_list <- list(burden_1, burden_2, burden_3, burden_4, burden_5, burden_6, burden_7, burden_8, burden_9, burden_10, burden_11, burden_12 )
sheet <- c("Total", "By State", "By Urban", "By Income", 
           "By State|Urban", "By State|Income", "By Urban|State", "By Income|State", "By Urban|Income", "By Income|Urban", "By State|Income|Urban", "By State|Urban|Income")

for(i in 1:length(sheet)) {
  print(i)
  print(sheet[i])
  # Printing Aggregated data to Excel
  xlsx::write.xlsx(burden_list[i], "Results/Tables/Burden_by_state.xlsx", sheetName = sheet[i], showNA=F, append = T, row.names = F)
  
  }



