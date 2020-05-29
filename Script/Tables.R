library(tidyverse)
library(xlsx)


tabled <- table1 %>% 
  mutate(case = paste0("(", format(round(as.numeric(case_low), 0), nsmall=0, big.mark=","), "-", format(round(as.numeric(case_up), 0), nsmall=0, big.mark=","), ")"),
         AC = paste0("(", format(round(as.numeric(AC_low), 0), nsmall=0, big.mark=","), "-", format(round(as.numeric(AC_up), 0), nsmall=0, big.mark=","), ")"),
         AF = paste0("(", AF_low, "-", AF_up, ")")
)



tabled <- table2 %>% 
  mutate(case = paste0("(", format(round(as.numeric(case_low), 0), nsmall=0, big.mark=","), "-", format(round(as.numeric(case_up), 0), nsmall=0, big.mark=","), ")"),
         AC = paste0("(", format(round(as.numeric(AC_low), 0), nsmall=0, big.mark=","), "-", format(round(as.numeric(AC_up), 0), nsmall=0, big.mark=","), ")"),
         AF = paste0("(", AF_low, "-", AF_up, ")")
  )




tabled <- table3 %>% 
  mutate(case = paste0("(", format(round(as.numeric(case_low), 0), nsmall=0, big.mark=","), "-", format(round(as.numeric(case_up), 0), nsmall=0, big.mark=","), ")"),
         AC = paste0("(", format(round(as.numeric(AC_low), 0), nsmall=0, big.mark=","), "-", format(round(as.numeric(AC_up), 0), nsmall=0, big.mark=","), ")"),
  )



write.xlsx(tabled, file = "myworkbook3.xlsx", sheetName = "tabled", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
