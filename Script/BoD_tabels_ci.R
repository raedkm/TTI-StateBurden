gather

library(scales)

table_out <- gather %>% 
  mutate(
    `Incident cases` = comma(case_m,1),
    AC = comma(ac_m, 1),
    case_ci = paste0("(", comma(case_l,1), "-", comma(case_h,1), ")" ),
    ac_ci = paste0("(", comma(ac_l,1), "-", comma(ac_h,1), ")")) %>% 
  select(State, `Incident cases` , case_ci, AC, ac_ci) %>% 
  as.data.frame()


xlsx::write.xlsx(table_out, "Output/Tables/Table_ci.xlsx", row.names = F)


table_out2 <- gather2 %>% 
  mutate(
    `Incident cases` = comma(case_m,1),
    AC = comma(ac_m, 1),
    case_ci = paste0("(", comma(case_l,1), "-", comma(case_h,1), ")" ),
    ac_ci = paste0("(", comma(ac_l,1), "-", comma(ac_h,1), ")")) %>% 
  select(Criteria, `Incident cases` , case_ci, AC, ac_ci) %>% 
  as.data.frame()

xlsx::write.xlsx(table_out2, "Output/Tables/Table_ci_2.xlsx", row.names = F)
