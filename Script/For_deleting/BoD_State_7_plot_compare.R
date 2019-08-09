#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Sub     : Burden comparison
#Purpose : To produce plots that compare the burden using national IR vs state-specific IR
#Created by Raed Alotaibi
#Created : July-22-2019
#Modified: July-22-2019
#---------------------------------------------#


library(ggplot2)
library(scales)
library(forcats)


# Preventing scientific notations
options(scipen=10000)



# Preparing data sets, joining old burden with new burden data sets -------


# Loading burden data from the main analysis using the national IR of 12.5 per 1,000  

Burden_old_path <- "Data\\Burden_old\\Burden_2010.csv"

Burden_old_var <- c("GISJOIN", "PAF")

Burden_old <- fread(file = Burden_old_path, data.table = F, stringsAsFactors = F, verbose = F, select = Burden_old_var) %>% 
  rename(OLD = PAF) %>% 
  as_tibble()
  

# Joining ol burden data set with new one
burden_join <- burden %>% 
  select(GISJOIN, STATE, URBAN, INCOME, AF) %>%
  rename(NEW = AF) %>% 
    left_join(Burden_old, by = "GISJOIN")



# Converting wide format to long format  -----------------------------------

burden_long <- burden_join %>% 
  gather("VERSION" , "PAF", -GISJOIN, -STATE,  -URBAN, -INCOME)




# Prepering plots ---------------------------------------------------------


# recoding INCOME levels
burden_s <- burden_long  %>%
  mutate(INCOME = recode(burden_long$INCOME,
                         "<20,000" = "<$20,000" ,
                         "20,000 to <35,000" = "$20,000 to <$35,000",
                         "35,000 to <50,000" = "$35,000 to <$50,000",
                         "50,000 to <75,000" = "$50,000 to <$75,000" ,
                         ">=75,000" = ">=$75,000"), 
         VERSION = recode(burden_long$VERSION, 
                          "OLD" = "OLD",
                          "NEW" = "NEW"))


# Assiging theme options

theme_text2 <- theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust=1),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.ticks.x=element_blank(),
                     strip.text = element_text(size = 9, 
                                               margin = margin(0.07,0,0.07,0,"cm")))



breaks_m <- seq(0,max(burden_s$PAF, na.rm = T),0.1)
scale_y_bod <-   scale_y_continuous(breaks  = breaks_m,  labels = percent_format(accuracy = 1))


mean_dot <- stat_summary(fun.y=mean, geom="point", shape=20, size=1, color="red", fill="red") 




# Plotting ----------------------------------------------------------------


#	AF by state 
burden_s %>%
  select(STATE, VERSION, PAF) %>% 
  ggplot(aes(x = VERSION, y = PAF,  col = VERSION)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("AF_state_compare.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)



#	AF by state and living location
burden_s %>%
  select(STATE, VERSION, URBAN, PAF) %>% 
  ggplot(aes(x = URBAN, y = PAF, fill= URBAN, col = VERSION)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("AF_Urban_state_compare.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)


# AF by state and income
burden_s %>%
  select(STATE, VERSION, INCOME , PAF) %>% 
  filter(INCOME != "Not defined") %>% 
  ggplot(aes(x = INCOME, y = PAF, fill= INCOME, col = VERSION)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.1) + 
  theme_bw() +
  theme_text2 +
  scale_y_bod +
  #mean_dot +
  scale_colour_manual(values=c("black", "red"), aesthetics = "colour")+
  scale_fill_manual(values=c("white","white","white","white", "white","white","white"))+
  ggsave("AF_Income_state_compare.png" ,path = "Results/Plots", width = 9.5, height = 12,  dpi = 360, pointsize=3)



