library(scales)







breaks_m <- seq(0,0.5,0.05)

scale_y_bod <-   scale_y_continuous(breaks  = breaks_m,  labels = percent_format(accuracy = 1))
theme_bod <- 
  
  
  

# Top contenders ----------------------------------------------------------

  
burden_s %>%
  ggplot(aes(x= INCOME, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(outlier.size = 0.2) + 
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
  ggtitle("Attributable Fraction by Median Houshold Income") +
  scale_y_bod +
  ggsave("theme_dark.png" ,path = "Plots",  width = 9.5, height = 12,  dpi = 920, pointsize=12 )



burden_s %>%
  ggplot(aes(x= INCOME, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot(outlier.size = 0.2) + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
  ggtitle("Attributable Fraction by Median Houshold Income") +
  scale_y_bod +
  ggsave("theme_light.png" ,path = "Plots",  width = 9.5, height = 12,  dpi = 920, pointsize=12 )


# 10.	Attributable Fraction (AF)
burden_s %>%
  ggplot(aes(y = AF)) +
  geom_boxplot() + 
  theme_dark()+
  scale_y_continuous(breaks  = breaks_m,  labels = scales::percent_format(accuracy = 0.5))




# Theme testing -----------------------------------------------------------

burden_s %>%
  ggplot(aes(x= INCOME, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) +
  #ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
  scale_y_bod +
  ggsave("theme_bw.png" ,path = "Plots", width = 9.5, height = 12)


burden_s %>%
  ggplot(aes(x= INCOME, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
  #ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
  scale_y_bod +
  # coord_flip() + 
  ggsave("theme_classic.png" ,path = "Plots", width = 8.7, height = 10.7)



burden_s %>%
  ggplot(aes(x= INCOME, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
  #ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
  scale_y_bod +
  coord_flip() + 
  ggsave("theme_none.png" ,path = "Plots")






burden_s %>%
  ggplot(aes(x= INCOME, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_get() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
  #ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
  scale_y_bod +
  coord_flip() + 
  ggsave("theme_get.png" ,path = "Plots")



burden_s %>%
  ggplot(aes(x= INCOME, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_void() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
  #ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
  scale_y_bod +
  coord_flip() + 
  ggsave("theme_void.png" ,path = "Plots")




burden_s %>%
  ggplot(aes(x= INCOME, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
  #ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
  scale_y_bod +
  coord_flip() + 
  ggsave("theme_gray.png" ,path = "Plots")



burden_s %>%
  ggplot(aes(x= INCOME, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
  #ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
  scale_y_bod +
  coord_flip() + 
  ggsave("theme_linedraw.png" ,path = "Plots")




# Creating mean boxplots instead of median --------------------------------

# function for computing mean, DS, max and min values
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

stat_mean <- stat_summary(fun.data = min.mean.sd.max, geom = "boxplot")  
state_jitter <-   geom_jitter(position=position_jitter(width=.2), size=3)

# ggplot code
p1 <- ggplot(aes(y = value, x = factor(group)), data = mydata)
p1 <- p1 + stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") + geom_jitter(position=position_jitter(width=.2), size=3) 

# 12.	AF by state 
burden_s %>%
  ggplot(aes(x= reorder(STATE, AF, FUN = mean), y = AF)) +
  geom_boxplot() + 
  theme_bw()+
  theme_text2 +
  scale_y_bod2 




 