#data for calculations Full_Inw2
summary(Full_Inw2)
unique(Full_Inw2$teritorija)

#1992
inw_92 <- Full_Inw2 %>%
  filter(Year == 1992) %>%
  arrange(desc(FDI))
inw_92

#2020
inw_20 <- Full_Inw2 %>%
  filter(Year == 2020) %>%
  arrange(desc(FDI))
inw_20