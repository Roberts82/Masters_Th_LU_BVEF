#inward 2000 & 2020 proc

#2000
inw_00 <- inw_lv_00_20 %>%
  filter(Year == 2000) %>%
  arrange(desc(FDI))
inw_00

#2020
inw_20 <- inw_lv_00_20 %>%
  filter(Year == 2020) %>%
  arrange(desc(FDI))
inw_20

#barplot - Doesn't work
#basic
pl_00 <- inw_lv_00_20 %>%
  select("teritorija", "FDI") %>%
  #filter(Year == 2000) %>%
  arrange(desc(FDI)) %>%
  ggplot(aes(x=Teritorija, y=FDI)) + geom_bar(stat = "identity")
pl_00
#horizontal
pl_00 + coord_flip()

p<-ggplot(data=inw_00, aes(x=teritorija, y=FDI)) +
  geom_bar(stat="identity")
p

# Horizontal bar plot
p + coord_flip()

inw_00_more1 <- inw_00 %>%
  filter(FDI>=1) %>%
  filter(!row_number() %in% c(1, 2, 3, 4))
inw_00_more1

p<-ggplot(data=inw_00_more1, aes(x=reorder(teritorija, FDI), y=FDI)) +
  geom_bar(stat="identity")
p

# Horizontal bar plot
p + coord_flip()


#2020
inw_20_more1 <- inw_20 %>%
  filter(FDI>=1) %>%
  filter(!row_number() %in% c(1, 2, 3, 16, 24, 36))
  inw_20_more1

p<-ggplot(data=inw_20_more1, aes(x=reorder(teritorija, FDI), y=FDI)) +
  geom_bar(stat="identity")
p

# Horizontal bar plot
p + coord_flip()

#facet grid
ggplot(data=inw_lv_00_20, aes(x=teritorija, y=FDI, group=Year)) + geom_line() +
  facet_wrap("teritorija")

ggplot(data=na.omit(inw_lv_00_20), aes(x=Year, y=FDI, group=teritorija)) + geom_col() +
  facet_wrap("teritorija")

#excluding totals (not to forget dplyr)
inw0020countr <- inw_lv_00_20 %>%
  na.omit() %>%
  filter(FDI>0&teritorija!="Kopā"&teritorija!="Eiropas Savienība 27 (kopā)"&teritorija!="Ārpus ES 27 (kopā)"&teritorija!="Nezināma ārpus ES valsts") %>%
  arrange(desc(FDI))
inw0020countr

ggplot(data=inw0020countr, aes(x=Year, y=FDI, group=teritorija)) + geom_col() +
  facet_wrap("teritorija", scales = "free")

#can I choose the format of facet_wrap? 