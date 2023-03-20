head()
summary()

#what is the difference with as.data.frame() ?
data.frame()

#install dplyr
install.packages("dplyr")
library(dplyr)

#a nicer view for dataframes
data <- tbl_df()

print(dataset, n = 1000)

library(dplyr)
dataset %>%
  summarize_all(mean)

citrev %>%
  group_by(Country) %>%
  arrange(desc(`Tax revenue`))

citrev %>%
  group_by(Year) %>%
  arrange(desc(`Tax revenue`))

class()
typeof()
attributes()

colnames(citrev)
nrow(citrev)
ncol(citrev)

dim(citrev)

#structure of data frame - name, type and preview of data in each column
str(citrev)

#shows the class of each column in the data frame
sapply(citrev, class)

class(citrev)

citrev %>%
  filter(Country == "Latvia")

citrev %>%
  filter(Country == "Lithuania")

citrev %>%
  filter(Country == "Estonia")

citrev %>%
  group_by(Country) %>%
  summarise(
    n = n(),
    Value = mean(Value, na.rm = TRUE)
  ) %>%
  filter(
    n > 1,
    Value > 50
  )

class(inw_lv_00_20)

#valstu saraksta izveidošana
valstusar <- tbl_df(inw_lv_00_20$teritorija)%>%
  unique()
#kolonnas nosaukšana
colnames(valstusar) <- c("teritorija")
valstusar

#vai setdiff no dplyr šeit nebūtu jāizmanto, lai pārliecinātos, ka neviena valsts nav izlaista?

#datu izvilkšana no konvenciju saraksta
typeof(lv_konv_fm)

konvsardf <- as.data.frame(lv_konv_fm) %>%
  na.omit(konvsardf)
konvsardf1 <- as.data.frame(konvsardf[1:66,])
konvsardf1

#this is optional. Seems doesn't change anything for this case
install.packages("tibble")
library("tibble")

#creating a df for LV DTTs
konvsardf1 <- as.data.frame(konvsardf[1:66,]) %>%
  tbl_df(konvsardf1)
konvsardf1

#changing names of df of DTTs to be consistent with the main data set.
#the process of finding different names is in the next step (setdiff())
konvsardf2 <- konvsardf1%>%
  mutate(Valsts=replace(Valsts, Valsts=="Dienvidkoreja","Korejas Republika")) %>%
  mutate(Valsts=replace(Valsts, Valsts=="Islande","Īslande")) %>%
  mutate(Valsts=replace(Valsts, Valsts=="Kirgīzija","Kirgizstāna")) %>%
  mutate(Valsts=replace(Valsts, Valsts=="–Ķīnas Tautas Republika","Ķīna")) %>%
  mutate(Valsts=replace(Valsts, Valsts=="Lielbritānija","Apvienotā Karaliste")) %>%
  mutate(Valsts=replace(Valsts, Valsts=="Vācijas Federatīvā Republika","Vācija"))
konvsardf2

#to check consistency of names. If so, go back to cure the differences
setdiff(konvsardf1$Valsts, valstusar$teritorija)

#Valstis ar konvencijām
IrKonv <- DataFDI_LV_DTT %>%
  filter(Year %in% c(2020)&Konv %in% c(1)) %>%
  select(kods, teritorija, KonvPiemGads)%>%
  arrange(KonvPiemGads)
IrKonv

#Valstis, kurām nav konvenciju
NavKonv <- DataFDI_LV_DTT %>%
  filter(Year %in% c(2020)&Konv %in% c(0)&!kods %in% nevalstis)%>%
  select(kods, teritorija)
NavKonv


# USEFUL FUNCTION
str(konvsardf1) 

str(konvsardf)

typeof()

summary()

# work with a package to convert dates

# save as an excel document - DOESN'T WORK
install.packages("xlsx")
library("xlsx")
write.xlsx(konvsardf1, file = "LvKonvWht.xlsx", sheetName = "LvKonvWht", append = FALSE)

install.packages("rio", "openxlsx")
export(konvsardf2, "LvKonvWhtRenamed.xlsx")
