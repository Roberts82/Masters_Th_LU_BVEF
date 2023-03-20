install.packages("tidyr")
install.packages("dplyr")
install.packages("cellranger")
install.packages("writexl")
install.packages("openxlsx")

library(tidyr)
library(dplyr)
library(cellranger)
library(writexl)
library(openxlsx)

#INWARD 2000-2020 data (million EUR) - 2021 at the moment is not available, but when available - will need to rerun the code with the new data
inw_split_lv_wide <- X01_TI_datu_valstu_dalij_INWARD %>% #splits a column
  # Split the Teritorija column into kods and teritorija columns
  separate(Teritorija, into = c("kods", "teritorija"), sep = " ", extra = "merge", fill = "right")
inw_split_lv_wide

#wide to long
inw_lv_00_20 <- inw_split_lv_wide %>%
  gather(key = "Year",
         value = "FDI", c(-kods, -teritorija)) %>%
  arrange(teritorija) %>% 
  mutate("Subject" = "INWARD", "Measure" = "MLN_EUR") #adds columns - INWARD&Measure
inw_lv_00_20

write.csv(inw_lv_00_20, "inw_lv_00_20.csv") #save as csv file

#INWARD 1992-2010 (thousand EUR)
inw_split_lv_wide_92 <- A_IVG060_20211129_173315_1992_2010_tkst %>% #splits a column
  # Split the Teritorija column into kods and teritorija columns
  separate(Valsts, into = c("kods", "teritorija"), sep = " ", extra = "merge", fill = "right")
inw_split_lv_wide_92

#wide to long
inw_lv_92 <- inw_split_lv_wide_92 %>%
  gather(key = "Year",
         value = "FDI", c(-kods, -teritorija)) %>%
  arrange(teritorija) %>% 
  mutate("Subject" = "INWARD", "Measure" = "MLN_EUR") #adds columns - INWARD&Measure
inw_lv_92

#convert to million
inw_lv_92_mil <- mutate(inw_lv_92, FDI = as.numeric(FDI)/1000)
inw_lv_92_mil

#save as csv file
write.csv(inw_lv_92_mil, "inw_lv_92_mil.csv")

#differences between a&b and b&a - to align the names in data sets in the next step
atskir20 <- as.data.frame(setdiff(inw_lv_00_20$teritorija, inw_lv_92_mil$teritorija))
atskir92 <- as.data.frame(setdiff(inw_lv_92_mil$teritorija, inw_lv_00_20$teritorija))

#to set the names of variables for year 92 - 2010 data set to be consistent with year names in year 2000-20 data set
inw_lv_92_mil_1 <- inw_lv_92_mil %>%
  mutate(teritorija=replace(teritorija, teritorija=="Džersija","Džērsija")) %>%
  mutate(teritorija=replace(teritorija, teritorija=="Gibraltārs (Lielbritānija)", "Gibraltārs")) %>%
  mutate(teritorija=replace(teritorija, teritorija=="Islande", "Īslande")) %>%
  mutate(teritorija=replace(teritorija, teritorija=="Menas sala", "Mena")) %>%
  mutate(kods=replace(kods, kods=="UK", "GB")) %>% # šis konstatēts vēlā, veidojot tabulu ar kodiem un teritorijām
#remove 2000-2010 as these years are covered in the data set of LB (2000-2020) (not to duplicate after merging)
  filter(Year<2000)

#bind 92-99 data to 00-2020 data
DataFDI_LV_DTT <- bind_rows(inw_lv_00_20, inw_lv_92_mil_1)%>%
  arrange(teritorija, Year)


#adding OUTWARD

#OUTWARD 2000-2020
outw_split_lv_wide <- X01_TI_datu_valstu_dalij_OUTWARD %>% #splits a column
  # Split the Teritorija column into kods and teritorija columns
  separate(Teritorija, into = c("kods", "teritorija"), sep = " ", extra = "merge", fill = "right")
outw_split_lv_wide
#wide to long
outw_lv_00_20 <- outw_split_lv_wide %>%
  gather(key = "Year",
         value = "FDI", c(-kods, -teritorija)) %>%
  arrange(teritorija) %>%
  mutate("Subject" = "OUTWARD", "Measure" = "MLN_EUR") #adds columnS - OUTWARD&Measure
outw_lv_00_20
outw_lv_00_20$...1 <- NULL
write.csv(outw_lv_00_20, "outw_lv_00_20.csv") #save as csv file

setdiff(outw_lv_00_20$teritorija, DataFDI_LV_DTT$teritorija)
setdiff(DataFDI_LV_DTT$teritorija, outw_lv_00_20$teritorija)

outw_lv_00_20$Year <- as.character(outw_lv_00_20$Year)
DataFDI_LV_DTT <- full_join(DataFDI_LV_DTT, outw_lv_00_20) %>%
  arrange(teritorija, Year)

#To filter out all data on non-country? Maybe can be left for now, but decided later when working with models.
#Maybe will be needed to calculate %. Maybe will be needed to align the names of 2 data sets.
#Not a bad idea to leave it as row as possible now. NAs are left as well.
unique(DataFDI_LV_DTT$teritorija)

#add column to the merged data set with date in the right format (to be able to extract the year of effect)
library(lubridate)
k1 <- konvsardf2 %>%
  mutate(Piemērojama=as.Date(Piemērojama, "%d.%m.%y")) %>%
  mutate(KonvPiemGads=as.numeric(year(Piemērojama)))
k1

#just to check the data type
class(k1$Piemērojama)
class(konvsardf1)

#select the columns (names of jurisdictions and years when DTTs became effective in respect
#to a particular jurisdiction) to be able join the data to the main data set in the next steps
inforce <- k1 %>% select(1, 6)

#to join the column on the years when DTTs became effective
DataFDI_LV_DTT <- left_join(DataFDI_LV_DTT, inforce, by=c("teritorija"="Valsts"), character())

#to calculate the number of years DTTs were in force regarding each year of each jurisdiction
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT, KonvGadi=as.numeric(Year)-as.numeric(KonvPiemGads)+1)
#replace NAs with 0s first
index <- is.na(DataFDI_LV_DTT$KonvGadi)
DataFDI_LV_DTT$KonvGadi[index] <- 0
#remove negative number KonvGadi (to replace them with "0")
DataFDI_LV_DTT$KonvGadi[DataFDI_LV_DTT$KonvGadi<0] <- 0


#remove negative values of FDI, as well as replace 0 with 0.1 (for logs)
DataFDI_LV_DTT$FDI[DataFDI_LV_DTT$FDI<=0] <- 0.1


#Vai tabulā pirms logarifmēšanas nevajaga 0 un negatīvajām vērtībām noteikt lielumu 0.1, kā tas izdarīts Berthel etc. pētījumā?
#vai arī +1, kā citā darbā. Joprojām problēma, jo log ir negatīvas vērtības.
#log of FDI
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT, "FDI_log"=log(FDI)) %>%
  relocate(FDI_log, .after = FDI)
DataFDI_LV_DTT


DataFDI_LV_DTT$Konv <- ifelse(DataFDI_LV_DTT$KonvGadi >= 1, '1', '0')
#or
#DataFDI_LV_DTT$Konv <- ifelse(DataFDI_LV_DTT$KonvGadi >= 1, 'TRUE', 'FALSE')

CIT_1980_2021 <- X1980_2021_Corporate_Tax_Rates_Around_the_World

#should I convert the type "Year" to factor? There are some cons
#convert year in CIT dataset to "character" as it is in the main dataset
CIT_1980_2021$year <- as.character(CIT_1980_2021$year)


#CIT_1980_2021$...1 <- NULL #zaudēja aktualitāti, kad df tika veidota savādāk, kā sākumā

write.csv(CIT_1980_2021, "CIT_1980_2021.csv") #save as csv file

DataFDI_LV_DTT <- DataFDI_LV_DTT %>%
  left_join(CIT_1980_2021, by=c("kods"="iso_2", "Year"="year")) %>%
  #reorder columns; leave the row index blank to keep all rows
  select("kods", "iso_3", "continent", "country", "teritorija", "Year", "FDI", "FDI_log", "Subject", "Measure", "KonvPiemGads", "KonvGadi", "Konv", "gdp", "rate", "oecd", "eu27", "gseven", "gtwenty", "brics")
DataFDI_LV_DTT

# add data on Distance between from jurisdictions to the Latvian border
DataFDI_LV_DTT <- DataFDI_LV_DTT %>%
  left_join(LatvDistAllignNames, by=c("country"="pays2"), across()) %>%
  relocate(dist, .after = Konv)
DataFDI_LV_DTT
#DataFDI_LV_DTT$dist <- DataFDI_LV_DTT$dist + 10 #lai izvairīties no "0", jo tās logafirmējot sanāk "bezgalība"
DataFDI_LV_DTT$dist <- ifelse(DataFDI_LV_DTT$dist == 0, 10, DataFDI_LV_DTT$dist)
  #DataFDI_LV_DTT$dist + 10 #lai izvairīties no "0", jo tās logafirmējot sanāk "bezgalība"


# dalība ES kopā ar LV - (1) gadi, kad attiecīgā valsts bija ES (izveidot jaunu kolonnu. Vesu būs jāizdzēš, ja ja jauna būs labāka))
EsValstis <- DataFDI_LV_DTT %>%
  filter(eu27 == 1) %>%
  distinct(kods)
EsValstis

DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT, euLV=ifelse(kods=="AT"&Year>=2004|kods=="BE"&Year>=2004|kods=="BG"&Year>=2007|kods=="CZ"&Year>=2004|
                                                   kods=="DK"&Year>=2004|kods=="ES"&Year>=2004|kods=="FR"&Year>=2004|
                                                   kods=="GR"&Year>=2004|kods=="HR"&Year>=2013|kods=="EE"&Year>=2004|
                                                   kods=="IE"&Year>=2004|kods=="IT"&Year>=2004|kods=="CY"&Year>=2004|
                                                   kods=="LT"&Year>=2004|kods=="LU"&Year>=2004|kods=="MT"&Year>=2004|
                                                   kods=="NL"&Year>=2004|kods=="PL"&Year>=2004|kods=="PT"&Year>=2004|
                                                   kods=="RO"&Year>=2007|kods=="SK"&Year>=2004|kods=="SI"&Year>=2004|
                                                   kods=="FI"&Year>=2004|kods=="HU"&Year>=2004|kods=="DE"&Year>=2004|
                                                   kods=="SE"&Year>=2004|kods=="GB"&Year>=2004&Year<2020, 1, 0), .after = eu27)

# the first step to creat a df of jurisdictions Blacklisted by LV
KodiUnTerit <- DataFDI_LV_DTT %>%
  select(kods, iso_3, teritorija) %>%
  filter(!kods %in% nevalstis) %>%
  distinct()
KodiUnTerit

write_xlsx(KodiUnTerit,"KodiUnTerit.xlsx")


# territories blacklisted by Latvia
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,BlacklistLv=ifelse(kods=="VI"&Year>=2001&Year<2022|kods=="AD"&Year>=1995&Year<2013|
                                                          kods=="AI"&Year>=1995&Year<2013|kods=="AG"&Year>=1995&Year<2013|
                                                          kods=="AE"&Year>=1995&Year<2013|kods=="BS"&Year>=1995&Year<2017|
                                                          kods=="BZ"&Year>=1995&Year<2013|kods=="BM"&Year>=1995&Year<2013|
                                                          kods=="VG"&Year>=1995&Year<2013|kods=="DM"&Year>=1995&Year<2017|
                                                          kods=="JE"&Year>=1995&Year<2013|kods=="GG"&Year>=1995&Year<2013|
                                                          kods=="GI"&Year>=1995&Year<2013|kods=="HK"&Year>=1995&Year<2013|
                                                          kods=="JO"&Year>=1995&Year<2017|kods=="KY"&Year>=1995&Year<2013|
                                                          kods=="QA"&Year>=2001&Year<2013|kods=="CY"&Year>=1995&Year<2013|
                                                          kods=="CW"&Year>=2013&Year<2017|kods=="CR"&Year>=1995&Year<2013|
                                                          kods=="KW"&Year>=1995&Year<2013|kods=="LB"&Year>=1995&Year<2013|
                                                          kods=="LR"&Year>=1995&Year<2017|kods=="LI"&Year>=2001&Year<2013|
                                                          kods=="MH"&Year>=1995&Year<2013|kods=="MU"&Year>=2001&Year<2013|
                                                          kods=="IM"&Year>=1995&Year<2013|kods=="PA"&Year>=1995&Year<2013&Year>2020|
                                                          kods=="SM"&Year>=1995&Year<2013|kods=="SC"&Year>=1995&Year<2013&Year>2020&Year<2022|
                                                          kods=="KN"&Year>=1995&Year<2013|kods=="VC"&Year>=1995&Year<2013|
                                                          kods=="SG"&Year>=1995&Year<1997|kods=="TC"&Year>=1995&Year<2013|
                                                          kods=="TO"&Year>=1995&Year<2017|kods=="UY"&Year>=1995&Year<2013|
                                                          kods=="VU"&Year>=2001|kods=="VE"&Year>=1995&Year<2013, 1, 0), .after = euLV)



#Bilateral Investment Treaties with Latvia
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
      InvestLig=ifelse(kods=="US"&Year>=1997|kods=="GB"&Year>=1995|
      kods=="AT"&Year>=1996|kods=="BY"&Year>=1999|kods=="BE"&Year>=1999|
      kods=="CZ"&Year>=1996|kods=="IT"&Year>=1999|kods=="IL"&Year>=1995|                                     
      kods=="DK"&Year>=1995|kods=="EG"&Year>=1998|kods=="CA"&Year>=1996|                                     
      kods=="FR"&Year>=1995|kods=="GR"&Year>=1998|kods=="KR"&Year>=1997|                                     
      kods=="EE"&Year>=1996|kods=="IS"&Year>=1999|kods=="LT"&Year>=1997|                                     
      kods=="LU"&Year>=1999|kods=="NO"&Year>=1993|kods=="PL"&Year>=1994|                                   
      kods=="NL"&Year>=1995|kods=="PT"&Year>=1998|kods=="SG"&Year>=1999|                           
      kods=="SK"&Year>=1999|kods=="FI"&Year>=1993|kods=="CH"&Year>=1993|                                
      kods=="TW"&Year>=1994|kods=="TR"&Year>=1999|kods=="UA"&Year>=1998|                                    
      kods=="UZ"&Year>=1997|kods=="DE"&Year>=1996|kods=="VN"&Year>=1996|                                   
      kods=="SE"&Year>=1993|kods=="ES"&Year>=1997, 1, 0), .after = euLV)
                                           
#precizēts datu tips
sapply(DataFDI_LV_DTT, mode)                                           
DataFDI_LV_DTT$gdp <- as.numeric(DataFDI_LV_DTT$gdp, na.action=na.omit)
DataFDI_LV_DTT$rate <- as.numeric(DataFDI_LV_DTT$rate, na.action=na.omit)

#Pievienots GDP PP (WB dati)
#wide to long
GdpPP_Long<- GDP_PP_WIDE %>%
  gather(key = "Year",
         value = "gdp_pp", c(-country, -iso_3)) %>%
  arrange(country, Year)
GdpPP_Long

write.csv(GdpPP_Long, "GdpPP_Long.csv") #save as csv file

DataFDI_LV_DTT <- DataFDI_LV_DTT %>%
  left_join(GdpPP_Long %>% dplyr::select(-country), by=c("iso_3", "Year"), .after = gdp)
DataFDI_LV_DTT
#neizdevās, lai kolonna būtu vajadzīgajā vietā. Būs vai nu jāpārtaisa, vai jāsarindo pareizi. "contry" tagad kā pēdējā kolonna, kas būs jāizlabo.

# Pievienot Offshore Financial Centres (OFC)

OfcTerit <- c("VG", "TW", "JE", "BM", "KY", "WS", "LI", "CW", "MT", "MU", "LU", "NR", "CY",
         "SC", "BS", "BZ", "GI", "AI", "LR", "VC", "GY", "HK", "MC", "NL", "GB", "CH", "SG", "IE")

DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
      ofc=ifelse(kods %in% OfcTerit, 1, 0), .after = BlacklistLv)

#sagatavošanās fails kā pirmais solis datu kopas par WHT veidošanai
WhtDttKodam <- LvKonvWht1 %>%
  left_join(KodiUnTerit, by = c("teritorija"), .before = teritorija)

write.xlsx(WhtDttKodam, 'WhtDttKodam.xlsx')


#1) creating an empty column with the right name; 2) assigning the value with case_when()
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
                                     WhtDtt10a=NA, .after = Konv)


  DataFDI_LV_DTT$WhtDtt10a <- case_when(
    DataFDI_LV_DTT$kods=="IT"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="QA"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="CY"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="CH"&DataFDI_LV_DTT$KonvGadi>=17|
      DataFDI_LV_DTT$kods=="TJ"&DataFDI_LV_DTT$KonvGadi>=1 ~ 0,
    DataFDI_LV_DTT$kods=="AL"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="US"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="AM"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="AE"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="AT"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="AZ"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="BE"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="BG"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="CZ"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="DK"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="KR"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="FR"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="GR"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="GE"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="HK"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="HR"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="EE"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="IE"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="IS"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="IL"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="CA"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="KZ"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="KG"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="RU"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="KW"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="CN"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="GB"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="LT"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="LU"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="MT"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="MX"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="ME"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="NL"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="NO"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="PL"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="RS"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="SG"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="SI"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="ES"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="FI"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="CH"&DataFDI_LV_DTT$KonvGadi>=1&DataFDI_LV_DTT$KonvGadi<17|
      DataFDI_LV_DTT$kods=="TM"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="UA"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="HU"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="DE"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="VN"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="SE"&DataFDI_LV_DTT$KonvGadi>=1 ~ 0.05,
    DataFDI_LV_DTT$kods=="MA"&DataFDI_LV_DTT$KonvGadi>=1 ~ 0.06,
    DataFDI_LV_DTT$kods=="BY"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="IN"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="JP"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="MD"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="PT"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="RO"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="SK"&DataFDI_LV_DTT$KonvGadi>=1|DataFDI_LV_DTT$kods=="TR"&DataFDI_LV_DTT$KonvGadi>=1|
      DataFDI_LV_DTT$kods=="UZ"&DataFDI_LV_DTT$KonvGadi>=1 ~ 0.1
  )


DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
                      CIT_LV=NA, .after = gdp)

DataFDI_LV_DTT$CIT_LV <- case_when(
  DataFDI_LV_DTT$Year>=1992&DataFDI_LV_DTT$Year<2002 ~ 0.25,
        DataFDI_LV_DTT$Year==2002 ~ 0.22,
        DataFDI_LV_DTT$Year==2003 ~ 0.19,
        DataFDI_LV_DTT$Year>=2003&DataFDI_LV_DTT$Year<2018 ~ 0.15,
        DataFDI_LV_DTT$Year>=2018 ~ 0.20
  )


DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
                       WhtDivLv=NA, .before = WhtDtt10a)

DataFDI_LV_DTT$WhtDivLv <- case_when(
  DataFDI_LV_DTT$Year>=1992&DataFDI_LV_DTT$Year<1995 ~ 0,
  DataFDI_LV_DTT$Year==1995 ~ 0.1,
  DataFDI_LV_DTT$Year>=1996&DataFDI_LV_DTT$Year<2004&DataFDI_LV_DTT$BlacklistLv==0 ~ 0.1,
  DataFDI_LV_DTT$Year>=2004&DataFDI_LV_DTT$Year<2013&DataFDI_LV_DTT$BlacklistLv==0&DataFDI_LV_DTT$euLV==0 ~ 0.1,
  DataFDI_LV_DTT$Year>=2004&DataFDI_LV_DTT$Year<2013&DataFDI_LV_DTT$BlacklistLv==0&DataFDI_LV_DTT$euLV==1 ~ 0,
  DataFDI_LV_DTT$Year>=2013&DataFDI_LV_DTT$BlacklistLv==0 ~ 0,
  DataFDI_LV_DTT$Year>=1996&DataFDI_LV_DTT$Year<2002&DataFDI_LV_DTT$BlacklistLv==1 ~ 0.25,
  DataFDI_LV_DTT$Year>=2002&DataFDI_LV_DTT$Year<2018&DataFDI_LV_DTT$BlacklistLv==1 ~ 0.15,
  DataFDI_LV_DTT$Year>=2018&DataFDI_LV_DTT$BlacklistLv==1 ~ 0.20
  )


DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
                                      DomTaxRel=NA, .after = rate)



DataFDI_LV_DTT$DomTaxRel <- case_when(
  DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="AE"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="AU"&DataFDI_LV_DTT$Year>=1992|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="AT"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="BE"&DataFDI_LV_DTT$Year>=1992|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="BG"&DataFDI_LV_DTT$Year>=2004|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="CA"&DataFDI_LV_DTT$Year>=1992|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="CH"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="CY"&DataFDI_LV_DTT$Year>=1992|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="CZ"&DataFDI_LV_DTT$Year>=2004|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="DE"&DataFDI_LV_DTT$Year>=2001|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="DK"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="ES"&DataFDI_LV_DTT$Year>=2000|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="EE"&DataFDI_LV_DTT$Year>=2005|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="FI"&DataFDI_LV_DTT$Year>=1992|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="FR"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="GB"&DataFDI_LV_DTT$Year>=2009|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="GE"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="GG"&DataFDI_LV_DTT$Year>=1992|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="GI"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="GR"&DataFDI_LV_DTT$Year>=2011|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="HK"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="HR"&DataFDI_LV_DTT$Year>=1992|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="HU"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="IM"&DataFDI_LV_DTT$Year>=1992|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="IE"&DataFDI_LV_DTT$Year>=2004|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="IS"&DataFDI_LV_DTT$Year>=2004|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="IL"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="IT"&DataFDI_LV_DTT$Year>=1992|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="JE"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="JP"&DataFDI_LV_DTT$Year>=2009|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="LI"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="LT"&DataFDI_LV_DTT$Year>=1992|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="LU"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="MT"&DataFDI_LV_DTT$Year>=1992|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="NL"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="NO"&DataFDI_LV_DTT$Year>=2004|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="NZ"&DataFDI_LV_DTT$Year>=1992|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="PL"&DataFDI_LV_DTT$Year>=2004|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="PT"&DataFDI_LV_DTT$Year>=2004|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="RO"&DataFDI_LV_DTT$Year>=2004|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="SK"&DataFDI_LV_DTT$Year>=2004|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="SI"&DataFDI_LV_DTT$Year>=2004|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="SE"&DataFDI_LV_DTT$Year>=2004|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="TR"&DataFDI_LV_DTT$Year>=2005|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="US"&DataFDI_LV_DTT$Year>=2018|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="SG"&DataFDI_LV_DTT$Year>=2003 ~ "Exemption",
  DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="ES"&DataFDI_LV_DTT$Year<2000|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="EE"&DataFDI_LV_DTT$Year<2005|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="GB"&DataFDI_LV_DTT$Year<2009|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="GR"&DataFDI_LV_DTT$Year<2011|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="MT"&DataFDI_LV_DTT$Year<2030|DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="NO"&DataFDI_LV_DTT$Year<2004|
    DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="RO"&DataFDI_LV_DTT$Year<2004 ~ "IndirectCredit",
  DataFDI_LV_DTT$Subject=="INWARD"&DataFDI_LV_DTT$kods=="CZ"&DataFDI_LV_DTT$Year<2004 ~ "Deduction",
  DataFDI_LV_DTT$kods=="RU"|DataFDI_LV_DTT$kods=="SK"&DataFDI_LV_DTT$Year<2004 ~ "NoRelief",
  !DataFDI_LV_DTT$kods %in% nevalstis&TRUE ~ "DirectCredit"
  )


DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
                                      DttTaxRel=NA, .after = DomTaxRel)

DataFDI_LV_DTT$DttTaxRel <- case_when(
  DataFDI_LV_DTT$kods=="AT"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="BE"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="CA"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="CH"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="CY"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="DE"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="DK"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="ES"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="FI"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="HR"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="IS"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="LT"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="NL"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="NO"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="SE"&DataFDI_LV_DTT$KonvGadi>0 ~ "Exemption",
  DataFDI_LV_DTT$kods=="AL"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="AE"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="AM"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="AZ"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="BY"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="GE"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="HU"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="IN"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="KZ"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="KG"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="MA"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="ME"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="PT"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="RU"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="RS"&DataFDI_LV_DTT$KonvGadi>0|DataFDI_LV_DTT$kods=="TJ"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="TM"&DataFDI_LV_DTT$KonvGadi>0|
    DataFDI_LV_DTT$kods=="VN"&DataFDI_LV_DTT$KonvGadi>0 ~ "DirectCredit",
  DataFDI_LV_DTT$KonvGadi>0&TRUE ~ "IndirectCredit"
)


#ExempTest <- DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT, FavourTaxRel=NA, .after = DttTaxRel)

DataFDI_LV_DTT$DomTaxRel <- factor(DataFDI_LV_DTT$DomTaxRel, levels = c("Exemption", "IndirectCredit", "DirectCredit", "Deduction", "NoRelief"), ordered = TRUE)
DataFDI_LV_DTT$DttTaxRel <- factor(DataFDI_LV_DTT$DttTaxRel, levels = c("Exemption", "IndirectCredit", "DirectCredit", "Deduction", "NoRelief"), ordered = TRUE)
levels(DataFDI_LV_DTT$DomTaxRel)

#ExempTest <- subset(ExempTest, select = -FavourTaxRel)
DataFDI_LV_DTT <- subset(DataFDI_LV_DTT, select = -FavourTaxRel)

DataFDI_LV_DTT$FavourTaxRel <- pmin(DataFDI_LV_DTT$DomTaxRel, DataFDI_LV_DTT$DttTaxRel, na.rm = TRUE)

#to get the column names not to forget any when reordering
colnames(DataFDI_LV_DTT)

#to reorder to make more sense
DataFDI_LV_DTT <- DataFDI_LV_DTT %>% select("kods", "iso_3", "teritorija", "country", "Year", "FDI", "FDI_log", "Subject", "Measure",
                                       "KonvPiemGads", "Konv", "KonvGadi", "WhtDivLv", "WhtDtt10a", "CIT_LV", "rate", "DomTaxRel",  "DttTaxRel",
                                       "FavourTaxRel", "BlacklistLv", "ofc", "euLV", "InvestLig", "gdp", "gdp_pp", "continent", "dist")

#to rename to be more consistent with other varible names
DataFDI_LV_DTT <- rename(DataFDI_LV_DTT, c("CIT_foreign"="rate"))

#to be more convenient to calculate
DataFDI_LV_DTT$CIT_foreign <- as.numeric(DataFDI_LV_DTT$CIT_foreign/100)

#a new column with "tax distance" taking into account DTTs
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT, TaxDistWithDTT=NA, .after = FavourTaxRel)

DataFDI_LV_DTT$TaxDistWithDTT <- case_when(
  DataFDI_LV_DTT$FavourTaxRel=="NoRelief" ~ 1 - (1 - DataFDI_LV_DTT$CIT_LV)*(1 - pmin(DataFDI_LV_DTT$WhtDivLv, DataFDI_LV_DTT$WhtDtt10a, na.rm = TRUE)) +
    DataFDI_LV_DTT$CIT_foreign - DataFDI_LV_DTT$CIT_LV*DataFDI_LV_DTT$CIT_foreign,
  DataFDI_LV_DTT$FavourTaxRel=="Deduction" ~ 1 - (1 - DataFDI_LV_DTT$CIT_LV)*(1 - pmin(DataFDI_LV_DTT$WhtDivLv, DataFDI_LV_DTT$WhtDtt10a, na.rm = TRUE)) *
    (1 - DataFDI_LV_DTT$CIT_foreign),
  DataFDI_LV_DTT$FavourTaxRel=="DirectCredit" ~ pmax(1 - (1 - DataFDI_LV_DTT$CIT_LV)*(1 - pmin(DataFDI_LV_DTT$WhtDivLv, DataFDI_LV_DTT$WhtDtt10a, na.rm = TRUE)),
                                                1 - (1 - DataFDI_LV_DTT$CIT_LV)*(1 - DataFDI_LV_DTT$CIT_foreign), na.rm = TRUE),
  DataFDI_LV_DTT$FavourTaxRel=="IndirectCredit" ~ pmax(1 - (1 - DataFDI_LV_DTT$CIT_LV)*(1 - pmin(DataFDI_LV_DTT$WhtDivLv, DataFDI_LV_DTT$WhtDtt10a, na.rm = TRUE)),
                                                  DataFDI_LV_DTT$CIT_foreign, na.rm = TRUE),
  DataFDI_LV_DTT$FavourTaxRel=="Exemption" ~ 1 - (1 - DataFDI_LV_DTT$CIT_LV)*(1 - pmin(DataFDI_LV_DTT$WhtDivLv, DataFDI_LV_DTT$WhtDtt10a, na.rm = TRUE))
)

#a new column with "tax distance" ignoring account DTTs
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT, TaxDistIgnoringDTT=NA, .after = TaxDistWithDTT)

DataFDI_LV_DTT$TaxDistIgnoringDTT <- case_when(
  DataFDI_LV_DTT$DomTaxRel=="NoRelief" ~ 1-(1 - DataFDI_LV_DTT$CIT_LV)*(1-DataFDI_LV_DTT$WhtDivLv) +
    DataFDI_LV_DTT$CIT_foreign - DataFDI_LV_DTT$CIT_LV*DataFDI_LV_DTT$CIT_foreign,
  DataFDI_LV_DTT$DomTaxRel=="Deduction" ~ 1 - (1 - DataFDI_LV_DTT$CIT_LV)*(1 - DataFDI_LV_DTT$WhtDivLv) * 
    (1 - DataFDI_LV_DTT$CIT_foreign),
  DataFDI_LV_DTT$DomTaxRel=="DirectCredit" ~ pmax(1 - (1 - DataFDI_LV_DTT$CIT_LV)*(1 - DataFDI_LV_DTT$WhtDivLv),
                                                     1 - (1 - DataFDI_LV_DTT$CIT_LV)*(1 - DataFDI_LV_DTT$CIT_foreign), na.rm = TRUE),
  DataFDI_LV_DTT$DomTaxRel=="IndirectCredit" ~ pmax(1 - (1 - DataFDI_LV_DTT$CIT_LV)*(1 - DataFDI_LV_DTT$WhtDivLv),
                                                       DataFDI_LV_DTT$CIT_foreign, na.rm = TRUE),
  DataFDI_LV_DTT$DomTaxRel=="Exemption" ~ 1 - (1 - DataFDI_LV_DTT$CIT_LV)*(1 - DataFDI_LV_DTT$WhtDivLv)
)

DataFDI_LV_DTT$TaxSavingPpDTT <- NULL #if needed to remove before rerun the code
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT, TaxSavingPpDTT=DataFDI_LV_DTT$TaxDistIgnoringDTT - DataFDI_LV_DTT$TaxDistWithDTT, .after = TaxDistIgnoringDTT)

#if needed to remove before rerun the code
DataFDI_LV_DTT$DivRelevantDTT <- NULL
DataFDI_LV_DTT$DivIrrelevantDTT <- NULL

# a new variable - Dividend Relevant DTTs
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
                         DivRelevantDTT=ifelse(Konv==1&TaxSavingPpDTT>0, 1, 0), .after = TaxSavingPpDTT)

# a new variable - Dividend Irrelevant DTTs
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
                         DivIrrelevantDTT=ifelse(Konv==1&TaxSavingPpDTT==0, 1, 0), .after = DivRelevantDTT)

# A column "StatusDTT" with 3 variables (factors - "DivRelevantDTT", "DivIrrelevantDTT", "NoDTT")

DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT, StatusDTT=NA, .before = BlacklistLv)

DataFDI_LV_DTT$StatusDTT <- case_when(
  DataFDI_LV_DTT$Konv==1&DataFDI_LV_DTT$TaxSavingPpDTT>0 ~ "DivRelevantDTT",
  DataFDI_LV_DTT$Konv==1&DataFDI_LV_DTT$TaxSavingPpDTT==0 ~ "DivIrrelevantDTT",
  DataFDI_LV_DTT$Konv==0&!DataFDI_LV_DTT$kods %in% nevalstis&TRUE ~ "NoDTT"
)

DataFDI_LV_DTT$StatusDTT <- factor(DataFDI_LV_DTT$StatusDTT, levels = c("NoDTT", "DivIrrelevantDTT", "DivRelevantDTT"), ordered = TRUE)

#if needed to remove before rerun the code
DataFDI_LV_DTT$DtRelExemption <- NULL
DataFDI_LV_DTT$DtRelIndirectCredit <- NULL
DataFDI_LV_DTT$DtRelDeduction <- NULL
DataFDI_LV_DTT$DtRelNoRelief <- NULL

#dummy var - Relief methods
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
                         DtRelExemption=ifelse(FavourTaxRel=="Exemption", 1, 0), .before = BlacklistLv)
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
                         DtRelIndirectCredit=ifelse(FavourTaxRel=="IndirectCredit", 1, 0), .before = BlacklistLv)
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
                         DtRelDirectCredit=ifelse(FavourTaxRel=="DirectCredit", 1, 0), .before = BlacklistLv)
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
                         DtRelDeduction=ifelse(FavourTaxRel=="Deduction", 1, 0), .before = BlacklistLv)
DataFDI_LV_DTT <- mutate(DataFDI_LV_DTT,
                         DtRelNoRelief=ifelse(FavourTaxRel=="NoRelief", 1, 0), .before = BlacklistLv)

DataFDI_LV_DTT$DtRelExemption <- as.factor(DataFDI_LV_DTT$DtRelExemption)
DataFDI_LV_DTT$DtRelIndirectCredit <- as.factor(DataFDI_LV_DTT$DtRelIndirectCredit)
DataFDI_LV_DTT$DtRelDeduction <- as.factor(DataFDI_LV_DTT$DtRelDeduction)
DataFDI_LV_DTT$DtRelNoRelief <- as.factor(DataFDI_LV_DTT$DtRelNoRelief)

#create a backap for the main dataset
backap210522DataFDI_LV_DTTS <- DataFDI_LV_DTT

write.csv(DataFDI_LV_DTT, "DataFDI_LV_DTT.csv") #save as csv file

