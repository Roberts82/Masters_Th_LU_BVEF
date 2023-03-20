install.packages("plm")
install.packages("stringr")
install.packages("ggrepel")
install.packages("corrplot")
install.packages("ggcorrplot")
library(plm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(stringr)
library(corrplot)
library(ggcorrplot)


# git remote add origin https://github.com/Roberts82/LU_Masters_Thesis.git
# git branch -M main
# git push -u origin main
# 
# git branch --set-upstream-to=origin/<branch> main

git branch --set-upstream-to=7f752a1 main

git push -u origin HEAD

19-11-35-81_timeSeries.csv

#to filter out unnecessary values
unique(DttsPlm$teritorija)
unique(DttsPlm$kods)

#nevalstis - vektors, kas satur sagatavotās datu kopas mainīgos, nav jāņem vērā analīzē, jo
#satur datus ne par valstīm. Tās ir:
#"Kopā"
#"Eiropas Savienība 27 (kopā)
#"Pārējās valstis"
#"Ārpus ES 27 (kopā)"
#"Nezināma ārpus ES valsts"
#"Eiropas Savienības institūcijas (izņemot Eirozonas institūcijas)"
#"Nav sadalīts pa valstīm"
#"Nezināma Eiropas Savienības valsts (izņemot Eiro zonas valstis)
#"Starptautiskās organizācijas, neskaitot Eiropas Savienības institūcijas"
#"PAVISAM" - "TOT"
#Nav sadalīts pa valstīm - "Unknown"

nevalstis <- c("noEU", "Z5", "ES", "4A", "ZZ3", "ZZ1", "ZZ2", "7Z", "Other", "TOT", "Unknown")

NavKonvCodes <- c("AF", "DZ", "VI", "AD", "AI", "AO", "AG", "AR", "AU", "BS", "BD", "BZ", "BM", "BA", "BW", "BR", "VG",
                  "BF", "CL", "ZA", "DM", "JE", "EG", "FO", "PH", "GH", "GG", "GI", "HN", "ID", "IQ", "IR", "NZ", "JO", "CV",
                  "KY", "CW", "CD", "XK", "CR", "CI", "CU", "LA", "LS", "LB", "LR", "LI", "MY", "MW", "MH", "MU", "MR", "IM",
                  "NP", "NE", "NG", "NU", "OM", "PK", "PA", "PY", "PE", "WS", "SV", "SM", "SA", "SC", "KN", "VC",
                  "SY", "SL", "LK", "SD", "SZ", "TW", "TH", "TZ", "TC", "TG", "TO", "TN", "UY", "VU", "VE", "ZM", "MK")

#Amerikāņu Virdžīnas - "VI"
#Angilja - "AI"
#Bahamu salas "BS"
#Beliza - "BZ"
#Bermudu salas - "BM"
#Britu Virdžīnas - "VG"
#Dominika - "DM"
#Džērsija - "JE"
#Gērnsija - "GG"
#Gibraltārs - "GI"
#Kaimanu salas - "KY"
#Kirasao - "CW"
#Libērija - "LR"
#Lihtenšteina - "LI"
#Māršala salas - "MH"
#Maurīcija - "MU"
#Mena - "IM"
#Panama - "PA"
#Sanmarīno - "SM"
#Seišeļu salas - "SC"
#Sentkitsa un Nevisa - "KN"
#Sentvinsenta un Grenadīnas - "VC"

arzonas <- c("VI", "AI", "BS", "BZ", "BM",  "VG",  "DM", "JE", "GG",  "GI",  "KY",  "CW", "LR",  "LI",  "MH",  "MU",  "IM", "PA",  "SM",  "SC",  "KN",  "VC")

#datu kopa, kura tiks analizēta modelī - VIŅA DER VIZUALIZĀCIJAI, BET MODELIM VAJAG AR NULLĒM! 
DttsPlmInw <- DataFDI_LV_DTT %>%
  #filter(!kods %in% IzsledzTeritMaterj&FDI>0&!kods %in% nevalstis)
  #filter(!kods %in% IzsledzTeritMaterj&FDI>1&!kods %in% nevalstis)
  #filter(!kods %in% IzsledzTeritMaterj&FDI>3&!kods %in% nevalstis)
  filter(!kods %in% IzsledzTeritMaterj&!kods %in% nevalstis)
DttsPlmInw

DttsPlmInw$Konv <- as.factor(DttsPlmInw$Konv)
DttsPlmInw$teritorija <- as.factor(DttsPlmInw$teritorija)
DttsPlmInw$Year <- as.numeric(DttsPlmInw$Year)
DttsPlmInw$DivIrrelevantDTT <- as.factor(DttsPlmInw$DivIrrelevantDTT)
DttsPlmInw$DivRelevantDTT <- as.factor(DttsPlmInw$DivRelevantDTT)
DttsPlmInw$BlacklistLv <- as.factor(DttsPlmInw$BlacklistLv)
DttsPlmInw$ofc <- as.factor(DttsPlmInw$ofc)
DttsPlmInw$euLV <- as.factor(DttsPlmInw$euLV)
DttsPlmInw$InvestLig <- as.factor(DttsPlmInw$InvestLig)

#for FIXED EFFECTS
DttsPlmInwFE <- DttsPlmInw %>% filter(!kods %in% NavKonvCodes)
DttsPlmInwFE

#preEU period
DttsPlmInwFE_preEU <- DttsPlmInwFE %>% filter(Year < 2004)

#postEU period
DttsPlmInwFE_postEU <- DttsPlmInwFE %>% filter(Year >= 2004)

#ofc countries only
DttsPlmInwFE_OFC <- DttsPlmInwFE %>% filter(kods %in% OfcTerit)

#blacklisted
DttsPlmInwFE_black <- DataFDI_LV_DTT %>% filter(kods %in% KodiBlacklistLV)

#izsledzamas manuāli - Grieķija, Maurīcija, Rumānija, Sentkitsa un Nevisa, Ungārija ("GR", "MU", "RO", "KN", "HU") - valstis, kurām vismaz 1 gadā bija 1 milj, bet ne 3 gados
IzsledzTeritMaterj <- c("GR", "MU", "RO", "KN", "HU", "AF", "AL", "DZ", "VI", "AD", "AO", "AG", "AR", "BD", "BA", "BR", "BF", "CL",
                        "EG", "FO", "PH", "GH", "HN", "HR", "ID", "IQ", "IR", "JP", "JO", "QA", "KG", "CD", "XK",
                         "CR", "CI", "CU", "KW", "LS", "LB", "MY", "MA", "MR", "MX", "ME", "MD", "NP", "NE", "NG",
                        "NU", "OM", "PK", "PE", "PT", "WS", "SV", "VC", "RS", "SY", "SL", "SI", "LK",
                         "SD", "SZ", "TJ", "TW", "TH", "TZ", "TC", "TG", "TO", "TN", "UY", "VU", "VE")



BezArzInw <- DttsPlmInw %>%
  filter(!kods %in% arzonas)

#BlacklistLv dataset

KodiBlacklistLV <- DataFDI_LV_DTT %>%
  filter(BlacklistLv=="1") %>%
  distinct(kods)
KodiBlacklistLV <- as.character(KodiBlacklistLV)
#backslash (escapes) removed manually
KodiBlacklistLV <- c("VI", "AD", "AI", "AG", "AE", "BS", "BZ",
                     "BM", "VG", "DM", "JE", "GG", "GI", "HK",
                     "JO", "KY", "QA", "CY", "CW", "CR", "KW",
                     "LB", "LR", "LI", "MH", "MU", "IM", "SM",
                     "KN", "VC", "SG", "TC", "TO", "UY", "VU",
                     "VE")


#KodiBlacklistLV <- gsub("[\"]", "", KodiBlacklistLV, fixed=T)
#str_replace_all(KodiBlacklistLV, "[\]", "")


#https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r

BlacklTerit <- DataFDI_LV_DTT %>%
  filter(kods %in% KodiBlacklistLV)
BlacklTerit

#Scatter Plot
ggplot(DttsPlmInw, aes(x=KonvGadi, y=FDI, colour = Konv)) + geom_point() + geom_smooth(method=lm, se=FALSE)
ggplot(DttsPlmInw, aes(x=KonvGadi, y=FDI_log, colour = Konv)) + geom_point() + geom_smooth(method=lm, se=FALSE) # + options(repr.P.width=8,repr.P.height=4)

ggplot(DttsPlmInw, aes(x=KonvGadi, y=FDI, colour = teritorija)) + geom_point()
ggplot(DttsPlmInw, aes(x=Year, y=FDI, colour = Konv)) + geom_point()
ggplot(DttsPlmInw, aes(x=KonvGadi, y=FDI_log)) + geom_point()# + options(repr.P.width=8,repr.P.height=4)

#Bar graph
ggplot(DttsPlmInw, aes(x = Year, y = FDI)) +
  geom_col()

#facet_wrap - #facet_wrap_with_colour as a factor
ggplot(data=DttsPlmInw, aes(x=Year, y=FDI, group=teritorija, fill = Konv)) + geom_col() +
  facet_wrap("teritorija", scales = "free") + xlab("Gads") + ylab("Ārvalstu tiešās investīcijas Latvijā") +
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill = "Nodokļu\nkonvencijas\nesamība") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), labels = c("Nav spēkā", "Ir spēkā"))

ggsave("DttFDIWrap.png", width = 36,  height = 23,  units = "cm", dpi = 400)


#faced_wrap for LV Blacklisted territories
BlacklTerit %>%
  filter(FDI>=0) %>%
  ggplot(aes(x=Year, y=FDI, fill = factor(BlacklistLv), group=teritorija)) + 
  scale_fill_manual(values = c("#81b29a", "#3d405b"), labels = c("Nav \"melnajā sarakstā\"", "Ir \"melnajā sarakstā\"")) + 
  geom_col() + facet_wrap("teritorija", scales = "free") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  xlab("Gads") + ylab("Ārvalstu tiešās investīcijas Latvijā") +
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill = "Esamība \"melnajā\nsarakstā\"")

ggsave("BlacklWrap.png", width = 43,  height = 21,  units = "cm", dpi = 400)


#histogram

#with log
# Set the width of each bin to 5 (each bin will span 5 x-axis units)
ggplot(DttsPlmInw, aes(x = FDI_log)) +
  geom_histogram(binwidth = 5, fill = "white", colour = "black")

# Divide the x range into 15 bins
binsize <- diff(range(DttsPlmInw$FDI_log))/7

ggplot(DttsPlmInw, aes(x = FDI_log), na.remove = TRUE) +
  geom_histogram(binwidth = binsize, fill = "white", colour = "black") +
  labs(x = "ĀTI Latvijā (log)", y = "Skaits")

ggsave("FDI_log_Hist.png", width = 8,  height = 5,  units = "cm", dpi = 300)

#without log
# Set the width of each bin to 5 (each bin will span 5 x-axis units)
ggplot(DttsPlmInw, aes(x = FDI)) +
  geom_histogram(binwidth = 5, fill = "white", colour = "black")

# Divide the x range into 15 bins
binsize <- diff(range(DttsPlmInw$FDI))/7

ggplot(DttsPlmInw, aes(x = FDI)) +
  geom_histogram(binwidth = binsize, fill = "white", colour = "black")+
  labs(x = "ĀTI Latvijā", y = "Skaits")

ggsave("FDI_Hist.png", width = 8,  height = 5,  units = "cm", dpi = 300)

# and with >0.1
DttsPlmInw %>%
  filter(FDI>0.1) %>%
  ggplot(aes(x = FDI)) +
  geom_histogram(binwidth = binsize, fill = "white", colour = "black")

#facet wrap - histogram FDI_log
ggplot(DttsPlmInw, aes(x = FDI_log)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_wrap("Year", scales = "free")

#facet wrap - histogram FDI
ggplot(DttsPlmInw, aes(x = FDI)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_wrap("Year", scales = "free")


#boxplots - TBC

#wht grafikam

UIN_WHT_grafiks <- ggplot(grafikam_cit_wht, aes(x = Gads)) + 
  geom_line(aes(y=UIN), color = "#8ac926", size = 1.5) +
  geom_line(aes(y = WHT_div), color = "#1982c4") +
  geom_line(aes(y = WhtDivBlacklist), color = "#ff595e") +
  labs(x = "Gads", y = "Nodokļa likme %")
ggsave("UIN_WHT_grafiks.png", width = 29,  height = 16,  units = "cm", dpi = 300)

krasas <- c("UIN pamatlikme Latvijā\n" = "green", "Vispārējs ieturējuma nodoklis\ndividendēm Latvijā\n" = "blue", "Ieturējuma nodoklis dividendēm\nuz zemo nodokļu valstīm" = "red")
UIN_WHT_grafiks <- ggplot(grafikam_cit_wht, aes(x = Gads)) + 
  geom_line(aes(y=UIN, color = "UIN pamatlikme Latvijā\n"), size = 1.5) +
  geom_line(aes(y = WHT_div, color = "Vispārējs ieturējuma nodoklis\ndividendēm Latvijā\n"), size = 0.5) +
  geom_line(aes(y = WhtDivBlacklist, color = "Ieturējuma nodoklis dividendēm\nuz zemo nodokļu valstīm"), size = 0.5) +
  labs(x = "Gads",
       y = "Nodokļa likme %",
       color = "Nodokļa veids") +
  scale_color_manual(values = krasas)
ggsave("UIN_WHT_grafiks.png", width = 29,  height = 16,  units = "cm", dpi = 300)



#corrplot - correlation matrix
KorMatricai <- DttsPlmInw %>%
  select(FDI_log, Konv, KonvGadi, WhtDivLv, WhtDtt10a, CIT_LV, CIT_foreign, TaxDistWithDTT, TaxDistIgnoringDTT, TaxSavingPpDTT,
         DivRelevantDTT, DivIrrelevantDTT, BlacklistLv, ofc, euLV, InvestLig, gdp, gdp_pp, dist)
KorMatricai$Konv <- as.numeric(KorMatricai$Konv)

corrplot(cor(KorMatricai), method = "number", na.rm = TRUE)

# Korelācijas matrica priekš Fixed Effects
KorMatricai <- DttsPlmInwFE %>%
  select(FDI_log, Konv, KonvGadi, TaxDistWithDTT, 
         DivRelevantDTT, DivIrrelevantDTT, BlacklistLv, euLV, InvestLig, gdp, gdp_pp, dist)
KorMatricai$Konv <- as.numeric(KorMatricai$Konv)
KorMatricai$DivRelevantDTT <- as.numeric(KorMatricai$DivRelevantDTT)
KorMatricai$DivIrrelevantDTT <- as.numeric(KorMatricai$DivIrrelevantDTT)
KorMatricai$InvestLig <- as.numeric(KorMatricai$InvestLig)
KorMatricai$BlacklistLv <- as.numeric(KorMatricai$BlacklistLv)

corrplot(cor(KorMatricai), method = "number", na.rm = TRUE)

library(ggcorrplot)
model.matrix(~0+., data=df) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

# correlation matrix ggplot
ggplot(data = DttsPlmInwFE, aes(FDI_log, TaxDistWithDTT, 
                                gdp, gdp_pp, dist, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#descriptive statistics
SumStat <- DttsPlmInw %>%
  select(-kods, -iso_3, -country, -teritorija, -Subject, -Measure, -continent) %>%
  summary()
SumStat

data.frame(unclass(summary(DttsPlmInw)), check.names = FALSE, stringsAsFactors = FALSE)



#Stripcharts & Scatterplots
#1) Konv
ggplot(DttsPlmInw, aes(x=Konv, y=FDI_log, color=as.factor(StatusDTT)))+
  geom_jitter(position = position_jitter(0.2)) + 
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar")

ggsave("JitterKonvStatus.png")

#1)a) with facet wrap - jitter
ggplot(DttsPlmInw, aes(x=Konv, y=FDI_log, color=as.factor(StatusDTT)))+
  geom_jitter(position = position_jitter(0.2)) + facet_wrap("Year", scales = "free") +
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar")

ggsave("JitterKonvStatusWrap.png", width = 33,  height = 21,  units = "cm", dpi = 300)

#2) FavourTaxRel - jitter
ggplot(DttsPlmInw, aes(x=FavourTaxRel, y=FDI_log))+
  geom_jitter(position = position_jitter(0.2)) + 
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar")
ggsave("FavourTaxRelJitter.png", width = 14,  height = 9,  units = "cm", dpi = 300)


#2a) with fated wrap
ggplot(DttsPlmInw, aes(x=FavourTaxRel, y=FDI_log))+
  geom_jitter(position = position_jitter(0.2)) + facet_wrap("Year", scales = "free") +
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar")
ggsave("FavourTaxRelJitterWrap.png", width = 33,  height = 21,  units = "cm", dpi = 300)

#2)b DttTaxRel - jitter
ggplot(data = subset(DttsPlmInw, !is.na(DttTaxRel)), aes(x=DttTaxRel, y=FDI_log)) +
  geom_jitter(position = position_jitter(0.2)) + 
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar") +
ggsave("DttTaxRelJitter.png", dpi = 400)

#2c) with fated wrap
ggplot(DttsPlmInw, aes(x=DttTaxRel, y=FDI_log))+
  geom_jitter(position = position_jitter(0.2)) + facet_wrap("Year", scales = "free") +
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
ggsave("DttTaxRelJitterWrap.png", width = 33,  height = 21,  units = "cm", dpi = 300)


#3)Tax distance - scatterplot
ggplot(DttsPlmInw, aes(x=TaxDistWithDTT, y=FDI_log)) + geom_point() + geom_smooth(method=lm, se=FALSE)
ggsave("TaxDistanceScatterpl.png", width = 12,  height = 10,  units = "cm", dpi = 400)


#4) TaxSavingPpDTT - scatterplot
ggplot(DttsPlmInw, aes(x=TaxSavingPpDTT, y=FDI_log)) + geom_point() + geom_smooth(method=lm, se=FALSE)
ggsave("TaxSavingsScatterpl.png", width = 10, height = 10,  units = "cm", dpi = 400)

#4)a - countries with tax savings - geom_col + fatet_wrap
DttsPlmInw %>%
  filter(TaxSavingPpDTT>0.005) %>%
  ggplot(aes(x=Year, y=TaxSavingPpDTT, group = teritorija)) +
  geom_col() + facet_wrap("teritorija")
ggsave("TaxSavingsWrap.png", width = 15, height = 10,  units = "cm", dpi = 400)

# TaxDist - geom line
ggplot(DttsPlmInw, aes(x=Year, y = TaxDistWithDTT, y = TaxDistIgnoringDTT, group = teritorija, na.remove = TRUE)) + 
  geom_line() + facet_wrap("teritorija")

ggplot(DttsPlmInw, aes(x = Year, group = "teritorija")) +
  geom_line(aes(y = TaxDistWithDTT, color = "ar Konv")) +
  geom_line(aes(y = TaxDistIgnoringDTT, color = "bez Konv")) +
  facet_wrap("teritorija") +
  labs(x = "Gads", y = "Kombinētā efektīvā nodokļa likme") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 4))

ggsave("TaxDistLinePlot.png", width = 42,  height = 23,  units = "cm", dpi = 400)



