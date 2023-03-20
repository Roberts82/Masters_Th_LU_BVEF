install.packages("plm")
install.packages("jtools")
install.packages("broom")
install.packages("ggstance")
install.packages("broom.mixed")
install.packages("huxtable")
install.packages("officer")
install.packages("flextable")
library(plm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jtools)
library(broom)
library(ggstance)
library(broom.mixed)
library(huxtable)
library(officer)
library(flextable)

#Modelis 1 FDI_log ~ KonvGadi
#PLM
dttsM1.fe <- plm(FDI_log ~ KonvGadi, data = DttsPlmInw, index = c("teritorija","Year"), model = "within")
dttsM1.re <- plm(FDI_log ~ KonvGadi, data = DttsPlmInw, index = c("teritorija","Year"), model = "random")

summary(dttsM1.fe)
summary(dttsM1.re)

# Modelis 1a FDI ~ KonvGadi

dttsM1a.fe <- plm(FDI ~ KonvGadi, data = DttsPlmInw, index = c("teritorija","Year"), model = "within")
dttsM1a.re <- plm(FDI ~ KonvGadi, data = DttsPlmInw, index = c("teritorija","Year"), model = "random")

summary(dttsM1a.fe)
summary(dttsM1a.re)

#Model 1b - FDI_log ~ KonvGadi #bez ārzonām

dttsM1b.fe <- plm(FDI_log ~ KonvGadi, data = BezArzInw, index = c("teritorija","Year"), model = "within")
dttsM1b.re <- plm(FDI_log ~ KonvGadi, data = BezArzInw, index = c("teritorija","Year"), model = "random")

summary(dttsM1b.fe)
summary(dttsM1b.re)

#Model 1c - FDI ~ KonvGadi #bez ārzonām

dttsM1c.fe <- plm(FDI ~ KonvGadi, data = BezArzInw, index = c("teritorija","Year"), model = "within")
dttsM1c.re <- plm(FDI ~ KonvGadi, data = BezArzInw, index = c("teritorija","Year"), model = "random")

summary(dttsM1c.fe)
summary(dttsM1c.re)

#Model 1c - FDI + Konv

#Modelis 2 - FDI_log + log(GDP)
#PLM
#na.omit or na.exclude? Which makes more sense? Or na.pass - we went have data on DTTs, but we replaced with 0's

dttsM2.fe <- plm(FDI_log ~ KonvGadi + log(gdp), data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM2.re <- plm(FDI_log ~ KonvGadi + log(gdp), data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM2.fe)
summary(dttsM2.re)

#Modelis 3 - FDI_log + log(GDP) + eu27
#PLM
#na.omit or na.exclude? Which makes more sense? Or na.pass - we went have data on DTTs, but we replaced with 0's

dttsM3.fe <- plm(FDI_log ~ KonvGadi + log(gdp) + eu27, data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM3.re <- plm(FDI_log ~ KonvGadi + log(gdp) + eu27, data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM3.fe)
summary(dttsM3.re)

#Modelis 4 - FDI_log + eu27
#PLM
#na.omit or na.exclude? Which makes more sense? Or na.pass - we went have data on DTTs, but we replaced with 0's

dttsM4.fe <- plm(FDI_log ~ KonvGadi +  eu27, data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM4.re <- plm(FDI_log ~ KonvGadi + eu27, data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM4.fe)
summary(dttsM4.re)

#Modelis 4a - FDI + eu27
#PLM
#na.omit or na.exclude? Which makes more sense? Or na.pass - we went have data on DTTs, but we replaced with 0's

dttsM4a.fe <- plm(FDI ~ KonvGadi +  eu27, data = DttsPlmInw,  index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM4a.re <- plm(FDI ~ KonvGadi + eu27, data = DttsPlmInw,  index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM4a.fe)
summary(dttsM4a.re)

#Modelis 4b - FDI + euLV
#PLM
#na.omit or na.exclude? Which makes more sense? Or na.pass - we went have data on DTTs, but we replaced with 0's

dttsM4b.fe <- plm(FDI ~ KonvGadi +  euLV, data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM4b.re <- plm(FDI ~ KonvGadi + euLV, data = DttsPlmInw,  index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM4b.fe)
summary(dttsM4b.re)


#Modelis 5 - FDI_log + log(dist) + eu27
#PLM
#na.omit or na.exclude? Which makes more sense? Or na.pass - we went have data on DTTs, but we replaced with 0's

dttsM5.fe <- plm(FDI_log ~ KonvGadi +  eu27 + log(dist), data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM5.re <- plm(FDI_log ~ KonvGadi + eu27 + log(dist), data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM5.fe)
summary(dttsM5.re)

# 5a

#5b FDI_log + log(dist (-arzonas)) + eu27

dttsM5b.fe <- plm(FDI_log ~ KonvGadi +  log(dist) + eu27, data = BezArzInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM5b.re <- plm(FDI_log ~ KonvGadi + log(dist) + eu27, data = BezArzInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)


summary(dttsM5b.fe)
summary(dttsM5b.re)

#5c FDI_log + log(dist) (-arzonas)) + euLV


dttsM5c.fe <- plm(FDI_log ~ KonvGadi +  log(dist) + euLV, data = BezArzInw, index = c("teritorija","Year"), model = "within", na.action=na.omit)
dttsM5c.re <- plm(FDI_log ~ KonvGadi + log(dist) + euLV, data = BezArzInw,  index = c("teritorija","Year"), model = "random", na.action=na.omit)

summary(dttsM5c.fe)
summary(dttsM5c.re)

#5d FDI_log + log(dist+2) (-arzonas)) + euLV + log(gdp)


dttsM5d.fe <- plm(FDI_log ~ KonvGadi + euLV + log(dist+1) + log(gdp), data = BezArzInw, index = c("teritorija", "Year"), model = "within", na.action=na.omit)
dttsM5d.re <- plm(FDI_log ~ KonvGadi + euLV + log(dist+1) + log(gdp), data = BezArzInw, index = c("teritorija", "Year"), model = "random", na.action=na.omit)

summary(dttsM5d.fe)
summary(dttsM5d.re)

# Model 6 - FDI_log ~ KonvGadi + euLV + dist + log(gdp) (VARBŪT nevajag, lai distancei būtu "0"s, jo tad ar logarimēšanu problēmas kaut kādas rodas?)
dttsM6.fe <- plm(FDI_log ~ KonvGadi +  euLV +log(dist+2) + log(gdp), data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM6.re <- plm(FDI_log ~ KonvGadi + euLV + log(dist+2) + log(gdp), data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM6.fe)
summary(dttsM6.re)

# Model 6a - FDI ~ KonvGadi + euLV + dist + log(gdp) (VARBŪT nevajag, lai distancei būtu "0"s, jo tad ar logarimēšanu problēmas kaut kādas rodas?)
dttsM6a.fe <- plm(FDI ~ KonvGadi +  euLV +log(dist+2) +  log(gdp), data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM6a.re <- plm(FDI ~ KonvGadi + euLV + log(dist+2) + log(gdp), data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM6a.fe)
summary(dttsM6a.re)

# Model 6b - FDI_log ~ Konv + euLV + dist + log(gdp) (VARBŪT nevajag, lai distancei būtu "0"s, jo tad ar logarimēšanu problēmas kaut kādas rodas?)
dttsM6b.fe <- plm(FDI_log ~ Konv + KonvGadi +  euLV + dist + log(gdp), data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM6b.re <- plm(FDI_log ~ Konv + KonvGadi + euLV + dist + log(gdp), data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM6b.fe)
summary(dttsM6b.re)

# Model 7 - FDI_log ~ Konv + KonvGadi + euLV + log(dist) + log(gdp) + BlacklistLv
dttsM7.fe <- plm(FDI_log ~ Konv + KonvGadi +  euLV +dist + log(gdp) + BlacklistLv, data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM7.re <- plm(FDI_log ~ Konv + KonvGadi + euLV + dist + log(gdp) + BlacklistLv, data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM7.fe)
summary(dttsM7.re)

#Model 8 FDI_log ~ Konv + KonvGadi + euLV + log(dist) + log(gdp) + BlacklistLv + InvestLig
dttsM8.fe <- plm(FDI_log ~ Konv + KonvGadi + euLV + log(gdp) + log(dist) + BlacklistLv + InvestLig, data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM8.re <- plm(FDI_log ~ Konv + KonvGadi + euLV + log(gdp) + log(dist) + BlacklistLv + InvestLig, data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM8.fe)
summary(dttsM8.re)


#Model 9 FDI_log ~ Konv + KonvGadi + euLV + log(dist) + log(gdp) + log(gdp_pp)+ BlacklistLv + InvestLig
dttsM9.fe <- plm(FDI_log ~ Konv + KonvGadi + euLV + log(gdp) + log(gdp_pp) + log(dist) + BlacklistLv + InvestLig, data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM9.re <- plm(FDI_log ~ Konv + KonvGadi + euLV + log(gdp) + log(gdp_pp) + log(dist) + BlacklistLv + InvestLig, data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM9.fe)
summary(dttsM9.re)

#Model 10 FDI_log ~ Konv + KonvGadi + euLV + log(dist) + log(gdp) + log(gdp_pp)+ BlacklistLv + InvestLig + ofc
dttsM10.fe <- plm(FDI_log ~ Konv + KonvGadi + euLV + log(gdp) + log(gdp_pp) + log(dist) + BlacklistLv + ofc + InvestLig, data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM10.re <- plm(FDI_log ~ Konv + KonvGadi + euLV + log(gdp) + log(gdp_pp) + log(dist) + BlacklistLv + ofc + InvestLig, data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM10.fe)
summary(dttsM10.re)

#Model 11 FDI_log ~ Konv + KonvGadi + euLV + log(dist) + log(gdp) + log(gdp_pp)+ BlacklistLv + InvestLig + ofc
dttsM11.fe <- plm(FDI_log ~ Konv + KonvGadi + euLV + log(gdp) + log(gdp_pp) + log(dist) + BlacklistLv + ofc + InvestLig + BlacklistLv*log(dist) + ofc*log(dist), data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM11.re <- plm(FDI_log ~ Konv + KonvGadi + euLV + log(gdp) + log(gdp_pp) + log(dist) + BlacklistLv + ofc + InvestLig + BlacklistLv*log(dist) + ofc*log(dist), data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM11.fe)
summary(dttsM11.re)

#Model 12 FDI_log ~ Konv + KonvGadi + euLV + log(dist) + log(gdp) + log(gdp_pp)+ BlacklistLv + InvestLig + ofc
dttsM12.fe <- plm(FDI_log ~ Konv + KonvGadi + euLV + log(gdp) + log(gdp_pp) + log(dist) + BlacklistLv + ofc + InvestLig + BlacklistLv*log(dist) + TaxDistWithDTT + TaxSavingPpDTT + DivRelevantDTT + DivIrrelevantDTT, data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM12.re <- plm(FDI_log ~ Konv + KonvGadi + euLV + log(gdp) + log(gdp_pp) + log(dist) + BlacklistLv + ofc + InvestLig + BlacklistLv*log(dist) + TaxDistWithDTT + TaxSavingPpDTT + DivRelevantDTT + DivIrrelevantDTT, data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM12.fe)
summary(dttsM12.re)

#Model 13 FDI_log ~ Konv + KonvGadi + euLV + log(dist) + log(gdp) + log(gdp_pp)+ BlacklistLv + InvestLig + ofc
dttsM13.fe <- plm(FDI_log ~ DivRelevantDTT + DivIrrelevantDTT, data = DttsPlmInw, index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM13.re <- plm(FDI_log ~ DivRelevantDTT + DivIrrelevantDTT, data = DttsPlmInw, index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM13.fe)
summary(dttsM13.re)

#Model 14 FDI_log ~ Konv + KonvGadi + euLV + log(dist) + log(gdp) + log(gdp_pp)+ BlacklistLv + InvestLig + ofc
dttsM14.fe <- plm(FDI_log ~ KonvGadi + euLV + log(gdp) + log(gdp_pp) + 
                    log(dist) + BlacklistLv + InvestLig +  
                    TaxDistWithDTT + DivRelevantDTT + DivRelevantDTT*TaxSavingPpDTT + DivIrrelevantDTT, data = DttsPlmInw, 
                  index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM14.re <- plm(FDI_log ~ KonvGadi + euLV + log(gdp) + log(gdp_pp) + log(dist) + BlacklistLv + 
                    InvestLig +  TaxDistWithDTT +
                    DivRelevantDTT + DivRelevantDTT*TaxSavingPpDTT + DivIrrelevantDTT, data = DttsPlmInw,
                  index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM14.fe)
summary(dttsM14.re)

#Model 15 FDI_log ~ TaxDistWithDTT 
dttsM15.fe <- plm(FDI_log ~ TaxDistWithDTT, data = DttsPlmInw, 
                  index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM15.re <- plm(FDI_log ~ TaxDistWithDTT, data = DttsPlmInw,
                  index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM15.fe)
summary(dttsM15.re)

#Model 16 FDI_log ~ TaxDistWithDTT + 
dttsM16.fe <- plm(FDI_log ~ TaxDistWithDTT + DtRelExemption + DtRelIndirectCredit + DtRelDirectCredit + DtRelNoRelief, data = DttsPlmInw, 
                  index = c("teritorija","Year"), model = "within", na.action=na.exclude)
dttsM16.re <- plm(FDI_log ~ TaxDistWithDTT + DtRelExemption + DtRelIndirectCredit + DtRelDirectCredit + DtRelNoRelief, data = DttsPlmInw,
                  index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(dttsM16.fe)
summary(dttsM16.re)



#TĪRRAKSTS - DARBAM

#MODELIS1
Modelis1.fe <- plm(FDI_log ~ Konv + KonvGadi, data = DttsPlmInw, 
                  index = c("teritorija","Year"), model = "within", na.action=na.exclude)
Modelis1.re <- plm(FDI_log ~ Konv + KonvGadi, data = DttsPlmInw,
                  index = c("teritorija","Year"), model = "random", na.action=na.exclude)
Modelis1.po <- plm(FDI_log ~ Konv + KonvGadi, data = DttsPlmInw,
                   index = c("teritorija","Year"), model = "pooling", na.action=na.exclude)

summary(Modelis1.fe)
summary(Modelis1.re)
summary(Modelis1.po)

#MODELIS2
Modelis2.fe <- plm(FDI_log ~ TaxDistWithDTT, data = DttsPlmInw, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)
Modelis2.re <- plm(FDI_log ~ TaxDistWithDTT, data = DttsPlmInw,
                   index = c("teritorija","Year"), model = "random", na.action=na.exclude)
Modelis2.po <- plm(FDI_log ~ TaxDistWithDTT, data = DttsPlmInw,
                   index = c("teritorija","Year"), model = "pooling", na.action=na.exclude)

summary(Modelis2.fe)
summary(Modelis2.re)
summary(Modelis2.po)

#MODELIS3
Modelis3.fe <- plm(FDI_log ~ TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT, data = DttsPlmInw, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)
Modelis3.re <- plm(FDI_log ~ TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT, data = DttsPlmInw,
                   index = c("teritorija","Year"), model = "random", na.action=na.exclude)
Modelis3.po <- plm(FDI_log ~ TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT, data = DttsPlmInw,
                   index = c("teritorija","Year"), model = "pooling", na.action=na.exclude)

summary(Modelis3.fe)
summary(Modelis3.re)
summary(Modelis3.po)


#MODELIS4
Modelis4.fe <- plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT + BlacklistLv + ofc + euLV + InvestLig, data = DttsPlmInw, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)
Modelis4.re <- plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT + BlacklistLv + ofc + euLV + InvestLig, data = DttsPlmInw,
                   index = c("teritorija","Year"), model = "random", na.action=na.exclude)
Modelis4.po <- plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT + BlacklistLv + ofc + euLV + InvestLig, data = DttsPlmInw,
                   index = c("teritorija","Year"), model = "pooling", na.action=na.exclude)

summary(Modelis4.fe)
summary(Modelis4.re)
summary(Modelis4.po)

#MODELIS5
Modelis5.fe <- plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT + BlacklistLv + ofc + euLV + InvestLig +
                     log(gdp) + log(gdp_pp) + log(dist), data = DttsPlmInw, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)
Modelis5.re <- plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT + BlacklistLv + ofc + euLV + InvestLig +
                     log(gdp) + log(gdp_pp) + log(dist), data = DttsPlmInw,
                   index = c("teritorija","Year"), model = "random", na.action=na.exclude)
Modelis5.po <- plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT + BlacklistLv + ofc + euLV + InvestLig +
                     log(gdp) + log(gdp_pp) + log(dist), data = DttsPlmInw,
                   index = c("teritorija","Year"), model = "pooling", na.action=na.exclude)

summary(Modelis5.fe)
summary(Modelis5.re)
summary(Modelis5.po)

#MODELIS 6
Modelis6.fe <- plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT + BlacklistLv + ofc + euLV + InvestLig +
                     log(gdp) + log(gdp_pp) + log(dist) + BlacklistLv*log(dist), data = DttsPlmInw, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)
Modelis6.re <- plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT + BlacklistLv + ofc + euLV + InvestLig +
                     log(gdp) + log(gdp_pp) + log(dist) + BlacklistLv*log(dist), data = DttsPlmInw,
                   index = c("teritorija","Year"), model = "random", na.action=na.exclude)
Modelis6.po <- plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT + BlacklistLv + ofc + euLV + InvestLig +
                     log(gdp) + log(gdp_pp) + log(dist) + BlacklistLv*log(dist), data = DttsPlmInw,
                   index = c("teritorija","Year"), model = "pooling", na.action=na.exclude)

summary(Modelis6.fe)
summary(Modelis6.re)
summary(Modelis6.po)



#SUMMARIES


summary(Modelis1.fe)
summary(Modelis1.re)
summary(Modelis1.po)
summary(Modelis2.fe)
summary(Modelis2.re)
summary(Modelis2.po)
summary(Modelis3.fe)
summary(Modelis3.re)
summary(Modelis3.po)
summary(Modelis4.fe) #doen't work
summary(Modelis4.re)
summary(Modelis4.po)
summary(Modelis5.fe)
summary(Modelis5.re) #doen't work
summary(Modelis5.po)
summary(Modelis6.fe)
summary(Modelis6.re)#doen't work
summary(Modelis6.po)

#Confidence Intervals

plot_summs(Modelis1.fe,  Modelis1.re, Modelis1.po, Modelis2.fe, Modelis2.re, 
             Modelis2.po, Modelis3.fe, Modelis3.re, Modelis3.po, Modelis4.fe,
             Modelis4.po,  Modelis5.fe,  Modelis5.po,
             Modelis6.fe,  Modelis6.po)

plot_summs(Modelis1.fe,  Modelis1.re, Modelis1.po, Modelis2.fe, Modelis2.re, 
           Modelis2.po)

plot_summs(Modelis3.fe, Modelis3.re, Modelis3.po, Modelis4.fe,
           Modelis4.po)

plot_summs(Modelis5.po, Modelis6.fe,  Modelis6.po)

#Table outputs

export_summs(Modelis1.fe,  Modelis1.re, Modelis1.po, Modelis2.fe, Modelis2.re, 
             Modelis2.po, Modelis3.fe, Modelis3.re, Modelis3.po, Modelis4.fe,
             Modelis4.po, Modelis5.po, Modelis6.fe,  Modelis6.po, scale = TRUE, to.file = "xlsx", file.name = "Output1.xlsx")



[
  "Konv"                "KonvGadi"                  
[17] "DomTaxRel"           "DttTaxRel"           "FavourTaxRel"        "TaxDistWithDTT"      "TaxDistIgnoringDTT"  "TaxSavingPpDTT"      "DivRelevantDTT"      "DivIrrelevantDTT"   
[25] "StatusDTT"           "DtRelDirectCredit"   "DtRelExemption"      "DtRelIndirectCredit" "DtRelDeduction"      "DtRelNoRelief"       "BlacklistLv"         "ofc"                
[33] "euLV"                "InvestLig"           "gdp"                 "gdp_pp"              "continent"           "dist"

  
  
  # OFC only
  
  ModelisFE1_OFC <-  plm(FDI_log ~ Konv + KonvGadi, data = DttsPlmInwFE_OFC, 
                         index = c("teritorija","Year"), model = "within", na.action=na.exclude)
  summary(ModelisFE1_OFC)
  
  ModelisFE2_OFC <-  plm(FDI_log ~ TaxDistWithDTT, data = DttsPlmInwFE_OFC, 
                         index = c("teritorija","Year"), model = "within", na.action=na.exclude)
  
  summary(ModelisFE2_OFC)
  
  ModelisFE3_OFC <-  plm(FDI_log ~ Konv + KonvGadi  +  InvestLig + log(gdp) + log(gdp_pp), data = DttsPlmInwFE_OFC, 
                         index = c("teritorija","Year"), model = "within", na.action=na.exclude)
  
  summary(ModelisFE3_OFC)
  
  ModelisFE4_OFC <-  plm(FDI_log ~ DivRelevantDTT + DivIrrelevantDTT +  InvestLig + log(gdp) + log(gdp_pp), data = DttsPlmInwFE_OFC, 
                         index = c("teritorija","Year"), model = "within", na.action=na.exclude)
  
  summary(ModelisFE4_OFC)
  
  ModelisFE5_OFC <-  plm(FDI_log ~ TaxDistWithDTT + Konv + KonvGadi + InvestLig + log(gdp) + log(gdp_pp), data = DttsPlmInwFE_OFC, 
                         index = c("teritorija","Year"), model = "within", na.action=na.exclude)
  
  summary(ModelisFE5_OFC)
  
  
  plot_summs(ModelisFE1_OFC, ModelisFE3_OFC, ModelisFE4_OFC)
  plot_summs(ModelisFE2_OFC, ModelisFE5_OFC)
  
  # MODELU GRUPA 1 FE

ModelisFE1 <-  plm(FDI_log ~ Konv + KonvGadi, data = DttsPlmInwFE, 
                                 index = c("teritorija","Year"), model = "within", na.action=na.exclude)
summary(ModelisFE1)

ModelisFE2 <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT, data = DttsPlmInwFE, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE2)

ModelisFE3 <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + euLV + InvestLig, data = DttsPlmInwFE, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE3)

ModelisFE4 <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + euLV + InvestLig + log(gdp) + log(gdp_pp), data = DttsPlmInwFE, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE4)



plot_summs(ModelisFE1, ModelisFE2, ModelisFE3, ModelisFE4)

export_summs(ModelisFE1, ModelisFE2, ModelisFE3, ModelisFE4, scale = TRUE, to.file = "xlsx", file.name = "FE_4_mod_grupa1.xlsx")


# MODELU GRUPA 2 FE

ModelisFE1_relev <-  plm(FDI_log ~ DivRelevantDTT + DivIrrelevantDTT, data = DttsPlmInwFE, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)
summary(ModelisFE1_relev)

ModelisFE2_relev <-  plm(FDI_log ~ DivRelevantDTT + DivIrrelevantDTT + KonvGadi + TaxDistWithDTT, data = DttsPlmInwFE, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE2_relev)

ModelisFE3_relev <-  plm(FDI_log ~ DivRelevantDTT + DivIrrelevantDTT + KonvGadi + TaxDistWithDTT + euLV + InvestLig, data = DttsPlmInwFE, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE3_relev)

ModelisFE4_relev <-  plm(FDI_log ~ DivRelevantDTT + DivIrrelevantDTT + KonvGadi + TaxDistWithDTT + euLV + InvestLig + log(gdp) + log(gdp_pp), data = DttsPlmInwFE, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE4_relev)



plot_summs(ModelisFE1_relev, ModelisFE2_relev, ModelisFE3_relev, ModelisFE4_relev)

export_summs(ModelisFE1_relev, ModelisFE2_relev, ModelisFE3_relev, ModelisFE4_relev, scale = TRUE, to.file = "xlsx", file.name = "FE_4_mod_grupa2.xlsx")


# MODELU GRUPA 3 FE - nesadarb

ModelisFE1_nesad <-  plm(FDI_log ~ BlacklistLv, data = DttsPlmInwFE_black, 
                         index = c("teritorija","Year"), model = "within", na.action=na.exclude)
summary(ModelisFE1_nesad)

ModelisFE2_nesad <-  plm(FDI_log ~ BlacklistLv + TaxDistWithDTT, data = DttsPlmInwFE_black, 
                         index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE2_nesad)

ModelisFE3_nesad <-  plm(FDI_log ~ BlacklistLv + TaxDistWithDTT +  log(gdp) + log(gdp_pp), data = DttsPlmInwFE_black, 
                         index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE3_nesad)



plot_summs(ModelisFE1_nesad, ModelisFE2_nesad, ModelisFE3_nesad)

export_summs(ModelisFE1_nesad, ModelisFE2_nesad, ModelisFE3_nesad, scale = TRUE, to.file = "xlsx", file.name = "FE_4_mod_grupa3_nesad.xlsx")

#RE effects - pamatmodelis

ModelisRE1 <-  plm(FDI_log ~ Konv + KonvGadi, data = DttsPlmInw, 
                   index = c("teritorija","Year"), model = "random", na.action=na.exclude)
summary(ModelisRE1)

ModelisRE2 <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT, data = DttsPlmInw, 
                   index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(ModelisRE2)

ModelisRE3 <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + euLV + InvestLig, data = DttsPlmInw, 
                   index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(ModelisRE3)

ModelisRE4 <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + euLV + InvestLig + log(gdp) + log(gdp_pp), data = DttsPlmInw, 
                   index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(ModelisRE4)

ModelisRE5 <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + euLV + InvestLig + log(gdp) + log(gdp_pp) + log(dist) + ofc + BlacklistLv, data = DttsPlmInw, 
                   index = c("teritorija","Year"), model = "random", na.action=na.exclude)

summary(ModelisRE5)


plot_summs(ModelisRE1, ModelisRE2, ModelisRE3, ModelisRE4, ModelisRE5)

export_summs(ModelisRE1, ModelisRE2, ModelisRE3, ModelisRE4, ModelisRE5, scale = TRUE, to.file = "xlsx", file.name = "RE_5_models.xlsx")

#PRE EU

ModelisFE1_preEU <-  plm(FDI_log ~ Konv + KonvGadi, data = DttsPlmInwFE_preEU, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)
summary(ModelisFE1_preEU)

ModelisFE2_preEU <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT, data = DttsPlmInwFE_preEU, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE2_preEU)

ModelisFE3_preEU <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + InvestLig, data = DttsPlmInwFE_preEU, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE3_preEU)

ModelisFE4_preEU <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + InvestLig + log(gdp) + log(gdp_pp), data = DttsPlmInwFE_preEU, 
                   index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE4_preEU)



plot_summs(ModelisFE1_preEU, ModelisFE2_preEU, ModelisFE3_preEU, ModelisFE4_preEU)

export_summs(ModelisFE1_preEU, ModelisFE2_preEU, ModelisFE3_preEU, ModelisFE4_preEU, scale = TRUE, to.file = "xlsx", file.name = "FE_4_models_preEU.xlsx")

#post EU

ModelisFE1_postEU <-  plm(FDI_log ~ Konv + KonvGadi, data = DttsPlmInwFE_postEU, 
                         index = c("teritorija","Year"), model = "within", na.action=na.exclude)
summary(ModelisFE1_postEU)

ModelisFE2_postEU <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT, data = DttsPlmInwFE_postEU, 
                         index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE2_postEU)

ModelisFE3_postEU <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + InvestLig, data = DttsPlmInwFE_postEU, 
                         index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE3_postEU)

ModelisFE4_postEU <-  plm(FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + InvestLig + log(gdp) + log(gdp_pp), data = DttsPlmInwFE_postEU, 
                         index = c("teritorija","Year"), model = "within", na.action=na.exclude)

summary(ModelisFE4_postEU)


plot_summs(ModelisFE1_postEU, ModelisFE2_postEU, ModelisFE3_postEU, ModelisFE4_postEU)

export_summs(ModelisFE1_postEU, ModelisFE2_postEU, ModelisFE3_postEU, ModelisFE4_postEU, scale = TRUE, to.file = "xlsx", file.name = "FE_4_models_postEU.xlsx")


# F Test for Individual and/or Time Effects

pFtest(ModelisFE4, ModelisRE5) # Breusch
pFtest(ModelisFE4, ModelisRE5, effect = "time") # Breusch

#Breusch-Pagan
pcdtest(ModelisFE4, type = "bp")
pcdtest(ModelisRE5, type = "bp")

#Hausman
phtest(ModelisFE4, ModelisRE5)
phtest(ModelisFE4, ModelisRE5, test = "chisq")
phtest(ModelisFE4, ModelisRE5, method = "aux")

# Lagrange
plmtest(ModelisFE4)
plmtest(ModelisRE5)

#serial correlation
pbgtest(ModelisFE4)
pbgtest(ModelisRE5)
