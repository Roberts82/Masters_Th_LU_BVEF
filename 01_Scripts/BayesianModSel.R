install.packages("olsrr", type="mac.binary")
install.packages("BMS", type="mac.binary")
install.packages("glnet")
install.packages("Matrix")
library(olsrr)
library(BMS)
library(glmnet)
library(Matrix)


Modelis3.fe

ModelisOLS_3fe <- (FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT +
                     BlacklistLv + ofc + euLV + InvestLig, data = DttsPlmInw)

FWDfit.p <- ols_step_forward_p(Modelis3.fe, penter = .05)

FDI_log ~ Konv + KonvGadi + TaxDistWithDTT + DivRelevantDTT + DivIrrelevantDTT + BlacklistLv + ofc + euLV + InvestLig +
  log(gdp) + log(gdp_pp) + log(dist), data = DttsPlmInw

glmnet(DttsPlmInw$FDI_log, DttsPlmInw$Konv, family = "binominal")