---
title: "04a_Model"
output: html_document
---




```r
library(plm)
library(dplyr)
library(tidyr)
library(ggplot2)
```

#Modelis 1 FDI_log


```r
ForPlm <- DttsPlmInw %>%
  select(teritorija, Year, FDI, FDI_log, KonvGadi, Konv, dist, eu27) %>%
  arrange(teritorija, Year)

dttsLog.fe <- plm(FDI_log ~ KonvGadi, data = ForPlm, model = "within")
dttsLog.re <- plm(FDI_log ~ KonvGadi, data = ForPlm, model = "random")

summary(dttsLog.fe)
```

```
## Oneway (individual) effect Within Model
## 
## Call:
## plm(formula = FDI_log ~ KonvGadi, data = ForPlm, model = "within")
## 
## Unbalanced Panel: n = 139, T = 1-29, N = 2014
## 
## Residuals:
##     Min.  1st Qu.   Median  3rd Qu.     Max. 
## -7.50946 -0.65666  0.00000  0.95894  5.13723 
## 
## Coefficients:
##           Estimate Std. Error t-value  Pr(>|t|)    
## KonvGadi 0.2255272  0.0071737  31.438 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    7200
## Residual Sum of Squares: 4713.9
## R-Squared:      0.3453
## Adj. R-Squared: 0.29674
## F-statistic: 988.364 on 1 and 1874 DF, p-value: < 2.22e-16
```

```r
summary(dttsLog.re)
```

```
## Oneway (individual) effect Random Effect Model 
##    (Swamy-Arora's transformation)
## 
## Call:
## plm(formula = FDI_log ~ KonvGadi, data = ForPlm, model = "random")
## 
## Unbalanced Panel: n = 139, T = 1-29, N = 2014
## 
## Effects:
##                 var std.dev share
## idiosyncratic 2.515   1.586 0.359
## individual    4.498   2.121 0.641
## theta:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.4011  0.8215  0.8491  0.8239  0.8624  0.8624 
## 
## Residuals:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -6.8851 -0.7208  0.0976  0.0857  1.2299  4.9986 
## 
## Coefficients:
##               Estimate Std. Error z-value  Pr(>|z|)    
## (Intercept) -0.8854580  0.1885017 -4.6973 2.636e-06 ***
## KonvGadi     0.2270564  0.0070293 32.3014 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    7583.9
## Residual Sum of Squares: 5006.1
## R-Squared:      0.34186
## Adj. R-Squared: 0.34153
## Chisq: 1043.38 on 1 DF, p-value: < 2.22e-16
```

# Modelis 1a FDI

```r
ForPlm <- DttsPlmInw %>%
  select(teritorija, Year, FDI, FDI_log, KonvGadi, Konv, dist, eu27) %>%
  arrange(teritorija, Year)

dtts.fe <- plm(FDI ~ KonvGadi, data = ForPlm, model = "within")
dtts.re <- plm(FDI ~ KonvGadi, data = ForPlm, model = "random")

summary(dtts.fe)
```

```
## Oneway (individual) effect Within Model
## 
## Call:
## plm(formula = FDI ~ KonvGadi, data = ForPlm, model = "within")
## 
## Unbalanced Panel: n = 139, T = 1-29, N = 2014
## 
## Residuals:
##     Min.  1st Qu.   Median  3rd Qu.     Max. 
## -900.994  -35.675    0.000   23.495 1392.132 
## 
## Coefficients:
##          Estimate Std. Error t-value  Pr(>|t|)    
## KonvGadi 19.52676    0.82752  23.597 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    81364000
## Residual Sum of Squares: 62727000
## R-Squared:      0.22906
## Adj. R-Squared: 0.17188
## F-statistic: 556.808 on 1 and 1874 DF, p-value: < 2.22e-16
```

```r
summary(dtts.re)
```

```
## Oneway (individual) effect Random Effect Model 
##    (Swamy-Arora's transformation)
## 
## Call:
## plm(formula = FDI ~ KonvGadi, data = ForPlm, model = "random")
## 
## Unbalanced Panel: n = 139, T = 1-29, N = 2014
## 
## Effects:
##                   var std.dev share
## idiosyncratic 33472.2   183.0 0.538
## individual    28735.8   169.5 0.462
## theta:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.2665  0.7468  0.7849  0.7520  0.8035  0.8035 
## 
## Residuals:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -731.66  -39.68    3.90    1.11   19.20 1548.04 
## 
## Coefficients:
##              Estimate Std. Error z-value Pr(>|z|)    
## (Intercept) -13.48526   15.59882 -0.8645   0.3873    
## KonvGadi     19.14051    0.79214 24.1631   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    84108000
## Residual Sum of Squares: 65401000
## R-Squared:      0.22244
## Adj. R-Squared: 0.22206
## Chisq: 583.854 on 1 DF, p-value: < 2.22e-16
```
#Model 1b - FDI #bez ārzonām

```r
dttsLog.fe <- plm(FDI_log ~ KonvGadi, data = BezArzInw, model = "within")
```

```
## Warning in pdata.frame(data, index): duplicate couples (id-time) in resulting pdata.frame
##  to find out which, use, e.g., table(index(your_pdataframe), useNA = "ifany")
```

```
## Warning in pdata.frame(data, index): at least one NA in at least one index dimension in resulting pdata.frame
##  to find out which, use, e.g., table(index(your_pdataframe), useNA = "ifany")
```

```
## Error in checkNA.index(index): NA in the time index variable
```

```r
dttsLog.re <- plm(FDI_log ~ KonvGadi, data = BezArzInw, model = "random")
```

```
## Warning in pdata.frame(data, index): duplicate couples (id-time) in resulting pdata.frame
##  to find out which, use, e.g., table(index(your_pdataframe), useNA = "ifany")

## Warning in pdata.frame(data, index): at least one NA in at least one index dimension in resulting pdata.frame
##  to find out which, use, e.g., table(index(your_pdataframe), useNA = "ifany")
```

```
## Error in checkNA.index(index): NA in the time index variable
```

```r
summary(dttsLog.fe)
```

```
## Oneway (individual) effect Within Model
## 
## Call:
## plm(formula = FDI_log ~ KonvGadi, data = ForPlm, model = "within")
## 
## Unbalanced Panel: n = 139, T = 1-29, N = 2014
## 
## Residuals:
##     Min.  1st Qu.   Median  3rd Qu.     Max. 
## -7.50946 -0.65666  0.00000  0.95894  5.13723 
## 
## Coefficients:
##           Estimate Std. Error t-value  Pr(>|t|)    
## KonvGadi 0.2255272  0.0071737  31.438 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    7200
## Residual Sum of Squares: 4713.9
## R-Squared:      0.3453
## Adj. R-Squared: 0.29674
## F-statistic: 988.364 on 1 and 1874 DF, p-value: < 2.22e-16
```

```r
summary(dttsLog.re)
```

```
## Oneway (individual) effect Random Effect Model 
##    (Swamy-Arora's transformation)
## 
## Call:
## plm(formula = FDI_log ~ KonvGadi, data = ForPlm, model = "random")
## 
## Unbalanced Panel: n = 139, T = 1-29, N = 2014
## 
## Effects:
##                 var std.dev share
## idiosyncratic 2.515   1.586 0.359
## individual    4.498   2.121 0.641
## theta:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.4011  0.8215  0.8491  0.8239  0.8624  0.8624 
## 
## Residuals:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -6.8851 -0.7208  0.0976  0.0857  1.2299  4.9986 
## 
## Coefficients:
##               Estimate Std. Error z-value  Pr(>|z|)    
## (Intercept) -0.8854580  0.1885017 -4.6973 2.636e-06 ***
## KonvGadi     0.2270564  0.0070293 32.3014 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    7583.9
## Residual Sum of Squares: 5006.1
## R-Squared:      0.34186
## Adj. R-Squared: 0.34153
## Chisq: 1043.38 on 1 DF, p-value: < 2.22e-16
```

```r
dtts.fe <- plm(FDI ~ KonvGadi, data = BezArzInw, model = "within")
```

```
## Warning in pdata.frame(data, index): duplicate couples (id-time) in resulting pdata.frame
##  to find out which, use, e.g., table(index(your_pdataframe), useNA = "ifany")

## Warning in pdata.frame(data, index): at least one NA in at least one index dimension in resulting pdata.frame
##  to find out which, use, e.g., table(index(your_pdataframe), useNA = "ifany")
```

```
## Error in checkNA.index(index): NA in the time index variable
```

```r
dtts.re <- plm(FDI ~ KonvGadi, data = BezArzInw, model = "random")
```

```
## Warning in pdata.frame(data, index): duplicate couples (id-time) in resulting pdata.frame
##  to find out which, use, e.g., table(index(your_pdataframe), useNA = "ifany")

## Warning in pdata.frame(data, index): at least one NA in at least one index dimension in resulting pdata.frame
##  to find out which, use, e.g., table(index(your_pdataframe), useNA = "ifany")
```

```
## Error in checkNA.index(index): NA in the time index variable
```

```r
summary(dttsLog.fe)
```

```
## Oneway (individual) effect Within Model
## 
## Call:
## plm(formula = FDI_log ~ KonvGadi, data = ForPlm, model = "within")
## 
## Unbalanced Panel: n = 139, T = 1-29, N = 2014
## 
## Residuals:
##     Min.  1st Qu.   Median  3rd Qu.     Max. 
## -7.50946 -0.65666  0.00000  0.95894  5.13723 
## 
## Coefficients:
##           Estimate Std. Error t-value  Pr(>|t|)    
## KonvGadi 0.2255272  0.0071737  31.438 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    7200
## Residual Sum of Squares: 4713.9
## R-Squared:      0.3453
## Adj. R-Squared: 0.29674
## F-statistic: 988.364 on 1 and 1874 DF, p-value: < 2.22e-16
```

```r
summary(dttsLog.re)
```

```
## Oneway (individual) effect Random Effect Model 
##    (Swamy-Arora's transformation)
## 
## Call:
## plm(formula = FDI_log ~ KonvGadi, data = ForPlm, model = "random")
## 
## Unbalanced Panel: n = 139, T = 1-29, N = 2014
## 
## Effects:
##                 var std.dev share
## idiosyncratic 2.515   1.586 0.359
## individual    4.498   2.121 0.641
## theta:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.4011  0.8215  0.8491  0.8239  0.8624  0.8624 
## 
## Residuals:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -6.8851 -0.7208  0.0976  0.0857  1.2299  4.9986 
## 
## Coefficients:
##               Estimate Std. Error z-value  Pr(>|z|)    
## (Intercept) -0.8854580  0.1885017 -4.6973 2.636e-06 ***
## KonvGadi     0.2270564  0.0070293 32.3014 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    7583.9
## Residual Sum of Squares: 5006.1
## R-Squared:      0.34186
## Adj. R-Squared: 0.34153
## Chisq: 1043.38 on 1 DF, p-value: < 2.22e-16
```

