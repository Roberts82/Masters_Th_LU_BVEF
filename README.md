# Masters_Th_LU_BVEF

This project was my first R project and it was created to ensure calculations for my Masters Theses at the University of Latvia. The Master Theses were defended in June 2022.

ANNOTATION (of the Masters Thsis)
Latvia has a relatively wide network of double tax treaties. These treaties are usually concluded with the aim to eliminate double taxation, thus, to promote foreign direct investment to Latvia. However, so far the question of whether double tax treaties promote foreign direct investment in Latvia has not been widely studied. In this work, the author studied the relationship between the existence of double tax treaties (taking into account different characteristics of the respective treaties) and foreign direct investment in Latvia. Impact of certain tax factors on foreign investment in Latvia has also been examined.
Using the panel data model, information on foreign direct investment from all countries to Latvia (for which information is publicly available for the period from 1992 to 2020) has been analysed, and evidence of a close relationship between the existence of a double tax treaty and foreign direct investment has been obtained.
Keywords: double tax treaty, foreign direct investment, international taxes, corporate income tax, repatriation of profits, taxation of dividends.

The structure of the project (folders):

*01_Scripts - contains R script files. 

*02_DatasetsAndTexts - the scripts were used to create datasets (joing the existing ones, as well as to create new datasets based on the information analised during the research (content of double tax treaties, of tax law etc)). The file "DataFDI_LV_DTT.csv" was created by joining other datasets in the folder. That was the dataset which on the models (mainly time-series-cross-sectional regression) were run. The file "DataFDI_LV_DTT.csv" contains data (1992-2020) regarding all countries data on which is available on:
**1)FDI (inward) to Latvia;
**2) data on double tax treaties (DTT) regarding particular countries (existance of DTT, year when DTT entered into force, age of particular DTT in a particular year, maximal whidholding tax to dividends, double tax relief method in accordance width the particular DTT);
**3) Corporate tax  data in particular year (rates in Latvia and partner countries, double tax relief methods used in Latvia and partner countries);
**4) Existance of Investment protection treaties width Latvia width partner countries;
**5) Membership in the EU of partner countries and Latvia together in a particular year;
**6) Macroeconomic data (GDP, GDP PP);
**7) Status of offshore financial centre of particular countries;
**8) Whether a particular country was "blacklisted" for tax purpuses in Latvia in a particular year;
**9) Physical distance from border of a particular country to Latvia;
**10) "Tax distance" - the percentage to be paid to repatriate capital (dividends) from a Latvian company to a company in a particular partner jurisdiction. "Tax distance" was calculated taking into account corporate tax rate in Latvia and partner jurisdiction, existance of widtholding tax in Latvia in a particular year, (most faforable available) double tax relief method. The detailde methodology can be find in the masters thesis.

*03_Results - graphs and results of the models used in the Masters thesis (as well as graphical data on confidence intervals of the coeficients).

*04_MastersThesisSubmitted - The Masters Theses submitted to the University (in Latvian).
