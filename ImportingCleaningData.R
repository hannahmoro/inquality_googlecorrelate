#In this code, I import and clean the data used in the manuscript starting with raw files. I create a spreadsheet for each class of variables, then create a master document with just the data used in analysis.

library(dplyr)
library(readr)
library(tidyverse)
library(readxl)
library(psych)
library(xtable)
library(stats)

########Median Household Income------------
SAIPE_income <- read_csv("./RawDataFiles/SAIPESNC_30SEP17_16_31_48_62.csv") %>%
  filter(State != "11") %>% #removing DC 
  filter(State != "72") %>% #removing Puerto Rico
  select(Year, State, `State / County Name`, `Median Household Income in Dollars`, `Ages 5 to 17 in Families in Poverty Percent`)
names(SAIPE_income) <- c("Year", "IDnum", "StateName", "MedIncome", "PercentStuPoverty")
#removing dollar signs and commas from income
SAIPE_income$MedIncome <- as.numeric(gsub('[$,]','',SAIPE_income$MedIncome))
#making other factors numeric
SAIPE_income[, c(1,2,4,5)] <- sapply(SAIPE_income[, c(1,2,4,5)], as.numeric)
#saving long version
SAIPE_income_long <- SAIPE_income
#long to wide
SAIPE_2005 <-dplyr::filter(SAIPE_income, Year==2005)
names(SAIPE_2005) <- c("Year", "IDnum", "StateName", "MedIncome_2005", "PercentStuPoverty_2005")
SAIPE_2006 <-dplyr::filter(SAIPE_income, Year==2006)
names(SAIPE_2006) <- c("Year", "IDnum", "StateName", "MedIncome_2006", "PercentStuPoverty_2006")
SAIPE_2007 <-dplyr::filter(SAIPE_income, Year==2007)
names(SAIPE_2007) <- c("Year", "IDnum", "StateName", "MedIncome_2007", "PercentStuPoverty_2007")
SAIPE_2008 <-dplyr::filter(SAIPE_income, Year==2008)
names(SAIPE_2008) <- c("Year", "IDnum", "StateName", "MedIncome_2008", "PercentStuPoverty_2008")
SAIPE_2009 <-dplyr::filter(SAIPE_income, Year==2009)
names(SAIPE_2009) <- c("Year", "IDnum", "StateName", "MedIncome_2009", "PercentStuPoverty_2009")
SAIPE_2010 <-dplyr::filter(SAIPE_income, Year==2010)
names(SAIPE_2010) <- c("Year", "IDnum", "StateName", "MedIncome_2010", "PercentStuPoverty_2010")
SAIPE_2011 <-dplyr::filter(SAIPE_income, Year==2011)
names(SAIPE_2011) <- c("Year", "IDnum", "StateName", "MedIncome_2011", "PercentStuPoverty_2011")
SAIPE_2012 <-dplyr::filter(SAIPE_income, Year==2012)
names(SAIPE_2012) <- c("Year", "IDnum", "StateName", "MedIncome_2012", "PercentStuPoverty_2012")
SAIPE_2013 <-dplyr::filter(SAIPE_income, Year==2013)
names(SAIPE_2013) <- c("Year", "IDnum", "StateName", "MedIncome_2013", "PercentStuPoverty_2013")
SAIPE_2014 <-dplyr::filter(SAIPE_income, Year==2014)
names(SAIPE_2014) <- c("Year", "IDnum", "StateName", "MedIncome_2014", "PercentStuPoverty_2014")
SAIPE_2015 <-dplyr::filter(SAIPE_income, Year==2015)
names(SAIPE_2015) <- c("Year", "IDnum", "StateName", "MedIncome_2015", "PercentStuPoverty_2015")
#merging
Income <- dplyr::left_join(SAIPE_2005, SAIPE_2006, "IDnum") %>% 
  left_join(SAIPE_2007, "IDnum") %>% 
  left_join(SAIPE_2008, "IDnum") %>% 
  left_join(SAIPE_2009, "IDnum") %>% 
  left_join(SAIPE_2010, "IDnum") %>% 
  left_join(SAIPE_2011, "IDnum") %>% 
  left_join(SAIPE_2012, "IDnum") %>% 
  left_join(SAIPE_2013, "IDnum") %>% 
  left_join(SAIPE_2014, "IDnum") %>% 
  left_join(SAIPE_2015, "IDnum")
Income <- dplyr::select(Income, IDnum, StateName, num_range("MedIncome_200", 5:9), num_range("MedIncome_20",10:15), num_range("PercentStuPoverty_200", 5:9), num_range("PercentStuPoverty_20",10:15))
#Exporting
write.csv(Income, "Income_SAIPE2005-2015.csv", row.names = FALSE)
write.csv(SAIPE_income_long, "MedianIncome_long", row.names = FALSE)

#Creating a new dataset with summaryscores for merging
Income_simple <- Income
Income_simple$MedIncome <- rowMeans(Income_simple[,3:13]) 
Income_simple$PercentStuPoverty <- rowMeans(Income_simple[,14:24])
Income_simple <- dplyr::select(Income_simple, StateName, MedIncome, PercentStuPoverty)

#Calculating reliability for income and storing values in a table
MedIncome_foralpha <- dplyr::select(Income, 3:13)
PercentStuPoverty_foralpha <- dplyr::select(Income, 14:24)
MedIncome_alpha <- alpha(MedIncome_foralpha)
PercentStuPoverty_alpha <-alpha(PercentStuPoverty_foralpha)


######## Per Capita Income------------
########Data from US Census, American Community Survey 2005 - 2015 inflation adjusted US dollars

#2005
ACS05_percapi <- read_csv("./RawDataFiles/ACS_05_EST_B19301_with_ann.csv") %>% 
  slice(2:53) %>%  #removing descriptive header
  filter(GEO.id2 != "72") %>%  #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01) #selecting variables of interest
ACS05_percapi[, c(1,3)] <- sapply(ACS05_percapi[, c(1,3)], as.numeric)
#Renaming
names(ACS05_percapi) <- c("IDnum", "StateName", "Percapi_2005")
#Making dataset for long form
ACS05_percapi_long <- ACS05_percapi
ACS05_percapi_long$Year <-  2005
ACS05_percapi_long$Percapi <-  ACS05_percapi_long$Percapi_2005
ACS05_percapi_long <- ACS05_percapi_long %>%
  select(Year, IDnum, StateName, Percapi)

#2006
ACS06_percapi <- read_csv("./RawDataFiles/ACS_06_EST_B19301_with_ann.csv") %>% 
  slice(2:53) %>%  #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01) #selecting variables of interest
ACS06_percapi[, c(1,3)] <- sapply(ACS06_percapi[, c(1,3)], as.numeric)
#Renaming
names(ACS06_percapi) <- c("IDnum", "StateName", "Percapi_2006")
#Making dataset for long form
ACS06_percapi_long <- ACS06_percapi
ACS06_percapi_long$Year <-  2006
ACS06_percapi_long$Percapi <-  ACS06_percapi_long$Percapi_2006
ACS06_percapi_long <- ACS06_percapi_long %>%
  select(Year, IDnum, StateName, Percapi)

#2007
ACS07_percapi <- read_csv("./RawDataFiles/ACS_07_1YR_B19301_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01) #selecting variables of interest
ACS07_percapi[, c(1,3)] <- sapply(ACS07_percapi[, c(1,3)], as.numeric)
#Renaming
names(ACS07_percapi) <- c("IDnum", "StateName", "Percapi_2007")
#Making dataset for long form
ACS07_percapi_long <- ACS07_percapi
ACS07_percapi_long$Year <-  2007
ACS07_percapi_long$Percapi <-  ACS07_percapi_long$Percapi_2007
ACS07_percapi_long <- ACS07_percapi_long %>%
  select(Year, IDnum, StateName, Percapi)

#2008
ACS08_percapi <- read_csv("./RawDataFiles/ACS_08_1YR_B19301_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01) #selecting variables of interest
ACS08_percapi[, c(1,3)] <- sapply(ACS08_percapi[, c(1,3)], as.numeric)
#Renaming
names(ACS08_percapi) <- c("IDnum", "StateName", "Percapi_2008")
#Making dataset for long form
ACS08_percapi_long <- ACS08_percapi
ACS08_percapi_long$Year <-  2008
ACS08_percapi_long$Percapi <-  ACS08_percapi_long$Percapi_2008
ACS08_percapi_long <- ACS08_percapi_long %>%
  select(Year, IDnum, StateName, Percapi)

#2009
ACS09_percapi <- read_csv("./RawDataFiles/ACS_09_1YR_B19301_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01) #selecting variables of interest
ACS09_percapi[, c(1,3)] <- sapply(ACS09_percapi[, c(1,3)], as.numeric)
#Renaming
names(ACS09_percapi) <- c("IDnum", "StateName", "Percapi_2009")
#Making dataset for long form
ACS09_percapi_long <- ACS09_percapi
ACS09_percapi_long$Year <-  2009
ACS09_percapi_long$Percapi <-  ACS09_percapi_long$Percapi_2009
ACS09_percapi_long <- ACS09_percapi_long %>%
  select(Year, IDnum, StateName, Percapi)

#2010
ACS10_percapi <- read_csv("./RawDataFiles/ACS_10_1YR_B19301_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01) #selecting variables of interest
ACS10_percapi[, c(1,3)] <- sapply(ACS10_percapi[, c(1,3)], as.numeric)
#Renaming
names(ACS10_percapi) <- c("IDnum", "StateName", "Percapi_2010")
#Making dataset for long form
ACS10_percapi_long <- ACS10_percapi
ACS10_percapi_long$Year <-  2010
ACS10_percapi_long$Percapi <-  ACS10_percapi_long$Percapi_2010
ACS10_percapi_long <- ACS10_percapi_long %>%
  select(Year, IDnum, StateName, Percapi)

#2011
ACS11_percapi <- read_csv("./RawDataFiles/ACS_11_1YR_B19301_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01) #selecting variables of interest
ACS11_percapi[, c(1,3)] <- sapply(ACS11_percapi[, c(1,3)], as.numeric)
#Renaming
names(ACS11_percapi) <- c("IDnum", "StateName", "Percapi_2011")
#Making dataset for long form
ACS11_percapi_long <- ACS11_percapi
ACS11_percapi_long$Year <-  2011
ACS11_percapi_long$Percapi <-  ACS11_percapi_long$Percapi_2011
ACS11_percapi_long <- ACS11_percapi_long %>%
  select(Year, IDnum, StateName, Percapi)

#2012
ACS12_percapi <- read_csv("./RawDataFiles/ACS_12_1YR_B19301_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01) #selecting variables of interest
ACS12_percapi[, c(1,3)] <- sapply(ACS12_percapi[, c(1,3)], as.numeric)
#Renaming
names(ACS12_percapi) <- c("IDnum", "StateName", "Percapi_2012")
#Making dataset for long form
ACS12_percapi_long <- ACS12_percapi
ACS12_percapi_long$Year <-  2012
ACS12_percapi_long$Percapi <-  ACS12_percapi_long$Percapi_2012
ACS12_percapi_long <- ACS12_percapi_long %>%
  select(Year, IDnum, StateName, Percapi)

#2013
ACS13_percapi <- read_csv("./RawDataFiles/ACS_13_1YR_B19301_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01) #selecting variables of interest
ACS13_percapi[, c(1,3)] <- sapply(ACS13_percapi[, c(1,3)], as.numeric)
#Renaming
names(ACS13_percapi) <- c("IDnum", "StateName", "Percapi_2013")
#Making dataset for long form
ACS13_percapi_long <- ACS13_percapi
ACS13_percapi_long$Year <-  2013
ACS13_percapi_long$Percapi <-  ACS13_percapi_long$Percapi_2013
ACS13_percapi_long <- ACS13_percapi_long %>%
  select(Year, IDnum, StateName, Percapi)

#2014
ACS14_percapi <- read_csv("./RawDataFiles/ACS_14_1YR_B19301_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01) #selecting variables of interest
ACS14_percapi[, c(1,3)] <- sapply(ACS14_percapi[, c(1,3)], as.numeric)
#Renaming
names(ACS14_percapi) <- c("IDnum", "StateName", "Percapi_2014")
#Making dataset for long form
ACS14_percapi_long <- ACS14_percapi
ACS14_percapi_long$Year <-  2014
ACS14_percapi_long$Percapi <-  ACS14_percapi_long$Percapi_2014
ACS14_percapi_long <- ACS14_percapi_long %>%
  select(Year, IDnum, StateName, Percapi)

#2015
ACS15_percapi <- read_csv("./RawDataFiles/ACS_15_1YR_B19301_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01) #selecting variables of interest
ACS15_percapi[, c(1,3)] <- sapply(ACS15_percapi[, c(1,3)], as.numeric)
#Renaming
names(ACS15_percapi) <- c("IDnum", "StateName", "Percapi_2015")
#Making dataset for long form
ACS15_percapi_long <- ACS15_percapi
ACS15_percapi_long$Year <-  2015
ACS15_percapi_long$Percapi <-  ACS15_percapi_long$Percapi_2015
ACS15_percapi_long <- ACS15_percapi_long %>%
  select(Year, IDnum, StateName, Percapi)

###Making long dataset with all years
ACS_percapi_long <- bind_rows(ACS15_percapi_long, ACS14_percapi_long) %>% 
  bind_rows(ACS13_percapi_long) %>% 
  bind_rows(ACS12_percapi_long) %>%
  bind_rows(ACS11_percapi_long) %>% 
  bind_rows(ACS10_percapi_long) %>% 
  bind_rows(ACS09_percapi_long) %>% 
  bind_rows(ACS08_percapi_long) %>% 
  bind_rows(ACS07_percapi_long) %>% 
  bind_rows(ACS06_percapi_long) %>% 
  bind_rows(ACS05_percapi_long)

###Merging all years together for wide
Percapi <- left_join(ACS05_percapi, ACS06_percapi) %>% 
  left_join(ACS07_percapi) %>% 
  left_join(ACS08_percapi) %>% 
  left_join(ACS09_percapi) %>% 
  left_join(ACS10_percapi) %>% 
  left_join(ACS11_percapi) %>% 
  left_join(ACS12_percapi) %>% 
  left_join(ACS13_percapi) %>% 
  left_join(ACS14_percapi) %>% 
  left_join(ACS15_percapi)
#Exporting
write.csv(Percapi, "Percapi_ACS2005-2015.csv", row.names = FALSE)
write.csv(ACS_percapi_long, "Percapi_long.csv", row.names = FALSE)

#Creating a new dataset with summaryscores for merging
Percapi_simple <- Percapi
Percapi_simple$Percapi <- rowMeans(Percapi_simple[,3:13]) 
Percapi_simple <- dplyr::select(Percapi_simple, StateName, Percapi)

#Calculating reliability for gini
Percapi_foralpha <- dplyr::select(Percapi, 3:13)
Percapi_alpha <- psych::alpha(as.data.frame(Percapi_foralpha))


########Gini------------
########Data from US Census, American Community Survey 2006 - 2015
#2006
ACS06_gini <- read_csv("./RawDataFiles/ACS_06_EST_B19083_with_ann.csv") %>% 
  slice(3:54) %>%  #removing descriptive header and US gini estimate
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01)
#making variables numeric
ACS06_gini[, c(1,3)] <- sapply(ACS06_gini[, c(1,3)], as.numeric)
#Renaming
names(ACS06_gini) <- c("IDnum", "StateName", "Gini_2006")
#Making dataset for long form
ACS06_gini_long <- ACS06_gini
ACS06_gini_long$Year <-  2006
ACS06_gini_long$Gini <-  ACS06_gini_long$Gini_2006
ACS06_gini_long <- ACS06_gini_long %>%
  select(Year, IDnum, StateName, Gini)

#2007
ACS07_gini <- read_csv("./RawDataFiles/ACS_07_1YR_B19083_with_ann.csv") %>% 
  slice(3:54) %>% #removing descriptive header and US gini estimate
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01)
#making variables numeric
ACS07_gini[, c(1,3)] <- sapply(ACS07_gini[, c(1,3)], as.numeric)
#Renaming
names(ACS07_gini) <- c("IDnum", "StateName", "Gini_2007")
#Making dataset for long form
ACS07_gini_long <- ACS07_gini
ACS07_gini_long$Year <-  2007
ACS07_gini_long$Gini <-  ACS07_gini_long$Gini_2007
ACS07_gini_long <- ACS07_gini_long %>%
  select(Year, IDnum, StateName, Gini)

#2008
ACS08_gini <- read_csv("./RawDataFiles/ACS_08_1YR_B19083_with_ann.csv") %>% 
  slice(3:54) %>% #removing descriptive header and US gini estimate
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01)
#making variables numeric
ACS08_gini[, c(1,3)] <- sapply(ACS08_gini[, c(1,3)], as.numeric)
#Renaming
names(ACS08_gini) <- c("IDnum", "StateName", "Gini_2008")
#Making dataset for long form
ACS08_gini_long <- ACS08_gini
ACS08_gini_long$Year <-  2008
ACS08_gini_long$Gini <-  ACS08_gini_long$Gini_2008
ACS08_gini_long <- ACS08_gini_long %>%
  select(Year, IDnum, StateName, Gini)

#2009
ACS09_gini <- read_csv("./RawDataFiles/ACS_09_1YR_B19083_with_ann.csv") %>% 
  slice(3:54) %>% #removing descriptive header and US gini estimate
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01)
#making variables numeric
ACS09_gini[, c(1,3)] <- sapply(ACS09_gini[, c(1,3)], as.numeric)
#Renaming
names(ACS09_gini) <- c("IDnum", "StateName", "Gini_2009")
#Making dataset for long form
ACS09_gini_long <- ACS09_gini
ACS09_gini_long$Year <-  2009
ACS09_gini_long$Gini <-  ACS09_gini_long$Gini_2009
ACS09_gini_long <- ACS09_gini_long %>%
  select(Year, IDnum, StateName, Gini)

#2010
ACS10_gini <- read_csv("./RawDataFiles/ACS_10_1YR_B19083_with_ann.csv") %>% 
  slice(3:54) %>% #removing descriptive header and US gini estimate
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01)
#making variables numeric
ACS10_gini[, c(1,3)] <- sapply(ACS10_gini[, c(1,3)], as.numeric)
#Renaming
names(ACS10_gini) <- c("IDnum", "StateName", "Gini_2010")
#Making dataset for long form
ACS10_gini_long <- ACS10_gini
ACS10_gini_long$Year <-  2010
ACS10_gini_long$Gini <-  ACS10_gini_long$Gini_2010
ACS10_gini_long <- ACS10_gini_long %>%
  select(Year, IDnum, StateName, Gini)

#2011
ACS11_gini <- read_csv("./RawDataFiles/ACS_11_1YR_B19083_with_ann.csv") %>% 
  slice(3:54) %>% #removing descriptive header and US gini estimate
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01)
#making variables numeric
ACS11_gini[, c(1,3)] <- sapply(ACS11_gini[, c(1,3)], as.numeric)
#Renaming
names(ACS11_gini) <- c("IDnum", "StateName", "Gini_2011")
#Making dataset for long form
ACS11_gini_long <- ACS11_gini
ACS11_gini_long$Year <-  2011
ACS11_gini_long$Gini <-  ACS11_gini_long$Gini_2011
ACS11_gini_long <- ACS11_gini_long %>%
  select(Year, IDnum, StateName, Gini)

#2012
ACS12_gini <- read_csv("./RawDataFiles/ACS_12_1YR_B19083_with_ann.csv") %>% 
  slice(3:54) %>% #removing descriptive header and US gini estimate
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01)
#making variables numeric
ACS12_gini[, c(1,3)] <- sapply(ACS12_gini[, c(1,3)], as.numeric)
#Renaming
names(ACS12_gini) <- c("IDnum", "StateName", "Gini_2012")
#Making dataset for long form
ACS12_gini_long <- ACS12_gini
ACS12_gini_long$Year <-  2012
ACS12_gini_long$Gini <-  ACS12_gini_long$Gini_2012
ACS12_gini_long <- ACS12_gini_long %>%
  select(Year, IDnum, StateName, Gini)

#2013
ACS13_gini <- read_csv("./RawDataFiles/ACS_13_1YR_B19083_with_ann.csv") %>% 
  slice(3:54) %>% #removing descriptive header and US gini estimate
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01)
#making variables numeric
ACS13_gini[, c(1,3)] <- sapply(ACS13_gini[, c(1,3)], as.numeric)
#Renaming
names(ACS13_gini) <- c("IDnum", "StateName", "Gini_2013")
#Making dataset for long form
ACS13_gini_long <- ACS13_gini
ACS13_gini_long$Year <-  2013
ACS13_gini_long$Gini <-  ACS13_gini_long$Gini_2013
ACS13_gini_long <- ACS13_gini_long %>%
  select(Year, IDnum, StateName, Gini)

#2014
ACS14_gini <- read_csv("./RawDataFiles/ACS_14_1YR_B19083_with_ann.csv") %>% 
  slice(3:54) %>% #removing descriptive header and US gini estimate
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01)
#making variables numeric
ACS14_gini[, c(1,3)] <- sapply(ACS14_gini[, c(1,3)], as.numeric)
#Renaming
names(ACS14_gini) <- c("IDnum", "StateName", "Gini_2014")
#Making dataset for long form
ACS14_gini_long <- ACS14_gini
ACS14_gini_long$Year <-  2014
ACS14_gini_long$Gini <-  ACS14_gini_long$Gini_2014
ACS14_gini_long <- ACS14_gini_long %>%
  select(Year, IDnum, StateName, Gini)

#2015
ACS15_gini <- read_csv("./RawDataFiles/ACS_15_1YR_B19083_with_ann.csv") %>% 
  slice(3:54) %>% #removing descriptive header and US gini estimate
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, HD01_VD01)
#making variables numeric
ACS15_gini[, c(1,3)] <- sapply(ACS15_gini[, c(1,3)], as.numeric)
#Renaming
names(ACS15_gini) <- c("IDnum", "StateName", "Gini_2015")
#Making dataset for long form
ACS15_gini_long <- ACS15_gini
ACS15_gini_long$Year <-  2015
ACS15_gini_long$Gini <-  ACS15_gini_long$Gini_2015
ACS15_gini_long <- ACS15_gini_long %>%
  select(Year, IDnum, StateName, Gini)

###Making long dataset with all years
ACS_gini_long <- bind_rows(ACS15_gini_long, ACS14_gini_long) %>% 
  bind_rows(ACS13_gini_long) %>% 
  bind_rows(ACS12_gini_long) %>%
  bind_rows(ACS11_gini_long) %>% 
  bind_rows(ACS10_gini_long) %>% 
  bind_rows(ACS09_gini_long) %>% 
  bind_rows(ACS08_gini_long) %>% 
  bind_rows(ACS07_gini_long) %>% 
  bind_rows(ACS06_gini_long)

###Merging all years together
Gini <- left_join(ACS06_gini, ACS07_gini) %>% 
  left_join(ACS08_gini) %>% 
  left_join(ACS09_gini) %>% 
  left_join(ACS10_gini) %>% 
  left_join(ACS11_gini) %>% 
  left_join(ACS12_gini) %>% 
  left_join(ACS13_gini) %>% 
  left_join(ACS14_gini) %>% 
  left_join(ACS15_gini)
#Exporting
write.csv(Gini, "Gini_ACS2006-2015.csv", row.names = FALSE)
write.csv(ACS_gini_long, "Gini_long.csv", row.names = FALSE)

#Creating a new dataset with summaryscores for merging
Gini_simple <- Gini
Gini_simple$Gini <- rowMeans(Gini_simple[,3:12]) 
Gini_simple <- dplyr::select(Gini_simple, StateName, Gini)

#Calculating reliability for gini
Gini_foralpha <- dplyr::select(Gini, 3:12)
Gini_alpha <- psych::alpha(as.data.frame(Gini_foralpha))


########Educational Attainment------------
########Data from US Census, American Community Survey 2005 - 2015

####ACS 2005
ACS05_attain <- read_csv("./RawDataFiles/ACS_05_EST_S1501_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, num_range("HC01_EST_VC0", 6:9), num_range("HC01_EST_VC", 10:22)) #selecting est population >25 and attainment est for that group
#making variables numeric
ACS05_attain[, c(1,3:19)] <- sapply(ACS05_attain[, c(1,3:19)], as.numeric)
#Renaming
names(ACS05_attain) <- c("IDnum", "StateName", "PopOver25_2005", "Less9th_2005.1", "Less9th_2005.2", "Less9th_2005.3", "Less9th_2005.4","HSnoDiploma_2005.1", "HSnoDiploma_2005.2", "HSnoDiploma_2005.3", "HSnoDiploma_2005.4","Hsgrad_2005","LilCollege_2005.1", "LilCollege_2005.2","AssocDeg_2005","BachDeg_2005","GradDeg_2005.1", "GradDeg_2005.2", "GradDeg_2005.3")
#Calculating totals
ACS05_attain <- ACS05_attain %>% 
 mutate(Less9th_2005 = Less9th_2005.1+Less9th_2005.2+Less9th_2005.3+Less9th_2005.4,
        HSnoDiploma_2005=HSnoDiploma_2005.1+HSnoDiploma_2005.2+HSnoDiploma_2005.3+HSnoDiploma_2005.4,
        LilCollege_2005=LilCollege_2005.1+LilCollege_2005.2,
        GradDeg_2005=GradDeg_2005.1+GradDeg_2005.2+GradDeg_2005.3,
        Hsgradormore_2005 = Hsgrad_2005+LilCollege_2005+AssocDeg_2005+ BachDeg_2005+GradDeg_2005,
        BachDegormore_2005 = BachDeg_2005+GradDeg_2005)
#Deleting old variables
ACS05_attain <- dplyr::select(ACS05_attain, "IDnum", "StateName", "PopOver25_2005", "Less9th_2005","HSnoDiploma_2005","Hsgrad_2005","LilCollege_2005","AssocDeg_2005","BachDeg_2005","GradDeg_2005", "Hsgradormore_2005", "BachDegormore_2005")
#Making dataset for long form
ACS05_attain_long <- ACS05_attain
names(ACS05_attain_long) <- c("IDnum", "StateName", "PopOver25", "Less9th","HSnoDiploma","Hsgrad","LilCollege","AssocDeg","BachDeg","GradDeg", "Hsgradormore", "BachDegormore") #Assigning new names that don't have the year
ACS05_attain_long$Year <- 2005
#Truncating new values at tenths place
ACS05_attain[, c(3:12)] <- round(ACS05_attain[, c(3:12)], digits = 1)

#Variable name and grouping changes in 2006
####ACS 2006
ACS06_attain <- read_csv("./RawDataFiles/ACS_06_EST_S1501_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, num_range("HC01_EST_VC0", 6:9), num_range("HC01_EST_VC", 10:15)) #selecting est population >25 and attainment est for that group
#making variables numeric
ACS06_attain[, c(1,3:12)] <- sapply(ACS06_attain[, c(1,3:12)], as.numeric)
#renaming variables 
names(ACS06_attain) <- c("IDnum", "StateName", "PopOver25_2006", "Less9th_2006","HSnoDiploma_2006","Hsgrad_2006","LilCollege_2006","AssocDeg_2006","BachDeg_2006","GradDeg_2006", "Hsgradormore_2006", "BachDegormore_2006")
#Making dataset for long form
ACS06_attain_long <- ACS06_attain
names(ACS06_attain_long) <- c("IDnum", "StateName", "PopOver25", "Less9th","HSnoDiploma","Hsgrad","LilCollege","AssocDeg","BachDeg","GradDeg", "Hsgradormore", "BachDegormore") #Assigning new names that don't have the year
ACS06_attain_long$Year <- 2006

####ACS 2007
ACS07_attain <- read_csv("./RawDataFiles/ACS_07_1YR_S1501_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, num_range("HC01_EST_VC0", 6:9), num_range("HC01_EST_VC", 10:15)) #selecting est population >25 and attainment est for that group
#making variables numeric
ACS07_attain[, c(1,3:12)] <- sapply(ACS07_attain[, c(1,3:12)], as.numeric)
#renaming variables 
names(ACS07_attain) <- c("IDnum", "StateName", "PopOver25_2007", "Less9th_2007","HSnoDiploma_2007","Hsgrad_2007","LilCollege_2007","AssocDeg_2007","BachDeg_2007","GradDeg_2007", "Hsgradormore_2007", "BachDegormore_2007")
#Making dataset for long form
ACS07_attain_long <- ACS07_attain
names(ACS07_attain_long) <- c("IDnum", "StateName", "PopOver25", "Less9th","HSnoDiploma","Hsgrad","LilCollege","AssocDeg","BachDeg","GradDeg", "Hsgradormore", "BachDegormore") #Assigning new names that don't have the year
ACS07_attain_long$Year <- 2007


####ACS 2008
ACS08_attain <- read_csv("./RawDataFiles/ACS_08_1YR_S1501_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, num_range("HC01_EST_VC0", 6:9), num_range("HC01_EST_VC", 10:15)) #selecting est population >25 and attainment est for that group
#making variables numeric
ACS08_attain[, c(1,3:12)] <- sapply(ACS08_attain[, c(1,3:12)], as.numeric)
#renaming variables 
names(ACS08_attain) <- c("IDnum", "StateName", "PopOver25_2008", "Less9th_2008","HSnoDiploma_2008","Hsgrad_2008","LilCollege_2008","AssocDeg_2008","BachDeg_2008","GradDeg_2008", "Hsgradormore_2008", "BachDegormore_2008")
#Making dataset for long form
ACS08_attain_long <- ACS08_attain
names(ACS08_attain_long) <- c("IDnum", "StateName", "PopOver25", "Less9th","HSnoDiploma","Hsgrad","LilCollege","AssocDeg","BachDeg","GradDeg", "Hsgradormore", "BachDegormore") #Assigning new names that don't have the year
ACS08_attain_long$Year <- 2008

####ACS 2009
ACS09_attain <- read_csv("./RawDataFiles/ACS_09_1YR_S1501_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, num_range("HC01_EST_VC0", 6:9), num_range("HC01_EST_VC", 10:15)) #selecting est population >25 and attainment est for that group
#making variables numeric
ACS09_attain[, c(1,3:12)] <- sapply(ACS09_attain[, c(1,3:12)], as.numeric)
#renaming variables 
names(ACS09_attain) <- c("IDnum", "StateName", "PopOver25_2009", "Less9th_2009","HSnoDiploma_2009","Hsgrad_2009","LilCollege_2009","AssocDeg_2009","BachDeg_2009","GradDeg_2009", "Hsgradormore_2009", "BachDegormore_2009")
#Making dataset for long form
ACS09_attain_long <- ACS09_attain
names(ACS09_attain_long) <- c("IDnum", "StateName", "PopOver25", "Less9th","HSnoDiploma","Hsgrad","LilCollege","AssocDeg","BachDeg","GradDeg", "Hsgradormore", "BachDegormore") #Assigning new names that don't have the year
ACS09_attain_long$Year <- 2009


#Slight variable name changes in 2010
####ACS 2010
ACS10_attain <- read_csv("./RawDataFiles/ACS_10_1YR_S1501_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, num_range("HC01_EST_VC0", 7:9), num_range("HC01_EST_VC", 10:14), num_range("HC01_EST_VC", 16:17)) #selecting est population >25 and attainment est for that group
#making variables numeric
ACS10_attain[, c(1,3:12)] <- sapply(ACS10_attain[, c(1,3:12)], as.numeric)
#renaming variables 
names(ACS10_attain) <- c("IDnum", "StateName", "PopOver25_2010", "Less9th_2010","HSnoDiploma_2010","Hsgrad_2010","LilCollege_2010","AssocDeg_2010","BachDeg_2010","GradDeg_2010", "Hsgradormore_2010", "BachDegormore_2010")
#Making dataset for long form
ACS10_attain_long <- ACS10_attain
names(ACS10_attain_long) <- c("IDnum", "StateName", "PopOver25", "Less9th","HSnoDiploma","Hsgrad","LilCollege","AssocDeg","BachDeg","GradDeg", "Hsgradormore", "BachDegormore") #Assigning new names that don't have the year
ACS10_attain_long$Year <- 2010


####ACS 2011
ACS11_attain <- read_csv("./RawDataFiles/ACS_11_1YR_S1501_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, num_range("HC01_EST_VC0", 7:9), num_range("HC01_EST_VC", 10:14), num_range("HC01_EST_VC", 16:17)) #selecting est population >25 and attainment est for that group
#making variables numeric
ACS11_attain[, c(1,3:12)] <- sapply(ACS11_attain[, c(1,3:12)], as.numeric)
#renaming variables 
names(ACS11_attain) <- c("IDnum", "StateName", "PopOver25_2011", "Less9th_2011","HSnoDiploma_2011","Hsgrad_2011","LilCollege_2011","AssocDeg_2011","BachDeg_2011","GradDeg_2011", "Hsgradormore_2011", "BachDegormore_2011")
#Making dataset for long form
ACS11_attain_long <- ACS11_attain
names(ACS11_attain_long) <- c("IDnum", "StateName", "PopOver25", "Less9th","HSnoDiploma","Hsgrad","LilCollege","AssocDeg","BachDeg","GradDeg", "Hsgradormore", "BachDegormore") #Assigning new names that don't have the year
ACS11_attain_long$Year <- 2011


####ACS 2012
ACS12_attain <- read_csv("./RawDataFiles/ACS_12_1YR_S1501_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") #removing DC
ACS12_attain <- dplyr::select(ACS12_attain$GEO.id2, `GEO.display-label`, num_range("HC01_EST_VC0", 7:9), num_range("HC01_EST_VC", 10:14), num_range("HC01_EST_VC", 16:17)) #selecting est population >25 and attainment est for that group
#making variables numeric
ACS12_attain[, c(1,3:12)] <- sapply(ACS12_attain[, c(1,3:12)], as.numeric)
#renaming variables 
names(ACS12_attain) <- c("IDnum", "StateName", "PopOver25_2012", "Less9th_2012","HSnoDiploma_2012","Hsgrad_2012","LilCollege_2012","AssocDeg_2012","BachDeg_2012","GradDeg_2012", "Hsgradormore_2012", "BachDegormore_2012")
#Making dataset for long form
ACS12_attain_long <- ACS12_attain
names(ACS12_attain_long) <- c("IDnum", "StateName", "PopOver25", "Less9th","HSnoDiploma","Hsgrad","LilCollege","AssocDeg","BachDeg","GradDeg", "Hsgradormore", "BachDegormore") #Assigning new names that don't have the year
ACS12_attain_long$Year <- 2012

####ACS 2013
ACS13_attain <- read_csv("./RawDataFiles/ACS_13_1YR_S1501_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(GEO.id2, `GEO.display-label`, num_range("HC01_EST_VC0", 7:9), num_range("HC01_EST_VC", 10:14), num_range("HC01_EST_VC", 16:17)) %>% #selecting est population >25 and attainment est for that group
#making variables numeric
ACS13_attain[, c(1,3:12)] <- sapply(ACS13_attain[, c(1,3:12)], as.numeric)
#renaming variables 
names(ACS13_attain) <- c("IDnum", "StateName", "PopOver25_2013", "Less9th_2013","HSnoDiploma_2013","Hsgrad_2013","LilCollege_2013","AssocDeg_2013","BachDeg_2013","GradDeg_2013", "Hsgradormore_2013", "BachDegormore_2013")
#Making dataset for long form
ACS13_attain_long <- ACS13_attain
names(ACS13_attain_long) <- c("IDnum", "StateName", "PopOver25", "Less9th","HSnoDiploma","Hsgrad","LilCollege","AssocDeg","BachDeg","GradDeg", "Hsgradormore", "BachDegormore") #Assigning new names that don't have the year
ACS13_attain_long$Year <- 2013

####ACS 2014
ACS14_attain <- read_csv("./RawDataFiles/ACS_14_1YR_S1501_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(ACS14_attain, GEO.id2, `GEO.display-label`, num_range("HC01_EST_VC0", 7:9), num_range("HC01_EST_VC", 10:14), num_range("HC01_EST_VC", 16:17)) %>% #selecting est population >25 and attainment est for that group
#making variables numeric
ACS14_attain[, c(1,3:12)] <- sapply(ACS14_attain[, c(1,3:12)], as.numeric)
#renaming variables 
names(ACS14_attain) <- c("IDnum", "StateName", "PopOver25_2014", "Less9th_2014","HSnoDiploma_2014","Hsgrad_2014","LilCollege_2014","AssocDeg_2014","BachDeg_2014","GradDeg_2014", "Hsgradormore_2014", "BachDegormore_2014")
#Making dataset for long form
ACS14_attain_long <- ACS14_attain
names(ACS14_attain_long) <- c("IDnum", "StateName", "PopOver25", "Less9th","HSnoDiploma","Hsgrad","LilCollege","AssocDeg","BachDeg","GradDeg", "Hsgradormore", "BachDegormore") #Assigning new names that don't have the year
ACS14_attain_long$Year <- 2014

#Slight variable name changes and variable changes in 2015
####ACS 2015
ACS15_attain <- read_csv("./RawDataFiles/ACS_15_1YR_S1501_with_ann.csv") %>% 
  slice(2:53) %>% #removing descriptive header
  filter(GEO.id2 != "72") %>% #removing Puerto Rico
  filter(GEO.id2 != "11") %>% #removing DC
  select(ACS15_attain, GEO.id2, `GEO.display-label`, num_range("HC01_EST_VC0", 8:9), num_range("HC01_EST_VC", 10:15)) %>% #selecting est population >25 and attainment est for that group
#note, didn't include two empty variables that should have percentage > hs and > bach
#making variables numeric
ACS15_attain[, c(1,3:10)] <- sapply(ACS15_attain[, c(1,3:10)], as.numeric)
#renaming variables 
names(ACS15_attain) <- c("IDnum", "StateName", "PopOver25_2015", "Less9th_2015","HSnoDiploma_2015","Hsgrad_2015","LilCollege_2015","AssocDeg_2015","BachDeg_2015","GradDeg_2015")
#Making dataset for long form
ACS15_attain_long <- ACS15_attain
names(ACS15_attain_long) <- c("IDnum", "StateName", "PopOver25", "Less9th","HSnoDiploma","Hsgrad","LilCollege","AssocDeg","BachDeg","GradDeg", "Hsgradormore", "BachDegormore") #Assigning new names that don't have the year
ACS15_attain_long$Year <- 2015

#calculating percentages
ACS15_attain <- ACS15_attain %>% 
  mutate(Less9th_2015 = (ACS15_attain$Less9th_2015/ACS15_attain$PopOver25_2015)*100,
    HSnoDiploma_2015 = (ACS15_attain$HSnoDiploma_2015/ACS15_attain$PopOver25_2015)*100,
    Hsgrad_2015 = (ACS15_attain$Hsgrad_2015/ACS15_attain$PopOver25_2015)*100,
    LilCollege_2015 = (ACS15_attain$LilCollege_2015/ACS15_attain$PopOver25_2015)*100,
    AssocDeg_2015 = (ACS15_attain$AssocDeg_2015/ACS15_attain$PopOver25_2015)*100,
    BachDeg_2015 = (ACS15_attain$BachDeg_2015/ACS15_attain$PopOver25_2015)*100,
    GradDeg_2015 = (ACS15_attain$GradDeg_2015/ACS15_attain$PopOver25_2015)*100,
#Calculating totals
    Hsgradormore_2015 = Hsgrad_2015+LilCollege_2015+AssocDeg_2015+ BachDeg_2015+GradDeg_2015,
    BachDegormore_2015 = BachDeg_2015+GradDeg_2015)
#Truncating new percentage variables at tenths place
ACS15_attain[, c(3:12)] <- round(ACS15_attain[, c(3:12)], digits = 1)

###Merging all years together
EducationalAttainment <- left_join(ACS05_attain, ACS06_attain) %>% 
  left_join(ACS07_attain) %>% 
  left_join(ACS08_attain) %>% 
  left_join(ACS09_attain) %>% 
  left_join(ACS10_attain) %>% 
  left_join(ACS11_attain) %>% 
  left_join(ACS12_attain) %>% 
  left_join(ACS13_attain) %>% 
  left_join(ACS14_attain) %>% 
  left_join(ACS15_attain)

###Making long dataset with all years
ACS_attain_long <- bind_rows(ACS15_attain_long, ACS14_attain_long) %>% 
  bind_rows(ACS13_attain_long) %>% 
  bind_rows(ACS12_attain_long) %>%
  bind_rows(ACS11_attain_long) %>% 
  bind_rows(ACS10_attain_long) %>% 
  bind_rows(ACS09_attain_long) %>% 
  bind_rows(ACS08_attain_long) %>% 
  bind_rows(ACS07_attain_long) %>% 
  bind_rows(ACS06_attain_long) %>% 
  bind_rows(ACS05_attain_long)


#Exporting
write.csv(EducationalAttainment, "EducationalAttainment_ACS2005-2015.csv", row.names = FALSE)
write.csv(ACS_attain_long, "Attain_long.csv", row.names = FALSE)

#Creating a new dataset for merging
EducationalAttainment_simple <- EducationalAttainment
Hsgrad_columns <- dplyr::select(EducationalAttainment_simple, starts_with ("Hsgradormore"))
EducationalAttainment_simple$Hsgradormore <- rowMeans(Hsgrad_columns) 
BachDegormore_columns <- dplyr::select(EducationalAttainment_simple, starts_with ("BachDegormore"))
EducationalAttainment_simple$BachDegormore <- rowMeans(BachDegormore_columns) 
GradDeg_columns <- dplyr::select(EducationalAttainment_simple, starts_with ("GradDeg"))
GradDeg_columns <- dplyr::select(GradDeg_columns, 4:14)
EducationalAttainment_simple$GradDeg <- rowMeans(GradDeg_columns) 
HSnoDiploma_columns <- dplyr::select(EducationalAttainment_simple, starts_with ("HSnoDiploma"))
HSnoDiploma_columns <- dplyr::select(HSnoDiploma_columns, 5:15)
EducationalAttainment_simple$HSnoDiploma <- rowMeans(HSnoDiploma_columns) 
Less9th_columns <- dplyr::select(EducationalAttainment_simple, starts_with ("Less9th"))
Less9th_columns <- dplyr::select(Less9th_columns, 5:15)
EducationalAttainment_simple$Less9th <- rowMeans(Less9th_columns) 
EducationalAttainment_simple <- dplyr::select(EducationalAttainment_simple, StateName, Less9th, HSnoDiploma, Hsgradormore, BachDegormore, GradDeg)


#Calculating reliability for attainment variables
Less9th_alpha <- psych::alpha(as.data.frame(Less9th_columns))
HSnoDiploma_alpha <- psych::alpha(as.data.frame(HSnoDiploma_columns))
Hsgradormore_alpha <- psych::alpha(as.data.frame(Hsgrad_columns))
BachDegormore_alpha <- psych::alpha(as.data.frame(BachDegormore_columns))
GradDeg_alpha <- psych::alpha(as.data.frame(GradDeg_columns))

########Educational Achievement
########Educational Achievement====
#Vocab====
NCES_Vocab8 <- read_excel("./RawDataFiles/NCES_Vocab8_Oct12017.xls", 
                          col_types = c("numeric", "text", "numeric", 
                                        "blank"), skip = 11)
#subsetting by year
#2013
NCES_Vocab8_2013 <- dplyr::slice(NCES_Vocab8, 53:104)
names(NCES_Vocab8_2013) <- c("Year", "StateName", "Vocab8_2013")
NCES_Vocab8_2013 <- dplyr::select(NCES_Vocab8_2013,"StateName", "Vocab8_2013")
#2011
NCES_Vocab8_2011 <- dplyr::slice(NCES_Vocab8, 105:156)
names(NCES_Vocab8_2011) <- c("Year", "StateName", "Vocab8_2011")
NCES_Vocab8_2011 <- dplyr::select(NCES_Vocab8_2011,"StateName", "Vocab8_2011")
#2009
NCES_Vocab8_2009 <- dplyr::slice(NCES_Vocab8, 157:208)
names(NCES_Vocab8_2009) <- c("Year", "StateName", "Vocab8_2009")
NCES_Vocab8_2009 <- dplyr::select(NCES_Vocab8_2009,"StateName", "Vocab8_2009")
#Merging
Vocab8 <- left_join(NCES_Vocab8_2009, NCES_Vocab8_2011, "StateName") %>% 
  left_join(NCES_Vocab8_2013, "StateName") 
Vocab8 <- Vocab8 %>% filter(Vocab8$StateName != "District of Columbia")
Vocab8 <- Vocab8 %>% filter(Vocab8$StateName != "DoDEA")
#making long dataset
Vocab8_long <- Vocab8 %>% 
  gather("Year", "Vocab8", -StateName) %>%
  separate(Year, c("Var", "Year")) %>% 
  select(-Var)


NCES_Vocab4 <- read_excel("./RawDataFiles/NCES_Vocab4_Oct12017.xls", 
                          col_types = c("numeric", "text", "numeric", 
                                        "blank"), skip = 11)
#subsetting by year
#2013
NCES_Vocab4_2013 <- dplyr::slice(NCES_Vocab4, 53:104)
names(NCES_Vocab4_2013) <- c("Year", "StateName", "Vocab4_2013")
NCES_Vocab4_2013 <- dplyr::select(NCES_Vocab4_2013,"StateName", "Vocab4_2013")
#2011
NCES_Vocab4_2011 <- dplyr::slice(NCES_Vocab4, 105:156)
names(NCES_Vocab4_2011) <- c("Year", "StateName", "Vocab4_2011")
NCES_Vocab4_2011 <- dplyr::select(NCES_Vocab4_2011,"StateName", "Vocab4_2011")
#2009
NCES_Vocab4_2009 <- dplyr::slice(NCES_Vocab4, 157:208)
names(NCES_Vocab4_2009) <- c("Year", "StateName", "Vocab4_2009")
NCES_Vocab4_2009 <- dplyr::select(NCES_Vocab4_2009,"StateName", "Vocab4_2009")
#Merging
Merge1 <- dplyr::left_join(NCES_Vocab4_2009, NCES_Vocab4_2011, "StateName")
Vocab4 <- dplyr::left_join(Merge1, NCES_Vocab4_2013, "StateName")
Vocab4 <- dplyr::filter(Vocab4, Vocab4$StateName != "District of Columbia")
Vocab4 <- dplyr::filter(Vocab4, Vocab4$StateName != "DoDEA")

Vocab <- dplyr::left_join(Vocab4, Vocab8, "StateName")

#making long dataset
Vocab4_long <- Vocab4 %>% 
  gather("Year", "Vocab4", -StateName) %>%
  separate(Year, c("Var", "Year")) %>% 
  select(-Var)

#Science====

NCES_Science8 <- read_excel("./RawDataFiles/NCES_Science8_Oct12017.xls", 
                            col_types = c("numeric", "text", "numeric", 
                                          "blank"), skip = 11)
#subsetting by year
#2015
NCES_Science8_2015 <- dplyr::slice(NCES_Science8, 1:52)
names(NCES_Science8_2015) <- c("Year", "StateName", "Science8_2015")
NCES_Science8_2015 <- dplyr::select(NCES_Science8_2015,"StateName", "Science8_2015")
#2011
NCES_Science8_2011 <- dplyr::slice(NCES_Science8, 53:104)
names(NCES_Science8_2011) <- c("Year", "StateName", "Science8_2011")
NCES_Science8_2011 <- dplyr::select(NCES_Science8_2011,"StateName", "Science8_2011")
#2009
NCES_Science8_2009 <- dplyr::slice(NCES_Science8, 105:156)
names(NCES_Science8_2009) <- c("Year", "StateName", "Science8_2009")
NCES_Science8_2009 <- dplyr::select(NCES_Science8_2009,"StateName", "Science8_2009")
#Merging
Merge1 <- dplyr::left_join(NCES_Science8_2009, NCES_Science8_2011, "StateName")
Science8 <- dplyr::left_join(Merge1, NCES_Science8_2015, "StateName")
Science8 <- dplyr::filter(Science8, Science8$StateName != "District of Columbia")
Science8 <- dplyr::filter(Science8, Science8$StateName != "DoDEA")

#making long dataset
Science8_long <- Science8 %>% 
  gather("Year", "Science8", -StateName) %>%
  separate(Year, c("Var", "Year")) %>% 
  select(-Var)


NCES_Science4 <- read_excel("./RawDataFiles/NCES_Science4_Oct12017.xls", 
                            col_types = c("numeric", "text", "numeric", 
                                          "blank"), skip = 11)
#subsetting by year
#2015
NCES_Science4_2015 <- dplyr::slice(NCES_Science4, 1:52)
names(NCES_Science4_2015) <- c("Year", "StateName", "Science4_2015")
NCES_Science4_2015 <- dplyr::select(NCES_Science4_2015,"StateName", "Science4_2015")
#2009
NCES_Science4_2009 <- dplyr::slice(NCES_Science4, 53:104)
names(NCES_Science4_2009) <- c("Year", "StateName", "Science4_2009")
NCES_Science4_2009 <- dplyr::select(NCES_Science4_2009,"StateName", "Science4_2009")
#Merging
Science4 <- dplyr::left_join(NCES_Science4_2009, NCES_Science4_2015, "StateName")
Science4 <- dplyr::filter(Science4, Science4$StateName != "District of Columbia")
Science4 <- dplyr::filter(Science4, Science4$StateName != "DoDEA")

Science <- dplyr::left_join(Science4, Science8, "StateName")

#making long dataset
Science4_long <- Science4 %>% 
  gather("Year", "Science4", -StateName) %>%
  separate(Year, c("Var", "Year")) %>% 
  select(-Var)

#Reading====

#8th grade
#subsetting by year
NCES_Reading8 <- read_excel("./RawDataFiles/NCES_Reading8_Oct12017.xls", 
                            col_types = c("numeric", "text", "numeric", 
                                          "blank"), skip = 11)
#2015
NCES_Reading8_2015 <- dplyr::slice(NCES_Reading8, 1:52)
names(NCES_Reading8_2015) <- c("Year", "StateName", "Reading8_2015")
NCES_Reading8_2015 <- dplyr::select(NCES_Reading8_2015, "StateName", "Reading8_2015")
#2013
NCES_Reading8_2013 <- dplyr::slice(NCES_Reading8, 53:104)
names(NCES_Reading8_2013) <- c("Year", "StateName", "Reading8_2013")
NCES_Reading8_2013 <- dplyr::select(NCES_Reading8_2013, "StateName", "Reading8_2013")
#2011
NCES_Reading8_2011 <- dplyr::slice(NCES_Reading8, 105:156)
names(NCES_Reading8_2011) <- c("Year", "StateName", "Reading8_2011")
NCES_Reading8_2011 <- dplyr::select(NCES_Reading8_2011, "StateName", "Reading8_2011")
#2009
NCES_Reading8_2009 <- dplyr::slice(NCES_Reading8, 157:208)
names(NCES_Reading8_2009) <- c("Year", "StateName", "Reading8_2009")
NCES_Reading8_2009 <- dplyr::select(NCES_Reading8_2009, "StateName", "Reading8_2009")
#2007
NCES_Reading8_2007 <- dplyr::slice(NCES_Reading8, 209:260)
names(NCES_Reading8_2007) <- c("Year", "StateName", "Reading8_2007")
NCES_Reading8_2007 <- dplyr::select(NCES_Reading8_2007, "StateName", "Reading8_2007")
#2005
NCES_Reading8_2005 <- dplyr::slice(NCES_Reading8, 261:312)
names(NCES_Reading8_2005) <- c("Year", "StateName", "Reading8_2005")
NCES_Reading8_2005 <- dplyr::select(NCES_Reading8_2005, "StateName", "Reading8_2005")

#Merging
Merge1 <- dplyr::left_join(NCES_Reading8_2005, NCES_Reading8_2007, "StateName")
Merge2 <- dplyr::left_join(Merge1, NCES_Reading8_2009, "StateName")
Merge3 <- dplyr::left_join(Merge2, NCES_Reading8_2011, "StateName")
Merge4 <- dplyr::left_join(Merge3, NCES_Reading8_2013, "StateName")
Reading8 <- dplyr::left_join(Merge4, NCES_Reading8_2015, "StateName")
Reading8 <- dplyr::filter(Reading8, Reading8$StateName != "District of Columbia")
Reading8 <- dplyr::filter(Reading8, Reading8$StateName != "DoDEA")

#4th grade
NCES_Reading4 <- read_excel("./RawDataFiles/NCES_Reading4_Oct12017.xls", 
                            col_types = c("numeric", "text", "numeric", 
                                          "blank"), skip = 11)
#2015
NCES_Reading4_2015 <- dplyr::slice(NCES_Reading4, 1:52)
names(NCES_Reading4_2015) <- c("Year", "StateName", "Reading4_2015")
NCES_Reading4_2015 <- dplyr::select(NCES_Reading4_2015, "StateName", "Reading4_2015")
#2013
NCES_Reading4_2013 <- dplyr::slice(NCES_Reading4, 53:104)
names(NCES_Reading4_2013) <- c("Year", "StateName", "Reading4_2013")
NCES_Reading4_2013 <- dplyr::select(NCES_Reading4_2013, "StateName", "Reading4_2013")
#2011
NCES_Reading4_2011 <- dplyr::slice(NCES_Reading4, 105:156)
names(NCES_Reading4_2011) <- c("Year", "StateName", "Reading4_2011")
NCES_Reading4_2011 <- dplyr::select(NCES_Reading4_2011, "StateName", "Reading4_2011")
#2009
NCES_Reading4_2009 <- dplyr::slice(NCES_Reading4, 157:208)
names(NCES_Reading4_2009) <- c("Year", "StateName", "Reading4_2009")
NCES_Reading4_2009 <- dplyr::select(NCES_Reading4_2009, "StateName", "Reading4_2009")
#2007
NCES_Reading4_2007 <- dplyr::slice(NCES_Reading4, 209:260)
names(NCES_Reading4_2007) <- c("Year", "StateName", "Reading4_2007")
NCES_Reading4_2007 <- dplyr::select(NCES_Reading4_2007, "StateName", "Reading4_2007")
#2005
NCES_Reading4_2005 <- dplyr::slice(NCES_Reading4, 261:312)
names(NCES_Reading4_2005) <- c("Year", "StateName", "Reading4_2005")
NCES_Reading4_2005 <- dplyr::select(NCES_Reading4_2005, "StateName", "Reading4_2005")

#Merging
Merge1 <- dplyr::left_join(NCES_Reading4_2005, NCES_Reading4_2007, "StateName")
Merge2 <- dplyr::left_join(Merge1, NCES_Reading4_2009, "StateName")
Merge3 <- dplyr::left_join(Merge2, NCES_Reading4_2011, "StateName")
Merge4 <- dplyr::left_join(Merge3, NCES_Reading4_2013, "StateName")
Reading4 <- dplyr::left_join(Merge4, NCES_Reading4_2015, "StateName")
Reading4 <- dplyr::filter(Reading4, Reading4$StateName != "District of Columbia")
Reading4 <- dplyr::filter(Reading4, Reading4$StateName != "DoDEA")

Reading <- dplyr::left_join(Reading4, Reading8, "StateName")

#Math====

#8th grade
#subsetting by year
NCES_Math8 <- read_excel("./RawDataFiles/NCES_Math8_Oct12017.xls", 
                         col_types = c("numeric", "text", "numeric", 
                                       "blank"), skip = 11)
#2015
NCES_Math8_2015 <- dplyr::slice(NCES_Math8, 1:52)
names(NCES_Math8_2015) <- c("Year", "StateName", "Math8_2015")
NCES_Math8_2015 <- dplyr::select(NCES_Math8_2015, "StateName", "Math8_2015")
#2013
NCES_Math8_2013 <- dplyr::slice(NCES_Math8, 53:104)
names(NCES_Math8_2013) <- c("Year", "StateName", "Math8_2013")
NCES_Math8_2013 <- dplyr::select(NCES_Math8_2013, "StateName", "Math8_2013")
#2011
NCES_Math8_2011 <- dplyr::slice(NCES_Math8, 105:156)
names(NCES_Math8_2011) <- c("Year", "StateName", "Math8_2011")
NCES_Math8_2011 <- dplyr::select(NCES_Math8_2011, "StateName", "Math8_2011")
#2009
NCES_Math8_2009 <- dplyr::slice(NCES_Math8, 157:208)
names(NCES_Math8_2009) <- c("Year", "StateName", "Math8_2009")
NCES_Math8_2009 <- dplyr::select(NCES_Math8_2009, "StateName", "Math8_2009")
#2007
NCES_Math8_2007 <- dplyr::slice(NCES_Math8, 209:260)
names(NCES_Math8_2007) <- c("Year", "StateName", "Math8_2007")
NCES_Math8_2007 <- dplyr::select(NCES_Math8_2007, "StateName", "Math8_2007")
#2005
NCES_Math8_2005 <- dplyr::slice(NCES_Math8, 261:312)
names(NCES_Math8_2005) <- c("Year", "StateName", "Math8_2005")
NCES_Math8_2005 <- dplyr::select(NCES_Math8_2005, "StateName", "Math8_2005")

#Merging
Merge1 <- dplyr::left_join(NCES_Math8_2005, NCES_Math8_2007, "StateName")
Merge2 <- dplyr::left_join(Merge1, NCES_Math8_2009, "StateName")
Merge3 <- dplyr::left_join(Merge2, NCES_Math8_2011, "StateName")
Merge4 <- dplyr::left_join(Merge3, NCES_Math8_2013, "StateName")
Math8 <- dplyr::left_join(Merge4, NCES_Math8_2015, "StateName")
Math8 <- dplyr::filter(Math8, Math8$StateName != "District of Columbia")
Math8 <- dplyr::filter(Math8, Math8$StateName != "DoDEA")


#4th grade
NCES_Math4 <- read_excel("./RawDataFiles/NCES_Math4_Oct12017.xls", 
                         col_types = c("numeric", "text", "numeric", 
                                       "blank"), skip = 11)
#2015
NCES_Math4_2015 <- dplyr::slice(NCES_Math4, 1:52)
names(NCES_Math4_2015) <- c("Year", "StateName", "Math4_2015")
NCES_Math4_2015 <- dplyr::select(NCES_Math4_2015, "StateName", "Math4_2015")
#2013
NCES_Math4_2013 <- dplyr::slice(NCES_Math4, 53:104)
names(NCES_Math4_2013) <- c("Year", "StateName", "Math4_2013")
NCES_Math4_2013 <- dplyr::select(NCES_Math4_2013, "StateName", "Math4_2013")
#2011
NCES_Math4_2011 <- dplyr::slice(NCES_Math4, 105:156)
names(NCES_Math4_2011) <- c("Year", "StateName", "Math4_2011")
NCES_Math4_2011 <- dplyr::select(NCES_Math4_2011, "StateName", "Math4_2011")
#2009
NCES_Math4_2009 <- dplyr::slice(NCES_Math4, 157:208)
names(NCES_Math4_2009) <- c("Year", "StateName", "Math4_2009")
NCES_Math4_2009 <- dplyr::select(NCES_Math4_2009, "StateName", "Math4_2009")
#2007
NCES_Math4_2007 <- dplyr::slice(NCES_Math4, 209:260)
names(NCES_Math4_2007) <- c("Year", "StateName", "Math4_2007")
NCES_Math4_2007 <- dplyr::select(NCES_Math4_2007, "StateName", "Math4_2007")
#2005
NCES_Math4_2005 <- dplyr::slice(NCES_Math4, 261:312)
names(NCES_Math4_2005) <- c("Year", "StateName", "Math4_2005")
NCES_Math4_2005 <- dplyr::select(NCES_Math4_2005, "StateName", "Math4_2005")

#Merging
Merge1 <- dplyr::left_join(NCES_Math4_2005, NCES_Math4_2007, "StateName")
Merge2 <- dplyr::left_join(Merge1, NCES_Math4_2009, "StateName")
Merge3 <- dplyr::left_join(Merge2, NCES_Math4_2011, "StateName")
Merge4 <- dplyr::left_join(Merge3, NCES_Math4_2013, "StateName")
Math4 <- dplyr::left_join(Merge4, NCES_Math4_2015, "StateName")
Math4 <- dplyr::filter(Math4, Math4$StateName != "District of Columbia")
Math4 <- dplyr::filter(Math4, Math4$StateName != "DoDEA")

Math <- dplyr::left_join(Math4, Math8, "StateName")


#Making complete dataset 
Merge1 <- dplyr::left_join(Vocab, Science, "StateName")
Merge2 <- dplyr::left_join(Merge1, Reading, "StateName")
EducationalAchievement <- dplyr::left_join(Merge2, Math, "StateName")

#Exporting
write.csv(EducationalAchievement, "EducationalAchievement_NAEP2005-2015.csv", row.names = FALSE)

#Creating a new dataset for merging
EducationalAchievement_simple <- EducationalAchievement
Vocab4_columns <- dplyr::select(EducationalAchievement_simple, starts_with("Vocab4"))
EducationalAchievement_simple$Vocab4 <- rowMeans(Vocab4_columns) 
Vocab8_columns <- dplyr::select(EducationalAchievement_simple, starts_with("Vocab8"))
EducationalAchievement_simple$Vocab8 <- rowMeans(Vocab8_columns)
Science4_columns <- dplyr::select(EducationalAchievement_simple, starts_with("Science4"))
EducationalAchievement_simple$Science4 <- rowMeans(Science4_columns) 
Science8_columns <- dplyr::select(EducationalAchievement_simple, starts_with("Science8"))
EducationalAchievement_simple$Science8 <- rowMeans(Science8_columns)
Reading4_columns <- dplyr::select(EducationalAchievement_simple, starts_with("Reading4"))
EducationalAchievement_simple$Reading4 <- rowMeans(Reading4_columns) 
Reading8_columns <- dplyr::select(EducationalAchievement_simple, starts_with("Reading8"))
EducationalAchievement_simple$Reading8 <- rowMeans(Reading8_columns)
Math4_columns <- dplyr::select(EducationalAchievement_simple, starts_with("Math4"))
EducationalAchievement_simple$Math4 <- rowMeans(Math4_columns) 
Math8_columns <- dplyr::select(EducationalAchievement_simple, starts_with("Math8"))
EducationalAchievement_simple$Math8 <- rowMeans(Math8_columns)

#Summary scores of just math and reading for 4th and 8th (b/c complete data for all states)
Achievement8_columns <- dplyr::select(EducationalAchievement_simple, starts_with("Math8_"), starts_with("Reading8_"))
EducationalAchievement_simple$Achievement8 <- rowMeans(Achievement8_columns)
Achievement4_columns <- dplyr::select(EducationalAchievement_simple, starts_with("Math4_"), starts_with("Reading4_"))
EducationalAchievement_simple$Achievement4 <- rowMeans(Achievement4_columns)

#Preping final dataset
EducationalAchievement_simple <- dplyr::select(EducationalAchievement_simple, StateName, Vocab4, Vocab8, Science4, Science8, Reading4, Reading8, Math4, Math8, Achievement4, Achievement8)

#Calculating reliability for attainment variables
Vocab4_alpha <- psych::alpha(as.data.frame(Vocab4_columns))
Vocab8_alpha <- psych::alpha(as.data.frame(Vocab8_columns))
Science4_alpha <- psych::alpha(as.data.frame(Science4_columns))
Science8_alpha <- psych::alpha(as.data.frame(Science8_columns))
Reading4_alpha <- psych::alpha(as.data.frame(Reading4_columns))
Reading8_alpha <- psych::alpha(as.data.frame(Reading8_columns))
Math4_alpha <- psych::alpha(as.data.frame(Math4_columns))
Math8_alpha <- psych::alpha(as.data.frame(Math8_columns))
Achievement4_alpha <- psych::alpha(as.data.frame(Achievement4_columns))
Achievement8_alpha <- psych::alpha(as.data.frame(Achievement8_columns))


########Google Correlate------------
SearchFrequencies <- read_csv("./RawDataFiles/GoogleCorrelateFrequencies_Feb2016.csv")

#making frequencies numeric
SearchFrequencies[, c(2:30)] <- sapply(SearchFrequencies[, c(2:30)], as.numeric)

#Exporting
write.csv(SearchFrequencies, "GoogleSearchFrequencies_GoogleCorrelate2016.csv", row.names = FALSE)


#Creating a new dataset for merging
SearchFrequencies_simple <- SearchFrequencies
Shortcut_columns <- dplyr::select(SearchFrequencies_simple,2:14)
SearchFrequencies_simple$Shortcut <- rowMeans(Shortcut_columns)
Learn_columns <- dplyr::select(SearchFrequencies_simple, 15:27)
SearchFrequencies_simple$Learn <- rowMeans(Learn_columns)

SearchFrequencies_simple <- dplyr::select(SearchFrequencies_simple, StateName, Shortcut, Learn)


#Calculating reliability for attainment variables
Shortcut_alpha <- psych::alpha(as.data.frame(Shortcut_columns))
Learn_alpha <- psych::alpha(as.data.frame(Learn_columns))



#Reliability table====

Reliability <- data.frame(Variable = c("Per Capita Income", "Gini", "Shortcut searches", "Learning searches","Before 9th dropout", "HS dropout", "HS grad or more", "BA or more", "Grad Deg", "Vocab 4th", "Vocab 8th", "Science 4th", "Science 8th", "Reading 4th", "Reading 8th", "Math 4th", "Math 8th", "Math+Reading 4th", "Math+Reading 8th"),
                    Years = c("2005-2015", "2006-2015", "2016", "2016","2005-2015","2005-2015","2005-2015","2005-2015","2005-2015", "2009, 2011, 2015", "2009, 2011, 2013", "2009, 2015", "2009, 2011, 2015", "2005 - 2015, odd", "2005 - 2015, odd", "2005 - 2015, odd", "2005 - 2015, odd", "multiple", "multiple"),
                    Mean = I(c(Percapi_alpha$total$mean, Gini_alpha$total$mean, Shortcut_alpha$total$mean, Learn_alpha$total$mean, Less9th_alpha$total$mean, HSnoDiploma_alpha$total$mean, Hsgradormore_alpha$total$mean, BachDegormore_alpha$total$mean, GradDeg_alpha$total$mean, Vocab4_alpha$total$mean, Vocab8_alpha$total$mean, Science4_alpha$total$mean, Science8_alpha$total$mean, Reading4_alpha$total$mean, Reading8_alpha$total$mean, Math4_alpha$total$mean, Math8_alpha$total$mean, Achievement4_alpha$total$mean, Achievement8_alpha$total$mean)),
                    SD = I(c(Percapi_alpha$total$sd, Gini_alpha$total$sd, Shortcut_alpha$total$sd, Learn_alpha$total$sd, Less9th_alpha$total$sd, HSnoDiploma_alpha$total$sd, Hsgradormore_alpha$total$sd, BachDegormore_alpha$total$sd, GradDeg_alpha$total$sd, Vocab4_alpha$total$sd, Vocab8_alpha$total$sd, Science4_alpha$total$sd, Science8_alpha$total$sd, Reading4_alpha$total$sd, Reading8_alpha$total$sd, Math4_alpha$total$sd, Math8_alpha$total$sd, Achievement4_alpha$total$sd, Achievement8_alpha$total$sd)),
                    CronbachsA = I(c(Percapi_alpha$total$raw_alpha, Gini_alpha$total$raw_alpha, Shortcut_alpha$total$raw_alpha, Learn_alpha$total$raw_alpha, Less9th_alpha$total$raw_alpha, HSnoDiploma_alpha$total$raw_alpha, Hsgradormore_alpha$total$raw_alpha, BachDegormore_alpha$total$raw_alpha, GradDeg_alpha$total$raw_alpha, Vocab4_alpha$total$raw_alpha, Vocab8_alpha$total$raw_alpha, Science4_alpha$total$raw_alpha, Science8_alpha$total$raw_alpha, Reading4_alpha$total$raw_alpha, Reading8_alpha$total$raw_alpha, Math4_alpha$total$raw_alpha, Math8_alpha$total$raw_alpha, Achievement4_alpha$total$raw_alpha, Achievement8_alpha$total$raw_alpha)))

Colnames(Reliability)[5] <- paste(Reliability[,5], collapse="")

#exporting
write.csv(Reliability, "DescriptiveStatistics.csv", row.names = FALSE)

#Creating final dataset====

Merge1 <- dplyr::left_join(Gini_simple, Percapi_simple, "StateName")
Merge2 <- dplyr::left_join(Merge1, SearchFrequencies_simple, "StateName")
Merge3 <- dplyr::left_join(Merge2, EducationalAttainment_simple, "StateName")
ShortcutAnalysisData <- dplyr::left_join(Merge3, EducationalAchievement_simple, "StateName")
#Adding Median Income and Percent Students in Poverty
ShortcutAnalysisData$MedIncome <- Income_simple$MedIncome
ShortcutAnalysisData$PercentStuPoverty <- Income_simple$PercentStuPoverty

#Adding Wealth Ratios
###Data from Keith (not sure where or what years)

WealthRatios<- read_csv("RawDataFiles/Ratios 90_50_10.csv")
WealthRatios <- dplyr:: select(WealthRatios, StateName, q90_q50, q50_q10)

ShortcutAnalysisData <- dplyr::left_join(ShortcutAnalysisData, WealthRatios, "StateName")

#exporting
write.csv(ShortcutAnalysisData, "ShortcutAnalysisData.csv", row.names = FALSE)

#making a dataset with all years and all data
Merge1 <- dplyr:: left_join(EducationalAchievement, EducationalAttainment, "StateName")
Merge2 <- dplyr:: left_join(Merge1, Gini, "StateName")
Merge3 <- dplyr:: left_join(Merge2, Income, "StateName")
Merge4 <- dplyr:: left_join(Merge3, SearchFrequencies_simple, "StateName")
Merge5 <- dplyr:: left_join(Merge4, SearchFrequencies, "StateName")
Merge6 <- dplyr:: left_join(Merge5, WealthRatios, "StateName")
CompleteData_Years <- Merge6

#exporting
write.csv(CompleteData_Years, "CompleteData_Years.csv", row.names = FALSE)

#calcualting residuals for upload to Google Correlate
ShortcutAnalysisData <- read_csv("ShortcutAnalysisData.csv")
giniincome_lm <- lm(Gini ~ Percapi, data = ShortcutAnalysisData)
ShortcutAnalysisData$GiniPercapiResiduals <- residuals(giniincome_lm)

#exporing 
GiniPercapiResiduals_forgoogle <- ShortcutAnalysisData %>% select(StateName, GiniPercapiResiduals)
write.csv(GiniPercapiResiduals_forgoogle, "GiniPercapiResiduals_forgoogle.csv", row.names = FALSE)

