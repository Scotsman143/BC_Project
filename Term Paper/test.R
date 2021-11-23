library(plyr)
library(reshape2)
library(ggplot2)
#library(Synth)
#library(gsynth)
#library(panelView)
library(glmnet)
#library(quadprog)


##########################################################################################
# Set working directory and import data
##########################################################################################

# set working directory
datadir <- "C:/Users/lukea/Desktop/GMU/Fall 2021/Economics of Regulation/Term Paper/data/use_data"
setwd(datadir)

##########################################################################################
# Unemployment
##########################################################################################

# bring in unemployment data
Unemploy.Data <- read.csv("new_unemploy.csv", stringsAsFactors=F)

# create province name variable for 10 provinces and 3 territories (59 is BC)
Unemploy.Data$prov.abbrev <- "BC"
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 10, "NL", Unemploy.Data$prov.abbrev)
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 11, "PE", Unemploy.Data$prov.abbrev)
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 12, "NS", Unemploy.Data$prov.abbrev)
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 13, "NB", Unemploy.Data$prov.abbrev)
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 24, "QC", Unemploy.Data$prov.abbrev)
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 35, "ON", Unemploy.Data$prov.abbrev)
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 46, "MB", Unemploy.Data$prov.abbrev)
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 47, "SK", Unemploy.Data$prov.abbrev)
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 48, "AB", Unemploy.Data$prov.abbrev)
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 59, "BC", Unemploy.Data$prov.abbrev)
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 60, "YT", Unemploy.Data$prov.abbrev)
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 61, "NT", Unemploy.Data$prov.abbrev)
Unemploy.Data$prov.abbrev <- ifelse(Unemploy.Data$geo_code == 62, "NU", Unemploy.Data$prov.abbrev)


# dummmy and interaction variables
Unemploy.Data$treated_year = ifelse(Unemploy.Data$Reference.period >=2001 & Unemploy.Data$Reference.period <=2008, 1, 0)
Unemploy.Data$treated_prov = ifelse(Unemploy.Data$prov.abbrev == "BC", 1, 0)
Unemploy.Data$interaction = Unemploy.Data$treated_year * Unemploy.Data$treated_prov

# did estimator
unemploy_didreg = glm(unemploy_rate ~ treated_prov + treated_year, data = Unemploy.Data)
summary(unemploy_didreg)

unemploy_didreg1 = glm(unemploy_rate ~ treated_prov*treated_year, data = Unemploy.Data)
summary(unemploy_didreg1)

#large province DiD
unemploy_data_2 <- read.csv("new_unemploy_slim.csv", stringsAsFactors=F)
unemploy_data_2$treated_year = ifelse(unemploy_data_2$Reference.period >=2001  & unemploy_data_2$Reference.period <=2008, 1, 0)
unemploy_data_2$treated_prov = ifelse(unemploy_data_2$geo_code == 59, 1, 0)
unemploy_data_2$interaction = unemploy_data_2$treated_year * unemploy_data_2$treated_prov

# large province did estimator
unemploy_didreg_2 = glm(unemploy_rate ~ treated_prov + treated_year, data = unemploy_data_2)
summary(unemploy_didreg_2)

unemploy_didreg1_2 = glm(unemploy_rate ~ treated_prov*treated_year, data = unemploy_data_2)
summary(unemploy_didreg1_2)

##########################################################################################
# Wages
##########################################################################################

# bring in wage data
Wages.Data <- read.csv("wages.csv", stringsAsFactors=F)

# create province name variable for 10 provinces and 3 territories (59 is BC)
Wages.Data$prov.abbrev <- "BC"
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 10, "NL", Wages.Data$prov.abbrev)
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 11, "PE", Wages.Data$prov.abbrev)
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 12, "NS", Wages.Data$prov.abbrev)
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 13, "NB", Wages.Data$prov.abbrev)
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 24, "QC", Wages.Data$prov.abbrev)
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 35, "ON", Wages.Data$prov.abbrev)
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 46, "MB", Wages.Data$prov.abbrev)
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 47, "SK", Wages.Data$prov.abbrev)
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 48, "AB", Wages.Data$prov.abbrev)
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 59, "BC", Wages.Data$prov.abbrev)
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 60, "YT", Wages.Data$prov.abbrev)
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 61, "NT", Wages.Data$prov.abbrev)
Wages.Data$prov.abbrev <- ifelse(Wages.Data$geo_code == 62, "NU", Wages.Data$prov.abbrev)

# dummmy and interaction variables
Wages.Data$treated_year = ifelse(Wages.Data$Reference.period >= 2001 & Wages.Data$Reference.period <=2008, 1, 0)
Wages.Data$treated_prov = ifelse(Wages.Data$prov.abbrev == "BC", 1, 0)
Wages.Data$interaction = Wages.Data$treated_year * Wages.Data$treated_prov

# did estimator
wage_didreg = glm(avg_hr_wage ~ treated_prov + treated_year, data = Wages.Data)
summary(wage_didreg)

wage_didreg1 = glm(avg_hr_wage ~ treated_prov*treated_year, data = Wages.Data)
summary(wage_didreg1)

