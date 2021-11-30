##########################################################################################
#Author: Luke Ashton
#Github_Tag: Scotsman143
#Date: November 23, 2021
#Last_Updated: November 24, 2021
#Github_Repo: https://github.com/Scotsman143/BC_Project
##########################################################################################
library(plyr)
library(reshape2)
library(glmnet)
library(memisc)
##########################################################################################
# Set working directory and import data
##########################################################################################
datadir <- "C:/directory"
setwd(datadir)
##########################################################################################
# Unemployment
##########################################################################################
# bring in unemployment data
Unemploy.Data <- read.csv("unemploy.csv", stringsAsFactors=F)

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
Unemploy.Data$year <- factor(Unemploy.Data$Reference.period)

# did estimator
unemploy_didreg = lm(unemploy_rate ~ treated_prov + treated_year, data = Unemploy.Data)

# did with no fixed effect
unemploy_didreg1 = lm(unemploy_rate ~ treated_prov*treated_year, data = Unemploy.Data)

# did w/prov fixed effect
unemploy_didreg_prov = lm(unemploy_rate ~ treated_prov*treated_year + prov.abbrev, data = Unemploy.Data)

# did w/year fixed effect
unemploy_didreg_year = lm(unemploy_rate ~ treated_prov*treated_year + year, data = Unemploy.Data)

# did w/industry fixed effect
unemploy_didreg_industry = lm(unemploy_rate ~ treated_prov*treated_year + industry, data = Unemploy.Data)

# did w/industry-by-year fixed effect
unemploy_didreg_industry_year = lm(unemploy_rate ~ treated_prov*treated_year + industry*year, data = Unemploy.Data)

summary(unemploy_didreg_industry_year)

# did w/year, industry, prov fixed effects
unemploy_didreg_all = lm(unemploy_rate ~ treated_prov*treated_year + prov.abbrev + year + industry, data = Unemploy.Data)

# large province DiD
#unemploy_data_2 <- read.csv("unemploy_slim.csv", stringsAsFactors=F)
#unemploy_data_2$treated_year = ifelse(unemploy_data_2$Reference.period >=2001 & unemploy_data_2$Reference.period <=2008, 1, 0)
#unemploy_data_2$treated_prov = ifelse(unemploy_data_2$geo_code == 59, 1, 0)
#unemploy_data_2$interaction = unemploy_data_2$treated_year * unemploy_data_2$treated_prov

# large province did estimator
#unemploy_didreg_2 = glm(unemploy_rate ~ treated_prov + treated_year, data = unemploy_data_2)
#summ(unemploy_didreg_2)

#unemploy_didreg1_2 = glm(unemploy_rate ~ treated_prov*treated_year, data = unemploy_data_2)
#summ(unemploy_didreg1_2)

##########################################################################################
# Collect and export results
##########################################################################################
unemploy_latex <- ("C:/directory/unemploy_latex.tex")

unemploy_models <- mtable("Model 1" = unemploy_didreg1, "Model 2" = unemploy_didreg_prov,  "Model 3" = unemploy_didreg_year, "Model 4" = unemploy_didreg_industry, "Model 5" = unemploy_didreg_all)

write.mtable(unemploy_models, forLaTeX=TRUE, unemploy_latex)

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
wage_didreg = lm(avg_hr_wage ~ treated_prov + treated_year, data = Wages.Data)

# did with no fixed effect
wage_didreg1 = lm(avg_hr_wage ~ treated_prov*treated_year, data = Wages.Data)

# did w/prov fixed effect
wage_didreg_prov = lm(avg_hr_wage ~ treated_prov*treated_year + as.factor(prov.abbrev), data = Wages.Data)

# did w/year fixed effect
wage_didreg_year = lm(avg_hr_wage ~ treated_prov*treated_year + as.factor(year), data = Wages.Data)

# did w/industry fixed effect
wage_didreg_industry = lm(avg_hr_wage ~ treated_prov*treated_year + industry, data = Wages.Data)

# did w/industry-by-year fixed effect
wage_didreg_industry_year = lm(avg_hr_wage ~ treated_prov*treated_year + industry*year, data = Wages.Data)

# did w/year, industry, prov fixed effects
wage_didreg_all = lm(avg_hr_wage ~ treated_prov*treated_year + industry + prov.abbrev + year, data = Wages.Data)

##########################################################################################
# Collect and export results
##########################################################################################
wages_latex <- ("C:/directory/wages_latex.tex")

wages_models <- mtable("Model 1" = wage_didreg1, "Model 2" = wage_didreg_prov,  "Model 3" = wage_didreg_year, "Model 4" = wage_didreg_industry, "Model 5" = wage_didreg_all)

write.mtable(wages_models, forLaTeX=TRUE, file=wages_latex)