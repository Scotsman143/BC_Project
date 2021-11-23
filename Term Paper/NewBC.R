# Topic: Regulatory Accumulation and Poverty/Income Inequality
# Created by: Luke Ashton
# Created on: 11/14/2021
# Version 1

pacman::p_load(tidyverse, regcensusAPI, hrbrthemes, plyr)

prov_list <- c("Alberta", "British Columbia", "Manitoba", "New Brunswick",
               "Newfoundland and Labrador", "Nova Scotia", "Ontario", "Prince Edward Island",
               "Quebec", "Saskatchewan")

#data paths
dir.create("C:/Users/lukea/Desktop/GMU/Fall 2021/Economics of Regulation/Term Paper/use_data")
data_loc <- ("C:/Users/lukea/Desktop/GMU/Fall 2021/Economics of Regulation/Term Paper/use_data")
data_out <- ("C:/Users/lukea/Desktop/GMU/Fall 2021/Economics of Regulation/Term Paper/data_output")

#bring in RegData summary
prov_list_ids <- get_jurisdictions() %>%
  filter(jurisdictionName %in% prov_list) %>%
  select(jurisdictionID)

series.list <- get_series()
agency.list <- get_agencies()
topics.list <- get_topics(topic=NA)
jurisdiction.list <- get_jurisdictions() %>%
  filter(country == "Canada")
document_list <- get_document_types()

#regulatory restriction summary

rgdata.summ <- get_values(date_is_range = TRUE, documentType = 1, filtered = TRUE, 
                          jurisdiction = 33, series = 1, time = 2015)

#bring industry data
rgdata.industries.list <- get_industries(33) %>%
  filter(nchar(industryCode) == 3) %>%
  select(industry_code = industryCode,
         industry_name = industryName)


prov.reg.industry <- get_values(jurisdiction = prov_list, industry = rgdata.industries.list$industry_code,
                                series = 1, time = 2020, documentType = 2)
  
  left_join(rgdata.industries.list, by = c("industry_code" = "industry_code"))


