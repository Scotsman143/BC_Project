library(plyr)
library(reshape2)
library(ggplot2)
#library(Synth)
#library(gsynth)
#library(panelView)
#library(glmnet)
#library(quadprog)


##########################################################################################
# Set working directory and import data
##########################################################################################

# set working directory
datadir <- "C:/Users/bentley.coffey/Dropbox/Documents/.R/Regs/BCRegsExp"
setwd(datadir)

# bring in all available data
All.Provinces.Data <- read.csv("gdp_province_FullData.csv", stringsAsFactors=F)
Provinces.Annual.Population <- read.csv("pop_province.csv", stringsAsFactors=F)
Reg.Data <- read.csv("RegData2-4DigitNACIS4BCnON.csv", stringsAsFactors=F)
Reg.Data.Quality.Meta.Data <- read.csv("quantgov_regcensus_filtering.csv", stringsAsFactors=F)



##########################################################################################
# merge and prepare GDP Data
##########################################################################################

# merge in population data
All.Provinces.Data <- merge(All.Provinces.Data,Provinces.Annual.Population)

# create province name variable for 10 provinces and 3 territories (59 is BC)
All.Provinces.Data$prov.abbrev <- "BC"
All.Provinces.Data$prov.abbrev <- ifelse(All.Provinces.Data$prov == 10, "NL", All.Provinces.Data$prov.abbrev)
All.Provinces.Data$prov.abbrev <- ifelse(All.Provinces.Data$prov == 11, "PE", All.Provinces.Data$prov.abbrev)
All.Provinces.Data$prov.abbrev <- ifelse(All.Provinces.Data$prov == 12, "NS", All.Provinces.Data$prov.abbrev)
All.Provinces.Data$prov.abbrev <- ifelse(All.Provinces.Data$prov == 13, "NB", All.Provinces.Data$prov.abbrev)
All.Provinces.Data$prov.abbrev <- ifelse(All.Provinces.Data$prov == 24, "QC", All.Provinces.Data$prov.abbrev)
All.Provinces.Data$prov.abbrev <- ifelse(All.Provinces.Data$prov == 35, "ON", All.Provinces.Data$prov.abbrev)
All.Provinces.Data$prov.abbrev <- ifelse(All.Provinces.Data$prov == 46, "MB", All.Provinces.Data$prov.abbrev)
All.Provinces.Data$prov.abbrev <- ifelse(All.Provinces.Data$prov == 47, "SK", All.Provinces.Data$prov.abbrev)
All.Provinces.Data$prov.abbrev<- ifelse(All.Provinces.Data$prov == 48, "AB", All.Provinces.Data$prov.abbrev)
All.Provinces.Data$prov.abbrev <- ifelse(All.Provinces.Data$prov == 59, "BC", All.Provinces.Data$prov.abbrev)
All.Provinces.Data$prov.abbrev <- ifelse(All.Provinces.Data$prov == 60, "YT", All.Provinces.Data$prov.abbrev)
All.Provinces.Data$prov.abbrev <- ifelse(All.Provinces.Data$prov == 61, "NT", All.Provinces.Data$prov.abbrev)
All.Provinces.Data$prov.abbrev <- ifelse(All.Provinces.Data$prov == 62, "NU", All.Provinces.Data$prov.abbrev)

# restrict data to provinces (not territories) and 1997 to 2014
All.Provinces.Data <- subset(All.Provinces.Data, (year < 2015) & (prov.abbrev %in% c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC")))

# create variables for gdp per capita and investment relative to GDP
All.Provinces.Data$gdp.per.capita <- All.Provinces.Data$gdp / (All.Provinces.Data$population / 10^6)

# Compute logs and Hyperbolic Inverse Sine (HIS)
All.Provinces.Data$log.gdp <- log(All.Provinces.Data$gdp)
All.Provinces.Data$log.gdp.per.capita <- log(All.Provinces.Data$gdp.per.capita)
All.Provinces.Data$HIS.gdp <- log(All.Provinces.Data$gdp + sqrt(1 + All.Provinces.Data$gdp^2))
All.Provinces.Data$HIS.gdp.per.capita <- log(All.Provinces.Data$gdp.per.capita + sqrt(1 + All.Provinces.Data$gdp.per.capita^2))



##########################################################################################
# prepare Regulation Data and merge it in
##########################################################################################

# merge in metadata
Merged.Reg.Data <- merge(Reg.Data,Reg.Data.Quality.Meta.Data,by="cansim_code")

# check the filter criteria
#Merged.Reg.Data.2000 <- subset(Merged.Reg.Data, (year == 2007) & (prov.abbrev == "BC"))
#write.csv(file="MergedData.csv",Merged.Reg.Data.2000)
#ggplot(data=Merged.Reg.Data) + geom_point(aes(x=f1_normalized_std,y=f1_normalized,color=as.factor(Filter_out)))

# remove the industries flagged for inadequate data quality
Merged.Reg.Data <- subset(Merged.Reg.Data,Filter_out == 0)

# merge with GDP data for all industries
BC.n.ON.Data <- merge(All.Provinces.Data,Merged.Reg.Data)

# create log of restrictions per capita
BC.n.ON.Data$log.restrictions <- log(BC.n.ON.Data$restrictions)
BC.n.ON.Data$HIS.restrictions <- log(BC.n.ON.Data$restrictions + sqrt(1 + BC.n.ON.Data$restrictions^2))
BC.n.ON.Data$restrictions.per.capita <- BC.n.ON.Data$restrictions / (BC.n.ON.Data$population / 10^6)
BC.n.ON.Data$log.restrictions.per.capita <- log(BC.n.ON.Data$restrictions.per.capita)
BC.n.ON.Data$HIS.restrictions.per.capita <- log(BC.n.ON.Data$restrictions.per.capita + sqrt(1 + BC.n.ON.Data$restrictions.per.capita^2))


##########################################################################################
# Create Leads, Lags, and Growth Rates
##########################################################################################

# Create the leads, lags, and growth rates
BC.n.ON.Data <- ddply(BC.n.ON.Data,.(prov,cansim_code),function(Data.Subset) {
  
  # sort data by year
  Data.Subset <- Data.Subset[order(Data.Subset$year),]
  
  #print(unique(Data.Subset$prov.abbrev))
  #print(unique(Data.Subset$cansim_code))
  
  # lead value
  Data.Subset$Lead.year <- c(Data.Subset$year[2:nrow(Data.Subset)],NA)
  Data.Subset$Lead.log.restrictions <- c(Data.Subset$log.restrictions[2:nrow(Data.Subset)],NA)
  Data.Subset$Lead.HIS.restrictions <- c(Data.Subset$HIS.restrictions[2:nrow(Data.Subset)],NA)
  Data.Subset$Lead.log.restrictions.per.capita <- c(Data.Subset$log.restrictions.per.capita[2:nrow(Data.Subset)],NA)
  Data.Subset$Lead.HIS.restrictions.per.capita <- c(Data.Subset$HIS.restrictions.per.capita[2:nrow(Data.Subset)],NA)
  Data.Subset$Lead.log.gdp <- c(Data.Subset$log.gdp[2:nrow(Data.Subset)],NA)
  Data.Subset$Lead.log.gdp.per.capita <- c(Data.Subset$log.gdp.per.capita[2:nrow(Data.Subset)],NA)
  Data.Subset$Lead.HIS.gdp <- c(Data.Subset$HIS.gdp[2:nrow(Data.Subset)],NA)
  Data.Subset$Lead.HIS.gdp.per.capita <- c(Data.Subset$HIS.gdp.per.capita[2:nrow(Data.Subset)],NA)
  
  # compute growth rate
  Data.Subset$log.restrictions.1st.Diff <- (Data.Subset$Lead.log.restrictions - Data.Subset$log.restrictions)/(Data.Subset$Lead.year - Data.Subset$year)
  Data.Subset$HIS.restrictions.1st.Diff <- (Data.Subset$Lead.HIS.restrictions - Data.Subset$HIS.restrictions)/(Data.Subset$Lead.year - Data.Subset$year)
  Data.Subset$log.restrictions.per.capita.1st.Diff <- (Data.Subset$Lead.log.restrictions.per.capita - Data.Subset$log.restrictions.per.capita)/(Data.Subset$Lead.year - Data.Subset$year)
  Data.Subset$HIS.restrictions.per.capita.1st.Diff <- (Data.Subset$Lead.HIS.restrictions.per.capita - Data.Subset$HIS.restrictions.per.capita)/(Data.Subset$Lead.year - Data.Subset$year)
  Data.Subset$log.gdp.1st.Diff <- (Data.Subset$Lead.log.gdp - Data.Subset$log.gdp)/(Data.Subset$Lead.year - Data.Subset$year)
  Data.Subset$log.gdp.per.capita.1st.Diff <- (Data.Subset$Lead.log.gdp.per.capita - Data.Subset$log.gdp.per.capita)/(Data.Subset$Lead.year - Data.Subset$year)
  Data.Subset$HIS.gdp.1st.Diff <- (Data.Subset$Lead.HIS.gdp - Data.Subset$HIS.gdp)/(Data.Subset$Lead.year - Data.Subset$year)
  Data.Subset$HIS.gdp.per.capita.1st.Diff <- (Data.Subset$Lead.HIS.gdp.per.capita - Data.Subset$HIS.gdp.per.capita)/(Data.Subset$Lead.year - Data.Subset$year)
  
  return(Data.Subset)
})

# get rid of logs of 0 and year 2015 (which has no growth rate due to no 2016 data)
BC.n.ON.Data <- subset(BC.n.ON.Data,is.finite(log.gdp.1st.Diff))
BC.n.ON.Data <- subset(BC.n.ON.Data,is.finite(log.restrictions.per.capita.1st.Diff))
BC.n.ON.Data <- subset(BC.n.ON.Data, year < 2015)



##########################################################################################
# Select Sample of Industries to Analyze
##########################################################################################

Synth.Control.Industries <- c(
                              "1114","111A","1125","112A","113","114","115",
                              "211","21223","21229","21231","21232",
                              "2211","2212","2213",
                              "23A","23B","23C1","23C2","23C3","23C4","23D","23E",
                              "3111","3112","3113","3114","3115","3116","3117","3118","3119",
                              "31211","31212","31A","3211","3212","3219","3221","3222",
                              "323","324","3251","3252","3253","3254","3255","3259",
                              "3261","3262","3273","327A",
                              "3312","3314","3321","3323","3324","3325","3326","3327","3328",
                              "3331","3332","3333","3334","3335","3336","3339",
                              "3341","3342","3344","3351","3352","3353","3359",
                              "3361","3362","33631","33634","33636","33639",
                              "3364","3366","3369","3371","3372","3379","3391","3399",
                              "41","44-45","481","482","483","484","4853",
                              "486A","4862","488","492","493",
                              "511","51213","5121A","5122","5151","517","519",
                              "52213","5221A","5241","5242","52A","5311","531A","5321","532A",
                              "5413","5415","5418","541A","541B","55","5615","5616","5617",
                              "7132","713A","7211","721A","722",
                              "8111","8122","8123","812A")

Synth.Control.Industries.Unlettered.n.ON.Lassoed <- c(
  "1125","114",
  "21223","21232",
  "2211","2212","2213",
  "3113","3118","3119",
  "3212","3219",
  "324","3251","3252","3259",
  "3262",
  "3321","3323","3324","3325","3328",
  "3331","3332","3334","3335",
  "3341","3351","3352","3353",
  "3361","33634",
  "3366","3372","3379","3399",
  "483",
  "4862","492","493",
  "519",
  "5242",
  "5413","5617")

Synth.Control.Industries.Aggregated.4.Canadian.Lettered.Codes <- c(
  "111","112","113","114","115",
  "211","21223","21229","21231","21232",
  "2211","2212","2213",
  "31","3211","3212","3219","3221","3222",
  "323","324","3251","3252","3253","3254","3255","3259",
  "3261","3262","327",
  "3312","3314","3321","3323","3324","3325","3326","3327","3328",
  "3331","3332","3333","3334","3335","3336","3339",
  "3341","3342","3344","3351","3352","3353","3359",
  "3361","3362","33631","33634","33636","33639",
  "3364","3366","3369","3371","3372","3379","3391","3399",
  "41","44-45","481","482","483","484","4853",
  "486","488","492","493",
  "511","5121","5122","5151","517","519",
  "52","531","532",
  "541","55","5615","5616","5617",
  "713","721","722",
  "8111","812")

Industries.3.Digits <- c(
  "111","112","113","114","115",
  "211","212","213",
  "221",
  "311","312","321","322",
  "323","324","325",
  "326","327",
  "331","332",
  "333",
  "334","335",
  "336",
  "337","339",
  "411","412","413","414","415","416","417","418","419","481","482","483","484",
  "486","488","492","493",
  "511","512","515","517","518","519",
  "524","531","532","533",
  "541","561",
  "713","721","722",
  "811","812")

Industries.3.Digits.Partially.ON.Lassoed <- c(
  "112","114",
  "212",
  "221",
  "311",
  "321",
  "324","325",
  "326",
  "332",
  "333",
  "334","335",
  "336",
  "337","339",
  "483",
  "486","492","493",
  "519",
  "524",
  "541","561")

Industries.3.Digits.Clearly.Treated <- c(
  "115",
  "311","312",
  "324","325",
  "326",
  "331",
  "333",
  "335",
  "336",
  "481","482","483",
  "488","493",
  "517","519",
  "561")

# subset data to the selected industries
#Industries.2.Keep <- Synth.Control.Industries
#Industries.2.Keep <- Synth.Control.Industries.Unlettered.n.ON.Lassoed
#Industries.2.Keep <- Synth.Control.Industries.Aggregated.4.Canadian.Lettered.Codes
#Industries.2.Keep <- Industries.3.Digits
Industries.2.Keep <- Industries.3.Digits.Clearly.Treated
#Industries.2.Keep <- Industries.3.Digits.Partially.ON.Lassoed
BC.n.ON.Subset.4.Anal <- subset(BC.n.ON.Data,cansim_code %in% Industries.2.Keep)

# print the number of industries and observations included
length(unique(BC.n.ON.Subset.4.Anal$cansim_code))
nrow(BC.n.ON.Subset.4.Anal)


##########################################################################################
# Compute BC GDP Shares and set Weights
##########################################################################################

# Compute share of GDP
BC.n.ON.Subset.4.Anal <- ddply(BC.n.ON.Subset.4.Anal,.(year),function(Data.Subset) {
  
  # grab the BC Data to make the weights
  BC.Data.Subset <- subset(Data.Subset,prov.abbrev == "BC")
  
  # find aggregate
  BC.gdp.per.capita.aggregate <- sum(BC.Data.Subset$gdp.per.capita)
  
  # create new variable in this subset equal to weights
  BC.Data.Subset$gdp.Share.Weights <- BC.Data.Subset$gdp.per.capita / BC.gdp.per.capita.aggregate
  
  # eliminate all but the industry code and weights
  BC.Data.Subset <- BC.Data.Subset[,names(BC.Data.Subset) %in% c("gdp.Share.Weights","cansim_code")]
  
  # merge it back into the data set so that all observations (from that year) with that industry code have that weight
  Data.Subset <- merge(Data.Subset,BC.Data.Subset)
  
  return(Data.Subset)
})

# Set Weights
BC.n.ON.Subset.4.Anal$Weighting.Variable <- BC.n.ON.Subset.4.Anal$gdp.Share.Weights
#BC.n.ON.Subset.4.Anal$Weighting.Variable <- BC.n.ON.Subset.4.Anal$gdp.Share.Weights*(BC.n.ON.Subset.4.Anal$f1_normalized - BC.n.ON.Subset.4.Anal$f1_normalized_std - 0.5)


##########################################################################################
# RegData Regressions
##########################################################################################


# create a BC only subset
BC.Subset.4.Anal <- subset(BC.n.ON.Subset.4.Anal, prov.abbrev == "BC")

# BC Basic regression predicting growth rate of GDP per capita from growth in regs
log.gdp.1st.Diff.from.log.regs.diff <- lm(log.gdp.1st.Diff ~ log.restrictions.1st.Diff, data=BC.Subset.4.Anal, weights = Weighting.Variable)
summary(log.gdp.1st.Diff.from.log.regs.diff)

# BC+ON Basic regression predicting growth rate of GDP per capita from growth in regs
log.gdp.1st.Diff.from.log.regs.diff <- lm(log.gdp.1st.Diff ~ log.restrictions.1st.Diff, data=BC.n.ON.Subset.4.Anal, weights = Weighting.Variable)
summary(log.gdp.1st.Diff.from.log.regs.diff)

# BC Basic regression predicting growth rate of GDP per capita from growth in regs with year FEs
log.gdp.1st.Diff.from.log.regs.diff <- lm(log.gdp.1st.Diff ~ log.restrictions.1st.Diff + as.factor(year), data=BC.Subset.4.Anal, weights = Weighting.Variable)
summary(log.gdp.1st.Diff.from.log.regs.diff)

# BC+ON Basic regression predicting growth rate of GDP per capita from growth in regs with year FEs
log.gdp.1st.Diff.from.log.regs.diff <- lm(log.gdp.1st.Diff ~ log.restrictions.1st.Diff + as.factor(year), data=BC.n.ON.Subset.4.Anal, weights = Weighting.Variable)
summary(log.gdp.1st.Diff.from.log.regs.diff)

# BC Basic regression predicting growth rate of GDP per capita from growth in regs with industry FEs
log.gdp.1st.Diff.from.log.regs.diff <- lm(log.gdp.1st.Diff ~ log.restrictions.1st.Diff + as.factor(cansim_code), data=BC.Subset.4.Anal, weights = Weighting.Variable)
summary(log.gdp.1st.Diff.from.log.regs.diff)

# BC+ON Basic regression predicting growth rate of GDP per capita from growth in regs with industry FEs
log.gdp.1st.Diff.from.log.regs.diff <- lm(log.gdp.1st.Diff ~ log.restrictions.1st.Diff + as.factor(cansim_code), data=BC.n.ON.Subset.4.Anal, weights = Weighting.Variable)
summary(log.gdp.1st.Diff.from.log.regs.diff)

# BC Basic regression predicting growth rate of GDP per capita from growth in regs with year and industry FEs
log.gdp.1st.Diff.from.log.regs.diff <- lm(log.gdp.1st.Diff ~ log.restrictions.1st.Diff + as.factor(year)+as.factor(cansim_code), data=BC.Subset.4.Anal, weights = Weighting.Variable)
summary(log.gdp.1st.Diff.from.log.regs.diff)

# BC+ON Basic regression predicting growth rate of GDP per capita from growth in regs with year and industry FEs
log.gdp.1st.Diff.from.log.regs.diff <- lm(log.gdp.1st.Diff ~ log.restrictions.1st.Diff + as.factor(year)+as.factor(cansim_code), data=BC.n.ON.Subset.4.Anal, weights = Weighting.Variable)
summary(log.gdp.1st.Diff.from.log.regs.diff)

# BC+ON Basic regression predicting growth rate of GDP per capita from growth in regs with year by industry FEs
log.gdp.1st.Diff.from.log.regs.diff <- lm(log.gdp.1st.Diff ~ log.restrictions.1st.Diff + as.factor(year)*as.factor(cansim_code), data=BC.n.ON.Subset.4.Anal, weights = Weighting.Variable)
summary(log.gdp.1st.Diff.from.log.regs.diff)


##########################################################################################
# Generate some Plots
##########################################################################################

# Create the industry label for the graph
BC.n.ON.Subset.4.Anal$Industry.Label <- paste(paste(BC.n.ON.Subset.4.Anal$cansim_code,substr(BC.n.ON.Subset.4.Anal$ind_description,1,15),sep=": "),"...",sep="")

# Pooled Scatterplot
ggplot(data=BC.n.ON.Subset.4.Anal,aes(x=log.restrictions.1st.Diff,y=log.gdp.1st.Diff,color=prov.abbrev)) + 
  geom_point(aes(color=prov.abbrev)) +
  geom_smooth(method="lm",aes(color=prov.abbrev)) +
  geom_smooth(method="lm",color="purple")

# Tiled Scatterplot
ggplot(data=BC.n.ON.Subset.4.Anal,aes(x=log.restrictions.1st.Diff,y=log.gdp.1st.Diff,color=prov.abbrev)) + 
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ Industry.Label, scales = "free")

# Tiled Timepath of Regulations
ggplot(data=BC.n.ON.Subset.4.Anal,aes(x=year,y=restrictions,color=prov.abbrev)) + 
  geom_line() + geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  facet_wrap(~ Industry.Label, scales = "free_y", ncol = 3)

write.csv(BC.n.ON.Subset.4.Anal,"Data4Figure6.csv")





