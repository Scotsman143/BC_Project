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
datadir <- "C:/Users/bentley.coffey/Dropbox/Documents/.R/Regs/BCRegsExp"
setwd(datadir)

# bring in all available data
All.Provinces.Data <- read.csv("gdp_province_FullData.csv", stringsAsFactors=F)
Provinces.Annual.Population <- read.csv("pop_province.csv", stringsAsFactors=F)


##########################################################################################
# Prepare Data for new analysis
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

# identify any industry for which BC has 0 GDP for a year to construct a kist without it delete it
#Zero.BC.Data <- subset(All.Provinces.Data, ((prov.abbrev == "BC") & (gdp == 0)))

# remove industries with almost all 0s (many industries get populated from 2007 onwards) for BC
# or industries that appear to be an aggregate containing other observations already included here 
# or 0s for gdp for some year for 6 or more non-BC provinces (hence no synthetic can be constructed to perfectly fit pre-period)
# 2121, 21221, 21222, 3256, 3313, 33632, 33635
#
# also removed because it has a hard 0 (for BC):
# 3256 (Soap, Cleaning compound, and Toilet Preparation Manufacturing) 
# 3315 (Foundaries)
# 3365 (Railroad Rolling Stock Manufacturing)
#
# also removed because not a private sector (for-profit) industry: 
# 2213	(Water, sewage, and other systems)
# 4851 (urban transit systems)
# 491 (postal service) 
# 521 (monetary authorities - central bank), 5311A (imputed rent for home owners)
# 562 (waste management and remediation services)
# 6111, 6112, 6113, 611A, 611B (Educational Services)
# 6211, 6212, 621A, 622, 623, 624 (Healthcare and Social Assistance)
# 8131, 813A (Religious organizations and grant-making civic organizations)
# 9111, 911A, 912, 913 (Government)

Industries.2.Keep <- c("1114","111A","1125","112A","113","114","115",
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
All.Provinces.Data <- subset(All.Provinces.Data,cansim_code %in% Industries.2.Keep)

# create variables for gdp per capita and investment relative to GDP
All.Provinces.Data$gdp.per.capita <- All.Provinces.Data$gdp / (All.Provinces.Data$population / 10^6)

# Replace logs with Hyperbolic Inverse Sine (HIS)
All.Provinces.Data$log.gdp <- log(All.Provinces.Data$gdp)
All.Provinces.Data$log.gdp.per.capita <- log(All.Provinces.Data$gdp.per.capita)
All.Provinces.Data$HIS.gdp <- log(All.Provinces.Data$gdp + sqrt(1 + All.Provinces.Data$gdp^2))
All.Provinces.Data$HIS.gdp.per.capita <- log(All.Provinces.Data$gdp.per.capita + sqrt(1 + All.Provinces.Data$gdp.per.capita^2))

# Create the diff-n-diff variables
All.Provinces.Data$Post.2001 <- as.numeric(All.Provinces.Data$year >= 2001)
All.Provinces.Data$BC <- as.numeric(All.Provinces.Data$prov == 59)
All.Provinces.Data$BC.times.Post.2001 <- All.Provinces.Data$BC * All.Provinces.Data$Post.2001

# Create the leads, lags, and growth rates
All.Provinces.Data <- ddply(All.Provinces.Data,.(prov,cansim_code),function(Data.Subset) {
  
  # sort data by year
  Data.Subset <- Data.Subset[order(Data.Subset$year),]
  
  # lead value
  Data.Subset$Lead.log.gdp <- c(Data.Subset$log.gdp[2:nrow(Data.Subset)],NA)
  Data.Subset$Lead.log.gdp.per.capita <- c(Data.Subset$log.gdp.per.capita[2:nrow(Data.Subset)],NA)
  Data.Subset$Lead.HIS.gdp <- c(Data.Subset$HIS.gdp[2:nrow(Data.Subset)],NA)
  Data.Subset$Lead.HIS.gdp.per.capita <- c(Data.Subset$HIS.gdp.per.capita[2:nrow(Data.Subset)],NA)
  
  # compute growth rate
  Data.Subset$log.gdp.1st.Diff <- Data.Subset$Lead.log.gdp - Data.Subset$log.gdp
  Data.Subset$log.gdp.per.capita.1st.Diff <- Data.Subset$Lead.log.gdp.per.capita - Data.Subset$log.gdp.per.capita
  Data.Subset$HIS.gdp.1st.Diff <- Data.Subset$Lead.HIS.gdp - Data.Subset$HIS.gdp
  Data.Subset$HIS.gdp.per.capita.1st.Diff <- Data.Subset$Lead.HIS.gdp.per.capita - Data.Subset$HIS.gdp.per.capita
  
  return(Data.Subset)
})

# Construct share of GDP weights
All.Provinces.Data <- ddply(All.Provinces.Data,.(year),function(Data.Subset) {
  
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

# restrict data to provinces (not territories) and 1997 to 2014
All.Provinces.Data.Subset.4.Anal <- subset(All.Provinces.Data, (year < 2015) & (prov.abbrev %in% c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC")))


##########################################################################################
# Generate Summary stats table
##########################################################################################

# display a table of the min and max for BC and other provinces
Summary.Stats.Table <- ddply(All.Provinces.Data.Subset.4.Anal,.(cansim_code),function(Data.Subset) {
  
  # grab the BC Data to make the weights
  BC.Data.Subset <- subset(Data.Subset,prov.abbrev == "BC")
  Other.Data.Subset <- subset(Data.Subset,prov.abbrev != "BC")
  
  # find min and max for each
  Min.BC <- min(BC.Data.Subset$gdp.per.capita)
  Max.BC <- max(BC.Data.Subset$gdp.per.capita)
  Min.Other <- min(Other.Data.Subset$gdp.per.capita)
  Max.Other <- max(Other.Data.Subset$gdp.per.capita)
  
  # merge it back into the data set so that all observations (from that year) with that industry code have that weight
  Industry.Row <- data.frame(Min.BC.GDP.per.Capita = Min.BC,
                             Max.BC.GDP.per.Capita = Max.BC,
                             Min.Other.GDP.per.Capita = Min.Other,
                             Max.Other.GDP.per.Capita = Max.Other)
  
  return(Industry.Row)
})



##########################################################################################
# Diff-n-diffs on all provinces vs BC in 2001
##########################################################################################

# Basic diff n diffs using hyperbolic inverse sine of gdp and gdp per capita
HIS.gdp.1st.Diff.Diff.n.Diff <- lm(HIS.gdp.1st.Diff ~ BC + Post.2001 + BC.times.Post.2001, data=All.Provinces.Data, weights = gdp.Share.Weights)
summary(HIS.gdp.1st.Diff.Diff.n.Diff)
HIS.gdp.per.capita.1st.Diff.Diff.n.Diff <- lm(HIS.gdp.per.capita.1st.Diff ~ BC + Post.2001 + BC.times.Post.2001, data=All.Provinces.Data, weights = gdp.Share.Weights)
summary(HIS.gdp.per.capita.1st.Diff.Diff.n.Diff)


# Basic diff n diffs using log gdp and gdp per capita for observations where GDP > 0 
All.Provinces.Data.Subset.4.log.Anal <- subset(All.Provinces.Data.Subset.4.Anal,is.finite(log.gdp.1st.Diff))
log.gdp.1st.Diff.Diff.n.Diff <- lm(log.gdp.1st.Diff ~ BC + Post.2001 + BC.times.Post.2001, data=All.Provinces.Data.Subset.4.log.Anal, weights = gdp.Share.Weights)
summary(log.gdp.1st.Diff.Diff.n.Diff)
log.gdp.per.capita.1st.Diff.Diff.n.Diff <- lm(log.gdp.per.capita.1st.Diff ~ BC + Post.2001 + BC.times.Post.2001, data=All.Provinces.Data.Subset.4.log.Anal, weights = gdp.Share.Weights)
summary(log.gdp.per.capita.1st.Diff.Diff.n.Diff)

# Including year fixed effects
log.gdp.per.capita.1st.Diff.Diff.n.Diff.Year.FEs <- lm(log.gdp.per.capita.1st.Diff ~ BC + Post.2001 + BC.times.Post.2001 + as.factor(year), data=All.Provinces.Data.Subset.4.log.Anal, weights = gdp.Share.Weights)
summary(log.gdp.per.capita.1st.Diff.Diff.n.Diff.Year.FEs)

# Including industry fixed effects
log.gdp.per.capita.1st.Diff.Diff.n.Diff.Province.FEs <- lm(log.gdp.per.capita.1st.Diff ~ BC + Post.2001 + BC.times.Post.2001 + as.factor(cansim_code), data=All.Provinces.Data.Subset.4.log.Anal, weights = gdp.Share.Weights)
summary(log.gdp.per.capita.1st.Diff.Diff.n.Diff.Province.FEs)

# Including industry and year fixed effects
log.gdp.per.capita.1st.Diff.Diff.n.Diff.YearnProvince.FEs <- lm(log.gdp.per.capita.1st.Diff ~ BC + Post.2001 + BC.times.Post.2001 + as.factor(year) + as.factor(cansim_code), data=All.Provinces.Data.Subset.4.log.Anal, weights = gdp.Share.Weights)
summary(log.gdp.per.capita.1st.Diff.Diff.n.Diff.YearnProvince.FEs)

# Including industry by year fixed effects
log.gdp.per.capita.1st.Diff.Diff.n.Diff.YearbyProvince.FEs <- lm(log.gdp.per.capita.1st.Diff ~ BC + Post.2001 + BC.times.Post.2001 + as.factor(year)*as.factor(cansim_code), data=All.Provinces.Data.Subset.4.log.Anal, weights = gdp.Share.Weights)
summary(log.gdp.per.capita.1st.Diff.Diff.n.Diff.YearbyProvince.FEs)

# Make Classic Diff-in-Diff Figure
Growth.Non.BC.Pre <- coef(log.gdp.per.capita.1st.Diff.Diff.n.Diff)[1]
Growth.BC.Pre <- coef(log.gdp.per.capita.1st.Diff.Diff.n.Diff)[1] + coef(log.gdp.per.capita.1st.Diff.Diff.n.Diff)[2]
Growth.Non.BC.Post <- coef(log.gdp.per.capita.1st.Diff.Diff.n.Diff)[1] + coef(log.gdp.per.capita.1st.Diff.Diff.n.Diff)[3]
Growth.BC.Post <- coef(log.gdp.per.capita.1st.Diff.Diff.n.Diff)[1] + 
  coef(log.gdp.per.capita.1st.Diff.Diff.n.Diff)[2] + 
  coef(log.gdp.per.capita.1st.Diff.Diff.n.Diff)[3] +
  coef(log.gdp.per.capita.1st.Diff.Diff.n.Diff)[4]
Growth.BC.Counterfactual.Post <- coef(log.gdp.per.capita.1st.Diff.Diff.n.Diff)[1] + 
  coef(log.gdp.per.capita.1st.Diff.Diff.n.Diff)[2] + 
  coef(log.gdp.per.capita.1st.Diff.Diff.n.Diff)[3]
Factual.Data.4.Plotting <- data.frame(Growth = c(Growth.Non.BC.Pre,Growth.BC.Pre,Growth.Non.BC.Post,Growth.BC.Post), 
                                      Period = factor(c("Pre 2001","Pre 2001","Post 2001","Post 2001"), levels=c("Pre 2001","Post 2001")), 
                                      Province = c("Others","BC","Others","BC"))
Counterfactual.Data.4.Plotting <- data.frame(Growth = c(Growth.BC.Pre,Growth.BC.Counterfactual.Post), 
                                             Period = factor(c("Pre 2001","Post 2001"), levels=c("Pre 2001","Post 2001")), 
                                             Province = c("Counterfactual BC","Counterfactual BC"))
ggplot() + 
  geom_point(data=Factual.Data.4.Plotting,aes(x=Period,y=Growth, group=Province, color=Province),size=3) +
  geom_line(data=Factual.Data.4.Plotting,aes(x=Period,y=Growth, group=Province, color=Province),size=1.5) +
  geom_point(data=Counterfactual.Data.4.Plotting,aes(x=Period,y=Growth,group=Province), color="navyblue",shape=21,size=3) +
  geom_line(data=Counterfactual.Data.4.Plotting,aes(x=Period,y=Growth,group=Province),color="navyblue",linetype="dashed",size=1.5) +
  scale_color_manual(values=c("blue","black")) +
  theme_bw() + theme(legend.position = "none")


##########################################################################################
# Reproduce James' Figure at Industry Level
##########################################################################################

# compute GDP per capita for all 10 of Canada's provinces
Data.4.Anal <- ddply(All.Provinces.Data.Subset.4.Anal,.(cansim_code,year),function(Data.Subset) {
  Data.Subset$Population.Weights <- Data.Subset$population / sum(Data.Subset$population)
  
  # create a new data frame for CA observations
  CA.data <- data.frame(year = unique(Data.Subset$year),
                        prov.abbrev = "CA",
                        cansim_code = unique(Data.Subset$cansim_code),
                        ind_description = unique(Data.Subset$ind_description),
                        gdp.per.capita = sum(Data.Subset$Population.Weights*Data.Subset$gdp.per.capita),
                        BC = 0,
                        Post.2001 = as.numeric(subset(Data.Subset, prov.abbrev == "BC")$year >= 2001),
                        BC.times.Post.2001 = 0)
  
  Data.Subset <- merge(Data.Subset,CA.data, all = TRUE)
  
  return(Data.Subset)
})

# Create the industry label for the graph
Data.4.Anal$Industry.Label <- paste(Data.4.Anal$cansim_code,substr(Data.4.Anal$ind_description,1,10),sep=": ")

# normalize data so that 2001 is 100 as in James' Figure  
Data.4.Anal <- ddply(Data.4.Anal,.(cansim_code,prov.abbrev),function(Data.Subset) {
  Data.Subset$Normalized.gdp.per.capita <- 100*Data.Subset$gdp.per.capita / Data.Subset[which((Data.Subset$year == 2001)),"gdp.per.capita"]
  
  return(Data.Subset)
})

# plot the gdp per capita by industry and provinces

ggplot() + 
  geom_line(data = Data.4.Plotting <- subset(Data.4.Anal,(year <= 2014) & !(prov.abbrev %in% c("DD","SC","BC","CA"))), aes(x=year, y=Normalized.gdp.per.capita, group = prov.abbrev),color = "gray") +
  geom_line(data = subset(Data.4.Anal,(year <= 2014) & (prov.abbrev %in% c("BC"))), aes(x=year,y=Normalized.gdp.per.capita, group = prov.abbrev),color="blue") +
  geom_line(data = subset(Data.4.Anal,(year <= 2014) & (prov.abbrev %in% c("CA"))), aes(x=year,y=Normalized.gdp.per.capita, group = prov.abbrev),color="orange") +
  geom_vline(xintercept = 2001, colour="black") +
  facet_wrap(~ Industry.Label, scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0))


##########################################################################################
# Find weights for each Industry via Lasso
##########################################################################################

# focus on 1 industry at a time
All.Provinces.Plus.Synthetic.Control.Data <- ddply(Data.4.Anal,.(cansim_code),function(Data.Subset) {
  
  # grab an industry
  #Data.Subset <- subset(Data.Summary.2.Digit.NAICS,cansim_code == "41")
  
  # don't use Canada as a whole
  Data.Subset.4.Lasso <- subset(Data.Subset,prov.abbrev != "CA")
  
  # print out progress
  print(unique(Data.Subset.4.Lasso$ind_description))
  
  # prepare data for glmnet
  Outcome.Variable.Name <- "gdp.per.capita"
  Variable.Names.to.Exclude <- c("BC","year")
  #Variable.Names.to.Exclude <- c(Variable.Names.to.Exclude,unique(Data.Subset.4.Lasso[which(Data.Subset.4.Lasso[,Outcome.Variable.Name] == 0),"prov.abbrev"]))
  
  # Create a Pre-period matrix for Donor and Target for fitting
  Melted.Long.Data.Subset.Pre.Period <- melt(subset(Data.Subset.4.Lasso, year <= 2001), id.vars=c("prov.abbrev","year"), measure.var = Outcome.Variable.Name)
  Casted.Wide.Data.Subset.Pre.Period <- dcast(Melted.Long.Data.Subset.Pre.Period, year ~ prov.abbrev)
  Donor.Provinces.Outcome.Variable.Pre.Period <- data.matrix(Casted.Wide.Data.Subset.Pre.Period[!(names(Casted.Wide.Data.Subset.Pre.Period) %in% Variable.Names.to.Exclude)])
  Target.Province.Outcome.Variable.Pre.Period <- data.matrix(Casted.Wide.Data.Subset.Pre.Period[names(Casted.Wide.Data.Subset.Pre.Period) == "BC"])
  
  # Create a matrix for Donor for the entire period to produce the synthetic control
  Melted.Long.Data.Subset <- melt(subset(Data.Subset.4.Lasso, year <= 2014), id.vars=c("prov.abbrev","year"), measure.var = Outcome.Variable.Name)
  Casted.Wide.Data.Subset <- dcast(Melted.Long.Data.Subset, year ~ prov.abbrev)
  Donor.Provinces.Outcome.Variable <- data.matrix(Casted.Wide.Data.Subset[!(names(Casted.Wide.Data.Subset) %in% Variable.Names.to.Exclude)])
  
  # run glmnet
  cv.fit = cv.glmnet(x=Donor.Provinces.Outcome.Variable.Pre.Period, y=Target.Province.Outcome.Variable.Pre.Period, lower.limit=0, intercept=FALSE, grouped=FALSE)
  #plot(cv.fit)
  Lasso.Parameters <- coef(cv.fit, s = "lambda.min", exact = TRUE)
  print(Lasso.Parameters)
  Donor.Weights <- Lasso.Parameters[-1]
  Predicted.Outcome.4.Synthetic.Control <- predict(cv.fit, newx = Donor.Provinces.Outcome.Variable, s = "lambda.min")
  
  # rescale so that outcome levels are exactly the same in 2001 
  Rescaling.Factor <- Target.Province.Outcome.Variable.Pre.Period[5]/Predicted.Outcome.4.Synthetic.Control[5]
  Donor.Weights <- Rescaling.Factor*Donor.Weights
  Predicted.Outcome.4.Synthetic.Control <- Rescaling.Factor*Predicted.Outcome.4.Synthetic.Control
  
  # create a new data frame for synthetic control
  Synthetic.Control.Data <- data.frame(year = seq(min(Data.Subset$year),max(Data.Subset$year)),
                                       prov.abbrev = "SC",
                                       cansim_code = unique(Data.Subset$cansim_code),
                                       ind_description = unique(Data.Subset$ind_description),
                                       gdp.per.capita = as.numeric(Predicted.Outcome.4.Synthetic.Control),
                                       Industry.Label = paste(unique(Data.Subset$cansim_code),substr(unique(Data.Subset$ind_description),1,10),sep=": "),
                                       gdp.Share.Weights = subset(Data.Subset, prov.abbrev == "BC")$gdp.Share.Weights,
                                       BC = 0,
                                       Post.2001 = as.numeric(subset(Data.Subset, prov.abbrev == "BC")$year >= 2001),
                                       BC.times.Post.2001 = 0,
                                       AB.Weight = Donor.Weights[1],
                                       MB.Weight = Donor.Weights[2],
                                       NB.Weight = Donor.Weights[3],
                                       NL.Weight = Donor.Weights[4],
                                       NS.Weight = Donor.Weights[5],
                                       ON.Weight = Donor.Weights[6],
                                       PE.Weight = Donor.Weights[7],
                                       QC.Weight = Donor.Weights[8],
                                       SK.Weight = Donor.Weights[9])
  
  # merge it in
  Data.Subset <- merge(Data.Subset,Synthetic.Control.Data, all = TRUE)
  
  return(Data.Subset)
})

# plot the gdp per capita by industry and provinces
ggplot() + 
  geom_line(data = subset(All.Provinces.Plus.Synthetic.Control.Data,(year <= 2014) & !(prov.abbrev %in% c("DD","CA","BC","SC"))), aes(x=year,y=gdp.per.capita, group = prov.abbrev),color="gray") +
  geom_line(data = subset(All.Provinces.Plus.Synthetic.Control.Data,(year <= 2014) & (prov.abbrev %in% c("BC"))), aes(x=year,y=gdp.per.capita, group = prov.abbrev),color="blue") +
  geom_line(data = subset(All.Provinces.Plus.Synthetic.Control.Data,(year <= 2014) & (prov.abbrev %in% c("SC"))), aes(x=year,y=gdp.per.capita, group = prov.abbrev),color="red") +
  geom_vline(xintercept = 2001, colour="black") + 
  #geom_text(data = subset(All.Provinces.Plus.Synthetic.Control.Data,(year == 2014) & !(prov.abbrev %in% c("DD","CA"))), aes(label = prov.abbrev,x = year + 0.5,y=gdp.per.capita)) + 
  facet_wrap(~ Industry.Label, scales = "free_y") + theme_bw() + theme(legend.position = "none")


##########################################################################################
# Final Preparation for analysis: Isolate BC and SC and compute growth rates 
##########################################################################################

# Create the leads, lags, and growth rates for the synthetic controls
BC.Plus.Synthetic.Control.Data <- ddply(subset(All.Provinces.Plus.Synthetic.Control.Data,prov.abbrev %in% c("BC","SC")),.(prov.abbrev,cansim_code),function(Data.Subset) {
  
  # sort data by year
  Data.Subset <- Data.Subset[order(Data.Subset$year),]
  
  # compute log gdp per capita
  Data.Subset$log.gdp.per.capita <- log(Data.Subset$gdp.per.capita)
  
  # lead value
  Data.Subset$Lead.log.gdp.per.capita <- c(Data.Subset$log.gdp.per.capita[2:nrow(Data.Subset)],NA)
  
  # compute growth rate
  Data.Subset$log.gdp.per.capita.1st.Diff <- Data.Subset$Lead.log.gdp.per.capita - Data.Subset$log.gdp.per.capita
  
  return(Data.Subset)
})

# remove 2014 since it doesn't have a growth rate (due to no 2015 data)
BC.Plus.Synthetic.Control.Data <- subset(BC.Plus.Synthetic.Control.Data,year < 2014)



##########################################################################################
# Regression with Synthetic Controls
##########################################################################################

Outcome.Variable.Name <- "log.gdp.per.capita.1st.Diff"

# run the diff and diff now with the synthetic control...
Specification.String <- paste(Outcome.Variable.Name," ~ BC + Post.2001 + BC.times.Post.2001",sep="")
Weighted.Diff.n.Diff.Synthetic.Control <- lm(Specification.String, data=BC.Plus.Synthetic.Control.Data, weights = gdp.Share.Weights)
summary(Weighted.Diff.n.Diff.Synthetic.Control)

# run the diff and diff now with the synthetic control and year FEs
Specification.String <- paste(Outcome.Variable.Name," ~ BC + Post.2001 + BC.times.Post.2001 + as.factor(year)",sep="")
Weighted.Diff.n.Diff.Synthetic.Control <- lm(Specification.String, data=BC.Plus.Synthetic.Control.Data, weights = gdp.Share.Weights)
summary(Weighted.Diff.n.Diff.Synthetic.Control)

# run the diff and diff now with the synthetic control and industry FEs
Specification.String <- paste(Outcome.Variable.Name," ~ BC + Post.2001 + BC.times.Post.2001 + as.factor(cansim_code)",sep="")
Weighted.Diff.n.Diff.Synthetic.Control <- lm(Specification.String, data=BC.Plus.Synthetic.Control.Data, weights = gdp.Share.Weights)
summary(Weighted.Diff.n.Diff.Synthetic.Control)

# run the diff and diff now with the synthetic control and both year and industry FEs
Specification.String <- paste(Outcome.Variable.Name," ~ BC + Post.2001 + BC.times.Post.2001 + as.factor(year) + as.factor(cansim_code)",sep="")
Weighted.Diff.n.Diff.Synthetic.Control <- lm(Specification.String, data=BC.Plus.Synthetic.Control.Data, weights = gdp.Share.Weights)
summary(Weighted.Diff.n.Diff.Synthetic.Control)

# run the diff and diff now with the synthetic control and industry by year fixed effects
Specification.String <- paste(Outcome.Variable.Name," ~ BC + Post.2001 + BC.times.Post.2001 + as.factor(year)*as.factor(cansim_code)",sep="")
Weighted.Diff.n.Diff.Synthetic.Control.with.Complete.FEs <- lm(Specification.String, data=BC.Plus.Synthetic.Control.Data, weights = gdp.Share.Weights)
summary(Weighted.Diff.n.Diff.Synthetic.Control.with.Complete.FEs)

# Make Classic Diff-in-Diff Figure
Growth.Non.BC.Pre <- coef(Weighted.Diff.n.Diff.Synthetic.Control)[1]
Growth.BC.Pre <- coef(Weighted.Diff.n.Diff.Synthetic.Control)[1] + coef(Weighted.Diff.n.Diff.Synthetic.Control)[2]
Growth.Non.BC.Post <- coef(Weighted.Diff.n.Diff.Synthetic.Control)[1] + coef(Weighted.Diff.n.Diff.Synthetic.Control)[3]
Growth.BC.Post <- coef(Weighted.Diff.n.Diff.Synthetic.Control)[1] + 
  coef(Weighted.Diff.n.Diff.Synthetic.Control)[2] + 
  coef(Weighted.Diff.n.Diff.Synthetic.Control)[3] +
  coef(Weighted.Diff.n.Diff.Synthetic.Control)[4]
Growth.BC.Counterfactual.Post <- coef(Weighted.Diff.n.Diff.Synthetic.Control)[1] + 
  coef(Weighted.Diff.n.Diff.Synthetic.Control)[2] + 
  coef(Weighted.Diff.n.Diff.Synthetic.Control)[3]
Factual.Data.4.Plotting <- data.frame(Growth = c(Growth.Non.BC.Pre,Growth.BC.Pre,Growth.Non.BC.Post,Growth.BC.Post), 
                                      Period = factor(c("Pre 2001","Pre 2001","Post 2001","Post 2001"), levels=c("Pre 2001","Post 2001")), 
                                      Province = c("Others","BC","Others","BC"))
Counterfactual.Data.4.Plotting <- data.frame(Growth = c(Growth.BC.Pre,Growth.BC.Counterfactual.Post), 
                                             Period = factor(c("Pre 2001","Post 2001"), levels=c("Pre 2001","Post 2001")), 
                                             Province = c("Counterfactual BC","Counterfactual BC"))
ggplot() + 
  geom_point(data=Factual.Data.4.Plotting,aes(x=Period,y=Growth, group=Province, color=Province),size=3) +
  geom_line(data=Factual.Data.4.Plotting,aes(x=Period,y=Growth, group=Province, color=Province),size=1.5) +
  geom_point(data=Counterfactual.Data.4.Plotting,aes(x=Period,y=Growth,group=Province), color="navyblue",shape=21,size=3) +
  geom_line(data=Counterfactual.Data.4.Plotting,aes(x=Period,y=Growth,group=Province),color="navyblue",linetype="dashed",size=1.5) +
  scale_color_manual(values=c("blue","black")) +
  theme_bw() + theme(legend.position = "none")


##########################################################################################
# Plot the weights for the synthetic controls
##########################################################################################

# manipulate the data into the form needed by ggplot
Data.4.Plotting <- subset(BC.Plus.Synthetic.Control.Data,(year == 2013) & (prov.abbrev == "SC"))
Columns.2.Retain <- c("Industry.Label","AB.Weight","MB.Weight","NB.Weight","NL.Weight","NS.Weight","ON.Weight","PE.Weight","QC.Weight","SK.Weight")
Data.4.Plotting <- melt(Data.4.Plotting[,Columns.2.Retain], id.vars=c("Industry.Label"))
names(Data.4.Plotting) <- c("Industry.Label","Donor.Province","Donor.Weight")
Data.4.Plotting$Donor.Province <- substr(as.character(Data.4.Plotting$Donor.Province),1,2)

# plot the weights by industry
ggplot(data = Data.4.Plotting, aes(y=Donor.Weight, x = Donor.Province, color = Donor.Province, fill= Donor.Province)) + 
  geom_bar(stat = "identity", width = 1.2, alpha = 0.5) +
  geom_hline(yintercept = 1, colour="black") + 
  geom_text(aes(label=Donor.Province), vjust=1, color="black", size=2.5)+
  facet_wrap(~ Industry.Label, ncol = 15, scales = "free_y") + theme_bw() + 
  theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

write.csv(Data.4.Plotting,"Data4Figure5.csv")
