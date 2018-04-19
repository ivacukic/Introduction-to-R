setwd("~/Dropbox/IALSA postdoc/R workshop/sample data/") # your own path to folder

# read in week 2 dataset
data1 <- read.csv("week2data.csv", header=TRUE)

### Regression ###
# continuous outcome
# does age predicts mmse
m1 <- lm(mmse ~ age, data=data1)
summary(m1)

#if we want to standardise a variable for ease of interpretation
m2 <- lm(scale(mmse) ~ age, data=data1)
summary(m2)

# does age predict mmse, controlling for sex and disability
m3 <- lm(scale(mmse) ~ age + as.factor(sex) + as.factor(disability), data=data1)
summary(m3)

# does age predict ces.d
m3 <- lm(scale(ces.d) ~ age, data=data1)
summary(m3)

# does age predict ces.d, controlling for sex and disability
m4 <- lm(scale(ces.d) ~ age + as.factor(sex) + as.factor(disability), data=data1)
summary(m4)


# binary outcome: glm function
# recode ces-d into binary accodring to cutoff

data1$dep_cat[(data1$ces.d>=16)] <- 1
data1$dep_cat[(data1$ces.d<16)] <- 0

m5 <- glm(dep_cat ~ age, data = data1, family = "binomial") 
summary(m5)

# does mmse score predict depression?

m5 <- glm(dep_cat ~ mmse, data = data1, family = "binomial") 
summary(m5)


### Exercise ###

# read in first dataset week4dataa
data2 <- read.csv("week4dataa.csv", header=TRUE) # converted from xls to csv

# inspect variables (variable type, range/frequency, distribution/histograms, 
#weird values, NAs)
# clean up if needed

names(data2) # see what variables are there
View(data2) # show dataframe as spreadsheet
summary(data2$id) # looks OK, no weird values
summary(data2$age) # 3 NAs, values look OK
table(data2$sex) # looks OK
summary(data2$bmi) # weird values, need to inspect further
hist(data2$bmi)
is.na(data2$bmi) = data2$bmi == -999
is.na(data2$bmi) = data2$bmi == 804
summary(data2$bmi) # still one unlikely value
is.na(data2$bmi) = data2$bmi == 7
summary(data2$bmi) # looking OK now
summary(data2$iq) # looks OK

# assign labels to factor levels
data2$sex <- factor(data2$sex, levels = c(1, 2), labels = c("Male", "Female"))
table(data2$sex)
View(data3)

# read in second dataset, week4datab, repeat like for dataset 1
data3 <- read.csv("week4datab.csv", header=TRUE) #converted from xls
names(data3)
summary(data3$ID) #OK
table(data3$dementia) #OK
table(data3$treatment) #OK
summary(data3$anxiety)
hist(data3$anxiety)

# recode anxiety variable into a new one with three categories:
# 0-3 no anxiety; 4-6 moderate; 7+ severe; assign labels

data3$anx_cat <- NA
data3$anx_cat[(data3$anxiety <=3)] <- 0
data3$anx_cat[(data3$anxiety > 3 & data3$anxiety < 7)] <- 1
data3$anx_cat[(data3$anxiety >=7)] <- 2
table(data3$anx_cat)

              
# prepare both datasets for merging
names(data3)[1] <- "id"

# merge data sets
merged_data <- merge(data2, data3, by="id", all=TRUE)

# main analyses:
# produce table of descriptives for all variables in the study, stratified by dementia status and total
library(tableone) 

#continuous vars
tablevars <- c("age", "bmi", "iq", "anxiety")
tableOne <- CreateTableOne(vars=tablevars, strata=c("sex"),data=merged_data) #stratified by sex
tableOne

tableOne <- CreateTableOne(vars=tablevars,data=merged_data) #total
tableOne

#categorical vars
catVars <- c("dementia", "treatment", "anx_cat")
catTableOverall <- CreateCatTable(vars = catVars,strata=c("sex"), data = merged_data) #stratified by sex
catTableOverall

catTableOverall <- CreateCatTable(vars = catVars, data = merged_data) #total
catTableOverall

# what is the correlation between bmi and iq. produce scatterplot. 

cor.test(merged_data$bmi, merged_data$iq)
plot(merged_data$bmi, merged_data$iq)

# does bmi differ between those with and without dementia
m1 <- t.test(bmi ~ dementia, data = merged_data, paired= F)
m1

# do mean levels of anxiety differ between treatment groups (1 no treatment; 2 placebo; 3 intervention)
m2 <- aov(anxiety ~ treatment, data=merged_data)
summary(m2)

# does iq predict dementia?
m3 <- glm(dementia ~ scale(iq), data=merged_data, family="binomial")
summary(m3)

# does iq predict dementia, controlling for age, sex and bmi?
m4 <- glm(dementia ~ scale(iq) + as.factor(sex) + age + scale(bmi), data=merged_data, family="binomial")
summary(m4)


### That's all folks! ###