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
# inspect variables (variable type, range/frequency, distribution/histograms, 
#weird values, NAs)

# clean up if needed
# assign labels to factor levels

# read in second dataset, week4datab, repeat like for dataset 1

# recode anxiety variable into a new one with three categories:
# 0-3 no anxiety; 4-6 moderate; 7+ severe; assign labels

# prepare both datasets for merging
# merge data sets

# main analyses:
# produce table of descriptives for all variables in the study, stratified by dementia status and total
# what is the correlation between bmi and iq. produce scatterplot. 
# does bmi differ between those with and without dementia
# do mean levels of anxiety differ between treatment groups (1 no treatment; 2 placebo; 3 intervention)
# does iq predict dementia?
# does iq predict dementia, controlling for age, sex and bmi?
