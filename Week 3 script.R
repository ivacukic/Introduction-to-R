setwd("~/Dropbox/IALSA postdoc/R workshop/sample data/") # your own path to folder

# read in data from week 1
data_old <- read.csv("AMSSurvey.csv", header=TRUE)

#recode levels of a factor (week 1 homework)
table(data_old$type)
# combine I(Pr) and I(Pu) to be I
data_old$type_recoded <- recode(data_old$type, "'I(Pr)' = 'I'; 'I(Pu)' = 'I';'Va' = 'V' ")
table(data_old$type_recoded)

# read in data from week 2
data <- read.csv("week2data.csv", header=TRUE)

# read in new data
newdata <- read.csv("week3data.csv", header=TRUE)
#have a look what's in this dataset, examine the variables

# merge two data frames
merged_data <- merge(data, newdata, by="id", all=TRUE)

#oops, looks like an error message! can you tell what went wrong?

names(data)
names(newdata)
#examine both id variables
# there is one wrong id value, substitute with the correct one
#data.frame[row_number, column_number] = new_value

data
data[20, 1] = 20
data

#another way using variable (column) name
data$id[20]=20 #row number in square brackets

#we also need to rename one of the id variables to match the other
names(newdata)[1] <- "id" #renaming the id variable in the second data frame to match the first

# now we can finally merge
merged_data <- merge(data, newdata, by="id", all=TRUE) # all.x T; all.y=T

# create data frame with no missing values

final_data <- na.omit(merged_data)


# independent t-test
# generic formula
model <- t.test(outcome ~ predictor, data = data, paired= T/F)

# Do ces.d scores differ between men and women?
# Do mmse scores differ between those with and without disability?

# calculate the effect size
model <- t.test(ces.d ~ sex, data=final_data) #run the model
t <- model$statistic[[1]] # extract the value of t
df <- model$parameter[[1]] # extract degrees of freedom
r <- sqrt(t^2/(t^2+df)) #calculate the effect size r based on t and df
round(r, digits=2) #round to two decimal places

# ANOVA

model2 <- aov(ces.d ~ group, data=final_data)
summary(model2)
plot(model2)

#effect size e.g. omega squared can be calculated based on the values provided in the summary()

# remove everything from the environment
rm(list=ls())