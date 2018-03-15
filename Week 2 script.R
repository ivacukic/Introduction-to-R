setwd("~/Dropbox/IALSA postdoc/R workshop/sample data/")

#read in data
data <- read.csv("week2data.csv", header=TRUE)

#check variable names
#how many men and women? 1=men; 2=women
#what is the age range?

#change the odd value into NA
is.na(data$age) = data$age == 219

#how many NAs in MMSE and CES-D variables?
#what is the range for MMSE and CES-D scores? 


#check distributions for continuous variables
hist() #variable you want to check in parentheses

#split into categories according to clinical cut-offs

#greater or equal 16 clinical depression
data$dep_cat <- NA
data$dep_cat[(data$ces.d>=16)] <- 1
data$dep_cat[(data$ces.d<16)] <- 0
table(data$dep_cat)

#DIY:make a categorical variable for mmse according to
# clinical cut off scores: 
#24-30 no impairment; 18-23 mild; 0-17 severe cognitive impairment

#scatterplot MMSE and CES-D
plot(data$mmse, data$ces.d)
?(plot) #see help for this function to see what else you can do
# ?() asks for help for any function, write the function in ()
#add labels to x and y axes
plot(data$mmse, data$ces.d, xlab="MMSE", ylab="CES-D")

#save as image in your working directory
png(filename="first-plot.png")
plot(data$mmse, data$ces.d, xlab="MMSE", ylab="CES-D")
dev.off()


### correlation coefficient ###

#two variables
cor(data$mmse, data$ces.d, use= "complete.obs", method = "pearson") 

#matrix
cor(data, use = "complete.obs", method = "pearson") 

# to see p-values, you can use the Hmisc package
install.packages("Hmisc")
library("Hmisc")
cor.test(data$mmse, data$ces.d)

 
### table of descriptives ###

install.packages("tableone")
library(tableone)

#continuous vars
tablevars <- c("age", "mmse", "ces.d")
tableOne <- CreateTableOne(vars=tablevars, strata=c("sex"),data=data) #stratified by sex
tableOne

tableOne <- CreateTableOne(vars=tablevars,data=data) #total
tableOne

#categorical vars
catVars <- c("disability")
catTableOverall <- CreateCatTable(vars = catVars,strata=c("sex"), data = data) #stratified by sex
catTableOverall

catTableOverall <- CreateCatTable(vars = catVars, data = data) #total
catTableOverall
