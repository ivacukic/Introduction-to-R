setwd("~/Dropbox/IALSA postdoc/R workshop/sample data/") # your own path to folder

data <- read.csv("AMSSurvey.csv", header=TRUE)

#Inspect the data set

head(data)
names(data)
View(data)

### Inspect variables ###

# variable types
is.factor(data$citizen) # is it categorical?
is.factor(data$count)
is.numeric(data$count) # is it continuous?

# how many cases per category?
table(data$sex)
table(data$type)
table(data$citizen)
table(data$type, data$citizen)

# how about numeric variables?
summary(data$count)
summary(data$count11)

### Basic manipulation ###

# rename a variable
names(data)
names(data)[1] <- "id" #telling R to go to the first variable, and give it name "id"
names(data)

# rename all variables
#names(data) <- c("one","two","three", "four", "five", "six")
#names(data)
# alternatively:  colnames(data) <- c("one","two","three", "four", "five", "six")

# decide you don't like the new names and go back to old
# comment out unwanted lines with # symbol 

# rename levels of a factor
data$sex <- factor(data$sex, levels = c(1, 2), labels = c("Male", "Female"))
levels(data$sex) # 1 = Male, 2 = Female
table(data$sex)

# DIY!
# combine two levels of the "type" into one (private and public unis from cat I into just one category "I")
table(data$type)
# Feel free to ask google for help!

# select only three uni related variables (type, count, count11)
names(data)

#  [A , B] this is how we note a matrix, A refers to rows, B refers to columns. comma is important 
# for R to know if you're talking about rows or columns! 

unis <- data[,c("type", "count", "count11")] #this is why we have , before c so R knows we want to select column
names(unis)
View(unis)

# select only data for men
men <- subset(data, data$sex=="Male")

# DIY task:
# select only data for women
# select only data for non-US women


# create a new variable that is sum of both counts
data$count_tot <- data$count + data$count11
names(data)
View(data)
summary(data$count_tot)

# delete a variable
data$count_tot <- NULL
names(data)

#alternatively, do not delete the count_tot variable, instead
#export an excel spreadsheet that contains it
write.csv(data, file = "new_data.csv") #don't forget to "undo" deleting it!

