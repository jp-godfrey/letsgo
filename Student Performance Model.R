#Project introduction and description
#In this kernel we are going to explore "Student Performance in Exams" dataset, generate various cool visualizations to better understand the data, and create a prediction model for scores in mathematics.

##The first step is to read our datatset into R and explore it's summary and structure. The dataset includes 8 variables with 1000 observations. Among the variables are math, reading and writing scores, demographical information (parents education, gender, race) and data on test preparation.

#The summary() functions provides us with information on each variable such as type of data: character, numerical, and if numerical, we find basic descriptive statisctics such as measure of central tendency and spread. It also provides as with information on missing values (NA values).

#Next, we rename our variables for cleaner look & coding.####Libraries

#install the following packages (all other packages are preloaded in R)
install.packages("kernlab")
install.packages("arules")
install.packages("InformationValue")

#load libraries 
library(ggplot2)
library(kernlab)
library(arules)
library(InformationValue)
library(readxl)
library(readr)

## Set working directory (make sure your file is in the working directory, no spaces)
### Reading/cleaning data
######################################################################
mydata <- read_csv("StudentsPerformance.csv")

# Creating a data frame
mydata <- as.data.frame(mydata)
summary(mydata) # summary
# There are no NA or missing values

## Replace column names
colnames(mydata)
namesOfColumns <- c("Gender","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")
colnames(mydata) <- namesOfColumns

#In this section we are going to explore into categorical variables. Student performance dataset contains five categorical variables. table() function allows us to group categorical variables by each category and determine how many observations belong to a specific category. The table() function to search for "dirty" data such as misspellings or data that doesn't make sense categories in our dataset.
## Looking into dirty data, exploring categorical variables
table(mydata$Gender)
table(mydata$Race)
table(mydata$Parent_Education)
table(mydata$Lunch)
table(mydata$Test_Prep)
# Categories are uniform throughout the dataset

#Up next is data visualization with an array of plots.

#We are going to plot various test scores verses gender using GGPLOT package to determine the scoring dynamics between males and females.. GGPLOT requires three key components:
##Define data in form of data frame.
###Describe the aesthetics for the visualization or how to map the attributes.(look in book)
####Define the geometry or type of graphics to be used.

## Plot Exam scores by Gender to determine if there is a different score tendency for each gender.
# Math scores by Gender plot (ensure all variables are as they are in the dataset)

p <- ggplot(mydata, aes(Math_Score)) + geom_histogram(binwidth=5, color="gray", aes(fill=Gender))
p <- p + xlab("Math_Scores") + ylab("Gender") + ggtitle("Math Scores by Gender")
p

# Reading scores by Gender plot
p1 <- ggplot(mydata, aes(Reading_Score)) + geom_histogram(binwidth=5, color="gray", aes(fill=Gender))
p1 <- p1 + xlab("Reading_Scores") + ylab("Gender") + ggtitle("Reading Scores by Gender")
p1

# Writing scores by Gender plot
p2 <- ggplot(mydata, aes(Writing_Score)) +  geom_histogram(binwidth=5, color="gray", aes(fill=Gender))
p2 <- p2 + xlab("Writing_Scores") + ylab("Gender") + ggtitle("Writing Scores by Gender")
p2

#Now we will make a boxplot:
#Students who completed the prep class had better scores in all three tests.
##male students have received better scores in Math while female students in reading and writing.
###there is a presence of outliers in all three tests.

# Boxplot of scores and Test Prep by Gender
b <- ggplot(mydata, aes(Gender, Writing_Score, color = Test_Prep))
b <- b + geom_boxplot()
b <- b + ggtitle("Writing scores by Gender Boxplot")
b <- b + xlab("Gender") + ylab("Writing_Scores")
b

b1 <- ggplot(mydata, aes(Gender, Math_Score, color = Test_Prep))
b1 <- b1 + geom_boxplot()
b1 <- b1 + ggtitle("Math scores by Gender Boxplot")
b1 <- b1 + xlab("Gender") + ylab("Math_Scores")
b1

b2 <- ggplot(mydata, aes(Gender, Reading_Score, color = Test_Prep))
b2 <- b2 + geom_boxplot()
b2 <- b2 + ggtitle("Reading scores by Gender Boxplot")
b2 <- b2 + xlab("Gender") + ylab("Reading_Scores")

#Prediction Model

#In this section we are going to build a linear regression model, predicting 
#Math scores. Math scores- dependent variable (Y), Writing_Score, Gender, Race, 
#Lunch, Parent_Education, Test_Prep - independent variables (X). First, we will 
#split our dataset into training and testing datasets. Then, we will run lm() function 
#with "training" data, predict() function on "testing" data, and create a visualization of 
#our regression model with regression line and 95% confidence intervals. Please note that 
#assumptions of linear regression are not explored in this kernel. I decided to run linear 
#regression with Writing scores as it produces slightly higher R squared value.

### Prediction model
randIndex <- sample(1:dim(mydata)[1])
#  # In order to split data, create a 2/3 cutpoint and round the number
cutpoint2_3 <- floor(2*dim(mydata)[1]/3)

# create train data set, which contains the first 2/3 of overall data
trainData <- mydata[randIndex[1:cutpoint2_3],]
# dim(trainData)
# head(trainData)

# create test data, which contains the left 1/3 of the overall data
testData <- mydata[randIndex[(cutpoint2_3+1):dim(mydata)[1]],]
# dim(testData)   # check test data set
# head(trainData)

#------------------------------------------------------lm model
model <- lm(Math_Score ~ Writing_Score + Gender + Race + Lunch + Parent_Education + Test_Prep,data=trainData)
summary(model)
lmPred <- predict(model,testData,interval = "prediction", level=0.95)
summary(lmPred)
head(lmPred)

# 1. Add predictions 
mydata1 <- cbind(testData, lmPred)
head(mydata1)
# 2. Regression line + confidence intervals
p <- ggplot(mydata1, aes( fit, Math_Score)) +
  geom_point() +
  stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  xlab("Predicted Scores") + ylab("Test Scores") 

