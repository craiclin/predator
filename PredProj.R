##James
##Packages and Wokpath===============================================================================
library(data.table)
install.packages("dplyr")
library(dplyr)
install.packages("rpart")
library(rpart)
install.packages("ggplot2")
library(ggplot2)
library(MASS)
install.packages("splice")

getwd()
workpath <- "E:/Econ" ##set workpath here
setwd(workpath)

##Cleaning and storing data==========================================================================
##TRAINING DATA======================================================================================
training <- "training.csv"
dfTrain = read.csv(training, na.strings = c("", "NA"))  ##Turning blank columns values into NA values 
dfTrain <- dfTrain %>%
  select_if(~ !any(is.na(.)))           ##Deleting all NA values
dfTrain <- dfTrain[ -c(2:5) ] ##removing usernames, timestamps, etc.
View(dfTrain)

trainClasses<-dfTrain$classe
plot(trainClasses)

##TESTING DATA=======================================================================================
testing <- "testing.csv"
dfTest = read.csv(testing, na.strings = c("", "NA"))  ##Turning blank columns values into NA values 
dfTest <- dfTest %>%
  select_if(~ !any(is.na(.)))           ##Deleting all NA values from the testing set
dfTest <- dfTest[ -c(2:5) ] ##removing usernames, timestamps, etc.
View(dfTest)

##https://stackoverflow.com/questions/29572906/data-prediction-using-decision-tree-of-rpart
##https://www.r-bloggers.com/using-decision-trees-to-predict-infant-birth-weights/

##Model fitting - Decision Tree [James]==============================================================

##Using all of the data==============================================================================
columns <- dfTrain[c(2:56)]
fitTree4<-rpart(classe~.,columns)
result4 <- predict(fitTree4, dfTest, type="class")
View(result4)
plot(result4)

#Only using x,y,z variables==========================================================================
fitTree<-rpart(classe~
                 roll_belt+
                 pitch_belt+
                 yaw_belt+
                 total_accel_belt+
                 gyros_belt_x+
                 gyros_belt_y+
                 gyros_belt_z+
                 accel_dumbbell_x+
                 accel_dumbbell_y+
                 accel_dumbbell_z, 
               dfTrain) 
result <- predict(fitTree, dfTest, type="class")
View(result)
plot(result)

#Trying to plot the two fucking results==============================================================
?geom_jitter
geom_jitter(result, result4)
?geom_point
geom_point(data=result, result4)
geom_dotplot(aes(),result)

##Using all variables fuck this=======================================================================
fitTree2<-rpart(classe~., 
               dfTrain)  
result2 <- predict(fitTree2, dfTest, type="class")
View(result2)
plot(result2)


##nonsense=======
yurt <- dfTrain[c(2:56)]
View(yurt)
fitTree2<-rpart(classe~col(yurt, 1:55),dfTrain)  ##Decision Tree
result2 <- predict(fitTree2, dfTest, type="class")
View(result2)
plot(result)
?col

fuck<- splice(result)

##logRegBullshitAttempt====================
logRegModel <- glm(classe ~.,family=binomial(link='logit'),data=dfTrain)
summary(logRegModel)
?predict
testing <- predict(logRegModel, newData=dfTest)
summary(testing)
View(testing)