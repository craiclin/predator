#Random Forest[James]=================================================================================

install.packages("randomForest")
require(randomForest)
require(MASS)

#https://rstudio-pubs-static.s3.amazonaws.com/103504_0acaf82195e5477f9f7a054058df5afa.html 
#^^^project solution found online

#https://www.r-bloggers.com/random-forests-in-r/

randy<- randomForest(classe~. , columns)

forestTest <- dfTest[-c(1)]
View(forestTest)
forestResult <- predict(randy, forestTest, type="class")    ##running into an error here
View(forestResult)
