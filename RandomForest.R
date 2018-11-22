#http://www.kaggle.com/c/titanic-gettingStarted/data
train <- read.csv("C:/Users/tanuja/OneDrive/Documents/imarticus/titanic/train.csv")
View(train)
summary(train)

test <- read.csv("C:/Users/tanuja/OneDrive/Documents/imarticus/titanic/test.csv")
View(test)
summary(test)
# 
#************************************Data transformations********************************** 
#(fixing NA, fixing blanks, creating new variables, reducing LEVEL size of factors)
# have shown the fixing of data by replacing the values and not removing NA or blanks. - backfill/forward fill
# have shown how linear regression can be used to predict missing values of age and back fill it

test$Survived <- NA  # creating survived col in test with NA value to match the col in train data
combi <- rbind(train, test)

summary(combi)

anyNA(combi)

is.na(combi)

#working with missing variables
combi$Name <- as.character(combi$Name)  # converted name to char
summary(combi$Age)
#________________________________________________________________________________________________
# adding new col to dataframe
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})  

combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)
#___________________________________________________________________________________________________
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
#_______________________________________________________________________________________________
combi$FamilySize <- combi$SibSp + combi$Parch + 1
#_____________________________________________________________________________________________
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
#_____________________________________________________________________________________________
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))

table(combi$FamilyID)

famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

combi$FamilyID <- factor(combi$FamilyID)
#__________________________________________________________________________________________________
train <- combi[1:891,]     
test <- combi[892:1309,]
#___________________________________________________________________________________________________
summary(combi$Age)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")

summary(Agefit)

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi)

summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)


summary(combi$Fare)


which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#Random Forests in R can only digest factors with up to 32 levels
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

#with satisfactory transformations done lets create final train and test data for our model
train <- combi[1:891,]
test <- combi[892:1309,]

#************************
#install.packages('randomForest')
library(randomForest)

#Number in seed as param does not matter. 
#We just need to make sure to use same number 
#so same number of  random numbers are gerneated each time we use random forest function 

set.seed(415)

# will take longer as we are creating 2000 decision trees (uses bagging internally)
#bagging example - > sample(1:10, replace = TRUE)
# run above line everytime and it gneartes 10 numbers but different values each time (between 1 to 10)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

# shows which variables are important based on accuracy and gini index
varImpPlot(fit)

#may take  longer as 2000 trees need to predic and decide the best
#Prediction <- predict(fit, test)
Prediction <- predict(fit, train)

table(Prediction,train$Survived)


#submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
#write.csv(submit, file = "C:/Users/nikhil/OneDrive/imarticus/data/titanic/firstforest.csv")


#another way***************************************************
#decisions made using statistical tests instead of impurity meassure
#install.packages('party')
library(party)

set.seed(415)

#mtry - restriction on the no. of variables to sample at each node
#will take longer for same reason as above model, 2000 trees!
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

#may take  longer as 2000 trees need to predic and decide the best
Prediction <- predict(fit, train, OOB=TRUE, type = "response")

