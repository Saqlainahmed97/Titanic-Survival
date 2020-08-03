# Setting the Directory ---------------------------------------------------

setwd("D:\\Kaggle\\Titanic Competition")
getwd()

# Reading the files -------------------------------------------------------

train=read.csv("train.csv")
test=read.csv("test.csv")

# Missing Value and empty valiue treatment --------------------------------

colSums(is.na(train))
train$Age[is.na(train$Age)]=mean(train$Age,na.rm = TRUE)
View(train)
colSums(is.na(test))
test$Age[is.na(test$Age)]=mean(test$Age,na.rm = TRUE)
View(test)
str(train)
summary(train)
colSums(train=="")
colSums(test=="")
summary(test)
test$Fare[153]<-mean(test$Fare,na.rm=TRUE)

# Removing columns which have higher missing values or are not tha --------
train$PassengerId<- NULL
train$Name<- NULL
train$Ticket<- NULL
train$Embarked<- NULL
train$Cabin<- NULL

# We see that age category less than 10 has 61% chance of survivin --------
table(train$Survived,train$Age<10)
train$Child<-train$Age<10
test$Child<-test$Age<10
str(train)

# Making sex category as numeric ------------------------------------------
train$Sex<-as.numeric(train$Sex)
test$Sex<-as.numeric(test$Sex)


# Creating Models ---------------------------------------------------------
model1 = glm(Survived~., data = train, family = binomial)
summary(model1)
model2=glm(Survived~Pclass+Sex+Age+SibSp+Child,family = "binomial",data = train)
summary(model2)
model3 = glm(Survived ~ . - Parch - Fare -Age, data = train, family = binomial)
summary(model3)

# Test Accuracy of Model --------------------------------------------------

baseAcur = 549/(549 + 342)
predicttrain = predict(model3, type = "response")
table(train$Survived, predicttrain >= 0.5)
accuracy = (240 + 478) / nrow(train)
sensitivity = 240 / (240 + 102)
specificity = 478 / (478 + 71)
cat("accuracy: ", accuracy, " > ", "baseline: ", baseAcur)

# Predicting Survivability ------------------------------------------------

predicttest = predict(model3, type = "response", newdata = test)

test$Survived = as.numeric(predicttest >= 0.5)
table(test$Survived)

Predictions = data.frame(test[c("PassengerId","Survived")])
write.csv(file = "TitanicPrediction", x = Predictions)
