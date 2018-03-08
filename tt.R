titanic.train <- read.csv(file = "TitanicTrain.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "TitanicTEST.csv", stringsAsFactors = FALSE, header = TRUE)

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

titanic.test$Survived <- NA

titanic.full <- rbind(titanic.train, titanic.test)

titanic.full[titanic.full$Embarked == "", "Embarked"] <- "S"
age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median
Fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- Fare.median

# boxplot(titanic.full$Fare)
# boxplot.stats(titanic.full$Fare)
# upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
# outlier.filter <- titanic.full$Fare < upper.whisker
# 
# fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
# 
# fare.model = lm(Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked, 
#                 data = titanic.full[outlier.filter,])
# fare.row <- titanic.full[is.na(titanic.full$Fare), 
#                          c("Pclass" , "Sex" , "Age" , "SibSp" , "Parch" , "Embarked")]
# fare.prediction <- predict(fare.model, newdata = fare.row)
# titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.prediction
# 
# boxplot(titanic.full$Age)
# boxplot.stats(titanic.full$Age)
# upper.whisker <- boxplot.stats(titanic.full$Age)$stats[5]
# outlier.filter <- titanic.full$Age < upper.whisker
# 
# Age.equation = "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
# 
# Age.model = lm(Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked, 
#                 data = titanic.full[outlier.filter,])
# 
# Age.row <- titanic.full[is.na(titanic.full$Age), 
#                          c("Pclass" , "Sex" , "Fare" , "SibSp" , "Parch" , "Embarked")]
# Age.prediction <- predict(Age.model, newdata = Age.row)
# titanic.full[is.na(titanic.full$Age), "Age"] <- Age.prediction

titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

titanic.train <- titanic.full[titanic.full$IsTrainSet == TRUE,]
titanic.test  <- titanic.full[titanic.full$IsTrainSet == FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)

Survived.Equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived.Formula <- as.factor(Survived.Equation)

install.packages("randomForest")
library(randomForest)

titanic.model <- randomForest(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                              data = titanic.train, 
                              ntree = 500, 
                              mtry = 3,
                              nodesize = .01 * nrow(titanic.train))

SurvivedTitanic <- predict(titanic.model, titanic.test)

PassengerID <- titanic.test$PassengerId

out.df = as.data.frame(PassengerID)
out.df$Survived <- SurvivedTitanic

str(out.df)

write.csv(out.df, file = "TitanicSubmission0.csv", row.names = FALSE)


