# Title     : Titanic
# Created by: cfiguera
# Created on: 19/05/2018

# llibreries
library(rpart)
library(rpart.plot)
library(C50)

# import
titanic <- read.table("C:\Users\usuario\Google Drive\UOC\Tipologia i cicle de vida de les dades\PR2\train.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
summary(titanic)
attach(titanic)

# esborrar camps innecessaris
titanic$PassengerId <- NULL
titanic$Name <- NULL
titanic$Ticket <- NULL
titanic$Fare <- NULL
titanic$Embarked <- NULL

# dades buides
titanic$Age = ifelse(is.na(titanic$Age), ave(titanic$Age, FUN = function(x) mean(x, na.rm = TRUE)), titanic$Age)
titanic$Cabin <- NULL

# clean data
write.csv(titanic, "titanic_clean.csv")

# distribucio normal
library(nortest)
alpha = 0.05
cat("No normal distribution:\n")
for (i in 1:ncol(titanic)) {
    if (is.integer(titanic[,i])) {
        p_val = ad.test(titanic[,i])$p.value
        if (p_val < alpha) {
            cat(colnames(titanic)[i])
            cat(", ")
        }
    }
}

# variancia
shapiro.test(titanic$Pclass)
shapiro.test(titanic$Age)
shapiro.test(titanic$SibSp)
shapiro.test(titanic$Parch)

# creació arbre
arbre <- rpart(Survived ~ ., method="class", data = titanic)
print(arbre)
rpart.plot(arbre)

