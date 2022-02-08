#Caricamento del Dataset
df <- read.csv("ionosphere_data.csv")
ion.df <- df

#Pulizia e organizzazione del Dataset
df <- na.omit(df)

names(df)[names(df) == "X1"] <- "a"
names(df)[names(df) == "X0"] <- "b"
names(df)[names(df) == "X0.99539"] <- "c"
names(df)[names(df) == "X.0.05889"] <- "d"
names(df)[names(df) == "X0.85243"] <- "e"
names(df)[names(df) == "X0.02306"] <- "f"
names(df)[names(df) == "X0.83398"] <- "g1"
names(df)[names(df) == "X.0.37708"] <- "h"
names(df)[names(df) == "X1.1"] <- "i"
names(df)[names(df) == "X0.03760"] <- "j"
names(df)[names(df) == "X0.85243.1"] <- "k"
names(df)[names(df) == "X.0.17755"] <- "l"
names(df)[names(df) == "X0.59755"] <- "m"
names(df)[names(df) == "X.0.44945"] <- "n"
names(df)[names(df) == "X0.60536"] <- "o"
names(df)[names(df) == "X.0.38223"] <- "p"
names(df)[names(df) == "X0.84356"] <- "q"
names(df)[names(df) == "X.0.38542"] <- "r"
names(df)[names(df) == "X0.58212"] <- "s"
names(df)[names(df) == "X.0.32192"] <- "t"
names(df)[names(df) == "X0.56971"] <- "u"
names(df)[names(df) == "X.0.29674"] <- "v"
names(df)[names(df) == "X0.36946"] <- "w"
names(df)[names(df) == "X.0.47357"] <- "x"
names(df)[names(df) == "X0.56811"] <- "y"
names(df)[names(df) == "X.0.51171"] <- "z"
names(df)[names(df) == "X0.41078"] <- "aa"
names(df)[names(df) == "X.0.46168"] <- "ab"
names(df)[names(df) == "X0.21266"] <- "ac"
names(df)[names(df) == "X.0.34090"] <- "ad"
names(df)[names(df) == "X0.42267"] <- "ae"
names(df)[names(df) == "X.0.54487"] <- "af"
names(df)[names(df) == "X0.18641"] <- "ag"
names(df)[names(df) == "X.0.45300"] <- "ah"
names(df)[names(df) == "g"] <- "target"

levels(df$target)
df$target <- as.factor(df$target)
levels(df$target)

summary(df)

df$a <- NULL
df$b <- NULL


#----------------------------------------------------------------------------
#SPLITTING

N <- nrow(df)
N.train <- 225
N.test <- 60
N.val <- 65
#Divisione del Dataset in TRAINING SET e TEST SET
train.sample <- sample(N, N.train)
df.train <- df[train.sample, ]
df.test <- df[-train.sample, ]
#Divisione ulteriore del Test Set in VALIDATION SET e TEST SET
val.sample <- sample(N.test + N.val, N.val) 
df.val <- df.test[val.sample, ]
df.test <- df.test[-val.sample, ]

df <- df.train 
N <- nrow(df)


#----------------------------------------------------------------------------
#VISUALIZZAZIONE

#Correlation Matrix
library(corrplot)
df.corr <- df
df.corr$target <- as.numeric(df.corr$target)
cor.matrix <- cor(df.corr)
corrplot(cor.matrix, method="circle")

#Boxplot
library(ggplot2)
df.good <- subset(df, subset=(target=='g'))
df.good$target <- NULL
df.bad <- subset(df, subset=(target=='b'))
df.bad$target <- NULL

boxplot(df.bad)
boxplot(df.good)
ggplot(data=df, aes(target, c)) + geom_boxplot()
ggplot(data=df, aes(target, i)) + geom_boxplot()
ggplot(data=df, aes(target, v)) + geom_boxplot()
ggplot(data=df, aes(target, x)) + geom_boxplot()

#Piechart
library(plotrix)
table(df$target)
pie3D(table(df$target),col = c('red', 'green'), labels = c("Bad: 36%", "Good: 64%"))


#----------------------------------------------------------------------------
#ADDESTRAMENTO DEL MODELLO

library(e1071)
model.SVM <- svm(target ~ ., df, kernel="polynomial", cost=1, degree=5) 
summary(model.SVM)

#Valutazione della performance del modello
MR <- function(y.pred, y.true) {
  res <- mean(y.pred != y.true)
  return(res)
}
Acc <- function(y.pred, y.true) {
  res <- 1 - mean(y.pred != y.true)
  return(res)
}

y.pred <- predict(model.SVM, df.test)

MR.linear <- MR(y.pred, df.test$target)
MR.linear 

Acc.linear <- Acc(y.pred, df.test$target)
Acc.linear 


#----------------------------------------------------------------------------
#Migliorie sul costo e il  grado del modello con kernel polinomiale analizzando
#il Training Set e il Validation Set
df.val <- na.omit(df)
MR.total <- 1:10
MR.val <- 1:10
for (d in 1:10) {
  model.SVM <- svm(target ~ ., df, kernel="polynomial", cost=1, degree=d)
  y.pred <- predict(model.SVM, df)
  MR.poly <- MR(y.pred, df$target)
  MR.total[d] <- MR.poly
  
  model.SVM <- svm(target ~ ., df, kernel="polynomial", cost=1, degree=d)
  y.pred <- predict(model.SVM, df.val)
  MR.poly <- MR(y.pred, df.val$target)
  MR.val[d] <- MR.poly
}
plot(MR.total, pch = 16, col='red', xlab="Degree", ylim=c(0, 1))
points(MR.val, type='p', col='black')
title(main="Modello con kernel polinomiale")
legend(x=6.5, y=1, legend=c("Validation Set", "Training Set"), fill = 1:2)

MR.total2 <- 1:10
MR.val2 <- 1:10
for (c in 1:10) {
  model.SVM <- svm(target ~ ., df, kernel="polynomial", cost=c, degree=2)
  y.pred <- predict(model.SVM, df)
  MR.poly <- MR(y.pred, df$target)
  MR.total2[c] <- MR.poly
  
  model.SVM <- svm(target ~ ., df, kernel="polynomial", cost=c, degree=2)
  y.pred <- predict(model.SVM, df.val)
  MR.poly <- MR(y.pred, df.val$target)
  MR.val2[c] <- MR.poly
}
plot(MR.total2, pch = 16, col='red', xlab="Cost", ylim=c(0, 0.5))
points(MR.val2, type='p', col='black')
title(main="Modello con kernel polinomiale")
legend(x=6.5, y=0.5, legend=c("Validation Set", "Training Set"), fill = 1:2)

#Si evidenziano COST=10 e DEGREE=2
model.SVM <- svm(target ~ ., df, kernel="polynomial", cost=10, degree=2)
y.pred <- predict(model.SVM, df)
MR.poly <- MR(y.pred, df$target)
MR.poly 

y.pred <- predict(model.SVM, df.test)
MR.polynomial <- MR(y.pred, df.test$target)
MR.polynomial 

Acc.polynomial <- Acc(y.pred, df.test$target)
Acc.polynomial 


#----------------------------------------------------------------------------
#Riprocesso utilizzando un modello con kernel radiale analizzando
#il Training Set e il Validation Set
df.val<- na.omit(df)
MR.total <- 1:10
MR.val <- 1:10
for (d in 1:10) {
  model.SVM <- svm(target ~ ., df, kernel="radial", cost=1, gamma=1/d)
  y.pred <- predict(model.SVM, df)
  MR.radial <- MR(y.pred, df$target)
  MR.total[d] <- MR.radial
  
  model.SVM <- svm(target ~ ., df, kernel="radial", cost=1, gamma=1/d)
  y.pred <- predict(model.SVM, df.val)
  MR.radial <- MR(y.pred, df.val$target)
  MR.val[d] <- MR.radial
}
plot(MR.total, pch = 16, col='red', xlab="Gamma", ylim=c(0, 0.5))
points(MR.val, type='p', col='black')
title(main="Modello con kernel radiale")
legend(x=6.5, y=0.5, legend=c("Validation Set", "Training Set"), fill = 1:2)

MR.total2 <- 1:10
MR.val2 <- 1:10
for (c in 1:10) {
  model.SVM <- svm(target ~ ., df, kernel="radial", cost=c, gamma=1/9)
  y.pred <- predict(model.SVM, df)
  MR.radial <- MR(y.pred, df$target)
  MR.total2[c] <- MR.radial
  
  model.SVM <- svm(target ~ ., df, kernel="radial", cost=c, gamma=1/9)
  y.pred <- predict(model.SVM, df.val)
  MR.radial <- MR(y.pred, df.val$target)
  MR.val2[c] <- MR.radial
}
plot(MR.total2, pch = 16, col='red', xlab="Cost", ylim=c(0, 0.5))
points(MR.val2, type='p', col='black')
title(main="Modello con kernel radiale")
legend(x=6.5, y=0.5, legend=c("Validation Set", "Training Set"), fill = 1:2)

#Si evidenziano COST=1 e GAMMA=1/9
model.SVM <- svm(target ~ ., df, kernel="radial", cost=1, gamma=1/9)
y.pred <- predict(model.SVM, df)
MR.radial <- MR(y.pred, df$target)
MR.radial 

y.pred <- predict(model.SVM, df.test)
MR.radial <- MR(y.pred, df.test$target)
MR.radial

Acc.radial <- Acc(y.pred, df.test$target)
Acc.radial


#----------------------------------------------------------------------------
#ADDESTRAMENTO DEL MODELLO DEFINITIVO

model.SVM <- svm(target ~ ., df, kernel="radial", cost=1, gamma=1/9)
y.pred <- predict(model.SVM, df.test)
MR.final <- MR(y.pred, df.test$target)
MR.final 


#----------------------------------------------------------------------------
#INTERPRETAZIONE PROBABILISTICA

library(dplyr)
df$target <- recode(df$target, g = "1", b = "0")
df$target <- as.factor(df$target)
levels(df$target)
df.test$target <- recode(df.test$target, g = "1", b = "0")
df.test$target <- as.factor(df.test$target)
levels(df.test$target)

SVM.probs <- svm(target ~ ., data=df, kernel="radial", cost=1, gamma=1/9, probability=TRUE)
y.pred <- predict(SVM.probs, df.test, probability=TRUE)
y.probs <- attr(y.pred, "probabilities")

y.probs <- y.probs[, 1]

y.total <- rep(0, 50)
y.total[y.probs > 0.7] <- 1

#Confusion Matrix
table(y.pred, df.test$target)

#Curva di ROC
library(ROCR)
pred <- prediction(y.probs, df.test$target)
perf <- performance(pred, "tpr", "fpr")

#AUC
auc <- performance(pred, "auc")
auc <- auc@y.values[[1]]

plot(perf, colorize=TRUE, main=auc)


#----------------------------------------------------------------------------
#FEATURE SELECTION

model.SVM <- svm(target ~ c + e, df, kernel="radial", cost=1, gamma=1/9)
y.pred <- predict(model.SVM, df.test)
MR(y.pred, df.test$target)
Acc(y.pred, df.test$target)

model.SVM <- svm(target ~ g1 + i, df, kernel="radial", cost=1, gamma=1/9)
y.pred <- predict(model.SVM, df.test)
MR(y.pred, df.test$target)
Acc(y.pred, df.test$target)



