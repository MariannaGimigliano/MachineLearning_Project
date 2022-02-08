#STUDIO STATISTICO SUI RISULTATI DELLA VALUTAZIONE 

df.SRS <- data.frame(MR.polynomial=double(), Acc.polynomial=double(), MR.radial=double(), Acc.radial=double(), MR.final=double())
df.descr <- data.frame(Media=double(), Varianza, Dev_Standard)

for (n in 1:10) {
  
  df <- read.csv("ionosphere_data.csv")
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
  
  df$a <- NULL
  df$b <- NULL
  
  N <- nrow(df)
  N.train <- 225
  N.test <- 60
  N.val <- 65
  train.sample <- sample(N, N.train)
  df.train <- df[train.sample, ]
  df.test <- df[-train.sample, ]
  val.sample <- sample(N.test + N.val, N.val) 
  df.val <- df.test[val.sample, ]
  df.test <- df.test[-val.sample, ]
  df <- df.train 
  N <- nrow(df)
  
  library(e1071)
  MR <- function(y.pred, y.true) {
    res <- mean(y.pred != y.true)
    return(res)
  }
  Acc <- function(y.pred, y.true) {
    res <- 1 - mean(y.pred != y.true)
    return(res)
  }
  
  model.SVM <- svm(target ~ ., df, kernel="polynomial", cost=10, degree=2)
  y.pred <- predict(model.SVM, df)
  MR.poly <- MR(y.pred, df$target)
  MR.poly 
  
  y.pred <- predict(model.SVM, df.test)
  MR.polynomial <- MR(y.pred, df.test$target)
  MR.polynomial 
  
  Acc.polynomial <- Acc(y.pred, df.test$target)
  Acc.polynomial 
  

  model.SVM <- svm(target ~ ., df, kernel="radial", cost=1, gamma=1/9)
  y.pred <- predict(model.SVM, df)
  MR.radial <- MR(y.pred, df$target)
  MR.radial 
  
  y.pred <- predict(model.SVM, df.test)
  MR.radial <- MR(y.pred, df.test$target)
  MR.radial
  
  Acc.radial <- Acc(y.pred, df.test$target)
  Acc.radial
 
  model.SVM <- svm(target ~ ., df, kernel="radial", cost=1, gamma=1/9)
  y.pred <- predict(model.SVM, df.test)
  MR.final <- MR(y.pred, df.test$target)
  MR.final 
  
  #----------------------------------------------------------------------------
  #Visualizzazione in un DataSet della variazione delle metriche di errore
  #per n-iterazioni
  n <- data.frame(MR_Polynomial=c(MR.polynomial), Acc_Polynomial=c(Acc.polynomial), MR_Radial=c(MR.radial), Acc_Radial=c(Acc.radial), MR_Final=c(MR.final))
  df.SRS <- rbind(df.SRS, n)
  
}


#----------------------------------------------------------------------------
#STATISTICA DESCRITTIVA
#Analisi delle misure del centro e diffusione dei dati

Media1 <- mean(df$c)
Varianza1 <- var(df$c)
Dev_Standard1 <- sd(df$c)

Media2 <- mean(df$e)
Varianza2 <- var(df$e)
Dev_Standard2 <- sd(df$e)

Media3 <- mean(df$x)
Varianza3 <- var(df$x)
Dev_Standard3 <- sd(df$x)

Media4 <- mean(df$f)
Varianza4 <- var(df$f)
Dev_Standard4 <- sd(df$f)

df.descr <- data.frame(Media=c(Media1, Media2, Media3, Media4), 
                       Varianza=c(Varianza1, Varianza2, Varianza3, Varianza4), 
                       Dev_Standard=c(Dev_Standard1, Dev_Standard2, Dev_Standard3, Dev_Standard4))


#STATISTICA INFERENZIALE
#Analisi degli intervalli di confidenza per la media
n <- 350  
alpha <- 0.05
z_alpha <-  qnorm(1-alpha/2, 0, 1)
int1 <- Media1 - z_alpha * Dev_Standard1 /sqrt(n)
int2 <- Media1 + z_alpha * Dev_Standard1 /sqrt(n)
int1
int2
Media1

n <- 350  
alpha <- 0.05
z_alpha <-  qnorm(1-alpha/2, 0, 1)
int1 <- Media2 - z_alpha * Dev_Standard2 /sqrt(n)
int2 <- Media2 + z_alpha * Dev_Standard2 /sqrt(n)
int1
int2
Media2

n <- 350  
alpha <- 0.05
z_alpha <-  qnorm(1-alpha/2, 0, 1)
int1 <- Media3 - z_alpha * Dev_Standard3 /sqrt(n)
int2 <- Media3 + z_alpha * Dev_Standard3 /sqrt(n)
int1
int2
Media3

n <- 350  
alpha <- 0.05
z_alpha <-  qnorm(1-alpha/2, 0, 1)
int1 <- Media4 - z_alpha * Dev_Standard4 /sqrt(n)
int2 <- Media4 + z_alpha * Dev_Standard4 /sqrt(n)
int1
int2
Media4


                     
                     
                     