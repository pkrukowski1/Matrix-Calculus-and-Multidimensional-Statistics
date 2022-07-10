# Import bibliotek
library(MASS)
library(class)
library(caret)

# Przygotowanie danych
setwd('C:/Users/Arek/Desktop/Studia_II_stopien/RMiSW/SW')
df <- read.csv('titanic.csv')
View(df)

# Usuwamy wartosci zakodowane jako NA
df <- subset(df, select = -c(X))
df <- na.omit(df)
df <- df[df$Age <= 3,]
df$Survived <- as.factor(df$Survived)

# Dzielimy dane na zbiór testowy i treningowy
smp_size <- floor(0.75 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

# Dopasowanie modelu regresji logistycznej, istnieja istotne zmienne
dir_logistic <- list()
dir_logistic$fit <- glm(Survived ~ ., family = binomial, data = train)
summary(dir_logistic$fit)

dir_logistic$fit <- glm(Survived ~ . -Parch -Fare -Embarked -IsAlone -Title, 
                        family = binomial, data = train)

# Residual deviance jest wieksze ni¿ null deviance, wiec model z 4 predyktorami
# jest lepszy, ni¿ model staly
summary(dir_logistic$fit)

# Skutecznosc regresji logistycznej
dir_logistic$probs <- predict(dir_logistic$fit, test, type = "response")
head(dir_logistic$probs)

contrasts(factor(df$Survived))
dir_logistic$predicted <- ifelse(dir_logistic$probs > 0.5, 1, 0)

# Skutecznosc modelu
table(dir_logistic$predicted, test$Survived)
mean(dir_logistic$predicted == test$Survived)

# Dopasowanie LDA
dir_lda <- list()
dir_lda$fit <- lda(Survived ~ . -Parch -Fare -Embarked -IsAlone -Title, 
                   family = binomial, data = train)
dir_lda$fit

# LDA - predykcja + skutecznosc
dir_lda$predicted <- predict(dir_lda$fit, test)
table(dir_lda$predicted$class, test$Survived)
mean(dir_lda$predicted$class == test$Survived)

# Dopasowanie QDA
dir_qda <- list()
dir_qda$fit <- qda(Survived ~ . -Parch -Fare -Embarked -IsAlone -Title, 
                   family = binomial, data = train)
dir_qda$fit

# QDA - predykcja + skutecznosc
dir_qda$predicted <- predict(dir_qda$fit, test)
table(dir_qda$predicted$class, test$Survived)
mean(dir_qda$predicted$class == test$Survived)


# Dopasowanie modelu kNN
tr_control <- trainControl(method  = "cv", number  = 10)

knn_fit <- train(Survived ~ . -Parch -Fare -Embarked -IsAlone -Title,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:30),
             trControl  = tr_control,
             metric     = "Accuracy",
             data       = train)

qplot(knn_fit$results$k, knn_fit$results$Accuracy,geom = "line",
      xlab = "k", ylab = "Accuracy", main = 'Wybór optymalnej liczby s¹siadów')
best_k <- as.integer(knn_fit$bestTune)

# Accuracy i macierz pomylek dla kNN
dir_knn <- knn(train, test, train$Survived, k = best_k)
table(dir_knn, test$Survived)
mean(dir_knn == test$Survived)
