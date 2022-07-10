library(tree)
library(randomForest)
library(xgboost)
library(ggplot2)
library(gbm)
library(caret)
library(dplyr)

# urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
# install.packages(urlPackage, repos=NULL, type="source") 

################################################################################
# REGRESJA
################################################################################

# Ustawiamy sciezke
setwd('C:/Users/Arek/Desktop/Studia_II_stopien/RMiSW/SW')


# Przygotowanie danych
# Life expectancy
lf <- read.csv('life_expectancy.csv')
lf <- na.omit(lf)
lf <- subset(lf, select = -c(Country, Year))
lf$Status <- factor(lf$Status)

# Drzewo decyzyjne (regresyjne)
lf_tree <- tree(Life.expectancy ~ ., data = lf)
summary(lf_tree)

# Istotne predyktory:  "Income.composition.of.resources", "HIV.AIDS",
# "Adult.Mortality", "thinness..1.19.years"


# Wizualizacja drzewa
plot(lf_tree)
text(lf_tree)

# Estymata bledu testowego (MSE)
set.seed(1)
lf_train <- sample(nrow(lf), nrow(lf) * 0.75)
lf_test <- -lf_train
lf_train_tree <- tree(Life.expectancy ~ Income.composition.of.resources +
                        HIV.AIDS + 
                        Adult.Mortality +
                        thinness..1.19.years, data = lf, subset = lf_train)

lf_pred <- predict(lf_train_tree, newdata = lf[lf_test,])
mean((lf_pred - lf$Life.expectancy[lf_test])^2)

# Wyznaczamy optymalne poddrzewo metodą przycinania sterowanego złożonością.
lf_cv <- cv.tree(lf_train_tree)
plot(lf_cv$size, lf_cv$dev, type = "b", xlab = "Tree size",
     ylab = "Deviance")

# Przyciete drzewo
lf_train_pruned <- prune.tree(lf_train_tree, best = 5)
plot(lf_train_pruned)
text(lf_train_pruned)

# Estymata bledu testowego (MSE) dla przycietego drzewa
lf_pred_pruned <- predict(lf_train_pruned, newdata = lf[lf_test,])
mean((lf_pred_pruned - lf$Life.expectancy[lf_test])^2)

# Lasy losowe
# Ocena istotnosci predyktorów
set.seed(2)
lf_rf_train <- randomForest(Life.expectancy ~ ., data = lf, subset = lf_train,
                            importance = TRUE)
lf_imp_data <- as.data.frame(importance(lf_rf_train))

lf_imp_data$Var.Names <- row.names(lf_imp_data)

ggplot(lf_imp_data, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

# Wybieramy istotne predyktory - Income.composition.of.resources, HIV.AIDS,
# Adult.Mortality, Total.expenditure

# Dopasowanie nowego modelu
lf_rf_train <- randomForest(Life.expectancy ~ Income.composition.of.resources +
                              HIV.AIDS +
                              Adult.Mortality +
                              Total.expenditure,
                            data = lf, subset = lf_train,
                            importance = TRUE,
                            ntree = 500)

lf_rf_pred <- predict(lf_rf_train, newdata = lf[lf_test,])
mean((lf_rf_pred - lf$Life.expectancy[lf_test])^2)


# OOB plot
plot(lf_rf_train, type = "l")

# Boosting
lf_boost_train <- gbm(Life.expectancy ~ ., data = lf[lf_train,],
                      distribution = "gaussian", 
                      n.trees = 5000, 
                      interaction.depth = 4)
summary(lf_boost_train)

# Istotne predyktory to Income.composition.of.resources, HIV.AIDS, Adult.Mortality

lf_new_boost_train <- gbm(Life.expectancy ~ Income.composition.of.resources +
                        HIV.AIDS +
                        Adult.Mortality,
                      data = lf[lf_train,],
                      distribution = "gaussian", 
                      n.trees = 5000, 
                      interaction.depth = 4)

lf_new_pred_boost <- predict(lf_new_boost_train, newdata = lf[lf_test,], n.trees = 5000)



# MSE
mean((lf_new_pred_boost - lf$Life.expectancy[lf_test])^2)

################################################################################
# KLASYFIKACJA
################################################################################

# Przygotowanie danych
titanic <- read.csv('titanic.csv')

# Usuwamy wartosci zakodowane jako NA
titanic <- na.omit(titanic)
titanic <- titanic[titanic$Age <= 3,]
titanic$X <- NULL
View(titanic)
titanic$Survived <- as.factor(titanic$Survived)

# Drzewo decyzyjne (klasyfikacyjne)
ti_tree <- tree(Survived ~ ., data = titanic)
summary(ti_tree)

# Istotne predyktory:  "Title", "Pclass", "Has_Cabin", "FamilySize"


# Wizualizacja drzewa
plot(ti_tree)
text(ti_tree)

# Accuracy
set.seed(1)
ti_train <- sample(nrow(titanic), nrow(titanic) * 0.75)
ti_test <- -ti_train
ti_train_tree <- tree(Survived ~ Title +
                        Pclass + 
                        Has_Cabin +
                        FamilySize,
                      data = titanic,
                      subset = ti_train)

ti_pred <- predict(ti_train_tree, newdata = titanic[ti_test,], type = 'class')
summary(ti_pred)

table(ti_pred, titanic$Survived[ti_test])

# Accuracy
mean(ti_pred == titanic$Survived[ti_test])

survived_cv <- cv.tree(ti_train_tree, FUN = prune.misclass)
survived_cv
plot(survived_cv$size, survived_cv$dev, type = "b",
     main = 'Deviance', xlab = 'Size', ylab = 'Deviance')

# Przycinamy drzewo do poddrzewa z najmniejszym poziomem bledow CV

ti_size_opt <- survived_cv$size[which.min(survived_cv$dev)]
survived_pruned <- prune.misclass(ti_train_tree, best = 6)
plot(survived_pruned)
text(survived_pruned, pretty = 0)

# Estymata bledu testowego dla przycietego drzewa, przyciecie drzewa daje takie
# same rezultaty co przed przycieciem
ti_pruned_class <- predict(survived_pruned, newdata = titanic[ti_test,], 
                        type = "class")
table(ti_pruned_class, titanic$Survived[ti_test])

# Accuracy
mean(ti_pruned_class == titanic$Survived[ti_test])

# Lasy losowe
# Ocena istotnosci predyktorów
ti_rf_train <- randomForest(Survived ~ ., data = titanic, subset = ti_train,
                            importance = TRUE)
ti_imp_data <- as.data.frame(importance(ti_rf_train))


# Najbardziej istotne sa Pclass, Title, Sex, FamilySize, Fare
varImpPlot(ti_rf_train)

# Dopasowanie nowego drzewa
ti_rf_new_train <- randomForest(Survived ~ Pclass + Title + 
                              FamilySize +
                              Fare,
                              data = titanic, subset = ti_train,
                            importance = TRUE,
                            ntree = 500)

ti_rf_new_pred <- predict(ti_rf_new_train, newdata = titanic[ti_test,])
mean(ti_rf_new_pred == titanic$Survived[ti_test])

# Boosting
titanic$Survived <- as.vector(droplevels(titanic$Survived))
ti_boost_train <- gbm(Survived ~ ., data = titanic[ti_train,],
                      distribution = "bernoulli", 
                      n.trees = 5000, 
                      interaction.depth = 4)

summary(ti_boost_train)

# Istotne predyktory to Fare, Title, Age, Embarked, Pclass
ti_train_data <- titanic[ti_train,]
ti_test_data <- titanic[ti_test,]
ti_train_data$Survived2 <- ifelse(ti_train_data$Survived==1,'yes','no')
ti_test_data$Survived2 <- ifelse(ti_test_data$Survived == 1, 'yes', 'no')


objControl <- trainControl(method='cv', number=10, returnResamp='none',
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE)

ti_new_boost_train <- train(ti_train_data[,c('Fare', 'Title', 'Embarked', 'Pclass')],
                  ti_train_data[,'Survived2'], 
                  method='gbm',
                  trControl = objControl,
                  metric = "ROC",
                  preProc = c("center", "scale"))

ti_new_pred_boost <- predict(ti_new_boost_train, 
                             newdata = titanic[ti_test,], 
                             n.trees = 5000,
                             type = 'raw')
mean(ti_new_pred_boost == ti_test_data$Survived2)
