library(leaps)
library(glmnet)

# Ustawiamy sciezke
setwd('C:/Users/Arek/Desktop/Studia_II_stopien/RMiSW/SW')


# Przygotowanie danych
# Life expectancy
lf <- read.csv('life_expectancy.csv')
lf <- na.omit(lf)
lf <- subset(lf, select = -c(Country, Year))
lf$Status <- factor(lf$Status)

# Titanic
titanic <- read.csv('titanic.csv')

# Usuwamy wartosci zakodowane jako NA
titanic <- na.omit(titanic)
titanic <- titanic[titanic$Age <= 3,]
titanic$Survived <- as.factor(titanic$Survived)

################################################################################
# Regresja
################################################################################

# Selekcja cech
# Wybór najlepszego podzbioru
life_exp_bs <- regsubsets(Life.expectancy ~ ., 
                          data = lf, 
                          nvmax = ncol(life_exp_bs),
                          really.big = TRUE)

life_exp_bs_sum <- summary(life_exp_bs)
life_exp_bs

# (a) poprzez Cp
lf_cp_min <-  which.min(life_exp_bs_sum$cp)
lf_cp_min
lf_model_cp <- life_exp_bs_sum$which[lf_cp_min, -1]
lf_predictors_cp <- names(which(lf_model_cp == TRUE))
lf_predictors_cp

# (b) poprzez BIC
lf_bic_min <-  which.min(life_exp_bs_sum$bic)
lf_bic_min
lf_model_bic <- life_exp_bs_sum$which[lf_bic_min, -1]
lf_predictors_bic <- names(which(lf_model_bic == TRUE))
lf_predictors_bic

# (c) poprzez R^2
lf_r_squared_max <- which.max(life_exp_bs_sum$adjr2)
lf_model_r_squared <- life_exp_bs_sum$which[lf_r_squared_max, -1]
lf_predictors_r_squared <- names(which(lf_model_r_squared == TRUE))
lf_predictors_r_squared

# Selekcja krokowa do przodu
lf_fwd <- regsubsets(Life.expectancy ~ ., data = lf, method = "forward", 
                     nvmax = 19)
lf_fwd_sum <- summary(lf_fwd)

# (a) poprzez Cp
lf_fwd_cp_min <-  which.min(lf_fwd_sum$cp)
lf_fwd_model_cp <- lf_fwd_sum$which[lf_fwd_cp_min, -1]
lf_fwd_predictors_cp <- names(which(lf_fwd_model_cp == TRUE))
lf_fwd_predictors_cp

# (b) poprzez BIC
lf_fwd_bic_min <-  which.min(lf_fwd_sum$bic)
lf_fwd_model_bic <- lf_fwd_sum$which[lf_fwd_bic_min, -1]
lf_fwd_predictors_bic <- names(which(lf_fwd_model_bic == TRUE))
lf_fwd_predictors_bic

# (c) poprzez R^2
lf_fwd_r_squared_max <-  which.max(lf_fwd_sum$adjr2)
lf_fwd_model_r_squared <- lf_fwd_sum$which[lf_fwd_r_squared_max, -1]
lf_fwd_predictors_r_squared <- names(which(lf_fwd_model_r_squared == TRUE))
lf_fwd_predictors_r_squared


# Selekcja krokowa wstecz
lf_back <- regsubsets(Life.expectancy ~ ., data = lf, nvmax = 19, 
                      method = "backward")
lf_back_sum <- summary(lf_back)

# (a) poprzez Cp
lf_back_cp_min <-  which.min(lf_back_sum$cp)
lf_back_model_cp <- lf_back_sum$which[lf_back_cp_min, -1]
lf_back_predictors_cp <- names(which(lf_back_model_cp == TRUE))
lf_back_predictors_cp

# (b) poprzez BIC
lf_back_bic_min <-  which.min(lf_back_sum$bic)
lf_back_model_bic <- lf_back_sum$which[lf_back_bic_min, -1]
lf_back_predictors_bic <- names(which(lf_back_model_bic == TRUE))
lf_back_predictors_bic

# (c) poprzez R^2
lf_back_r_squared_max <-  which.max(lf_back_sum$adjr2)
lf_back_model_r_squared <- lf_back_sum$which[lf_back_r_squared_max, -1]
lf_back_predictors_r_squared <- names(which(lf_back_model_r_squared == TRUE))
lf_back_predictors_r_squared

################################################################################
# Klasyfikacja
################################################################################

# Selekcja cech
# Wybór najlepszego podzbioru
titanic_bs <- regsubsets(Survived ~ ., 
                         data = titanic, 
                         nvmax = 19,
                         really.big = TRUE)

titanic_bs_sum <- summary(titanic_bs)

# (a) poprzez Cp
titanic_cp_min <-  which.min(titanic_bs_sum$cp)
titanic_cp_min
titanic_model_cp <- titanic_bs_sum$which[titanic_cp_min, -1]
titanic_predictors_cp <- names(which(titanic_model_cp == TRUE))
titanic_predictors_cp

# (b) poprzez BIC
titanic_bic_min <-  which.min(titanic_bs_sum$bic)
titanic_bic_min
titanic_model_bic <- titanic_bs_sum$which[titanic_bic_min, -1]
titanic_predictors_bic <- names(which(titanic_model_bic == TRUE))
titanic_predictors_bic

# (c) poprzez R^2
titanic_r_squared_max <- which.max(titanic_bs_sum$adjr2)
titanic_model_r_squared <- titanic_bs_sum$which[titanic_r_squared_max, -1]
titanic_predictors_r_squared <- names(which(titanic_model_r_squared == TRUE))
titanic_predictors_r_squared

# Selekcja krokowa do przodu
titanic_fwd <- regsubsets(Survived ~ ., 
                          data = titanic, 
                          nvmax = 19,
                          method = 'forward')
titanic_fwd_sum <- summary(titanic_fwd)

# (a) poprzez Cp
titanic_fwd_cp_min <-  which.min(titanic_fwd_sum$cp)
titanic_fwd_model_cp <- titanic_fwd_sum$which[titanic_fwd_cp_min, -1]
titanic_fwd_predictors_cp <- names(which(titanic_fwd_model_cp == TRUE))
titanic_fwd_predictors_cp

# (b) poprzez BIC
titanic_fwd_bic_min <-  which.min(titanic_fwd_sum$bic)
titanic_fwd_model_bic <- titanic_fwd_sum$which[titanic_fwd_bic_min, -1]
titanic_fwd_predictors_bic <- names(which(titanic_fwd_model_bic == TRUE))
titanic_fwd_predictors_bic

# (c) poprzez R^2
titanic_fwd_r_squared_max <-  which.max(titanic_fwd_sum$adjr2)
titanic_fwd_model_r_squared <- titanic_fwd_sum$which[titanic_fwd_r_squared_max, -1]
titanic_fwd_predictors_r_squared <- names(which(titanic_fwd_model_r_squared == TRUE))
titanic_fwd_predictors_r_squared


# Selekcja krokowa wstecz
titanic_back <- regsubsets(Survived ~ ., 
                           data = titanic, 
                           nvmax = 19, 
                           method = "backward")
titanic_back_sum <- summary(titanic_back)

# (a) poprzez Cp
titanic_back_cp_min <-  which.min(titanic_back_sum$cp)
titanic_back_model_cp <- titanic_back_sum$which[titanic_back_cp_min, -1]
titanic_back_predictors_cp <- names(which(titanic_back_model_cp == TRUE))
titanic_back_predictors_cp

# (b) poprzez BIC
titanic_back_bic_min <-  which.min(titanic_back_sum$bic)
titanic_back_model_bic <- titanic_back_sum$which[titanic_back_bic_min, -1]
titanic_back_predictors_bic <- names(which(titanic_back_model_bic == TRUE))
titanic_back_predictors_bic

# (c) poprzez R^2
titanic_back_r_squared_max <-  which.max(titanic_back_sum$adjr2)
titanic_back_model_r_squared <- titanic_back_sum$which[titanic_back_r_squared_max, -1]
titanic_back_predictors_r_squared <- names(which(titanic_back_model_r_squared == TRUE))
titanic_back_predictors_r_squared

################################################################################
# Lasso
################################################################################

# Regresja
lf_data <- model.matrix(Life.expectancy ~ ., data = lf)[, -1]
lf_target <- lf$Life.expectancy

lf_fit_lasso <- glmnet(lf_data, lf_target, alpha = 1)
plot(lf_fit_lasso, xvar = "lambda")

lf_cv_out <- cv.glmnet(lf_data, lf_target, alpha = 1)
plot(lf_cv_out)
lf_cv_out$lambda.min

# Najlepsza wedlug nas lambda = exp(-2)
lf_pred_lasso <- predict(lf_fit_lasso, s = exp(-2),
                         type = 'coefficients')

lf_pred_lasso

# Klasyfikacja
titanic_data <- model.matrix(Survived ~ ., data = titanic)[, -1]
titanic_target <- titanic$Survived

titanic_fit_lasso <- glmnet(titanic_data, titanic_target, alpha = 1, 
                            family = 'binomial')
plot(titanic_fit_lasso, xvar = "lambda")

titanic_cv_out <- cv.glmnet(titanic_data, titanic_target, alpha = 1,
                            family = 'binomial')
plot(titanic_cv_out)
titanic_cv_out$lambda.min

# Najlepsza wedlug nas lambda = exp(-4)
titanic_pred_lasso <- predict(titanic_fit_lasso, s = exp(-4),
                         type = 'coefficients')

titanic_pred_lasso

