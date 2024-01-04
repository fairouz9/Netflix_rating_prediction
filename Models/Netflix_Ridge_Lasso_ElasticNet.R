###############################
# RIDGE ~ LASSO ~ ELASTIC_NET #
###############################

rm(list = ls())

### Import libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(glmnet)

### Load the data
user = 'Mattia'
if (user == 'Mattia'){
  setwd("/Users/mattiapiazza/Documents/University/Statistical Methods for High Dimensional Data/Project/Dataset")
}
if (user == 'Theresa'){
  setwd("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Padova/Statistical Methods for High-Dim Data/Project/Data")
}
if (user == 'Fairouz'){
  setwd("C:/Users/fairouz/Desktop/statistical method project/dataset")
}

data_movie = read.csv("data_movie.csv", header=TRUE)
data_show = read.csv('data_show.csv', header = TRUE)

data_movie_num = read.csv("data_movie_num.csv", header=TRUE)
data_show_num = read.csv('data_show_num.csv', header = TRUE)


### MOVIES



########################################
##          Train Test Split          ##
########################################


set.seed(1234)
random <- sample(1:nrow(data_movie), ceiling(0.8*dim(data_movie)[1]))

train_movie <- data_movie[random,] 
test_movie <- data_movie[-random,]


### Train

data_train_movie <- train_movie %>% select(-c(X,title, type, genres,
                                          production_countries, age_certification))
data_train_movie<- na.omit(data_train_movie)

## All Features
x0 <- model.matrix(~., data = select(data_train_movie, -c('imdb_score')) )

## Group
x1 <- model.matrix(~.*., data = select(data_train_movie, -c('imdb_score')))

## Best Correlation
cor_imdb_score_movie <- as.data.frame(as.matrix(cor(data_movie_num)[,'imdb_score']))
cor_imdb_score_movie <- cor_imdb_score_movie %>% filter(!rownames(.) %in% c('X', 'imdb_score'))

fname <- rownames(cor_imdb_score_movie)
mask <- abs(cor_imdb_score_movie) >= colMeans(abs(cor_imdb_score_movie))

best_formula <- na.omit(as.formula(paste("~", paste(fname[mask], collapse = ' + ')) ))


x2 <- model.matrix(best_formula, data = select(data_train_movie, -c('imdb_score')))

y <- data_train_movie$imdb_score 

### Test
data_test_movie <- select(test_movie,-c(X,title, type, genres,
                                        production_countries, age_certification))
data_test_movie <- na.omit(data_test_movie)


x0_test <- model.matrix(~., data = select(data_test_movie, -c(imdb_score)))
x1_test <- model.matrix(~.*., data = select(data_test_movie, -c(imdb_score)))
x2_test <- model.matrix(best_formula, data = data_test_movie)

y_test <- data_test_movie$imdb_score 



####################
##     Ridge      ##
####################



## All Features


ridge_mdl_movie_0 <- cv.glmnet(x0, na.omit(y), alpha = 0)
plot(ridge_mdl_movie_0)
summary(ridge_mdl_movie_0$glmnet.fit$beta)

# Prediction
ridge_pred_movie_0 <- predict(ridge_mdl_movie_0, s = 'lambda.min', newx = x0_test)

# MSE
mse_ridge_movie_0 <- mean((ridge_pred_movie_0 - y_test)^2)
mse_ridge_movie_0



## Group


ridge_mdl_movie_1 <- cv.glmnet(x1, na.omit(y), alpha = 0)
plot(ridge_mdl_movie_1)
summary(ridge_mdl_movie_1$glmnet.fit$beta)

# Prediction
ridge_pred_movie_1 <- predict(ridge_mdl_movie_1, s = 'lambda.min', newx = x1_test)

# MSE
mse_ridge_movie_1 <- mean((ridge_pred_movie_1 - y_test)^2)
mse_ridge_movie_1



## Best Correlation


ridge_mdl_movie_2 <- cv.glmnet(x2, na.omit(y), alpha = 0)
plot(ridge_mdl_movie_2)
summary(ridge_mdl_movie_2$glmnet.fit$beta)

# Prediction
ridge_pred_movie_2 <- predict(ridge_mdl_movie_2, s = 'lambda.min', newx = x2_test)

# MSE
mse_ridge_movie_2 <- mean((ridge_pred_movie_2 - y_test)^2)
mse_ridge_movie_2


mse_ridge_movie <- c(mse_ridge_movie_0, mse_ridge_movie_1, mse_ridge_movie_2)





####################
##     Lasso      ##
####################



## All Features


lasso_mdl_movie_0 <- cv.glmnet(x0, na.omit(y), alpha = 1)
plot(lasso_mdl_movie_0)
summary(lasso_mdl_movie_0$glmnet.fit$beta)

# Prediction
lasso_pred_movie_0 <- predict(lasso_mdl_movie_0, s = 'lambda.min', newx = x0_test)

# MSE
mse_lasso_movie_0 <- mean((lasso_pred_movie_0 - y_test)^2)
mse_lasso_movie_0



## Group


lasso_mdl_movie_1 <- cv.glmnet(x1, na.omit(y), alpha = 1)
plot(lasso_mdl_movie_1)
summary(lasso_mdl_movie_1$glmnet.fit$beta)

# Prediction
lasso_pred_movie_1 <- predict(lasso_mdl_movie_1, s = 'lambda.min', newx = x1_test)

# MSE
mse_lasso_movie_1 <- mean((lasso_pred_movie_1 - y_test)^2)
mse_lasso_movie_1



## Best Correlation


lasso_mdl_movie_2 <- cv.glmnet(x2, na.omit(y), alpha = 1)
plot(lasso_mdl_movie_2)
summary(lasso_mdl_movie_2$glmnet.fit$beta)

# Prediction
lasso_pred_movie_2 <- predict(lasso_mdl_movie_2, s = 'lambda.min', newx = x2_test)

# MSE
mse_lasso_movie_2 <- mean((lasso_pred_movie_2 - y_test)^2)
mse_lasso_movie_2

mse_lasso_movie <- c(mse_lasso_movie_0, mse_lasso_movie_1, mse_lasso_movie_2)





####################
##  Elastic Net   ##
####################



## All Features


it <- 500
n <- 1000
mse_en_movie_0 <- data.frame( mse = rep(0, it),
                            alpha = rep(0, it))
i <- 1

for (alpha in sample(0:n, it)){
  en_mdl_movie_0 <- cv.glmnet(x0, na.omit(y), alpha = alpha*(1/n))
  
  # Prediction
  en_pred_movie_0 <- predict(en_mdl_movie_0, s = 'lambda.min', newx = x0_test)
  
  # MSE
  mse_en_movie_0[i, 'mse'] <- mean((en_pred_movie_0 - y_test)^2)
  mse_en_movie_0[i, 'alpha'] <- alpha*(1/n)
  i <- i + 1
}

mse_en_movie_0 <- mse_en_movie_0[mse_en_movie_0[, 'mse'] == min(mse_en_movie_0[, 'mse']),]



## Group


it <- 500
n <- 1000
mse_en_movie_1 <- data.frame( mse = rep(0, it),
                              alpha = rep(0, it))
i <- 1

for (alpha in sample(0:n, it)){
  en_mdl_movie_1 <- cv.glmnet(x1, na.omit(y), alpha = alpha*(1/n))
  
  # Prediction
  en_pred_movie_1 <- predict(en_mdl_movie_1, s = 'lambda.min', newx = x1_test)
  
  # MSE
  mse_en_movie_1[i, 'mse'] <- mean((en_pred_movie_1 - y_test)^2)
  mse_en_movie_1[i, 'alpha'] <- alpha*(1/n)
  i <- i + 1
}

mse_en_movie_1 <- mse_en_movie_1[mse_en_movie_1[, 'mse'] == min(mse_en_movie_1[, 'mse']),]



## Best Correlation


it <- 500
n <- 1000
mse_en_movie_2 <- data.frame( mse = rep(0, it),
                              alpha = rep(0, it))
i <- 1

for (alpha in sample(0:n, it)){
  en_mdl_movie_2 <- cv.glmnet(x2, na.omit(y), alpha = alpha*(1/n))
  
  # Prediction
  en_pred_movie_2 <- predict(en_mdl_movie_2, s = 'lambda.min', newx = x2_test)
  
  # MSE
  mse_en_movie_2[i, 'mse'] <- mean((en_pred_movie_2 - y_test)^2)
  mse_en_movie_2[i, 'alpha'] <- alpha*(1/n)
  i <- i + 1
}

mse_en_movie_2 <- mse_en_movie_2[mse_en_movie_2[, 'mse'] == min(mse_en_movie_2[, 'mse']),]


mse_en_movie <- c(mse_en_movie_0[,'mse'], mse_en_movie_1[,'mse'], mse_en_movie_2[,'mse'])
alpha_en_movie <- c(mse_en_movie_0[,'alpha'], mse_en_movie_1[,'alpha'], mse_en_movie_2[,'alpha'])



##############################
##      Visualization       ##
##############################



mse_movie <- data.frame( model = c('Ridge_0', 'Ridge_1', 'Ridge_2',
                                   'Lasso_0', 'Lasso_1', 'Lasso_2',
                                   'ElasticNet_0', 'ElasticNet_1', 'ElasticNet_2'),
                         mse = c(mse_ridge_movie, mse_lasso_movie, mse_en_movie),
                         alpha = c(0, 0, 0, 1, 1, 1, alpha_en_movie) )

## MSE 

ggplot(mse_movie, aes(x = mse_movie[, 'model'], y = mse_movie[, 'mse']) ) +
  geom_col() +
  coord_cartesian(ylim = c(0.85, 1)) +
  labs(
    title = 'MSE',
    x = 'Models',
    y = 'MSE'
  )

mask <- test_movie$title == 'Rambo'

beat_the_model <- data.frame(name = c('Ridge_0', 'Ridge_1', 'Ridge_2',
                                      'Lasso_0', 'Lasso_1', 'Lasso_2',
                                      'ElasticNet_0', 'ElasticNet_1', 'ElasticNet_2', 
                                      'IMDB Score'),
                             pred = c(ridge_pred_movie_0[mask], ridge_pred_movie_1[mask], ridge_pred_movie_2[mask], 
                                      lasso_pred_movie_0[mask], lasso_pred_movie_1[mask], lasso_pred_movie_2[mask], 
                                      en_pred_movie_0[mask], en_pred_movie_1[mask], en_pred_movie_1[mask], 
                                      y_test[mask]) )


ggplot(beat_the_model, aes(x = name, y = pred)) +
  geom_col(color = 'black', fill = 'purple')



