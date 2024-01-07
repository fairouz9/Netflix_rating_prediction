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
  setwd("/Users/mattiapiazza/Documents/University/Statistical Methods for High Dimensional Data/Project")
}
if (user == 'Theresa'){
  setwd("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Padova/Statistical Methods for High-Dim Data/Project/Data")
}
if (user == 'Fairouz'){
  setwd("C:/Users/fairouz/Desktop/statistical method project/dataset")
}

data_movie <- na.omit(read.csv("csv/data_movie.csv", header=TRUE) %>% select(-c('X')) )
data_show <- na.omit(read.csv('csv/data_show.csv', header = TRUE) %>% select(-c('X')) )

data_movie_scaled <- read.csv("csv/data_movie_scaled.csv", header=TRUE) %>% select(-c('X')) 
data_show_scaled <- read.csv("csv/data_show_scaled.csv", header=TRUE) %>% select(-c('X'))


data_movie_num <- data_movie %>% select(-c("genres", "production_countries", "title",
                                           "type")) %>%
  mutate(age_certification = as.factor(age_certification)) 

data_show_num <- data_show %>% select(-c("genres", "production_countries", "title",
                                         "type")) %>%
  mutate(age_certification = as.factor(age_certification))


### MOVIES



########################################
##          Train Test Split          ##
########################################


set.seed(1234)
random <- sample(1:nrow(data_movie), ceiling(0.8*dim(data_movie)[1]))

train_movie <- data_movie_num[random,]
test_movie <- data_movie_num[-random,]

train_movie_scaled <- data_movie_scaled[random,] 
train_movie_scaled <- select(train_movie_scaled, -c('title'))

test_movie_scaled <- data_movie_scaled[-random,]
test_movie_scaled <- select(test_movie_scaled, -c('title'))



### Train


## All Features
x0 <- model.matrix(imdb_score~., data = train_movie)

## Group
x1 <- model.matrix(imdb_score~.*., data = train_movie)

## Best Correlation
cor_imdb_score_movie <- as.data.frame(as.matrix(cor(select(data_movie_num, -c('age_certification')))[,'imdb_score']))
cor_imdb_score_movie <- cor_imdb_score_movie %>% filter(!rownames(.) %in% c('imdb_score'))

fname <- rownames(cor_imdb_score_movie)
mask <- abs(cor_imdb_score_movie) >= colMeans(abs(cor_imdb_score_movie))

best_formula <- na.omit(as.formula(paste("imdb_score~", paste(fname[mask], collapse = ' + ')) ))



x2 <- model.matrix(best_formula, data = train_movie)

y <- train_movie$imdb_score 


### Test


x0_test <- model.matrix(imdb_score~., data = test_movie)
x1_test <- model.matrix(imdb_score~.*., data = test_movie)
x2_test <- model.matrix(best_formula, data = test_movie)

y_test <- test_movie$imdb_score



### Train ~ Scaled


## All Features
x0_scaled <- model.matrix(imdb_score~., data = train_movie_scaled)

## Group
x1_scaled <- model.matrix(imdb_score~.*., data = train_movie_scaled)

## Best Correlation
cor_imdb_score_movie_scaled <- as.data.frame(as.matrix(cor(select(data_movie_num, -c('age_certification')))[,'imdb_score']))
cor_imdb_score_movie_scaled <- cor_imdb_score_movie %>% filter(!rownames(.) %in% c('imdb_score'))

fname <- rownames(cor_imdb_score_movie_scaled)
mask <- abs(cor_imdb_score_movie_scaled) >= colMeans(abs(cor_imdb_score_movie_scaled))

best_formula <- na.omit(as.formula(paste("imdb_score~", paste(fname[mask], collapse = ' + ')) ))



x2_scaled <- model.matrix(best_formula, data = train_movie_scaled)

y_scaled <- train_movie_scaled$imdb_score 



### Test ~ Scaled


x0_test_scaled <- model.matrix(imdb_score~., data = test_movie_scaled)
x1_test_scaled <- model.matrix(imdb_score~.*., data = test_movie_scaled)
x2_test_scaled <- model.matrix(best_formula, data = test_movie_scaled)

y_test_scaled <- test_movie_scaled$imdb_score


####################
##     Ridge      ##
####################



## All Features


ridge_mdl_movie_0 <- cv.glmnet(x0, na.omit(y), alpha = 0)


# Prediction
ridge_pred_movie_0 <- predict(ridge_mdl_movie_0, s = 'lambda.min', newx = x0_test)

# MSE
mse_ridge_movie_0 <- mean((ridge_pred_movie_0 - y_test)^2)
mse_ridge_movie_0



## Group


ridge_mdl_movie_1 <- cv.glmnet(x1, na.omit(y), alpha = 0)

# Prediction
ridge_pred_movie_1 <- predict(ridge_mdl_movie_1, s = 'lambda.min', newx = x1_test)

# MSE
mse_ridge_movie_1 <- mean((ridge_pred_movie_1 - y_test)^2)
mse_ridge_movie_1



## Best Correlation


ridge_mdl_movie_2 <- cv.glmnet(x2, na.omit(y), alpha = 0)

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

# Prediction
lasso_pred_movie_0 <- predict(lasso_mdl_movie_0, s = 'lambda.min', newx = x0_test)

# MSE
mse_lasso_movie_0 <- mean((lasso_pred_movie_0 - y_test)^2)
mse_lasso_movie_0



## Group


lasso_mdl_movie_1 <- cv.glmnet(x1, na.omit(y), alpha = 1)

# Prediction
lasso_pred_movie_1 <- predict(lasso_mdl_movie_1, s = 'lambda.min', newx = x1_test)

# MSE
mse_lasso_movie_1 <- mean((lasso_pred_movie_1 - y_test)^2)
mse_lasso_movie_1



## Best Correlation


lasso_mdl_movie_2 <- cv.glmnet(x2, na.omit(y), alpha = 1)

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

it <- 5
n <- 1000

mse_en_movie_0 <- data.frame( mse = rep(0, it),
                            alpha = rep(0, it))
i <- 1

for (alpha in sample(0:n, it)){
  en_mdl_movie_0 <- cv.glmnet(x0, na.omit(y), alpha = alpha/n)
  
  # Prediction
  en_pred_movie_0 <- predict(en_mdl_movie_0, s = 'lambda.min', newx = x0_test)
  
  # MSE
  mse_en_movie_0[i, 'mse'] <- mean((en_pred_movie_0 - y_test)^2)
  mse_en_movie_0[i, 'alpha'] <- alpha*(1/n)
  i <- i + 1
}

mse_en_movie_0 <- mse_en_movie_0[mse_en_movie_0[, 'mse'] == min(mse_en_movie_0[, 'mse']),]



## Group

mse_en_movie_1 <- data.frame( mse = rep(0, it),
                              alpha = rep(0, it))
i <- 1

for (alpha in sample(0:n, it)){
  en_mdl_movie_1 <- cv.glmnet(x1, na.omit(y), alpha = alpha/n)
  
  # Prediction
  en_pred_movie_1 <- predict(en_mdl_movie_1, s = 'lambda.min', newx = x1_test)
  
  # MSE
  mse_en_movie_1[i, 'mse'] <- mean((en_pred_movie_1 - y_test)^2)
  mse_en_movie_1[i, 'alpha'] <- alpha*(1/n)
  i <- i + 1
}

mse_en_movie_1 <- mse_en_movie_1[mse_en_movie_1[, 'mse'] == min(mse_en_movie_1[, 'mse']),]



## Best Correlation

mse_en_movie_2 <- data.frame( mse = rep(0, it),
                              alpha = rep(0, it))
i <- 1

for (alpha in sample(0:n, it)){
  en_mdl_movie_2 <- cv.glmnet(x2, na.omit(y), alpha = alpha/n)
  
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
##      Ridge ~ Scaled      ##
##############################



## All Features

ridge_mdl_movie_scaled_0 <- cv.glmnet(x0_scaled, na.omit(y_scaled), alpha = 0)


# Prediction
ridge_pred_movie_scaled_0 <- predict(ridge_mdl_movie_scaled_0, s = 'lambda.min', newx = x0_test_scaled)

# MSE
mse_ridge_movie_scaled_0 <- mean((ridge_pred_movie_scaled_0 - y_test_scaled)^2)
mse_ridge_movie_scaled_0



## Group

ridge_mdl_movie_scaled_1 <- cv.glmnet(x1_scaled, na.omit(y_scaled), alpha = 0)

# Prediction
ridge_pred_movie_scaled_1 <- predict(ridge_mdl_movie_scaled_1, s = 'lambda.min', newx = x1_test_scaled)

# MSE
mse_ridge_movie_scaled_1 <- mean((ridge_pred_movie_scaled_1 - y_test_scaled)^2)
mse_ridge_movie_scaled_1



## Best Correlation

ridge_mdl_movie_scaled_2 <- cv.glmnet(x2_scaled, na.omit(y_scaled), alpha = 0)

# Prediction
ridge_pred_movie_scaled_2 <- predict(ridge_mdl_movie_scaled_2, s = 'lambda.min', newx = x2_test_scaled)

# MSE
mse_ridge_movie_scaled_2 <- mean((ridge_pred_movie_scaled_2 - y_test_scaled)^2)
mse_ridge_movie_scaled_2



mse_ridge_movie_scaled <- c(mse_ridge_movie_scaled_0, mse_ridge_movie_scaled_1, mse_ridge_movie_scaled_2)





##############################
##      Lasso ~ Scaled      ##
##############################



## All Features

lasso_mdl_movie_scaled_0 <- cv.glmnet(x0_scaled, na.omit(y_scaled), alpha = 1)

# Prediction
lasso_pred_movie_scaled_0 <- predict(lasso_mdl_movie_scaled_0, s = 'lambda.min', newx = x0_test_scaled)

# MSE
mse_lasso_movie_scaled_0 <- mean((lasso_pred_movie_scaled_0 - y_test_scaled)^2)
mse_lasso_movie_scaled_0



## Group

lasso_mdl_movie_scaled_1 <- cv.glmnet(x1_scaled, na.omit(y_scaled), alpha = 1)

# Prediction
lasso_pred_movie_scaled_1 <- predict(lasso_mdl_movie_scaled_1, s = 'lambda.min', newx = x1_test_scaled)

# MSE
mse_lasso_movie_scaled_1 <- mean((lasso_pred_movie_scaled_1 - y_test_scaled)^2)
mse_lasso_movie_scaled_1



## Best Correlation

lasso_mdl_movie_scaled_2 <- cv.glmnet(x2_scaled, na.omit(y_scaled), alpha = 1)

# Prediction
lasso_pred_movie_scaled_2 <- predict(lasso_mdl_movie_scaled_2, s = 'lambda.min', newx = x2_test_scaled)

# MSE
mse_lasso_movie_scaled_2 <- mean((lasso_pred_movie_scaled_2 - y_test_scaled)^2)
mse_lasso_movie_scaled_2



mse_lasso_movie_scaled <- c(mse_lasso_movie_scaled_0, mse_lasso_movie_scaled_1, mse_lasso_movie_scaled_2)





##############################
##   Elastic Net ~ Scaled   ##
##############################



## All Features

mse_en_movie_scaled_0 <- data.frame( mse = rep(0, it),
                                     alpha = rep(0, it))
i <- 1

for (alpha in sample(0:n, it)){
  en_mdl_movie_scaled_0 <- cv.glmnet(x0_scaled, na.omit(y_scaled), alpha = alpha*(1/n))
  
  # Prediction
  en_pred_movie_scaled_0 <- predict(en_mdl_movie_scaled_0, s = 'lambda.min', newx = x0_test_scaled)
  
  # MSE
  mse_en_movie_scaled_0[i, 'mse'] <- mean((en_pred_movie_scaled_0 - y_test_scaled)^2)
  mse_en_movie_scaled_0[i, 'alpha'] <- alpha*(1/n)
  i <- i + 1
}

mse_en_movie_scaled_0 <- mse_en_movie_scaled_0[mse_en_movie_scaled_0[, 'mse'] == min(mse_en_movie_scaled_0[, 'mse']),]



## Group

mse_en_movie_scaled_1 <- data.frame( mse = rep(0, it),
                                     alpha = rep(0, it))
i <- 1

for (alpha in sample(0:n, it)){
  en_mdl_movie_scaled_1 <- cv.glmnet(x1_scaled, na.omit(y_scaled), alpha = alpha/n)
  
  # Prediction
  en_pred_movie_scaled_1 <- predict(en_mdl_movie_scaled_1, s = 'lambda.min', newx = x1_test_scaled)
  
  # MSE
  mse_en_movie_scaled_1[i, 'mse'] <- mean((en_pred_movie_scaled_1 - y_test_scaled)^2)
  mse_en_movie_scaled_1[i, 'alpha'] <- alpha*(1/n)
  i <- i + 1
}

mse_en_movie_scaled_1 <- mse_en_movie_scaled_1[mse_en_movie_scaled_1[, 'mse'] == min(mse_en_movie_scaled_1[, 'mse']),]



## Best Correlation

mse_en_movie_scaled_2 <- data.frame( mse = rep(0, it),
                                     alpha = rep(0, it))
i <- 1

for (alpha in sample(0:n, it)){
  en_mdl_movie_scaled_2 <- cv.glmnet(x2_scaled, na.omit(y_scaled), alpha = alpha/n)
  
  # Prediction
  en_pred_movie_scaled_2 <- predict(en_mdl_movie_scaled_2, s = 'lambda.min', newx = x2_test_scaled)
  
  # MSE
  mse_en_movie_scaled_2[i, 'mse'] <- mean((en_pred_movie_scaled_2 - y_test_scaled)^2)
  mse_en_movie_scaled_2[i, 'alpha'] <- alpha*(1/n)
  i <- i + 1
}

mse_en_movie_scaled_2 <- mse_en_movie_scaled_2[mse_en_movie_scaled_2[, 'mse'] == min(mse_en_movie_scaled_2[, 'mse']),]


mse_en_movie_scaled <- c(mse_en_movie_scaled_0[,'mse'], mse_en_movie_scaled_1[,'mse'], mse_en_movie_scaled_2[,'mse'])
alpha_en_movie_scaled <- c(mse_en_movie_scaled_0[,'alpha'], mse_en_movie_scaled_1[,'alpha'], mse_en_movie_scaled_2[,'alpha'])



##############################
##      Visualization       ##
##############################



mse_movie <- data.frame( model = c('R_0', 'R_1', 'R_2',
                                   'L_0', 'L_1', 'L_2',
                                   'EN_0', 'EN_1', 'EN_2'),
                         mse = c(mse_ridge_movie, mse_lasso_movie, mse_en_movie),
                         alpha = c(0, 0, 0, 1, 1, 1, alpha_en_movie) )

mse_movie_scaled <- data.frame( model = c('R_0', 'R_1', 'R_2',
                                          'L_0', 'L_1', 'L_2',
                                          'EN_0', 'EN_1', 'EN_2'),
                         mse = c(mse_ridge_movie_scaled, mse_lasso_movie_scaled, mse_en_movie_scaled),
                         alpha = c(0, 0, 0, 1, 1, 1, alpha_en_movie_scaled) )

## MSE 

ggplot(mse_movie, aes(x = mse_movie[, 'model'], y = mse_movie[, 'mse']) ) +
  geom_col() +
  coord_cartesian(ylim = c(0.7, 1)) +
  labs(
    title = 'MSE',
    x = 'Models',
    y = 'MSE'
  )

ggplot(mse_movie_scaled, aes(x = mse_movie_scaled[, 'model'], y = mse_movie_scaled[, 'mse']) ) +
  geom_col() +
  coord_cartesian(ylim = c(0.7, 1)) +
  labs(
    title = 'MSE with Scaled Data',
    x = 'Models',
    y = 'MSE'
  )



### Beat the Model

mask <- data_movie_scaled$title == 'Titanic'

beat_the_model <- data.frame(name = c('Ridge_0', 'Ridge_1', 'Ridge_2',
                                      'Lasso_0', 'Lasso_1', 'Lasso_2',
                                      'ElasticNet_0', 'ElasticNet_1', 'ElasticNet_2', 
                                      'IMDB Score'),
                             pred = c(ridge_pred_movie_0[mask], ridge_pred_movie_1[mask], ridge_pred_movie_2[mask], 
                                      lasso_pred_movie_0[mask], lasso_pred_movie_1[mask], lasso_pred_movie_2[mask], 
                                      en_pred_movie_0[mask], en_pred_movie_1[mask], en_pred_movie_2[mask], 
                                      y_test[mask]) )


