# Erasmus Padova
# Statistical Methods for High-Dimensional Data

# Project
rm(list = ls())

### Import libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(glmnet)

### Load the data
user = 'Theresa'
if (user == 'Mattia'){
  setwd("/Users/mattiapiazza/Documents/University/Statistical Methods for High Dimensional Data/Project/Dataset")
}
if (user == 'Theresa'){
  setwd("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Padova/Statistical Methods for High-Dim Data/Project/Data")
}

data_show= read.csv('data_show.csv', header = TRUE)
data_movie = read.csv("data_movie.csv", header=TRUE)

###### First Model Ideas for Shows

# Simple linear regression
lm1 <- lm(imdb_score~release_year + age_certification + runtime + seasons + imdb_votes+
            isNotUS + drama + comedy + documentation + horror + crime + 
            action + thriller + fantasy + romance + history + scifi + 
            animation + reality + sport + family + music + war + western, data = data_show)
summary(lm1)

## Analysis of single continuous components
# Can we observe a linear trend?
par(mfrow = c(3,2))

# Analysis of the annual mean imdb (-> decreasing trend!)
mean_imdb = rep(0,22)

for (year in c(2000:2022)){
  mean_imdb[year-1999] = mean(na.omit(data_show[data_show$release_year == year,]$imdb_score))
}
plot(c(2000:2022), mean_imdb, main = "Mean IMDb Score Over Time", xlab = "Year", ylab = "Mean IMDb Score")

# Fit a linear regression model
regression_model <- lm(mean_imdb ~ c(2000:2022))

# Add the regression line to the plot
abline(regression_model, col = "red")
# --> clear linear effect

# Analysis of run time
levels_runtime = as.numeric(levels(as.factor(data_show$runtime)))
mean_runtime = rep(0,length(levels_runtime))
i = 1
for (time in levels_runtime){
  mean_runtime[i] =  mean(na.omit(data_show[data_show$runtime == time,]$imdb_score))
  i = i+1
}

plot(levels_runtime, mean_runtime, main = "Mean IMDb Score For different Run Times", xlab = "Run Time", ylab = "Mean IMDb Score")

# Fit a linear regression model
regression_model <- lm(mean_runtime ~ levels_runtime)

# Add the regression line to the plot
abline(regression_model, col = "red")
## -> clear linear positive trend

# Analysis of seasons
levels_seasons = as.numeric(levels(as.factor(data_show$seasons)))
mean_seasons = rep(0,length(levels_seasons))
i = 1
for (seasons in levels_seasons){
  mean_seasons[i] =  mean(na.omit(data_show[data_show$seasons == seasons,]$imdb_score))
  i = i+1
}

plot(levels_seasons, mean_seasons, main = "Mean IMDb Score For different Seasons", xlab = "Number of Seasons", ylab = "Mean IMDb Score")

# Fit a linear regression model
regression_model <- lm(mean_seasons ~ levels_seasons)

# Add the regression line to the plot
abline(regression_model, col = "red")
## -> clear linear positive trend

# Analysis of tmdb_score
plot(data_show$tmdb_score, data_show$imdb_score, main = "IMDb Score vs. TMDb Score", xlab = "TMDb Score", ylab = "IMDb Score", xlim = c(1,10))

# Fit a linear regression model
regression_model <- lm(data_show$imdb_score ~ data_show$tmdb_score)

# Add the regression line to the plot
abline(regression_model, col = "red")
## -> linear trend

plot(data_show$imdb_votes, data_show$imdb_score, main = "IMDb Score vs. IMDb Votes", xlab = "IMDb Votes", ylab = "IMDb Score")

# Fit a linear regression model
regression_model <- lm(data_show$imdb_score ~ data_show$imdb_votes)

# Add the regression line to the plot
abline(regression_model, col = "red")
## -> linear trend (?)

# Analysis of tmdb_popularity
plot(data_show$tmdb_popularity, data_show$imdb_score, main = "IMDb Score vs. TMDb Popularity", xlab = "TMDb Popularity", ylab = "IMDb Score")

# Fit a linear regression model
regression_model <- lm(data_show$imdb_score ~ data_show$tmdb_popularity)

# Add the regression line to the plot
abline(regression_model, col = "red")
## -> linear trend(?)

par(mfrow = c(1,1))


# Ridge Regression
## Problem with Lasso: genres could be excluded. Solution: Ridge

# Train-/Test-Split
set.seed(1234)
random <- sample(1:nrow(data_show), ceiling(0.8*dim(data_show)[1]))
train_show <- data_show[random,] 
test_show <- data_show[-random,]

# First Try: with tmdb score
model_data_show <- select(train_show,-c(X,title, type,description, genres,production_countries))
model_data_show <- na.omit(model_data_show)
x <- model.matrix(~.*., data = select(model_data_show, -c(imdb_score))) # no need to specify response, '.' means for every variable in the data set
ridge_model1 <- cv.glmnet(x, na.omit(model_data_show$imdb_score), alpha = 0)
plot(ridge_model1)
summary(ridge_model1$glmnet.fit$beta)

# Make predictions
data_test_show <- select(test_show,-c(X,title, type,description, genres,production_countries))
data_test_show <- na.omit(data_test_show)
x_test <- model.matrix(~.*., data = select(data_test_show, -c(imdb_score))) # no need to specify response, '.' means for every variable in the data set

predictions <- predict(ridge_model1, s = 'lambda.min', newx = x_test)

# Evaluation
mse_ridge1 <- mean((predictions - data_test_show$imdb_score)^2)
mse_ridge1

# Second Try: without tmdb score
model_data_show <- select(train_show,-c(X,title, type,description, genres,production_countries,tmdb_score))
model_data_show <- na.omit(model_data_show)
x <- model.matrix(~.*., data = select(model_data_show, -c(imdb_score))) # no need to specify response, '.' means for every variable in the data set
ridge_model2 <- cv.glmnet(x, na.omit(model_data_show$imdb_score), alpha = 0)
plot(ridge_model2)
# summary(ridge_model2$glmnet.fit$beta)

# Make predictions
data_test_show <- select(test_show,-c(X,title, type,description, genres,production_countries, tmdb_score))
data_test_show <- na.omit(data_test_show)
x_test <- model.matrix(~.*., data = select(data_test_show, -c(imdb_score))) # no need to specify response, '.' means for every variable in the data set

predictions <- predict(ridge_model2, s = 'lambda.min', newx = x_test)

# Evaluation
mse_ridge2 <- mean((predictions - data_test_show$imdb_score)^2)
mse_ridge2

data.frame('with tmdb_score' = mse_ridge1, 'without tmdb_score' = mse_ridge2)

# Take a look at highest coefficients
## With tmdb score
# Extract coefficients for the optimal lambda
coef_optimal <- coef(ridge_model1)

# Find the 15 largest coefficients
top_coeffs <- data.frame(value = coef_optimal@x,name = coef_optimal@Dimnames[[1]][coef_optimal@i+1])
top_coeffs <- top_coeffs[-1,]
top_indices <- order(abs(top_coeffs$value), decreasing = TRUE)[1:15]
top_variable_names <- top_coeffs$name[coef_optimal@i + 1][top_indices]
top_coeff_values <- top_coeffs$value[top_indices]

# Create a data frame for plotting
plot_data <- data.frame(
  Variable = top_variable_names,
  Coefficient = top_coeff_values
)

# Plot the 15 largest coefficients with positive in red and negatives in blue
ggplot(plot_data, aes(x = Variable, y = Coefficient, fill = factor(sign(Coefficient)))) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("blue", "red"), guide = FALSE) +
  coord_flip() +
  labs(title = "Top 15 Ridge Regression Coefficients - with tmbd score",
       x = "Variable",
       y = "Coefficient") +
  theme_minimal()

## Without tmdb score
# Extract coefficients for the optimal lambda
coef_optimal <- coef(ridge_model2)

# Find the 15 largest coefficients
top_coeffs <- data.frame(value = coef_optimal@x,name = coef_optimal@Dimnames[[1]][coef_optimal@i+1])
top_coeffs <- top_coeffs[-1,]
top_indices <- order(abs(top_coeffs$value), decreasing = TRUE)[1:15]
top_variable_names <- top_coeffs$name[coef_optimal@i + 1][top_indices]
top_coeff_values <- top_coeffs$value[top_indices]

# Create a data frame for plotting
plot_data <- data.frame(
  Variable = top_variable_names,
  Coefficient = top_coeff_values
)

# Plot the 15 largest coefficients with positive in red and negatives in blue
ggplot(plot_data, aes(x = Variable, y = Coefficient, fill = factor(sign(Coefficient)))) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("blue", "red"), guide = FALSE) +
  coord_flip() +
  labs(title = "Top 15 Ridge Regression Coefficients - without tmbd score",
       x = "Variable",
       y = "Coefficient") +
  theme_minimal()


# Now: with scaled data 

data_show_scaled <- data_show %>%
  select(-c(X,title,type,description,genres, production_countries)) %>%
  mutate(release_year = scale(release_year),
         runtime = scale(runtime),
         seasons = scale(seasons),
         imdb_score = scale(imdb_score),
         imdb_votes = scale(imdb_votes),
         tmdb_popularity = scale(tmdb_popularity),
         tmdb_score = scale(tmdb_score))

# Ridge Regression - Scaled
## Problem with Lasso: genres could be excluded. Solution: Ridge

# Train-/Test-Split
set.seed(1234)
random <- sample(1:nrow(data_show_scaled), ceiling(0.8*dim(data_show_scaled)[1]))
train_show <- data_show_scaled[random,] 
test_show <- data_show_scaled[-random,]

# First Try: with tmdb score
#model_data_show <- select(train_show,-c(X,title, type,description, genres,production_countries))
model_data_show <- na.omit(model_data_show)
x <- model.matrix(~.*., data = select(model_data_show, -c(imdb_score))) # no need to specify response, '.' means for every variable in the data set
ridge_model1_scaled <- cv.glmnet(x, na.omit(model_data_show$imdb_score), alpha = 0)
plot(ridge_model1_scaled)
#summary(ridge_model1_scaled$glmnet.fit$beta)

# Make predictions
#data_test_show <- select(test_show,-c(X,title, type,description, genres,production_countries))
data_test_show <- na.omit(data_test_show)
x_test <- model.matrix(~.*., data = select(data_test_show, -c(imdb_score))) # no need to specify response, '.' means for every variable in the data set

predictions <- predict(ridge_model1_scaled, s = 'lambda.min', newx = x_test)

# Evaluation
mse_ridge1_scaled <- mean((predictions - data_test_show$imdb_score)^2)
mse_ridge1_scaled


# Second Try: without tmdb score
model_data_show <- select(train_show,-c(tmdb_score))
model_data_show <- na.omit(model_data_show)
x <- model.matrix(~.*., data = select(model_data_show, -c(imdb_score))) # no need to specify response, '.' means for every variable in the data set
ridge_model2_scaled <- cv.glmnet(x, na.omit(model_data_show$imdb_score), alpha = 0)
plot(ridge_model2_scaled)
#summary(ridge_model2_scaled$glmnet.fit$beta)

# Make predictions
data_test_show <- select(test_show,-c(tmdb_score))
data_test_show <- na.omit(data_test_show)
x_test <- model.matrix(~.*., data = select(data_test_show, -c(imdb_score))) # no need to specify response, '.' means for every variable in the data set

predictions <- predict(ridge_model2_scaled, s = 'lambda.min', newx = x_test)

# Evaluation
mse_ridge2_scaled <- mean((predictions - data_test_show$imdb_score)^2)
mse_ridge2_scaled

data.frame('with tmdb_score' = mse_ridge1_scale, 'without tmdb_score' = mse_ridge2_scaled)

# Take a look at highest coefficients
## With tmdb score
# Extract coefficients for the optimal lambda
coef_optimal <- coef(ridge_model1_scaled)

# Find the 15 largest coefficients
top_coeffs <- data.frame(value = coef_optimal@x,name = coef_optimal@Dimnames[[1]][coef_optimal@i+1])
top_coeffs <- top_coeffs[-1,]
top_indices <- order(abs(top_coeffs$value), decreasing = TRUE)[1:15]
top_variable_names <- top_coeffs$name[coef_optimal@i + 1][top_indices]
top_coeff_values <- top_coeffs$value[top_indices]

# Create a data frame for plotting
plot_data <- data.frame(
  Variable = top_variable_names,
  Coefficient = top_coeff_values
)

# Plot the 15 largest coefficients with positive in red and negatives in blue
ggplot(plot_data, aes(x = Variable, y = Coefficient, fill = factor(sign(Coefficient)))) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("blue", "red"), guide = FALSE) +
  coord_flip() +
  labs(title = "Top 15 Ridge Regression Coefficients - with tmbd score, scaled",
       x = "Variable",
       y = "Coefficient") +
  theme_minimal()

## Without tmdb score
# Extract coefficients for the optimal lambda
coef_optimal <- coef(ridge_model2_scaled)

# Find the 15 largest coefficients
top_coeffs <- data.frame(value = coef_optimal@x,name = coef_optimal@Dimnames[[1]][coef_optimal@i+1])
top_coeffs <- top_coeffs[-1,]
top_indices <- order(abs(top_coeffs$value), decreasing = TRUE)[1:15]
top_variable_names <- top_coeffs$name[coef_optimal@i + 1][top_indices]
top_coeff_values <- top_coeffs$value[top_indices]

# Create a data frame for plotting
plot_data <- data.frame(
  Variable = top_variable_names,
  Coefficient = top_coeff_values
)

# Plot the 15 largest coefficients with positive in red and negatives in blue
ggplot(plot_data, aes(x = Variable, y = Coefficient, fill = factor(sign(Coefficient)))) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("blue", "red"), guide = FALSE) +
  coord_flip() +
  labs(title = "Top 15 Ridge Regression Coefficients - without tmbd score, scaled",
       x = "Variable",
       y = "Coefficient") +
  theme_minimal()
