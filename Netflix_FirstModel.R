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

###### First Model Ideas

# Simple linear regression
lm1 <- lm(imdb_score~release_year + age_certification + runtime + seasons + imdb_votes+
            isNotUS + drama + comedy + documentation + horror + crime + 
            action + thriller + fantasy + romance + history + scifi + 
            animation + reality + sport + family + music + war + western, data = data_show)
summary(lm1)

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

# Ridge Regression
## Problem with Lasso: genres could be excluded. Solution: Ridge

# Train-/Test-Split
set.seed(1234)
random <- sample(1:nrow(data_show), ceiling(0.8*dim(data_show)[1]))
train_show <- data_show[random,] 
test_show <- data_show[-random,]

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


# Take a look at highest coefficients
# Extract coefficients for the optimal lambda
coef_optimal <- coef(ridge_model1)

# Find the 15 largest coefficients
top_coeffs <- data_frame(value <- as.data.frame(coef_optimal@x),name <- as.data.frame(coef_optimal@Dimnames[[1]][coef_optimal@i+1]))
top_coeffs <- top_coeffs[-1,]
top_indices <- order(abs(top_coeffs$`coef_optimal@x`), decreasing = TRUE)[1:15]
top_variable_names <- top_coeffs$`coef_optimal@Dimnames[[1]][coef_optimal@i + 1]`[top_indices]
top_coeff_values <- top_coeffs$`coef_optimal@x`[top_indices]

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
  labs(title = "Top 15 Ridge Regression Coefficients",
       x = "Variable",
       y = "Coefficient") +
  theme_minimal()
