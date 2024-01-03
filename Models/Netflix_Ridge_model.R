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

# data_show= read.csv('data_show.csv', header = TRUE)
data_movie = read.csv("data_movie.csv", header=TRUE)

###### Analysis of single continuous components
# Can we observe a linear trend?
pdf('Plots/linear_trend_analysis.pdf', width = 12, height = 4)
par(mfrow = c(1,3))

## Analysis of the annual mean imdb (-> decreasing trend!)
mean_imdb = rep(0,22)

for (year in c(2000:2022)){
  mean_imdb[year-1999] = mean(na.omit(data_movie[data_movie$release_year == year,]$imdb_score))
}
plot(c(2000:2022), mean_imdb, main = "Mean IMDb score over time", xlab = "Year", ylab = "Mean IMDb Score")
regression_model <- lm(mean_imdb ~ c(2000:2022))
abline(regression_model, col = "red")
# --> clear linear effect

## Analysis of run time
levels_runtime = as.numeric(levels(as.factor(data_movie$runtime)))
mean_runtime = rep(0,length(levels_runtime))
i = 1
for (time in levels_runtime){
  mean_runtime[i] =  mean(na.omit(data_movie[data_movie$runtime == time,]$imdb_score))
  i = i+1
}

plot(levels_runtime, mean_runtime, main = "Mean IMDb score for different run times", xlab = "Run Time", ylab = "Mean IMDb Score")
regression_model <- lm(mean_runtime ~ levels_runtime)
abline(regression_model, col = "red")
## -> clear linear positive trend

## Analysis of imdb_votes
levels_votes = as.numeric(levels(as.factor(data_movie$imdb_votes)))
mean_votes = rep(0,length(levels_votes))
i = 1
for (votes in levels_votes){
  mean_votes[i] =  mean(na.omit(data_movie[data_movie$imdb_votes == votes,]$imdb_score))
  i = i+1
}

plot(levels_votes, mean_votes, main = "Mean IMDb score for different IMDb votes", xlab = "IMDb Votes", ylab = "Mean IMDb Score")
regression_model <- lm(mean_votes ~ levels_votes)
abline(regression_model, col = "red")
# -> linear trend (?)
dev.off()
par(mfrow = c(1,1))

##################### Ridge Regression
# Problem with Lasso: genres could be excluded. Solution: Ridge

## Data scaling
data_movie_scaled <- data_movie %>%
  select(-c(X,title,type,description,genres, production_countries, tmdb_popularity, tmdb_score)) %>%
  mutate(release_year = scale(release_year),
         runtime = scale(runtime),
         imdb_votes = scale(imdb_votes)) %>%
  mutate(age_certification = as.factor(age_certification))

## Train-/Test-Split
set.seed(1234)
random <- sample(1:nrow(data_movie_scaled), ceiling(0.8*dim(data_movie_scaled)[1]))
train_movie <- data_movie_scaled[random,] 
test_movie <- data_movie_scaled[-random,]

model_data_movie <- na.omit(train_movie)
x <- model.matrix(~.*., data = select(model_data_movie, -c(imdb_score))) # no need to specify response, '.' means for every variable in the data set
ridge_model_scaled <- cv.glmnet(x, na.omit(model_data_movie$imdb_score), alpha = 0)
plot(ridge_model_scaled)
#summary(ridge_model_scaled$glmnet.fit$beta)

## Make predictions
data_test_movie <- na.omit(test_movie)
x_test <- model.matrix(~.*., data = select(data_test_movie, -c(imdb_score))) # no need to specify response, '.' means for every variable in the data set

predictions <- predict(ridge_model_scaled, s = 'lambda.min', newx = x_test)

## Evaluation
mse_ridge_scaled <- mean((predictions - data_test_movie$imdb_score)^2)
mse_ridge_scaled

## Plot fitted vs. true
# Combine true and predicted values into a data frame
plot_data <- data.frame(True = data_test_movie$imdb_score, Predicted = as.vector(predictions))

# Calculate axis limits based on the range of true and predicted values
limits <- range(c(plot_data$True, plot_data$Predicted))

# Create a scatter plot
ggplot(plot_data, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "solid") +
  labs(title = "Fitted vs. True Values on IMDb Scores - Linear Ridge Regression",
       x = "True Values",
       y = "Fitted Values") +
  theme_minimal() +
  xlim(limits) +
  ylim(limits) +
  theme(axis.title.x = element_text(size = 14),  # Adjust the font size for x-axis title
        axis.title.y = element_text(size = 14),  # Adjust the font size for y-axis title
        axis.text = element_text(size = 12))     # Adjust the font size for axis tick labels

# Save the plot as a PDF file
ggsave("Plots/Ridge_fitted_vs_true.pdf", width = 8, height = 8)

## Plot 15 most important estimated coefficients
# Extract coefficients for the optimal lambda
coef_optimal <- coef(ridge_model_scaled)

# Find the 15 largest coefficients
top_coeffs <- data.frame(value = coef_optimal@x,name = coef_optimal@Dimnames[[1]][coef_optimal@i+1])
top_coeffs <- top_coeffs[-1,]
top_indices <- order(abs(top_coeffs$value), decreasing = TRUE)[1:15]
top_variable_names <- top_coeffs$name[top_indices]
top_coeff_values <- top_coeffs$value[top_indices]

# Create a data frame for plotting
plot_data <- data.frame(
  Variable = top_variable_names,
  Coefficient = top_coeff_values
)

# Plot the 15 largest coefficients with positive in red and negatives in blue
ggplot(plot_data, aes(x = reorder(Variable, Coefficient), y = Coefficient, fill = factor(sign(Coefficient)))) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("blue", "red"), guide = FALSE) +
  coord_flip() +
  labs(title = "Top 15 Ridge Regression Coefficients",
       x = "Variable",
       y = "Coefficient") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),  # Adjust the font size for x-axis title
        axis.title.y = element_text(size = 14),  # Adjust the font size for y-axis title
        axis.text = element_text(size = 12))     # Adjust the font size for axis tick labels



ggsave('Plots/ridge_top15.pdf', height = 8, width = 10)
