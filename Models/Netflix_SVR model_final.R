### Import libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(glmnet)
library(gridExtra)
library(e1071)



### Load the data
user = 'Fairouz'
if (user == 'Mattia'){
  setwd("/Users/mattiapiazza/Documents/University/Statistical Methods for High Dimensional Data/Project/Dataset")
}
if (user == 'Theresa'){
  setwd("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Padova/Statistical Methods for High-Dim Data/Project/Data")
}
if (user == 'Fairouz'){
  setwd("C:/Users/fairouz/Desktop/statistical method project/dataset")
}

data_raw = read.csv('titles.csv', header = TRUE)
data_movie = read.csv("data_movie.csv", header=TRUE)

#load scaled movies dataset with no NAs 
data_movie_scaled<- read.csv('data_movie_scaled.csv', header = TRUE)



###SVR without scaling 

##test-train split 
set.seed(1234)

data_movie_num_scaled<- data_movie_scaled %>% select(-c("X","title","type"))

random <- sample(1:nrow(data_movie_num_scaled), ceiling(0.8*dim(data_movie_num_scaled)[1]))
train_movie_scaled <- data_movie_num_scaled[random,] 
test_movie_scaled <- data_movie_num_scaled[-random,]

X_train_movie_scaled<- select(train_movie_scaled ,-c(imdb_score))
y_train_movie_scaled<- train_movie_scaled$imdb_score
X_test_movie_scaled<- select(test_movie_scaled ,-c(imdb_score))
y_test_movie_scaled<- test_movie_scaled$imdb_score


###SVR with scaling and without tmdb features 
SVR_movie_scaled <- svm(imdb_score~release_year + age_certification + runtime + imdb_votes + 
                          isNotUS + drama + comedy + documentation + horror + crime + 
                          action + thriller + fantasy + romance + history + scifi + 
                          animation + reality + sport + family + music + war + western, data = train_movie_scaled , kernel = "radial", type = "nu-regression", epsilon = 0.1, cost = 2, gamma = 0.03)

SVR_pred_movie_scaled <- predict(SVR_movie_scaled, X_test_movie_scaled)

# Calculate MSE (Mean Squared Error)

SVR_movie_mse_scaled2 <- mean((SVR_pred_movie_scaled - y_test_movie_scaled)^2)
#~0.789221


#visualize 

# Create a data frame with true and predicted values
plot_data5 <- data.frame(Actual = y_test_movie_scaled, Predicted = SVR_pred_movie_scaled)

# Scatter plot
ggplot(plot_data5, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Add a line of perfect prediction
  labs(x = "True IMDb Scores", y = "Predicted IMDb Scores") +
  ggtitle("SVR Predictions vs. True Values for movies, scaled without tmdb ")


