### Import libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(glmnet)
library(gridExtra)
library(e1071)
library(caret)



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

data_movie_num_scaled<- data_movie_scaled %>% select(-c("X","title","type"))%>%
  mutate(age_certification = as.factor(age_certification))


random <- sample(1:nrow(data_movie_num_scaled), ceiling(0.8*dim(data_movie_num_scaled)[1]))
train_movie_scaled <- data_movie_num_scaled[random,] 
test_movie_scaled <- data_movie_num_scaled[-random,]

X_train_movie_scaled<- select(train_movie_scaled ,-c(imdb_score))
y_train_movie_scaled<- train_movie_scaled$imdb_score
X_test_movie_scaled<- select(test_movie_scaled ,-c(imdb_score))
y_test_movie_scaled<- test_movie_scaled$imdb_score

dt<-data_movie_scaled[-random,]

# Perform grid search using tune (radial kernel is the default one)
svm_model <- tune(
  svm,
  imdb_score ~ release_year + age_certification + runtime + imdb_votes + 
    isNotUS + drama + comedy + documentation + horror + crime + 
    action + thriller + fantasy + romance + history + scifi + 
    animation + reality + sport + family + music + war + western,
  data = train_movie_scaled,
  ranges = list(
    gamma = c(0.01,0.03,0.05,0.1),
    cost = 2^(1:3),
    epsilon = c(0.1,0.2,0.3)
  )
)

# Get the best model from the grid search
best_model <- svm_model$best.model

#best model: 
#SVM-Type:eps-regression, SVM-Kernel:radial, cost:2,  gamma:0.03, epsilon:0.3
 
#ie best model can be defined as below 
SVR_movie_scaled <- svm(imdb_score~release_year + age_certification + runtime + imdb_votes + 
+                           isNotUS + drama + comedy + documentation + horror + crime + 
+                           action + thriller + fantasy + romance + history + scifi + 
+                           animation + reality + sport + family + music + war + western, data = train_movie_scaled , kernel = "radial",
                            type = "eps-regression", epsilon = 0.3, cost = 2, gamma = 0.03)



# Print the best parameters
print(best_model)

#predict using best model obtained from grid search 
SVR_pred_movie_scaled <- predict(best_model, X_test_movie_scaled)


## Calculate MSE (Mean Squared Error)
SVR_movie_mse_scaled <- mean((SVR_pred_movie_scaled - y_test_movie_scaled)^2)
#~0.7881993


##visualize 

# Create a data frame with true and predicted values
plot_data_scaled <- data.frame(Actual = y_test_movie_scaled, Predicted = SVR_pred_movie_scaled_best)

# Scatter plot
ggplot(plot_data_scaled_best, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Add a line of perfect prediction
  labs(x = "True IMDb Scores", y = "Predicted IMDb Scores") +
  ggtitle("SVR Predictions vs. True Values for movies")
