
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
credits = read.csv("credits.csv", header=TRUE)

data_show= read.csv('data_show.csv', header = TRUE)
data_movie = read.csv("data_movie.csv", header=TRUE)

# Remove rows with NA values from the dataset
data_movie <- na.omit(data_movie)
data_show <- na.omit(data_show)




###SVR without scaling 

##test-train split 
set.seed(1234)

data_movie_num <- data_movie %>% select(-c("X","title","description","genres","production_countries","type"))
data_show_num <- data_show %>% select(-c("X","title","description","genres","production_countries","type"))

random <- sample(1:nrow(data_movie_num), ceiling(0.8*dim(data_movie_num)[1]))
train_movie <- data_movie_num[random,] 
test_movie <- data_movie_num[-random,]
X_train_movie<- select(train_movie ,-c(imdb_score))
y_train_movie<- train_movie$imdb_score
X_test_movie<- select(test_movie ,-c(imdb_score))
y_test_movie<- test_movie$imdb_score


random2 <- sample(1:nrow(data_show_num), ceiling(0.8*dim(data_show_num)[1]))
train_show <- data_show_num[random2,] 
test_show <- data_show_num[-random2,]
X_train_show<- select(train_show ,-c(imdb_score))
y_train_show<- train_show$imdb_score
X_test_show<- select(test_show ,-c(imdb_score))
y_test_show<- test_show$imdb_score


##FOR MOVIES  

#without tmdb scores and popularity 

SVR_movie <- svm(imdb_score~release_year + age_certification + runtime + imdb_votes + 
                   isNotUS + drama + comedy + documentation + horror + crime + 
                   action + thriller + fantasy + romance + history + scifi + 
                   animation + reality + sport + family + music + war + western, data = train_movie , kernel = "radial", type = "nu-regression", epsilon = 0.1, cost = 2, gamma = 0.03)

SVR_pred_movie <- predict(SVR_movie, X_test_movie)

# Calculate MSE (Mean Squared Error)
SVR_movie_mse <- mean((SVR_pred_movie - y_test_movie)^2)



#visualize 

# Create a data frame with true and predicted values
plot_data <- data.frame(Actual = y_test_movie, Predicted = SVR_pred_movie)

# Scatter plot
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Add a line of perfect prediction
  labs(x = "True IMDb Scores", y = "Predicted IMDb Scores") +
  ggtitle("SVR Predictions vs. True Values for movies without tmdb ")

#the model did not predict the lower values well 
#the red line is the x=y which is the perfect prediction 

#with tmdb 
SVR_movie2 <- svm(imdb_score~release_year + age_certification + runtime + imdb_votes + tmdb_score + tmdb_popularity +
                   isNotUS + drama + comedy + documentation + horror + crime + 
                   action + thriller + fantasy + romance + history + scifi + 
                   animation + reality + sport + family + music + war + western, data = train_movie , kernel = "radial", type = "nu-regression", epsilon = 0.1, cost = 2, gamma = 0.03)


SVR_pred_movie2 <- predict(SVR_movie2, X_test_movie)

# Calculate MSE (Mean Squared Error)
SVR_movie_mse2 <- mean((SVR_pred_movie2 - y_test_movie)^2)


#visualize 

# Create a data frame with true and predicted values
plot_data2 <- data.frame(Actual = y_test_movie, Predicted = SVR_pred_movie2)

# Scatter plot
ggplot(plot_data2, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Add a line of perfect prediction
  labs(x = "True IMDb Scores", y = "Predicted IMDb Scores") +
  ggtitle("SVR Predictions vs. True Values for movies with tmdb")



##FOR SHOWS 

#without tmdb scores and popularity 

SVR_show <- svm(imdb_score~release_year + age_certification + runtime + imdb_votes + seasons+
                   isNotUS + drama + comedy + documentation + horror + crime + 
                   action + thriller + fantasy + romance + history + scifi + 
                   animation + reality + sport + family + music + war + western, data = train_show , kernel = "radial", type = "nu-regression", epsilon = 0.1, cost = 2, gamma = 0.03)

SVR_pred_show<- predict(SVR_show, X_test_show)

# Calculate MSE (Mean Squared Error)
SVR_show_mse <- mean((SVR_pred_show - y_test_show)^2)



#visualize 
# Create a data frame with true and predicted values
plot_data3 <- data.frame(Actual = y_test_show, Predicted = SVR_pred_show)

# Scatter plot
ggplot(plot_data3, aes(x = Actual, y = Predicted)) +
  geom_point(color = "purple") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "green") +  # Add a line of perfect prediction
  labs(x = "True IMDb Scores", y = "Predicted IMDb Scores") +
  ggtitle("SVR Predictions vs. True Values for shows without tmdb")

#with tmdb 
SVR_show2 <- svm(imdb_score~release_year + age_certification + runtime + seasons + imdb_votes + tmdb_score + tmdb_popularity +
                    isNotUS + drama + comedy + documentation + horror + crime + 
                    action + thriller + fantasy + romance + history + scifi + 
                    animation + reality + sport + family + music + war + western, data = train_show , kernel = "radial", type = "nu-regression", epsilon = 0.1, cost = 2, gamma = 0.03)


SVR_pred_show2 <- predict(SVR_show2, X_test_show)

# Calculate MSE (Mean Squared Error)
SVR_show_mse2 <- mean((SVR_pred_show2 - y_test_show)^2)


#visualize 
# Create a data frame with true and predicted values
plot_data4 <- data.frame(Actual = y_test_show, Predicted = SVR_pred_show2)

# Scatter plot
ggplot(plot_data4, aes(x = Actual, y = Predicted)) +
  geom_point(color = "purple") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "green") +  # Add a line of perfect prediction
  labs(x = "True IMDb Scores", y = "Predicted IMDb Scores") +
  ggtitle("SVR Predictions vs. True Values for shows with tmdb")

#maybe because low scores have low number of votes and higher scores have higher number of votes and we're not scaling 


##next maybe look more into interpretation of loess,
#changing parameters for svr to improve 
#scaling before 



###SVR with scaling 

###Feature Scaling
#normalizing out data might improve the correlation matrix....It did not 

# Scale feature values using min/max scaling
z_score_norm <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}

train_movie_scaled <-train_movie %>%
  mutate_at(.vars = c(1,3,5,6,7), .funs = list(z_score_norm))

test_movie_scaled <-test_movie %>%
  mutate_at(.vars = c(1,3,5,6,7), .funs = list(z_score_norm))


X_test_movie_scaled<- select(test_movie_scaled ,-c(imdb_score))
y_test_movie_scaled<- test_movie_scaled$imdb_score


train_show_scaled <-train_show %>%
  mutate_at(.vars = c(1,3,4,6,7,8), .funs = list(z_score_norm))

test_show_scaled <-test_show %>%
  mutate_at(.vars = c(1,3,4,6,7,8), .funs = list(z_score_norm))


X_test_show_scaled<- select(test_show_scaled ,-c(imdb_score))
y_test_show_scaled<- test_show_scaled$imdb_score


#the y test doesn't change so no need to define it again
#and we don;t really need x_train


##FOR MOVIES
#without tmdb scores and popularity 

SVR_movie_scaled <- svm(imdb_score~release_year + age_certification + runtime + imdb_votes + 
                   isNotUS + drama + comedy + documentation + horror + crime + 
                   action + thriller + fantasy + romance + history + scifi + 
                   animation + reality + sport + family + music + war + western, data = train_movie_scaled , kernel = "radial", type = "nu-regression", epsilon = 0.1, cost = 2, gamma = 0.03)

SVR_pred_movie_scaled <- predict(SVR_movie_scaled, X_test_movie_scaled)

# Calculate MSE (Mean Squared Error)
SVR_movie_mse_scaled <- mean((SVR_pred_movie_scaled - y_test_movie_scaled)^2)



#visualize 

# Create a data frame with true and predicted values
plot_data5 <- data.frame(Actual = y_test_movie_scaled, Predicted = SVR_pred_movie_scaled)

# Scatter plot
ggplot(plot_data5, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Add a line of perfect prediction
  labs(x = "True IMDb Scores", y = "Predicted IMDb Scores") +
  ggtitle("SVR Predictions vs. True Values for movies, scaled without tmdb ")



#with tmdb 
SVR_movie_scaled2 <- svm(imdb_score~release_year + age_certification + runtime + imdb_votes + tmdb_score + tmdb_popularity +
                    isNotUS + drama + comedy + documentation + horror + crime + 
                    action + thriller + fantasy + romance + history + scifi + 
                    animation + reality + sport + family + music + war + western, data = train_movie_scaled , kernel = "radial", type = "nu-regression", epsilon = 0.1, cost = 2, gamma = 0.03)


SVR_pred_movie_scaled2 <- predict(SVR_movie_scaled2, X_test_movie_scaled)

# Calculate RMSE (Root Mean Squared Error)
SVR_movie_mse_scaled2 <- mean((SVR_pred_movie_scaled2 - y_test_movie_scaled)^2)


#visualize 

# Create a data frame with true and predicted values
plot_data6 <- data.frame(Actual = y_test_movie_scaled, Predicted = SVR_pred_movie_scaled2)

# Scatter plot
ggplot(plot_data6, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Add a line of perfect prediction
  labs(x = "True IMDb Scores", y = "Predicted IMDb Scores") +
  ggtitle("SVR Predictions vs. True Values for movies, scaled with tmdb")



##for shows


#without tmdb scores and popularity 

SVR_show_scaled <- svm(imdb_score~release_year + age_certification + runtime + imdb_votes + 
                          isNotUS + drama + comedy + documentation + horror + crime + 
                          action + thriller + fantasy + romance + history + scifi + 
                          animation + reality + sport + family + music + war + western, data = train_show_scaled , kernel = "radial", type = "nu-regression", epsilon = 0.1, cost = 2, gamma = 0.03)

SVR_pred_show_scaled <- predict(SVR_show_scaled, X_test_show_scaled)

# Calculate MSE (Mean Squared Error)
SVR_show_mse_scaled <- mean((SVR_pred_show_scaled - y_test_show_scaled)^2)



#visualize 

# Create a data frame with true and predicted values
plot_data7 <- data.frame(Actual = y_test_show_scaled, Predicted = SVR_pred_show_scaled)

# Scatter plot
ggplot(plot_data7, aes(x = Actual, y = Predicted)) +
  geom_point(color = "purple") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "green") +  # Add a line of perfect prediction
  labs(x = "True IMDb Scores", y = "Predicted IMDb Scores") +
  ggtitle("SVR Predictions vs. True Values for shows, scaled without tmdb ")



#with tmdb 
SVR_show_scaled2 <- svm(imdb_score~release_year + age_certification + runtime + imdb_votes + tmdb_score + tmdb_popularity +
                           isNotUS + drama + comedy + documentation + horror + crime + 
                           action + thriller + fantasy + romance + history + scifi + 
                           animation + reality + sport + family + music + war + western, data = train_show_scaled , kernel = "radial", type = "nu-regression", epsilon = 0.1, cost = 2, gamma = 0.03)


SVR_pred_show_scaled2 <- predict(SVR_show_scaled2, X_test_show_scaled)

# Calculate MSE (Mean Squared Error)
SVR_show_mse_scaled2 <- mean((SVR_pred_show_scaled2 - y_test_show_scaled)^2)


#visualize 

# Create a data frame with true and predicted values
plot_data8 <- data.frame(Actual = y_test_show_scaled, Predicted = SVR_pred_show_scaled2)

# Scatter plot
ggplot(plot_data8, aes(x = Actual, y = Predicted)) +
  geom_point(color = "purple") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "green") +  # Add a line of perfect prediction
  labs(x = "True IMDb Scores", y = "Predicted IMDb Scores") +
  ggtitle("SVR Predictions vs. True Values for shows, scaled with tmdb")

#show is even worse with lower values bcz probably we have even a lower number of votings and shows 

#the model does not improve much when we scale 
#still doesn't predict lower values that well, probably because of lack of data 
#use mattea's graph to explain 
# IMDB sores (Movies + Shows)

#save results in dataframe 

results <- data.frame(
  with_scaling = c("No", "No","No", "No", "Yes", "Yes","Yes", "Yes"),
  type = c("movie", "movie", "show", "show","movie", "movie", "show", "show"),
  with_tmdb = c("no", "Yes", "no", "Yes","no", "Yes", "no", "Yes"),
  mse = c(SVR_movie_mse,SVR_movie_mse2,SVR_show_mse,SVR_show_mse2,SVR_movie_mse_scaled,SVR_movie_mse_scaled2,SVR_show_mse_scaled,SVR_show_mse_scaled2))


