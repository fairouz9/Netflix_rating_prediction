# Statistical Methods for High-Dimensional Data

### Import libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(corrplot)

library(caret)
library(randomForest)

data_movie_scaled <- read.csv("data_movie_scaled.csv", header = TRUE)
data_movie <- read.csv("data_movie.csv", header = TRUE)
data_movie <- na.omit(data_movie)

# Removing the tmdb_popularity and tmdb_score
data_movie_scaled <- data_movie_scaled %>% select(-tmdb_popularity)
data_movie_scaled <- data_movie_scaled %>% select(-tmdb_score)
data_movie_scaled <- data_movie_scaled %>% select(-X)
data_movie_scaled <- data_movie_scaled %>% select(-age_certification)


interactive_movies <- c("The Amazing Spider-Man", "Titanic")
interactive_movies_data <- data_movie_scaled %>% filter(title %in% interactive_movies)
interactive_movies_data <- interactive_movies_data %>% select(-title)

data_movie_scaled <- data_movie_scaled %>% filter(!title %in% interactive_movies)

data_movie_scaled <- data_movie_scaled %>% select(-title)

######################
###  WITHOUT TMDB  ###
######################

# Train-/Test-Split
set.seed(1234)
random <- sample(1:nrow(data_movie_scaled), ceiling(0.8 * dim(data_movie_scaled)[1])) # modify the size ?
train_movie <- data_movie_scaled[random, ]
test_movie <- data_movie_scaled[-random, ]

test_movie <- rbind(test_movie, interactive_movies_data)

tuneGrid <- expand.grid(
    mtry = c(2, sqrt(ncol(train_movie))),
    splitrule = c("variance", "maxstat", "extratrees"),
    min.node.size = c(1, 5, 10)
)

# Set up cross-validation
fitControl <- trainControl(
    method = "cv",
    number = 5,
    search = "grid",
    allowParallel = TRUE
)

tunedModel <- train(imdb_score ~ .,
    data = train_movie,
    method = "ranger",
    trControl = fitControl,
    tuneGrid = tuneGrid,
    num.trees = 50
)

# Best model parameters
bestModel <- tunedModel$finalModel
print(tunedModel)

predictions <- predict(tunedModel, test_movie)

# Calculate MSE
mse <- mean((predictions - test_movie$imdb_score)^2)
print(paste("Mean Squared Error:", mse))

# Plot true values and predictions
plot(test_movie$imdb_score, predictions, main = "Predictions", xlab = "True score", ylab = "Predicted score", xlim = c(0, max(test_movie$imdb_score)), ylim = c(0, max(predictions)))
abline(a = 0, b = 1, col = "red")
