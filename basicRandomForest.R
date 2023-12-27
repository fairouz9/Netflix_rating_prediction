# Erasmus Padova
# Statistical Methods for High-Dimensional Data

### Import libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(corrplot)

library(randomForest)

wd <- getwd()
titleDirectory = paste(wd,"/titles.csv", sep="")
data_raw = read.csv(titleDirectory, header = TRUE)

credits = read.csv("credits.csv", header=TRUE)

#Removing the tmdb_popularity
#data_mov_num <- data_mov_num %>% select(-tmdb_popularity)

# Train-/Test-Split
set.seed(1234)
random <- sample(1:nrow(data_mov_num), ceiling(0.8*dim(data_mov_num)[1])) # modify the size?
train_movie <- data_mov_num[random,] 
test_movie <- data_mov_num[-random,]

# Train the Random Forest model
rf_model <- randomForest(imdb_score ~ ., data = train_movie)

# Make predictions
predictions <- predict(rf_model, test_movie)

# Evaluate the model (e.g., using Mean Squared Error)
mse <- mean((predictions - test_movie$imdb_score)^2)
print(paste("Mean Squared Error:", mse))

#Plot true values and predictions
plot(test_movie$imdb_score, predictions, xlim = c(0, max(test_movie$imdb_score)), ylim = c(0, max(predictions)))
abline(a = 0, b = 1, col = "red")
