# Erasmus Padova
# Statistical Methods for High-Dimensional Data

### Import libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(corrplot)

library(caret)
library(randomForest)

wd <- getwd()
titleDirectory = paste(wd,"/titles.csv", sep="")
data_raw = read.csv(titleDirectory, header = TRUE)

credits = read.csv("credits.csv", header=TRUE)

#Removing the tmdb_popularity
#data_mov_num <- data_mov_num %>% select(-tmdb_popularity)

# Train-/Test-Split
random <- sample(1:nrow(data_mov_num), ceiling(0.8*dim(data_mov_num)[1])) # modify the size ?
train_movie <- data_mov_num[random,] 
test_movie <- data_mov_num[-random,]

tuneGrid <- expand.grid(mtry = c(2, sqrt(ncol(train_movie))), 
                        splitrule = c("gini", "extratrees"), 
                        min.node.size = c(1, 5, 10))

# Set up cross-validation
fitControl <- trainControl(method = "cv", 
                           number = 5, 
                           search = "grid",
                           allowParallel = TRUE)
set.seed(1234)
tunedModel <- train(imdb_score ~ ., data = train_movie,
                    method = "ranger",  # ranger is a faster implementation of randomForest
                    trControl = fitControl,
                    tuneGrid = tuneGrid,
                    num.trees = 500) # could be changed ?

# Best model parameters
bestModel <- tunedModel$finalModel
print(tunedModel)

predictions <- predict(bestModel, test_movie)

# Calculate MSE
mse <- mean((predictions$predictions - test_movie$imdb_score)^2)
print(paste("Mean Squared Error:", mse))

#Plot true values and predictions
plot(test_movie$imdb_score, predictions$predictions, xlim = c(0, max(test_movie$imdb_score)), ylim = c(0, max(predictions$predictions)))
abline(a = 0, b = 1, col = "red")
