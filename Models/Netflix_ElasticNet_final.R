###############################
#         ELASTIC_NET         #
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

## Load the data

# With TMDB
data_movie_scaled_tmdb <- read.csv("csv/data_movie_scaled.csv", header=TRUE) %>% select(-c('X'))

# NO TMDB
data_movie_scaled <- data_movie_scaled_tmdb %>% select(-c('tmdb_score', 'tmdb_popularity'))



## Beat the Model Data

interactive_movies = c('The Amazing Spider-Man', 'Titanic')

# With TMDB
interactive_movies_data_tmdb <- data_movie_scaled_tmdb %>% filter(title %in% interactive_movies)

# NO TMDB
interactive_movies_data <- data_movie_scaled %>% filter(title %in% interactive_movies)

# Removing the movies chose for the "Beat the Model"
data_movie_scaled_tmdb <- data_movie_scaled_tmdb %>% filter(!title %in% interactive_movies)
data_movie_scaled <- data_movie_scaled_tmdb %>% filter(!title %in% interactive_movies)



## Factorization of age_certification

# With TMDB
data_movie_scaled_tmdb <- data_movie_scaled_tmdb %>%
  mutate(age_certification = as.factor(age_certification))

# NO TMDB
data_movie_scaled <- data_movie_scaled %>%
  mutate(age_certification = as.factor(age_certification))


# With TMDB
interactive_movies_data <- interactive_movies_data %>%
  mutate(age_certification = as.factor(age_certification))

# NO TMDB
interactive_movies_data <- interactive_movies_data %>%
  mutate(age_certification = as.factor(age_certification))





########################################
##          Train Test Split          ##
########################################

set.seed(1234)
random <- sample(1:nrow(data_movie_scaled), ceiling(0.8*dim(data_movie_scaled)[1]))



### TRAIN 

# With TMDB
train_movie_scaled_tmdb <- data_movie_scaled_tmdb[random,] %>% select(-c('title'))

x_scaled_tmdb <- model.matrix(imdb_score~.*., data = train_movie_scaled_tmdb)
y_scaled_tmdb <- train_movie_scaled_tmdb$imdb_score 

# NO TMDB
train_movie_scaled <- select(train_movie_scaled_tmdb, -c('tmdb_score', 'tmdb_popularity'))

x_scaled <- model.matrix(imdb_score~.*., data = train_movie_scaled)
y_scaled <- train_movie_scaled$imdb_score 



### TEST

test_movie_scaled_tmdb <- data_movie_scaled_tmdb[-random,] %>% select(-c('title')) %>%
  rbind(., select(interactive_movies_data_tmdb, -c('title')))

# With TMDB
x_test_scaled_tmdb <- model.matrix(imdb_score~.*., data = test_movie_scaled_tmdb)
y_test_scaled_tmdb <- test_movie_scaled_tmdb$imdb_score

# NO TMDB
test_movie_scaled <- select(test_movie_scaled_tmdb, -c('tmdb_score', 'tmdb_popularity'))

x_test_scaled <- model.matrix(imdb_score~.*., data = test_movie_scaled)
y_test_scaled <- test_movie_scaled$imdb_score





##############################
##    Elastic Net ~ TMDB    ##
##############################


it <- 700
n <- 1000


## Random Search for the optimal alpha

mse_en_movie_tmdb <- data.frame( mse = rep(0, it),
                                     alpha = rep(0, it))
i <- 1

for (alpha in sample(0:n, it)){
  en_mdl_movie_scaled_tmdb <- cv.glmnet(x_scaled_tmdb, y_scaled_tmdb, alpha = alpha/n)
  
  # Prediction
  en_pred_movie_scaled_tmdb <- predict(en_mdl_movie_scaled_tmdb, s = 'lambda.min', newx = x_test_scaled_tmdb)
  
  # MSE
  mse_en_movie_tmdb[i, 'mse'] <- mean((en_pred_movie_scaled_tmdb - y_test_scaled_tmdb)^2)
  mse_en_movie_tmdb[i, 'alpha'] <- alpha*(1/n)
  print(i, 'w/TMDB')
  i <- i + 1
}

mse_en_movie_opt_tmdb <- mse_en_movie_tmdb[mse_en_movie_tmdb[, 'mse'] == min(mse_en_movie_tmdb[, 'mse']),]





##############################
##  Elastic Net ~ NO TMDB   ##
##############################


## Random Search for the optimal alpha

mse_en_movie <- data.frame( mse = rep(0, it),
                            alpha = rep(0, it))
i <- 1

for (alpha in sample(0:n, it)){
  en_mdl_movie_scaled <- cv.glmnet(x_scaled, y_scaled, alpha = alpha/n)
  
  # Prediction
  en_pred_movie_scaled <- predict(en_mdl_movie_scaled, s = 'lambda.min', newx = x_test_scaled)
  
  # MSE
  mse_en_movie[i, 'mse'] <- mean((en_pred_movie_scaled - y_test_scaled)^2)
  mse_en_movie[i, 'alpha'] <- alpha*(1/n)
  print(c(i, 'noTMDB'))
  i <- i + 1
}

mse_en_movie_opt <- mse_en_movie[mse_en_movie[, 'mse'] == min(mse_en_movie[, 'mse']),]




## Prediction for the best alpha

# With TMDB
en_mdl_movie_tmdb <- cv.glmnet(x_scaled_tmdb, y_scaled_tmdb, alpha = 0.894)#mse_en_movie_opt_tmdb[,'alpha'])
en_pred_movie_tmdb <- predict(en_mdl_movie_tmdb, s = 'lambda.min', newx = x_test_scaled_tmdb)

# NO TMDB
en_mdl_movie <- cv.glmnet(x_scaled, y_scaled, alpha = 0.109)#mse_en_movie_opt[,'alpha'])
en_pred_movie <- predict(en_mdl_movie, s = 'lambda.min', newx = x_test_scaled)




## Plotting

# With TMDB
en_plot_tmdb <- data.frame(Pred = en_pred_movie_tmdb, True = y_test_scaled_tmdb)

ggplot(en_plot_tmdb, aes(x = y_test_scaled, y = en_pred_movie) ) +
  geom_point(color = 'blue') +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = 'Elastic Net Predictions vs. True Values ~ TMDB',
       x = "True IMDb Scores",
       y = "Predicted IMDb Scores") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(2, NA) +
  xlim(2, NA)



# NO TMDB
en_plot <- data.frame(Pred = en_pred_movie, True = y_test_scaled)

ggplot(en_plot, aes(x = y_test_scaled, y = en_pred_movie) ) +
  geom_point(color = 'blue') +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = 'Elastic Net Predictions vs. True Values',
       x = "True IMDb Scores",
       y = "Predicted IMDb Scores") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(2, NA) +
  xlim(2, NA)

ggsave("Plots/plot_EN_True_vs_Pred.pdf", width = 8, height = 8)
  


coef_optimal <- coef(en_mdl_movie)

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
  labs(title = "Top 15 Elastic Net Coefficients",
       x = "Variable",
       y = "Coefficient") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),  # Adjust the font size for x-axis title
        axis.title.y = element_text(size = 14),  # Adjust the font size for y-axis title
        axis.text = element_text(size = 12)) +   # Adjust the font size for axis tick labels
  theme(plot.title = element_text(hjust = 0.5))



ggsave('Plots/en_top_15.pdf', height = 8, width = 10)

