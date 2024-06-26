# Erasmus Padova
# Statistical Methods for High-Dimensional Data

# Project
rm(list = ls())

### Import libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(corrplot)


### Load the data
user = 'Theresa'
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
#credits = read.csv("credits.csv", header=TRUE)

###Pre-processing 

data_raw = data_raw %>%
  # remove redundant id variables
  select(-c(id,imdb_id, description)) %>% 
  # remove movies/films with no genre, country and year
  filter(genres !='[]' & production_countries != '[]') %>%
  # transform variables into factors
  mutate(type = as.factor(type),
         age_certification = as.factor(age_certification)) %>%
  # extract genre as binary features
  mutate(drama = ifelse(grepl('drama',genres), 1, 0),
         comedy = ifelse(grepl('comedy',genres), 1, 0),
         documentation = ifelse(grepl('documentation',genres), 1, 0),
         horror = ifelse(grepl('horror',genres), 1, 0),
         crime = ifelse(grepl('crime',genres), 1, 0),
         action = ifelse(grepl('action',genres), 1, 0),
         thriller = ifelse(grepl('thriller',genres), 1, 0),
         fantasy = ifelse(grepl('fantasy',genres), 1, 0),
         romance = ifelse(grepl('romance',genres), 1, 0),
         history = ifelse(grepl('history',genres), 1, 0),
         scifi = ifelse(grepl('scifi',genres), 1, 0),
         animation = ifelse(grepl('animation',genres), 1, 0),
         reality = ifelse(grepl('reality',genres), 1, 0),
         sport = ifelse(grepl('sport',genres), 1, 0),
         family = ifelse(grepl('family',genres), 1, 0),
         music = ifelse(grepl('music',genres), 1, 0),
         war = ifelse(grepl('war',genres), 1, 0),
         western = ifelse(grepl('western',genres), 1, 0)) %>%
  mutate(isNotUS = ifelse(production_countries %in% c("['US']"), 0, 1)) #%>%
  #mutate(age_certification = ifelse(age_certification == '', NA, as.factor(age_certification)))




# Dividing our dataset in Movies and TV Shows
data_mov <- select(data_raw[data_raw$type == "MOVIE", ], -c(seasons))
data_sho <- data_raw[data_raw$type == "SHOW", ]


#omit NAs from the dataset
data_mov<- na.omit(data_mov)

write.csv(data_mov, file = 'data_movie.csv')
write.csv(data_sho, file = 'data_show.csv')


# Insert scaling
data_movie_scaled <- data_mov %>%
  select(-c(type,genres, production_countries)) %>%
  mutate(release_year = scale(release_year),
         runtime = scale(runtime),
         imdb_votes = scale(imdb_votes),
         tmdb_score = scale(tmdb_score),
         tmdb_popularity = scale(tmdb_popularity)) %>%
  mutate(age_certification = as.factor(age_certification))

# Omit NAs
data_movie_scaled <- na.omit(data_movie_scaled)

# Save scaled data set
write.csv(data_movie_scaled, file = 'data_movie_scaled.csv')

## Train-/Test-Split
# set.seed(1234)
# random <- sample(1:nrow(data_movie_scaled), ceiling(0.8*dim(data_movie_scaled)[1]))
# train_movie <- data_movie_scaled[random,] 
# test_movie <- data_movie_scaled[-random,]

########################################
###        FIRST IMPRESSIONS         ###
########################################

## Release year
hist(data_raw$release_year, breaks = c(1945:2022), xlab = 'Year of Release', main = 'Year of release for movies and shows')

hist(data_mov$release_year, breaks = c(1945:2022), xlab = 'Year of Release', main = 'Year of release for movies')
hist(data_sho$release_year, breaks = c(1945:2022), xlab = 'Year of Release', main = 'Year of release for shows')

## Types
table(data_raw$type)
par(mfrow = c(1,2))
boxplot(data_mov$runtime, xlab = 'Movies', ylab = 'run time', ylim = c(0,250))
boxplot(data_sho$runtime, xlab = 'Shows', ylab = 'run time', ylim = c(0,250))
par(mfrow = c(1,1))

## Genres
column_sums <- colSums(data_raw[, c(12:29)])
genere_plot <- data.frame(
  column_names = names(column_sums),
  column_sums = column_sums
)
genere_plot$column_names <- reorder(genere_plot$column_names, -genere_plot$column_sums)

ggplot(genere_plot, aes(x = column_names, y = column_sums)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Genre", y = "Number of movies/shows") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('Distribution of genres')

## Seasons
hist(data_sho$seasons, breaks = c(1:43), xlab = 'Number of seasons', main = 'Number of seasons for shows')

## Runtime
par(mfrow = c(1, 2))
boxplot(data_mov$runtime, xlab = "Movies runtime", ylab = "Minutes")
boxplot(data_sho$runtime, xlab = "Shows runtime", ylab = "Minutes")
par(mfrow = c(1, 1))

## Age Certification
ageCert_mov <- count(data.frame(age_certification = data_mov$age_certification), age_certification)

ageCert_sho <- count(data.frame(age_certification = data_sho$age_certification), age_certification)

plot_grid(
ggplot(ageCert_mov, aes(x = ageCert_mov$age_certification, y = ageCert_mov$n) ) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Certified age for movies", y = "Frequency"),

ggplot(ageCert_sho, aes(x = ageCert_sho$age_certification, y = ageCert_sho$n) ) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Certified age for shows", y = "Frequency"),

nrow = 2)

## IMDB Scores 
par(mfrow = c(2, 2))
boxplot(data_mov$imdb_score, xlab="Movies", ylab="IMDB Score", ylim = c(0,10))
boxplot(data_sho$imdb_score, xlab="Shows", ylab="IMDB Score", ylim = c(0,10))
boxplot(data_mov$imdb_votes, xlab="Movies", ylab="Number of Votes")
boxplot(data_sho$imdb_votes, xlab="Shows", ylab="Number of Votes")
par(mfrow = c(1,1))

# IMDB sores (Movies + Shows)
imdb_plot <- data_raw[is.na(data_raw$imdb_score) == 0 & is.na(data_raw$imdb_votes) == 0, c("imdb_score", "imdb_votes")]

# Average IMDB scores (Movie + Shows)
num_imdb_votes <- matrix(nrow = 10)
mean_imdb_votes <- matrix(nrow = 10)
lbl_imdb_votes <- matrix(nrow = 10)

for (i in seq(1, 10, by=1)){
  num_imdb_votes[i] <- sum(imdb_plot[!is.na(imdb_plot$imdb_votes) & imdb_plot$imdb_score > i-1 & imdb_plot$imdb_score <= i,]$imdb_votes)
  mean_imdb_votes[i] <- mean(imdb_plot[!is.na(imdb_plot$imdb_votes) & imdb_plot$imdb_score > i-1 & imdb_plot$imdb_score <= i,]$imdb_votes)
  lbl_imdb_votes[i] <- paste(i-0.99, i, sep = "-")
}
imdb_mean_plot <- data.frame(mean_imdb_votes, num_imdb_votes, plot_sizes = num_imdb_votes/max(num_imdb_votes), lbl_imdb_votes)
imdb_mean_plot <- imdb_mean_plot[!is.na(imdb_mean_plot$mean_imdb_votes) & !is.na(imdb_mean_plot$num_imdb_votes),]


# Plotting Time! 
ggplot(imdb_plot, aes(x = imdb_plot$imdb_votes, y = imdb_plot$imdb_score)) +
  geom_point(color = "orange") +
  # Average Number of voting per IMDB score
  geom_point(data = imdb_mean_plot, 
             aes(x = imdb_mean_plot$mean_imdb_votes, y = c(as.integer(rownames(imdb_mean_plot))) ),
             color="red", size= 20*imdb_mean_plot$plot_sizes) +
  # Labels for the number of votes in each score range
  #geom_label(data = imdb_mean_plot, aes(x = imdb_mean_plot$mean_imdb_votes, y = c(as.integer(rownames(imdb_mean_plot))), 
  #            label = imdb_mean_plot$num_imdb_votes), vjust = 0, hjust = -0.2) +
  geom_label_repel(data = imdb_mean_plot, 
                   aes(x = imdb_mean_plot$mean_imdb_votes, y = c(as.integer(rownames(imdb_mean_plot))) ),
                   label = imdb_mean_plot$lbl_imdb_votes,
                   box.padding   = 0.5, point.padding = 0,
                   segment.color = 'black') +
  # Name of the graph and axis
  labs(title ="Average number of voting per IMDB score", x = "Number of votes", y = "IMDB Score") +
  # Zoomed view
  xlim(0, 300000) +
  scale_y_continuous(name = "IMDB Score", breaks = seq(0, 10, by = 1))
  
# IMDB Scores (Movies)
imdb_plot_mov <- data.frame(data_mov[!is.na(data_mov$imdb_score) & !is.na(data_mov$imdb_votes),
                                     c("imdb_score", "imdb_votes")])
# Average IMDB Scores (Movies)
num_imdb_votes_mov <- matrix(nrow = 10)
mean_imdb_votes_mov <- matrix(nrow = 10)

for (i in seq(1, 10, by=1)){
  num_imdb_votes_mov[i] <- sum(imdb_plot_mov[!is.na(imdb_plot_mov$imdb_votes) & imdb_plot_mov$imdb_score > i-1 & imdb_plot_mov$imdb_score <= i,]$imdb_votes)
  mean_imdb_votes_mov[i] <- mean(imdb_plot_mov[!is.na(imdb_plot_mov$imdb_votes) & imdb_plot_mov$imdb_score > i-1 & imdb_plot_mov$imdb_score <= i,]$imdb_votes)
}
imdb_mean_plot_mov <- data.frame(mean_imdb_votes_mov, num_imdb_votes_mov, plot_sizes = num_imdb_votes_mov/max(num_imdb_votes_mov))
imdb_mean_plot_mov <- imdb_mean_plot_mov[!is.na(imdb_mean_plot_mov$mean_imdb_votes) & !is.na(imdb_mean_plot_mov$num_imdb_votes),]

ggplot(imdb_plot_mov, aes(x = imdb_plot_mov$imdb_votes, y = imdb_plot_mov$imdb_score)) +
  geom_point(color = "green") +
  
  geom_point(data = imdb_mean_plot_mov, aes(x = imdb_mean_plot_mov$mean_imdb_votes_mov, y = c(as.integer(rownames(imdb_mean_plot_mov)))), 
             color = "darkgreen", size = 20*imdb_mean_plot_mov$plot_sizes ) +
  labs(title = "Average number of voting per IMDB score for Movies", x = "Number of votes", y = "IMDB Score") +
  # Zoomed view
  xlim(0, 300000) +
  scale_y_continuous(breaks = c(1:10))
  
# IMDB Score (Shows)
imdb_plot_sho <- data.frame(data_sho[!is.na(data_raw$imdb_score) & !is.na(data_raw$imdb_votes),
                                     c("imdb_score", "imdb_votes")])
# Average IMDB Scores (Shows)
num_imdb_votes_sho <- matrix(nrow = 10)
mean_imdb_votes_sho <- matrix(nrow = 10)

for (i in seq(1, 10, by=1)){
  num_imdb_votes_sho[i] <- sum(imdb_plot_sho[!is.na(imdb_plot_sho$imdb_votes) & imdb_plot_sho$imdb_score > i-1 & imdb_plot_sho$imdb_score <= i,]$imdb_votes)
  mean_imdb_votes_sho[i] <- mean(imdb_plot_sho[!is.na(imdb_plot_sho$imdb_votes) & imdb_plot_sho$imdb_score > i-1 & imdb_plot_sho$imdb_score <= i,]$imdb_votes)
}
imdb_mean_plot_sho <- data.frame(mean_imdb_votes_sho, num_imdb_votes_sho, plot_sizes = num_imdb_votes_sho/max(num_imdb_votes_sho))
imdb_mean_plot_sho <- imdb_mean_plot_sho[!is.na(imdb_mean_plot_sho$mean_imdb_votes) & !is.na(imdb_mean_plot_sho$num_imdb_votes),]

ggplot(imdb_plot_sho, aes(x = imdb_plot_sho$imdb_votes, y = imdb_plot_sho$imdb_score)) +
  geom_point(color = "skyblue") +
  
  geom_point(data = imdb_mean_plot_sho, aes(x = imdb_mean_plot_sho$mean_imdb_votes_sho, y = c(as.integer(rownames(imdb_mean_plot_sho)))), 
             color = "blue", size = 20*imdb_mean_plot_sho$plot_sizes ) +
  labs(title = "Average number of voting per IMDB score for Movies", x = "Number of votes", y = "IMDB Score") +
  # Zoomed view
  xlim(0, 300000) +
  scale_y_continuous(breaks = c(1:10))


########################################
###        CORRELATION MATRIX        ###
########################################


#remove NA values from the data-frames
data_mov<- na.omit(data_mov)
data_sho<- na.omit(data_sho)

#remove non-numerical features: description, genre, production_countries, type and title 
data_mov_num <- data_mov %>% select(-c("title", "genres", "production_countries",
                                       "type","age_certification"))
data_sho_num <- data_sho %>% select(-c("title", "genres", "production_countries",
                                       "type","age_certification"))



#write.csv(data_mov_num, file = 'data_movie_num.csv')
#write.csv(data_mov_num, file = 'data_show_num.csv')

corr_matrix_mov<- cor(data_mov_num)
corrplot_mov <- corrplot(corr_matrix_mov,
                         method = "number",
                         diag = TRUE,
                         tl.cex = 0.4,
                         number.cex = 0.5,
                         tl.col = "black")

corr_matrix_sho<- cor(data_sho_num)
corrplot_sho <- corrplot(corr_matrix_sho,
                         method = "number",
                         diag = TRUE,
                         tl.cex = 0.4,
                         number.cex = 0.5,
                         tl.col = "black")

