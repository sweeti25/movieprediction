library(tidyverse)
setwd("C:/Users/aakam/Desktop/Classes/Spring'24/Data Mining and Predictive Analytics/Mini Project")

movie_data <- read_csv('movie_training.csv') 

summary(movie_data)

############### Cleaning Data ##################

data_clean <- movie_data %>% 
  select(gross, title_year, budget, IMDB_score, genres, 
         duration, country, content_rating,
         movie_title) %>%
  filter(!is.na(IMDB_score)) %>%
  mutate(gross = ifelse(is.na(gross), mean(gross, na.rm=TRUE), gross)/1000000,
         budget = ifelse(is.na(budget), mean(budget, na.rm=TRUE), budget)/1000000,
         duration = ifelse(is.na(duration), mean(duration, na.rm=TRUE), duration)) %>%
  separate_wider_delim(genres, names = "genre", delim="|", too_many = "drop") %>%
  group_by(genre) %>% 
  mutate(n_movies = n(),
         genre = ifelse(n_movies <= 25, 'Other', genre),
         genre = as.factor(genre)) %>%
  ungroup() %>%
  group_by(content_rating) %>% 
  mutate(n_movies = n(),
         rating = ifelse(n_movies <= 25, 'Other', content_rating),
         rating = ifelse(rating %in% c("Not Rated", "Unrated"), 'Other', rating),
         rating = ifelse(is.na(rating), 'Other', rating),
         rating = as.factor(rating)) %>%
  ungroup() %>%
  select(-content_rating) %>%
  group_by(rating) %>%
  mutate(avg_budget = mean(budget),
         expensive = as.factor(ifelse(budget > avg_budget, "YES", "NO"))) %>%
  select(movie_title, gross, budget, title_year, duration, genre, rating, expensive, 
         IMDB_score)

summary(data_clean)

############### Linear Regression Models #################

# linear regression only on our numerical variables
model1 <- lm(data = data_clean, IMDB_score ~ gross + title_year + budget)
summary(model1)


# Add in a categorical variable
model2 <- lm(data = data_clean, IMDB_score ~ gross + title_year + budget + rating)
summary(model2)

# Adding an interaction effect between gross and rating to regression.
model3 <- lm(data = data_clean, IMDB_score ~ gross + title_year + budget + rating+ gross*rating)
summary(model3)

############### Linear Regression for Prediction #################

# We can use a model to predict IMDB_score, either on a single data point or a whole other data set
# Let's say I made a movie called "I Love Data Mining"
# I need to make a new dataframe with one row, containing the information for my movie.
dm_movie <- data.frame(gross = 1000, title_year = 2023, budget = 1)

# I can make predictions using any of the models above using the predict() function
# Specify the model name and the dataset to make predictions for.
est_IMDB1 <- predict(model1, newdata = dm_movie)

# Use model2 to make a prediction for "I Love Data Mining."
dm_movie_1 <- data.frame(gross = 1000, title_year = 2023, budget = 1, rating = "G")

est_IMDB2<- predict(model2, newdata = dm_movie_1)

############### Training Linear Regression Models for Prediction #################

train_insts <- sample(nrow(data_clean), .7*nrow(data_clean))

data_train <- data_clean[train_insts,]
data_valid <- data_clean[-train_insts,]

# Goal here is to estimate predictive performance on the validation set.
# We want to compare three model specifications from above - need to train on the train instances only.
model1_tr <- lm(data = data_train, IMDB_score ~ gross + title_year + budget )
model2_tr <- lm(data = data_train, IMDB_score ~ gross + title_year + budget + rating)
model3_tr <- lm(data = data_train, IMDB_score ~ gross + title_year + budget + rating + gross*rating)


# Comparing model1_tr to original model1
model1_tr
model1
# (model specification vs. learned/trained model): model1 and model1_tr have the same specification
# but they are different learned models since they used different data

# Make predictions and calculate RMSE on validation data for model1_tr
m1_predict <- predict(model1_tr, newdata = data_valid)
m1_RMSE <- sqrt(mean((m1_predict - data_valid$IMDB_score)^2))


# Computing the RMSE for model2_tr and model3_tr in the validation data.
m2_predict <- predict(model2_tr, newdata = data_valid)
m2_RMSE <- sqrt(mean((m2_predict - data_valid$IMDB_score)^2))

m3_predict <- predict(model3_tr, newdata = data_valid)
m3_RMSE <- sqrt(mean((m3_predict - data_valid$IMDB_score)^2))

# Comparing the three models' performance in terms of MAE - mean absolute error.

model1_MAE <- print(mean(abs(m1_predict - data_valid$IMDB_score)))
model2_MAE <- print(mean(abs(m2_predict - data_valid$IMDB_score)))
model3_MAE <- print(mean(abs(m3_predict - data_valid$IMDB_score)))

# Model 3 is better for better prediction of IMDB Scores