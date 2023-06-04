##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

print("---------------------------------------------------------------")

# Detect any missing values in data

print(sprintf("Any missing values in edx?: %s", anyNA(edx)))
print(sprintf("Any missing values in holdout?: %s", anyNA(final_holdout_test)))
print("---------------------------------------------------------------")

# Detect any items in the genres column which do not fit the apparent format

print("Genres in edx which do not fit format:")
print(edx$genres[str_detect(
  edx$genres, "^[A-Z][a-z]+|^[A-Z][a-z]+\\|[A-Z][a-z]+", negate = TRUE)])
print("------------")

print("Genres in holdout which do not fit format:")
print(final_holdout_test$genres[str_detect(
  final_holdout_test$genres, "^[A-Z][a-z]+|^[A-Z][a-z]+\\|[A-Z][a-z]+", 
  negate = TRUE)])

print("---------------------------------------------------------------")

# Detect any items in the title column which do not fit the apparent format

print("Titles in edx which do not fit format:")
print(edx$title[str_detect(edx$title, "\\s\\(\\d+\\)$", negate = TRUE)])
print("------------")

print("Titles in holdout which do not fit format:")
print(final_holdout_test$title[str_detect(
  final_holdout_test$title, "\\s\\(\\d+\\)$", negate = TRUE)])

# Visualize rating distributions in edx and holdout

hist(edx$rating, breaks = 20, xlim = c(0, max(edx$rating)), xlab = "Rating", 
     main = "Histogram of Ratings in edx")

hist(final_holdout_test$rating, breaks = 20, xlim = c(0, max(edx$rating)),  
     xlab = "Rating", main = "Histogram of Ratings in holdout")

# Convert edx to data.table object
setDT(edx)

print("---------------------------------------------------------------")

# See if any titles have more than one associated movie ID

print("Titles with more than one movie ID:")
print(edx[, .(id_count = length(unique(movieId))), by = title][id_count > 1,])

# Find the most common movie ID associated with this title and replace instances
# of the less common ID

primary_id <- edx[title == "War of the Worlds (2005)", 
                  length(title), keyby=movieId][1, movieId]

edx$movieId[which(edx$title == "War of the Worlds (2005)")] <- primary_id

print("---------------------------------------------------------------")

# Evaluate simple model for baseline RMSE
global_mean <- mean(edx$rating)
baseline_rmse <- sqrt(mean((global_mean - edx$rating)^2))
print(sprintf("Baseline RMSE: %f", baseline_rmse))

# Visualize proportion of rows containing 6, 7, or 8 listed genres 

genre_count_edx <- str_count(edx$genres, "\\|") + 1
hist(genre_count_edx, breaks = 8, xlab = "Number of Genres", 
     main = "Histogram of Genre Count in edx")

genre_count_holdout <- str_count(final_holdout_test$genres, "\\|") + 1
hist(genre_count_holdout, breaks = 8, xlab = "Number of Genres", 
     main = "Histogram of Genre Count in holdout")

# Separate genres and calculate number of genres given. Convert genres to 
# factors

separate_genres <- function(dt){
  
  dt[, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5") := 
        tstrsplit(dt$genres, "|", fixed=TRUE, fill = "None", keep = 1:5)]
  
  dt[, n_genres := factor(rowSums(.SD != "None")), .SDcols =
        c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5")]
  
  dt[, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5") :=
        list(factor(genre_1), factor(genre_2), factor(genre_3), factor(genre_4),
             factor(genre_5))]
}

separate_genres(edx)

# Extract timestamp-related data

extract_ts <- function(dt){
  iso <- as_datetime(dt$timestamp)
  
  dt[, c("rev_year", "rev_month", "rev_day", "rev_hour") := 
        list(factor(year(iso)), factor(month(iso)), 
             factor(day(iso)), factor(hour(iso)))]
}

extract_ts(edx)

# Extract movie year from title
m_year_edx <- str_match(edx$title, "\\s\\((\\d+)\\)$")[,2]
edx[, movie_year := factor(year(mdy(paste("1-1-", m_year_edx))))]

# Create the initial bias column which will be used to calculate average biases
edx[, rbias := rating - global_mean]

# Hyperparameter values to be used in model-tuning loop. The commented lines
# represent the full range of values tested during model tuning. The 
# uncommented lines contain the best hyperparameter values found. 

# era_len_pars <- seq(5, 25, 5)
# lambda_pars <- 2:8

era_len_pars <- c(15)  # era_len controls era length
lambda_pars <- c(6)  # lambda controls extent of regularization

# Array to hold hyperparameter combinations and results
params <- expand.grid(era_len_pars, lambda_pars)
names(params) <- c("era_len_pars", "lambda_pars")
params$rmse <- NaN
params$sd <- NaN

print("---------------------------------------------------------------")

# Model-tuning loop
print("Starting model-tuning loop")

for(row in 1:nrow(params)){
  print("------------")
  print(sprintf("Testing era_len of %g and lambda of %g:", 
                params[row, 1], params[row, 2]))
  print("------------")
  rmses <- c()

  # Set era_len hyperparameter and extract movie era from movie year
  era_len <- params$era_len_pars[row]
  edx[, movie_era := factor(floor(as.integer(m_year_edx) / era_len) * era_len)]
  
  # Set lambda hyperparameter
  lambda = params$lambda_pars[row]

  for(i in 1:3){
    
    # Split edx dataset into train and validate sets
    
    validate_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, 
                                          list = FALSE)
    train <- edx[-validate_index,]
    temp <- edx[validate_index,]

    validate <- temp |>
      semi_join(train, by = "movieId") |> semi_join(train, by = "userId")

    removed <- anti_join(temp, validate, by = c("userId", "movieId"))
    train <- rbind(train, removed)
    
    # Model-training loop
    
    for(col in c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5", 
                 "n_genres", "movie_era", "movie_year", "rev_hour", "rev_day", 
                 "rev_month", "rev_year", "movieId", "userId")){
      
      # Calculate regularized average biases based on training set
      train[, paste0(col, "_bias") := sum(rbias) / (length(rbias) + lambda),
          by = col]
      
      # Remove effect of average biases from training set rbias column
      train[, rbias := rbias - .SD, .SDcols = paste0(col, "_bias")]
      
      # Join average biases with appropriate values in validation set
      temp <- train[, .SD[1], .SDcols = paste0(col, "_bias"), by= col]
      validate <- temp[validate, on = col]
    }
    
    # Calculate predictions by summing all bias columns with global_mean
    col_names <- names(train)
    bias_cols <- col_names[str_detect(col_names, "_bias")]
    train[, pred := rowSums(.SD) + global_mean, .SDcols = bias_cols]
    validate[, pred := rowSums(.SD) + global_mean, .SDcols = bias_cols]
    
    # Clip predictions such that they are between 0.5 and 5
    train[, pred := fifelse(pred > 5, 5, pred)]
    train[, pred := fifelse(pred < 0.5, 0.5, pred)]
    validate[, pred := fifelse(pred > 5, 5, pred)]
    validate[, pred := fifelse(pred < 0.5, 0.5, pred)]
    
    # Calculate and report training and validation RMSEs
    train_rmse <- sqrt(mean((train$pred - train$rating)^2))
    rmses[i] <- sqrt(mean((validate$pred - validate$rating)^2))
    print(sprintf("Training RMSE: %f  Validation RMSE: %f", 
                  train_rmse, rmses[i]))
  }
  
  # Record mean and SD of RMSEs for each hyperparameter combination
  params$rmse[row] <- mean(rmses)
  params$sd[row] <- sd(rmses)
}
print("------------")
print("Finished model-tuning loop")
print("---------------------------------------------------------------")

# Uncomment below code to record hyperparameter array in csv file
# fwrite(params, "temp_model-tuning2.csv")


# Uncomment below code to visualize relationship between hyperparameter values 
# and average RMSE. Will only work if model-tuning loop is run for multiple 
# hyperparameter combinations.

# params |> ggplot(aes(lambda_pars, rmse, group = lambda_pars)) +
#   geom_boxplot() + geom_point()

# params |> ggplot(aes(era_len_pars, rmse, group = era_len_pars)) +
#   geom_boxplot() + geom_point()


# Preprocess holdout dataset
setDT(final_holdout_test)
final_holdout_test$movieId[
  which(final_holdout_test$title == "War of the Worlds (2005)")] <- primary_id
separate_genres(final_holdout_test)
extract_ts(final_holdout_test)
m_year_fht <- str_match(final_holdout_test$title, "\\s\\((\\d+)\\)$")[,2]
final_holdout_test[, movie_year := factor(year(mdy(paste("1-1-", m_year_fht))))]

# Extract movie era from title using the best era_len value
era_len <- 15
edx[, movie_era := factor(floor(as.integer(m_year_edx) / era_len) * era_len)]
final_holdout_test[, movie_era := 
                     factor(floor(as.integer(m_year_fht) / era_len) * era_len)]

# Set lambda equal to best value
lambda = 6

# Model-training loop for final model

for(col in c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5", "n_genres",
             "movie_era", "movie_year", "rev_hour", "rev_day", "rev_month",
             "rev_year", "movieId", "userId")){
  
  # Calculate regularized average biases based on edx set
  edx[, paste0(col, "_bias") := sum(rbias) / (length(rbias) + lambda),
      by = col]
  
  # Remove effect of average biases from edx rbias column
  edx[, rbias := rbias - .SD, .SDcols = paste0(col, "_bias")]
  
  # Join average biases with appropriate values in holdout set
  temp <- edx[, .SD[1], .SDcols = paste0(col, "_bias"), by= col]
  final_holdout_test <- temp[final_holdout_test, on = col]

}

# Calculate final predictions by summing all bias columns with global_mean
col_names <- names(edx)
bias_cols <- col_names[str_detect(col_names, "_bias")]
final_holdout_test[, pred := rowSums(.SD) + global_mean, .SDcols = bias_cols]

# Clip final predictions such that they are between 0.5 and 5
final_holdout_test[, pred := fifelse(pred > 5, 5, pred)]
final_holdout_test[, pred := fifelse(pred < 0.5, 0.5, pred)]

# Calculate RMSE of final model
final_holdout_test[, error := pred - rating]
final_rmse <- sqrt(mean((final_holdout_test$error)^2))
print(sprintf("Final RMSE: %f", final_rmse))

# Visualize relationship between model inputs and model errors

print(final_holdout_test |> ggplot(aes(movie_era, error, group = movie_era)) + 
  geom_boxplot() + 
  ggtitle("Error Distribution with respect to Movie Era") + 
  xlab("movie era"))

print(final_holdout_test |> ggplot(aes(n_genres, error, group = n_genres)) + 
  geom_boxplot() + 
  ggtitle("Error Distribution with respect to Number of Genres") + 
  xlab("number of genres"))

print(final_holdout_test |> ggplot(aes(rev_year, error, group = rev_year)) + 
  geom_boxplot() + 
  ggtitle("Error Distribution with respect to Review Year") + 
  xlab("review year"))

# Calculate final model's mean error
print(sprintf("Mean model error: %f",mean(final_holdout_test$error)))
