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

print(edx$genres[str_detect(edx$genres, "[A-Z][a-z]+|
+            [A-Z][a-z]+\\|+[A-Z][a-z]", negate = TRUE)])
print(edx$title[str_detect(edx$title, "\\s\\(\\d+\\)$", negate = TRUE)])

# temp <- createDataPartition(y = edx$rating, times = 1, p = 0.15, list = FALSE)
# edx <- edx[temp,]

# Genres 6, 7, and 8 collectively account for only 0.868% of data

# data.table method to separate genres column is 16.267 times faster than  
# analogous tidyverse method (separate)

setDT(edx)
setDT(final_holdout_test)

# Note that one title has two associated movie IDs. We assume this to be an 
# error

print(edx[, .(n = length(unique(movieId))), by = title][n > 1,])

# Find the most common movie Id associated with this title and replace instances
# of the less common ID

primary_id <- edx[title == "War of the Worlds (2005)", 
                  length(title), keyby=movieId][1, movieId]

edx$movieId[which(edx$title == "War of the Worlds (2005)")] <- primary_id
final_holdout_test$movieId[which(final_holdout_test$title == "War of the Worlds (2005)")] <- primary_id

# Convert movieId to factor
edx[, movieId := factor(movieId)]
final_holdout_test[, movieId := factor(movieId)]

# Separate genres and calculate number of genres given. Convert genres to 
# factors

edx[, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5") := 
      tstrsplit(edx$genres, "|", fixed=TRUE, fill = "None", keep = 1:5)]

edx[, n_genres := factor(rowSums(.SD != "None")), .SDcols = 
      c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5")]

edx[, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5") :=
      list(factor(genre_1), factor(genre_2), factor(genre_3), factor(genre_4),
           factor(genre_5))]

final_holdout_test[, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5") := 
                     tstrsplit(final_holdout_test$genres, "|", fixed=TRUE, fill = "None", keep = 1:5)]

final_holdout_test[, n_genres := factor(rowSums(.SD != "None")), .SDcols = 
                     c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5")]

final_holdout_test[, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5") :=
                     list(factor(genre_1), factor(genre_2), factor(genre_3), factor(genre_4),
                          factor(genre_5))]


# Extract time-stamp data
iso <- as_datetime(edx$timestamp)

edx[, c("ts_year", "ts_month", "ts_day", "ts_hour") := 
      list(factor(year(iso)), factor(month(iso)), 
           factor(day(iso)), factor(hour(iso)))]

iso <- as_datetime(final_holdout_test$timestamp)

final_holdout_test[, c("ts_year", "ts_month", "ts_day", "ts_hour") := 
                     list(factor(year(iso)), factor(month(iso)), 
                          factor(day(iso)), factor(hour(iso)))]


# Extract movie year from title

m_year <- str_match(edx$title, "\\s\\((\\d+)\\)$")[,2]
edx[, movie_year := factor(year(mdy(paste("1-1-", m_year))))]

m_year2 <- str_match(final_holdout_test$title, "\\s\\((\\d+)\\)$")[,2]
final_holdout_test[, movie_year := factor(year(mdy(paste("1-1-", m_year2))))]


# Calculate and remove biases

global_mean <- mean(edx$rating)
edx[, unbiased := rating - global_mean]

# elen_pars <- seq(5, 25, 10)
# gsize_pars <- seq(20, 100, 40)
# lambda_a_pars <- seq(5, 505, 100)
# lambda_b_pars <- seq(3, 12, 3)

elen_pars <- c(25)
gsize_pars <- c(60)
lambda_a_pars <- c(5)
lambda_b_pars <- c(6)

params <- expand.grid(elen_pars, gsize_pars, lambda_a_pars, lambda_b_pars)
names(params) <- c("elen_pars", "gsize_pars", "lambda_a_pars", "lambda_b_pars")
params$rmse <- NaN
params$sd <- NaN




# for(row in 1:nrow(params)){
#   print(params[row,])
#   print("---------------------------------------------------------------")
#   rmses <- c()
#   
#   # Extract movie era from title
#   era_len <- params$elen_pars[row]
#   edx[, movie_era := factor(floor(as.integer(m_year) / era_len) * era_len)]
#   
#   # Make sure all variables to be used as join keys are factors or characters
#   edx[, userId := factor(userId)]
#   
#   # Extract user group
#   group_size = params$gsize_pars[row]
#   edx[, user_group := factor(ceiling(as.integer(userId) / group_size))]
#   
#   # Determine which features should be used to calculate "nrev" biases
#   # print(apply(edx, 2, function(v){length(unique(v))}))
#   # str(edx)
#   
#   
#   lambda_a = params$lambda_a_pars[row]
#   lambda_b = params$lambda_b_pars[row]
#   
#   for(i in 1:3){
#     validate_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
#     train <- edx[-validate_index,]
#     validate <- edx[validate_index,]
#     # temp <- copy(validate)
#     
#     # for(sj_col in names(train)){
#     #   validate <- semi_join(validate, train, by = sj_col)
#     # }
#     
#     validate <- validate |>
#       semi_join(train, by = "movieId") |> semi_join(train, by = "userId")
#     
#     # removed <- anti_join(temp, validate, by = c("userId", "movieId"))
#     # train <- rbind(train, removed)
#     
#     # print(sum(apply(is.na(validate), 1, any)))
#     # print("Starting bias column joins")
#     
#     for(col in c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5", "n_genres",
#                  "movie_era", "movie_year", "ts_hour", "ts_day", "ts_month", 
#                  "ts_year")){
#     
#       train[, paste0(col, "_bias") := sum(unbiased) / (length(unbiased) + lambda_a),
#           by = col]
#     
#       train[, unbiased := unbiased - .SD, .SDcols = paste0(col, "_bias")]
#       
#       # validate <- validate[train[, .SD[1], .SDcols = paste0(col, "_bias"), by= col], on = col, nomatch=NULL]
#       temp <- train[, .SD[1], .SDcols = paste0(col, "_bias"), by= col]
#       validate <- temp[validate, on = col]
#       # validate <- validate[complete.cases(validate)]
#       # print(col)
#       # print(sum(apply(is.na(validate), 1, any)))
#     }
#     
#     for(col in c("genres", "movieId", "user_group", "userId")){
# 
#       train[, paste0(col, "_nrev") := ceiling(length(unbiased) / 100), by = col]
# 
#       train[, paste0(col, "_nrev_bias") :=
#             sum(unbiased) / (length(unbiased) + lambda_b),
#           by = eval(paste0(col, "_nrev"))]
# 
#       train[, unbiased := unbiased - .SD, .SDcols = paste0(col, "_nrev_bias")]
# 
#       train[, paste0(col, "_bias") := sum(unbiased) / (length(unbiased) + lambda_b),
#           by = col]
# 
#       train[, unbiased := unbiased - .SD, .SDcols = paste0(col, "_bias")]
#       train[, paste0(col, "_nrev") := NULL]
#       
#       # validate <- validate[train[, .SD[1], .SDcols = paste0(col, c("_bias", "_nrev_bias")), by= col], on = col, nomatch=NULL]
#       temp <- train[, .SD[1], .SDcols = paste0(col, c("_bias", "_nrev_bias")), by = col]
#       validate <- temp[validate, on = col]
#       # validate <- validate[complete.cases(validate)]
#       # print(col)
#       # print(sum(apply(is.na(validate), 1, any)))
#     }
#   
#     # print(names(validate))
#     col_names <- names(train)
#     bias_cols <- col_names[str_detect(col_names, "_bias")]
#     validate[, pred := rowSums(.SD) + global_mean, .SDcols = bias_cols]
#     
#     # Clip predictions such that they are between 0 and 5
#     validate[, pred := fifelse(pred > 5, 5, pred)]
#     validate[, pred := fifelse(pred < 0, 0, pred)]
#     
#     rmses[i] <- sqrt(mean((validate$pred - validate$rating)^2))
#     print(rmses[i])
#   }
#   
#   params$rmse[row] <- mean(rmses)
#   params$sd[row] <- sd(rmses)
# }
# 
# print(params[order(params$rmse),])
# # fwrite(params, "temp_model-tuning.csv")





# Calculate centered mean rating, standard deviation, and number of reviews for
# all combinations of genre (taking into account genre order) and user

# for(col in c("genres", "genre_1", "genre_2", "genre_3", "genre_4", "genre_5")){
#   edx[, paste0(col, c("_user_avg_rel", "_user_sdev", "_user_nrev")) 
#       := list(mean(rating), sd(rating), length(rating)), by = c("userId", col)]
# }


# Calculate mean rating, standard deviation, and number of reviews
# for each user across all genres

# edx[, c("user_avg_rel", "user_sdev", "user_nrev") := 
#       list(mean(rating), sd(rating), length(rating)), by = userId]

# str(edx)
# print(edx |> head(10))

# temp <- t(edx[, lapply(.SD, function(v){c(sd(v), mean(v))}), .SDcols = colnames(edx[, unbiased:userId_bias])])
# colnames(temp) <- c("sd", "mean")
# temp2 <- temp
# temp2[, 2] <- log(abs(temp2[, 2]))
# as.data.frame(temp2) |> ggplot(aes(sd, mean, label = rownames(temp2))) + geom_point() + geom_text()
# temp
# edx[, prelim := rating - global_mean]
# temp3 <- edx[, .(initial = sum(prelim) / (length(prelim) + 20), final = sum(unbiased) / (length(unbiased) + 20) ), by = "genre_1"]
# temp3[, ratio := initial/final]
# temp3

# Return rows with one or more empty values
# validate[which(apply(is.na(validate), 1, any)),]




# Extract movie era from title
era_len <- 25
edx[, movie_era := factor(floor(as.integer(m_year) / era_len) * era_len)]
final_holdout_test[, movie_era := factor(floor(as.integer(m_year2) / era_len) * era_len)]

# Make sure all variables to be used as join keys are factors or characters
edx[, userId := factor(userId)]
final_holdout_test[, userId := factor(userId)]

# Extract user group
group_size = 60
edx[, user_group := factor(ceiling(as.integer(userId) / group_size))]
final_holdout_test[, user_group := factor(ceiling(as.integer(userId) / group_size))]

# Determine which features should be used to calculate "nrev" biases
# print(apply(edx, 2, function(v){length(unique(v))}))
# str(edx)

lambda_a = 5
lambda_b = 6

for(col in c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5", "n_genres",
             "movie_era", "movie_year", "ts_hour", "ts_day", "ts_month", 
             "ts_year")){
  
  edx[, paste0(col, "_bias") := sum(unbiased) / (length(unbiased) + lambda_a),
      by = col]
  
  edx[, unbiased := unbiased - .SD, .SDcols = paste0(col, "_bias")]
  
  temp <- edx[, .SD[1], .SDcols = paste0(col, "_bias"), by= col]
  final_holdout_test <- temp[final_holdout_test, on = col]
  
}

for(col in c("movieId", "userId")){
  
  edx[, paste0(col, "_nrev") := ceiling(length(unbiased) / 100), by = col]
  
  edx[, paste0(col, "_nrev_bias") :=
        sum(unbiased) / (length(unbiased) + lambda_b),
      by = eval(paste0(col, "_nrev"))]
  
  edx[, unbiased := unbiased - .SD, .SDcols = paste0(col, "_nrev_bias")]
  
  edx[, paste0(col, "_bias") := sum(unbiased) / (length(unbiased) + lambda_b),
      by = col]
  
  edx[, unbiased := unbiased - .SD, .SDcols = paste0(col, "_bias")]
  edx[, paste0(col, "_nrev") := NULL]
  
  # validate <- validate[train[, .SD[1], .SDcols = paste0(col, c("_bias", "_nrev_bias")), by= col], on = col, nomatch=NULL]
  temp <- edx[, .SD[1], .SDcols = paste0(col, c("_bias", "_nrev_bias")), by = col]
  final_holdout_test <- temp[final_holdout_test, on = col]
  # validate <- validate[complete.cases(validate)]
  # print(col)
  # print(sum(apply(is.na(validate), 1, any)))
}

col_names <- names(edx)
bias_cols <- col_names[str_detect(col_names, "_bias")]
final_holdout_test[, pred := rowSums(.SD) + global_mean, .SDcols = bias_cols]

# Clip predictions such that they are between 0 and 5
final_holdout_test[, pred := fifelse(pred > 5, 5, pred)]
final_holdout_test[, pred := fifelse(pred < 0, 0, pred)]

final_rmse <- sqrt(mean((final_holdout_test$pred - final_holdout_test$rating)^2))
print(final_rmse)
