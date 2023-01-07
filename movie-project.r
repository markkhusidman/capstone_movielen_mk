##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)

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


print(edx$genres[str_detect(edx$genres, "[A-Z][a-z]+|
+            [A-Z][a-z]+\\|+[A-Z][a-z]", negate = TRUE)])
print(edx$title[str_detect(edx$title, "\\s\\(\\d+\\)$", negate = TRUE)])

# temp <- createDataPartition(y = edx$rating, times = 1, p = 0.15, list = FALSE)
# edx <- edx[temp,]

# Genres 6, 7, and 8 collectively account for only 0.868% of data
edx <- edx |>separate(genres, 
                      c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5"), 
                      sep = "\\|", remove = FALSE, extra = "drop") |> 
  mutate(n_genre = rowSums(str_split(genres, "\\|", simplify = TRUE) != ""))

iso <- as_datetime(edx$timestamp)

edx <- edx |> mutate(ts_year=year(iso), ts_month=month(iso), ts_day=day(iso), 
                     ts_hour=hour(iso))

m_year <- str_match(edx$title, "\\s\\((\\d+)\\)$")[,2]
era_len <- 5
m_era = floor(as.integer(m_year) / era_len) * era_len

edx <- edx |> mutate(movie_year = year(mdy(paste("1-1-", m_year))), 
                     movie_era = as.character(m_era))

str(edx)
print(edx |> head(15))

rm(dl, ratings, movies, test_index, temp, movielens, removed, iso, m_year, 
   m_era)
