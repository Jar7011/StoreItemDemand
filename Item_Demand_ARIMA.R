library(modeltime)
library(timetk)
library(tidyverse)
library(tidymodels)
library(vroom)

# Read in the data
train_data <- vroom('train.csv')
test_data <- vroom('test.csv')

# Get two store-item combinations
train_combination_1 <- train_data %>% 
  filter(store == 1, item == 1)
train_combination_2 <- train_data %>% 
  filter(store == 5, item == 10)

test_combination_1 <- test_data %>% 
  filter(store == 1, item == 1)
test_combination_2 <- test_data %>% 
  filter(store == 5, item == 10)


# Create the CV split
cv_split_1 <- time_series_split(train_combination_1, assess="3 months", cumulative = TRUE)
cv_split_2 <- time_series_split(train_combination_2, assess="3 months", cumulative = TRUE)

# Create ARIMA recipe
