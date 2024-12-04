library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(vroom)
library(embed)
library(bonsai)
library(lightgbm)

# Read in the data
train_data <- vroom('train.csv')
test_data <- vroom('test.csv')

# Get number of stores and items
n_stores <- max(train_data$store)
n_items <- max(train_data$item)

# Create recipe
boost_recipe <- recipe(sales~., data=train_data) %>%
  step_date(date, features=c("dow", "month", "decimal", "doy", "year")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome=vars(sales)) %>%
  step_rm(date, item, store) %>%
  step_normalize(all_numeric_predictors())

# Create model 
boost_model <- boost_tree(tree_depth = 2,
                          trees = 1000,
                          learn_rate = 0.01) %>% 
  set_engine('lightgbm') %>% 
  set_mode('regression')

# Create workflow
boost_wf <- workflow() %>% 
  add_recipe(boost_recipe) %>% 
  add_model(boost_model)

# Loop through each combination
for (s in 1:n_stores) {
  for (i in 1:n_items) {
    train_combination <- train_data %>% 
      filter(store == s, item == i)
    test_combination <- test_data %>% 
      filter(store == s, item == i)
    
    # Fit data
    final_wf <- boost_wf %>% 
      fit(data = train_combination)
    
    # Forecast
    preds <- predict(final_wf, new_data = test_combination) %>% 
      bind_cols(test_combination) %>% 
      rename(sales = .pred) %>% 
      select(id, sales)
    
    
    # Bind results
    if (s == 1 & i == 1) {
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
}

# Write out to file
vroom_write(all_preds, file="submission.csv", delim=",")