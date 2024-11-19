library(tidymodels)
library(tidyverse)
library(vroom)
library(bonsai)
library(themis)
library(embed)

# Read in the data
train_data <- vroom('train.csv')
test_data <- vroom('test.csv')

# Get a store-item combination
store_item <- train_data %>% 
  filter(store == 1, item == 1)

# Create recipe
boost_recipe <- recipe(sales ~ ., data = store_item) %>% 
  step_date(date, features = c('dow', 'month', 'doy')) %>% 
  step_range(date_doy, min = 0, max = pi) %>% 
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>% 
  step_rm(c('store', 'item', 'date')) %>%
  step_lencode_glm(all_nominal_predictors(), outcome = vars(sales))
  
# Create model 
boost_model <- boost_tree(tree_depth = tune(),
                          trees = tune(),
                          learn_rate = tune()) %>% 
  set_engine('xgboost') %>% 
  set_mode('regression')

# Create workflow
boost_wf <- workflow() %>% 
  add_recipe(boost_recipe) %>% 
  add_model(boost_model)

# Grid of values to tune over
tuning_grid <- grid_regular(tree_depth(), trees(), learn_rate(), levels = 5)

# Split data for CV
folds <- vfold_cv(store_item, v = 10, repeats = 1)

# Run the CV
cv_results_best_tuned <- boost_wf %>% 
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(smape)) %>% 
  show_best(metric = 'smape', n = 1)