library(modeltime)
library(tidyverse)
library(tidymodels)
library(vroom)
library(timetk)

# Read in the data
train_data <- vroom('train.csv')
test_data <- vroom('test.csv')

# Get number of stores and items
n_stores <- max(train_data$store)
n_items <- max(train_data$item)

# Loop through each combination
for (s in 1:n_stores) {
  for (i in 1:n_items) {
    train_combination <- train_data %>% 
      filter(store == s, item == i)
    test_combination <- test_data %>% 
      filter(store == s, item == i)
    
    # Create the CV split
    cv_split <- time_series_split(train_combination, assess="3 months", cumulative = TRUE)
    
    # Define prophet model
    prophet_model <- prophet_reg() %>%
      set_engine(engine = 'prophet') %>% 
      fit(sales ~ date, data = training(cv_split))
    
    # Tune the model
    cv_results <- modeltime_calibrate(prophet_model, new_data = testing(cv_split))
    
    # Refit to whole dataset
    preds <- cv_results %>% 
      modeltime_forecast(new_data = test_combination,
                         actual_data = train_combination)
    
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