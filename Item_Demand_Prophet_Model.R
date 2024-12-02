library(modeltime)
library(timetk)
library(tidyverse)
library(tidymodels)
library(vroom)
library(plotly)

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


# Create the CV split and graphs
cv_split_1 <- time_series_split(train_combination_1, assess="3 months", cumulative = TRUE)
cv_plot_1 <- cv_split_1 %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split_2 <- time_series_split(train_combination_2, assess="3 months", cumulative = TRUE)
cv_plot_2 <- cv_split_2 %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

# Define prophet models
prophet_model_1 <- prophet_reg() %>%
  set_engine(engine = 'prophet') %>% 
  fit(sales ~ date, data = training(cv_split_1))

prophet_model_2 <- prophet_reg() %>%
  set_engine(engine = 'prophet') %>% 
  fit(sales ~ date, data = training(cv_split_2))

# Tune the models
cv_results_1 <- modeltime_calibrate(prophet_model_1, new_data = testing(cv_split_1))
cv_results_2 <- modeltime_calibrate(prophet_model_2, new_data = testing(cv_split_2))

# Visualize results
cv_results_1_plot <- cv_results_1 %>% 
  modeltime_forecast(new_data = testing(cv_split_1),
                     actual_data = training(cv_split_1)) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

cv_results_2_plot <- cv_results_2 %>% 
  modeltime_forecast(new_data = testing(cv_split_2),
                     actual_data = training(cv_split_2)) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

# Refit to whole dataset
fullfit_1 <- cv_results_1 %>% 
  modeltime_forecast(new_data = test_combination_1,
                     actual_data = train_combination_1) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

fullfit_2 <- cv_results_2 %>% 
  modeltime_forecast(new_data = test_combination_2,
                     actual_data = train_combination_2) %>% 
  plot_modeltime_forecast(.interactive = FALSE)

# Create a 4-panel plot
plot <- subplot(cv_results_1_plot, cv_results_2_plot,
                fullfit_1, fullfit_2, nrows = 2)