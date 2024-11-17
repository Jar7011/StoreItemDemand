library(tidymodels)
library(tidyverse)
library(vroom)
library(patchwork)
library(forecast)

# Read in the data
train_data <- vroom('train.csv')
test_data <- vroom('test.csv')

# Get two store-item combinations
store_item_1 <- train_data %>% 
  filter(store == 1, item == 1)

store_item_2 <- train_data %>% 
  filter(store == 3, item == 6)

# Create time series plots
time_series_plot_1 <- store_item_1 %>% 
  ggplot(aes(x = date, y = sales)) +
  geom_line() +
  labs(title = 'Store #1 & Item #1') +
  geom_smooth(se=FALSE) +
  theme_minimal()

time_series_plot_2 <- store_item_2 %>% 
  ggplot(aes(x = date, y = sales)) +
  geom_line() +
  labs(title = 'Store #3 & Item #6') +
  geom_smooth(se=FALSE) +
  theme_minimal()

# Create autocorrelation plots
autocorrelation_month_lag_1 <- store_item_1 %>% 
  pull(sales) %>% 
  ggAcf(., lag.max = 30)

autocorrelation_month_lag_2 <- store_item_2 %>% 
  pull(sales) %>% 
  ggAcf(., lag.max = 30)

autocorrelation_2_year_lag_1 <- store_item_1 %>% 
  pull(sales) %>% 
  ggAcf(., lag.max = 2 * 365)

autocorrelation_2_year_lag_2 <- store_item_2 %>% 
  pull(sales) %>% 
  ggAcf(., lag.max = 2 * 365)

# Create a 2x3 plot containing all previous plots
(time_series_plot_1 + autocorrelation_month_lag_1 + autocorrelation_2_year_lag_1) /
  (time_series_plot_2 + autocorrelation_month_lag_2 + autocorrelation_2_year_lag_2)
