#############################
#                           #
#     Broadway data set     #
#                           #
#   DA2 - Assignment 2      #
#                           #
#     Julianna Szabo        #
#                           #
#       Analysis            #
#                           #
#############################



# Libraries
rm(list = ls())
library(tidyverse)
library(lubridate)
library(ggplot2)


# Load the data -----------------------------------------------------------

my_url <- "https://raw.githubusercontent.com/Julianna-szabo/DA2_Assignment_2/main/data/clean/broadway_clean.csv"
df <- read.csv(my_url)
df <- data.frame(df)


# Train and test ----------------------------------------------------------

# I have decided to create a train and test data. Since I have data from 1991 to 2016,
# I will use data from 1990 to 2015 as train and 2016 as test.

train <- df %>% 
  group_by(date_year) %>% 
  filter(date_year < 2016)

my_path <- "/Users/Terez/OneDrive - Central European University/Data_Analysis_02/DA2_Assignment_2/data/"
write_csv( train , paste0(my_path,'clean/broadway_train.csv'))

test <- df %>% 
  group_by(date_year) %>% 
  filter(date_year == 2016)

write_csv( test , paste0(my_path,'clean/broadway_test.csv'))


# Look at the two variables -----------------------------------------------

# I am interested how revenue and the number of attendants changed over time
# My two y variables will therefore be revenue and number of attendants

x_summary <-train %>% 
  summarise(
    n = length(date),
    min = min(date),
    max = max(date))

revenue_summary <- train %>% 
  summarise(
    n = length(revenue),
    mean = mean(revenue),
    median = median(revenue),
    min = min(revenue),
    max = max(revenue),
    sd = sd(revenue))

attendance_summary <- train %>% 
  summarise(
    n = length(num_of_attendance),
    mean = mean(num_of_attendance),
    median = median(num_of_attendance),
    min = min(num_of_attendance),
    max = max(num_of_attendance),
    sd = sd(num_of_attendance))


# Plot the two y variables ------------------------------------------------

train %>% 
  group_by(date_year) %>% 
  summarise(
    "sum_rev" = sum(revenue),
    "sum_att" = sum(num_of_attendance)
  ) %>% 
  ggplot(aes( x = date_year ) )+
  geom_line( aes( y = sum_rev , color = "revenue" ) , size = 1 ) +
  geom_line( aes( y = sum_att , color = "number of attendance" ) , size = 1  ) +
  labs(x="Year") +
  scale_color_manual(name = "Variable",
                     values = c( "revenue" = "blue", "number of attendance" = "orange"),
                     labels = c("revenue"="Revenue","number of attendance"="Number of Attendance")) +
  theme_bw()

# Looks like the revenue increased dramatically over the years. The number of attendance seems to also
# increase but it is by a fraction of the revenue. They both seem to have a positive trend.



# Aggregation -------------------------------------------------------------

# There is no need to aggregate because of the nature of the data.
# I will be using grouping to be able to keep some of the other aspects of the data.
# I will be using some of the aggregation to yearly level to make visualizations nicer







# Create my final y variable ----------------------------------------------------

# I will be using Revenue / Attendant which I first need to create

df$revenue_per_att <- df$revenue/df$num_of_attendance

# Data description --------------------------------------------------------

x_summary <- df %>% 
  summarise(
    n = length(date),
    min = min(date),
    max = max(date)
  )

x_summary

y_summary <- df %>% 
  summarise(
    n = length(revenue_per_att),
    mean = mean(revenue_per_att),
    median = median(revenue_per_att),
    min = min(revenue_per_att),
    max = max(revenue_per_att),
    sd = sd(revenue_per_att))

y_summary

# There are no missing values in the column. Even if there would be, I would keep them to not disrupt my time series


# FIrst plot --------------------------------------------------------------

  group_by(date) %>% 
  summarise(
    "mean" = mean(revenue_per_att)
  ) %>% 
  ggplot(aes( x = date , y = mean) )+
  geom_line(color = 'red',size=1) +
  labs(x = "Weeks", y = "Revenue per Attendant") +
  theme_bw()



# Weekly

# Check this one because it doesn't work
train %>%
  group_by(date) %>% 
  summarise(
    "mean" = mean(revenue_per_att)
  ) %>% 
  ggplot(aes( x = date , y = mean ) )+
  geom_line(color = 'red',size=1) +
  labs(x = "Weeks", y = "Revenue per Attendant") +
  theme_bw()

# Yearly

train %>% 
  group_by(date_year) %>% 
  summarise(
    "mean" = mean(revenue_per_att)
  ) %>% 
  ggplot(aes( x = date_year , y = mean ) )+
  geom_line(color = 'red',size=1) +
  labs(x = "Years", y = "Revenue per Attendant") +
  theme_bw()
