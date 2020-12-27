#############################
#                           #
#     Broadway data set     #
#                           #
#   DA2 - Assignment 2      #
#                           #
#     Julianna Szabo        #
#                           #
#       Cleaning  2         #
#                           #
#############################


#Libraries to use
rm(list=ls())
library(tidyverse)
library(lubridate)
library(scales)

# Download the broadway data set
broadway_url <- "https://raw.githubusercontent.com/Julianna-szabo/DA2_Assignment_2/main/data/raw/broadway.csv"
df <- read.csv(broadway_url)
df <- data.frame(df)

# Let us take a quick look at the data
glimpse(df)

# Lets reorder the columns
df <- df[, c(2, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)]

# First let's rename the columns
col_names <- c("date", "date_day", "date_month", "date_year", "show_name", "show_theater", "show_type", "num_of_attendance", "capacity_filled", "revenue", "percentage_of_poss_profit", "num_of_performances")
colnames(df) <- col_names
rm(col_names)

# Looks like most things are okay. I need to make it into cross sectional data, so we need to do some agregation.
# Also the date columns can be dropped since they become irrelevant.

df$date <- NULL
df$date_day <- NULL
df$date_month <- NULL
df$date_year <- NULL

# Now to the aggregation

df_2 <- df %>% 
  group_by(show_name) %>% 
  summarise(
    "show_theater" = show_theater,
    "show_type" = show_type,
    "num_of_attendance" = sum(num_of_attendance),
    "capacity_filled" = mean(capacity_filled),
    "revenue" = sum(revenue),
    "percentage_of_poss_profit" =mean(percentage_of_poss_profit),
    "num_of_performances" = sum(num_of_performances)
  )

df<- distinct(df_2)

rm(df_2)


# Let us convert the percentage columns into percentages

df <- df %>%  mutate( capacity_filled = capacity_filled/100,
                      percentage_of_poss_profit = percentage_of_poss_profit/100)

# In Percentage of Profit 0 means Null so we need to make this transformation

for (i in 1:length(df$percentage_of_poss_profit)) {
  if (df$percentage_of_poss_profit[i] == 0) {
    df$percentage_of_poss_profit[i] <- NA
  }
}

# Let's check for missing values
# I using percentage_of_profit to filter since it is the only one that will have missing values

View( df %>% filter( !complete.cases(percentage_of_poss_profit) ) )

# There are about 30 with missing values. I will remove these.

df <- df %>% filter( complete.cases(percentage_of_poss_profit) ) 

# Now the data is clean so we can save it

# Save clean data
my_path <- "/Users/Terez/OneDrive - Central European University/Data_Analysis_02/DA2_Assignment_2/data/"
write_csv( df , paste0(my_path,'clean/broadway_clean_xsec.csv'))
