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
df_2 <- read.csv(broadway_url)
df_2 <- data.frame(df_2)

# Let us take a quick look at the data
glimpse(df)

# Lets reorder the columns
df <- df[, c(2, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)]

# First let's rename the columns
col_names <- c("date", "date_day", "date_month", "date_year", "show_name", "show_theater", "show_type", "num_of_attendance", "occupancy_percentage", "revenue", "percentage_of_poss_revenue", "num_of_performances")
colnames(df) <- col_names
rm(col_names)

# Looks like most things are okay. I need to make it into cross sectional data, so we need to do some agregation.
# Also the date columns can be dropped since they become irrelevant.

df$date <- NULL
df$date_day <- NULL
df$date_month <- NULL
df$date_year <- NULL
df$show_theater <- NULL

# Now to the aggregation

df_2 <- df %>% 
  group_by(show_name) %>% 
  summarise(
    "show_type" = show_type,
    "num_of_attendance" = sum(num_of_attendance),
    "occupancy_percentage" = mean(occupancy_percentage),
    "revenue" = sum(revenue),
    "percentage_of_poss_revenue" =mean(percentage_of_poss_revenue),
    "num_of_performances" = sum(num_of_performances)
  )

df<- distinct(df_2)

rm(df_2)


# Let us convert the percentage columns into percentages

df <- df %>%  mutate( occupancy_percentage = occupancy_percentage/100,
                      percentage_of_poss_revenue = percentage_of_poss_revenue/100)

# In Percentage of Profit 0 means Null so we need to make this transformation

for (i in 1:length(df$percentage_of_poss_revenue)) {
  if (df$percentage_of_poss_revenue[i] == 0) {
    df$percentage_of_poss_revenue[i] <- NA
  }
}

# Let's check for missing values
# I using percentage_of_profit to filter since it is the only one that will have missing values

View( df %>% filter( !complete.cases(percentage_of_poss_revenue) ) )

# There are about 30 with missing values. I will remove these.

df <- df %>% filter( complete.cases(percentage_of_poss_revenue) ) 

# For percentages I will eliminate any values I may have about 1
# since that would be impossible

df <- df %>% filter(occupancy_percentage <= 1)
df <- df %>% filter(percentage_of_poss_revenue <= 1)

# Seems like there are some shows that have zero performances

View( df_2 %>% filter(show_name == c("Bobbi Boland", "Chronicle Of A Death Foretold", 
                                     "Company 95", "Hello, Dolly!", "Holiday", "Patti Lupone On Broadway")))

# Now the data is clean so we can save it

# Save clean data
my_path <- "/Users/Terez/OneDrive - Central European University/Data_Analysis_02/DA2_Assignment_2/data/"
write_csv( df , paste0(my_path,'clean/broadway_clean_xsec.csv'))
