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


# Load the data -----------------------------------------------------------

my_url <- "https://raw.githubusercontent.com/Julianna-szabo/DA2_Assignment_2/main/data/clean/broadway_clean.csv"
df <- read.csv(my_url)
df <- data.frame(df)


# Create my y variable ----------------------------------------------------

# I will be using Revenue / Attendant which I first need to create

df$revenue_per_att <- df$revenue/df$num_of_attendance

# Data description --------------------------------------------------------

y_summary <- df %>% 
  summarise(
    mean = mean(revenue_per_att),
    median = median(revenue_per_att),
    min = min(revenue_per_att),
    max = max(revenue_per_att),
    sd = sd(revenue_per_att))

y_summary

