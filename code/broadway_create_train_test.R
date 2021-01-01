#############################
#                           #
#     Broadway data set     #
#                           #
#   DA2 - Assignment 2      #
#                           #
#     Julianna Szabo        #
#                           #
#     Train and test        #
#                           #
#############################

# Libraries
rm(list = ls())
library(tidyverse)

# Load full data frame

my_url <- "https://raw.githubusercontent.com/Julianna-szabo/DA2_Assignment_2/main/data/clean/broadway_clean_xsec.csv"
df <- read.csv(my_url)
df <- data.frame(df)

# Add all the transformations that will be done during the analysis

df$revenue_per_att <- df$revenue/df$num_of_attendance

df <- df %>% mutate( ln_capacity_filled = log( capacity_filled ),
                     ln_revenue_per_att= log( revenue_per_att),
                     ln_percentage_of_poss_profit = log( percentage_of_poss_profit),
                     ln_num_of_performances = log(num_of_performances))

df <- df %>% mutate( capacity_filled_sq = capacity_filled^2)


# Now split it into train and test

train <- df[sample(nrow(df), 606), ]
test <- df[ !(df$show_name %in% train$show_name), ]


# Save these as files in the clean data folder

my_path <- "/Users/Terez/OneDrive - Central European University/Data_Analysis_02/DA2_Assignment_2/data/"
write_csv( train , paste0(my_path,'clean/broadway_train.csv'))
write_csv( test , paste0(my_path,'clean/broadway_test.csv'))

