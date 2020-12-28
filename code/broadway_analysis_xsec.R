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


# Libraries ---------------------------------------------------------------

rm(list = ls())

# Packages to use
library(tidyverse)
# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
#install.packages("lspline")
library(lspline)
# Estimate robust SE
#install.packages("estimatr")
library(estimatr)
# Compare models with robust SE
#install.packages("texreg")
library(texreg)
# For different themes
#install.packages(ggthemes)
library(ggthemes)
library(ggplot2)
library(flextable)
library(magrittr)

# Load the data -----------------------------------------------------------

my_url <- "https://raw.githubusercontent.com/Julianna-szabo/DA2_Assignment_2/main/data/clean/broadway_clean_xsec.csv"
df <- read.csv(my_url)
df <- data.frame(df)


# Create my final y variable ----------------------------------------------------

# I will be using Revenue / Attendant which I first need to create

df$revenue_per_att <- df$revenue/df$num_of_attendance

# Data description --------------------------------------------------------

# Distribution of x
x_summary <- df %>% 
  summarise(
    n = length(capacity_filled),
    mean = mean(capacity_filled),
    median = median(capacity_filled),
    min = min(capacity_filled),
    max = max(capacity_filled),
    sd = sd(capacity_filled))

df %>%
  ggplot(aes(x = capacity_filled)) +
  geom_histogram(bins= 20)+
  labs(x = "Occupancy percentage", y = "Count")

# Looks like there are some outliers that are errors, since it cannot be more than 1

# Distribution of y
y_summary <- df %>% 
  summarise(
    n = length(revenue_per_att),
    mean = mean(revenue_per_att),
    median = median(revenue_per_att),
    min = min(revenue_per_att),
    max = max(revenue_per_att),
    sd = sd(revenue_per_att))

df %>%
  ggplot(aes(x = revenue_per_att)) +
  geom_histogram(bins= 20)+
  labs(x = "Revenue per attendant", y = "Count")

summary_table 
  
# Looks like they are distributed somehwat normall, but y has a long right tail, while x is more heavy
# on the right and left tail

# Remove the outliers in occupancy

df <- df %>% filter(capacity_filled <= 1)


# Ln Transformations ------------------------------------------------------

# Checking different transformations using scatter plots

# First I will add variables with the ln transformation
df <- df %>% mutate( ln_capacity_filled = log( capacity_filled ),
                     ln_revenue_per_att= log( revenue_per_att) )


# Different types of models

# 1, Level - level regression

df %>% 
  ggplot(aes(x = capacity_filled, y = revenue_per_att)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Occupancy percentage",y = "Revenue per attendant")

# 2, Log - level regression

df %>% 
  ggplot(aes(x = ln_capacity_filled, y = revenue_per_att)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "ln (Occupancy percentage)",y = "Revenue per attendant")

# 3, Level - log regression

df %>% 
  ggplot(aes(x = capacity_filled, y = ln_revenue_per_att)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Occupancy percentage",y = "ln (Revenue per attendant)")

# 4, Log - log regression

df %>% 
  ggplot(aes(x = ln_capacity_filled, y = ln_revenue_per_att)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "ln (Occupancy percentage)",y = "ln (Revenue per attendant)")


# Level- log makes the most sense




# Regression Models -------------------------------------------------------
# Different models:
#     reg1: ln_deaths_ppc = alpha + beta * ln_cases_ppc
#     reg2: ln_deaths_ppc = alpha + beta_1 * ln_cases_ppc + beta_2 * ln_cases_ppc^2
#     reg3: ln_deaths_ppc = alpha + beta_1 * ln_cases_ppc * 1(ln_cases_ppc < 50) + beta_2 * ln_cases_ppc * 1(ln_cases_ppc >= 50)
#     reg4: ln_deaths_ppc = alpha + beta * ln_cases_ppc, weights: population

# First I will add the square and cube of the x variable to df

df <- df %>% mutate( capacity_filled_sq = capacity_filled^2)

# Regression 1 - Simple linear regression

reg1 <- lm_robust( ln_revenue_per_att ~ capacity_filled , data = df,  se_type = "HC2" )
reg1

# Summary statistics
summary( reg1 )
# Visual inspection:
ggplot( data = df, aes( x = capacity_filled, y = ln_revenue_per_att ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' ) +
  labs(x = "Occupancy percentage",y = "ln (Revenue per attendant)")

# Regression 2 - Quadratic (linear) regression

reg2 <- lm_robust( ln_revenue_per_att ~ capacity_filled + capacity_filled_sq , data = df )
summary( reg2 )
ggplot( data = df, aes( x = capacity_filled, y = ln_revenue_per_att ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' ) +
  labs(x = "Occupancy percentage",y = "ln (Revenue per attendant)")

# Regressipn 3 - Piecewise linear spline regression

# Define the cutoff for occupancy
cutoff <- 0.5
# Use simple regression with the lspline function
reg3 <- lm_robust(ln_revenue_per_att ~ lspline( capacity_filled , cutoff ), data = df )
summary( reg3 )
ggplot( data = df, aes( x = capacity_filled, y = ln_revenue_per_att ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff) , method = lm , color = 'red' ) +
  labs(x = "Occupancy percentage",y = "ln (Revenue per attendant)")

# Regression 4 - Weighted linear regression, where  weights = percentage of total revenue

reg4 <- lm_robust(ln_revenue_per_att ~ capacity_filled, data = df , weights = percentage_of_poss_profit)
summary( reg4 )

ggplot(data = df, aes(x = capacity_filled, y = ln_revenue_per_att)) +
  geom_point(data = df, aes(size=percentage_of_poss_profit),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = percentage_of_poss_profit), method = "lm", color='red')+
  labs(x = "Occupancy percentage",y = "ln (Revenue per attendant)")

# Regression 5 - Weighted linear regression, where weights = number of performances

reg5 <- lm_robust(ln_revenue_per_att ~ capacity_filled, data = df , weights = num_of_performances)
summary( reg4 )

ggplot(data = df, aes(x = capacity_filled, y = ln_revenue_per_att)) +
  geom_point(data = df, aes(size=num_of_performances),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = num_of_performances), method = "lm", color='red')+
  labs(x = "Occupancy percentage",y = "ln (Revenue per attendant)")


# Comparing models --------------------------------------------------------

# Creating model summary with texreg
data_out <- "/Users/Terez/OneDrive - Central European University/Data_Analysis_02/DA2_Assignment_2/out/"
htmlreg( list(reg1 , reg2 , reg3 , reg4 ,reg5 ),
         type = 'html',
         custom.model.names = c("Occupancy percentage - linear","Occupancy percentage - quadratic",
                                "Occupancy percentage - PLS",
                                "Occupancy percentage- weighted (profit) linear",
                                "Occupancy percentage- weighted (number of per) linear"),
         caption = "Modelling Revenue per attendant on Occupancy percentage for different shows",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)

# Pick Weighted OLS with number of performances

# Check if it becomes better if one of the weights are included as variables

reg6 <-  lm_robust( ln_revenue_per_att ~ capacity_filled + percentage_of_poss_profit, data = df,  se_type = "HC2" )
reg6

reg7 <-  lm_robust( ln_revenue_per_att ~ capacity_filled + num_of_performances, data = df,  se_type = "HC2" )
reg7

reg8 <-  lm_robust( ln_revenue_per_att ~ capacity_filled + percentage_of_poss_profit + num_of_performances, data = df,  se_type = "HC2" )
reg8


# Export again and compare

data_out <- "/Users/Terez/OneDrive - Central European University/Data_Analysis_02/DA2_Assignment_2/out/"
htmlreg( list(reg1 , reg2 , reg3 , reg4 , reg5 , reg6, reg7, reg8),
         type = 'html',
         custom.model.names = c("Occupancy percentage - linear","Occupancy percentage - quadratic",
                                "Occupancy percentage - PLS",
                                "Occupancy percentage- weighted (profit) linear",
                                "Occupancy percentage- weighted (number of per) linear",
                                "Occupancy percentage + Profit - linear",
                                "Occupancy percentage + number of per - linear",
                                "Occupancy percentage + profit + number of per - linear"),
         caption = "Modelling Revenue per attendant on Occupancy percentage for different shows",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)

# Looks like including as x revenue percentage is a tiny bit better

# Testing hypothesis ------------------------------------------------------

# 1) Coefficient is equal to 0:
# Implemented by default...
summary( reg6 )

# 2) Checking it using the unique formula
library(car)
# Let test: H0: ln_cases_ppc = 0, HA: ln_cases_ppc neq 0
linearHypothesis( reg6 , "capacity_filled = 0")


