##################################
## Load modules
##################################

library(tidyverse)

##################################
## Load data
##################################

liyab_df <- read.csv('liyab_cleaned.csv')

##################################
## Assign variables
##################################

industry_choices <- sort(unique(liyab_df$industry))
school_choices <- sort(unique(liyab_df$school_graduated))
min_year <- min(liyab_df$year_of_first_job)
max_year <- max(liyab_df$year_of_first_job)