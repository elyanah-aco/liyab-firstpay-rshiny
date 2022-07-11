##################################
## Load modules
##################################

library(glue)
library(bslib)
library(shiny)
library(tidyverse)

##################################
## Assign data, variables and functions
##################################

liyab_df <- read.csv('liyab_cleaned.csv')

industry_choices <- sort(unique(liyab_df$industry))
school_choices <- sort(unique(liyab_df$school_graduated))
min_year <- min(liyab_df$year_of_first_job)
max_year <- max(liyab_df$year_of_first_job)

linebreaks <- function(n){
  HTML(strrep(br(),n))
}