##################################
## Load modules
##################################

library(shiny)
library(vroom)
library(tidyverse)

##################################
## Load data
##################################

liyab_df <- read.csv('liyab_cleaned.csv')

##################################
## UI
##################################


ui <- fluidPage(
  fluidRow(
    column(8,selectInput('industry', 'Industry', 
                         choices = sort(liyab_df$industry), 
                         multiple = TRUE,
                         width = '100%')),
    column(2, selectInput('school', 'University graduated', 
                          choices = sort(liyab_df$school_graduated), 
                          multiple = TRUE,
                          width = '100%')),
    column(2, sliderInput('date_slider', 'Years',
                          min = 1987, max = 2022, value = c(1987, 2022))),
    column(2, checkboxInput('incl_inflation', 'Inflation-adjusted?', 
                            value = FALSE))
    ),
  fluidRow(
    column(12, plotOutput('avg_salary_per_year'))
  )
)

##################################
## UI
##################################

server <- function(input, output, session){
 df_filt <- reactive(df |> 
                       filter(industry == input$industry 
                              & school_graduated == input$school 
                              & year_of_first_job >= input$date_slider[1] 
                              & year_of_first_job <= input$date_slider[2]))
 
 output$avg_salary_per_year <- renderPlot({
   if (input$incl_inflation == TRUE){
     df_filt() |>
       ggplot(aes(year_of_first_job, monthly_salary_adjusted)) +
       geom_boxplot(aes(factor(year_of_first_job))) +
       labs(x = 'Year', y = 'Distribution of monthly salary (adjusted, in PHP)')
   } else {
     df_filt() |>
       ggplot(aes(year_of_first_job, monthly_salary_unadjusted)) +
       geom_boxplot(aes(factor(year_of_first_job))) + 
       labs(x = 'Year', y = 'Distribution of monthly salary (unadjusted, in PHP)')
   }
 }, res = 96)
}


# Run the application 
shinyApp(ui, server)
