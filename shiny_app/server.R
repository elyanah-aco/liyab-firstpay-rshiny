
##################################
## Server
##################################

server <- function(input, output, session){
  df_filt <- reactive(liyab_df |> 
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
