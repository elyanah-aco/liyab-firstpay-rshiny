
##################################
## Server
##################################

server <- function(input, output, session){
  df_filt_one <- reactive(liyab_df |> 
                            filter(industry == input$industry_one
                                   & school_graduated == input$school_one 
                                   & year_of_first_job >= input$date_slider_one[1] 
                                   & year_of_first_job <= input$date_slider_one[2]))
  
  df_filt_two <- reactive(liyab_df |> 
                            filter(industry == input$industry_two
                                    & school_graduated == input$school_two 
                                    & year_of_first_job >= input$date_slider_two[1] 
                                    & year_of_first_job <= input$date_slider_two[2]))
  
  output$salary_boxplot_one <- renderPlot({
    if (input$incl_inflation == TRUE){
      df_filt_one() |>
        ggplot(aes(year_of_first_job, monthly_salary_adjusted)) +
        theme_bw() + 
        geom_boxplot(aes(factor(year_of_first_job)),
                     fill = 'steelblue3',
                     colour = 'grey30') +
        labs(x = 'Year', y = 'Monthly (adjusted, in PHP)')
    } else {
      df_filt_one() |>
        ggplot(aes(year_of_first_job, monthly_salary_unadjusted)) +
        theme_bw() + 
        geom_boxplot(aes(factor(year_of_first_job)), 
                     fill = 'steelblue3',
                     colour = 'grey30') + 
        labs(x = 'Year', y = 'Monthly (unadjusted, in PHP)')
    }
  }, res = 96)
  
  
  output$salary_boxplot_two <- renderPlot({
    if (input$incl_inflation == TRUE){
      df_filt_two() |>
        ggplot(aes(year_of_first_job, monthly_salary_adjusted)) +
        theme_bw() + 
        geom_boxplot(aes(factor(year_of_first_job)),
                     fill = 'salmon2',
                     colour = 'grey10') +
        labs(x = 'Year', y = 'Monthly salary (adjusted, in PHP)')
    } else {
      df_filt_two() |>
        ggplot(aes(year_of_first_job, monthly_salary_unadjusted)) +
        theme_bw() + 
        geom_boxplot(aes(factor(year_of_first_job)), 
                     fill = 'salmon2',
                     colour = 'grey10') + 
        labs(x = 'Year', y = 'Monthly salary (unadjusted, in PHP)')
    }
  }, res = 96)
  
  output$median_text <- renderText({
    if (input$incl_inflation == TRUE){
      median(df_filt_one()$monthly_salary_adjusted)
    } else {
      median(df_filt_one()$monthly_salary_unadjusted)
    }
  })
}
