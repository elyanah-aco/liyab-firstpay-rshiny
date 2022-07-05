
##################################
## Server
##################################

server <- function(input, output, session){
  
  df_filt_one <- reactive(liyab_df |> 
                            filter(industry == input$industry_one
                                   & school_graduated == input$school_one 
                                   & year_of_first_job >= input$date_slider_one[1] 
                                   & year_of_first_job <= input$date_slider_one[2]) |>
                            filter(if(input$excl_negotiation == TRUE) (offer_negotiated != 'Yes') else TRUE))
  
  df_filt_two <- reactive(liyab_df |> 
                            filter(industry == input$industry_two
                                    & school_graduated == input$school_two 
                                    & year_of_first_job >= input$date_slider_two[1] 
                                    & year_of_first_job <= input$date_slider_two[2]) |>
                            filter(if(input$excl_negotiation == TRUE) (offer_negotiated != 'Yes') else TRUE))
  
  output$salary_boxplot_one <- renderPlot({
      if (input$incl_inflation == TRUE){
        df_filt_one() |>
        ggplot(aes(year_of_first_job, monthly_salary_adjusted)) +
          theme_bw() + 
          geom_boxplot(aes(factor(year_of_first_job)),
                   fill = 'steelblue3',
                   colour = 'grey30') +
          labs(title = glue('Monthly salary (', input$industry_one, 
                          ', ', input$school_one, ')'),
               subtitle = glue('Median salary: Php ', 
                             median(df_filt_one()$monthly_salary_adjusted)),
               x = 'Year', 
               y = 'Monthly salary (adjusted, in PHP)')
      } else {
        df_filt_one() |>
        ggplot(aes(year_of_first_job, monthly_salary_unadjusted)) +
          theme_bw() + 
          geom_boxplot(aes(factor(year_of_first_job)),
                       fill = 'steelblue3',
                       colour = 'grey30') +
          labs(title = glue('Monthly salary (', input$industry_one, 
                          ', ', input$school_one, ')'),
               subtitle = glue('Median salary: Php ', 
                               median(df_filt_one()$monthly_salary_unadjusted)),
               x = 'Year', 
               y = 'Monthly salary (adjusted, in PHP)')
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
          labs(title = glue('Monthly salary (', input$industry_two, 
                          ', ', input$school_two, ')'),
               subtitle = glue('Median salary: Php ', 
                           median(df_filt_two()$monthly_salary_adjusted)),
               x = 'Year', 
               y = 'Monthly salary (adjusted, in PHP)')
    } else {
      df_filt_two() |>
      ggplot(aes(year_of_first_job, monthly_salary_unadjusted)) +
        theme_bw() + 
        geom_boxplot(aes(factor(year_of_first_job)),
                     fill = 'salmon2',
                     colour = 'grey10') + 
      labs(title = glue('Monthly salary (', input$industry_two, 
                        ', ', input$school_two, ')'),
           subtitle = glue('Median salary: Php ', 
                           median(df_filt_two()$monthly_salary_unadjusted)),
           x = 'Year', 
           y = 'Monthly salary (adjusted, in PHP)')
    }
  }, res = 96)
}
