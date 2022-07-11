
##################################
## Server
##################################

server <- function(input, output, session){
  
  ## Filter dataframes
  
  df_filt_industry <- reactive(liyab_df |>
                                 filter(industry == input$industry))
  
  df_filt_one <- reactive(liyab_df |> 
                            filter(industry == input$compare_industry_one
                                   & school_graduated == input$compare_school_one 
                                   & year_of_first_job >= input$compare_date_slider_one[1] 
                                   & year_of_first_job <= input$compare_date_slider_one[2]) |>
                            filter(if(input$compare_excl_negotiation == TRUE) (offer_negotiated != 'Yes') else TRUE))
  
  df_filt_two <- reactive(liyab_df |> 
                            filter(industry == input$compare_industry_two
                                    & school_graduated == input$compare_school_two 
                                    & year_of_first_job >= input$compare_date_slider_two[1] 
                                    & year_of_first_job <= input$compare_date_slider_two[2]) |>
                            filter(if(input$compare_excl_negotiation == TRUE) (offer_negotiated != 'Yes') else TRUE))
  
  ## Create plot outputs
  
  output$top_schools_count_barplot <- renderPlot({
    df_filt_industry() |> 
      count(school_graduated, sort = TRUE) |>
      top_n(5) |>
      mutate(school_graduated = fct_reorder(school_graduated, n)) |>
      ggplot(aes(n, school_graduated)) +
      theme_bw() + 
      geom_segment(aes(x = 0, xend = n,
                       y = school_graduated, yend = school_graduated)) + 
      geom_point(size = 6,
                 color = 'aquamarine3') + 
      labs(title = glue('Top schools - ', input$industry), 
           subtitle = glue('Total number of respondents: ', nrow(df_filt_industry())),
           x = '',
           y = 'Number of respondents')
  }, res = 96)
  
  
  nego_cols <- c('Yes' = '#3498db',
                 'No' = '#f39c12',
                 'Unspecified' = '#95a5a6')
  
  output$offer_nego_pieplot <- renderPlot({
    df_filt_industry() |>
      count(offer_negotiated, sort = TRUE) |>
      mutate(pct = n/sum(n)) |> 
      ggplot(aes(x = '', y = pct, 
                 fill = factor(offer_negotiated, levels = c('Yes', 'No', 'Unspecified')))) + 
      geom_bar(stat = 'identity', 
               color = 'white',
               width = 1, 
               size = 1.2) +
      coord_polar('y', start = 0) + 
      labs(title = 'Offer negotiated', 
           subtitle = glue('Percent who negotiated: ', 
                           round(nrow(df_filt_industry() |> filter(offer_negotiated == 'Yes')) 
                                / nrow(df_filt_industry() |> filter(offer_negotiated != 'Unspecified')) * 100,2), '%'),
           x = '',
           y = '') + 
      scale_fill_manual(values = nego_cols) + 
      theme_bw() + 
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_blank(),
            legend.background = element_rect(colour = 'black')) 
  }, res = 96)
  
  
  output$salary_boxplot_one <- renderPlot({
      if (input$compare_incl_inflation == TRUE){
        df_filt_one() |>
        ggplot(aes(year_of_first_job, monthly_salary_adjusted)) +
          theme_bw() + 
          geom_boxplot(aes(factor(year_of_first_job)),
                   fill = '#20c997',
                   colour = 'grey30') +
          labs(title = glue('Monthly salary (', input$compare_industry_one, 
                          ', ', input$compare_school_one, ')'),
               subtitle = glue('Median salary: Php ', 
                             median(df_filt_one()$monthly_salary_adjusted)),
               x = 'Year', 
               y = 'Monthly salary (adjusted, in PHP)')
      } else {
        df_filt_one() |>
        ggplot(aes(year_of_first_job, monthly_salary_unadjusted)) +
          theme_bw() + 
          geom_boxplot(aes(factor(year_of_first_job)),
                       fill = '#20c997',
                       colour = 'grey30') +
          labs(title = glue('Monthly salary (', input$compare_industry_one, 
                          ', ', input$compare_school_one, ')'),
               subtitle = glue('Median salary: Php ', 
                               median(df_filt_one()$monthly_salary_unadjusted)),
               x = 'Year', 
               y = 'Monthly salary (adjusted, in PHP)')
      }
  }, res = 96)
  
  
  output$salary_boxplot_two <- renderPlot({
      if (input$compare_incl_inflation == TRUE){
        df_filt_two() |>
        ggplot(aes(year_of_first_job, monthly_salary_adjusted)) +
          theme_bw() + 
          geom_boxplot(aes(factor(year_of_first_job)),
                       fill = '#3498db',
                       colour = 'grey10') + 
          labs(title = glue('Monthly salary (', input$compare_industry_two, 
                          ', ', input$compare_school_two, ')'),
               subtitle = glue('Median salary: Php ', 
                           median(df_filt_two()$monthly_salary_adjusted)),
               x = 'Year', 
               y = 'Monthly salary (adjusted, in PHP)')
    } else {
      df_filt_two() |>
      ggplot(aes(year_of_first_job, monthly_salary_unadjusted)) +
        theme_bw() + 
        geom_boxplot(aes(factor(year_of_first_job)),
                     fill = '#3498db',
                     colour = 'grey10') + 
      labs(title = glue('Monthly salary (', input$compare_industry_two, 
                        ', ', input$compare_school_two, ')'),
           subtitle = glue('Median salary: Php ', 
                           median(df_filt_two()$monthly_salary_unadjusted)),
           x = 'Year', 
           y = 'Monthly salary (adjusted, in PHP)')
    }
  }, res = 96)
}
