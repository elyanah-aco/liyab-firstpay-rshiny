
##################################
## Server
##################################

server <- function(input, output, session){
  
  ##################################
  ## Filter dataframes
  ##################################
  
  df_filt_industry <- reactive(liyab_df |>
                                 filter(industry == input$industry 
                                        & year_of_first_job >= input$industry_date_slider[1]
                                        & year_of_first_job <= input$industry_date_slider[2]))
  
  df_filt_industry_top_schools <- reactive(df_filt_industry() |>
                                             count(school_graduated, sort = TRUE) |>
                                             top_n(5))
  
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
  
  
  ##################################
  ## Create plots
  ##################################
  
  ## By Industry
  ## Top schools (Respondents)
  
  output$top_schools_count_barplot <- renderPlot({
    df_filt_industry_top_schools() |>
      mutate(school_graduated = fct_reorder(school_graduated, n)) |>
      ggplot(aes(n, school_graduated)) +
      theme_bw() + 
      geom_segment(aes(x = 0, xend = n,
                       y = school_graduated, yend = school_graduated)) + 
      geom_point(size = 6,
                 color = 'aquamarine3') + 
      labs(title = glue('Top schools, number of respondents - ', input$industry), 
           subtitle = glue('Total number of respondents: ', nrow(df_filt_industry())),
           x = 'Number of respondents',
           y = '')
  }, res = 96)
  
  
  ## Offer negotiated
  
  nego_cols <- c('Yes' = '#20c997',
                 'No' = '#ced4da')
  
  output$offer_nego_pieplot <- renderPlot({
    df_filt_industry() |>
      count(offer_negotiated, sort = TRUE) |>
      mutate(pct = n/sum(n)) |> 
      filter(offer_negotiated != 'Unspecified') |>
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
  
  ## Top schools (Salary)
  
  salary_cols <- c('Female' = '#E74C3C',
                   'Male' = '#3498DB', 
                   'Unspecified' = '#20c997')
  
  output$top_schools_salary_barplot <- renderPlot({
    if (input$industry_incl_inflation == TRUE) { 
      df_filt_industry() |>
      inner_join(df_filt_industry_top_schools()) |>
      mutate(school_graduated = fct_reorder(school_graduated, n)) |>
      group_by(school_graduated, sex) |> 
      summarize(min = min(monthly_salary_adjusted),
                median = median(monthly_salary_adjusted),
                max = max(monthly_salary_adjusted)) |>
      ggplot(color = sex) + 
        geom_segment(aes(x = min, xend = max,
                         y = school_graduated, yend = school_graduated),
                         size = 4.5,
                         color = '#ced4da') + 
        geom_point(aes(x = max, y = school_graduated, color = sex), 
                     size = 5) + 
        geom_point(aes(x = min, y = school_graduated, color = sex),
                     size = 5) + 
        facet_grid(rows = vars(sex)) + 
        scale_x_continuous(n.breaks = 10) +
        labs(title = glue('Maximum and minimum salaries of top schools - ', input$industry),
               x = 'Monthly salary (PHP)',
               y = '') + 
        scale_color_manual(values = salary_cols) + 
        theme_bw() +
        theme(legend.position = 'none')
      } else {
        df_filt_industry() |>
          inner_join(df_filt_industry_top_schools()) |>
          mutate(school_graduated = fct_reorder(school_graduated, n)) |>
          group_by(school_graduated, sex) |> 
          summarize(min = min(monthly_salary_unadjusted),
                    median = median(monthly_salary_unadjusted),
                    max = max(monthly_salary_unadjusted)) |>
          ggplot(color = sex) + 
          geom_segment(aes(x = min, xend = max,
                           y = school_graduated, yend = school_graduated),
                       size = 4.5,
                       color = '#ced4da') + 
          geom_point(aes(x = max, y = school_graduated, color = sex), 
                     size = 5) + 
          geom_point(aes(x = min, y = school_graduated, color = sex),
                     size = 5) + 
          facet_grid(rows = vars(sex)) + 
          scale_x_continuous(n.breaks = 10) +
          labs(title = glue('Maximum and minimum salaries of top schools - ', input$industry),
               x = 'Monthly salary (PHP)',
               y = '') + 
          scale_color_manual(values = salary_cols) + 
          theme_bw() +
          theme(legend.position = 'none')
      }
  }, height = 600, res = 96)
  
  
  # Cross-comparison
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
