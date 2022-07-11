
##################################
## UI
##################################

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = 'flatly'),
  title = 'Liyab First Pay Dashboard',
  h2('Liyab First Pay Dashboard'),
  h4('This dashboard displays self-reported data about Filipino workers\' first pay, 
     as collated by RISE for Youth Philippines (formerly Liyab)'),
  h4('To contribute to this dataset,',  a('click here.', href = 'https://forms.gle/YYaZ9R6k6NhXZBqj7'),
     'Note that data must be manually updated, so new entries will not be immediately shown.'),
  h6('Data last updated on June 20, 2022.'),
  br(),
  tabsetPanel(
    tabPanel('By industry',
             br(),
             h5('View trends in salaries per industry.'),
             sidebarLayout(
               sidebarPanel(width = 3,
                            h4('Industry'),
                            selectInput('industry', '',
                                        choices = industry_choices,
                                        width = '75%')),
               mainPanel(width = 9,
                         fluidRow(column(8, plotOutput('top_schools_count_barplot')),
                                  column(4, plotOutput('offer_nego_pieplot'))),
                         fluidRow(column(8, plotOutput('top_schools_salary_barplot'))))
             )),
    
    
    ## For cross-comparison
    tabPanel('Cross-comparison',
             br(),
             h5('Compare salaries between industries and universities.'),
             sidebarLayout(
               sidebarPanel(width = 3,
                            h4('Pair 1'),
                            selectInput('compare_industry_one', 'Industry',
                                        choices = industry_choices,
                                        width = '75%'),
                            selectInput('compare_school_one', 'University/college graduated',
                                        choices = school_choices,
                                        width = '75%'),
                            sliderInput('compare_date_slider_one', 'Date range',
                                        min = min_year,
                                        max = max_year,
                                        sep = '', 
                                        value = c(min_year, max_year), 
                                        step = 1,
                                        width = '85%'),
                            linebreaks(2),
                            h4('Pair 2'),
                            selectInput('compare_industry_two', 'Industry',
                                        choices = industry_choices,
                                        width = '75%'),
                            selectInput('compare_school_two', 'University/college graduated',
                                        choices = school_choices,
                                        width = '75%'),
                            sliderInput('compare_date_slider_two', 'Date range',
                                        min = min_year,
                                        max = max_year,
                                        sep = '', 
                                        value = c(min_year, max_year), 
                                        step = 1,
                                        width = '85%'),
                            br(),
                            checkboxInput('compare_incl_inflation', 'Adjust salaries to inflation',
                                          value = FALSE),
                            checkboxInput('compare_excl_negotiation', 'Exclude negotiated offers',
                                          value = FALSE)
               ),
               mainPanel(width = 9,
                         fluidRow(column(10, 
                                         plotOutput('salary_boxplot_one'),
                                         plotOutput('salary_boxplot_two')))
                         )
             )
             
    )
  )
)
  
