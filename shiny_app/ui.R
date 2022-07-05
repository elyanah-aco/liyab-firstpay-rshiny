
##################################
## UI
##################################

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = 'flatly'),
  h2('Liyab First Pay Dashboard'),
  h4('This dashboard displays self-reported data about Filipino workers\' first pay, as collated by RISE for Youth Philippines (formerly Liyab)'),
  h4('To contribute to this dataset,',  a('click here.', href = 'https://forms.gle/YYaZ9R6k6NhXZBqj7')),
  title = 'Liyab First Pay Dashboard',
  tabsetPanel(
    tabPanel('Placeholder'),
    tabPanel('Cross-comparison',
             br(),
             h5('Compare salaries between industries and universities.'),
             sidebarLayout(
               sidebarPanel(width = 3,
                 h4('Pair 1'),
                 selectInput('industry_one', 'Industry',
                             choices = industry_choices,
                             width = '75%'),
                 selectInput('school_one', 'University/college graduated',
                             choices = school_choices,
                             width = '75%'),
                 sliderInput('date_slider_one', 'Date range',
                             min = min_year,
                             max = max_year,
                             sep = '', 
                             value = c(min_year, max_year), 
                             step = 1,
                             width = '85%'),
                 linebreaks(2),
                 h4('Pair 2'),
                 selectInput('industry_two', 'Industry',
                             choices = industry_choices,
                             width = '75%'),
                 selectInput('school_two', 'University/college graduated',
                             choices = school_choices,
                             width = '75%'),
                 sliderInput('date_slider_two', 'Date range',
                             min = min_year,
                             max = max_year,
                             sep = '', 
                             value = c(min_year, max_year), 
                             step = 1,
                             width = '85%'),
                 br(),
                 checkboxInput('incl_inflation', 'Adjust salaries to inflation',
                               value = FALSE),
                 checkboxInput('excl_negotiation', 'Exclude negotiated offers',
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
  
