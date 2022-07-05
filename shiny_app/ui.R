
##################################
## UI
##################################

ui <- fluidPage(
  title = 'Liyab First Pay Dashboard',
  titlePanel('Liyab First Pay Dashboard'),
  h4('Compare salaries between industries and universities.'),
  sidebarPanel(
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
                step = 1),
    br(),
    br(),
    br(),
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
                step = 1),
    br(),
    checkboxInput('incl_inflation', 'Adjust salaries to inflation',
                  value = FALSE)
  ),
  mainPanel(
    fluidRow(column(12, 
                    plotOutput('salary_boxplot_one'),
                    plotOutput('salary_boxplot_two'))),
    fluidRow(column(4, 'The median salary for all selected years is Php', textOutput('median_text')))
  )
)
