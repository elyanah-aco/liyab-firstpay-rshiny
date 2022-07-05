
##################################
## UI
##################################

ui <- fluidPage(
  title = 'Liyab First Pay Dashboard',
  
  fluidRow(
    column(8,selectInput('industry', 'Industry', 
                         choices = industry_choices, 
                         multiple = TRUE,
                         width = '100%')),
    column(2, selectInput('school', 'University graduated', 
                          choices = school_choices, 
                          multiple = TRUE, 
                          width = '100%')),
    column(2, sliderInput('date_slider', 'Years',
                          min = min_year, 
                          max = max_year, 
                          value = c(min_year, max_year),
                          step = 1)),
    column(2, checkboxInput('incl_inflation', 'Inflation-adjusted?', 
                            value = FALSE))
  ),
  fluidRow(
    column(12, plotOutput('avg_salary_per_year'))
  )
)
