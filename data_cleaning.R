
######################
## Load modules
######################

library(dplyr)
library(janitor)
library(stringr)
library(lubridate)
library(stringdist)

######################
## Load datasets
######################

liyab_raw <- read.csv('raw_data/liyab_raw.csv', skip = 2, header = T)
php_inflation <- read.csv('raw_data/php_inflation.csv', header = T)
colnames(liyab_raw) <- c('timestamp', 'year_of_first_job',
                         'industry_raw', 'role', 'monthly_salary_unadjusted',
                         'school_graduated_raw', 'sex_raw', 
                         'offer_negotiated', 'other_details')

##################################
## Data cleaning - Liyab dataset
##################################

## General cleaning
# Includes removing columns, changing cases and filling NAs
liyab_raw <- liyab_raw |> 
  select(-c(timestamp, role)) |>
  mutate(industry_raw = str_trim(str_to_title(industry_raw)),
         school_graduated_raw = str_trim(str_to_title(school_graduated_raw),'both'),
         offer_negotiated = ifelse(offer_negotiated == '', 'Unspecified', offer_negotiated),
         sex_raw = str_trim(str_to_title(sex_raw))) 

## Industry
liyab_raw <- liyab_raw |>
  
  ## Academe & Education
  mutate(industry = ifelse(grepl('Academ|Ed|Elearning|School|Teach', industry_raw), 
                           'Academe & Education', 'Other industries')) |>
  
  ## Arts and Design
  mutate(industry = ifelse(grepl('Anima|Art|Creative|Design|Graphic', industry_raw), 'Arts and Design', industry)) |>
   
  ## Accounting/Audit
  mutate(industry = ifelse(grepl('Audit|Accounting', industry_raw), 'Accounting', industry)) |>
  
  ## Advertising
  mutate(industry = ifelse(grepl('Ad', industry_raw), 'Advertising', industry)) |>
  
  ## Agriculture
  mutate(industry = ifelse(grepl('Agri', industry_raw), 'Agriculture', industry)) |>
  
  ## Architecture
  mutate(industry = ifelse(grepl('Archi', industry_raw), 'Architecture', industry)) |>
  
  ## Beauty and Fashion
  mutate(industry = ifelse(grepl('Beauty|Cloth|Cosmetic|Fashion', industry_raw), 'Beauty and Fashion', industry)) |>

  ## BPO
  mutate(industry = ifelse(grepl('Bpo|Bpoo|Business Processing|Call Center|Csr|Customer Service|Offshore|Outsour|Shared Service', 
                                 industry_raw), 'Business Process Outsourcing', industry)) |>
  
  ## Business
  mutate(industry = ifelse(grepl('Business|Conglo|Corpo', industry_raw) & !grepl('Process', industry_raw), 
                           'Business', industry)) |>
  
  ## Commerce
  mutate(industry = ifelse(grepl('Commerce|Retail', industry_raw), 'Retail', industry)) |>
  
  ## Construction and Engineering
  mutate(industry = ifelse(grepl('Construct|Engineer', industry_raw), 'Construction and Engineering', industry)) |>
   
  ## Consulting
  mutate(industry = ifelse(grepl('Consult', industry_raw), 'Consulting', industry)) |>
  
  ## Finance
  mutate(industry = ifelse(grepl('Bank|Financ|Fintech|Forex|Insurance|Investment', industry_raw), 
                           'Finance', industry)) |>
  
  ## Food
  mutate(industry = ifelse(grepl('Catering|Culinary|Food|Restaurant', industry_raw), 'Food', industry)) |>
  
  ## Government
  mutate(industry = ifelse(grepl('Gov', industry_raw), 'Government', industry)) |>
  
  ## Healthcare
  mutate(industry = ifelse(grepl('Dental|Healt|Medic|medic|Pharma', industry_raw), 'Healthcare', industry)) |>
  
  ## Hospitality and Tourism
  mutate(industry = ifelse(grepl('Hospitality|Hotel|Resort|Travel|Tour', industry_raw), 'Hospitality', industry)) |>
  
  ## Human Relations
  mutate(industry = ifelse(grepl('Hr|Human Resource|Recruit', industry_raw), 'Human Relations', industry)) |>
  
  ## Law
  mutate(industry = ifelse(grepl('Law|Legal', industry_raw), 'Law', industry)) |>
  
  ## Logistics
  mutate(industry = ifelse(grepl('Logistic|Shipping', industry_raw), 'Logistics', industry)) |>
  
  ## Manufacturing (all types)
  mutate(industry = ifelse(grepl('Consumer Elec|Fmcg|Manu', industry_raw), 'Manufacturing', industry)) |>
  
  ## Marketing 
  mutate(industry = ifelse(grepl('Marketing', industry_raw), 'Marketing', industry)) |>
  
  ## Media and Communication
  mutate(industry = ifelse(grepl('Audio|Broadcast|Communication|Film|Journ|Media|Publi|Tv', industry_raw) 
                           & !grepl('Tele', industry_raw), 'Media and Communication', industry)) |>

  ## Non-Government Organization
  mutate(industry = ifelse(grepl('Ngo|Non-Government', industry_raw), 'Non-Government Organization', industry)) |>
  
  ## Non-Profit Organization
  mutate(industry = ifelse(grepl('Profit|profit', industry_raw), 'Non-Profit Organization', industry)) |>
  
  ## Oil and Gas
  mutate(industry = ifelse(grepl('Oil|Gas|Petroleum', industry_raw), 'Oil and Gas', industry)) |>
  
  ## Real Estate
  mutate(industry = ifelse(grepl('Real Estate|Property', industry_raw), 'Real Estate', industry)) |>
  
  ## Research
  mutate(industry = ifelse(grepl('Lab|Research', industry_raw), 'Research', industry)) |>

  ## Sales
  mutate(industry = ifelse(grepl('Sales', industry_raw), 'Sales', industry)) |>
  
  ## Startups
  mutate(industry = ifelse(grepl('Start', industry_raw), 'Startup', industry)) |>
  
  ## Technology
  mutate(industry = ifelse(grepl('Analytic|Cloud|Data|It|I.T.|I.t|Program|Software|Tech', industry_raw),
                           'Technology', industry)) |>
  
  ## Telecommunications
  mutate(industry = ifelse(grepl('Telco|Telecom', industry_raw), 'Telecomumunications', industry)) |>
  
  ## Transportation
  mutate(industry = ifelse(grepl('Auto|Aviation|Airline|Transpo', industry_raw), 'Transportation', industry)) |>

  ## Utilities
  mutate(industry = ifelse(grepl('Electricity|Energy|Power|Utilit|Water|Web', industry_raw), 'Utilities', industry)) |>
  
  select(-industry_raw)


## Schools graduated 
# Create dataframe containing keywords for each university
# Note: Only select universities and university systems are specified;
# others will be under 'Other universities'

liyab_raw <- liyab_raw |>
  
  ## UPD
  mutate(school_graduated = ifelse(grepl('Up Diliman|Upd', school_graduated_raw) 
                                    | (grepl('Philippine', school_graduated_raw) & grepl('Diliman', school_graduated_raw)),
                                    'University of the Philippines - Diliman', 'Other universities')) |> 
  
  ## ADMU
  mutate(school_graduated = ifelse(grepl('Admu', school_graduated_raw)
                                    | (grepl('Ateneo', school_graduated_raw) & grepl('Manila', school_graduated_raw)),
                                    'Ateneo de Manila University', school_graduated)) |>
  
  ## UST
  mutate(school_graduated = ifelse(grepl('Ust|Santo Tomas', school_graduated_raw), 
                                    'University of Santos Tomas', school_graduated)) |>
  
  ## DLSU Manila 
  mutate(school_graduated = ifelse(grepl('Dlsu', school_graduated_raw) 
                                    | (grepl('La Salle University', school_graduated_raw) & !grepl('Dasma', school_graduated_raw)),
                                    'De La Salle University - Manila', school_graduated)) |>

  ## UP Los Baños
  mutate(school_graduated = ifelse(grepl('Uplb', school_graduated_raw)
                                    | (grepl('Philippines|Up', school_graduated_raw) & grepl('Los', school_graduated_raw)),
                                    'University of the Philippines - Los Baños', school_graduated)) |>
  
  ## PUP
  mutate(school_graduated = ifelse(grepl('Pup|Polytechnic', school_graduated_raw) & !grepl('Mfi', school_graduated_raw),
                                    'Polytechnic University of the Philippines - Manila', school_graduated)) |>
  ## Mapua University Intramuros
  mutate(school_graduated = ifelse(grepl('Mapua', school_graduated_raw) & !grepl('Makati', school_graduated_raw),
                                    'Mapua University - Intramuros', school_graduated)) |>
  
  ## PLM
  mutate(school_graduated = ifelse(grepl('Plm', school_graduated_raw) 
                                    | (grepl('Pamantasan', school_graduated_raw) & grepl('Manila', school_graduated_raw)),
                                    'Pamantasan ng Lungsod ng Manila', school_graduated)) |>
  ## USC
  mutate(school_graduated = ifelse(grepl('Usc|San Carlos', school_graduated_raw), 
                                    'University of San Carlos', school_graduated)) |> 
  
  ## Saint Louis University
  mutate(school_graduated = ifelse(grepl('Louis', school_graduated_raw), 
                                    'Saint Louis University', school_graduated)) |>
  
  ## System Universities
  mutate(school_graduated = ifelse(school_graduated == 'Other universities' 
                                    & grepl('Up|University of the Philippines', school_graduated_raw)
                                    & !grepl('Technological', school_graduated_raw),
                                    'UP System University', school_graduated)) |>
  mutate(school_graduated = ifelse(school_graduated == 'Other universities' 
                                    & grepl('Ateneo', school_graduated_raw),
                                    'Ateneo System University', school_graduated)) |> 
  mutate(school_graduated = ifelse(school_graduated == 'Other universities' 
                                    & grepl('Dls|La Salle', school_graduated),
                                    'La Salle System University', school_graduated)) |>
  mutate(school_graduated = ifelse(grepl('Feu|Far East', school_graduated_raw),
                                    'FEU System University', school_graduated)) |>
  
  ## No universities (either undergraduate, did not finish or none specified)
  mutate(school_graduated = ifelse(grepl('Undergrad|Not Yet Graduated', school_graduated_raw), 
                                    'N/A (Undergrad)', school_graduated)) |>
  mutate(school_graduated = ifelse(grepl('Didn\'t', school_graduated_raw)
                                    | (grepl('High', school_graduated_raw) & grepl('Grad', school_graduated_raw)),
                                    'N/A (Did not take college)', school_graduated)) |>
  mutate(school_graduated = ifelse(school_graduated_raw == '' | grepl('N/A', school_graduated_raw), 
                                    'N/A (Unspecified)', school_graduated)) |>
  select(-school_graduated_raw)

## Sex
liyab_raw <- liyab_raw |>
  mutate(sex = ifelse(sex_raw == 'M' | grepl('Male|Mqle|Man', sex_raw),
                           'Male', 'Unspecified')) |>
  mutate(sex = ifelse(sex_raw == 'F' | grepl ('Fe|Frmale|Wom', sex_raw),
                       'Female', sex)) |> 
  select(-sex_raw)

## Years
liyab_raw <- liyab_raw |>
  mutate(year_of_first_job = ifelse(year_of_first_job == 208, 2008, 
                                    ifelse(year_of_first_job == 209, 2009, 
                                           ifelse(year_of_first_job == 2027, 2017,
                                                  ifelse(year_of_first_job == 2108, 2018, year_of_first_job))))) |>
  mutate(year_of_first_job = ifelse(str_length(year_of_first_job) == 2, 
                                    2000 + year_of_first_job, year_of_first_job))

## Final data cleaning (to remove entries with drastic, obvious and unfixable errors)
liyab_raw <- liyab_raw |>
  filter(year_of_first_job != 2) |>
  filter(year_of_first_job != 18000) |>
  filter(monthly_salary_unadjusted != 13) 

##################################
## Data cleaning - PHP inflation
##################################

php_inflation <- php_inflation |>
  clean_names() |>
  mutate(cumulative_inflation = (as.numeric(sub("%", "", cumulative_inflation, fixed=TRUE))/100) + 1)

liyab_raw <- liyab_raw |>
  left_join(php_inflation, by = c('year_of_first_job' = 'year')) |>
  mutate(monthly_salary_adjusted = monthly_salary_unadjusted * cumulative_inflation)

##################################
## Final data cleaning
##################################
liyab_raw <- liyab_raw |>
  select(year_of_first_job, industry, school_graduated, sex, monthly_salary_unadjusted, 
         cumulative_inflation, monthly_salary_adjusted, offer_negotiated, other_details)

######################
## Save datasets
######################

write.csv(liyab_raw, 'shiny_app/liyab_cleaned.csv', row.names = FALSE)