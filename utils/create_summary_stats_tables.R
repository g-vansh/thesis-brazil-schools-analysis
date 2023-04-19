# R script to create a summary stats table for each of the 2 datasets - schools and outcomes
# The tables will be exported to LaTeX format

# Load libraries
library(dplyr)
library(knitr)
library(psych)
library(kableExtra)
library(skimr)
library(stargazer)

# Load data
load("G:/Other computers/My PC/Desktop/Cornell/Research/Thesis/Schools Analysis/thesis-brazil-schools-analysis/data/schools_processed.Rdata")

# Variables to include in the summary stats table
physical_vars_schools <- c("FC_LIBRARY", "FC_COMPUTER_LAB", "FC_INTERNET", "FC_SCIENCE_LAB", "FC_HAS_SPORTS_QUAD", "FC_ELECTR")
human_vars_schools <- c("ST_TOTAL_STAFF", "STR_TEACHERS_ADVANCED_FUNDEDU", "ST_TEACHER_EDUCATION_YEARS", "CLASS_SIZE", "CLASS_STUDENT_TEACHER_RATIO")

# Create a summary stats table for the schools dataset, with two panels - Physical Capital and Human Capital
# Use the lists of variables defined above
# Rename the variables to more readable names
# Use the kableExtra package to add a caption and label to the table
# Export the table to LaTeX format
# Round the values to 3 decimal places
# Each variable should be a row in the table
# The columns should be the mean, standard deviation, minimum, median, and maximum values for each variable
schools_summary <- schools %>%
  select(physical_vars_schools, human_vars_schools) %>%
  rename("Library" = FC_LIBRARY,
         "Computer Lab" = FC_COMPUTER_LAB,
         "Internet" = FC_INTERNET,
         "Science Lab" = FC_SCIENCE_LAB,
         "Sports Quad" = FC_HAS_SPORTS_QUAD,
         "Electricity" = FC_ELECTR,
         "Total Staff" = ST_TOTAL_STAFF,
         "Teachers with Advanced Education" = STR_TEACHERS_ADVANCED_FUNDEDU,
         "Teacher Education Years" = ST_TEACHER_EDUCATION_YEARS,
         "Class Size" = CLASS_SIZE,
         "Class Student-Teacher Ratio" = CLASS_STUDENT_TEACHER_RATIO)

stargazer(schools_summary)

# Create summary stats table for the outcomes dataset

# Create two new columns which are means of PROVA_MEAN_MAT_I and PROVA_MEAN_MAT_T, and PROVA_MEAN_PORT_I and PROVA_MEAN_PORT_T repectively
schools_outcomes <- schools %>%
  mutate(PROVA_MEAN_MAT = (PROVA_MEAN_MAT_I + PROVA_MEAN_MAT_T)/2,
         PROVA_MEAN_PORT = (PROVA_MEAN_PORT_I + PROVA_MEAN_PORT_T)/2)

outcome_vars <- c("RATE_APROV", "RATE_FAILURE", "RATE_ABANDON", "PROVA_MEAN_MAT", "PROVA_MEAN_PORT")

schools_outcomes_summary <- schools_outcomes %>%
  select(outcome_vars) %>%
  rename("Pass Rate" = RATE_APROV,
         "Failure Rate" = RATE_FAILURE,
         "Abandonment Rate" = RATE_ABANDON,
         "Math Mean Score" = PROVA_MEAN_MAT,
         "Portuguese Mean Score" = PROVA_MEAN_PORT)

# Use stargazer to create a summary stats table for the outcomes dataset, with no N values, and round to 3 decimal places
stargazer(schools_outcomes_summary, digits = 3, omit.stat = "N")