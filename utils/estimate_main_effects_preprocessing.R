############################################
# Estimate the main effects of the strategies using regressions
############################################

# The objective of this script is to run a fixed effects OLS regression
# to estimate the main effects of the strategies on the outcome variables
# of interest. First, the data is cleaned to contain schools and their characteristics
# that are common across all years. Then, the data is merged with the outcome variables
# of interest. It is then cleaned to ensure that no columns have "_" in their names. 
# Finally, the data is merged with the strategy variables and the regression is run.

library(sandwich)
library(lfe)
library(glmnet)
library(tidyverse)
library(dplyr)
library(fixest)

# Load the data
load("data/schools_cleaned.Rdata")

################################################
# Clean and process the dataset for training
################################################

# Rename variables as characteristics and facilities
schools <- plyr::rename(schools, c(
  'FEDERAL' = 'CH_FEDERAL',
  'ESTADUAL' = 'CH_STATE',
  'MUNICIPAL' = 'CH_MUNICIPAL',
  'PRIVADA' = 'CH_PRIVATE',
  'URBANA' = 'CH_URBAN',
  'NIVFUND_1' = 'CH_FUNDAMENTAL_EDU_I',
  'NIVFUND_2' = 'CH_FUNDAMENTAL_EDU_T',
  'TURMAFUND' = 'CH_CLASSES_FUNDAMENTAL_EDU',
  'TURMAFUND_I' = 'CH_CLASSES_FUNDAMENTAL_EDU_I',
  'TURMAFUND_T' = 'CH_CLASSES_FUNDAMENTAL_EDU_T',
  'MATFUND' = 'CH_MATRI_FUNDAMENTAL_EDU',
  'MATFUND_I' = 'CH_MATRI_FUNDAMENTAL_EDU_I',
  'MATFUND_T' = 'CH_MATRI_FUNDAMENTAL_EDU_T'
  ))
schools <- plyr::rename(schools, c(
  'IN_BIBLIOTECA' = 'FC_LIBRARY',
  'IN_LABORATORIO_INFORMATICA' = 'FC_COMPUTER_LAB',
  'IN_LABORATORIO_CIENCIAS' = 'FC_SCIENCE_LAB',
  'IN_QUADRA_ESPORTES' = 'FC_HAS_SPORTS_QUAD',
  'IN_EQUIP_TV' = 'FC_TV_EQUIPMENT',
  'IN_EQUIP_PARABOLICA' = 'FC_ANTENNA_EQUIPMENT',
  'IN_COMPUTADOR' = 'FC_COMPUTER',
  'IN_INTERNET' = 'FC_INTERNET',
  'NU_COMPUTADOR' = 'FC_NUMBER_COMPUTERS',
  'SCHOOL_WATER_PUBLIC' = 'FC_WATER_PUBLIC',
  'SCHOOL_WATER' = 'FC_WATER',
  'SCHOOL_ELECTR_PUB' = 'FC_ELECTR_PUBLIC',
  'SCHOOL_ELECTR' = 'FC_ELECTR',
  'SCHOOL_SEWAGE_PUB' = 'FC_SEWAGE_PUBLIC',
  'SCHOOL_SEWAGE' = 'FC_SEWAGE'
))
schools <- plyr::rename(schools, c(
  'NU_FUNCIONARIOS' = 'ST_TOTAL_STAFF',
  'PROFFUNDTOT' = 'ST_FUND_EDU_TEACHERS',
  'PCPROFFUNDINC' = 'STR_TEACHERS_INCOMPLETE_FUNDEDU',
  'PCPROFFUNDCOMP' = 'STR_TEACHERS_ONLY_FUNDEDU',
  'PCPROFMED' = 'STR_TEACHERS_SECONDARY_FUNDEDU',
  'PCPROFSUP' = 'STR_TEACHERS_ADVANCED_FUNDEDU',
  'EDUCTEACH' = 'ST_TEACHER_EDUCATION_YEARS',
  'CLASSSIZE' = 'CLASS_SIZE',
  'STUDTEACH' = 'CLASS_STUDENT_TEACHER_RATIO',
  'ROOMS_AVAIL' = 'FC_ROOMS_AVAILABLE',
  'ROOMS_USED' = 'FC_ROOMS_USED'
))

school_info <- schools %>% select(CO_SCHOOL, CO_MUNICIPALITY, NAME_STATE, NAME_MUNICIPALITY)

schools_collapsed <- schools %>% group_by(CO_SCHOOL) %>% dplyr::summarize(
  CH_FEDERAL = max(CH_FEDERAL, na.rm = TRUE),
  CH_STATE = max(CH_STATE, na.rm = TRUE),
  CH_MUNICIPAL = max(CH_MUNICIPAL, na.rm = TRUE),
  CH_PRIVATE = max(CH_PRIVATE, na.rm = TRUE),
  CH_URBAN = max(CH_URBAN, na.rm = TRUE),
  CH_FUNDAMENTAL_EDU_I = max(CH_FUNDAMENTAL_EDU_I, na.rm = TRUE),
  CH_FUNDAMENTAL_EDU_T = max(CH_FUNDAMENTAL_EDU_T, na.rm = TRUE),
  CH_CLASSES_FUNDAMENTAL_EDU = mean(CH_CLASSES_FUNDAMENTAL_EDU, na.rm = TRUE),
  CH_CLASSES_FUNDAMENTAL_EDU_I = mean(CH_CLASSES_FUNDAMENTAL_EDU_I, na.rm = TRUE),
  CH_CLASSES_FUNDAMENTAL_EDU_T = mean(CH_CLASSES_FUNDAMENTAL_EDU_T, na.rm = TRUE),
  CH_MATRI_FUNDAMENTAL_EDU = mean(CH_MATRI_FUNDAMENTAL_EDU, na.rm = TRUE),
  CH_MATRI_FUNDAMENTAL_EDU_I = mean(CH_MATRI_FUNDAMENTAL_EDU_I, na.rm = TRUE),
  CH_MATRI_FUNDAMENTAL_EDU_T = mean(CH_MATRI_FUNDAMENTAL_EDU_T, na.rm = TRUE),
  FC_LIBRARY = max(FC_LIBRARY, na.rm = TRUE),
  FC_COMPUTER_LAB = max(FC_COMPUTER_LAB, na.rm = TRUE),
  FC_SCIENCE_LAB = max(FC_SCIENCE_LAB, na.rm = TRUE),
  FC_HAS_SPORTS_QUAD = max(FC_HAS_SPORTS_QUAD, na.rm = TRUE),
  FC_TV_EQUIPMENT = max(FC_TV_EQUIPMENT, na.rm = TRUE),
  FC_ANTENNA_EQUIPMENT = max(FC_ANTENNA_EQUIPMENT, na.rm = TRUE),
  FC_COMPUTER = max(FC_COMPUTER, na.rm = TRUE),
  FC_INTERNET = max(FC_INTERNET, na.rm = TRUE),
  FC_NUMBER_COMPUTERS = max(FC_NUMBER_COMPUTERS, na.rm = TRUE),
  FC_WATER_PUBLIC = max(FC_WATER_PUBLIC, na.rm = TRUE),
  FC_WATER = max(FC_WATER, na.rm = TRUE),
  FC_ELECTR_PUBLIC = max(FC_ELECTR_PUBLIC, na.rm = TRUE),
  FC_ELECTR = max(FC_ELECTR, na.rm = TRUE),
  FC_SEWAGE_PUBLIC = max(FC_SEWAGE_PUBLIC, na.rm = TRUE),
  FC_SEWAGE = max(FC_SEWAGE, na.rm = TRUE),
  ST_TOTAL_STAFF = mean(ST_TOTAL_STAFF, na.rm = TRUE),
  ST_FUND_EDU_TEACHERS = mean(ST_FUND_EDU_TEACHERS, na.rm = TRUE),
  STR_TEACHERS_INCOMPLETE_FUNDEDU = mean(STR_TEACHERS_INCOMPLETE_FUNDEDU, na.rm = TRUE),
  STR_TEACHERS_ONLY_FUNDEDU = mean(STR_TEACHERS_ONLY_FUNDEDU, na.rm = TRUE),
  STR_TEACHERS_SECONDARY_FUNDEDU = mean(STR_TEACHERS_SECONDARY_FUNDEDU, na.rm = TRUE),
  STR_TEACHERS_ADVANCED_FUNDEDU = mean(STR_TEACHERS_ADVANCED_FUNDEDU, na.rm = TRUE),
  ST_TEACHER_EDUCATION_YEARS = mean(ST_TEACHER_EDUCATION_YEARS, na.rm = TRUE),
  CLASS_SIZE = mean(CLASS_SIZE, na.rm = TRUE),
  CLASS_STUDENT_TEACHER_RATIO = mean(CLASS_STUDENT_TEACHER_RATIO, na.rm = TRUE),
  FC_ROOMS_AVAILABLE = mean(FC_ROOMS_AVAILABLE, na.rm = TRUE),
  FC_ROOMS_USED = mean(FC_ROOMS_USED, na.rm = TRUE)
)

schools <- schools_collapsed

# Add rural to urban
schools <- schools %>% mutate(CH_RURAL = ifelse(CH_URBAN == 0, 1, 0))
schools <- schools %>% mutate(FC_NO_SPORTS_QUAD = ifelse(FC_HAS_SPORTS_QUAD == 0, 1, 0))
schools <- schools %>% mutate(CH_SCHOOL_LARGE_SIZE = ifelse(FC_ROOMS_AVAILABLE>=10, 1, 0))

# Keep only schools and schools_outcomes variables in memory
rm(schools_collapsed)

# Keep only the CO_SCHOOL and CO_MUNICIPALITY variables in school_info
school_info <- school_info %>% select(CO_SCHOOL, CO_MUNICIPALITY)
# drop duplicates
school_info <- school_info %>% distinct()
# Merge school_info with schools on CO_SCHOOL
schools <- merge(schools, school_info, by = "CO_SCHOOL")
rm(school_info)

# Keep CO_SCHOOL, CH_FEDERAL, CH_STATE, CH_PRIVATE, CH_MUNICIPAL, CH_URBAN, CO_MUNICIPALITY
schools <- schools %>% select(CO_SCHOOL, CH_FEDERAL, CH_STATE, CH_PRIVATE, CH_MUNICIPAL, CH_URBAN, CO_MUNICIPALITY)

# Aggregate schools_outcomes by CO_SCHOOL, averaging all variables for each school ignoring NA values
# All the variables either start with "RATE" or "PROVA"" and are numeric
# They are RATE_APROV, RATE_APROV_I, RATE_APROV_T,
# RATE_ABANDON, RATE_ABANDON_I, RATE_ABANDON_T,
# RATE_FAILURE, RATE_FAILURE_I, RATE_FAILURE_T,
# PROVA_MEAN_PORT_I, PROVA_MEAN_PORT_T, PROVA_MEAN_MATH_I, PROVA_MEAN_MATH_T
# Keep only these variables in schools_outcomes
schools_outcomes <- schools_outcomes %>% select(CO_SCHOOL, RATE_APROV, RATE_APROV_I, RATE_APROV_T,
                                                RATE_ABANDON, RATE_ABANDON_I, RATE_ABANDON_T,
                                                RATE_FAILURE, RATE_FAILURE_I, RATE_FAILURE_T,
                                                PROVA_MEAN_PORT_I, PROVA_MEAN_PORT_T, PROVA_MEAN_MAT_I, PROVA_MEAN_MAT_T)
schools_outcomes_collapsed <- aggregate(schools_outcomes, by = list(schools_outcomes$CO_SCHOOL), FUN = mean, na.rm = TRUE)


# Merge schools with schools_outcomes on CO_SCHOOL
schools <- merge(schools, schools_outcomes_collapsed, by = "CO_SCHOOL")
rm(schools_outcomes, schools_outcomes_collapsed)

# Drop Group.1 column
schools <- schools %>% select(-Group.1)

# Drop all schools with NA in RATE_APROV
schools <- schools %>% filter(!is.na(RATE_APROV))
schools <- schools %>% filter(!is.na(RATE_ABANDON))
schools <- schools %>% filter(!is.na(RATE_FAILURE))

# Create a new variable with the average of the 2 PROVA_MEAN variables
schools <- schools %>% mutate(PROVA_MEAN_PORT = (PROVA_MEAN_PORT_I + PROVA_MEAN_PORT_T)/2)
schools <- schools %>% mutate(PROVA_MEAN_MAT = (PROVA_MEAN_MAT_I + PROVA_MEAN_MAT_T)/2)

# Standardize all PROVA_MEAN variables
schools <- schools %>% mutate(PROVA_MEAN_PORT_I = (PROVA_MEAN_PORT_I - mean(PROVA_MEAN_PORT_I, na.rm = TRUE))/sd(PROVA_MEAN_PORT_I, na.rm = TRUE))
schools <- schools %>% mutate(PROVA_MEAN_PORT_T = (PROVA_MEAN_PORT_T - mean(PROVA_MEAN_PORT_T, na.rm = TRUE))/sd(PROVA_MEAN_PORT_T, na.rm = TRUE))
schools <- schools %>% mutate(PROVA_MEAN_MAT_I = (PROVA_MEAN_MAT_I - mean(PROVA_MEAN_MAT_I, na.rm = TRUE))/sd(PROVA_MEAN_MAT_I, na.rm = TRUE))
schools <- schools %>% mutate(PROVA_MEAN_MAT_T = (PROVA_MEAN_MAT_T - mean(PROVA_MEAN_MAT_T, na.rm = TRUE))/sd(PROVA_MEAN_MAT_T, na.rm = TRUE))
schools <- schools %>% mutate(PROVA_MEAN_PORT = (PROVA_MEAN_PORT - mean(PROVA_MEAN_PORT, na.rm = TRUE))/sd(PROVA_MEAN_PORT, na.rm = TRUE))
schools <- schools %>% mutate(PROVA_MEAN_MAT = (PROVA_MEAN_MAT - mean(PROVA_MEAN_MAT, na.rm = TRUE))/sd(PROVA_MEAN_MAT, na.rm = TRUE))

# Convert the CH_FEDERAL, CH_STATE, CH_PRIVATE, CH_MUNICIPAL into one variable CH_SCHOOL_TYPE with 4 levels
schools <- schools %>% mutate(CH_SCHOOL_TYPE = ifelse(CH_FEDERAL == 1, "Federal", ifelse(CH_STATE == 1, "State", ifelse(CH_PRIVATE == 1, "Private", "Municipal"))))

# Convert the CH_URBAN, CH_RURAL into one variable CH_SCHOOL_LOCATION with 2 levels
schools <- schools %>% mutate(CH_SCHOOL_LOCATION = ifelse(CH_URBAN == 1, "Urban", "Rural"))

# Drop the CH_FEDERAL, CH_STATE, CH_PRIVATE, CH_MUNICIPAL, CH_URBAN, CH_RURAL variables
schools <- schools %>% select(-CH_FEDERAL, -CH_STATE, -CH_PRIVATE, -CH_MUNICIPAL, -CH_URBAN)

# Save the schools data frame to an RData file
save(schools, file = "data/schools_OLS.RData")

################################################
# Clean and process the municipalities data
################################################

# Load the municipalities data from Rdata file
load("data/schools_OLS.RData")
load("data/municipalities_main_effects.RData")

# Keep CO_MUNICIPALITY, physical_capital_focus, human_capital_focus, human_capital_and_physical_capital_focus
municipalities <- municipalities %>% select(CO_MUNICIPALITY, physical_capital_focus, human_capital_focus, human_capital_and_physical_capital_focus)

# Merge municipalities with schools on CO_MUNICIPALITY
schools <- merge(schools, municipalities, by = "CO_MUNICIPALITY")
rm(municipalities)

# Standardize the PROVA variables
schools <- schools %>% mutate(PROVA_MEAN_PORT_I = (PROVA_MEAN_PORT_I - mean(PROVA_MEAN_PORT_I, na.rm = TRUE))/sd(PROVA_MEAN_PORT_I, na.rm = TRUE))
schools <- schools %>% mutate(PROVA_MEAN_PORT_T = (PROVA_MEAN_PORT_T - mean(PROVA_MEAN_PORT_T, na.rm = TRUE))/sd(PROVA_MEAN_PORT_T, na.rm = TRUE))
schools <- schools %>% mutate(PROVA_MEAN_MAT_I = (PROVA_MEAN_MAT_I - mean(PROVA_MEAN_MAT_I, na.rm = TRUE))/sd(PROVA_MEAN_MAT_I, na.rm = TRUE))
schools <- schools %>% mutate(PROVA_MEAN_MAT_T = (PROVA_MEAN_MAT_T - mean(PROVA_MEAN_MAT_T, na.rm = TRUE))/sd(PROVA_MEAN_MAT_T, na.rm = TRUE))
schools <- schools %>% mutate(PROVA_MEAN_PORT = (PROVA_MEAN_PORT - mean(PROVA_MEAN_PORT, na.rm = TRUE))/sd(PROVA_MEAN_PORT, na.rm = TRUE))
schools <- schools %>% mutate(PROVA_MEAN_MAT = (PROVA_MEAN_MAT - mean(PROVA_MEAN_MAT, na.rm = TRUE))/sd(PROVA_MEAN_MAT, na.rm = TRUE))

# Replace all underscores in column names with periods
colnames(schools) <- gsub("_", ".", colnames(schools))

save(schools, file = "data/schools_OLS_cleaned.RData")
