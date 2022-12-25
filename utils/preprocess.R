rm(list=ls())

# Import Libraries
library(readstata13)
library(glmnet)
library(randomForest)
library(pROC)
library(dotwhisker)
library(rcompanion)
library(caret)
library(repr)
library(plyr)
library(binsreg)
library(stargazer)
library(ggplot2)
library(gtools)
library(tidyr)
library(dplyr)
library(hdm)
library(stringr)
library(lfe)
library(sandwich)
library(AER)
library(ggpubr)
library(remotes)
library(lubridate)
library(plyr)

# Load Data
setwd("G:/Other computers/My PC/Desktop/Cornell/Research/Thesis/Schools Analysis/thesis-brazil-schools-analysis")
#schools <- read.csv("data/BrazilEduPanel_School.csv")
#save(schools, file = "data/schools_raw.Rdata")
load("data/schools_raw.Rdata")

# Name Columns
cols <- names(schools)
cols <- gsub(" ", "", cols)

# Drop Columns: "UF", "CO_UF", "NO_ENTIDADE" by name
drop_list = c("UF", "CO_UF", "NO_ENTIDADE")
schools <- schools[, !(names(schools) %in% drop_list)]

# Rename Columns: "CO_MUNICIPIO" to "CO_MUNICIPALITY", "CO_ENTIDADE" to "CO_SCHOOL", "NU_ANO_CENSO" to "CO_YEAR", "SIGLA" to "NAME_STATE", "MUNIC" to "NAME_MUNICIPALITY"
schools <- schools %>% dplyr::rename("CO_MUNICIPALITY" = "CO_MUNICIPIO", 'CO_SCHOOL' = 'CO_ENTIDADE', 'CO_YEAR' = 'NU_ANO_CENSO', 'NAME_STATE' = 'SIGLA', 'NAME_MUNICIPALITY' = 'MUNIC')

# Perform an operation on each CO_SCHOOL - drop it if "STATUS" is 0 after ever being 1
# schools <- schools %>% group_by(CO_SCHOOL) %>% mutate(STATUS = ifelse(STATUS == 1, 1, 0)) %>% filter(STATUS == 1) %>% ungroup() %>% select(-STATUS)

# Drop all schools which have less than 10 years of data
# schools <- schools %>% group_by(CO_SCHOOL) %>% filter(n() >= 10) %>% ungroup()

# Drop "TP_SITUACAO_FUNCIONAMENTO" and "TP_DEPENDENCIA" by name
drop_list = c("TP_SITUACAO_FUNCIONAMENTO", "TP_DEPENDENCIA")
schools <- schools[, !(names(schools) %in% drop_list)]

# Drop "TP_LOCALIZACAO" by name
drop_list = c("TP_LOCALIZACAO")
schools <- schools[, !(names(schools) %in% drop_list)]

# Subset the data to keep only the rows from 2005 to 2015
schools <- subset(schools, CO_YEAR >= 2005 & CO_YEAR <= 2015)

# Drop every school that is inactive in its last year of entry
schools <- schools %>% filter(!(STATUS == "Inactive"))


# Drop every school with less than 3 observations
#schools <- schools %>% group_by(CO_SCHOOL) %>% filter(n() >= 3) %>% ungroup()

# Drop columns with most entries as NA
drop_list = c("EIGHTYEARS", "NINEYEARS", "NU_SALAS_EXISTENTES", "NU_SALAS_UTILIZADAS", "NU_EQUIP_TV", "PROFESS", "CLASSSIZE_T", "CLASSSIZE_I", "STATUS")
schools <- schools[, !(names(schools) %in% drop_list)]

# Create a list of outcome vars
outcome_vars = grep("^RATE_|^PROVA_", names(schools), value = TRUE)
outcome_vars = append("CO_SCHOOL", outcome_vars)
                      
# create the schools_outcomes data frame by subsetting the schools data frame to include only the specified columns
schools_outcomes <- subset(schools, select = outcome_vars)
outcome_vars <- setdiff(outcome_vars, "CO_SCHOOL")

# Filter schools to remove outcome vars
schools <- schools %>% select(-one_of(outcome_vars))

# DROP TP_CATEGORIA_ESCOLA_PRIVADA ONLY IF YOURE NOT ANALYZING PRIVATE SCHOOLS SEPERATELY
drop_list = c("TP_CATEGORIA_ESCOLA_PRIVADA")
schools <- schools[, !(names(schools) %in% drop_list)]

# save(schools, schools_outcomes, file = "data/schools_cleaned.Rdata")

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


#fun_list <- list(max, max, max, max, max, max, max, 
#                 mean, mean, mean, mean, mean, mean, 
#                 max, max, max, max, max, max, max, max, max, max, max, max, max, max, max, 
#                 mean, mean, mean, mean, mean, mean, mean, mean, mean, mean, mean)


#names(fun_list) <- setdiff(names(schools), c("CO_YEAR", "CO_SCHOOL", "CO_MUNICIPALITY", "NAME_STATE", "NAME_MUNICIPALITY"))

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
# Can create variable for large school physically vs small school physically by rooms available 

schools <- drop_na(schools)
##############################################################################
# Update school_outcomes
##############################################################################

# Collapsing OUTCOMES
schools_outcomes_collapsed <- aggregate(schools_outcomes, by = list(schools_outcomes$CO_SCHOOL), FUN = mean, na.rm = TRUE)
schools_outcomes <- drop_na(schools_outcomes_collapsed)

rm(schools_collapsed, schools_outcomes_collapsed, fun_list)

# Filter all variables
schools = schools[schools$CO_SCHOOL %in% schools_outcomes$CO_SCHOOL, ]
school_info = school_info[school_info$CO_SCHOOL %in% schools$CO_SCHOOL, ]
schools_outcomes = schools_outcomes[schools_outcomes$CO_SCHOOL %in% schools$CO_SCHOOL, ]
school_info <- school_info[!duplicated(school_info$CO_SCHOOL), ]

school_info <- school_info %>%
  mutate(
    CH_STATE_AC = case_when(NAME_STATE == "AC" ~ 1, TRUE ~ 0),
    CH_STATE_AL = case_when(NAME_STATE == "AL" ~ 1, TRUE ~ 0),
    CH_STATE_AM = case_when(NAME_STATE == "AM" ~ 1, TRUE ~ 0),
    CH_STATE_AP = case_when(NAME_STATE == "AP" ~ 1, TRUE ~ 0),
    CH_STATE_BA = case_when(NAME_STATE == "BA" ~ 1, TRUE ~ 0),
    CH_STATE_CE = case_when(NAME_STATE == "CE" ~ 1, TRUE ~ 0),
    CH_STATE_DF = case_when(NAME_STATE == "DF" ~ 1, TRUE ~ 0),
    CH_STATE_ES = case_when(NAME_STATE == "ES" ~ 1, TRUE ~ 0),
    CH_STATE_GO = case_when(NAME_STATE == "GO" ~ 1, TRUE ~ 0),
    CH_STATE_MA = case_when(NAME_STATE == "MA" ~ 1, TRUE ~ 0),
    CH_STATE_MG = case_when(NAME_STATE == "MG" ~ 1, TRUE ~ 0),
    CH_STATE_MS = case_when(NAME_STATE == "MS" ~ 1, TRUE ~ 0),
    CH_STATE_MT = case_when(NAME_STATE == "MT" ~ 1, TRUE ~ 0),
    CH_STATE_PA = case_when(NAME_STATE == "PA" ~ 1, TRUE ~ 0),
    CH_STATE_PB = case_when(NAME_STATE == "PB" ~ 1, TRUE ~ 0),
    CH_STATE_PE = case_when(NAME_STATE == "PE" ~ 1, TRUE ~ 0),
    CH_STATE_PI = case_when(NAME_STATE == "PI" ~ 1, TRUE ~ 0),
    CH_STATE_PR = case_when(NAME_STATE == "PR" ~ 1, TRUE ~ 0),
    CH_STATE_RJ = case_when(NAME_STATE == "RJ" ~ 1, TRUE ~ 0),
    CH_STATE_RN = case_when(NAME_STATE == "RN" ~ 1, TRUE ~ 0),
    CH_STATE_RO = case_when(NAME_STATE == "RO" ~ 1, TRUE ~ 0),
    CH_STATE_RR = case_when(NAME_STATE == "RR" ~ 1, TRUE ~ 0),
    CH_STATE_RS = case_when(NAME_STATE == "RS" ~ 1, TRUE ~ 0),
    CH_STATE_SC = case_when(NAME_STATE == "SC" ~ 1, TRUE ~ 0),
    CH_STATE_SE = case_when(NAME_STATE == "SE" ~ 1, TRUE ~ 0),
    CH_STATE_SP = case_when(NAME_STATE == "SP" ~ 1, TRUE ~ 0),
    CH_STATE_TO = case_when(NAME_STATE == "TO" ~ 1, TRUE ~ 0)
  )

drop_list = c("NAME_STATE", "NAME_MUNICIPALITY", "CO_MUNICIPALITY")
school_info <- school_info[, !(names(school_info) %in% drop_list)]
drop_list = c("Group.1")
schools_outcomes <- schools_outcomes[, !(names(schools_outcomes) %in% drop_list)]

# Merge the datasets
merged_schools <- merge(school_info, schools, by = "CO_SCHOOL")
schools <- merged_schools
rm(merged_schools, school_info)

# Create location interactions
states <- grep("^CH_STATE_", names(schools), value = TRUE)
characteristics <- setdiff(grep("^CH_", names(schools), value = TRUE), grep("^CH_STATE_", names(schools), value = TRUE))
characteristics <- setdiff(characteristics, grep("^CH_CLASSES", names(schools), value = TRUE))
characteristics <- setdiff(characteristics, grep("^CH_MATRI", names(schools), value = TRUE))
facilities <- setdiff(grep("^FC_", names(schools), value = TRUE), c("FC_NUMBER_COMPUTERS", "FC_ROOMS_AVAILABLE", "FC_ROOMS_USED"))

################################################################################
# FOR NOW WE ONLY CREATE THESE INTERACTIONS, but next we INTERACT STAFF AS WELL
################################################################################

# loop over the list of variables
for (i in 1:length(states)) {
  # loop over the list of variables again
  for (j in 1:length(characteristics)) {
    # create an interaction variable between variables i and j
    schools[, paste0("IN_", states[i], "_x_", characteristics[j])] <- schools[, states[i]] * schools[, characteristics[j]]
  }
  for (j in 1:length(facilities)) {
    # create an interaction variable between variables i and j
    schools[, paste0("IN_", states[i], "_x_", facilities[j])] <- schools[, states[i]] * schools[, facilities[j]]
  }
}

for (i in 1:length(characteristics)) {
  # loop over the list of variables again
  for (j in 1:length(facilities)) {
    # create an interaction variable between variables i and j
    schools[, paste0("IN_", characteristics[i], "_x_", facilities[j])] <- schools[, characteristics[i]] * schools[, facilities[j]]
  }
}

ml_vars <- setdiff(names(schools), c("CO_SCHOOL"))
#save(schools, schools_outcomes, ml_vars, file = "data/schools_processed.Rdata")
load("data/schools_processed.Rdata")

# Figure out how jorge handles continuous variables

###############################################################################
# Try w continuous variables first, and if they dont work, try single
###############################################################################

# Try running the program now

install_github("g-vansh/STE")
library(STE)

ml_vars <- setdiff(ml_vars, grep("FC_SCIENCE_LAB", names(schools), value = TRUE))

p_scores <- estimate_propensity(
  treatment = schools$FC_SCIENCE_LAB,
  X = schools[, ml_vars]
)

p_scores_science_lab <- p_scores
save(p_scores_science_lab, file = "data/p_scores/p_scores_science_lab.Rdata")
