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
# #schools <- read.csv("data/BrazilEduPanel_School.csv")
# #save(schools, file = "data/schools_raw.Rdata")
# load("data/schools_raw.Rdata")

# # Name Columns
# cols <- names(schools)
# cols <- gsub(" ", "", cols)

# # Drop Columns: "UF", "CO_UF", "NO_ENTIDADE" by name
# drop_list = c("UF", "CO_UF", "NO_ENTIDADE")
# schools <- schools[, !(names(schools) %in% drop_list)]

# # Rename Columns: "CO_MUNICIPIO" to "CO_MUNICIPALITY", "CO_ENTIDADE" to "CO_SCHOOL", "NU_ANO_CENSO" to "CO_YEAR", "SIGLA" to "NAME_STATE", "MUNIC" to "NAME_MUNICIPALITY"
# schools <- schools %>% dplyr::rename("CO_MUNICIPALITY" = "CO_MUNICIPIO", 'CO_SCHOOL' = 'CO_ENTIDADE', 'CO_YEAR' = 'NU_ANO_CENSO', 'NAME_STATE' = 'SIGLA', 'NAME_MUNICIPALITY' = 'MUNIC')

# # Perform an operation on each CO_SCHOOL - drop it if "STATUS" is 0 after ever being 1
# # schools <- schools %>% group_by(CO_SCHOOL) %>% mutate(STATUS = ifelse(STATUS == 1, 1, 0)) %>% filter(STATUS == 1) %>% ungroup() %>% select(-STATUS)

# # Drop all schools which have less than 10 years of data
# # schools <- schools %>% group_by(CO_SCHOOL) %>% filter(n() >= 10) %>% ungroup()

# # Drop "TP_SITUACAO_FUNCIONAMENTO" and "TP_DEPENDENCIA" by name
# drop_list = c("TP_SITUACAO_FUNCIONAMENTO", "TP_DEPENDENCIA")
# schools <- schools[, !(names(schools) %in% drop_list)]

# # Drop "TP_LOCALIZACAO" by name
# drop_list = c("TP_LOCALIZACAO")
# schools <- schools[, !(names(schools) %in% drop_list)]

# # Subset the data to keep only the rows from 2005 to 2015
# schools <- subset(schools, CO_YEAR >= 2005 & CO_YEAR <= 2015)

# # Drop every school that is inactive in its last year of entry
# schools <- schools %>% filter(!(STATUS == "Inactive"))


# # Drop every school with less than 3 observations
# #schools <- schools %>% group_by(CO_SCHOOL) %>% filter(n() >= 3) %>% ungroup()

# # Drop columns with most entries as NA
# drop_list = c("EIGHTYEARS", "NINEYEARS", "NU_SALAS_EXISTENTES", "NU_SALAS_UTILIZADAS", "NU_EQUIP_TV", "PROFESS", "CLASSSIZE_T", "CLASSSIZE_I", "STATUS")
# schools <- schools[, !(names(schools) %in% drop_list)]

# # Create a list of outcome vars
# outcome_vars = grep("^RATE_|^PROVA_", names(schools), value = TRUE)
# outcome_vars = append("CO_SCHOOL", outcome_vars)
                      
# # create the schools_outcomes data frame by subsetting the schools data frame to include only the specified columns
# schools_outcomes <- subset(schools, select = outcome_vars)
# outcome_vars <- setdiff(outcome_vars, "CO_SCHOOL")

# # Filter schools to remove outcome vars
# schools <- schools %>% select(-one_of(outcome_vars))

# # DROP TP_CATEGORIA_ESCOLA_PRIVADA ONLY IF YOURE NOT ANALYZING PRIVATE SCHOOLS SEPERATELY
# drop_list = c("TP_CATEGORIA_ESCOLA_PRIVADA")
# schools <- schools[, !(names(schools) %in% drop_list)]

# # save(schools, schools_outcomes, file = "data/schools_cleaned.Rdata")

# load("data/schools_cleaned.Rdata")
# 
# ################################################
# # Clean and process the dataset for training
# ################################################
# # 
# # Rename variables as characteristics and facilities
# schools <- plyr::rename(schools, c(
#   'FEDERAL' = 'CH_FEDERAL',
#   'ESTADUAL' = 'CH_STATE',
#   'MUNICIPAL' = 'CH_MUNICIPAL',
#   'PRIVADA' = 'CH_PRIVATE',
#   'URBANA' = 'CH_URBAN',
#   'NIVFUND_1' = 'CH_FUNDAMENTAL_EDU_I',
#   'NIVFUND_2' = 'CH_FUNDAMENTAL_EDU_T',
#   'TURMAFUND' = 'CH_CLASSES_FUNDAMENTAL_EDU',
#   'TURMAFUND_I' = 'CH_CLASSES_FUNDAMENTAL_EDU_I',
#   'TURMAFUND_T' = 'CH_CLASSES_FUNDAMENTAL_EDU_T',
#   'MATFUND' = 'CH_MATRI_FUNDAMENTAL_EDU',
#   'MATFUND_I' = 'CH_MATRI_FUNDAMENTAL_EDU_I',
#   'MATFUND_T' = 'CH_MATRI_FUNDAMENTAL_EDU_T'
#   ))
# schools <- plyr::rename(schools, c(
#   'IN_BIBLIOTECA' = 'FC_LIBRARY',
#   'IN_LABORATORIO_INFORMATICA' = 'FC_COMPUTER_LAB',
#   'IN_LABORATORIO_CIENCIAS' = 'FC_SCIENCE_LAB',
#   'IN_QUADRA_ESPORTES' = 'FC_HAS_SPORTS_QUAD',
#   'IN_EQUIP_TV' = 'FC_TV_EQUIPMENT',
#   'IN_EQUIP_PARABOLICA' = 'FC_ANTENNA_EQUIPMENT',
#   'IN_COMPUTADOR' = 'FC_COMPUTER',
#   'IN_INTERNET' = 'FC_INTERNET',
#   'NU_COMPUTADOR' = 'FC_NUMBER_COMPUTERS',
#   'SCHOOL_WATER_PUBLIC' = 'FC_WATER_PUBLIC',
#   'SCHOOL_WATER' = 'FC_WATER',
#   'SCHOOL_ELECTR_PUB' = 'FC_ELECTR_PUBLIC',
#   'SCHOOL_ELECTR' = 'FC_ELECTR',
#   'SCHOOL_SEWAGE_PUB' = 'FC_SEWAGE_PUBLIC',
#   'SCHOOL_SEWAGE' = 'FC_SEWAGE'
# ))
# schools <- plyr::rename(schools, c(
#   'NU_FUNCIONARIOS' = 'ST_TOTAL_STAFF',
#   'PROFFUNDTOT' = 'ST_FUND_EDU_TEACHERS',
#   'PCPROFFUNDINC' = 'STR_TEACHERS_INCOMPLETE_FUNDEDU',
#   'PCPROFFUNDCOMP' = 'STR_TEACHERS_ONLY_FUNDEDU',
#   'PCPROFMED' = 'STR_TEACHERS_SECONDARY_FUNDEDU',
#   'PCPROFSUP' = 'STR_TEACHERS_ADVANCED_FUNDEDU',
#   'EDUCTEACH' = 'ST_TEACHER_EDUCATION_YEARS',
#   'CLASSSIZE' = 'CLASS_SIZE',
#   'STUDTEACH' = 'CLASS_STUDENT_TEACHER_RATIO',
#   'ROOMS_AVAIL' = 'FC_ROOMS_AVAILABLE',
#   'ROOMS_USED' = 'FC_ROOMS_USED'
# ))
# 
# school_info <- schools %>% select(CO_SCHOOL, CO_MUNICIPALITY, NAME_STATE, NAME_MUNICIPALITY)
# schools <- schools %>% select(-c(CO_MUNICIPALITY, NAME_STATE, NAME_MUNICIPALITY, CO_YEAR))
# max_vars <-   c("CH_FEDERAL", "CH_STATE", "CH_MUNICIPAL", "CH_PRIVATE", "CH_URBAN",
#                 "CH_FUNDAMENTAL_EDU_I", "CH_FUNDAMENTAL_EDU_T",
#                 "FC_LIBRARY", "FC_COMPUTER_LAB", "FC_SCIENCE_LAB", "FC_HAS_SPORTS_QUAD",
#                 "FC_TV_EQUIPMENT", "FC_ANTENNA_EQUIPMENT", "FC_COMPUTER", "FC_INTERNET",
#                 "FC_NUMBER_COMPUTERS", "FC_WATER_PUBLIC", "FC_WATER", "FC_ELECTR_PUBLIC",
#                 "FC_ELECTR", "FC_SEWAGE_PUBLIC", "FC_SEWAGE")
# 
# schools_collapsed_max <- schools %>% group_by(CO_SCHOOL) %>% summarise_at(max_vars, max, na.rm = TRUE)
# mean_vars <- setdiff(names(schools), max_vars)
# mean_vars <- setdiff(mean_vars, "CO_SCHOOL")
# schools_collapsed_mean <- schools %>% group_by(CO_SCHOOL) %>% summarise_at(mean_vars, mean, na.rm = TRUE)
# merge_collapsed <- merge(schools_collapsed_max, schools_collapsed_mean, by = "CO_SCHOOL")
# schools <- merge_collapsed
# rm(merge_collapsed, max_vars, schools_collapsed_max, schools_collapsed_mean, max_vars, mean_vars)
# # Add rural to urban
# schools <- schools %>% mutate(CH_RURAL = ifelse(CH_URBAN == 0, 1, 0))
# schools <- schools %>% mutate(FC_NO_SPORTS_QUAD = ifelse(FC_HAS_SPORTS_QUAD == 0, 1, 0))
# schools <- schools %>% mutate(CH_SCHOOL_LARGE_SIZE = ifelse(FC_ROOMS_AVAILABLE>=10, 1, 0))
# # Can create variable for large school physically vs small school physically by rooms available
# 
# 
# # drop any columns with -inf values as a result of the max function
# schools <- schools[!apply(schools, 1, function(row) any(is.infinite(row))), ]
# save(schools, school_info, file = "data/schools_for_municipality_processing.Rdata")
# 
load("data/schools_for_municipality_processing.Rdata")

##############################################################################
# Update school_outcomes
##############################################################################

# Drop duplicates in school info
municipal_info <- subset(school_info, select = c("CO_MUNICIPALITY", "NAME_STATE"))
school_info <- subset(school_info, select = c("CO_SCHOOL", "CO_MUNICIPALITY"))
municipal_info <- municipal_info[!duplicated(municipal_info$CO_MUNICIPALITY), ]

# Match school info
school_info <- school_info[!duplicated(school_info$CO_SCHOOL), ]
school_info <- school_info[school_info$CO_SCHOOL %in% schools$CO_SCHOOL, ]

# Collapse schools by municipality by merging
schools <- merge(school_info, schools, by = "CO_SCHOOL")
rm(school_info)

schools_collapsed <- schools %>% group_by(CO_MUNICIPALITY) %>% summarise_all(mean, na.rm = TRUE)
municipalities <- subset(schools_collapsed, select = -CO_SCHOOL)
rm(schools_collapsed, schools)

municipal_info <- municipal_info %>%
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

drop_list = c("NAME_STATE")
municipal_info <- municipal_info[, !(names(municipal_info) %in% drop_list)]

# Merge the datasets
merged_muns <- merge(municipal_info, municipalities, by = "CO_MUNICIPALITY")
municipalities <- merged_muns
rm(merged_muns, municipal_info, drop_list)

# Remove municipalities with -inf characteristics
municipalities_clean <- municipalities %>%  na.omit()
municipalities_clean <- municipalities_clean[!apply(municipalities_clean, 1, function(row) any(is.infinite(row))), ]

municipalities <- municipalities_clean
rm(municipalities_clean)

# Create location interactions
states <- grep("^CH_STATE_", names(municipalities), value = TRUE)
characteristics <- setdiff(grep("^CH_", names(municipalities), value = TRUE), grep("^CH_STATE_", names(municipalities), value = TRUE))
characteristics <- setdiff(characteristics, grep("^CH_CLASSES", names(municipalities), value = TRUE))
characteristics <- setdiff(characteristics, grep("^CH_MATRI", names(municipalities), value = TRUE))
facilities <- setdiff(grep("^FC_", names(municipalities), value = TRUE), c("FC_NUMBER_COMPUTERS", "FC_ROOMS_AVAILABLE", "FC_ROOMS_USED"))

################################################################################
# FOR NOW WE ONLY CREATE THESE INTERACTIONS, but next we INTERACT STAFF AS WELL
################################################################################

# loop over the list of variables
for (i in 1:length(states)) {
  # loop over the list of variables again
  for (j in 1:length(characteristics)) {
    # create an interaction variable between variables i and j
    municipalities[, paste0("IN_", states[i], "_x_", characteristics[j])] <- municipalities[, states[i]] * municipalities[, characteristics[j]]
  }
  for (j in 1:length(facilities)) {
    # create an interaction variable between variables i and j
    municipalities[, paste0("IN_", states[i], "_x_", facilities[j])] <- municipalities[, states[i]] * municipalities[, facilities[j]]
  }
}

for (i in 1:length(characteristics)) {
  # loop over the list of variables again
  for (j in 1:length(facilities)) {
    # create an interaction variable between variables i and j
    municipalities[, paste0("IN_", characteristics[i], "_x_", facilities[j])] <- municipalities[, characteristics[i]] * municipalities[, facilities[j]]
  }
}

ml_vars <- setdiff(names(municipalities), c("CO_MUNICIPALITY"))

# Figure out how jorge handles continuous variables

###############################################################################
# Try w continuous variables first, and if they dont work, try single
###############################################################################

###############################################################################
# Create municipal outcomes
###############################################################################

municipalities_outcomes <- read.csv("data/BrazilEduPanel_Municipal.csv")
load("data/schools_cleaned.Rdata")
schools_outcomes <- schools_outcomes %>% group_by(CO_SCHOOL) %>% summarise_all(mean, na.rm = TRUE)
schools_outcomes <- schools_outcomes %>% select(matches("PROVA|CO"))
municipal_dict <- schools %>% select(c(CO_SCHOOL, CO_MUNICIPALITY)) %>% unique()
schools_outcomes_merged <- merge(schools_outcomes, municipal_dict, by = "CO_SCHOOL")
schools_outcomes_collapsed <- schools_outcomes_merged %>% group_by(CO_MUNICIPALITY) %>% summarise_all(mean, na.rm = TRUE)
municipal_scores <- schools_outcomes_collapsed %>% select(-CO_SCHOOL)
rm(schools_collapsed, schools, municipal_dict, schools_outcomes, schools_outcomes_collapsed, schools_outcomes_merged)

municipalities_outcomes_clean <- subset(municipalities_outcomes, NU_ANO_CENSO >= 2005 & NU_ANO_CENSO <= 2015) %>% 
  select(matches("RATE_|CO_MUNICIPIO")) %>%
  group_by(CO_MUNICIPIO) %>% summarise_all(mean, na.rm = TRUE)
municipalities_outcomes <- municipalities_outcomes_clean
rm(municipalities_outcomes_clean)
municipalities_outcomes <- municipalities_outcomes %>% rename(CO_MUNICIPALITY = CO_MUNICIPIO)
municipalities_outcomes <- merge(municipalities_outcomes, municipal_scores, by = "CO_MUNICIPALITY")
rm(municipal_scores)
municipalities_outcomes <- municipalities_outcomes %>% 
  select(CO_MUNICIPALITY, RATE_APROV, RATE_FAILURE, RATE_ABANDON,
         RATE_APROV_PUB, RATE_FAILURE_PUB, RATE_ABANDON_PUB,
         PROVA_MEAN_MAT_I, PROVA_MEAN_MAT_T, PROVA_MEAN_PORT_I, PROVA_MEAN_PORT_T)

# OK SO 826 municipalities lack Rate info, and only 51 lack Prova info
# YOU can try to aggregate rate info from schools as well
# Or keep it this way, and conduct analyses twice - one dataset for rate analysis and other for Prova analysis

municipalities_outcomes <- municipalities_outcomes %>% na.omit()
municipalities_merged <- merge(municipalities, municipalities_outcomes, by = "CO_MUNICIPALITY")
municipalities <- municipalities_merged
rm(municipalities_outcomes, municipalities_merged)

municipalities$RATE_APROV_DIFF <- municipalities$RATE_APROV_PUB - municipalities$RATE_APROV
municipalities$RATE_FAILURE_DIFF <- municipalities$RATE_FAILURE_PUB - municipalities$RATE_FAILURE
municipalities$RATE_ABANDON_DIFF <- municipalities$RATE_ABANDON_PUB - municipalities$RATE_ABANDON

municipalities_outcomes <- municipalities %>% select(c(matches("PROVA|RATE"), "CO_MUNICIPALITY"))
municipalities <- municipalities %>% select(-matches("PROVA|RATE"))

save(municipalities, municipalities_outcomes, ml_vars, file = "data/municipalities_processed.Rdata")