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

# Load Data
setwd("G:/Other computers/My PC/Desktop/Cornell/Research/Thesis/Schools Analysis/thesis-brazil-schools-analysis")
schools <- read.csv("data/BrazilEduPanel_School.csv")
save(schools, file = "data/schools_raw.Rdata")
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
schools <- schools %>% group_by(CO_SCHOOL) %>% mutate(STATUS = ifelse(STATUS == 1, 1, 0)) %>% filter(STATUS == 1) %>% ungroup() %>% select(-STATUS)

# Drop all schools which have less than 10 years of data
schools <- schools %>% group_by(CO_SCHOOL) %>% filter(n() >= 10) %>% ungroup()

# Drop "TP_SITUACAO_FUNCIONAMENTO" and "TP_DEPENDENCIA" by name
drop_list = c("TP_SITUACAO_FUNCIONAMENTO", "TP_DEPENDENCIA")
schools <- schools[, !(names(schools) %in% drop_list)]

# Drop "TP_LOCALIZACAO" by name
drop_list = c("TP_LOCALIZACAO")
schools <- schools[, !(names(schools) %in% drop_list)]


