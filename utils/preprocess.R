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

# Name Columns
cols <- names(schools)
#cb_startups = cb_startups %>%
#  mutate(early_stage_amount_log = log(early_stage_amount),
#         bearly_stage_has_vc = as.double(cb_startups$bearly_stage_has_vc),
#         early_stage_bin = floor(log10(early_stage_amount)*2))

#ml_vars <- c(cols[grepl("^I.*", cols)],
             #cols[grepl(".*_ed.*", cols)],
             #cols[grepl("^ed.*", cols)],"early_stage_bin")
#sum(is.na(cb_startups[,ml_vars]))

#colSums(is.na(cb_startups[,ml_vars]))
#dim(cb_startups)

