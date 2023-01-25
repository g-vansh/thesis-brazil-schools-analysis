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
library(STE)

# Load Data
setwd("G:/Other computers/My PC/Desktop/Cornell/Research/Thesis/Schools Analysis/thesis-brazil-schools-analysis")
load("data/schools_processed.Rdata")

# Try internet next
# Look at only private schools as well

################################################################################
# Things that can be changed in the analysis:
# 1. Strategy (library, science class, etc)
# 2. Outcome Studied (Pass rate, abandon rate, scores prova brasil)
# 3. Subsetting only a certain kind of school which actually has agency 
#    (we can avoid this by looking at literature and creatiing a strategy that
#     is actually implementable by any schools)
################################################################################

################################################################################\
# ONLY FOR P_SCORE GENERATION
################################################################################
# ml_vars <- setdiff(ml_vars, grep("FC_SCIENCE_LAB", names(schools), value = TRUE))
# p_scores <- estimate_propensity(
#   treatment = schools$FC_SCIENCE_LAB,
#   X = schools[, ml_vars]
# )
# p_scores_science_lab <- p_scores
# save(p_scores_science_lab, file = "data/p_scores/p_scores_science_lab.Rdata")
################################################################################

# Create a function that performs all of the following steps for different outcomes and saves the histograms of the STEs and treatment effects, along with a dataframe of the coherence values for each outcome.
# 1. Estimate STE
# 2. Get top STE determinants
# 3. Remove NA values for analysis
# 4. Estimate coherence value
# 5. Save the histograms of the STEs and treatment effects
# 6. Save the dataframe of the top 20 STE determinants and the bottom 20 STE determinants (by coefficient) for each outcome

# How to define the function:
# 1. Input: outcome, strategy, propensity, df
# 2. Output: dataframe of the top 20 STE determinants and the bottom 20 STE determinants (by coefficient) for each outcome
# 3. Save the histograms of the STEs and treatment effects
# 4. Save the dataframe of the top 20 STE determinants and the bottom 20 STE determinants (by coefficient) for each outcome
# 5. Return the coherence value

outcome_analyser <- function(outcome, strategy, propensity, schools_outcomes, schools, ml_vars, outcome_name, strategy_name) {
  # Estimate the strategic treatment effect.

  schools_ste <- estimate_ste(
    y = outcome,
    treatment = strategy,
    propensity = propensity,
    df = schools
  )
  
  # Study the determinants of STE.
  ste_features <- STE::get_top_ste_determinants(
    X = schools_ste[, ml_vars],
    teffect = schools_ste$teffect
  )
  
  # Remove NA values for analysis. 
  schools_ste.clean <- schools_ste %>%
    filter(!is.na(ste))
  
  # Estimate the coherence value.
  ml_vars.no_inter <- setdiff(ml_vars, ml_vars[grep("_x_",ml_vars)])
  schools_coherence <- STE::estimate_coherence(
    y = schools_ste.clean$teffect,
    x = schools_ste.clean[, ml_vars],
    x.no_inter = schools_ste.clean[, ml_vars.no_inter]
  )
  
  # Save the histograms of the STEs and treatment effects
  title_main <- paste("STE Histogram - ", outcome_name)
  p <- ggplot(schools_ste, aes(x=ste)) + 
    geom_histogram(color="black", fill="white") +
    labs(title=title_main,x="Strategic Treatment Effect", y = "Frequency")
  ggsave(filename = file.path(paste0("plots/", strategy_name, "/STE_Histograms"), paste0(outcome_name, ".png")), plot = p, device = "png")
  rm(p)
  
  title_main <- paste("Treatment Effect Histogram - ", outcome_name)
  p <- ggplot(schools_ste, aes(x=teffect)) + 
    geom_histogram(color="black", fill="white") +
    labs(title=title_main,x="Treatment Effect", y = "Frequency")
  ggsave(filename = file.path(paste0("plots/", strategy_name, "/TE_Histograms"), paste0(outcome_name, ".png")), plot = p, device = "png")
  
  # Save the dataframe of the top 20 STE determinants and the bottom 20 STE determinants (by coefficient) for each outcome
  path <- paste("tables/", strategy_name, "/STE_Determinants/STE_Determinants_", outcome_name, ".csv", sep = "")
  write.csv(ste_features, path)
  print(paste("Processed outcome: ", outcome_name, sep = ""))
  return(schools_coherence)
}
  
# a for loop that runs the above function for outcomes and saves the coherence values in a dataframe
coherence_values <- data.frame()
outcomes <- c("RATE_APROV", "RATE_ABANDON", "RATE_TRANSFER", "PROVA_MEAN_PORT_I", "PROVA_MEAN_PORT_T", "PROVA_MEAN_MAT_I", "PROVA_MEAN_MAT_T")
strategy <- schools$FC_INTERNET
load("data/p_scores/p_scores_internet.Rdata")
p_scores <- p_scores_internet
ml_vars <- setdiff(ml_vars, grep("FC_INTERNET", names(schools), value = TRUE))

# Write the for loop
for(outcome_name in outcomes) {
  outcome <- sapply(schools_outcomes[[outcome_name]], as.numeric)
  strategy_name <- "internet"
  schools_coherence <- outcome_analyser(outcome, strategy, p_scores, schools_outcomes, schools, ml_vars, outcome_name, strategy_name)
  # Save coherence values with info on the outcome and strategy
  coherence_values <- rbind(coherence_values, data.frame(strategy_name, outcome_name, schools_coherence))
}
# Save coherence values in CSV file
write.csv(coherence_values, "tables/coherence/Coherence_Values_Internet.csv")