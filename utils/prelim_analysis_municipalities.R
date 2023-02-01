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
setwd("U:/Research/Thesis/Schools Analysis/thesis-brazil-schools-analysis")
load("data/municipalities_processed.Rdata")

################################################################################
# Things that can be changed in the analysis:
# 1. Strategy (library, science class, etc)
# 2. Outcome Studied (Pass rate, abandon rate, scores prova brasil)
# 3. Subsetting only a certain kind of school which actually has agency 
#    (we can avoid this by looking at literature and creatiing a strategy that
#     is actually implementable by any schools)
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

outcome_analyser <- function(outcome, strategy, propensity, municipality_outcomes, municipalities, ml_vars, outcome_name, strategy_name) {
  # Create the empty folders for the plots and tables
  dir.create(paste0("plots/", strategy_name, "/STE_Histograms_Municipalities"), showWarnings = FALSE)
  dir.create(paste0("plots/", strategy_name, "/TE_Histograms_Municipalities"), showWarnings = FALSE)
  dir.create(paste0("tables/", strategy_name, "/STE_Determinants_Municipalities"), showWarnings = FALSE)

  # Estimate the strategic treatment effect.
  municipalities_ste <- estimate_ste(
    y = outcome,
    treatment = strategy,
    propensity = propensity,
    df = municipalities
  )
  
  # Study the determinants of STE.
  ste_features <- STE::get_top_ste_determinants(
    X = municipalities_ste[, ml_vars],
    teffect = municipalities_ste$teffect
  )
  
  # Remove NA values for analysis. 
  municipalities_ste.clean <- municipalities_ste %>%
    filter(!is.na(ste))
  
  # Estimate the coherence value.
  ml_vars.no_inter <- setdiff(ml_vars, ml_vars[grep("_x_",ml_vars)])
  municipalities_coherence <- STE::estimate_coherence(
    y = municipalities_ste.clean$teffect,
    x = municipalities_ste.clean[, ml_vars],
    x.no_inter = municipalities_ste.clean[, ml_vars.no_inter]
  )
  
  # Save the histograms of the STEs and treatment effects
  title_main <- paste("STE Histogram - ", outcome_name)
  p <- ggplot(municipalities_ste, aes(x=ste)) + 
    geom_histogram(color="black", fill="white") +
    labs(title=title_main,x="Strategic Treatment Effect", y = "Frequency")
  ggsave(filename = file.path(paste0("plots/", strategy_name, "/STE_Histograms_Municipalities"), paste0(outcome_name, ".png")), plot = p, device = "png")
  rm(p)
  
  title_main <- paste("Treatment Effect Histogram - ", outcome_name)
  p <- ggplot(municipalities_ste, aes(x=teffect)) + 
    geom_histogram(color="black", fill="white") +
    labs(title=title_main,x="Treatment Effect", y = "Frequency")
  ggsave(filename = file.path(paste0("plots/", strategy_name, "/TE_Histograms_Municipalities"), paste0(outcome_name, ".png")), plot = p, device = "png")
  
  # Save the dataframe of the top 20 STE determinants and the bottom 20 STE determinants (by coefficient) for each outcome
  path <- paste("tables/", strategy_name, "/STE_Determinants_Municipalities/STE_Determinants_", outcome_name, ".csv", sep = "")
  write.csv(ste_features, path)
  print(paste("Processed outcome: ", outcome_name, sep = ""))
  return(municipalities_coherence)
}
  
# Write a comprehensive function that runs the above analyses for multiple strategies, taking in a list of strategies and a list of strategy names as inputs with the dataframes being "municipalities" and "municipalities_outcomes" instead of "schools" and "schools_outcomes"
# The function can use functions that have been already created.

main <- function(strategies, strategy_names, municipalities, municipalities_outcomes, outcomes) {
  # Create folders for strategy_names if they don't already exist
  for(i in 1:length(strategy_names)) {
    dir.create(paste0("plots/", strategy_names[i]), showWarnings = FALSE)
    dir.create(paste0("tables/", strategy_names[i]), showWarnings = FALSE)
  }

  # Create a function that estimates the propensity score for a given strategy
  create_p_scores <- function(strategy, municipalities, ml_vars) {
    # Estimate the propensity score for each strategy
    p_scores <- estimate_propensity(
      treatment = strategy,
      X = municipalities[, ml_vars]
    )
    return(p_scores)
  }
  
  # Create p-scores for each strategy
  # for(i in 1:length(strategies)) {
  #   ml_vars_strategy <- setdiff(ml_vars, grep(strategies[i], names(municipalities), value = TRUE))
  #   # A strategy is a variable that is 1 if the school is in the top 25% of municipalities in the dataset for that strategy
  #   strategy <- municipalities[[strategies[i]]]
  #   strategy <- ifelse(strategy > quantile(strategy, 0.75), 1, 0)  
  #   p_scores <- create_p_scores(strategy, municipalities, ml_vars_strategy)
  #   save(p_scores, file = paste0("data/p_scores/p_scores_", strategy_names[i], "_municipal.Rdata"))
  # }
  
  for(i in 1:length(strategies)) {
    strategy <- municipalities[[strategies[i]]]
    strategy <- ifelse(strategy > quantile(strategy, 0.75), 1, 0)  
    load(paste0("data/p_scores/p_scores_", strategy_names[i], "_municipal.Rdata"))
    ml_vars_strategy <- setdiff(ml_vars, grep(strategies[i], names(municipalities), value = TRUE))
    coherence_values <- data.frame()
    for(outcome_name in outcomes) {
      outcome <- sapply(municipalities_outcomes[[outcome_name]], as.numeric)
      municipalities_coherence <- outcome_analyser(outcome, strategy, p_scores, municipalities_outcomes, municipalities, ml_vars_strategy, outcome_name, strategy_names[i])
      # Save coherence values with info on the outcome and strategy
      coherence_values <- rbind(coherence_values, data.frame(strategy_names[i], outcome_name, municipalities_coherence))
    }
    # Save coherence values in CSV file
    write.csv(coherence_values, paste0("tables/coherence/Coherence_Values_", strategy_names[i], "_municipal.csv"))
  }
}

############################################################################################################
# Run the main function
############################################################################################################

outcomes <- c("RATE_APROV", "RATE_ABANDON", "RATE_FAILURE",
              "RATE_APROV_PUB", "RATE_ABANDON_PUB", "RATE_FAILURE_PUB",
              "RATE_APROV_DIFF", "RATE_ABANDON_DIFF", "RATE_FAILURE_DIFF",
              "PROVA_MEAN_PORT_I", "PROVA_MEAN_PORT_T", "PROVA_MEAN_MAT_I", "PROVA_MEAN_MAT_T")
strategies <- c("FC_SCIENCE_LAB", "STR_TEACHERS_ADVANCED_FUNDEDU",
                "CLASS_SIZE", "CLASS_STUDENT_TEACHER_RATIO", "CH_SCHOOL_LARGE_SIZE",
                "ST_TOTAL_STAFF", "CH_STATE", "CH_MUNICIPAL", "CH_PRIVATE", "CH_URBAN", 
                "CH_RURAL", "FC_NUMBER_COMPUTERS")
                
strategy_names <- c("science_lab", "TEACHERS_ADVANCED_DEGREES",
                    "CLASS_SIZE", "STUDENT_TEACHER_RATIO", "SCHOOL_LARGE_SIZE",
                    "TOTAL_STAFF", "STATE_SCHOOL_CONC", "MUNICIPAL_SCHOOL_CONC", 
                    "PRIVATE_SCHOOL_CONC", "URBAN_SCHOOL_CONC", 
                    "RURAL_SCHOOL_CONC", "NUMBER_COMPUTERS")
strategy_names <- tolower(strategy_names)

main(strategies, strategy_names, municipalities, municipalities_outcomes, outcomes)
