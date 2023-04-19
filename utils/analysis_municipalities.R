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
# setwd("U:/Research/Thesis/Schools Analysis/thesis-brazil-schools-analysis")

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

main <- function(strategy_names, municipalities, municipalities_outcomes, outcomes) {
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
  for(i in 1:length(strategy_names)) {
    # Subset using strategy_names
    strategy <- municipalities[[strategy_names[i]]]
    p_scores <- create_p_scores(strategy, municipalities, ml_vars)
    save(p_scores, file = paste0("data/p_scores/p_scores_", strategy_names[i], "_municipal.Rdata"))
  }
  
  for(i in 1:length(strategy_names)) {
    strategy <- municipalities[[strategy_names[i]]]
    load(paste0("data/p_scores/p_scores_", strategy_names[i], "_municipal.Rdata"))
    ml_vars_strategy <- ml_vars
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
                
strategy_names <- c("human_capital_focus", "physical_capital_focus", "human_capital_and_physical_capital_focus")

# Strategy 1: Physical Capital Focus
# This is defined as the municipality having a high proportion of schools with a high proportion of physical capital
# The following criteria are used to define a high proportion of physical capital (facilities that should be provided regardless of school size):
# 1. Atleast 75% of the schools in the municipality have access to computers, internet, computer labs, and libraries, sewage, electricity, and water facilities
# Facilities omitted for now: TV, sports quad, # of computers, science_lab
# These facility variables are prefixed with "FC_" in the dataset
# The variable physical_capital_focus is 1 if the municipality meets the above criteria and 0 otherwise
variables <- c("FC_COMPUTER", "FC_INTERNET", "FC_COMPUTER_LAB", "FC_LIBRARY", "FC_SEWAGE", "FC_ELECTR", "FC_WATER")
municipalities$physical_capital_focus <- 1
for (variable in variables) {
  # If statement that changes the value of physical_capital_focus to 0 if the municipality does not meet the criteria
  municipalities$physical_capital_focus <- ifelse(municipalities[[variable]] < 0.75, 0, municipalities$physical_capital_focus)
}

# Strategy 2: Human Capital Focus
# This is defined as the municipality having a high proportion of schools with a high proportion of human capital
# The following criteria are used to define a high proportion of human capital:
# 1. The municipality is in the bottom 25% of municipalities in the dataset for the following variables:
#    - STR_TEACHERS_INCOMPLETE_FUNDEDU, STR_TEACHERS_ONLY_FUNDEDU
# 2. The municipality is in the top 25% of municipalities in the dataset for the following variables:
#   - STR_TEACHERS_SECONDARY_FUNDEDU
# 3. The municipality is in the top 50% of municipalities in the dataset for the following variables:
#   - STR_TEACHERS_ADVANCED_FUNDEDU, ST_TEACHER_EDUCATION_YEARS
# 4. The municipality is in the bottom 90% of municipalities in the dataset for the following variables:
#   - CLASS_SIZE, CLASS_STUDENT_TEACHER_RATIO

# The variable human_capital_focus is 1 if the municipality meets the above criteria and 0 otherwise
variables_bottom_10 <- c("STR_TEACHERS_INCOMPLETE_FUNDEDU", "STR_TEACHERS_ONLY_FUNDEDU")
variables_top_50 <- c("STR_TEACHERS_SECONDARY_FUNDEDU")
variables_top_25 <- c("STR_TEACHERS_ADVANCED_FUNDEDU", "ST_TEACHER_EDUCATION_YEARS")
variables_bottom_75 <- c("CLASS_SIZE", "CLASS_STUDENT_TEACHER_RATIO")
municipalities$human_capital_focus <- 1
for (variable in variables_bottom_10) {
  # If statement that changes the value of human_capital_focus to 0 if the municipality does not meet the criteria
  municipalities$human_capital_focus <- ifelse(municipalities[[variable]] > quantile(municipalities[[variable]], 0.25), 0, municipalities$human_capital_focus)
}
for (variable in variables_top_50) {
  # If statement that changes the value of human_capital_focus to 0 if the municipality does not meet the criteria
  municipalities$human_capital_focus <- ifelse(municipalities[[variable]] < quantile(municipalities[[variable]], 0.25), 0, municipalities$human_capital_focus)
}
for (variable in variables_top_25) {
  # If statement that changes the value of human_capital_focus to 0 if the municipality does not meet the criteria
  municipalities$human_capital_focus <- ifelse(municipalities[[variable]] < quantile(municipalities[[variable]], 0.5), 0, municipalities$human_capital_focus)
}
for (variable in variables_bottom_75) {
  # If statement that changes the value of human_capital_focus to 0 if the municipality does not meet the criteria
  municipalities$human_capital_focus <- ifelse(municipalities[[variable]] > quantile(municipalities[[variable]], 0.9), 0, municipalities$human_capital_focus)
}

# Strategy 3: Human Capital and Physical Capital Focus
# Created as an interaction between the two strategies above
municipalities$human_capital_and_physical_capital_focus <- municipalities$human_capital_focus * municipalities$physical_capital_focus

main(strategy_names, municipalities, municipalities_outcomes, outcomes)


############################################################################################################
# Study multiple effects at once
############################################################################################################

# I now want to study the effect of multiple strategies at once, and see if they are complementary or not
# Moreover, I want plots that show the effect of all strategies on each outcome 
# I will use the same strategies as before (physical capital focus, human capital focus, and human capital and physical capital focus)
# I will use the same outcomes as before (all the outcomes that are related to the rate of approval, rate of abandonment, rate of failure, and the mean scores of the students)

# I will use the same municipalities as before (all the municipalities in the dataset)
# I will use the same municipalities_outcomes as before (all the municipalities in the dataset)

# I will use the same main function as before, but I will modify it to study multiple effects at once

# Function to return the treatment effect and ste for a given outcome and strategy
get_teffect_ste <- function(outcome, strategy, p_scores, df, df_outcomes) {
  # Get the strategy and outcome columns from the dataframe
  strategy <- df[[strategy]]
  outcome <- sapply(df_outcomes[[outcome]], as.numeric)

  municipalities_ste <- estimate_ste(
    y = as.numeric(outcome),
    treatment = strategy,
    propensity = p_scores,
    df = df
  )

  teffect <- municipalities_ste$teffect
  ste <- municipalities_ste$ste

  return(list(teffect, ste))
}

# Function to apply the get_teffect_ste function to all outcomes and strategies, creating a dataframe that has columns <strategy_name>.teffect and <strategy_name>.ste with teffect and ste for each strategy and outcome
get_teffect_ste_df <- function(outcomes, strategy, p_scores, df, df_outcomes) {
  # Create an empty dataframe with the number of rows equal to number or rows in df
  teffect_ste_df <- data.frame(matrix(ncol = 0, nrow = nrow(df)))
  for (outcome in outcomes) {
    teffect_ste <- get_teffect_ste(outcome, strategy, p_scores, df, df_outcomes)
    teffect <- teffect_ste[[1]]
    ste <- teffect_ste[[2]]
    temp_df <- data.frame(teffect, ste)
    # Rename the columns of the dataframe to <strategy_name>.<outcome>.teffect and <strategy_name>.<outcome>.ste
    colnames(temp_df) <- c(paste(strategy, outcome, "teffect", sep = "."), paste(strategy, outcome, "ste", sep = "."))
    # Create a dataframe with the teffect and ste so that the output can be appended as columns
    teffect_ste_df <- cbind(teffect_ste_df, temp_df)
    print(teffect_ste_df)
  }
  return(teffect_ste_df)
}


# Function to plot the effect of all strategies on each outcome
create_and_save_plots <- function(teffect_ste_df, strategy_names, outcomes){
  for (outcome in outcomes) {
    # Subset teffect_ste_df to only include the columns that have <outcome> in the name
    teffect_ste_df_outcome <- teffect_ste_df[, grep(outcome, names(teffect_ste_df))]
    
    # Subset teffect_ste_df to only include the columns that have teffect in the name
    teffects_outcomes <- teffect_ste_df_outcome[, grep("teffect", names(teffect_ste_df_outcome))]
    ste_outcomes <- teffect_ste_df_outcome[, grep("ste", names(teffect_ste_df_outcome))]
    
    # Convert the teffects_outcomes to a dataframe which has columns <strategy_name>
    teffects_outcomes <- as.data.frame(teffects_outcomes) 
    colnames(teffects_outcomes) <- strategy_names
    
    # Convert the ste_outcomes to a dataframe which has columns <strategy_name>
    ste_outcomes <- as.data.frame(ste_outcomes)
    colnames(ste_outcomes) <- strategy_names
    x_lim <- FALSE
    if (outcome == "PROVA_MEAN_MAT_STD"){
      outcome_title <- "Math Scores"
    }
    else if (outcome == "PROVA_MEAN_MAT_STD"){
      outcome_title <- "Portuguese Scores"
    }
    else if (outcome == "RATE_APROV_PUB"){
      outcome_title <- "Pass Rates"
    }
    else if (outcome == "RATE_FAILURE_PUB"){
      outcome_title <- "Failure Rates"
      x_lim <- TRUE
    }
    else if (outcome == "RATE_ABANDON_PUB"){
      outcome_title <- "Drop-Out Rates"
      x_lim <- TRUE
    }
    # Save the plots in a pdf file, with one PDF file per outcome
    # pdf(paste0("plots/PDF/", outcome, ".pdf"), width = 11, height = 8.5)
    # par(mfrow= c(2,2))
    # Create three histograms of teffects representing each strategy for the outcome
    # Overlay the histograms on each other
    # Omit the na values
    # Add a mean line to each histogram
    # Make the fill labels for each histogram the strategy name
    # Make the vline colored according to the strategy
    # plot_hist_teffect <- ggplot() +
    #   geom_histogram(data = teffects_outcomes, aes(x = physical_capital_focus, fill = "Physical Capital Focus"), alpha = 0.5, na.rm = TRUE) +
    #   geom_histogram(data = teffects_outcomes, aes(x = human_capital_focus, fill = "Human Capital Focus"), alpha = 0.5, na.rm = TRUE) +
    #   geom_histogram(data = teffects_outcomes, aes(x = human_capital_and_physical_capital_focus, fill = "Human Capital and Physical Capital Focus"), alpha = 0.5, na.rm = TRUE) +
    #   labs(title = paste("Treatment Effect of strategies on", outcome), x = "Treatment Effect", y = "Frequency") +
    #   theme(plot.title = element_text(hjust = 0.5)) + 
    #   geom_vline(data = teffects_outcomes, aes(xintercept = mean(physical_capital_focus, na.rm = TRUE)), color = "blue", linetype = "dashed", size = 1.1) +
    #   geom_vline(data = teffects_outcomes, aes(xintercept = mean(human_capital_focus, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1.1) +
    #   geom_vline(data = teffects_outcomes, aes(xintercept = mean(human_capital_and_physical_capital_focus, na.rm = TRUE)), color = "green", linetype = "dashed", size = 1.1) +
    #   scale_fill_manual(values = c("Physical Capital Focus" = "blue", "Human Capital Focus" = "red", "Human Capital and Physical Capital Focus" = "green")) 
    
    if (x_lim){
      plot_density_teffect <- ggplot() +
        geom_density(data = teffects_outcomes, aes(x = physical_capital_focus, fill = "Physical Capital Focus"), alpha = 0.5, position = "identity", na.rm = TRUE) +
        geom_density(data = teffects_outcomes, aes(x = human_capital_focus, fill = "Human Capital Focus"), alpha = 0.5, position = "identity", na.rm = TRUE) +
        geom_density(data = teffects_outcomes, aes(x = human_capital_and_physical_capital_focus, fill = "Human Capital and Physical Capital Focus"), alpha = 0.5, position = "identity", na.rm = TRUE) +
        labs(title = paste("Effect of Strategies on", outcome_title), x = "Effect", y = "Probability Density") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        geom_vline(data = teffects_outcomes, aes(xintercept = mean(physical_capital_focus, na.rm = TRUE)), color = "blue", linetype = "dashed", size = 1.1) +
        geom_vline(data = teffects_outcomes, aes(xintercept = mean(human_capital_focus, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1.1) +
        geom_vline(data = teffects_outcomes, aes(xintercept = mean(human_capital_and_physical_capital_focus, na.rm = TRUE)), color = "green", linetype = "dashed", size = 1.1) +
        scale_fill_manual(values = c("Physical Capital Focus" = "blue", "Human Capital Focus" = "red", "Human Capital and Physical Capital Focus" = "green")) + 
        theme(legend.position = "bottom") + xlim(-2,2) + theme(text = element_text(size = 28)) + theme(legend.title=element_blank())
    }
    else{
    plot_density_teffect <- ggplot() +
      geom_density(data = teffects_outcomes, aes(x = physical_capital_focus, fill = "Physical Capital Focus"), alpha = 0.5, position = "identity", na.rm = TRUE) +
      geom_density(data = teffects_outcomes, aes(x = human_capital_focus, fill = "Human Capital Focus"), alpha = 0.5, position = "identity", na.rm = TRUE) +
      geom_density(data = teffects_outcomes, aes(x = human_capital_and_physical_capital_focus, fill = "Human Capital and Physical Capital Focus"), alpha = 0.5, position = "identity", na.rm = TRUE) +
      labs(title = paste("Effect of Strategies on", outcome_title), x = "Effect", y = "Probability Density") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      geom_vline(data = teffects_outcomes, aes(xintercept = mean(physical_capital_focus, na.rm = TRUE)), color = "blue", linetype = "dashed", size = 1.1) +
      geom_vline(data = teffects_outcomes, aes(xintercept = mean(human_capital_focus, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1.1) +
      geom_vline(data = teffects_outcomes, aes(xintercept = mean(human_capital_and_physical_capital_focus, na.rm = TRUE)), color = "green", linetype = "dashed", size = 1.1) +
      scale_fill_manual(values = c("Physical Capital Focus" = "blue", "Human Capital Focus" = "red", "Human Capital and Physical Capital Focus" = "green")) + 
      theme(legend.position = "bottom") + theme(text = element_text(size = 28)) + theme(legend.title=element_blank())
    }
    # plot_hist_ste <- ggplot() +
    #   geom_histogram(data = ste_outcomes, aes(x = physical_capital_focus, fill = "Physical Capital Focus"), alpha = 0.5, na.rm = TRUE) +
    #   geom_histogram(data = ste_outcomes, aes(x = human_capital_focus, fill = "Human Capital Focus"), alpha = 0.5, na.rm = TRUE) +
    #   geom_histogram(data = ste_outcomes, aes(x = human_capital_and_physical_capital_focus, fill = "Human Capital and Physical Capital Focus"), alpha = 0.5, na.rm = TRUE) +
    #   labs(title = paste("Strategic Treatment Effect of strategies on", outcome), x = "Strategic Treatment Effect", y = "Frequency") +
    #   theme(plot.title = element_text(hjust = 0.5)) + 
    #   geom_vline(data = ste_outcomes, aes(xintercept = mean(physical_capital_focus, na.rm = TRUE)), color = "blue", linetype = "dashed", size = 1.1) +
    #   geom_vline(data = ste_outcomes, aes(xintercept = mean(human_capital_focus, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1.1) +
    #   geom_vline(data = ste_outcomes, aes(xintercept = mean(human_capital_and_physical_capital_focus, na.rm = TRUE)), color = "green", linetype = "dashed", size = 1.1) +
    #   scale_fill_manual(values = c("Physical Capital Focus" = "blue", "Human Capital Focus" = "red", "Human Capital and Physical Capital Focus" = "green")) 
    # 
    # plot_density_ste <- ggplot() +
    #   geom_density(data = ste_outcomes, aes(x = physical_capital_focus, fill = "Physical Capital Focus"), alpha = 0.5, position = "identity", na.rm = TRUE) +
    #   geom_density(data = ste_outcomes, aes(x = human_capital_focus, fill = "Human Capital Focus"), alpha = 0.5, position = "identity", na.rm = TRUE) +
    #   geom_density(data = ste_outcomes, aes(x = human_capital_and_physical_capital_focus, fill = "Human Capital and Physical Capital Focus"), alpha = 0.5, position = "identity", na.rm = TRUE) +
    #   labs(title = paste("Strategic Treatment Effect of strategies on", outcome), x = "Strategic Treatment Effect", y = "Frequency") +
    #   theme(plot.title = element_text(hjust = 0.5)) + 
    #   geom_vline(data = ste_outcomes, aes(xintercept = mean(physical_capital_focus, na.rm = TRUE)), color = "blue", linetype = "dashed", size = 1.1) +
    #   geom_vline(data = ste_outcomes, aes(xintercept = mean(human_capital_focus, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1.1) +
    #   geom_vline(data = ste_outcomes, aes(xintercept = mean(human_capital_and_physical_capital_focus, na.rm = TRUE)), color = "green", linetype = "dashed", size = 1.1) +
    #   scale_fill_manual(values = c("Physical Capital Focus" = "blue", "Human Capital Focus" = "red", "Human Capital and Physical Capital Focus" = "green")) 
    
    # Use ggarrange to arrange the plots in a grid
    # ggarrange(plot_hist_teffect, plot_hist_ste, plot_density_teffect, plot_density_ste, ncol = 2, nrow = 2)
    
    # Save the plot as a pdf file
    ggsave(paste("plots/PDF/Final/", outcome, "_teffect_ste.png", sep = ""), width = 20, height = 12.5)
  }
}

# Function to create one datafram with all the treatment effects and ste for all strategies and outcomes
create_teffect_ste_df <- function(strategy_names, municipalities, municipalities_outcomes, outcomes) {
  
  # Get the treatment effect and ste for each outcome and strategy

  # Create an empty dataframe with the number of rows equal to number or rows in municipalities
  teffect_ste_df <- data.frame(matrix(ncol = 0, nrow = nrow(municipalities)))
  for (strategy in strategy_names) {
    # Get the propensity scores from the saved Rdata file for the strategy
    load(paste("data/p_scores/p_scores_", strategy, "_municipal.Rdata", sep = ""))
    p_scores <- p_scores
    teffect_ste_df <- cbind(teffect_ste_df, get_teffect_ste_df(outcomes, strategy, p_scores, municipalities, municipalities_outcomes))
    print("Created DF")
  }
  return(teffect_ste_df)
}

##############################
# Modify municipalities_outcomes to create 8 new outcomes
##############################

# Outcomes 1 to 4: Standardized Prova Brasil scores (any column with "PROVA" in it)
# Outcome 5: Average of "PROVA_MEAN_MAT_I" and "PROVA_MEAN_MAT_T"
# Outcome 6: Average of "PROVA_MEAN_PORT_I" and "PROVA_MEAN_PORT_T"
# Outcomes 7 and 8: Standardized versions of outcomes 5 and 6
# Append these outcomes to municipalities_outcomes
# All of the variables necessary to create these are available in municipalities_outcomes

# Create a new dataframe with the standardized Prova Brasil scores
municipalities_outcomes <- cbind(municipalities_outcomes, 
                                 PROVA_MEAN_MAT_I_STD = (municipalities_outcomes$PROVA_MEAN_MAT_I - mean(municipalities_outcomes$PROVA_MEAN_MAT_I, na.rm = TRUE)) / sd(municipalities_outcomes$PROVA_MEAN_MAT_I, na.rm = TRUE),
                                 PROVA_MEAN_MAT_T_STD = (municipalities_outcomes$PROVA_MEAN_MAT_T - mean(municipalities_outcomes$PROVA_MEAN_MAT_T, na.rm = TRUE)) / sd(municipalities_outcomes$PROVA_MEAN_MAT_T, na.rm = TRUE),
                                 PROVA_MEAN_PORT_I_STD = (municipalities_outcomes$PROVA_MEAN_PORT_I - mean(municipalities_outcomes$PROVA_MEAN_PORT_I, na.rm = TRUE)) / sd(municipalities_outcomes$PROVA_MEAN_PORT_I, na.rm = TRUE),
                                 PROVA_MEAN_PORT_T_STD = (municipalities_outcomes$PROVA_MEAN_PORT_T - mean(municipalities_outcomes$PROVA_MEAN_PORT_T, na.rm = TRUE)) / sd(municipalities_outcomes$PROVA_MEAN_PORT_T, na.rm = TRUE))

# Create a new dataframe with the average of the Prova Brasil scores
municipalities_outcomes <- cbind(municipalities_outcomes, 
                                 PROVA_MEAN_MAT = (municipalities_outcomes$PROVA_MEAN_MAT_I + municipalities_outcomes$PROVA_MEAN_MAT_T) / 2,
                                 PROVA_MEAN_PORT = (municipalities_outcomes$PROVA_MEAN_PORT_I + municipalities_outcomes$PROVA_MEAN_PORT_T) / 2)

# Create a new dataframe with the standardized average of the Prova Brasil scores
municipalities_outcomes <- cbind(municipalities_outcomes, 
                                 PROVA_MEAN_MAT_STD = (municipalities_outcomes$PROVA_MEAN_MAT - mean(municipalities_outcomes$PROVA_MEAN_MAT, na.rm = TRUE)) / sd(municipalities_outcomes$PROVA_MEAN_MAT, na.rm = TRUE),
                                 PROVA_MEAN_PORT_STD = (municipalities_outcomes$PROVA_MEAN_PORT - mean(municipalities_outcomes$PROVA_MEAN_PORT, na.rm = TRUE)) / sd(municipalities_outcomes$PROVA_MEAN_PORT, na.rm = TRUE))

# Add new columns to the outcomes list
outcomes <- c(outcomes, "PROVA_MEAN_MAT_I_STD", "PROVA_MEAN_MAT_T_STD", "PROVA_MEAN_PORT_I_STD", "PROVA_MEAN_PORT_T_STD", "PROVA_MEAN_MAT", "PROVA_MEAN_PORT", "PROVA_MEAN_MAT_STD", "PROVA_MEAN_PORT_STD")

outcomes <- c("PROVA_MEAN_MAT_STD", "PROVA_MEAN_PORT_STD", "RATE_APROV_PUB", "RATE_FAILURE_PUB", "RATE_ABANDON_PUB")


# Create a dataframe with the treatment effects and ste for all strategies and outcomes
teffect_ste_df <- create_teffect_ste_df(strategy_names, municipalities, municipalities_outcomes, outcomes)


# Create plot pdfs
create_and_save_plots(teffect_ste_df, strategy_names, outcomes)
