
############################################
# Estimate the main effects of the strategies using regressions
############################################
# Estiamte the main effects with the doubly robust model
library(sandwich)
library(glmnet)
library(lfe)
library(fixest)

# Join municipalities and municipalities_outcomes on "CO_MUNICIPALITY"
municipalities_reg <- merge(municipalities, municipalities_outcomes, by = "CO_MUNICIPALITY")
strategies <- strategy_names


do_double_lasso <- function(X,treatment, y,formula_stem,formula_end = "") {
    
  print(paste0("Model: ", formula_stem))
  print("Running first lasso")
  l1 <- cv.glmnet(data.matrix(X),y=as.double(treatment))
  print("Running second lasso")
  l2 <- cv.glmnet(data.matrix(X),y=as.double(y))
  print("Done")
  
  coefs_l1 = ml_vars[coef(l1)[-1]!=0]
  coefs_l2 = ml_vars[coef(l2)[-1]!=0]
  double_lasso_coefs <- union(coefs_l1, coefs_l2)
  
  m2_formula  = formula_stem
  for(cd in double_lasso_coefs) {
    if(nchar(cd) > 0 ){
      m2_formula = paste0(m2_formula, "+", cd)
    }
  }

  m2_formula = paste0(m2_formula, "", formula_end)
  m2_formula = as.formula(m2_formula)
  print(m2_formula)
  m4 <- felm(m2_formula, data=municipalities_reg)
  ret_obj = list(formula=m2_formula, 
                 regression = m4)
  return (ret_obj)
}

# Load the municipalities data in data/schools_processed.Rdata
load("data/schools_cleaned.Rdata")
# Keep the co_municipality and the name_state columns in the schools data
schools <- schools[,c("CO_MUNICIPALITY", "NAME_STATE")]
# Convert this to a list of unique municipalities
municipalities_states <- unique(schools)

# Add the state name to the municipalities data
municipalities_reg <- merge(municipalities_reg, municipalities_states, by = "CO_MUNICIPALITY")
# Save municipalities_reg with outcomes
save(municipalities_reg, ml_vars, outcomes, strategy_names, file = "data/municipalities_reg.Rdata")

############################################
# Estimate the main effects of the strategies using regressions
############################################

# Load the municipalities data in data/schools_processed.Rdata
load("data/municipalities_reg.Rdata")

# Run OLS regressions for outcomes (PROVA_MEAN_MAT_STD, PROVA_MEAN_PORT_STD, RATE_APROV, RATE_FAILURE, RATE_ABANDON, RATE_APROV_PUB, RATE_FAILURE_PUB, RATE_ABANDON_PUB) and strategies (physical_capital_focus, human_capital_focus, human_capital_and_physical_capital_focus)
# Conduct a cross-sectional regression with state and municipality fixed effects, and the strategy as the independent variable, and the outcome as the dependent variable
# The strategy is a dummy variable that takes the value 1 if the municipality is in the strategy group and 0 otherwise

# Use a for loop to run the regressions for all outcomes and strategies
# Use the felm function from the lfe package
# Use the CO_MUNICIPALITY variable as the id variable, and NAME_STATE as the state variable
# Save the results in a list of regressions
# Use the summary function to print the results of the regressions

regs <- list()

for (outcome in outcomes) {
  for (strategy in strategy_names) {
    formula <- paste0(outcome, " ~ ", strategy, " | factor(NAME_STATE)")
    formula <- as.formula(formula)
    regression <- felm(formula, data = municipalities_reg)
    regs[[paste0(outcome, "_", strategy)]] <- regression
  }
}

# There are 5 outcome groups
# Group 1 (RATE_APROV, RATE_APROV_PUB)
# Group 2 (RATE_FAILURE, RATE_FAILURE_PUB)
# Group 3 (RATE_ABANDON, RATE_ABANDON_PUB)
# Group 4 (PROVA_MEAN_MAT_STD, PROVA_MEAN_MAT_T_STD, PROVA_MEAN_MAT_I_STD)
# Group 5 (PROVA_MEAN_PORT_STD, PROVA_MEAN_PORT_T_STD, PROVA_MEAN_PORT_I_STD)
# Create two tables, one for each outcome group, where each row is a strategy and each column is an outcome
# Use the summary function to print the results of the regressions
# Export the tables to latex using stargazer

outcomes_groups <- c("RATE_APROV", "RATE_APROV_PUB", "RATE_FAILURE", "RATE_FAILURE_PUB", "RATE_ABANDON", "RATE_ABANDON_PUB", "PROVA_MEAN_MAT_STD", "PROVA_MEAN_MAT_T_STD", "PROVA_MEAN_MAT_I_STD", "PROVA_MEAN_PORT_STD", "PROVA_MEAN_PORT_T_STD", "PROVA_MEAN_PORT_I_STD")

regs_group1 <- list()
regs_group2 <- list()
regs_group3 <- list()
regs_group4 <- list()
regs_group5 <- list()

for (outcome in outcomes_groups) {
  for (strategy in strategy_names) {
    if (outcome %in% c("RATE_APROV", "RATE_APROV_PUB")) {
      regs_group1[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
    } else if (outcome %in% c("RATE_FAILURE", "RATE_FAILURE_PUB")) {
      regs_group2[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
    } else if (outcome %in% c("RATE_ABANDON", "RATE_ABANDON_PUB")) {
      regs_group3[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
    } else if (outcome %in% c("PROVA_MEAN_MAT_STD", "PROVA_MEAN_MAT_T_STD", "PROVA_MEAN_MAT_I_STD")) {
      regs_group4[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
    } else if (outcome %in% c("PROVA_MEAN_PORT_STD", "PROVA_MEAN_PORT_T_STD", "PROVA_MEAN_PORT_I_STD")) {
      regs_group5[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
    }
  }
}

# Add labels to the tables
labels_group1 <- c("Approval Rate", "Approval Rate - Public")
labels_group2 <- c("Failure Rate", "Failure Rate - Public")
labels_group3 <- c("Abandonment Rate", "Abandonment Rate - Public")
labels_group4 <- c("Math Scores", "Math Scores - Terminal", "Math Scores - Initial")
labels_group5 <- c("Portuguese Scores", "Portuguese Scores - Terminal", "Portuguese Scores - Initial")


# Update the strategy names to match the labels
strategy_names_labels <- c("Physical Capital Focus", "Human Capital Focus", "Human and Physical Capital Focus")

# Groups 1 to 5
stargazer(regs_group1, type = "text", title = "Group 1", dep.var.labels = labels_group1, omit.stat = c("f", "ser"), digits = 3, out = "tex/tables/regs_group1.tex", covariate.labels = strategy_names_labels)
stargazer(regs_group2, type = "text", title = "Group 2", dep.var.labels = labels_group2, omit.stat = c("f", "ser"), digits = 3, out = "tex/tables/regs_group2.tex", covariate.labels = strategy_names_labels)
stargazer(regs_group3, type = "text", title = "Group 3", dep.var.labels = labels_group3, omit.stat = c("f", "ser"), digits = 3, out = "tex/tables/regs_group3.tex", covariate.labels = strategy_names_labels)
stargazer(regs_group4, type = "text", title = "Group 4", dep.var.labels = labels_group4, omit.stat = c("f", "ser"), digits = 3, out = "tex/tables/regs_group4.tex", covariate.labels = strategy_names_labels)
stargazer(regs_group5, type = "text", title = "Group 5", dep.var.labels = labels_group5, omit.stat = c("f", "ser"), digits = 3, out = "tex/tables/regs_group5.tex", covariate.labels = strategy_names_labels)

# Outcomes is a list of outcomes to be studied
# Strategies is a list of strategies to be studied
double_lasso_regressions <- list()
for (outcome in outcomes){
    for (strategy in strategy_names){
        # Create the formula
        formula <- paste0(outcome, " ~ ", strategy)
        # Create the model
        model <- do_double_lasso(X=municipalities_reg[,ml_vars], 
                                 treatment=municipalities_reg[[strategy]],
                                 y=municipalities_reg[[outcome]], 
                                 formula_stem=formula,
                                 formula_end="| factor(NAME_STATE)")
        # Add the model to the list
        double_lasso_regressions[[paste(outcome, strategy, sep=".")]] <- model
    }
}

save(double_lasso_regressions, file = "data/double_lasso_regs.Rdata")
load("data/double_lasso_regs.Rdata")
# double_lasso_regressions is a list of lists which contain a formula and a regression object
# Convert it into just a list of regression objects
double_lasso_regressions <- lapply(double_lasso_regressions, function(x) x$regression)

regvars <- c("physical_capital_focus", "human_capital_focus", "human_capital_and_physical_capital_focus")

# There are 5 outcome groups
# Group 1 (RATE_APROV, RATE_APROV_PUB)
# Group 2 (RATE_FAILURE, RATE_FAILURE_PUB)
# Group 3 (RATE_ABANDON, RATE_ABANDON_PUB)
# Group 4 (PROVA_MEAN_MAT_STD, PROVA_MEAN_MAT_T_STD, PROVA_MEAN_MAT_I_STD)
# Group 5 (PROVA_MEAN_PORT_STD, PROVA_MEAN_PORT_T_STD, PROVA_MEAN_PORT_I_STD)
# Create a list called regs_group1 with the regressions for the first group of outcomes
# regs_group1_complete is in the form of a list of regressions alternating between an OLS regression (extracted from regs_group1) and the respective double lasso regression (extracted from double_lasso_regressions)
regs_group1_complete <- list()
for (outcome in c("RATE_APROV", "RATE_APROV_PUB")){
    for (strategy in strategy_names){
        regs_group1_complete[paste0(outcome, "_", strategy)] <- regs[[paste0(outcome, "_", strategy)]]
        regs_group1_complete[paste0(outcome, "_", strategy, "_double_lasso")] <- double_lasso_regressions[[paste0(outcome, ".", strategy)]]
    }
}
# # Update the standard errors
# regs_group1_se <- list()
# for (outcome in c("RATE_APROV", "RATE_APROV_PUB")){
#     for (strategy in strategy_names){
#         regs_group1_se[[paste0(outcome, "_", strategy)]] <- sqrt(diag(as.matrix(vcovHC(regs[[paste0(outcome, "_", strategy)]], type = "HC1"))))
#         regs_group1_se[[paste0(outcome, "_", strategy, "_double_lasso")]] <- sqrt(diag(as.matrix(vcovHC(double_lasso_regressions[[paste0(outcome, ".", strategy)]], type = "HC1"))))
#     }
# }

# Create a list called regs_group2 with the regressions for the second group of outcomes
# regs_group2_complete is in the form of a list of regressions alternating between an OLS regression (extracted from regs_group2) and the respective double lasso regression (extracted from double_lasso_regressions)
regs_group2_complete <- list()
for (outcome in c("RATE_FAILURE", "RATE_FAILURE_PUB")){
    for (strategy in strategy_names){
        regs_group2_complete[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
        regs_group2_complete[[paste0(outcome, "_", strategy, "_double_lasso")]] <- double_lasso_regressions[[paste0(outcome, ".", strategy)]]$regression
    }
}
# # Update the standard errors
# regs_group2_se <- list()
# for (outcome in c("RATE_FAILURE", "RATE_FAILURE_PUB")){
#     for (strategy in strategy_names){
#         regs_group2_se[[paste0(outcome, "_", strategy)]] <- sqrt(diag(as.matrix(vcovHC(regs[[paste0(outcome, "_", strategy)]], type = "HC1"))))
#         regs_group2_se[[paste0(outcome, "_", strategy, "_double_lasso")]] <- sqrt(diag(as.matrix(vcovHC(double_lasso_regressions[[paste0(outcome, ".", strategy)]], type = "HC1"))))
#     }
# }

# Create a list called regs_group3 with the regressions for the third group of outcomes
# regs_group3_complete is in the form of a list of regressions alternating between an OLS regression (extracted from regs_group3) and the respective double lasso regression (extracted from double_lasso_regressions)
regs_group3_complete <- list()
for (outcome in c("RATE_ABANDON", "RATE_ABANDON_PUB")){
    for (strategy in strategy_names){
        regs_group3_complete[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
        regs_group3_complete[[paste0(outcome, "_", strategy, "_double_lasso")]] <- double_lasso_regressions[[paste0(outcome, ".", strategy)]]$regression
    }
}
# # Update the standard errors
# regs_group3_se <- list()
# for (outcome in c("RATE_ABANDON", "RATE_ABANDON_PUB")){
#     for (strategy in strategy_names){
#         regs_group3_se[[paste0(outcome, "_", strategy)]] <- sqrt(diag(as.matrix(vcovHC(regs[[paste0(outcome, "_", strategy)]], type = "HC1"))))
#         regs_group3_se[[paste0(outcome, "_", strategy, "_double_lasso")]] <- sqrt(diag(as.matrix(vcovHC(double_lasso_regressions[[paste0(outcome, ".", strategy)]], type = "HC1"))))
#     }
# }

# Create a list called regs_group4 with the regressions for the fourth group of outcomes
# Group 4 (PROVA_MEAN_MAT_STD, PROVA_MEAN_MAT_T_STD, PROVA_MEAN_MAT_I_STD)
# regs_group4_complete is in the form of a list of regressions alternating between an OLS regression (extracted from regs_group4) and the respective double lasso regression (extracted from double_lasso_regressions)
regs_group4_complete <- list()
for (outcome in c("PROVA_MEAN_MAT_STD", "PROVA_MEAN_MAT_T_STD", "PROVA_MEAN_MAT_I_STD")){
    for (strategy in strategy_names){
        regs_group4_complete[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
        regs_group4_complete[[paste0(outcome, "_", strategy, "_double_lasso")]] <- double_lasso_regressions[[paste0(outcome, ".", strategy)]]$regression
    }
}
# # Update the standard errors
# regs_group4_se <- list()
# for (outcome in c("PROVA_MEAN_MAT_STD", "PROVA_MEAN_MAT_T_STD", "PROVA_MEAN_MAT_I_STD")){
#     for (strategy in strategy_names){
#         regs_group4_se[[paste0(outcome, "_", strategy)]] <- sqrt(diag(as.matrix(vcovHC(regs[[paste0(outcome, "_", strategy)]], type = "HC1"))))
#         regs_group4_se[[paste0(outcome, "_", strategy, "_double_lasso")]] <- sqrt(diag(as.matrix(vcovHC(double_lasso_regressions[[paste0(outcome, ".", strategy)]], type = "HC1"))))
#     }
# }

# Create a list called regs_group5 with the regressions for the fifth group of outcomes
# Group 5 (PROVA_MEAN_PORT_STD, PROVA_MEAN_PORT_T_STD, PROVA_MEAN_PORT_I_STD)
# regs_group5_complete is in the form of a list of regressions alternating between an OLS regression (extracted from regs_group5) and the respective double lasso regression (extracted from double_lasso_regressions)
regs_group5_complete <- list()
for (outcome in c("PROVA_MEAN_PORT_STD", "PROVA_MEAN_PORT_T_STD", "PROVA_MEAN_PORT_I_STD")){
    for (strategy in strategy_names){
        regs_group5_complete[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
        regs_group5_complete[[paste0(outcome, "_", strategy, "_double_lasso")]] <- double_lasso_regressions[[paste0(outcome, ".", strategy)]]$regression
    }
}
# Update the standard errors
# regs_group5_se <- list()
# for (outcome in c("PROVA_MEAN_PORT_STD", "PROVA_MEAN_PORT_T_STD", "PROVA_MEAN_PORT_I_STD")){
#     for (strategy in strategy_names){
#         regs_group5_se[[paste0(outcome, "_", strategy)]] <- sqrt(diag(as.matrix(vcovHC(regs[[paste0(outcome, "_", strategy)]], type = "HC1"))))
#         regs_group5_se[[paste0(outcome, "_", strategy, "_double_lasso")]] <- sqrt(diag(as.matrix(vcovHC(double_lasso_regressions[[paste0(outcome, ".", strategy)]], type = "HC1"))))
#     }
# }

# Add labels to the tables
labels_group1 <- c("Approval Rate", "Approval Rate - Public")
labels_group2 <- c("Failure Rate", "Failure Rate - Public")
labels_group3 <- c("Abandonment Rate", "Abandonment Rate - Public")
labels_group4 <- c("Math Scores", "Math Scores - Terminal", "Math Scores - Initial")
labels_group5 <- c("Portuguese Scores", "Portuguese Scores - Terminal", "Portuguese Scores - Initial")


# Update the strategy names to match the labels
strategy_names_labels <- c("Physical Capital Focus", "Human Capital Focus", "Human and Physical Capital Focus")

# Use stargazer to create the tables
# Group 1
library(lfe)
library(fixest)
library(stargazer)
l <- list()
i <- 0
for (reg in regs_group1_complete){
  i <- i+1
  l[i] <- reg
}

# For each regression in regs_group1_complete, replace all underscores with spaces
for (reg in names(regs_group1_complete)){
    names(regs_group1_complete)[names(regs_group1_complete) == reg] <- gsub("_", " ", reg)
}
library(xtable)


stargazer(regs_group1_complete,
          out="tex/tables/double_lasso_group1.tex",
          type="text")
regvars <- c("physical_capital_focus", "human_capital_focus", "human_capital_and_physical_capital_focus")


# create a reg_vars list with the variables to be included in the table
# It should be a regular expression that matches any variables with the phrase "capital_focus" in the name




stargazer(regs_group1_complete,
          type="text",
          keep = "cap")


stargazer(regs, type="text",
          keep=regvars,
          se = regs_se,
          dep.var.labels = c("Math Scores - Physical Capital","Math Scores - Physical Capital",
                             "Math Scores - Human Capital","Math Scores - Human Capital",
                             "Math Scores - Human and Physical Capital","Math Scores - Human and Physical Capital"),
          model.names=FALSE,
          column.labels=c("OLS","Double LASSO",
                          "OLS","Double LASSO",
                          "OLS","Double LASSO"),  
          covariate.labels = municipalities_reg.labels[regvars],
          keep.stat = c("n","rsq"),
          omit.table.layout = "n")






stargazer(regs,
          out="tex/double_robust_estimator.tex",
          type="latex", 
          keep=regvars,
          se = regs_se,
          dep.var.labels = c("Equity Growth","Raised Series B",
                             "Equity Growth"),
          model.names=FALSE,
          column.labels=c("OLS","Double LASSO",
                          "OLS","Double LASSO",
                          "OLS","Double LASSO"),  
          covariate.labels = cb_startups.labels[regvars],
          keep.stat = c("n","rsq"),
          title="Double LASSO Estimates of the Main Effect of Early Stage VC Financing", 
          omit.table.layout = "n"
)

##################################
# Running the regressions using school-level data, with the school fixed effects, clustering by municipality
##################################

load("data/schools_processed.Rdata")
# Combine schools and schools outcomes into a schools_reg dataframe
schools_reg <- merge(schools, schools_outcomes, by="CO_SCHOOL")
load("data/schools_cleaned.Rdata")
# Keep the co_municipality and the name_state columns in the schools data
schools <- schools[,c("CO_MUNICIPALITY", "CO_SCHOOL", "NAME_STATE")]
# Convert schools to a dataframe which has unique CO_SCHOOL values
schools <- as.data.frame(unique(schools))
# Add the strategy and municipality columns to the schools_reg dataframe from the municipalities_reg dataframe, matching by CO_SCHOOL
schools_reg <- merge(schools_reg, schools[,c("CO_SCHOOL", "CO_MUNICIPALITY")], by="CO_SCHOOL")

# Add strategy columns to the schools_reg dataframe using a m:m merge with the municipalities_reg dataframe
schools_reg <- merge(schools_reg, municipalities_reg[,c("CO_MUNICIPALITY", "physical_capital_focus", "human_capital_focus", "human_capital_and_physical_capital_focus", "NAME_STATE")], by="CO_MUNICIPALITY")

# Create a new dataframe with the standardized Prova Brasil scores
schools_reg <- cbind(schools_reg, 
                                 PROVA_MEAN_MAT_I_STD = (schools_reg$PROVA_MEAN_MAT_I - mean(schools_reg$PROVA_MEAN_MAT_I, na.rm = TRUE)) / sd(schools_reg$PROVA_MEAN_MAT_I, na.rm = TRUE),
                                 PROVA_MEAN_MAT_T_STD = (schools_reg$PROVA_MEAN_MAT_T - mean(schools_reg$PROVA_MEAN_MAT_T, na.rm = TRUE)) / sd(schools_reg$PROVA_MEAN_MAT_T, na.rm = TRUE),
                                 PROVA_MEAN_PORT_I_STD = (schools_reg$PROVA_MEAN_PORT_I - mean(schools_reg$PROVA_MEAN_PORT_I, na.rm = TRUE)) / sd(schools_reg$PROVA_MEAN_PORT_I, na.rm = TRUE),
                                 PROVA_MEAN_PORT_T_STD = (schools_reg$PROVA_MEAN_PORT_T - mean(schools_reg$PROVA_MEAN_PORT_T, na.rm = TRUE)) / sd(schools_reg$PROVA_MEAN_PORT_T, na.rm = TRUE))

# Create a new dataframe with the average of the Prova Brasil scores
schools_reg <- cbind(schools_reg, 
                                 PROVA_MEAN_MAT = (schools_reg$PROVA_MEAN_MAT_I + schools_reg$PROVA_MEAN_MAT_T) / 2,
                                 PROVA_MEAN_PORT = (schools_reg$PROVA_MEAN_PORT_I + schools_reg$PROVA_MEAN_PORT_T) / 2)

# Create a new dataframe with the standardized average of the Prova Brasil scores
schools_reg <- cbind(schools_reg, 
                                 PROVA_MEAN_MAT_STD = (schools_reg$PROVA_MEAN_MAT - mean(schools_reg$PROVA_MEAN_MAT, na.rm = TRUE)) / sd(schools_reg$PROVA_MEAN_MAT, na.rm = TRUE),
                                 PROVA_MEAN_PORT_STD = (schools_reg$PROVA_MEAN_PORT - mean(schools_reg$PROVA_MEAN_PORT, na.rm = TRUE)) / sd(schools_reg$PROVA_MEAN_PORT, na.rm = TRUE))



regs_schools <- list()
schools_reg$factor_mun <- as.factor(schools_reg$CO_MUNICIPALITY)

for (outcome in outcomes) {
  for (strategy in strategy_names) {
    formula <- paste0(outcome, " ~ ", strategy, " | CO_MUNICIPALITY")
    formula <- as.formula(formula)
    regression <- feols(formula, data = schools_reg)
    regs_schools[[paste0(outcome, "_", strategy)]] <- regression
  }
}

# There are 5 outcome groups
# Group 1 (RATE_APROV, RATE_APROV_PUB)
# Group 2 (RATE_FAILURE, RATE_FAILURE_PUB)
# Group 3 (RATE_ABANDON, RATE_ABANDON_PUB)
# Group 4 (PROVA_MEAN_MAT_STD, PROVA_MEAN_MAT_T_STD, PROVA_MEAN_MAT_I_STD)
# Group 5 (PROVA_MEAN_PORT_STD, PROVA_MEAN_PORT_T_STD, PROVA_MEAN_PORT_I_STD)
# Create two tables, one for each outcome group, where each row is a strategy and each column is an outcome
# Use the summary function to print the results of the regressions
# Export the tables to latex using stargazer

outcomes_groups <- c("RATE_APROV", "RATE_APROV_PUB", "RATE_FAILURE", "RATE_FAILURE_PUB", "RATE_ABANDON", "RATE_ABANDON_PUB", "PROVA_MEAN_MAT_STD", "PROVA_MEAN_MAT_T_STD", "PROVA_MEAN_MAT_I_STD", "PROVA_MEAN_PORT_STD", "PROVA_MEAN_PORT_T_STD", "PROVA_MEAN_PORT_I_STD")

regs_group1 <- list()
regs_group2 <- list()
regs_group3 <- list()
regs_group4 <- list()
regs_group5 <- list()

for (outcome in outcomes_groups) {
  for (strategy in strategy_names) {
    if (outcome %in% c("RATE_APROV", "RATE_APROV_PUB")) {
      regs_group1[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
    } else if (outcome %in% c("RATE_FAILURE", "RATE_FAILURE_PUB")) {
      regs_group2[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
    } else if (outcome %in% c("RATE_ABANDON", "RATE_ABANDON_PUB")) {
      regs_group3[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
    } else if (outcome %in% c("PROVA_MEAN_MAT_STD", "PROVA_MEAN_MAT_T_STD", "PROVA_MEAN_MAT_I_STD")) {
      regs_group4[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
    } else if (outcome %in% c("PROVA_MEAN_PORT_STD", "PROVA_MEAN_PORT_T_STD", "PROVA_MEAN_PORT_I_STD")) {
      regs_group5[[paste0(outcome, "_", strategy)]] <- regs[[paste0(outcome, "_", strategy)]]
    }
  }
}

# Add labels to the tables
labels_group1 <- c("Approval Rate", "Approval Rate - Public")
labels_group2 <- c("Failure Rate", "Failure Rate - Public")
labels_group3 <- c("Abandonment Rate", "Abandonment Rate - Public")
labels_group4 <- c("Math Scores", "Math Scores - Terminal", "Math Scores - Initial")
labels_group5 <- c("Portuguese Scores", "Portuguese Scores - Terminal", "Portuguese Scores - Initial")


# Update the strategy names to match the labels
strategy_names_labels <- c("Physical Capital Focus", "Human Capital Focus", "Human and Physical Capital Focus")

# Groups 1 to 5
stargazer(regs_group1, type = "text", title = "Group 1", dep.var.labels = labels_group1, omit.stat = c("f", "ser"), digits = 3, out = "tex/tables/regs_group1.tex", covariate.labels = strategy_names_labels)
stargazer(regs_group2, type = "text", title = "Group 2", dep.var.labels = labels_group2, omit.stat = c("f", "ser"), digits = 3, out = "tex/tables/regs_group2.tex", covariate.labels = strategy_names_labels)
stargazer(regs_group3, type = "text", title = "Group 3", dep.var.labels = labels_group3, omit.stat = c("f", "ser"), digits = 3, out = "tex/tables/regs_group3.tex", covariate.labels = strategy_names_labels)
stargazer(regs_group4, type = "text", title = "Group 4", dep.var.labels = labels_group4, omit.stat = c("f", "ser"), digits = 3, out = "tex/tables/regs_group4.tex", covariate.labels = strategy_names_labels)
stargazer(regs_group5, type = "text", title = "Group 5", dep.var.labels = labels_group5, omit.stat = c("f", "ser"), digits = 3, out = "tex/tables/regs_group5.tex", covariate.labels = strategy_names_labels)