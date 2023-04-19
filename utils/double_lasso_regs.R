############################################
# Estimate the main effects of the strategies using regressions
############################################
# Estimate the main effects with double lasso techniques
library(sandwich)
library(glmnet)
library(lfe)
library(fixest)
library(stargazer)

# Load the data
setwd("G:/Other computers/My PC/Desktop/Cornell/Research/Thesis/Schools Analysis/thesis-brazil-schools-analysis")

# Load the municipalities data in data/schools_processed.Rdata
load("data/municipalities_reg.Rdata")
# Replace all underscores in column names with periods
colnames(municipalities_reg) <- gsub("_", ".", colnames(municipalities_reg))

# Do the same for ml_vars, outcomes, and strategy_names which are all lists of strings
ml_vars <- lapply(ml_vars, function(x) gsub("_", ".", x))
outcomes <- lapply(outcomes, function(x) gsub("_", ".", x))
strategy_names <- lapply(strategy_names, function(x) gsub("_", ".", x))

# Convert ml_vars, outcomes, and strategy_names to character vectors
ml_vars <- unlist(ml_vars)
outcomes <- unlist(outcomes)
strategy_names <- unlist(strategy_names)

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
  m4 <- felm(m2_formula, data=municipalities_reg)
  return(m4)
}

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
                                 formula_stem=formula)
        # Add the model to the list
        double_lasso_regressions[[paste(outcome, strategy, sep="...")]] <- model
    }
}

save(double_lasso_regressions, file = "data/double_lasso_regs.Rdata")
load("data/double_lasso_regs.Rdata")

############################################
# Using municipalities reg, create a table with the results of the double lasso regressions compared to the OLS regressions
############################################

# Run OLS regressions
ols_regs <- list()
for (outcome in outcomes){
    for (strategy in strategy_names){
        # Create the formula
        formula <- paste0(outcome, " ~ ", strategy)
        # Create the model
        model <- felm(formula, data=municipalities_reg)
        # Add the model to the list
        ols_regs[[paste(outcome, strategy, sep="...")]] <- model
    }
}


# There are 4 outcome groups
# Group 1 (RATE.APROV, RATE.FAILURE, RATE.ABANDON)
# Group 2 (RATE.APROV.PUB, RATE.FAILURE.PUB, RATE.ABANDON.PUB)
# Group 3 (PROVA.MEAN.MAT.STD, PROVA.MEAN.PORT.STD)
# Group 4 (PROVA.MEAN.MAT.T.STD, PROVA.MEAN.MAT.I.STD, PROVA.MEAN.PORT.T.STD, PROVA.MEAN.PORT.I.STD)
# Create four tables, one for each outcome group, where each row is a strategy and each column is an outcome
# Use the summary function to print the results of the regressions
# Export the tables to latex using stargazer

outcomes_groups <- c("RATE.APROV", "RATE.FAILURE", "RATE.ABANDON", 
                     "RATE.APROV.PUB", "RATE.FAILURE.PUB", "RATE.ABANDON.PUB", 
                     "PROVA.MEAN.MAT.STD", "PROVA.MEAN.PORT.STD", 
                     "PROVA.MEAN.MAT.T.STD", "PROVA.MEAN.MAT.I.STD", "PROVA.MEAN.PORT.T.STD", "PROVA.MEAN.PORT.I.STD")

regs_group1 <- list()
regs_group2 <- list()
regs_group3 <- list()
regs_group4 <- list()

for (outcome in outcomes_groups) {
  for (strategy in strategy_names) {
    if (outcome %in% c("RATE.APROV", "RATE.FAILURE", "RATE.ABANDON")) {
      regs_group1[[paste0(outcome, "_ols_", strategy)]] <- ols_regs[[paste0(outcome, "...", strategy)]]
      regs_group1[[paste0(outcome, "_dl_", strategy)]] <- double_lasso_regressions[[paste0(outcome, "...", strategy)]]
    } else if (outcome %in% c("RATE.APROV.PUB", "RATE.FAILURE.PUB", "RATE.ABANDON.PUB")) {
      regs_group2[[paste0(outcome, "_ols_", strategy)]] <- ols_regs[[paste0(outcome, "...", strategy)]]
      regs_group2[[paste0(outcome, "_dl_", strategy)]] <- double_lasso_regressions[[paste0(outcome, "...", strategy)]]
      } else if (outcome %in% c("PROVA.MEAN.MAT.STD", "PROVA.MEAN.PORT.STD")) {
      regs_group3[[paste0(outcome, "_ols_", strategy)]] <- ols_regs[[paste0(outcome, "...", strategy)]]
      regs_group3[[paste0(outcome, "_dl_", strategy)]] <- double_lasso_regressions[[paste0(outcome, "...", strategy)]]
      } else if (outcome %in% c("PROVA.MEAN.MAT.T.STD", "PROVA.MEAN.MAT.I.STD", "PROVA.MEAN.PORT.T.STD", "PROVA.MEAN.PORT.I.STD")) {
      regs_group4[[paste0(outcome, "_ols_", strategy)]] <- ols_regs[[paste0(outcome, "...", strategy)]]
      regs_group4[[paste0(outcome, "_dl_", strategy)]] <- double_lasso_regressions[[paste0(outcome, "...", strategy)]]
      }
    }
}



# Add labels to the tables
labels_group1 <- c("Approval Rate", "Failure Rate", "Abandonment Rate")
labels_group2 <- c("Approval Rate - Public", "Failure Rate - Public", "Abandonment Rate - Public")
labels_group3 <- c("Mathematics Score", "Portuguese Score")
labels_group4 <- c("Mathematics Score - Terminal", "Mathematics Score - Initial", "Portuguese Score - Terminal", "Portuguese Score - Initial")


# Update the strategy names to match the labels
strategy_names_labels <- c("Physical Capital Focus", "Human Capital Focus", "Human and Physical Capital Focus")

# Create the tables
stargazer(regs_group1, type = "text", title = "Enrollment Outcomes", 
                        dep.var.labels = labels_group1, 
                        covariate.labels = strategy_names_labels, 
                        omit.stat = c("f", "ser"), 
                        digits = 3, 
                        column.labels = rep(c("OLS", "Double Lasso"), 9), 
                        model.names = FALSE,
                        out = "data/Enrollment_OLS_DL.tex",
                        keep = c("human.capital.focus", "physical.capital.focus", "human.and.physical.capital.focus"))

stargazer(regs_group2, type = "text", title = "Enrollment Outcomes - Public",
                        dep.var.labels = labels_group2, 
                        covariate.labels = strategy_names_labels, 
                        omit.stat = c("f", "ser"), 
                        digits = 3, 
                        column.labels = c("OLS", "Double Lasso"), 
                        single.row = TRUE, 
                        no.space = TRUE, 
                        align = TRUE, 
                        header = FALSE, 
                        omit.coef = ".*Intercept.*",
                        out = "data/Enrollment_OLS_DL_Pub.tex")

stargazer(regs_group3, type = "text", title = "Learning Outcomes",
                        dep.var.labels = labels_group3, 
                        covariate.labels = strategy_names_labels, 
                        omit.stat = c("f", "ser"), 
                        digits = 3, 
                        column.labels = c("OLS", "Double Lasso"), 
                        single.row = TRUE, 
                        no.space = TRUE, 
                        align = TRUE, 
                        header = FALSE, 
                        omit.coef = ".*Intercept.*",
                        out = "data/Learning_OLS_DL.tex")
                    
stargazer(regs_group4, type = "text", title = "Learning Outcomes - Terminal and Initial",
                        dep.var.labels = labels_group4, 
                        covariate.labels = strategy_names_labels, 
                        omit.stat = c("f", "ser"), 
                        digits = 3, 
                        column.labels = c("OLS", "Double Lasso"), 
                        single.row = TRUE, 
                        no.space = TRUE, 
                        align = TRUE, 
                        header = FALSE, 
                        omit.coef = ".*Intercept.*",
                        out = "data/Learning_OLS_DL_Terminal_Initial.tex")

# Treat each variable as an outcome group and do the same as above
# Create a list of the variables
outcomes_groups <- c("RATE.APROV", "RATE.FAILURE", "RATE.ABANDON", 
                     "RATE.APROV.PUB", "RATE.FAILURE.PUB", "RATE.ABANDON.PUB", 
                     "PROVA.MEAN.MAT.STD", "PROVA.MEAN.PORT.STD", 
                     "PROVA.MEAN.MAT.T.STD", "PROVA.MEAN.MAT.I.STD", "PROVA.MEAN.PORT.T.STD", "PROVA.MEAN.PORT.I.STD")

# Create 10 reg lists
regs_group1 <- list()
regs_group2 <- list()
regs_group3 <- list()
regs_group4 <- list()
regs_group5 <- list()
regs_group6 <- list()
regs_group7 <- list()
regs_group8 <- list()
regs_group9 <- list()
regs_group10 <- list()

# Create a list that holds all regs_group lists
regs_groups <- list(regs_group1, regs_group2, regs_group3, regs_group4, regs_group5, regs_group6, regs_group7, regs_group8, regs_group9, regs_group10)

single_outcome_groups <- c("RATE.APROV", "RATE.FAILURE", "RATE.ABANDON", 
                           "RATE.APROV.PUB", "RATE.FAILURE.PUB", "RATE.ABANDON.PUB", 
                           "PROVA.MEAN.MAT.STD", "PROVA.MEAN.PORT.STD")

i <- 0
for (outcome in single_outcome_groups) {
    i <- i + 1
    for (strategy in strategy_names) {
        # Add the OLS and DL regressions to the list of regs_groups[i]
        regs_groups[[i]][[paste0(outcome, "_ols_", strategy)]] <- ols_regs[[paste0(outcome, "...", strategy)]]
        regs_groups[[i]][[paste0(outcome, "_dl_", strategy)]] <- double_lasso_regressions[[paste0(outcome, "...", strategy)]]
    }
}

# Add labels to the tables
labels_group1 <- c("Approval Rate")
labels_group2 <- c("Failure Rate")
labels_group3 <- c("Abandonment Rate")
labels_group4 <- c("Approval Rate - Public")
labels_group5 <- c("Failure Rate - Public")
labels_group6 <- c("Abandonment Rate - Public")
labels_group7 <- c("Mathematics Score")
labels_group8 <- c("Portuguese Score")

labels_group_list <- list(labels_group1, labels_group2, labels_group3, labels_group4, labels_group5, labels_group6, labels_group7, labels_group8)

# Update the strategy names to match the labels
strategy_names_labels <- c("Physical Capital Focus", "Human Capital Focus", "Human and Physical Capital Focus")

# Create a list of titles
titles <- c("Enrollment Outcomes", "Enrollment Outcomes", "Enrollment Outcomes",
            "Enrollment Outcomes - Public", "Enrollment Outcomes - Public", "Enrollment Outcomes - Public",
            "Learning Outcomes", "Learning Outcomes")

filenames <- c("tex/ols_dl_mun/Enrollment_OLS_DL_Approval.tex", "tex/ols_dl_mun/Enrollment_OLS_DL_Failure.tex", "tex/ols_dl_mun/Enrollment_OLS_DL_Abandonment.tex",
                "tex/ols_dl_mun/Enrollment_OLS_DL_Approval_Pub.tex", "tex/ols_dl_mun/Enrollment_OLS_DL_Failure_Pub.tex", "tex/ols_dl_mun/Enrollment_OLS_DL_Abandonment_Pub.tex",
                "tex/ols_dl_mun/Learning_OLS_DL_Mathematics.tex", "tex/ols_dl_mun/Learning_OLS_DL_Portuguese.tex")

i <- 0
for (outcome in single_outcome_groups) {
    i <- i + 1
    stargazer(regs_groups[[i]], type = "text", title = titles[i],
                        dep.var.labels = labels_group_list[[i]], 
                        covariate.labels = strategy_names_labels, 
                        omit.stat = c("f", "ser"), 
                        digits = 3, 
                        column.labels = rep(c("OLS", "Double Lasso"), 3), 
                        model.names = FALSE,
                        out = filenames[i], 
                        keep = c("human.capital.focus", "physical.capital.focus", "human.and.physical.capital.focus"))
}

