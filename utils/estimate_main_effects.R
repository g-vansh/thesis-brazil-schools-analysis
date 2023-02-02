
############################################
# Estimate the main effects of the strategies using regressions
############################################
# Estiamte the main effects with the doubly robust model
library(sandwich)


# Join municipalities and municipalities_outcomes on "CO_MUNICIPALITY"
municipalities_reg <- merge(municipalities, municipalities_outcomes, by = "CO_MUNICIPALITY")
stratgies <- strategy_names


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
    if(str_length(cd) > 0 ){
      m2_formula = paste0(m2_formula, "+", cd)
    }
  }

  m2_formula = paste0(m2_formula, "", formula_end)
  m2_formula = as.formula(m2_formula)
  print(m2_formula)
  m4 <- felm(m2_formula, data=cb_startups)
  ret_obj = list(formula=m2_formula, 
                 regression = m4)
  return (ret_obj)
}

# Run OLS regressions for outcomes (PROVA_MEAN_MAT_STD, PROVA_MEAN_PORT_STD, RATE_APROV, RATE_FAILURE, RATE_ABANDON, RATE_APROV_PUB, RATE_FAILURE_PUB, RATE_ABANDON_PUB) and strategies (physical_capital_focus, human_capital_focus, human_capital_and_physical_capital_focus)
ols.MAT.Physical_Capital <- felm(PROVA_MEAN_MAT_STD ~ physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg_reg )
ols.MAT.Human_Capital <- felm(PROVA_MEAN_MAT_STD ~ human_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.MAT.Human_and_Physical_Capital <- felm(PROVA_MEAN_MAT_STD ~ human_capital_and_physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.PORT.Physical_Capital <- felm(PROVA_MEAN_PORT_STD ~ physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.PORT.Human_Capital <- felm(PROVA_MEAN_PORT_STD ~ human_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.PORT.Human_and_Physical_Capital <- felm(PROVA_MEAN_PORT_STD ~ human_capital_and_physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.APROV.Physical_Capital <- felm(RATE_APROV ~ physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.APROV.Human_Capital <- felm(RATE_APROV ~ human_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.APROV.Human_and_Physical_Capital <- felm(RATE_APROV ~ human_capital_and_physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.FAILURE.Physical_Capital <- felm(RATE_FAILURE ~ physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.FAILURE.Human_Capital <- felm(RATE_FAILURE ~ human_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.FAILURE.Human_and_Physical_Capital <- felm(RATE_FAILURE ~ human_capital_and_physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )  
ols.ABANDON.Physical_Capital <- felm(RATE_ABANDON ~ physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.ABANDON.Human_Capital <- felm(RATE_ABANDON ~ human_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.ABANDON.Human_and_Physical_Capital <- felm(RATE_ABANDON ~ human_capital_and_physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.APROV_PUB.Physical_Capital <- felm(RATE_APROV_PUB ~ physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.APROV_PUB.Human_Capital <- felm(RATE_APROV_PUB ~ human_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.APROV_PUB.Human_and_Physical_Capital <- felm(RATE_APROV_PUB ~ human_capital_and_physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.FAILURE_PUB.Physical_Capital <- felm(RATE_FAILURE_PUB ~ physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.FAILURE_PUB.Human_Capital <- felm(RATE_FAILURE_PUB ~ human_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.FAILURE_PUB.Human_and_Physical_Capital <- felm(RATE_FAILURE_PUB ~ human_capital_and_physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.ABANDON_PUB.Physical_Capital <- felm(RATE_ABANDON_PUB ~ physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.ABANDON_PUB.Human_Capital <- felm(RATE_ABANDON_PUB ~ human_capital_focus | CO_MUNICIPALITY,  municipalities_reg )
ols.ABANDON_PUB.Human_and_Physical_Capital <- felm(RATE_ABANDON_PUB ~ human_capital_and_physical_capital_focus | CO_MUNICIPALITY,  municipalities_reg )

# Outcomes is a list of outcomes to be studied
# Strategies is a list of strategies to be studied
double_lasso_regressions <- list()
for (outcome in outcomes){
    for (strategy in strategies){
        # Create the formula
        formula <- as.formula(paste(outcome, "~", strategy))
        # Create the model
        model <- do_double_lasso(X=municipalities_reg[,ml_vars], 
                                 treatment=municipalities_reg[[strategy]],
                                 y=municipalities_reg[[outcome]], 
                                 formula_stem=formula,
                                 formula_end="| CO_MUNICIPALITY")
        # Add the model to the list
        double_lasso_regressions[[paste(outcome, strategy, sep=".")]] <- model
    }
}

regvars <- c("physical_capital_focus", "human_capital_focus", "human_capital_and_physical_capital_focus")

# Regs is in the form of a list of regressions alternating between an OLS regression and the respective double lasso regression
regs <- list(ols.MAT.Physical_Capital, double_lasso_regressions[["MAT.Physical_Capital"]], 
             ols.MAT.Human_Capital, double_lasso_regressions[["MAT.Human_Capital"]], 
             ols.MAT.Human_and_Physical_Capital, double_lasso_regressions[["MAT.Human_and_Physical_Capital"]])

regs_se <- list(sqrt(diag(as.matrix(vcovHC(ols.MAT.Physical_Capital, type = "HC1")))),
                sqrt(diag(as.matrix(vcovHC(double_lasso_regressions[["MAT.Physical_Capital"]], type = "HC1")))),
                sqrt(diag(as.matrix(vcovHC(ols.MAT.Human_Capital, type = "HC1")))),
                sqrt(diag(as.matrix(vcovHC(double_lasso_regressions[["PORT.Human_Capital"]], type = "HC1")))),
                sqrt(diag(as.matrix(vcovHC(ols.MAT.Human_and_Physical_Capital, type = "HC1")))),
                sqrt(diag(as.matrix(vcovHC(double_lasso_regressions[["MAT.Human_and_Physical_Capital"]], type = "HC1")))))

# save(regs,file="double_lasso_objects.RData")
municipalities_reg.labels <- names(municipalities_reg)
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

