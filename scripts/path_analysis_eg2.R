
#SEM - structural equation modelng 

#load library
library('lavaan')
library('semPlot')

#using summary_BEF pulling in moddat_S bc variables are scaled

#building model 
model_s <- '
  # Regressions for Fish Stability of Biomass
  F_stab ~ tempS + salS + Sasym
  
  # Covariances between predictors
  tempS ~~ salS        
  #F_stab ~~ Sasym
  #F_stab ~~ 1*F_stab 
  '

#fit the SEM model
fit <- sem(model_s, data = moddat_S)

#summary of model fit 
summary(fit, fit.measures = T)


#parameter estimates 
parameterEstimates(fit)

# sem path plots
semPaths(fit, 'std', layout='tree2')

  


# fish biomass model 

model_b <- '
  # Regressions for Fish Biomass (additional equation)
  F_bio ~ tempS + salS + Sasym
  F_bio ~ Sasym
  
  # Covariances between predictors
  tempS ~~ salS        
  #F_bio ~~ Sasym

  '

#fit the SEM model
fit <- sem(model_b, data = moddat_S)
summary(fit, fit.measures = T)


#parameter estimates 
parameterEstimates(fit)


semPaths(fit, 'std', layout='tree2')






# try simplier model 
# Start with a simpler model
model_simple <- '
  # Fish stability model
  F_stab ~ tempS + salS + Sasym
  F_stab ~ F_bio
  
  # Covariances between predictors
  F_bio ~~ N        
  tempS ~~ salS        
  N ~~ Sasym     
  F_bio ~~ Sasym
  F_bio ~~ F_stab
  N ~~ Sasym
'

# Fit the simpler model
fit_simple <- sem(model_simple, data = moddat_S)

# Check model fit summary
summary(fit_simple, fit.measures = TRUE)

  

#continue working on these two models, what else needs to be edited in the results?






