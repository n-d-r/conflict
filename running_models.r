#===========================================================================================================================#
# Setup                                                                                                                     #
#===========================================================================================================================#

rm(list = ls(all = TRUE))

setwd('C:/IPSC/data_&_code/data and code for ILPI pres., 02-11-2015')

source('functions.r')
source('models.r')
source('constants.r')

require(pscl)     # For zero-inflated negative binomial regression model
require(pROC)     # For Receiver Operating Characteristic curve and Area Under Curve metric
require(lme4)     # For random-effects model
require(glmnet)   # For lasso models

set.seed(333)

#===========================================================================================================================#
# Main                                                                                                                      #
#===========================================================================================================================#

dat <- read.csv('historical_data_imputed_by_R.csv', header = TRUE)

models <- list()
models[['BaseModel']]                           <- BaseModel
models[['HighlyViolentBaseModel']]              <- HighlyViolentBaseModel
models[['AllInteractions']]                     <- AllInteractions
#odels[['RandomEffectsRegions']]                <- RandomEffectsRegions
# models[['InteractionsAndRandomEffectsRegions']] <- InteractionsAndRandomEffectsRegions    # Under development; do not use
#models[['ZeroinflatedNegativeBinomial']]        <- ZeroinflatedNegativeBinomial
#models[['ZeroinflAndInteractions']]             <- ZeroinflAndInteractions
#models[['HurdleModel']]                         <- HurdleModel
#models[['HurdleInteractions']]                  <- HurdleInteractions
# models[['LassoMin']]                            <- LassoMin                               # Under development; do not use
# models[['Lasso1se']]                            <- Lasso1se                               # Under development; do not use
# models[['LassoTwoModelsMin']]                   <- LassoTwoModelsMin                      # Under development; do not use
# models[['LassoTwoModels1se']]                   <- LassoTwoModels1se                      # Under development; do not use
#models[['PrincipalComponents']]                 <- PrincipalComponents                    
# models[['PrincipalComponentsByRiskArea']]       <- PrincipalComponentsByRiskArea          # Under development; do not use
#models[['POPandGDP_CAP']]                       <- POPandGDP_CAP
#models[['STRUCTandGDP_CAP']]                    <- STRUCTandGDP_CAP
#models[['ETHNICandREG_U']]                      <- ETHNICandREG_U
#models[['ETHNICandREG_P2']]                     <- ETHNICandREG_P2
#models[['INEQandGDP_CAP']]                      <- INEQandGDP_CAP
#models[['FUEL_EXPandGDP_CAP']]                  <- FUEL_EXPandGDP_CAP
#models[['FUEL_EXPandREG_U']]                    <- FUEL_EXPandREG_U
#models[['FUEL_EXPandREG_P2']]                   <- FUEL_EXPandREG_P2
#models[['FUEL_EXPandECON_ISO']]                 <- FUEL_EXPandECON_ISO
#models[['GDP_CAPsquared']]                      <- GDP_CAPsquared
#models[['REG_P2squared']]                       <- REG_P2squared
#models[['GDP_CAPandDISPER']]                    <- GDP_CAPandDISPER

all.model.results    <- runModels(list.of.models = models, dat = dat)
compareModels(all.model.results, save.as.csv = TRUE, custom.name = 'old-vars__no-cv')

cv.all.model.results <- runModels(list.of.models = models, dat = dat, cross.validation = TRUE)
compareModels(cv.all.model.results, save.as.csv = TRUE, custom.name = 'old-vars__with-cv')    

dt.all.model.results <- runModels(list.of.models = models, dat = dat, threshold = 'dynamic')
compareModels(dt.all.model.results, save.as.csv = TRUE, custom.name = 'old-vars__no-cv__dynamic-threshold')

cv.dt.all.model.results <- runModels(list.of.models = models, dat = dat, threshold = 'dynamic', cross.validation = TRUE)
compareModels(cv.dt.all.model.results, save.as.csv = TRUE, custom.name = 'old-vars__with-cv__dynamic-threshold')

#===========================================================================================================================#
# Main - new variables                                                                                                      #
#===========================================================================================================================#

dat.new.var <- read.csv('historical_data_imputed_by_R_new_vars.csv', header = TRUE)

models <- list()
models[['BaseModelNewVarsMale']]                           <- BaseModelNewVarsMale
models[['BaseModelNewVarsBoth']]                           <- BaseModelNewVarsBoth
models[['InteractionNewVarsMale']]                         <- InteractionNewVarsMale
models[['InteractionNewVarsBoth']]                         <- InteractionNewVarsBoth
models[['HighlyViolentInteractionNewVarsMale']]            <- HighlyViolentInteractionNewVarsMale
models[['HighlyViolentInteractionNewVarsBoth']]            <- HighlyViolentInteractionNewVarsBoth
models[['AllInteractionsNewVarsMale']]                     <- AllInteractionsNewVarsMale
models[['RandomEffectsRegionsNewVarsMale']]                <- RandomEffectsRegionsNewVarsMale
# models[['InteractionsAndRandomEffectsRegionsNewVarsMale']] <- InteractionsAndRandomEffectsRegionsNewVarsMale
models[['ZeroinflatedNegativeBinomialNewVarsMale']]        <- ZeroinflatedNegativeBinomialNewVarsMale
models[['ZeroinflAndInteractionsNewVarsMale']]             <- ZeroinflAndInteractionsNewVarsMale
models[['HurdleModelNewVarsMale']]                         <- HurdleModelNewVarsMale
models[['HurdleInteractionsNewVarsMale']]                  <- HurdleInteractionsNewVarsMale
# models[['LassoMinNewVarsMale']]                            <- LassoMinNewVarsMale
# models[['Lasso1seNewVarsMale']]                            <- Lasso1seNewVarsMale
# models[['LassoTwoModelsMinNewVarsMale']]                   <- LassoTwoModelsMinNewVarsMale
# models[['LassoTwoModels1seNewVarsMale']]                   <- LassoTwoModels1seNewVarsMale
models[['PrincipalComponentsNewVarsMale']]                 <- PrincipalComponentsNewVarsMale
models[['POPandGDP_CAPNewVarsMale']]                       <- POPandGDP_CAPNewVarsMale
models[['STRUCTandGDP_CAPNewVarsMale']]                    <- STRUCTandGDP_CAPNewVarsMale
models[['ETHNICandREG_UNewVarsMale']]                      <- ETHNICandREG_UNewVarsMale
models[['ETHNICandREG_P2NewVarsMale']]                     <- ETHNICandREG_P2NewVarsMale
models[['INEQandGDP_CAPNewVarsMale']]                      <- INEQandGDP_CAPNewVarsMale
models[['FUEL_EXPandGDP_CAPNewVarsMale']]                  <- FUEL_EXPandGDP_CAPNewVarsMale
models[['FUEL_EXPandREG_UNewVarsMale']]                    <- FUEL_EXPandREG_UNewVarsMale
models[['FUEL_EXPandREG_P2NewVarsMale']]                   <- FUEL_EXPandREG_P2NewVarsMale
models[['FUEL_EXPandECON_ISONewVarsMale']]                 <- FUEL_EXPandECON_ISONewVarsMale
models[['GDP_CAPsquaredNewVarsMale']]                      <- GDP_CAPsquaredNewVarsMale
models[['REG_P2squaredNewVarsMale']]                       <- REG_P2squaredNewVarsMale
models[['GDP_CAPandDISPERNewVarsMale']]                    <- GDP_CAPandDISPERNewVarsMale

nv.model.results <- runModels(list.of.models = models, dat = dat.new.var)
compareModels(nv.model.results, save.as.csv = TRUE, custom.name = 'new-vars__no-cv')

nv.cv.model.results <- runModels(list.of.models = models, dat = dat.new.var, cross.validation = TRUE)
compareModels(nv.cv.model.results, save.as.csv = TRUE, custom.name = 'new-vars__with-cv')

nv.dt.model.results <- runModels(list.of.models = models, dat = dat.new.var, threshold = 'dynamic')
compareModels(nv.dt.model.results, save.as.csv = TRUE, custom.name = 'new-vars__no-cv__dynamic-threshold')

nv.cv.dt.model.results <- runModels(list.of.models = models, dat = dat.new.var, threshold = 'dynamic', cross.validation = TRUE)
compareModels(nv.cv.dt.model.results, save.as.csv = TRUE, custom.name = 'new-vars__with-cv__dynamic-threshold')


#===========================================================================================================================#
# Applying the models                                                                                                       #
#===========================================================================================================================#

most.recent.data <- read.csv('preprocessed_current_data.csv')
most.recent.data <- switchColnames(most.recent.data, mode='forth', new.var=FALSE)

applied.models.static <- applyModels(list.of.models = models, train.data = dat, apply.data = most.recent.data)
saveAppliedModelsOutput(applied.models.static)

applied.models.dynamic <- applyModels(list.of.models = models, train.data = dat,
                                      apply.data = most.recent.data, threshold = 'dynamic')
saveAppliedModelsOutput(applied.models.dynamic)      

entire.dataset <- read.csv('preproc_entire_dataset_R-imputed.csv', header = TRUE)
train.data <- subset(entire.dataset, entire.dataset$YEAR < 2010)    # Only years 1989-2009 are used for training
applied.static.all.years <- applyModels(list.of.models = models, 
                                        train.data = train.data, 
                                        apply.data = entire.dataset)
saveAppliedModelsOutput(applied.static.all.years, applied.to = 'all')                                         