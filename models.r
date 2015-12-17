#===========================================================================================================================#
# Models                                                                                                                    #
#---------------------------------------------------------------------------------------------------------------------------#
# This file contains the statistical model specifications used in the GCRI. Every model is wrapped by a function named      #
# accordingly, and returns at the least the statistical model(s) and the predictions                                        #
#===========================================================================================================================#

BaseModel <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

HighlyViolentBaseModel <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    # warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'HVC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'HVC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y     = test[, conflict.intensity], 
                                predicted.y    = predictions.list$predictions,  
                                probabilities  = predictions.list$probabilities,
                                highly.violent = TRUE)
    model.output[['metrics']] <- metrics
  } else {                                        
    # warning('No metrics computed')                                         
  }                                       
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

AllInteractionsOld <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: all
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ POP  * GDP_CAP  + STRUCT    * GDP_CAP   + ', ethnic, ' * REG_U  + ',
                                ethnic, ' * REG_P2  + INEQ        * GDP_CAP  + FUEL_EXP  * GDP_CAP   + FUEL_EXP     * REG_U  + 
                                FUEL_EXP  * REG_P2  + FUEL_EXP    * ECON_ISO + I(GDP_CAP^2) + I(REG_P2^2)  + GDP_CAP      * DISPER + 
                                GOV_EFF   + EMPOWER + REPRESS     + CON_NB   + YRS_HVC   + CON_TREND + CON_INT      + MORT   + 
                                DISPER    + HOMIC   + FOOD        + WATER', sep = '')), data = train, family = 'binomial')
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ POP  * GDP_CAP  + STRUCT    * GDP_CAP   + ', ethnic, ' * REG_U  + ',
                                ethnic, ' * REG_P2  + INEQ        * GDP_CAP  + FUEL_EXP  * GDP_CAP   + FUEL_EXP     * REG_U  + 
                                FUEL_EXP  * REG_P2  + FUEL_EXP    * ECON_ISO + I(GDP_CAP^2) + I(REG_P2^2)  + GDP_CAP      * DISPER + 
                                GOV_EFF   + EMPOWER + REPRESS     + CON_NB   + YRS_HVC   + CON_TREND + CON_INT      + MORT   + 
                                DISPER    + HOMIC   + FOOD        + WATER', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))              
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity],
                                predicted.y   = predictions.list$predictions,
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics
  } else {
    warning('No metrics computed')
  }  
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

AllInteractions <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: all
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + 
                                POP:GDP_CAP + STRUCT:GDP_CAP + ', ethnic, ':REG_U + ', ethnic, ':REG_P2 + 
                                INEQ:GDP_CAP + FUEL_EXP:GDP_CAP + FUEL_EXP:REG_U + FUEL_EXP:REG_P2 + 
                                FUEL_EXP:ECON_ISO + I(GDP_CAP^2) + I(REG_P2^2) + GDP_CAP:DISPER', sep = '')), data = train, family = 'binomial')
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + 
                                POP:GDP_CAP + STRUCT:GDP_CAP + ', ethnic, ':REG_U + ', ethnic, ':REG_P2 + 
                                INEQ:GDP_CAP + FUEL_EXP:GDP_CAP + FUEL_EXP:REG_U + FUEL_EXP:REG_P2 + 
                                FUEL_EXP:ECON_ISO + I(GDP_CAP^2) + I(REG_P2^2) + GDP_CAP:DISPER', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))              
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity],
                                predicted.y   = predictions.list$predictions,
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics
  } else {
    warning('No metrics computed')
  }
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

RandomEffectsRegions <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: 'REGIONS' variable
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glmer(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + (1 | REGIONS)', sep = '')), data = train, family = 'binomial')
  linear.model <- lmer(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + (1 | REGIONS)', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))     
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity],
                                predicted.y   = predictions.list$predictions,
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics  
  } else {
    warning('No metrics computed')
  }
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

InteractionsAndRandomEffectsRegions <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: all
  # Random factors: all
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glmer(as.formula(paste(conflict.boolean, '~ POP  * GDP_CAP  + STRUCT       * GDP_CAP      + ', ethnic, ' * REG_U  + ',
                                  ethnic, ' * REG_P2  + INEQ      * GDP_CAP  + FUEL_EXP     * GDP_CAP      + FUEL_EXP     * REG_U  + 
                                  FUEL_EXP  * REG_P2  + FUEL_EXP  * ECON_ISO + I(GDP_CAP^2) + I(REG_P2^2)  + GDP_CAP      * DISPER + 
                                  GOV_EFF   + EMPOWER + REPRESS   + CON_NB   + YRS_HVC      + CON_TREND    + CON_INT      + MORT   + 
                                  DISPER    + HOMIC   + FOOD      + WATER    + (1 | REGIONS)', 
                                  sep = '')), data = train, family = 'binomial', nAGQ = 10,
                                  control = glmerControl(optimizer = 'bobyqa', check.conv.singular = 'warning',
                                            optCtrl = list(maxfun = 10000)))
  linear.model <- lmer(as.formula(paste(conflict.intensity, '~ POP * GDP_CAP  + STRUCT       * GDP_CAP      + ', ethnic, ' * REG_U  + ',
                                  ethnic, ' * REG_P2  + INEQ       * GDP_CAP  + FUEL_EXP     * GDP_CAP      + FUEL_EXP     * REG_U  + 
                                  FUEL_EXP  * REG_P2  + FUEL_EXP   * ECON_ISO + I(GDP_CAP^2) + I(REG_P2^2)  + GDP_CAP      * DISPER + 
                                  GOV_EFF   + EMPOWER + REPRESS    + CON_NB   + YRS_HVC      + CON_TREND    + CON_INT      + MORT   + 
                                  DISPER    + HOMIC   + FOOD       + WATER    + (1 | REGIONS)', sep = '')), 
                                  data = subset(train, train[, conflict.boolean] == 1),
                                  control = lmerControl(optimizer = 'bobyqa', check.conv.singular = 'warning'))              
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity],
                                predicted.y   = predictions.list$predictions,
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics
  } else {
    warning('No metrics computed')
  }
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

ZeroinflatedNegativeBinomial <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: zero-inflated negative binomial regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  }
  train[, conflict.intensity] <- as.integer(train[, conflict.intensity])
  zeroinfl.model <- zeroinfl(as.formula(paste(conflict.intensity, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT', sep = '')), data = train, dist = 'negbin', EM = TRUE)
  predictions.list <- computePredictions(model.one = zeroinfl.model, new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y  = test[, conflict.intensity],
                                predicted.y = predictions.list$predictions)
    model.output[['metrics']] <- metrics                                         
  } else {
    warning('No metrics computed')
  }
  model.output[['zeroinfl.model']]   <- zeroinfl.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

ZeroinflAndInteractions <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: zero-inflated negative binomial regression
  # Features/Variables: all
  # Interactions: all
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  }
  train[, conflict.intensity] <- as.integer(train[, conflict.intensity])
  zeroinfl.model <- zeroinfl(as.formula(paste(conflict.intensity, '~ POP     * GDP_CAP      + STRUCT       * GDP_CAP      + ', ethnic, ' * 
                                REG_U  + ', ethnic, ' * REG_P2    + INEQ     * GDP_CAP      + FUEL_EXP     * GDP_CAP      + FUEL_EXP     * 
                                REG_U  + FUEL_EXP     * REG_P2    + FUEL_EXP * ECON_ISO     + I(GDP_CAP^2) + I(REG_P2^2)  + GDP_CAP      * 
                                DISPER + GOV_EFF      + EMPOWER   + REPRESS  + CON_NB       + YRS_HVC      + CON_TREND    + CON_INT      + 
                                MORT   + DISPER       + HOMIC     + FOOD     + WATER', sep = '')), data = train, dist = 'negbin', EM = TRUE)
  predictions.list <- computePredictions(model.one = zeroinfl.model, new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y  = test[, conflict.intensity],
                                predicted.y = predictions.list$predictions)
    model.output[['metrics']] <- metrics                                         
  } else {
    warning('No metrics computed')
  }  
  model.output[['zeroinfl.model']]   <- zeroinfl.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

HurdleModel <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Helpful: http://stats.stackexchange.com/questions/60643/difference-between-binomial-negative-binomial-and-poisson-regression
  # Helpful: http://stats.stackexchange.com/questions/81457/what-is-the-difference-between-zero-inflated-and-hurdle-distributions-models
  # Statistical model: hurdle
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  }
  train[, conflict.intensity] <- as.integer(train[, conflict.intensity])
  hurdle.model <- hurdle(as.formula(paste(conflict.intensity, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT', sep = '')), data = train)
  predictions.list <- computePredictions(model.one = hurdle.model, new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y  = test[, conflict.intensity],
                                predicted.y = predictions.list$predictions)
    model.output[['metrics']] <- metrics                                         
  } else {
    warning('No metrics computed')
  }    
  model.output[['hurdle.model']]     <- hurdle.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

HurdleInteractions <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: hurdle
  # Features/Variables: all
  # Interactions: all
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  }
  train[, conflict.intensity] <- as.integer(train[, conflict.intensity])
  hurdle.model <- hurdle(as.formula(paste(conflict.intensity, '~ POP       * GDP_CAP      + STRUCT      * GDP_CAP   + ', ethnic, ' * REG_U  + ',
                                ethnic, ' * REG_P2  + INEQ      * GDP_CAP  + FUEL_EXP     * GDP_CAP     + FUEL_EXP  * REG_U  + 
                                FUEL_EXP  * REG_P2  + FUEL_EXP  * ECON_ISO + I(GDP_CAP^2) + I(REG_P2^2) + GDP_CAP   * DISPER + 
                                GOV_EFF   + EMPOWER + REPRESS   + CON_NB   + YRS_HVC      + CON_TREND   + CON_INT   + MORT   + 
                                DISPER    + HOMIC   + FOOD      + WATER', sep = '')), data = train)
  predictions.list <- computePredictions(model.one = hurdle.model, new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y  = test[, conflict.intensity],
                                predicted.y = predictions.list$predictions)
    model.output[['metrics']] <- metrics                                         
  } else {
    warning('No metrics computed')
  }
  model.output[['hurdle.model']]     <- hurdle.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

LassoMin <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  stop('LassoMin: do not yet use this function/model')
  # Statistical model: generalized linear model with lasso regularisation
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  # Note: 'glmnet' is a generalisation of the lasso; said differently, lasso is a special case
  #   of the elastic net (glmnet) dependent on the regularisation parameter lambda
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  design.matrix <- as.matrix(cbind(train[, 'REG_U'],   train[, 'REG_P2'],  train[, 'GOV_EFF'],  train[, 'EMPOWER'], 
                                   train[, 'REPRESS'], train[, 'CON_NB'],  train[, 'YRS_HVC'],  train[, 'CON_TREND'],
                                   train[, 'CON_INT'], train[, 'MORT'],    train[, 'DISPER'],   train[, 'HOMIC'],
                                   train[, ethnic],    train[, 'GDP_CAP'], train[, 'ECON_ISO'], train[, 'INEQ'],
                                   train[, 'FOOD'],    train[, 'POP'],     train[, 'WATER'],    train[, 'FUEL_EXP'],
                                   train[, 'STRUCT']))
  cv.lasso.model <- cv.glmnet(x = design.matrix, y = train[, conflict.intensity], family = 'gaussian')
  newx.matrix <- as.matrix(cbind(test[, 'REG_U'],   test[, 'REG_P2'],  test[, 'GOV_EFF'],  test[, 'EMPOWER'], 
                                 test[, 'REPRESS'], test[, 'CON_NB'],  test[, 'YRS_HVC'],  test[, 'CON_TREND'],
                                 test[, 'CON_INT'], test[, 'MORT'],    test[, 'DISPER'],   test[, 'HOMIC'],
                                 test[, ethnic],    test[, 'GDP_CAP'], test[, 'ECON_ISO'], test[, 'INEQ'],
                                 test[, 'FOOD'],    test[, 'POP'],     test[, 'WATER'],    test[, 'FUEL_EXP'],
                                 test[, 'STRUCT']))
  predictions.list <- computePredictions(model.one = cv.lasso.model, new.data = newx.matrix, 
                                         threshold = NULL, s = 'lambda.min')
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y  <- test[, conflict.intensity],
                                predicted.y <- predictions.list$predictions)
    model.output[['metrics']] <- metrics                                    
  } else {
    warning('No metrics computed')
  }
  model.output[['cv.lasso.model']]   <- cv.lasso.model                                   
  model.output[['predictions.list']] <- predictions.list                                                                      
  return(model.output)
}

Lasso1se <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  stop('Lasso1se: do not yet use this function/model')
  # Statistical model: generalized linear model with lasso regularisation
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  design.matrix <- as.matrix(cbind(train[, 'REG_U'],   train[, 'REG_P2'],  train[, 'GOV_EFF'],  train[, 'EMPOWER'], 
                                   train[, 'REPRESS'], train[, 'CON_NB'],  train[, 'YRS_HVC'],  train[, 'CON_TREND'],
                                   train[, 'CON_INT'], train[, 'MORT'],    train[, 'DISPER'],   train[, 'HOMIC'],
                                   train[, ethnic],    train[, 'GDP_CAP'], train[, 'ECON_ISO'], train[, 'INEQ'],
                                   train[, 'FOOD'],    train[, 'POP'],     train[, 'WATER'],    train[, 'FUEL_EXP'],
                                   train[, 'STRUCT']))
  cv.lasso.model <- cv.glmnet(x = design.matrix, y = train[, conflict.intensity], family = 'gaussian')
  newx.matrix <- as.matrix(cbind(test[, 'REG_U'],   test[, 'REG_P2'],  test[, 'GOV_EFF'],  test[, 'EMPOWER'], 
                                 test[, 'REPRESS'], test[, 'CON_NB'],  test[, 'YRS_HVC'],  test[, 'CON_TREND'],
                                 test[, 'CON_INT'], test[, 'MORT'],    test[, 'DISPER'],   test[, 'HOMIC'],
                                 test[, ethnic],    test[, 'GDP_CAP'], test[, 'ECON_ISO'], test[, 'INEQ'],
                                 test[, 'FOOD'],    test[, 'POP'],     test[, 'WATER'],    test[, 'FUEL_EXP'],
                                 test[, 'STRUCT']))
  predictions.list <- computePredictions(model.one = cv.lasso.model, new.data = newx.matrix, 
                                         threshold = NULL, s = 'lambda.1se')
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y  <- test[, conflict.intensity],
                                predicted.y <- predictions.list$predictions)
    model.output[['metrics']] <- metrics                                    
  } else {
    warning('No metrics computed')
  }
  model.output[['cv.lasso.model']]   <- cv.lasso.model                                   
  model.output[['predictions.list']] <- predictions.list                                                                      
  return(model.output)
}

LassoTwoModelsMin <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  stop('LassoTwoModelsMin: do not yet use this function/model')
  # Statistical model: generalized linear model with lasso regularisation
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }  
  design.matrix <- as.matrix(cbind(train[, 'REG_U'],   train[, 'REG_P2'],  train[, 'GOV_EFF'],  train[, 'EMPOWER'], 
                                   train[, 'REPRESS'], train[, 'CON_NB'],  train[, 'YRS_HVC'],  train[, 'CON_TREND'],
                                   train[, 'CON_INT'], train[, 'MORT'],    train[, 'DISPER'],   train[, 'HOMIC'],
                                   train[, ethnic],    train[, 'GDP_CAP'], train[, 'ECON_ISO'], train[, 'INEQ'],
                                   train[, 'FOOD'],    train[, 'POP'],     train[, 'WATER'],    train[, 'FUEL_EXP'],
                                   train[, 'STRUCT']))
  newx.matrix <- as.matrix(cbind(test[, 'REG_U'],   test[, 'REG_P2'],  test[, 'GOV_EFF'],  test[, 'EMPOWER'], 
                                 test[, 'REPRESS'], test[, 'CON_NB'],  test[, 'YRS_HVC'],  test[, 'CON_TREND'],
                                 test[, 'CON_INT'], test[, 'MORT'],    test[, 'DISPER'],   test[, 'HOMIC'],
                                 test[, ethnic],    test[, 'GDP_CAP'], test[, 'ECON_ISO'], test[, 'INEQ'],
                                 test[, 'FOOD'],    test[, 'POP'],     test[, 'WATER'],    test[, 'FUEL_EXP'],
                                 test[, 'STRUCT'],  test[, conflict.boolean]))
  cv.lasso.binomial.model <- cv.glmnet(x = design.matrix, y = as.factor(train[, conflict.boolean]), family = 'binomial')
  cv.lasso.gaussian.model <- cv.glmnet(x = design.matrix, y = train[, conflict.intensity], family = 'gaussian')
  predictions.list <- computePredictions(model.one = cv.lasso.binomial.model, model.two = cv.lasso.gaussian.model,
                                         new.data = newx.matrix, threshold = threshold, s = 'lambda.min')
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    <- test[, conflict.intensity],
                                predicted.y   <- predictions.list$predictions,
                                probabilities <- predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                
  } else {
    warning('No metrics computed')                              
  }                               
  model.output[['cv.lasso.binomial.model']] <- cv.lasso.binomial.model
  model.output[['cv.lasso.gaussian.model']] <- cv.lasso.gaussian.model
  model.output[['predictions.list']]        <- predictions.list
  return(model.output)                                 
}

LassoTwoModels1se <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  stop('LassoTwoModels1se: do not yet use this function/model')
  # Statistical model: generalized linear model with lasso regularisation
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }  
  design.matrix <- as.matrix(cbind(train[, 'REG_U'],   train[, 'REG_P2'],  train[, 'GOV_EFF'],  train[, 'EMPOWER'], 
                                   train[, 'REPRESS'], train[, 'CON_NB'],  train[, 'YRS_HVC'],  train[, 'CON_TREND'],
                                   train[, 'CON_INT'], train[, 'MORT'],    train[, 'DISPER'],   train[, 'HOMIC'],
                                   train[, ethnic],    train[, 'GDP_CAP'], train[, 'ECON_ISO'], train[, 'INEQ'],
                                   train[, 'FOOD'],    train[, 'POP'],     train[, 'WATER'],    train[, 'FUEL_EXP'],
                                   train[, 'STRUCT']))
  newx.matrix <- as.matrix(cbind(test[, 'REG_U'],   test[, 'REG_P2'],  test[, 'GOV_EFF'],  test[, 'EMPOWER'], 
                                 test[, 'REPRESS'], test[, 'CON_NB'],  test[, 'YRS_HVC'],  test[, 'CON_TREND'],
                                 test[, 'CON_INT'], test[, 'MORT'],    test[, 'DISPER'],   test[, 'HOMIC'],
                                 test[, ethnic],    test[, 'GDP_CAP'], test[, 'ECON_ISO'], test[, 'INEQ'],
                                 test[, 'FOOD'],    test[, 'POP'],     test[, 'WATER'],    test[, 'FUEL_EXP'],
                                 test[, 'STRUCT'],  test[, conflict.boolean]))
  cv.lasso.binomial.model <- cv.glmnet(x = design.matrix, y = as.factor(train[, conflict.boolean]), family = 'binomial')
  cv.lasso.gaussian.model <- cv.glmnet(x = design.matrix, y = train[, conflict.intensity], family = 'gaussian')
  predictions.list <- computePredictions(model.one = cv.lasso.binomial.model, model.two = cv.lasso.gaussian.model,
                                         new.data = newx.matrix, threshold = threshold, s = 'lambda.1se')
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    <- test[, conflict.intensity],
                                predicted.y   <- predictions.list$predictions,
                                probabilities <- predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                
  } else {
    warning('No metrics computed')                              
  }                               
  model.output[['cv.lasso.binomial.model']] <- cv.lasso.binomial.model
  model.output[['cv.lasso.gaussian.model']] <- cv.lasso.gaussian.model
  model.output[['predictions.list']]        <- predictions.list
  return(model.output)                                 
}

PrincipalComponents <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # stop('PrincipalComponents: do not yet use this function/model')
  # Statistical model: principal component logistic and linear regression
  # Features/Variables: principal components of all original variables up to cumulative proportion of ~ .80,
  #   which corresponds to 8 components with a cumulative proportion of .815
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }  
  vars <- c('REG_U', 'REG_P2', 'GOV_EFF', 'EMPOWER', 'REPRESS', 'CON_NB', 'YRS_HVC', 'CON_TREND', 
            'CON_INT', 'MORT', 'DISPER', 'HOMIC', ethnic, 'GDP_CAP', 'ECON_ISO', 'INEQ', 'FOOD', 'POP',
            'WATER', 'FUEL_EXP', 'STRUCT')
  # pca <- prcomp(as.matrix(cbind(train[, 'REG_U'],   train[, 'REG_P2'],  train[, 'GOV_EFF'],  train[, 'EMPOWER'], 
                                # train[, 'REPRESS'], train[, 'CON_NB'],  train[, 'YRS_HVC'],  train[, 'CON_TREND'],
                                # train[, 'CON_INT'], train[, 'MORT'],    train[, 'DISPER'],   train[, 'HOMIC'],
                                # train[, ethnic],    train[, 'GDP_CAP'], train[, 'ECON_ISO'], train[, 'INEQ'],
                                # train[, 'FOOD'],    train[, 'POP'],     train[, 'WATER'],    train[, 'FUEL_EXP'],
                                # train[, 'STRUCT'])))
  pca <- prcomp(train[, vars])                                
  design.matrix.glm <- cbind(train[, conflict.boolean], pca$x[, 1:8])
  colnames(design.matrix.glm)[1] <- as.character(conflict.boolean)
  design.matrix.lm  <- cbind(train[, conflict.intensity], pca$x[, 1:8])
  colnames(design.matrix.lm)[1] <- as.character(conflict.intensity)
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ .')),   data = as.data.frame(design.matrix.glm), family = 'binomial')
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ .')), data = as.data.frame(design.matrix.lm))
  # logit.model <- glm(train[, conflict.boolean] ~ ., data = as.data.frame(design.matrix), family = 'binomial')
  # linear.model <- lm(train[, conflict.intensity] ~ ., data = as.data.frame(design.matrix))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold, pca = pca, vars = vars)
  model.output <- list()
  if (compute.metrics) { 
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity],
                                predicted.y   = predictions.list$predictions,
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                
  } else {
    warning('No metrics computed')
  }
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

PrincipalComponentsByRiskArea <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: principal component logistic and linear regression
  # Features/Variables: principal components of all original variables up to cumulative proportion of ~ .80,
  #   which corresponds to 8 components with a cumulative proportion of .815
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }  
  # Risk areas:
  #   Political: REG_U, REG_P2, GOV_EFF, REPRESS, EMPOWER
  #   Social: ETHNIC_NP/ETHNIC_SN, DISPER, HOMIC, MORT
  #   Security: CON_INT, CON_TREND, YRS_HVC, CON_NB
  #   Geography: WATER, FUEL_EXP, POP, STRUCT
  #   Economy: GDP_CAP, INEQ, FOOD, ECON_ISO
  pca.political <- prcomp(as.matrix(cbind(train[, 'REG_U'],   train[, 'REG_P2'],    train[, 'GOV_EFF'],
                                          train[, 'REPRESS'], train[, 'EMPOWER'])))
  pca.social    <- prcomp(as.matrix(cbind(train[, ethnic],    train[, 'DISPER'],    train[, 'HOMIC'],
                                          train[, 'MORT'])))
  pca.security  <- prcomp(as.matrix(cbind(train[, 'CON_INT'], train[, 'CON_TREND'], train[, 'YRS_HVC'],
                                          train[, 'CON_NB'])))
  pca.geography <- prcomp(as.matrix(cbind(train[, 'WATER'],   train[, 'FUEL_EXP'],  train[, 'POP'],
                                          train[, 'STRUCT'])))
  pca.economy   <- prcomp(as.matrix(cbind(train[, 'GDP_CAP'], train[, 'INEQ'],      train[, 'FOOD'],
                                          train[, 'ECON_ISO'])))
  pca.list <- list()
  pca.list[['pca.political']] <- pca.political
  pca.list[['pca.social']]    <- pca.social
  pca.list[['pca.security']]  <- pca.security
  pca.list[['pca.geography']] <- pca.geography
  pca.list[['pca.economy']]   <- pca.economy  
  # Choices for retaining principal components:
  #   Political: 3, with cumulative proportion of .8816
  #   Social:    3, with cumulative proportion of .9278 (nat. power) and .8718 (subnat.)
  #   Security:  2, with cumulative proportion of .8963
  #   Geography: 3, with cumulative proportion of .8961
  #   Economy:   3, with cumulative proportion of .9294
  design.matrix <- cbind(pca.political$x[, 1:3], pca.social$x[, 1:3], pca.security$x[, 1:2], 
                         pca.geography$x[, 1:3], pca.economy$x[, 1:3])
  logit.model <- glm(train[, conflict.boolean] ~ ., data = as.data.frame(design.matrix), family = 'binomial')
  linear.model <- lm(train[, conflict.intensity] ~ ., data = as.data.frame(design.matrix))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model,
                                         new.data = test, threshold = threshold, pca = pca.list)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity],
                                predicted.y   = predictions.list$predictions,
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                
  } else { 
    warning('No metrics computed')
  }  
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

POPandGDP_CAP <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + POP:GDP_CAP', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + POP:GDP_CAP', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

STRUCTandGDP_CAP <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + STRUCT:GDP_CAP', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + STRUCT:GDP_CAP', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

ETHNICandREG_U <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + ', ethnic, ':REG_U', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + ', ethnic, ':REG_U', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

ETHNICandREG_P2 <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + ', ethnic, ':REG_P2', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + ', ethnic, ':REG_P2', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

INEQandGDP_CAP <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + INEQ:GDP_CAP', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + INEQ:GDP_CAP', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

FUEL_EXPandGDP_CAP <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + FUEL_EXP:GDP_CAP', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + FUEL_EXP:GDP_CAP', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

FUEL_EXPandREG_U <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + FUEL_EXP:REG_U', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + FUEL_EXP:REG_U', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

FUEL_EXPandREG_P2 <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + FUEL_EXP:REG_P2', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + FUEL_EXP:REG_P2', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

FUEL_EXPandECON_ISO <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + FUEL_EXP:ECON_ISO', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + FUEL_EXP:ECON_ISO', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

GDP_CAPsquared <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + I(GDP_CAP^2)', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + I(GDP_CAP^2)', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

REG_P2squared <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + I(REG_P2^2)', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + I(REG_P2^2)', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

GDP_CAPandDISPER <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + GDP_CAP:DISPER', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + GDP_CAP:DISPER', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}


#===========================================================================================================================#
# Models below are identical to models above except that they use new variables, too (currently UNEMP, YOUTHB)              #
#===========================================================================================================================#

BaseModelNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ_SWIID  + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBMALE + CORRUPT', sep = '')), 
                                data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ_SWIID    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBMALE + CORRUPT', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

BaseModelNewVarsBoth <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ_SWIID  + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBBOTH + CORRUPT', sep = '')), 
                                data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ_SWIID    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBBOTH + CORRUPT', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

InteractionNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ_SWIID  + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBMALE + CORRUPT + 
                                FUEL_EXP:CORRUPT + YOUTHBMALE:UNEMP', sep = '')), 
                                data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ_SWIID    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBMALE + CORRUPT + 
                                FUEL_EXP:CORRUPT + YOUTHBMALE:UNEMP', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

InteractionNewVarsBoth <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ_SWIID  + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBBOTH + CORRUPT + 
                                FUEL_EXP:CORRUPT + YOUTHBBOTH:UNEMP', sep = '')), 
                                data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ_SWIID    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBBOTH + CORRUPT + 
                                FUEL_EXP:CORRUPT + YOUTHBBOTH:UNEMP', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

HighlyViolentInteractionNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'HVC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'HVC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ_SWIID    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBMALE + CORRUPT + 
                                FUEL_EXP:CORRUPT + YOUTHBMALE:UNEMP', sep = '')), 
                                data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ_SWIID    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBMALE + CORRUPT + 
                                FUEL_EXP:CORRUPT + YOUTHBMALE:UNEMP', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y     = test[, conflict.intensity], 
                                predicted.y    = predictions.list$predictions,  
                                probabilities  = predictions.list$probabilities,
                                highly.violent = TRUE)
    model.output[['metrics']] <- metrics
  } else {                                        
    warning('No metrics computed')                                         
  }                                       
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

HighlyViolentInteractionNewVarsBoth <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'HVC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'HVC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ_SWIID    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBBOTH + CORRUPT + 
                                FUEL_EXP:CORRUPT + YOUTHBBOTH:UNEMP', sep = '')), 
                                data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ_SWIID    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBBOTH + CORRUPT + 
                                FUEL_EXP:CORRUPT + YOUTHBBOTH:UNEMP', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y     = test[, conflict.intensity], 
                                predicted.y    = predictions.list$predictions,  
                                probabilities  = predictions.list$probabilities,
                                highly.violent = TRUE)
    model.output[['metrics']] <- metrics
  } else {                                        
    warning('No metrics computed')                                         
  }                                       
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

AllInteractionsNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: all
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT +     + UNEMP      + YOUTHBMALE + 
                                POP:GDP_CAP + STRUCT:GDP_CAP + ', ethnic, ':REG_U + ', ethnic, ':REG_P2 + 
                                INEQ:GDP_CAP + FUEL_EXP:GDP_CAP + FUEL_EXP:REG_U + FUEL_EXP:REG_P2 + 
                                FUEL_EXP:ECON_ISO + I(GDP_CAP^2) + I(REG_P2^2) + GDP_CAP:DISPER', sep = '')), data = train, family = 'binomial')
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT +     + UNEMP      + YOUTHBMALE + 
                                POP:GDP_CAP + STRUCT:GDP_CAP + ', ethnic, ':REG_U + ', ethnic, ':REG_P2 + 
                                INEQ:GDP_CAP + FUEL_EXP:GDP_CAP + FUEL_EXP:REG_U + FUEL_EXP:REG_P2 + 
                                FUEL_EXP:ECON_ISO + I(GDP_CAP^2) + I(REG_P2^2) + GDP_CAP:DISPER', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))              
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity],
                                predicted.y   = predictions.list$predictions,
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics
  } else {
    warning('No metrics computed')
  }
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

RandomEffectsRegionsNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: 'REGIONS' variable
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glmer(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBMALE + (1 | REGIONS)', sep = '')), 
                                data = train, family = 'binomial')
  linear.model <- lmer(as.formula(paste(conflict.intensity, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBMALE + (1 | REGIONS)', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))     
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity],
                                predicted.y   = predictions.list$predictions,
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics  
  } else {
    warning('No metrics computed')
  }
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

InteractionsAndRandomEffectsRegionsNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: all
  # Random factors: all
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glmer(as.formula(paste(conflict.boolean, '~ POP  * GDP_CAP  + STRUCT       * GDP_CAP      + ', ethnic, ' * REG_U  + ',
                                  ethnic, ' * REG_P2  + INEQ      * GDP_CAP  + FUEL_EXP     * GDP_CAP      + FUEL_EXP     * REG_U  + 
                                  FUEL_EXP  * REG_P2  + FUEL_EXP  * ECON_ISO + I(GDP_CAP^2) + I(REG_P2^2)  + GDP_CAP      * DISPER + 
                                  GOV_EFF   + EMPOWER + REPRESS   + CON_NB   + YRS_HVC      + CON_TREND    + CON_INT      + MORT   + 
                                  DISPER    + HOMIC   + FOOD      + WATER    + UNEMP        + YOUTHBMALE   + (1 | REGIONS)', 
                                  sep = '')), data = train, family = 'binomial', nAGQ = 10,
                                  control = glmerControl(optimizer = 'bobyqa', check.conv.singular = 'warning',
                                            optCtrl = list(maxfun = 10000)))
  linear.model <- lmer(as.formula(paste(conflict.intensity, '~ POP * GDP_CAP  + STRUCT       * GDP_CAP      + ', ethnic, ' * REG_U  + ',
                                  ethnic, ' * REG_P2  + INEQ       * GDP_CAP  + FUEL_EXP     * GDP_CAP      + FUEL_EXP     * REG_U  + 
                                  FUEL_EXP  * REG_P2  + FUEL_EXP   * ECON_ISO + I(GDP_CAP^2) + I(REG_P2^2)  + GDP_CAP      * DISPER + 
                                  GOV_EFF   + EMPOWER + REPRESS    + CON_NB   + YRS_HVC      + CON_TREND    + CON_INT      + MORT   + 
                                  DISPER    + HOMIC   + FOOD       + WATER    + UNEMP        + YOUTHBMALE    + (1 | REGIONS)', 
                                  sep = '')), data = subset(train, train[, conflict.boolean] == 1),
                                  control = lmerControl(optimizer = 'bobyqa', check.conv.singular = 'warning'))              
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity],
                                predicted.y   = predictions.list$predictions,
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics
  } else {
    warning('No metrics computed')
  }
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

ZeroinflatedNegativeBinomialNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: zero-inflated negative binomial regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  }
  train[, conflict.intensity] <- as.integer(train[, conflict.intensity])
  zeroinfl.model <- zeroinfl(as.formula(paste(conflict.intensity, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBMALE', sep = '')), data = train, dist = 'negbin', EM = TRUE)
  predictions.list <- computePredictions(model.one = zeroinfl.model, new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y  = test[, conflict.intensity],
                                predicted.y = predictions.list$predictions)
    model.output[['metrics']] <- metrics                                         
  } else {
    warning('No metrics computed')
  }
  model.output[['zeroinfl.model']]   <- zeroinfl.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

ZeroinflAndInteractionsNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: zero-inflated negative binomial regression
  # Features/Variables: all
  # Interactions: all
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  }
  train[, conflict.intensity] <- as.integer(train[, conflict.intensity])
  zeroinfl.model <- zeroinfl(as.formula(paste(conflict.intensity, '~ POP     * GDP_CAP      + STRUCT       * GDP_CAP      + ', ethnic, ' * 
                                REG_U  + ', ethnic, ' * REG_P2    + INEQ     * GDP_CAP      + FUEL_EXP     * GDP_CAP      + FUEL_EXP     * 
                                REG_U  + FUEL_EXP     * REG_P2    + FUEL_EXP * ECON_ISO     + I(GDP_CAP^2) + I(REG_P2^2)  + GDP_CAP      * 
                                DISPER + GOV_EFF      + EMPOWER   + REPRESS  + CON_NB       + YRS_HVC      + CON_TREND    + CON_INT      + 
                                MORT   + DISPER       + HOMIC     + FOOD     + WATER    + UNEMP      + YOUTHBMALE', 
                                sep = '')), data = train, dist = 'negbin', EM = TRUE)
  predictions.list <- computePredictions(model.one = zeroinfl.model, new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y  = test[, conflict.intensity],
                                predicted.y = predictions.list$predictions)
    model.output[['metrics']] <- metrics                                         
  } else {
    warning('No metrics computed')
  }  
  model.output[['zeroinfl.model']]   <- zeroinfl.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

HurdleModelNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Helpful: http://stats.stackexchange.com/questions/60643/difference-between-binomial-negative-binomial-and-poisson-regression
  # Helpful: http://stats.stackexchange.com/questions/81457/what-is-the-difference-between-zero-inflated-and-hurdle-distributions-models
  # Statistical model: hurdle
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  }
  train[, conflict.intensity] <- as.integer(train[, conflict.intensity])
  hurdle.model <- hurdle(as.formula(paste(conflict.intensity, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT    + UNEMP      + YOUTHBMALE', sep = '')), data = train)
  predictions.list <- computePredictions(model.one = hurdle.model, new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y  = test[, conflict.intensity],
                                predicted.y = predictions.list$predictions)
    model.output[['metrics']] <- metrics                                         
  } else {
    warning('No metrics computed')
  }    
  model.output[['hurdle.model']]     <- hurdle.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

HurdleInteractionsNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: hurdle
  # Features/Variables: all
  # Interactions: all
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  }
  train[, conflict.intensity] <- as.integer(train[, conflict.intensity])
  hurdle.model <- hurdle(as.formula(paste(conflict.intensity, '~ POP       * GDP_CAP      + STRUCT      * GDP_CAP   + ', ethnic, ' * REG_U  + ',
                                ethnic, ' * REG_P2  + INEQ      * GDP_CAP  + FUEL_EXP     * GDP_CAP     + FUEL_EXP  * REG_U  + 
                                FUEL_EXP  * REG_P2  + FUEL_EXP  * ECON_ISO + I(GDP_CAP^2) + I(REG_P2^2) + GDP_CAP   * DISPER + 
                                GOV_EFF   + EMPOWER + REPRESS   + CON_NB   + YRS_HVC      + CON_TREND   + CON_INT   + MORT   + 
                                DISPER    + HOMIC   + FOOD      + WATER    + UNEMP      + YOUTHBMALE', sep = '')), data = train)
  predictions.list <- computePredictions(model.one = hurdle.model, new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y  = test[, conflict.intensity],
                                predicted.y = predictions.list$predictions)
    model.output[['metrics']] <- metrics                                         
  } else {
    warning('No metrics computed')
  }
  model.output[['hurdle.model']]     <- hurdle.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

LassoMinNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  stop('LassoMin: do not yet use this function/model')
  # Statistical model: generalized linear model with lasso regularisation
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  # Note: 'glmnet' is a generalisation of the lasso; said differently, lasso is a special case
  #   of the elastic net (glmnet) dependent on the regularisation parameter lambda
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  design.matrix <- as.matrix(cbind(train[, 'REG_U'],   train[, 'REG_P2'],  train[, 'GOV_EFF'],  train[, 'EMPOWER'], 
                                   train[, 'REPRESS'], train[, 'CON_NB'],  train[, 'YRS_HVC'],  train[, 'CON_TREND'],
                                   train[, 'CON_INT'], train[, 'MORT'],    train[, 'DISPER'],   train[, 'HOMIC'],
                                   train[, ethnic],    train[, 'GDP_CAP'], train[, 'ECON_ISO'], train[, 'INEQ'],
                                   train[, 'FOOD'],    train[, 'POP'],     train[, 'WATER'],    train[, 'FUEL_EXP'],
                                   train[, 'STRUCT'],  train[, 'UNEMP'],   train[, 'YOUTHBMALE']))
  cv.lasso.model <- cv.glmnet(x = design.matrix, y = train[, conflict.intensity], family = 'gaussian')
  newx.matrix <- as.matrix(cbind(test[, 'REG_U'],   test[, 'REG_P2'],  test[, 'GOV_EFF'],  test[, 'EMPOWER'], 
                                 test[, 'REPRESS'], test[, 'CON_NB'],  test[, 'YRS_HVC'],  test[, 'CON_TREND'],
                                 test[, 'CON_INT'], test[, 'MORT'],    test[, 'DISPER'],   test[, 'HOMIC'],
                                 test[, ethnic],    test[, 'GDP_CAP'], test[, 'ECON_ISO'], test[, 'INEQ'],
                                 test[, 'FOOD'],    test[, 'POP'],     test[, 'WATER'],    test[, 'FUEL_EXP'],
                                 test[, 'STRUCT'],  test[, 'UNEMP'],   test[, 'YOUTHBMALE']))
  predictions.list <- computePredictions(model.one = cv.lasso.model, new.data = newx.matrix, 
                                         threshold = NULL, s = 'lambda.min')
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y  <- test[, conflict.intensity],
                                predicted.y <- predictions.list$predictions)
    model.output[['metrics']] <- metrics                                    
  } else {
    warning('No metrics computed')
  }
  model.output[['cv.lasso.model']]   <- cv.lasso.model                                   
  model.output[['predictions.list']] <- predictions.list                                                                      
  return(model.output)
}

Lasso1seNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  stop('Lasso1se: do not yet use this function/model')
  # Statistical model: generalized linear model with lasso regularisation
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  design.matrix <- as.matrix(cbind(train[, 'REG_U'],   train[, 'REG_P2'],  train[, 'GOV_EFF'],  train[, 'EMPOWER'], 
                                   train[, 'REPRESS'], train[, 'CON_NB'],  train[, 'YRS_HVC'],  train[, 'CON_TREND'],
                                   train[, 'CON_INT'], train[, 'MORT'],    train[, 'DISPER'],   train[, 'HOMIC'],
                                   train[, ethnic],    train[, 'GDP_CAP'], train[, 'ECON_ISO'], train[, 'INEQ'],
                                   train[, 'FOOD'],    train[, 'POP'],     train[, 'WATER'],    train[, 'FUEL_EXP'],
                                   train[, 'STRUCT'],  train[, 'UNEMP'],   train[, 'YOUTHBMALE']))
  cv.lasso.model <- cv.glmnet(x = design.matrix, y = train[, conflict.intensity], family = 'gaussian')
  newx.matrix <- as.matrix(cbind(test[, 'REG_U'],   test[, 'REG_P2'],  test[, 'GOV_EFF'],  test[, 'EMPOWER'], 
                                 test[, 'REPRESS'], test[, 'CON_NB'],  test[, 'YRS_HVC'],  test[, 'CON_TREND'],
                                 test[, 'CON_INT'], test[, 'MORT'],    test[, 'DISPER'],   test[, 'HOMIC'],
                                 test[, ethnic],    test[, 'GDP_CAP'], test[, 'ECON_ISO'], test[, 'INEQ'],
                                 test[, 'FOOD'],    test[, 'POP'],     test[, 'WATER'],    test[, 'FUEL_EXP'],
                                 test[, 'STRUCT'],  test[, 'UNEMP'],   test[, 'YOUTHBMALE']))
  predictions.list <- computePredictions(model.one = cv.lasso.model, new.data = newx.matrix, 
                                         threshold = NULL, s = 'lambda.1se')
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y  <- test[, conflict.intensity],
                                predicted.y <- predictions.list$predictions)
    model.output[['metrics']] <- metrics                                    
  } else {
    warning('No metrics computed')
  }
  model.output[['cv.lasso.model']]   <- cv.lasso.model                                   
  model.output[['predictions.list']] <- predictions.list                                                                      
  return(model.output)
}

LassoTwoModelsMinNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  stop('LassoTwoModelsMin: do not yet use this function/model')
  # Statistical model: generalized linear model with lasso regularisation
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }  
  design.matrix <- as.matrix(cbind(train[, 'REG_U'],   train[, 'REG_P2'],  train[, 'GOV_EFF'],  train[, 'EMPOWER'], 
                                   train[, 'REPRESS'], train[, 'CON_NB'],  train[, 'YRS_HVC'],  train[, 'CON_TREND'],
                                   train[, 'CON_INT'], train[, 'MORT'],    train[, 'DISPER'],   train[, 'HOMIC'],
                                   train[, ethnic],    train[, 'GDP_CAP'], train[, 'ECON_ISO'], train[, 'INEQ'],
                                   train[, 'FOOD'],    train[, 'POP'],     train[, 'WATER'],    train[, 'FUEL_EXP'],
                                   train[, 'STRUCT'],  train[, 'UNEMP'],   train[,' YOUTHBMALE']))
  newx.matrix <- as.matrix(cbind(test[, 'REG_U'],   test[, 'REG_P2'],  test[, 'GOV_EFF'],  test[, 'EMPOWER'], 
                                 test[, 'REPRESS'], test[, 'CON_NB'],  test[, 'YRS_HVC'],  test[, 'CON_TREND'],
                                 test[, 'CON_INT'], test[, 'MORT'],    test[, 'DISPER'],   test[, 'HOMIC'],
                                 test[, ethnic],    test[, 'GDP_CAP'], test[, 'ECON_ISO'], test[, 'INEQ'],
                                 test[, 'FOOD'],    test[, 'POP'],     test[, 'WATER'],    test[, 'FUEL_EXP'],
                                 test[, 'STRUCT'],  test[, 'UNEMP'],   test[, 'YOUTHBMALE'],   test[, conflict.boolean]))
                                 # Attention: conflict.boolean HAS to be last column 
  cv.lasso.binomial.model <- cv.glmnet(x = design.matrix, y = as.factor(train[, conflict.boolean]), family = 'binomial')
  cv.lasso.gaussian.model <- cv.glmnet(x = design.matrix, y = train[, conflict.intensity], family = 'gaussian')
  predictions.list <- computePredictions(model.one = cv.lasso.binomial.model, model.two = cv.lasso.gaussian.model,
                                         new.data = newx.matrix, threshold = threshold, s = 'lambda.min')
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    <- test[, conflict.intensity],
                                predicted.y   <- predictions.list$predictions,
                                probabilities <- predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                
  } else {
    warning('No metrics computed')                              
  }                               
  model.output[['cv.lasso.binomial.model']] <- cv.lasso.binomial.model
  model.output[['cv.lasso.gaussian.model']] <- cv.lasso.gaussian.model
  model.output[['predictions.list']]        <- predictions.list
  return(model.output)                                 
}

LassoTwoModels1seNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  stop('LassoTwoModels1se: do not yet use this function/model')
  # Statistical model: generalized linear model with lasso regularisation
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }  
  design.matrix <- as.matrix(cbind(train[, 'REG_U'],   train[, 'REG_P2'],  train[, 'GOV_EFF'],  train[, 'EMPOWER'], 
                                   train[, 'REPRESS'], train[, 'CON_NB'],  train[, 'YRS_HVC'],  train[, 'CON_TREND'],
                                   train[, 'CON_INT'], train[, 'MORT'],    train[, 'DISPER'],   train[, 'HOMIC'],
                                   train[, ethnic],    train[, 'GDP_CAP'], train[, 'ECON_ISO'], train[, 'INEQ'],
                                   train[, 'FOOD'],    train[, 'POP'],     train[, 'WATER'],    train[, 'FUEL_EXP'],
                                   train[, 'STRUCT'],  train[, 'UNEMP'],   train[, 'YOUTHBMALE']))
  newx.matrix <- as.matrix(cbind(test[, 'REG_U'],   test[, 'REG_P2'],  test[, 'GOV_EFF'],  test[, 'EMPOWER'], 
                                 test[, 'REPRESS'], test[, 'CON_NB'],  test[, 'YRS_HVC'],  test[, 'CON_TREND'],
                                 test[, 'CON_INT'], test[, 'MORT'],    test[, 'DISPER'],   test[, 'HOMIC'],
                                 test[, ethnic],    test[, 'GDP_CAP'], test[, 'ECON_ISO'], test[, 'INEQ'],
                                 test[, 'FOOD'],    test[, 'POP'],     test[, 'WATER'],    test[, 'FUEL_EXP'],
                                 test[, 'STRUCT'],  test[, 'UNEMP'],   test[, 'YOUTHBMALE'],   test[, conflict.boolean]))
                                 # Attention: conflict.boolean HAS to be last column 
  cv.lasso.binomial.model <- cv.glmnet(x = design.matrix, y = as.factor(train[, conflict.boolean]), family = 'binomial')
  cv.lasso.gaussian.model <- cv.glmnet(x = design.matrix, y = train[, conflict.intensity], family = 'gaussian')
  predictions.list <- computePredictions(model.one = cv.lasso.binomial.model, model.two = cv.lasso.gaussian.model,
                                         new.data = newx.matrix, threshold = threshold, s = 'lambda.1se')
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    <- test[, conflict.intensity],
                                predicted.y   <- predictions.list$predictions,
                                probabilities <- predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                
  } else {
    warning('No metrics computed')                              
  }                               
  model.output[['cv.lasso.binomial.model']] <- cv.lasso.binomial.model
  model.output[['cv.lasso.gaussian.model']] <- cv.lasso.gaussian.model
  model.output[['predictions.list']]        <- predictions.list
  return(model.output)                                 
}

PrincipalComponentsNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: principal component logistic and linear regression
  # Features/Variables: principal components of all original variables up to cumulative proportion of ~ .80,
  #   which corresponds to 8 components with a cumulative proportion of .815
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train
    warning('No cross-validation: train and test datasets are the same')
  }
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }  
  vars <- c('REG_U', 'REG_P2', 'GOV_EFF', 'EMPOWER', 'REPRESS', 'CON_NB', 'YRS_HVC', 'CON_TREND',
            'CON_INT', 'MORT', 'DISPER', 'HOMIC', ethnic, 'GDP_CAP', 'ECON_ISO', 'INEQ_SWIID', 
            'FOOD', 'POP', 'WATER', 'FUEL_EXP', 'STRUCT', 'UNEMP', 'YOUTHBMALE', 'CORRUPT')
  # pca <- prcomp(as.matrix(cbind(train[, 'REG_U'],   train[, 'REG_P2'],  train[, 'GOV_EFF'],  train[, 'EMPOWER'], 
                                # train[, 'REPRESS'], train[, 'CON_NB'],  train[, 'YRS_HVC'],  train[, 'CON_TREND'],
                                # train[, 'CON_INT'], train[, 'MORT'],    train[, 'DISPER'],   train[, 'HOMIC'],
                                # train[, ethnic],    train[, 'GDP_CAP'], train[, 'ECON_ISO'], train[, 'INEQ'],
                                # train[, 'FOOD'],    train[, 'POP'],     train[, 'WATER'],    train[, 'FUEL_EXP'],
                                # train[, 'STRUCT'],  train[, 'UNEMP'],   train[, 'YOUTHBMALE'])))
  pca <- prcomp(train[, vars])                                
  design.matrix.glm <- cbind(train[, conflict.boolean], pca$x[, 1:9])
  colnames(design.matrix.glm)[1] <- as.character(conflict.boolean)
  design.matrix.lm  <- cbind(train[, conflict.intensity], pca$x[, 1:9])
  colnames(design.matrix.lm)[1] <- as.character(conflict.intensity)
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ .')),   data = as.data.frame(design.matrix.glm), family = 'binomial')
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ .')), data = as.data.frame(design.matrix.lm))  
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold, pca = pca, vars = vars)
  model.output <- list()
  if (compute.metrics) { 
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity],
                                predicted.y   = predictions.list$predictions,
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                
  } else {
    warning('No metrics computed')
  }
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list
  return(model.output)
}

POPandGDP_CAPNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + POP:GDP_CAP', sep = '')), 
                                data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + POP:GDP_CAP', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

STRUCTandGDP_CAPNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + STRUCT:GDP_CAP', sep = '')), 
                                data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + STRUCT:GDP_CAP', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

ETHNICandREG_UNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + ', ethnic, ':REG_U', sep = '')), 
                                data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + ', ethnic, ':REG_U', sep = '')), 
                                data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

ETHNICandREG_P2NewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + ', ethnic, ':REG_P2', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + ', ethnic, ':REG_P2', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

INEQandGDP_CAPNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + INEQ:GDP_CAP', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + INEQ:GDP_CAP', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

FUEL_EXPandGDP_CAPNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + FUEL_EXP:GDP_CAP', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + FUEL_EXP:GDP_CAP', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

FUEL_EXPandREG_UNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + FUEL_EXP:REG_U', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + FUEL_EXP:REG_U', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

FUEL_EXPandREG_P2NewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + FUEL_EXP:REG_P2', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + FUEL_EXP:REG_P2', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

FUEL_EXPandECON_ISONewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + FUEL_EXP:ECON_ISO', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + FUEL_EXP:ECON_ISO', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

GDP_CAPsquaredNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + I(GDP_CAP^2)', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + I(GDP_CAP^2)', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

REG_P2squaredNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + I(REG_P2^2)', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + I(REG_P2^2)', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}

GDP_CAPandDISPERNewVarsMale <- function(train, test = NULL, threshold, conflict.dimension, compute.metrics) {
  # Statistical model: logistic and linear regression
  # Features/Variables: all
  # Interactions: none
  # Random factors: none
  if (is.null(test)) {
    test <- train     
    warning('No cross-validation: train and test datasets are the same')
  }
  # The conflict.dimension variable determines which dependent variables and dimension-specific ETHNIC 
  # variable are to be used in the regression models below. This way was chosen to avoid duplicating the 
  # model specification for each conflict.dimension
  if (tolower(conflict.dimension) == 'np') {
    conflict.intensity <- 'Intensity_Y4_NP'
    conflict.boolean   <- 'VC_Y4_NP'
    ethnic             <- 'ETHNIC_NP'
  } else if (tolower(conflict.dimension) == 'sn') {
    conflict.intensity <- 'Intensity_Y4_SN'
    conflict.boolean   <- 'VC_Y4_SN'
    ethnic             <- 'ETHNIC_SN'
  } else {
    stop('conflict.dimension must either be national power (\'np\') or subnational (\'sn\')')
  }
  logit.model <- glm(as.formula(paste(conflict.boolean, '~ REG_U   + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   + 
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + GDP_CAP:DISPER', sep = '')), data = train, family = 'binomial')                             
  linear.model <- lm(as.formula(paste(conflict.intensity, '~ REG_U + REG_P2  + GOV_EFF + EMPOWER + REPRESS + 
                                CON_NB    + YRS_HVC   + CON_TREND  + CON_INT + MORT    + DISPER  + HOMIC   + ', 
                                ethnic, ' + GDP_CAP   + ECON_ISO   + INEQ    + FOOD    + POP     + WATER   +
                                FUEL_EXP  + STRUCT + UNEMP + YOUTHBMALE + GDP_CAP:DISPER', sep = '')), data = subset(train, train[, conflict.boolean] == 1))
  predictions.list <- computePredictions(model.one = logit.model, model.two = linear.model, 
                                         new.data = test, threshold = threshold)
  model.output <- list()
  if (compute.metrics) {
    metrics <- calculateMetrics(observed.y    = test[, conflict.intensity], 
                                predicted.y   = predictions.list$predictions,  
                                probabilities = predictions.list$probabilities)
    model.output[['metrics']] <- metrics                                         
  } else {  
    warning('No metrics computed')                                         
  }                                          
  model.output[['logit.model']]      <- logit.model
  model.output[['linear.model']]     <- linear.model
  model.output[['predictions.list']] <- predictions.list  
  return(model.output)
}
























