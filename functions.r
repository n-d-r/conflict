#===========================================================================================================================#
# Functions                                                                                                                 #
#---------------------------------------------------------------------------------------------------------------------------#
# This file contains various R functions to run models, calculate metrics, apply the models to new data, etc.				        #
#===========================================================================================================================#


#===========================================================================================================================#
# Preprocessing																									                                                      			#
#===========================================================================================================================#

switchColnames <- function(dat, mode = c('back', 'forth'), new.var = FALSE) {
  # Description:
  #   Just an auxiliary function to switch back and forth between column names as they are more verbose
  #   in the original input .csv file and less verbose in the model specifications
  # Input:
  #   dat: input data in the form of a data.frame with verbose or non-verbose column names
  #   mode: character string, either 'back' or 'forth', depending on the direction the switch
  #     should occur. 'forth' switches from verbose to non-verbose, and 'back' accordingly
  #     switches from non-verbose to verbose
  #   new.var: logical, whether or not new variables (UNEMP, YOUTHB) is part of the data set
  # Output:
  #   The input data.frame just with switched column names
	if (mode == 'forth') {
		colnames(dat)[which(names(dat) == 'GCRI.POL.REG_U')]     <- 'REG_U'
		colnames(dat)[which(names(dat) == 'GCRI.POL.REG_P2')]    <- 'REG_P2'
		colnames(dat)[which(names(dat) == 'GCRI.POL.GOV_EFF')]   <- 'GOV_EFF'
		colnames(dat)[which(names(dat) == 'GCRI.POL.EMPOWER')]   <- 'EMPOWER'
		colnames(dat)[which(names(dat) == 'GCRI.POL.REPRESS')]   <- 'REPRESS'
		colnames(dat)[which(names(dat) == 'GCRI.SEC.CON_NB')]    <- 'CON_NB'
		colnames(dat)[which(names(dat) == 'GCRI.SEC.YRS_HVC')]   <- 'YRS_HVC'
		colnames(dat)[which(names(dat) == 'GCRI.SEC.CON_TREND')] <- 'CON_TREND'
		colnames(dat)[which(names(dat) == 'GCRI.SEC.CON_INT')]   <- 'CON_INT'
		colnames(dat)[which(names(dat) == 'GCRI.SOC.MORT')]      <- 'MORT'
		colnames(dat)[which(names(dat) == 'GCRI.SOC.DISPER')]    <- 'DISPER'
		colnames(dat)[which(names(dat) == 'GCRI.SOC.HOMIC')]     <- 'HOMIC'
		colnames(dat)[which(names(dat) == 'GCRI.SOC.ETHNIC_SN')] <- 'ETHNIC_SN'
		colnames(dat)[which(names(dat) == 'GCRI.SOC.ETHNIC_NP')] <- 'ETHNIC_NP'
		colnames(dat)[which(names(dat) == 'GCRI.ECO.GDP_CAP')]   <- 'GDP_CAP'
		colnames(dat)[which(names(dat) == 'GCRI.ECO.ECON_ISO')]  <- 'ECON_ISO'
		colnames(dat)[which(names(dat) == 'GCRI.ECO.INEQ')]      <- 'INEQ'
		colnames(dat)[which(names(dat) == 'GCRI.ECO.FOOD.ACC')]  <- 'FOOD'
		colnames(dat)[which(names(dat) == 'GCRI.STR.POP')]       <- 'POP'
		colnames(dat)[which(names(dat) == 'GCRI.STR.WATER')]     <- 'WATER'
		colnames(dat)[which(names(dat) == 'GCRI.STR.FUEL_EXP')]  <- 'FUEL_EXP'
		colnames(dat)[which(names(dat) == 'GCRI.STR.STRUCT')]    <- 'STRUCT'
    if (new.var) {
      colnames(dat)[which(names(dat) == 'GCRI.UNEMP')]       <- 'UNEMP'
      colnames(dat)[which(names(dat) == 'GCRI.YOUTHBMALE')]  <- 'YOUTHBMALE'      
    }
	} else if (mode == 'back') {
		colnames(dat)[which(names(dat) == 'REG_U')]         <- 'GCRI.POL.REG_U'
		colnames(dat)[which(names(dat) == 'REG_P2')]        <- 'GCRI.POL.REG_P2'
		colnames(dat)[which(names(dat) == 'GOV_EFF')]       <- 'GCRI.POL.GOV_EFF'
		colnames(dat)[which(names(dat) == 'EMPOWER')]       <- 'GCRI.POL.EMPOWER'
		colnames(dat)[which(names(dat) == 'REPRESS')]       <- 'GCRI.POL.REPRESS'
		colnames(dat)[which(names(dat) == 'CON_NB')]        <- 'GCRI.SEC.CON_NB'
		colnames(dat)[which(names(dat) == 'YRS_HVC')]       <- 'GCRI.SEC.YRS_HVC'
		colnames(dat)[which(names(dat) == 'CON_TREND')]     <- 'GCRI.SEC.CON_TREND'
		colnames(dat)[which(names(dat) == 'CON_INT')]       <- 'GCRI.SEC.CON_INT'
		colnames(dat)[which(names(dat) == 'MORT')]          <- 'GCRI.SOC.MORT'
		colnames(dat)[which(names(dat) == 'DISPER')]        <- 'GCRI.SOC.DISPER'
		colnames(dat)[which(names(dat) == 'HOMIC')]         <- 'GCRI.SOC.HOMIC'
		colnames(dat)[which(names(dat) == 'ETHNIC_SN')]     <- 'GCRI.SOC.ETHNIC_SN'
		colnames(dat)[which(names(dat) == 'ETHNIC_NP')]     <- 'GCRI.SOC.ETHNIC_NP'
		colnames(dat)[which(names(dat) == 'GDP_CAP')]       <- 'GCRI.ECO.GDP_CAP'
		colnames(dat)[which(names(dat) == 'ECON_ISO')]      <- 'GCRI.ECO.ECON_ISO'
		colnames(dat)[which(names(dat) == 'INEQ')]          <- 'GCRI.ECO.INEQ'
		colnames(dat)[which(names(dat) == 'FOOD')]          <- 'GCRI.ECO.FOOD.ACC'
		colnames(dat)[which(names(dat) == 'POP')]           <- 'GCRI.STR.POP'
		colnames(dat)[which(names(dat) == 'WATER')]         <- 'GCRI.STR.WATER'
		colnames(dat)[which(names(dat) == 'FUEL_EXP')]      <- 'GCRI.STR.FUEL_EXP'
		colnames(dat)[which(names(dat) == 'STRUCT')]        <- 'GCRI.STR.STRUCT'		
    if (new.var) {
      colnames(dat)[which(names(dat) == 'UNEMP')]       <- 'GCRI.UNEMP'
      colnames(dat)[which(names(dat) == 'YOUTHBMALE')]  <- 'GCRI.YOUTHBMALE'
    } 
	} else {
		stop('\'mode\' can only be \'back\' or \'forth\'')
	}
	return(dat)
}

addRegions <- function(dat) {
  # Description:
  #   Adds a new column to the input data.frame that represents a country's region according
  #   to EEAS classifications
  # Input:
  #   dat: a data.frame of (historical) variable data without a REGION column
  # Output:
  #   The input data.frame with an added column of REGION for each country-year
	all.regions <- read.csv('G:/Projects_Critech/DG_2014_EEAS_ConflictRiskIndex/Methodology/Model/Model 2015 January/countries_and_their_EEAS_regions.csv', row.names = 1)
	REGION <- rep('None', nrow(dat))
	dat <- cbind(dat, REGION)
	dat[, 'REGION'] <- as.character(dat[, 'REGION'])
	for (iso in as.character(unique(dat$ISO))) {
		dat[dat$ISO == iso, 'REGION'] <- as.character(all.regions[iso, ])
	}	
  return(dat)
}

computeOnset <- function(dat) {
  # Description:
  #   The main function to be called when computing whether a given country-year constitutes
  #   a conflict-year, peace-year, or onset-year. In the process, it calls the two helper functions
  #   called computeOnsetHelperOne() and computeOnsetHelperTwo(). This approach was chosen in order
  #   to use apply() instead of for-loops
  # Input:
  #   dat: a data.frame of (historical) data that is lacking columns denoting what type a specific
  #     country-year is, with 'type' referring to conflict-year, peace-year, or onset-year
  # Output:
  #   A new data.frame that is identical to the input data.frame plus four new columns to denote 
  #   the type of a country-year; one column for each intensity-type (Violent Conflict, and Highly
  #   Violent Conflict), and one for each conflict.dimension (national power, subnational), therefore
  #   four altogether
  country.years <- list()
  for (unique.iso in unique(dat$ISO)) {
    country.years[[unique.iso]] <- subset(dat, dat$ISO == unique.iso)
  }
  # country.years is now a named list where each element consists of all country-years for a single
  # ISO-code, e.g. the first element should contain all historical data years for Afghanistan;
  # This makes it possible to use lapply() below to apply the helper function to all elements of said list
  processed.country.years <- lapply(country.years, computeOnsetHelperOne)  
  output <- data.frame()
  for (unique.iso in names(country.years)) {
    output <- rbind(output, processed.country.years[[unique.iso]])
  }
  return(output)
}

computeOnsetHelperOne <- function(country.years) {
  # Description:
  #   The first helper function for computeOnset() that is called via lapply() for each element
  #   of the country.years named list. The function loops through all four combinations of conflict type
  #   (Violent, Highly Violent) and conflict.dimension (national power, subnational). It then creates
  #   a new data.frame 'temp.df' that consists just of two columns, the first of which contains the binary
  #   (1 or 0) flags for whether a conflict happened in that year, and the second of which initially
  #   only contains 'peace' in every row. It then calls the second helper function (see below) on that new 
  #   data.frame to compute in which rows the 'peace' has to be changed (if at all). The computeOnsetHelperTwo()
  #   function returns a new data.frame with two columns where the second one corresponds to the 'type'
  #   column. This function here then cbinds the 'type' column to the country.years it is currenly working
  #   on and returns it
  # Input:
  #   country.years: this is NOT the full named list from computeOnset(), but a single element of it. Meaning,
  #     at any time, computeOnsetHelperOne() only operates on country.years of a single country, e.g. 
  #     Afghanistan. The input data.frame should therefore have around 20 rows (for 20 years)
  # Output:
  #   A data.frame consisting of all the country-years for a single country, added with four new columns 
  #   where each new column represents the computed types with regards to a specific combination of 
  #   conflict type (Violent, Highly Violent) and dimension (national power, subnational)
  output <- country.years
  for (conflict.dimension in c('VC_Y_NP', 'VC_Y_SN', 'HVC_Y_NP', 'HVC_Y_SN')) {
    temp.df <- as.data.frame(cbind(country.years[, conflict.dimension], rep('peace', nrow(country.years))))
    temp.df <- computeOnsetHelperTwo(temp.df)
    output  <- cbind(output, temp.df[, 2])
    colnames(output)[length(colnames(output))] <- paste(conflict.dimension, '_type', sep = '')
  }
  return(output)
}

computeOnsetHelperTwo <- function(conflict.df) {
  # Description:
  #   This function directly computes whether a given input year (row) is a war-year, peace-year,
  #   or onset-year. To explain the logic, it is important to remember that currently, conflict data
  #   is shifted by one year. This means that the conflict data (intensity, binary, etc.) from 2003 
  #   is assigned to 2002. This was done because the hope is to use the 2002 data to predict/model the 
  #   risk for conflict in 2003. Additionally, 'onset' is currently defined as being the four years 
  #   before a conflict broke out. In combination with the shift in conflict data mentioned above,
  #   this means that, in a time series of five years where the binary (yes or no) conflict variable 
  #   looks like this: 0, 0, 0, 1, 1 (chronologically from left to right), the three 0s would be 
  #   classified as 'onset' years, PLUS the first 1 since this year, due to the shift in conflict data,
  #   actually was NOT yet in conflict, but is denoted as such. That way, we get four years of onset 
  #   one of each is already denoted as being in conflict due to the shift. 
  # Input: 
  #   conflict.df: a data.frame with two columns, the first of which is a binary variable for a specific
  #     conflict dimension and type, and the second of which is initially a character vector of 'peace'
  # Output:
  #   A data.frame with two columns, identical in meaning to the input data.frame, but with the second column
  #   now not only containing 'peace' in every row, but also 'war' and 'onset' where appropriate
  # Note: 'previous.year' refers to the year of the row above; chronologically, the 'previous.year' 
  #   therefore is the year AFTER the one currently under investigation
  conflict.df[, 2] <- ifelse(conflict.df[, 1] == 1, 'war', 'peace')   # Capturing war-years directly
  if (conflict.df[1, 1] == 1) {
    previous.year <- 'war'
  } else {
    previous.year <- 'peace'
  }
  for (row in 2:nrow(conflict.df)) {
    if (previous.year == 'war' & conflict.df[row, 1] == 0) {
      if (row + 2 <= nrow(conflict.df)) {
        conflict.df[(row - 1), 2] <- 'onset'
        conflict.df[row:(row + 2), 2] <- ifelse(conflict.df[row:(row + 2), 2] != 'war', 'onset', 'war')
      } else {
        conflict.df[(row - 1):nrow(conflict.df), 2] <- 'onset'
      }
      previous.year <- 'peace'
    }
    if (conflict.df[row, 1] == 1) {
      previous.year <- 'war'
    }
  }
  return(conflict.df)
}

#===========================================================================================================================#
# Metrics     																										                                                      		#
#===========================================================================================================================#

computePredictions <- function(model.one, model.two = NULL, new.data, threshold, s = NULL, pca = NULL, vars = NULL) {
  # Description:
  #   This function's main task is to call the sub-functions to compute predictions depending
  #   on the types of the input models
  # Input: 
  #   model.one: a statistical model, usually the result of one of the models/functions defined 
  #     in 'models.r'; this first one MUST be given. Currently, it can either be of type 'glm',
  #     'glmerMod', 'zeroinfl', 'cv.glmnet', or 'hurdle'
  #   model.two: a statistical model; this one is optional. It can either be of type 'lm', 'lmerMod',
  #     or 'cv.glmnet'; otherwise, it should be NULL (default).
  #   new.data: the new.data to be predicted with; data.frame
  #   threshold: the threshold to combine two models, if relevant; can be 'static', 'dynamic', or 
  #     numeric scalar if previously determined
  #   s: lambda regularisation parameter, see computePredictionsSingleLASSO()
  #   pca: an object of class 'prcomp', or a named list whose elements are objects of class 'prcomp'; 
  #     for details see computePredictionsPCAGLMandPCALM() 
  #   vars: only relevant for PCA-type models; list of variables to summarise with PCA
  # Output:
  #   A named list that is the output of one of the sub-functions called from computePredictions(). The
  #   elements of the list will depend on the type of statistical models input; usually, for the case
  #   in which two models are combined (e.g. logistic and linear regression), the output will contain
  #   'probabilities', which are the result of using predict() with a logistic model; 'intensities', which
  #   are the result of predict() with linear regression; and 'predictions', which combine probabilities 
  #   and intensities via a specified threshold that is either static or dynamic. In the cases in which
  #   only one statistical model is used (and model.two is therefore NULL), the named list will normally
  #   contain only 'predictions' which result from using predict() with whatever the statistical model was
  # Note:
  #   In cases where two statistical models are combined, the first one should always be the classifier one
  #   (e.g. logistic regression) and the second one the linear one (e.g. linear regression)
  if (class(model.one) == 'glm' && class(model.two) == 'lm') {
    if (is.null(pca)) {
      output.list <- computePredictionsGLMandLM(model.one = model.one, model.two = model.two,
                                                new.data = new.data, threshold = threshold)
    } else if (class(pca) == 'prcomp') {
      output.list <- computePredictionsPCAGLMandPCALM(model.one = model.one, model.two = model.two,
                                                new.data = new.data, threshold = threshold, pca = pca, vars = vars)
    } else if (class(pca) == 'list') {
      output.list <- computePredictionsPCAbyRiskAreaGLMandLM(model.one = model.one, model.two = model.two,
                                                new.data = new.data, threshold = threshold, pca = pca, vars = vars)
    } else {
      stop('something went entirely wrong')
    }    
  } else if (class(model.one) == 'glmerMod' && class(model.two) == 'lmerMod') {       # && is logical AND
    output.list <- computePredictionsGLMERMODandLMERMOD(model.one = model.one, model.two = model.two,
                                              new.data = new.data, threshold = threshold)
  } else if (class(model.one) == 'zeroinfl' || (class(model.one) == 'hurdle')) {      # || is logical OR
    output.list <- computePredictionsZEROINFLorHURDLE(model.one = model.one, new.data = new.data)
  } else if (class(model.one) == 'cv.glmnet' && is.null(model.two)) {
    output.list <- computePredictionsSingleLASSO(model.one = model.one, new.data = new.data, s = s)
  } else if (class(model.one) == 'cv.glmnet' && class(model.two) == 'cv.glmnet') {
    output.list <- computePredictionsTwoLASSO(model.one = model.one, model.two = model.two,
                                              new.data = new.data, s = s, threshold = threshold)
  } else {
    stop('invalid model class: must be either glm (+ lm), glmerMod + lmerMod, zeroinfl, or hurdle')
  }
  return(output.list)
}

computePredictionsGLMandLM <- function(model.one, model.two, new.data, threshold) {
  # Description: 
  #   This function is called by computePredictions() if the two models supplied are of types 'glm'
  #   and 'lm'
  # Input:
  #   model.one: a statistical model as output by glm()
  #   model.two: a statistical model as output by lm()
  #   new.data: a data.frame of new.data that should be used for predict()
  #   threshold: a variable denoting whether to use a static or dynamic threshold to 
  #     combine the two statistical mdoels. The threshold is used such that, if the 
  #     prediction for any particular data point has a predicted probability >= the threshold,
  #     the final prediction will be as predicted from the linear model, and zero otherwise.
  #     Can be character strings 'static' or 'dynamic', or previously determined numeric 
  #     value (see explanation below)
  # Output:
  #   A named list containing the elements 'probabilities' (between 0.0 and 1.0, from the logistic
  #   regression model prediction), 'intensities' (between 0 and 10, from the linear regression
  #   model prediction), and 'predictions' (between 0 and 10, the combination of the probabilities
  #   and intensities)
  probabilities <- predict(model.one, newdata = new.data, type = 'response')
  intensities   <- predict(model.two, newdata = new.data)
  intensities   <- ifelse(intensities > kMaximumIntensity, 10, 
                   ifelse(intensities < kMinimumIntensity, 0, intensities))
  output.list   <- list()  
  if (threshold == 'static') {
    predictions <- ifelse(probabilities >= kClassifierThreshold, intensities, 0)
    output.list[['threshold']] <- kClassifierThreshold
  } else if (threshold == 'dynamic') {
    dynamic.threshold <- determineThresholdDynamically(new.data[, as.character(model.one$formula[2])], probabilities)
    # model.one$formula[2] extracts the dependent variable in the model, e.g. VC_Y4_NP
    predictions <- ifelse(probabilities >= dynamic.threshold, intensities, 0)
    output.list[['threshold']] <- dynamic.threshold
  } else if (class(threshold) == 'numeric') {
    # This is the case if computePredictions() passes down a previously 
    # determined numeric value for the threshold, for example from a previous 
    # execution of the model with test data; specifically, this applies 
    # when the model is executed from within applyModels() with a dynamic
    # threshold; as the threshold cannot be dynamically determined with the 
    # most recent data the model is applied to via predict() since this most
    # recent data does not contain conflict data 
    predictions <- ifelse(probabilities >= threshold, intensities, 0)
    output.list[['threshold']] <- threshold
  } else {
    stop(paste('invalid \'threshold\': ', threshold, sep = ''))
  }
  output.list[['probabilities']] <- probabilities
  output.list[['intensities']]   <- intensities
  output.list[['predictions']]   <- predictions  
  return(output.list)
}

computePredictionsGLMERMODandLMERMOD <- function(model.one, model.two, new.data, threshold) {
  # Description:
  #   This function is identical to computePredictionsGLMandLM() with the difference of the 
  #   additional argument 'allow.new.levels = TRUE' in the predict() functions directly below
  probabilities <- predict(model.one, newdata = new.data, allow.new.levels = TRUE, type = 'response')
  intensities   <- predict(model.two, newdata = new.data, allow.new.levels = TRUE)
  intensities   <- ifelse(intensities > kMaximumIntensity, 10,
                   ifelse(intensities < kMinimumIntensity, 0, intensities))
  output.list   <- list()
  if (threshold == 'static') {
    predictions <- ifelse(probabilities >= kClassifierThreshold, intensities, 0)
    output.list[['threshold']] <- kClassifierThreshold
  } else if (threshold == 'dynamic') {
    dynamic.threshold <- determineThresholdDynamically(new.data[, as.character(formula(model.one)[2])], probabilities)
    predictions <- ifelse(probabilities >= dynamic.threshold, intensities, 0)
    output.list[['threshold']] <- dynamic.threshold
  } else if (class(threshold) == 'numeric') {
    # See above in computePredictionsGLMandLM()
    predictions <- ifelse(probabilities >= threshold, intensities, 0)
    output.list[['threshold']] <- threshold
  } else {
    stop(paste('invalid \'threshold\': ', threshold, sep = ''))
  }
  output.list[['probabilities']] <- probabilities
  output.list[['intensities']]   <- intensities
  output.list[['predictions']]   <- predictions
  return(output.list)
}

computePredictionsZEROINFLorHURDLE <- function(model.one, new.data) {
  # Description:
  #   Called from computePredictions() if model.one is either of class 'zeroinfl' or 'hurdle'
  #   and model.two is NULL; computes predictions simply by calling predict() with the new.data
  # Input:
  #   model.one: a statistical model output from either zeroinfl() or hurdle() from the pscl library
  #   new.data: data.frame containing new.data to be used by predict()
  # Output:
  #   A named list only containing one element called 'predictions' that contains the output from
  #   predict() with the input statistical model and new data
  predictions <- predict(model.one, newdata = new.data)
  predictions <- ifelse(predictions > kMaximumIntensity, 10, 
                 ifelse(predictions < kMinimumIntensity, 0, predictions))
  output.list <- list()
  output.list[['predictions']] <- predictions
  return(output.list)
}

computePredictionsSingleLASSO <- function(model.one, new.data, s) {
  # Description:
  #   Called from computePredictions() if model.one is of class 'cv.glmnet' and model.two is NULL;
  #   computes predictions simply by calling predict() with new.data
  # Input:
  #   model.one: a statistical model from cv.glmnet() from the glmnet library
  #   new.data: the new.data to be used in predict(); data.frame
  #   s: value of penalty parameter lambda for which predictions should be computed; must be
  #     either 'lambda.min' (for the value that results in minimum mean cross-validated error), 
  #     or 'lambda.1se' (largest value of lambda such that error is within 1 standard error of the 
  #     minimum); for more information see ?cv.glmnet and ?predict.cv.glmnet
  # Output:
  #   A named list whose only element 'predictions' contains the results from predict()
  if (is.null(s)) {
    stop('\'s\' parameter needed for predictions with lasso; choose \'lambda.min\' or \'lambda.1se\'')
  }
  output.list <- list()
  predictions <- predict(model.one, newx = new.data, s = s, type = 'response')  
  predictions <- ifelse(predictions > kMaximumIntensity, 10, 
                 ifelse(predictions < kMinimumIntensity, 0, predictions))  
  output.list[['predictions']] <- predictions
  return(output.list)
}
  
computePredictionsTwoLASSO <- function(model.one, model.two, new.data, s, threshold) {
  # Description:
  #   Called from computePredictions() if model.one and model.two are both of class 'cv.glmnet';
  #   computes predictions by calling predict() on both models, and then combining them via the
  #   threshold that is either static or dynamic
  # Input:
  #   model.one: a statistical model resultant from cv.glmnet(); needs to be of family 'binomial'
  #   model.two: a statistical model resultant from cv.glmnet(); needs to be of family 'gaussian'
  #   ATTENTION: does NOT check whether the families are correct!
  #   new.data: data.frame, contains new.data to be predicted with
  #   s: lambda parameter (see above in computePredictionsSingleLASSO()), can either be 'lambda.min'
  #     or 'lambda.1se'
  #   threshold: either character strings 'static' or 'dynamic', or previously determined numeric
  #     value (for details see computePredictionsGLMandLM() above)
  # Output:
  #   A named list containing the elements 'probabilities', 'intensities', and 'predictions'; for
  #   details, see above in computePredictionsGLMandLM()
  probabilities <- predict(model.one, newx = new.data[, 1:(ncol(new.data) - 1)], s = s, type = 'response')    # Fitted probabilities
  intensities   <- predict(model.two, newx = new.data[, 1:(ncol(new.data) - 1)], s = s, type = 'response')    # Fitted values
  intensities   <- ifelse(intensities > kMaximumIntensity, 10, 
                   ifelse(intensities < kMinimumIntensity, 0, intensities))
  output.list <- list()                     
  if (threshold == 'static') {
    predictions <- ifelse(probabilities >= kClassifierThreshold, intensities, 0)
    output.list[['threshold']] <- kClassifierThreshold
  } else if (threshold == 'dynamic') {
    dynamic.threshold <- determineThresholdDynamically(new.data[, ncol(new.data)], probabilities)
    predictions <- ifelse(probabilities >= dynamic.threshold, intensities, 0)
    output.list[['threshold']] <- dynamic.threshold
  } else if (class(threshold) == 'numeric') {
    # See above in computePredictionsGLMandLM()
    predictions <- ifelse(probabilities >= threshold, intensities, 0)
    output.list[['threshold']] <- threshold
  } else {
    stop(paste('invalid \'threshold\': ', threshold, sep = ''))
  }
  output.list[['probabilities']] <- probabilities
  output.list[['intensities']]   <- intensities
  output.list[['predictions']]   <- predictions
  return(output.list)
}  

computePredictionsPCAGLMandPCALM <- function(model.one, model.two, new.data, threshold, pca, vars) {
  # Description:
  #   Called from computePredictions() if model.one is of class 'glm' and model.two of class 'lm'
  #   AND the 'pca' variable is not NULL; first predicts the PCA scores of the most recent data,
  #   then uses them to predict() the intensities and probabilities
  # Input:
  #   model.one: statistical model as output by glm()
  #   model.two: statistical model as output by lm()
  #   new.data: a data.frame of new.data that should be used for predict()
  #   threshold: a variable denoting whether to use a static or dynamic threshold to 
  #     combine the two statistical mdoels. The threshold is used such that, if the 
  #     prediction for any particular data point has a predicted probability >= the threshold,
  #     the final prediction will be as predicted from the linear model, and zero otherwise.
  #     Can be character strings 'static' or 'dynamic', or previously determined numeric 
  #     value 
  # Output:
  #   A named list containing the elements 'probabilities' (between 0.0 and 1.0, from the logistic
  #   regression model prediction), 'intensities' (between 0 and 10, from the linear regression
  #   model prediction), and 'predictions' (between 0 and 10, the combination of the probabilities
  #   and intensities)
  if (as.character(model.one$formula[[2]]) == 'VC_Y4_NP' || as.character(model.one$formula[[2]]) == 'HVC_Y4_NP') {
    ethnic <- 'ETHNIC_NP'
  } else if (as.character(model.one$formula[[2]]) == 'VC_Y4_SN' || as.character(model.one$formula[[2]]) == 'HVC_Y4_SN') {
    ethnic <- 'ETHNIC_SN'
  } else {
    stop('computePredictionsPCAGLMandPCALM: something went wrong')
  } 
  # pca.data <- as.matrix(cbind(new.data[, 'REG_U'],   new.data[, 'REG_P2'],    new.data[, 'GOV_EFF'],
                              # new.data[, 'EMPOWER'], new.data[, 'REPRESS'],   new.data[, 'CON_NB'],
                              # new.data[, 'YRS_HVC'], new.data[, 'CON_TREND'], new.data[, 'CON_INT'],
                              # new.data[, 'MORT'],    new.data[, 'DISPER'],    new.data[, 'HOMIC'],
                              # new.data[, ethnic],    new.data[, 'GDP_CAP'],   new.data[, 'ECON_ISO'],
                              # new.data[, 'INEQ'],    new.data[, 'FOOD'],      new.data[, 'POP'], 
                              # new.data[, 'WATER'],   new.data[, 'FUEL_EXP'],  new.data[, 'STRUCT']))
  pca.data <- new.data[, vars]
  # The principal component scores have to be calculated for the rows in the most recent data set
  if (class(pca) == 'list') {
    stop('invalid \'pca\': must be object of class \'prcomp\'')
  }
  # It needs to be determined whether new variables are included or not since this has an influence
  # on how many principal components are needed to explain more than 80% of the variation in the 
  # original variables. For the dataset without the new variables, it currently requires 8 PCs 
  # whereas the dataset with the new variablse requires 9 PCs to cross this threshold of .8
  if ('CORRUPT' %in% vars || 'UNEMP' %in% vars || 'YOUTHBMALE' %in% vars || 'YOUTHBBOTH' %in% vars) {
    pca.scores <- predict(pca, newdata = pca.data)[, 1:9]
  } else {
    pca.scores <- predict(pca, newdata = pca.data)[, 1:8]
  }
  probabilities <- predict(model.one, newdata = as.data.frame(pca.scores), type = 'response')
  intensities   <- predict(model.two, newdata = as.data.frame(pca.scores))
  intensities   <- ifelse(intensities > kMaximumIntensity, 10,
                   ifelse(intensities < kMinimumIntensity, 0, intensities))
  output.list   <- list()
  if (threshold == 'static') {
    predictions <- ifelse(probabilities >= kClassifierThreshold, intensities, 0)
    output.list[['threshold']] <- kClassifierThreshold
  } else if (threshold == 'dynamic') {
    dynamic.threshold <- determineThresholdDynamically(new.data[, as.character(model.one$formula[2])], probabilities)
    predictions <- ifelse(probabilities >= dynamic.threshold, intensities, 0)
    output.list[['threshold']] <- dynamic.threshold
  } else if (class(threshold) == 'numeric') {
    predictions <- ifelse(probabilities >= threshold, intensities, 0)
    output.list[['threshold']] <- threshold
  } else {
    stop(paste('invalid \'threshold\': ', threshold, sep = ''))
  }   
  output.list[['probabilities']] <- probabilities
  output.list[['intensities']]   <- intensities
  output.list[['predictions']]   <- predictions
  return(output.list)
}

computePredictionsPCAbyRiskAreaGLMandLM <- function(model.one, model.two, new.data, threshold, pca, vars) {
# Description:
#   Called from computePredictions() if model.one is of class 'glm', model.two of class 'lm', and the 'pca'
#   argument is not NULL; first predicts the PCA scores for the different risk areas with the elements from
#   the named 'pca' list, then uses them to predict() the intensities and probabilities
# Input:
#   model.one: a statistical model as output by glm()
#   model.two: a statistical model as output by lm()
#   new.data: a data.frame of new.data that should be used for predict()
#   threshold: a variable denoting whether to use a static or dynamic threshold to 
#     combine the two statistical mdoels. The threshold is used such that, if the 
#     prediction for any particular data point has a predicted probability >= the threshold,
#     the final prediction will be as predicted from the linear model, and zero otherwise.
#     Can be character strings 'static' or 'dynamic', or previously determined numeric 
#     value 
#   pca: a list (not a single object of class 'prcomp') where each named element is an object of class
#     'prcomp', i.e. the output from the prcomp() function; the names of the 'pca' list should correspond
#     to the respective risk areas whose variables they summarise, e.g. 'pca.political', 'pca.social', etc.
# Output:
#   A named list containing the elements 'probabilities' (between 0.0 and 1.0), 'intensities' (between 0 
#   and 10), and 'predictions' (between 0 and 10, combination of the probabilities and intensities)
  
  if (as.character(model.one$formula[[2]]) == 'VC_Y4_NP' || as.character(model.one$formula[[2]]) == 'HVC_Y4_NP') {
    ethnic <- 'ETHNIC_NP'
  } else if (as.character(model.one$formula[[2]]) == 'VC_Y4_SN' || as.character(model.one$formula[[2]]) == 'HVC_Y4_SN') {
    ethnic <- 'ETHNIC_SN'
  } else {
    stop('computePredictionsPCAbyRiskAreaGLMandLM: something went wrong')
  }
  pol.vars <- c('REG_U', 'REG_P2', 'GOV_EFF', 'REPRESS', 'EMPOWER')
  soc.vars <- c(ethnic, 'DISPER', 'HOMIC', 'MORT')
  sec.vars <- c('CON_INT', 'CON_TREND', 'YRS_HVC', 'CON_NB')
  geo.vars <- c('WATER', 'FUEL_EXP', 'POP', 'STRUCT')
  eco.vars <- c('GDP_CAP', 'INEQ', 'FOOD', 'ECON_ISO')
  pca.political <- predict(pca[['pca.political']], newdata = new.data[, pol.vars])[, 1:3]
  pca.social    <- predict(pca[['pca.social']],    newdata = new.data[, soc.vars])[, 1:3]
  pca.security  <- predict(pca[['pca.security']],  newdata = new.data[, sec.vars])[, 1:2]
  pca.geography <- predict(pca[['pca.geography']], newdata = new.data[, geo.vars])[, 1:3]
  pca.economy   <- predict(pca[['pca.economy']],   newdata = new.data[, eco.vars])[, 1:3]
  pca.scores <- as.data.frame(cbind(pca.political, pca.social, pca.security, pca.geography, pca.economy))
  probabilities <- predict(model.one, newdata = pca.scores, type = 'response')
  intensities   <- predict(model.two, newdata = pca.scores)
  intensities   <- ifelse(intensities > kMaximumIntensity, 10, 
                   ifelse(intensities < kMinimumIntensity, 0, intensities))
  output.list <- list()
  if (threshold == 'static') {
    predictions <- ifelse(probabilities >= kClassifierThreshold, intensities, 0)
    output.list[['threshold']] <- kClassifierThreshold
  } else if (threshold == 'dynamic') {
    dynamic.threshold <- determineThresholdDynamically(new.data[, as.character(model.one$formula[2])], probabilities)
    predictions <- ifelse(probabilities >= dynamic.threshold, intensities, 0)
    output.list[['threshold']] <- dynamic.threshold    
  } else if (class(threshold) == 'numeric') {
    predictions <- ifelse(probabilities >= threshold, intensities, 0)
    output.list[['threshold']] <- threshold
  } else {
    stop(paste('invalid \'threshold\': ', threshold, sep = ''))
  }  
  output.list[['probabilities']] <- probabilities
  output.list[['intensities']]   <- intensities
  output.list[['predictions']]   <- predictions
  return(output.list)
}

determineThresholdDynamically <- function(observed.y, predicted.y) {
  # Description:
  #   Function to determine the threshold above which a positive outcome should be predicted
  # Input:
  #   observed.y: binary vector representing whether a country-year is denoted as conflict or not
  #   predicted.y: vector containing the probabilities predicted by a binary classifier (e.g. glm(), glmerMod())
  # Output:
  #   A numeric scalar representing the threshold that results in the highest sum of sensitivity (true positive
  #   rate) and specificity (true negative rate); see e.g. http://en.wikipedia.org/wiki/Sensitivity_and_specificity
  #   for more details
  # Notes:
  #   The criterium to choose the dynamic threshold, i.e. summing specificities and sensitivities, can be changed
  #   as needed or wanted and does not necessarily mean that this is the best way of going about this;
  #   also note that 'roc' stands for Receiver Operating Characteristic;
  #   note further that 'auc' (for Area Under Curve) is a function name used in both the pROC and glmnet 
  #   libraries. While not used in this particular function, for 'safety' reasons, the specific library for 
  #   roc() is specified (with pROC::), and it is advised to do this in other places too in which ROC curves, and the area
  #   under those curves, are calculated 
  roc               <- pROC::roc(observed.y, predicted.y)   
  sum               <- roc$sensitivities + roc$specificities
  dynamic.threshold <- roc$thresholds[match(max(sum), sum)]
  return(dynamic.threshold)
}

calculateMetrics <- function(observed.y, predicted.y, probabilities = NULL, highly.violent = FALSE) {
  # Description:
  #   Function to calculate model metrics
  # Input:
  #   observed.y: numeric vector, observed intensity scores
  #   predicted.y: numeric vector, predicted intensity scores
  #   probabilities: numeric vector, probabilities predicted by classifier models
  #   highly.violent: logical, whether boolen vectors should be computed for Violent 
  #     or Highly Violent conflicts
  # Output:
  #   A named list where each element is a specific metric computed for the respective
  #   statistical model used; same metrics for all models with the exception of the
  #   area under the Receiver Operating Characteristic curve - this can only be computed
  #   if probabilities are supplied, which in turn can only be done if the underlying
  #   statistical model consists of two parts: one for binary classification (war or peace),
  #   and one for the intensity predictions
  # Conditional probabilities:
  #   True positive rate:        P(high risk | conflict)
  #   False negative rate:       P(low risk  | conflict)
  #   True negative rate:        P(low risk  | no conflict)
  #   False positive rate:       P(high risk | no conflict
  #   Positive predictive value: P(conflict    | high risk)
  #   Negative predictive value: P(no conflict | low risk)
  outcome       <- determineOutcome(observed.y, predicted.y, highly.violent)
  outcome.table <- as.data.frame(table(outcome), row.names = 1)
  for (i in c('TP', 'TN', 'FP', 'FN')) {
    if (!i %in% rownames(outcome.table)) {
      outcome.table <- rbind(outcome.table, 0)
      rownames(outcome.table)[nrow(outcome.table)] <- i
    }
  }
  output.list <- list()
  output.list[['mean.squared.error']]        <- mean((predicted.y - observed.y)^2)
  output.list[['root.mean.squared.error']]   <- sqrt(mean((predicted.y - observed.y)^2))
  # output.list[['false']]                     <- outcome.table['FN', ] / sum(outcome.table['FN', ], outcome.table['TP', ])
  output.list[['true.positive.rate']]        <- outcome.table['TP', ] / sum(outcome.table['TP', ], outcome.table['FN', ])
  output.list[['false.negative.rate']]       <- outcome.table['FN', ] / sum(outcome.table['TP', ], outcome.table['FN', ])
  output.list[['true.negative.rate']]        <- outcome.table['TN', ] / sum(outcome.table['TN', ], outcome.table['FP', ])
  output.list[['false.positive.rate']]       <- outcome.table['FP', ] / sum(outcome.table['TN', ], outcome.table['FP', ])
  output.list[['positive.predictive.value']] <- outcome.table['TP', ] / sum(outcome.table['TP', ], outcome.table['FP', ])
  output.list[['negative.predictive.value']] <- outcome.table['TN', ] / sum(outcome.table['TN', ], outcome.table['FN', ])
  if (!is.null(probabilities)) {
    if (highly.violent) {
      observed.y.bool <- ifelse(observed.y > kHighlyViolentConflictIntensityThreshold, 1, 0)
    } else if (!highly.violent) {
      observed.y.bool <- ifelse(observed.y > kViolentConflictIntensityThreshold, 1, 0)
    } else {
      stop('invalid input for \'highly.violent\' parameter')
    }
    output.list[['area.under.curve']] <- as.numeric(pROC::auc(pROC::roc(response  = observed.y.bool, 
                                                                        predictor = probabilities)))
  }
  return(output.list)
}

determineOutcome <- function(observed.y, predicted.y, highly.violent) {
  # Description:
  #   Computes the raw outcome with regards to predictions made, i.e. classifies
  #   whether a certain prediction was a True Positive, True Negative, False Positive,
  #   or False Negative
  # Input:
  #   observed.y: numeric vector, observed intensity scores
  #   predicted.y: numeric vector, predicted intensity scores
  #   highly.violent: logical, whether boolen vectors should be computed for Violent 
  #     or Highly Violent conflicts
  # Output:
  #   A character vector whose elements are either 'TP' (True Positive), 'FN' (False Negative),
  #   'FP' (False Positive), or 'TN' (True Negative)
	output.vector <- character()
	if (highly.violent) {
		predicted.bool <- ifelse(predicted.y >= kHighlyViolentConflictIntensityThreshold, 1, 0)
		observed.bool  <- ifelse(observed.y  >= kHighlyViolentConflictIntensityThreshold, 1, 0)
	} else if (!highly.violent) {
		predicted.bool <- ifelse(predicted.y >= kViolentConflictIntensityThreshold, 1, 0)
		observed.bool  <- ifelse(observed.y  >= kViolentConflictIntensityThreshold, 1, 0)
	} else {
		stop('invalid: highly.violent can only be TRUE or FALSE')
	}
	for (i in 1:length(predicted.bool)) {
		if (observed.bool[i] == 1 && predicted.bool[i] == 1) {
      output.vector <- c(output.vector, 'TP')
		} else if (observed.bool[i] == 1 && predicted.bool[i] == 0) {
      output.vector <- c(output.vector, 'FN')
		} else if (observed.bool[i] == 0 && predicted.bool[i] == 1) {
      output.vector <- c(output.vector, 'FP')
		} else if (observed.bool[i] == 0 && predicted.bool[i] == 0) {
      output.vector <- c(output.vector, 'TN')
		} 
	}
	return(output.vector)
}

compareModels <- function(list.of.model.outputs, save.as.csv = FALSE, custom.name = NULL) {
  # Description:
  #   Function to compare the performance between different models with each other in a 
  #   neat overview/comparison matrix. Can also save said matrix in a .csv file
  # Input:
  #   list.of.model.outputs: the list of model outcomes returned by runModels()
  #   save.as.csv: logical, whether or not the comparison should be returned or saved as .csv file
  #   custom.name: character string to be added to the default file name if save.as.csv == TRUE
  # Output:
  #   If save.as.csv == FALSE, the function returns a named list whose two elements are called
  #   NATIONAL.POWER and SUBNATIONAL respectively; otherwise, it saves the comparison matrices
  #   to .csv files
  national.power.overview <- as.data.frame(list.of.model.outputs[[1]]$national.power[1:8])
  subnational.overview    <- as.data.frame(list.of.model.outputs[[1]]$subnational[1:8])
  if (length(list.of.model.outputs) > 1) {
    for (i in 2:length(list.of.model.outputs)) {
      national.power.overview <- rbind(national.power.overview, list.of.model.outputs[[i]]$national.power[1:8])
      subnational.overview    <- rbind(subnational.overview, list.of.model.outputs[[i]]$subnational[1:8]) 
    }  
  }
  row.names(national.power.overview) <- row.names(subnational.overview) <- names(list.of.model.outputs)
  comparison.matrices <- list(national.power.overview, subnational.overview)
  names(comparison.matrices) <- c('NATIONAL.POWER', 'SUBNATIONAL')
  if (!save.as.csv) {
    return(comparison.matrices)
  } else {
    if (!is.null(custom.name)) {
      write.csv(comparison.matrices[['NATIONAL.POWER']], paste(Sys.Date(), 
              'comparison-matrix', 'NATIONAL.POWER', custom.name, '.csv', sep = '__'))
      write.csv(comparison.matrices[['SUBNATIONAL']], paste(Sys.Date(),
              'comparison-matrix', 'SUBNATIONAL', custom.name, '.csv', sep = '__'))
    } else {
      write.csv(comparison.matrices[['NATIONAL.POWER']], paste(Sys.Date(), 
              'comparison-matrix', 'NATIONAL.POWER', custom.name, '.csv', sep = '__'))
      write.csv(comparison.matrices[['SUBNATIONAL']], paste(Sys.Date(),
              'comparison-matrix', 'SUBNATIONAL', custom.name, '.csv', sep = '__'))
    }
  }
}

#===========================================================================================================================#
# Functions to run the models 																				                                                    	#
#===========================================================================================================================#

runModels <- function(list.of.models, dat, threshold = 'static', cross.validation = FALSE, 
                      folds = 5, compute.metrics = TRUE) {
  # Description:
  #   This function runs all the models in list.of.models with the data supplied to the dat argument as training
  #   data. It allows for cross-validation where the training data is split into the number of folds specified by 
  #   the respective argument, and then uses all folds except one to run the models, and then calculate the metrics
  #   by applying the model to the fold it did not use to train the respective model. It does that such that every
  #   fold is used once for calculating the metrics. Afterwards, it averages the results per each model and outputs
  #   the metrics in the same format it would without cross-validation
  # Input:
  #   list.of.models: an object of type 'list' whose named elements consist of model functions
  #   dat: a data.frame containing variable and conflict data to be used to train the models
  #   threshold: a string character, either 'static' or 'dynamic'
  #   cross.validation: logical, determines whether cross-validation should be used or not
  #   folds: the number of folds in case cross.validation is TRUE
  #   compute.metrics: logical, passed directly to the individual models, determines whether metrics should be 
  #     calculated for each model
  # Output:
  #   An object of type 'list' whose names correspond to the names of list.of.models, and where each element
  #   in turn is a list with the named elements 'national.power' and 'subnational' that, in turn, are lists 
  #   whose elements consist of the respective metrics of the model
  models.output <- list()
  if (!cross.validation) {
    for (i in 1:length(list.of.models)) {
      np.model.results <- list.of.models[[i]](train = dat, threshold = threshold, conflict.dimension = 'np', 
                                              compute.metrics = compute.metrics)$metrics
      sn.model.results <- list.of.models[[i]](train = dat, threshold = threshold, conflict.dimension = 'sn', 
                                              compute.metrics = compute.metrics)$metrics
      models.output[[names(list.of.models)[[i]]]][['national.power']] <- np.model.results
      models.output[[names(list.of.models)[[i]]]][['subnational']]    <- sn.model.results
    }
  } else if (cross.validation) {
    cross.validation.data  <- dat
    cross.validation.folds <- list()
    for (fold in 1:folds) {
      random.sample <- sample(1:nrow(cross.validation.data), floor((1 / folds) * nrow(dat)), replace = FALSE)
      cross.validation.folds[[fold]] <- random.sample
      cross.validation.data <- cross.validation.data[setdiff(1:nrow(cross.validation.data), random.sample), ]
    }
    for (j in 1:length(list.of.models)) {
      np.intermediate.metrics <- list()
      sn.intermediate.metrics <- list()
      for (fold in 1:folds) {
        np.model.results <- list.of.models[[j]](train = dat[setdiff(1:nrow(dat), cross.validation.folds[[fold]]), ],
                                    test = dat[cross.validation.folds[[fold]], ], 
                                    threshold = threshold,
                                    conflict.dimension = 'np',
                                    compute.metrics = compute.metrics)$metrics
        sn.model.results <- list.of.models[[j]](train = dat[setdiff(1:nrow(dat), cross.validation.folds[[fold]]), ],
                                    test = dat[cross.validation.folds[[fold]], ], 
                                    threshold = threshold,
                                    conflict.dimension = 'sn',
                                    compute.metrics = compute.metrics)$metrics
        for (metric in 1:length(np.model.results)) {
          np.intermediate.metrics[[names(np.model.results)[[metric]]]] <- c(np.intermediate.metrics[[names(np.model.results)[[metric]]]],
                                                                      np.model.results[[metric]])
          sn.intermediate.metrics[[names(sn.model.results)[[metric]]]] <- c(sn.intermediate.metrics[[names(sn.model.results)[[metric]]]],
                                                                      sn.model.results[[metric]])
        }
      }
      np.averaged.metrics        <- lapply(lapply(np.intermediate.metrics, as.numeric), mean)
      sn.averaged.metrics        <- lapply(lapply(sn.intermediate.metrics, as.numeric), mean)
      names(np.averaged.metrics) <- names(np.intermediate.metrics)
      names(sn.averaged.metrics) <- names(sn.intermediate.metrics)
      models.output[[names(list.of.models)[[j]]]][['national.power']] <- np.averaged.metrics
      models.output[[names(list.of.models)[[j]]]][['subnational']]    <- sn.averaged.metrics
    }
  }
  return(models.output)
} 

#===========================================================================================================================#
# Functions to apply the models																				                                                    	#
#===========================================================================================================================#

applyModelsOld <- function(list.of.models, train.data, apply.data, threshold = 'static') {
  # Description:
  #   This function executes all models in list.of.models with the supplied train data and applies
  #   them to the apply.data; it is supposed to be used to create the predictions of different models
  #   for national power and subnational conflict dimensions
  # Input:
  #   list.of.models: an object of type 'list' whose named elements consist of model functions
  #   train.data: a data.frame containing variable and conflict data to be used to train the models
  #   apply.data: a data.frame containing the most recent variable data (but no conflict data) to
  #     apply the models to
  #   threshold: a character string that is either 'dynamic' for dynamical threshold determination,
  #     or 'static'; in the latter case, it uses kClassifierThreshold from 'constants.r'
  # Output:
  #   A named list where the names correspond to the names of the list.of.models input, and where each
  #   element is a two-column data.frame consisting of national power and subnational conflict dimension
  #   predictions. The order of the rows is unchanged from the order of the apply.data input so they can 
  #   be combined in the saveAppliedModelsOutput() function
  # Notes:
  #   All the function calls below are wrapped with suppressWarnings() because the warnings in this case
  #   are not needed, and more importantly, they should not trigger the tryCatch() warning function
  if (class(list.of.models) != 'list') {
    stop('invalid class \'list.of.models\': must be \'list\'')
  }
  if (class(train.data) != 'data.frame') {
    stop('invalid class \'train.data\': must be \'data.frame\'')
  }
  if (class(apply.data) != 'data.frame') {
    stop('invalid class \'apply.data\': must be \'data.frame\'')
  }
  if (!threshold %in% c('static', 'dynamic')) {
    stop('invalid \'threshold\': must be \'static\' or \'dynamic\'')
  } 
  applied.output <- list()
  for (i in 1:length(list.of.models)) {
    tryCatch.return <- tryCatch({
        np.results <- suppressWarnings(list.of.models[[i]](train = train.data, test = apply.data, threshold = threshold,
                                          conflict.dimension = 'np', compute.metrics = FALSE))
        sn.results <- suppressWarnings(list.of.models[[i]](train = train.data, test = apply.data, threshold = threshold,
                                          conflict.dimension = 'sn', compute.metrics = FALSE))
        temp.df <- cbind(apply.data, np.results[['predictions.list']][['predictions']],
                                     sn.results[['predictions.list']][['predictions']])
        colnames(temp.df)[(ncol(temp.df)-1):ncol(temp.df)] <- c('national.power.predictions', 'subnational.predictions')
        # print('finished executing \'try\' part')    # For debugging
        temp.df   # This is not returned (via return()) as that would exit the entire function; this way,
                  # it will simply be passed to the finally() function below
      }, error = function(condition) {
        # For dynamic thresholds, the models are first run with the entire dataset as training and test set
        # in order to determine the optimal threshold, then the models are run again with the thus determined
        # threshold. This could not be done directly since the most.recent.data passed to the apply.data argument
        # does not contain true/observed values for conflict and therefore cannot serve to calculate the ROC 
        # curve that determines which threshold to use. 
        # First round to determine dynamic thresholds:
        np.intermediate <- suppressWarnings(list.of.models[[i]](train = train.data, threshold = threshold, 
                                            conflict.dimension = 'np', compute.metrics = FALSE))
        sn.intermediate <- suppressWarnings(list.of.models[[i]](train = train.data, threshold = threshold, 
                                            conflict.dimension = 'sn', compute.metrics = FALSE))  
        # Second round using dynamic threshold to apply models to most.recent.data:
        np.results <- suppressWarnings(list.of.models[[i]](train = train.data, test = apply.data, 
                                            threshold = np.intermediate[['predictions.list']][['threshold']],
                                            conflict.dimension = 'np', compute.metrics = FALSE))
        sn.results <- suppressWarnings(list.of.models[[i]](train = train.data, test = apply.data,
                                            threshold = sn.intermediate[['predictions.list']][['threshold']],
                                            conflict.dimension = 'sn', compute.metrics = FALSE))                                           
        temp.df <- cbind(apply.data, np.results[['predictions.list']][['predictions']],
                                     sn.results[['predictions.list']][['predictions']])
        # print('temp.df created')     # For debugging            
        colnames(temp.df)[(ncol(temp.df)-1):ncol(temp.df)] <- c('national.power.predictions', 'subnational.predictions')
        temp.df
      }, warning = function(condition) {
        stop('this should not have happened')
      }, finally = function(temp.df) {
        applied.output[[names(list.of.models)[[i]]]] <- temp.df
        next
      }
    )
    applied.output[[names(list.of.models)[[i]]]] <- tryCatch.return
    # print('added tryCatch.return to applied.output')    # For debugging
  }
  return(applied.output)
}

applyModels <- function(list.of.models, train.data, apply.data, threshold = 'static') {
  # Description:
  #   This function executes all models in list.of.models with the supplied train data and applies
  #   them to the apply.data; it is supposed to be used to create the predictions of different models
  #   for national power and subnational conflict dimensions. It can be used to either compute
  #   predictions for the most recent data by supplying this data to the apply.data argument; 
  #   or it can be used to predict values for the historical data in order to investigate what a 
  #   specific model predicts for historical years and whether or not it would have "foreseen"
  #   a specific context. This data can be used for the country profiles. If this is done,
  #   when saving the output with saveAppliedModelsOutput(), the 'historical' argument to that
  #   function should be set to TRUE so that the final file name indicates this fact; it's default
  #   value is FALSE otherwise
  # Input:
  #   list.of.models: an object of type 'list' whose named elements consist of model functions
  #   train.data: a data.frame containing variable and conflict data to be used to train the models
  #   apply.data: a data.frame containing the most recent variable data (but no conflict data) to
  #     apply the models to
  #   threshold: a character string that is either 'dynamic' for dynamical threshold determination,
  #     or 'static'; in the latter case, it uses kClassifierThreshold from 'constants.r'
  # Output:
  #   A named list where the names correspond to the names of the list.of.models input, and where each
  #   element is a data.frame consisting of national power and subnational conflict dimension
  #   predictions and all columns from apply.data. The predictions altogether are six columns for the 
  #   predicted probabilities, intensities, and combined predictions for both conflict dimensions
  #   (i.e. national power and subnational). The order of the rows is unchanged from the order 
  #   of the apply.data input 
  # Notes:
  #   All the function calls below are wrapped with suppressWarnings() because the warnings in this case
  #   are not needed, and more importantly, they should not trigger the tryCatch() warning function
  if (class(list.of.models) != 'list') {
    stop('invalid class \'list.of.models\': must be \'list\'')
  }
  if (class(train.data) != 'data.frame') {
    stop('invalid class \'train.data\': must be \'data.frame\'')
  }
  if (class(apply.data) != 'data.frame') {
    stop('invalid class \'apply.data\': must be \'data.frame\'')
  }
  if (!threshold %in% c('static', 'dynamic')) {
    stop('invalid \'threshold\': must be \'static\' or \'dynamic\'')
  } 
  applied.output <- list()
  for (i in 1:length(list.of.models)) {
    tryCatch.return <- tryCatch({
      np.results <- suppressWarnings(list.of.models[[i]](train = train.data, test = apply.data, threshold = threshold,
                                        conflict.dimension = 'np', compute.metrics = FALSE))
      sn.results <- suppressWarnings(list.of.models[[i]](train = train.data, test = apply.data, threshold = threshold,
                                        conflict.dimension = 'sn', compute.metrics = FALSE))
      if ('probabilities' %in% names(np.results[['predictions.list']]) &&     # If it exists in np.results, it should also 
          'intensities' %in% names(np.results[['predictions.list']])) {       # exist in sn.results
        temp.df <- cbind(apply.data, np.results[['predictions.list']][['probabilities']],
                                     np.results[['predictions.list']][['intensities']],
                                     np.results[['predictions.list']][['predictions']],
                                     sn.results[['predictions.list']][['probabilities']],
                                     sn.results[['predictions.list']][['intensities']],
                                     sn.results[['predictions.list']][['predictions']])
        colnames(temp.df)[(ncol(temp.df)-5):ncol(temp.df)] <- c('national.power.probabilities', 'national.power.intensities',
                                                                'national.power.predictions',
                                                                'subnational.probabilities', 'subnational.intensities',
                                                                'subnational.predictions')
      } else {
        temp.df <- cbind(apply.data, np.results[['predictions.list']][['predictions']],
                                     sn.results[['predictions.list']][['predictions']])
        colnames(temp.df)[(ncol(temp.df)-1):ncol(temp.df)] <- c('national.power.predictions', 'subnational.predictions')
        # print('finished executing \'try\' part')    # For debugging
      }
      temp.df   # This is not returned (via return()) as that would exit the entire function; this way,
                # it will simply be passed to the finally() function below
    }, error = function(condition) {
      # For dynamic thresholds, the models are first run with the entire dataset as training and test set
      # in order to determine the optimal threshold, then the models are run again with the thus determined
      # threshold. This could not be done directly since the most.recent.data passed to the apply.data argument
      # does not contain true/observed values for conflict and therefore cannot serve to calculate the ROC 
      # curve that determines which threshold to use. 
      # First round to determine dynamic thresholds:
      np.intermediate <- suppressWarnings(list.of.models[[i]](train = train.data, threshold = threshold, 
                                          conflict.dimension = 'np', compute.metrics = FALSE))
      sn.intermediate <- suppressWarnings(list.of.models[[i]](train = train.data, threshold = threshold, 
                                          conflict.dimension = 'sn', compute.metrics = FALSE))  
      # Second round using dynamic threshold to apply models to most.recent.data:
      np.results <- suppressWarnings(list.of.models[[i]](train = train.data, test = apply.data, 
                                          threshold = np.intermediate[['predictions.list']][['threshold']],
                                          conflict.dimension = 'np', compute.metrics = FALSE))
      sn.results <- suppressWarnings(list.of.models[[i]](train = train.data, test = apply.data,
                                          threshold = sn.intermediate[['predictions.list']][['threshold']],
                                          conflict.dimension = 'sn', compute.metrics = FALSE))      
      if ('probabilities' %in% names(np.results[['predictions.list']]) &&     # If it exists in np.results, it should also 
          'intensities' %in% names(np.results[['predictions.list']])) {       # exist in sn.results
        temp.df <- cbind(apply.data, np.results[['predictions.list']][['probabilities']],
                                     np.results[['predictions.list']][['intensities']],
                                     np.results[['predictions.list']][['predictions']],
                                     sn.results[['predictions.list']][['probabilities']],
                                     sn.results[['predictions.list']][['intensities']],
                                     sn.results[['predictions.list']][['predictions']])
        colnames(temp.df)[(ncol(temp.df)-5):ncol(temp.df)] <- c('national.power.probabilities', 'national.power.intensities',
                                                                'national.power.predictions',
                                                                'subnational.probabilities', 'subnational.intensities',
                                                                'subnational.predictions')
      } else {
        temp.df <- cbind(apply.data, np.results[['predictions.list']][['predictions']],
                                     sn.results[['predictions.list']][['predictions']])
        colnames(temp.df)[(ncol(temp.df)-1):ncol(temp.df)] <- c('national.power.predictions', 'subnational.predictions')
      }
      temp.df
    }, warning = function(condition) {
      stop('this should not have happened')
    }, finally = function(temp.df) {
      applied.output[[names(list.of.models)[[i]]]] <- temp.df
      next
    })
    applied.output[[names(list.of.models)[[i]]]] <- tryCatch.return
    # print('added tryCatch.return to applied.output')    # For debugging
  }
  return(applied.output)
}

saveAppliedModelsOutput <- function(applied.models, applied.to = 'most-recent') {
  # Description:
  #   Small function to save to file the output from applyModels()
  # Input:
  #   applied.models: named list output from applyModels()
  # Output:
  #   For each model (named element) in applied.models, saves most recent data with
  #   national power and subnational predictions to .csv file, names them by date
  #   and model 
  if (applied.to == 'most-recent') {
    charv <- 'applied-to-most-recent.csv'
  } else if (applied.to == 'historical') {
    charv <- 'applied-to-historical.csv'
  } else if (applied.to == 'all') {
    charv <- 'applied-to-all-years.csv'
  } else {
    stop('invalid \'applied.to\': choose \'most-recent\', \'historical\', or \'all\'')
  }
  for (model.index in 1:length(applied.models)) {
    write.csv(applied.models[[model.index]], paste(Sys.Date(), 
              names(applied.models)[[model.index]], charv, sep = '__'),
              row.names = FALSE)
  }
}