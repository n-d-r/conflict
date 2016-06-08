#===========================================================================================================================#
# Preprocessing                                                                                                             #
#---------------------------------------------------------------------------------------------------------------------------#
# This file contains code to preprocess the input data, i.e. process type (peace, onset, war), etc.       			        		#
#===========================================================================================================================#

rm(list = ls(all = TRUE))

source('functions.r')
source('constants.r')

#===========================================================================================================================#
# Historical data without new variables                                                                                     #
#===========================================================================================================================#

rawdat <- read.csv('historical data v3.0.csv', header = TRUE)

rawdat[, 5:26] <- apply(rawdat[, 5:26], 2, as.numeric)
rawdat         <- rawdat[with(rawdat, order(ISO, -YEAR)), ]

VC_Y_NP  <- ifelse(rawdat$Intensity_Y_NP >= kViolentConflictIntensityThreshold, 1, 0)
VC_Y_SN  <- ifelse(rawdat$Intensity_Y_SN >= kViolentConflictIntensityThreshold, 1, 0)
HVC_Y_NP <- ifelse(rawdat$Intensity_Y_NP >= kHighlyViolentConflictIntensityThreshold, 1, 0)
HVC_Y_SN <- ifelse(rawdat$Intensity_Y_SN >= kHighlyViolentConflictIntensityThreshold, 1, 0)

rawdat.bool      <- cbind(rawdat, VC_Y_NP, VC_Y_SN, HVC_Y_NP, HVC_Y_SN)
rawdat.bool      <- switchColnames(rawdat.bool, 'forth')
preprocessed.dat <- computeOnset(rawdat.bool)
preprocessed.dat <- addRegions(preprocessed.dat)
write.csv(preprocessed.dat, 'preprocessed_input_data_unimputed.csv')

#===========================================================================================================================#
# Historical data with new variables                                                                                        #
#===========================================================================================================================#

rawdat.new.var <- read.csv('historical data with YB and Unemployment.csv')

rawdat.new.var[, 5:31] <- apply(rawdat.new.var[, 5:31], 2, as.numeric)
rawdat.new.var         <- rawdat.new.var[with(rawdat.new.var, order(ISO, -YEAR)), ]

VC_Y_NP.new.var  <- ifelse(rawdat.new.var$Intensity_Y_NP >= kViolentConflictIntensityThreshold, 1, 0)
VC_Y_SN.new.var  <- ifelse(rawdat.new.var$Intensity_Y_SN >= kViolentConflictIntensityThreshold, 1, 0)
HVC_Y_NP.new.var <- ifelse(rawdat.new.var$Intensity_Y_NP >= kHighlyViolentConflictIntensityThreshold, 1, 0)
HVC_Y_SN.new.var <- ifelse(rawdat.new.var$Intensity_Y_SN >= kHighlyViolentConflictIntensityThreshold, 1, 0)

rawdat.bool.new.var <- cbind(rawdat.new.var, VC_Y_NP.new.var, VC_Y_SN.new.var, HVC_Y_NP.new.var, HVC_Y_SN.new.var)
colnames(rawdat.bool.new.var)[(ncol(rawdat.bool.new.var)-3):ncol(rawdat.bool.new.var)] <- c('VC_Y_NP', 'VC_Y_SN',
                                                                                            'HVC_Y_NP', 'HVC_Y_SN')
rawdat.bool.new.var <- switchColnames(rawdat.bool.new.var, 'forth', new.var = TRUE)
preprocessed.dat.new.var <- computeOnset(rawdat.bool.new.var)
preprocessed.dat.new.var <- addRegions(preprocessed.dat.new.var)
write.csv(preprocessed.dat.new.var, 'preprocessed_input_data_unimputed_new_vars.csv')

#===========================================================================================================================#
# Entire data set                                                                                                           #
#===========================================================================================================================#

entire.dataset <- read.csv('historical data-Entire hist.series 89_13.csv')

entire.dataset[, 5:31] <- apply(entire.dataset[, 5:31], 2, as.numeric)
entire.dataset         <- entire.dataset[with(entire.dataset, order(ISO, -YEAR)), ]

VC_Y_NP.ed  <- ifelse(entire.dataset$Intensity_Y_NP >= kViolentConflictIntensityThreshold, 1, 0)
VC_Y_SN.ed  <- ifelse(entire.dataset$Intensity_Y_SN >= kViolentConflictIntensityThreshold, 1, 0)
HVC_Y_NP.ed <- ifelse(entire.dataset$Intensity_Y_NP >= kHighlyViolentConflictIntensityThreshold, 1, 0)
HVC_Y_SN.ed <- ifelse(entire.dataset$Intensity_Y_SN >= kHighlyViolentConflictIntensityThreshold, 1, 0)

entire.dataset <- cbind(entire.dataset, VC_Y_NP.ed, VC_Y_SN.ed, HVC_Y_NP.ed, HVC_Y_SN.ed)
colnames(entire.dataset)[(ncol(entire.dataset)-3):ncol(entire.dataset)] <- c('VC_Y_NP', 'VC_Y_SN',
                                                                            'HVC_Y_NP', 'HVC_Y_SN')
entire.dataset <- switchColnames(entire.dataset, 'forth', new.var = TRUE)

train.dataset <- subset(entire.dataset, entire.dataset$YEAR < 2010)
rest.dataset  <- subset(entire.dataset, entire.dataset$YEAR > 2009)
rest.dataset <- cbind(rest.dataset, rep('unknown', nrow(rest.dataset)), rep('unknown', nrow(rest.dataset)),
                                    rep('unknown', nrow(rest.dataset)), rep('unknown', nrow(rest.dataset)))
colnames(rest.dataset)[(ncol(rest.dataset)-3):ncol(rest.dataset)] <- c('VC_Y_NP_type', 'VC_Y_SN_type',
                                                                      'HVC_Y_NP_type', 'HVC_Y_SN_type')

train.dataset <- computeOnset(train.dataset)                                                                      
preproc.entire.dataset <- rbind(rest.dataset, train.dataset)                                                                      
preproc.entire.dataset <- addRegions(preproc.entire.dataset)
preproc.entire.dataset <- preproc.entire.dataset[with(preproc.entire.dataset, order(ISO, -YEAR)), ]
write.csv(preproc.entire.dataset, 'preprocessed_entire_dataset.csv')                                                                            

#===========================================================================================================================#
# Most recent/current data without new variables                                                                            #
#===========================================================================================================================#

currdat <- read.csv('unprocessed_current_data.csv')
currdat.switched <- switchColnames(currdat, 'forth')
currdat.switched[, 8:ncol(currdat.switched)] <- apply(currdat.switched[, 8:ncol(currdat.switched)], 2, as.numeric)

write.csv(currdat.switched, 'preprocessed_current_data.csv')

#===========================================================================================================================#
# Most recent/current data with new variables                                                                               #
#===========================================================================================================================#

currdat <- read.csv()
# To be continued...