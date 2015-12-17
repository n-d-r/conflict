#===========================================================================================================================#
# Imputation                                                                                                                #
#---------------------------------------------------------------------------------------------------------------------------#
# This file contains a short script to impute missing values in the historical data set with the R package MICE (Multiple   #
# Imputation by Changed Equations). It creates ten imputed data sets and then simply averages them. The MICE package        #
# contains more complex functions to experiment with which are not used in the current iteration. See ?mice                 #
#===========================================================================================================================#

#===========================================================================================================================#
# Imputation for data set without new variables (UNEMP, YOUTHB)                                                             #
#===========================================================================================================================#

rm(list = ls(all = TRUE))
set.seed(333)     # The seed is for random number generation; setting it ensures that the script will always output the same results
require(mice); require(abind)

impute.this  <- read.csv('preprocessed_input_data_unimputed.csv')

# Data split up because only the covariates/features/variables need imputation; row order is maintained
record.dat   <- impute.this[, 1:5]
to.impute    <- impute.this[, 6:27]
conflict.dat <- impute.this[, 28:47]
REGIONS      <- impute.this[, 48]

mids <- mice(to.impute, m = 10)
d0   <- complete(mids, 1)
d1   <- complete(mids, 2)
d2   <- complete(mids, 3)
d3   <- complete(mids, 4)
d4   <- complete(mids, 5)
d5   <- complete(mids, 6)
d6   <- complete(mids, 7)
d7   <- complete(mids, 8)
d8   <- complete(mids, 9)
d9   <- complete(mids, 10)

d <- abind(d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, along = 3)
imputed   <- apply(d, c(1:2), mean)
full.data <- cbind(record.dat, imputed, conflict.dat, REGIONS)
write.csv(full.data, 'historical_data_imputed_by_R.csv')

#===========================================================================================================================#
# Imputation for data WITH new variables (UNEMP, YOUTHB, CORRUPT)                                                           #
#===========================================================================================================================#

rm(list = ls(all = TRUE))
set.seed(444)
require(mice); require(abind)

impute.this.new.var  <- read.csv('preprocessed_input_data_unimputed_new_vars.csv')

record.dat.new.var   <- impute.this.new.var[, 1:5]
to.impute.new.var    <- impute.this.new.var[, 6:32]
conflict.dat.new.var <- impute.this.new.var[, 33:52]
REGIONS              <- impute.this.new.var[, 53]

mids.new.var <- mice(to.impute.new.var, m = 10)
d0.new.var <- complete(mids.new.var, 1)
d1.new.var <- complete(mids.new.var, 2)
d2.new.var <- complete(mids.new.var, 3)
d3.new.var <- complete(mids.new.var, 4)
d4.new.var <- complete(mids.new.var, 5)
d5.new.var <- complete(mids.new.var, 6)
d6.new.var <- complete(mids.new.var, 7)
d7.new.var <- complete(mids.new.var, 8)
d8.new.var <- complete(mids.new.var, 9)
d9.new.var <- complete(mids.new.var, 10)

d.new.var <- abind(d0.new.var, d1.new.var, d2.new.var, d3.new.var, d4.new.var, 
                   d5.new.var, d6.new.var, d7.new.var, d8.new.var, d9.new.var,
                   along = 3)
imputed.new.var   <- apply(d.new.var, c(1:2), mean)
full.data.new.var <- cbind(record.dat.new.var, imputed.new.var, conflict.dat.new.var, REGIONS)
write.csv(full.data.new.var, 'historical_data_imputed_by_R_new_vars.csv')                   

#===========================================================================================================================#
# Imputation for data WITH new variables (UNEMP, YOUTHB, CORRUPT)                                                           #
#===========================================================================================================================#

rm(list = ls(all = TRUE))
set.seed(555)
require(mice); require(abind)

impute.this.ed <- read.csv('preprocessed_entire_dataset.csv')

records.ed   <- impute.this.ed[, 1:5]
to.impute.ed <- impute.this.ed[, 6:32]
conflict.ed  <- impute.this.ed[, 33:52]
REGIONS      <- impute.this.ed[, 53]

mids.ed <- mice(to.impute.ed, m = 10)
d0.ed <- complete(mids.ed, 1)
d1.ed <- complete(mids.ed, 2)
d2.ed <- complete(mids.ed, 3)
d3.ed <- complete(mids.ed, 4)
d4.ed <- complete(mids.ed, 5)
d5.ed <- complete(mids.ed, 6)
d6.ed <- complete(mids.ed, 7)
d7.ed <- complete(mids.ed, 8)
d8.ed <- complete(mids.ed, 9)
d9.ed <- complete(mids.ed, 10)

d.ed <- abind(d0.ed, d1.ed, d2.ed, d3.ed, d4.ed, d5.ed, d6.ed, 
              d7.ed, d8.ed, d9.ed, along = 3)
imputed.ed <- apply(d.ed, c(1:2), mean)
fulldata.ed <- cbind(records.ed, imputed.ed, conflict.ed, REGIONS)              
write.csv(fulldata.ed, 'preproc_entire_dataset_R-imputed.csv')