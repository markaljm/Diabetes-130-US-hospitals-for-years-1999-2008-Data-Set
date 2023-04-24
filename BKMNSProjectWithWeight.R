# Power Data Analytics Team!

# Diabetes Patient Hospital Re-admission Analysis

admit <- read.csv("diabetic_datanew.csv", header=TRUE)
#head(admit)

#table(admit$readmitted)
#table(admit$time_in_hospital)



# Standardize the continuous variables, giving new names:
admit$STAY <- scale(admit$time_in_hospital - mean(admit$time_in_hospital))
admit$LABS <- scale(admit$num_lab_procedures - mean(admit$num_lab_procedures))
admit$NONLABS <- scale(admit$num_procedures - mean(admit$num_procedures))
admit$MEDS <- scale(admit$num_medications - mean(admit$num_medications))
admit$OUTVISITS <- scale(admit$number_outpatient - mean(admit$number_outpatient))
admit$EMERGS <- scale(admit$number_emergency - mean(admit$number_emergency))
admit$INVISITS <- scale(admit$number_inpatient - mean(admit$number_inpatient))

# This one is strange!  (Not Normally distributed)
#table(admit$number_diagnoses)

admit$DIAGS <- scale(admit$number_diagnoses - mean(admit$number_diagnoses))

#table(admit$gender)	# Previous data cleaning removed 3 "Unknown/Invalid" rows


# Create new factor columns for all non-Binary Categorical columns:
# (The values look the same, but they're not strings anymore!)
admit$RACE <- relevel(factor(admit$race), ref = "Caucasian")
admit$SEX <- relevel(factor(admit$gender), ref = "Female")


# Previous data cleaning created new continuous AGE & WEIGHT columns.
# No longer needed:
#admit$AGE <- relevel(factor(admit$age), ref = "[70-80)")
admit$WEIGHT <- scale(admit$weight - mean(admit$weight))

#table(admit$admission_type_id)
#table(admit$discharge_disposition_id)
#table(admit$admission_source_id)

admit$ADMTYPE <- relevel(factor(admit$admission_type_id), ref = "1")
admit$DISP <- relevel(factor(admit$discharge_disposition_id), ref = "1")
admit$SOURCE <- relevel(factor(admit$admission_source_id), ref = "7")

install.packages("modeest")
library(modeest)
#mlv(admit$diag_1, method = "mfv")
#mlv(admit$diag_2, method = "mfv")

admit$DIAG1_ <- relevel(factor(admit$diag_1), ref = "428")
admit$DIAG2_ <- relevel(factor(admit$diag_2), ref = "276")

#table(admit$diag_3)	# 1423 occurrences of "?" -- A separate factor value created.
#mlv(admit$diag_3, method = "mfv")

admit$DIAG3_ <- relevel(factor(admit$diag_3), ref = "250")

#table(admit$DIAG3_)

#table(admit$max_glu_serum)	# 4 values

admit$MAXGLU_ <- relevel(factor(admit$max_glu_serum), ref = "None")
admit$A1C_ <- relevel(factor(admit$A1Cresult), ref = "None")
admit$METFORM_ <- relevel(factor(admit$metformin), ref = "No")
admit$REPA_ <- relevel(factor(admit$repaglinide), ref = "No")
admit$NATE_ <- relevel(factor(admit$nateglinide), ref = "No")
admit$CHLOR_ <- relevel(factor(admit$chlorpropamide), ref = "No")
admit$GLIME_ <- relevel(factor(admit$glimepiride), ref = "No")
admit$ACETO_ <- relevel(factor(admit$acetohexamide), ref = "No")
admit$GLI_ <- relevel(factor(admit$glipizide), ref = "No")
admit$GLY_ <- relevel(factor(admit$glyburide), ref = "No")
admit$TOLBUT_ <- relevel(factor(admit$tolbutamide), ref = "No")
admit$PIO_ <- relevel(factor(admit$pioglitazone), ref = "No")
admit$ROSI_ <- relevel(factor(admit$rosiglitazone), ref = "No")
admit$ACARB_ <- relevel(factor(admit$acarbose), ref = "No")
admit$MIGLIT_ <- relevel(factor(admit$miglitol), ref = "No")
admit$TROG_ <- relevel(factor(admit$troglitazone), ref = "No")
admit$TOLAZ_ <- relevel(factor(admit$tolazamide), ref = "No")

# The following are ALWAYS "No"!
admit$EXA_ <- relevel(factor(admit$examide), ref = "No")
admit$CITO_ <- relevel(factor(admit$citoglipton), ref = "No")
# Include as Factors in case different data is fed through later:
# With analyzed data, variables will surely be deemed Insignificant by the model!

admit$INSULIN_ <- relevel(factor(admit$insulin), ref = "No")
admit$GLY.MET_ <- relevel(factor(admit$glyburide.metformin), ref = "No")
admit$GLI.MET_ <- relevel(factor(admit$glipizide.metformin), ref = "No")
admit$GLI.PIO_ <- relevel(factor(admit$glimepiride.pioglitazone), ref = "No")
admit$MET.ROSI_ <- relevel(factor(admit$metformin.rosiglitazone), ref = "No")
admit$MET.PIO_ <- relevel(factor(admit$metformin.pioglitazone), ref = "No")


# Add new Binary column for each 2-value Category:
admit$MEDCHG_ <- apply(admit, 1, FUN = function(x) if(x["change"]=="Ch") TRUE else FALSE)
admit$DBMED_ <- apply(admit, 1, FUN = function(x) if(x["diabetesMed"]=="Yes") TRUE else FALSE)

# Turn dependent variable into a Binary for LR output:
admit$READMIT <- apply(admit, 1, FUN = function(x) if(x["readmitted"]=="NO") FALSE else TRUE)

#table(admit$readmitted)
#table(admit$READMIT)

#head(admit)


# Attempting a glm() on 41 variables pegged 2 CPU cores for 2.5+ hours without finishing!

# Idea:
# Get p-values to detect and remove silly medicine columns with 99+% of a single value
# and anything else that won't matter:

# "Silly medicine column":
#table(admit$TOLAZ_)
#     No Steady     Up 
# 101727     38      1 

#chisq.test(table(admit$TOLAZ_))
# X-squared = 203298, df = 2, p-value < 2.2e-16
# VERY Significant?


# Non-silly column (i.e., with decent distribution of more than 2 values):
#table(admit$INSULIN_)
#    No   Down Steady     Up 
# 47383  12218  30849  11316 

#chisq.test(table(admit$INSULIN_))
# X-squared = 34788, df = 3, p-value < 2.2e-16
# ALSO VERY Significant??

# NO HELP!?!


# Reduce dataset by removing unnecessary columns:
# (We know WEIGHT is totally dependent on age, so exclude age.)
admit2 <- admit[, c('READMIT', 'STAY', 'LABS', 'NONLABS', 'MEDS', 'OUTVISITS', 'EMERGS', 'INVISITS'
	, 'DIAGS', 'RACE', 'WEIGHT', 'SEX', 'ADMTYPE', 'DISP', 'SOURCE', 'DIAG1_', 'DIAG2_', 'DIAG3_'
	, 'MAXGLU_', 'A1C_', 'METFORM_', 'REPA_', 'NATE_', 'CHLOR_', 'GLIME_', 'ACETO_'
	, 'GLI_', 'GLY_', 'TOLBUT_', 'PIO_', 'ROSI_', 'ACARB_', 'MIGLIT_', 'TROG_', 'TOLAZ_'
	, 'EXA_', 'CITO_', 'INSULIN_', 'GLY.MET_', 'GLI.MET_', 'GLI.PIO_', 'MET.ROSI_', 'MET.PIO_'
	, 'MEDCHG_', 'DBMED_')]
# glm() for  25K rows with 21 variables took less than 1 hour (wall-clock timed) on a 4-core 11th Gen Intel i7-1185G7 @ 3GHz
# glm() for  40K rows with 20 variables took 5.2 minutes on a 4-core 11th Gen Intel i7-1185G7 @ 3GHz
# glm() for 102K rows with 21 variables took <22 minutes on a 4-core 11th Gen Intel i7-1185G7 @ 3GHz

#head(admit2)


library("caret")

# Train model on a subset of the rows:
set.seed(51)
trainIndex <- createDataPartition(admit2$READMIT, p = .8,
                                  list = FALSE,
                                  times = 1)

admit2.train <- admit2[ trainIndex,]
admit2.test  <- admit2[-trainIndex,]


# To determine which predictor variables are perfectly correlated, we can use the cor() function to create a correlation matrix for the variables:
#cor(admit2[, unlist(lapply(admit2, is.numeric))])
# Don't see any perfect correlation among any numeric variables!?


# Check for correlation between each remaining categorical column pair:
#categorical_columns = c('RACE', 'WEIGHT', 'SEX', 'ADMTYPE', 'DISP',
#	'SOURCE', 'DIAG1_', 'DIAG2_', 'DIAG3_', 'MEDCHG_', 'DBMED_')
#categorical_columns = c('SEX', 'DISP', 'DIAG1_', 'MEDCHG_',
#	'MAXGLU_', 'A1C_', 'METFORM_', 'REPA_', 'GLIME_'
#	)
#categorical_columns = c('SEX', 'DIAG1_', 'MEDCHG_',
#	'MAXGLU_', 'A1C_', 'METFORM_', 'REPA_'
#	, 'GLI_', 'GLY_', 'TOLBUT_', 'PIO_', 'ROSI_', 'ACARB_', 'TROG_', 'TOLAZ_'
#	)

# Re-doing after we imputed new WEIGHT variable:
#categorical_columns = c('WEIGHT', 'SEX', 'DIAG1_',
#	'MAXGLU_', 'A1C_', 'METFORM_', 'REPA_'
#	, 'TOLBUT_', 'PIO_', 'ROSI_', 'ACARB_', 'TROG_', 'TOLAZ_'
#	)

#categorical_columns = c('WEIGHT', 'SEX', 'DIAG1_',
#	'MAXGLU_'
#	, 'TOLBUT_', 'PIO_', 'ROSI_', 'ACARB_', 'TROG_', 'TOLAZ_'
#	, 'INSULIN_', 'GLY.MET_'
#	)
# Excluded variables with only 1 level:
# EXA_, CITO_, GLI.MET_, GLI.PIO_, MET.ROSI_, MET.PIO_
#table(admit2$MET.PIO_)


#pairs = combn(categorical_columns, 2, simplify=TRUE)
#pair_seq = seq(1, length(pairs)-1, 2)

#for (i in pair_seq)
#{
#	print (pairs[i])
#	print (pairs[i+1])
#	Pearson <- chisq.test(getElement(admit2, pairs[i]), getElement(admit2, pairs[i+1]))
#	print (Pearson)
#}
# p-value < 0.05 means variables ARE related.

# This claims the following are correlated:
[1] "RACE" X
[1] "SEX"
X-squared = 13.949, df = 5, p-value = 0.01594

[1] "RACE" X
[1] "ADMTYPE" X
X-squared = 31.856, df = 15, p-value = 0.006734

[1] "RACE" X
[1] "MEDCHG_"
X-squared = 13.446, df = 5, p-value = 0.01954

[1] "AGE" X
[1] "SEX"
X-squared = 18.907, df = 9, p-value = 0.02599

[1] "AGE" X
[1] "ADMTYPE" X
X-squared = 40.922, df = 27, p-value = 0.04191

[1] "AGE" X
[1] "DISP"
X-squared = 94.069, df = 72, p-value = 0.04152

[1] "AGE" X
[1] "DIAG1_"
X-squared = 1597.4, df = 1215, p-value = 7.229e-13

[1] "AGE" X
[1] "DIAG2_" X
X-squared = 1345.9, df = 1152, p-value = 5.973e-05

[1] "AGE" X
[1] "DIAG3_" X
X-squared = 1548.6, df = 1260, p-value = 3.841e-08

[1] "AGE" X
[1] "DBMED_" X
X-squared = 21.105, df = 9, p-value = 0.01219

[1] "ADMTYPE" X
[1] "DISP"
X-squared = 111.08, df = 21, p-value = 3.006e-14

[1] "ADMTYPE" X
[1] "SOURCE" X
X-squared = 118.16, df = 9, p-value < 2.2e-16

[1] "ADMTYPE" X
[1] "DIAG1_"
X-squared = 222.55, df = 171, p-value = 0.004889

[1] "ADMTYPE" X
[1] "MEDCHG_"
X-squared = 21.667, df = 3, p-value = 7.652e-05

[1] "ADMTYPE" X
[1] "DBMED_" X
X-squared = 14.139, df = 3, p-value = 0.002722

[1] "DISP"
[1] "DBMED_" X
X-squared = 19.355, df = 7, p-value = 0.007146

[1] "DISP"
[1] "SOURCE" X
X-squared = 239.17, df = 48, p-value < 2.2e-16

[1] "SOURCE" X
[1] "DIAG1_"
X-squared = 1142.4, df = 810, p-value = 9.418e-14

[1] "SOURCE" X
[1] "MEDCHG_"
X-squared = 14.595, df = 6, p-value = 0.02366

[1] "SOURCE" X
[1] "DBMED_" X
X-squared = 22.107, df = 6, p-value = 0.001158

[1] "DIAG1"
[1] "DIAG2" X
X-squared = 3681.3, df = 3021, p-value = 9.078e-16

[1] "DIAG1"
[1] "DIAG3" X
X-squared = 2929.3, df = 2622, p-value = 2.089e-05

[1] "DIAG1_"
[1] "DBMED_" X
X-squared = 171.03, df = 135, p-value = 0.01956

[1] "DIAG2" X
[1] "DIAG3" X
X-squared = 2666.1, df = 2438, p-value = 0.0007389

[1] "MEDCHG_"
[1] "DBMED_" X
X-squared = 7.6335, df = 1, p-value = 0.005729

# Choose to remove RACE, AGE, ADMTYPE, DBMED_, SOURCE, & DIAG2_ & DIAG3_


[1] "SEX"
[1] "DIAG1_"
X-squared = 238.89, df = 191, p-value = 0.0106

[1] "DISP" X
[1] "MEDCHG_"
X-squared = 34.426, df = 9, p-value = 7.519e-05

[1] "DISP" X
[1] "MAXGLU_"
X-squared = 169.19, df = 27, p-value < 2.2e-16

[1] "DISP" X
[1] "A1C_"
X-squared = 70.036, df = 27, p-value = 1.108e-05

[1] "DIAG1_"
[1] "A1C_"
X-squared = 631.26, df = 573, p-value = 0.04595

[1] "A1C_"
[1] "GLIME_" X
X-squared = 18.196, df = 9, p-value = 0.03297

[1] "METFORM_"
[1] "GLIME_" X
X-squared = 61.875, df = 9, p-value = 5.824e-10

# Choose to remove DISP, GLIME_
# We dare not remove A1C_ (our only *** variable!), SEX, nor DIAG1_!!


[1] "DIAG1_"
[1] "GLI_" X
X-squared = 891.76, df = 696, p-value = 6.484e-07

[1] "DIAG1_"
[1] "GLY_" X
X-squared = 1300.5, df = 696, p-value < 2.2e-16

[1] "DIAG1_"
[1] "TOLBUT_"
X-squared = 382.89, df = 232, p-value = 1.646e-09

[1] "DIAG1_"
[1] "ROSI_"
X-squared = 796.69, df = 464, p-value < 2.2e-16

[1] "DIAG1_"
[1] "ACARB_"
X-squared = 276.35, df = 232, p-value = 0.02435

[1] "MEDCHG_" X
[1] "MAXGLU_"
X-squared = 17.947, df = 3, p-value = 0.000451

[1] "MEDCHG_" X
[1] "A1C_"
X-squared = 21.801, df = 3, p-value = 7.176e-05

[1] "MEDCHG_" X
[1] "METFORM_"
X-squared = 131.94, df = 3, p-value < 2.2e-16

[1] "MEDCHG_" X
[1] "REPA_"
X-squared = 14.769, df = 3, p-value = 0.002025

[1] "MEDCHG_" X
[1] "GLI_" X
X-squared = 93.641, df = 3, p-value < 2.2e-16

[1] "MEDCHG_" X
[1] "GLY_" X
X-squared = 51.239, df = 3, p-value = 4.351e-11

[1] "MEDCHG_" X
[1] "PIO_"
X-squared = 15.869, df = 3, p-value = 0.001206

[1] "MEDCHG_" X
[1] "ROSI_"
X-squared = 74.989, df = 2, p-value < 2.2e-16

[1] "A1C_"
[1] "GLI_" X
X-squared = 18.864, df = 9, p-value = 0.02637

[1] "A1C_"
[1] "PIO_"
X-squared = 18.71, df = 9, p-value = 0.02778

[1] "METFORM_"
[1] "GLY_" X
X-squared = 20.344, df = 9, p-value = 0.01591

[1] "GLI_" X
[1] "GLY_" X
X-squared = 19.565, df = 9, p-value = 0.02079

[1] "GLI_" X
[1] "ROSI_"
X-squared = 45.546, df = 6, p-value = 3.645e-08

[1] "GLY_" X
[1] "PIO_"
X-squared = 166.28, df = 9, p-value < 2.2e-16

[1] "GLY_" X
[1] "ACARB_"
X-squared = 22.421, df = 3, p-value = 5.331e-05

[1] "PIO_"
[1] "ACARB_"
X-squared = 11.653, df = 3, p-value = 0.008673

# Choose to remove MEGDHG_, GLI_, GLY_


# WE MUST KEEP OUR STAR VARIABLE WEIGHT!!
[1] "WEIGHT"
[1] "SEX"
X-squared = 34533, df = 2219, p-value < 2.2e-16

[1] "WEIGHT"
[1] "DIAG1_"
X-squared = 1434373, df = 1364685, p-value < 2.2e-16

[1] "WEIGHT"
[1] "METFORM_"
X-squared = 7810.9, df = 6657, p-value < 2.2e-16

[1] "WEIGHT"
[1] "TOLBUT_"
X-squared = 3274.3, df = 2219, p-value < 2.2e-16

[1] "WEIGHT"
[1] "ACARB_"
X-squared = 9393.9, df = 4438, p-value < 2.2e-16

[1] "SEX"
[1] "DIAG1_"
X-squared = 2373.3, df = 615, p-value < 2.2e-16

[1] "SEX"
[1] "A1C_" X
X-squared = 15.919, df = 3, p-value = 0.001178

[1] "SEX"
[1] "ROSI_"
X-squared = 13.398, df = 3, p-value = 0.00385

[1] "DIAG1_"
[1] "MAXGLU_"
X-squared = 3913.1, df = 1845, p-value < 2.2e-16

[1] "DIAG1_"
[1] "A1C_" X
X-squared = 4849.4, df = 1845, p-value < 2.2e-16

[1] "DIAG1_"
[1] "METFORM_" X
X-squared = 3358.4, df = 1845, p-value < 2.2e-16

[1] "DIAG1_"
[1] "TOLBUT_"
X-squared = 1338.8, df = 615, p-value < 2.2e-16

[1] "DIAG1_"
[1] "PIO_"
X-squared = 2556.3, df = 1845, p-value < 2.2e-16

[1] "DIAG1_"
[1] "TOLAZ_"
X-squared = 1678.9, df = 1230, p-value < 2.2e-16

[1] "MAXGLU_"
[1] "A1C_" X
X-squared = 722.19, df = 9, p-value < 2.2e-16

[1] "MAXGLU_"
[1] "METFORM_" X
X-squared = 53.864, df = 9, p-value = 2.003e-08

[1] "MAXGLU_"
[1] "REPA_"
X-squared = 49.027, df = 9, p-value = 1.641e-07

[1] "MAXGLU_"
[1] "TOLBUT_"
X-squared = 9.7936, df = 3, p-value = 0.0204

[1] "A1C_" X
[1] "METFORM_" X
X-squared = 182.21, df = 9, p-value < 2.2e-16

[1] "A1C_" X
[1] "REPA_"
X-squared = 60.559, df = 9, p-value = 1.046e-09

[1] "A1C_" X
[1] "PIO_"
X-squared = 37.736, df = 9, p-value = 1.944e-05

[1] "A1C_" X
[1] "ROSI_"
X-squared = 18.507, df = 9, p-value = 0.02973

[1] "A1C_" X
[1] "ACARB_"
X-squared = 20.252, df = 6, p-value = 0.002497

[1] "A1C_" X
[1] "TOLAZ_"
X-squared = 30.084, df = 6, p-value = 3.789e-05

[1] "METFORM_" X
[1] "PIO_"
X-squared = 70.664, df = 9, p-value = 1.128e-11

[1] "METFORM_" X
[1] "ROSI_"
X-squared = 322.2, df = 9, p-value < 2.2e-16

[1] "METFORM_" X
[1] "ACARB_"
X-squared = 40.766, df = 6, p-value = 3.219e-07

[1] "METFORM_" X
[1] "TOLAZ_"
X-squared = 20.23, df = 6, p-value = 0.00252

[1] "REPA_"
[1] "PIO_"
X-squared = 44.878, df = 9, p-value = 9.718e-07

[1] "REPA_"
[1] "ROSI_"

X-squared = 35.476, df = 9, p-value = 4.91e-05

[1] "REPA_"
[1] "ACARB_"
X-squared = 38.526, df = 6, p-value = 8.865e-07

[1] "PIO_"
[1] "ROSI_"
X-squared = 145.45, df = 9, p-value < 2.2e-16

# Choose to remove most-dependently-paired variables: A1C_, METFORM_, REPA_


[1] "DIAG1_"
[1] "INSULIN_"
X-squared = 4788, df = 1845, p-value < 2.2e-16

[1] "DIAG1_"
[1] "GLY.MET_"
X-squared = 2022.5, df = 1845, p-value = 0.002229

[1] "MAXGLU_"
[1] "TOLBUT_"
X-squared = 9.7936, df = 3, p-value = 0.0204

[1] "MAXGLU_"
[1] "INSULIN_"
X-squared = 1897.4, df = 9, p-value < 2.2e-16

[1] "PIO_"
[1] "INSULIN_"
X-squared = 25.308, df = 9, p-value = 0.002649

[1] "PIO_"
[1] "GLY.MET_"
X-squared = 20.443, df = 9, p-value = 0.01537

[1] "ROSI_"
[1] "INSULIN_"
X-squared = 18.087, df = 9, p-value = 0.03417

[1] "TOLAZ_"
[1] "INSULIN_"
X-squared = 17.274, df = 6, p-value = 0.008329


# INSULIN_ might be our next one to remove, but
# We'll try with all these and see what the LR has for multi-variate p-values.



# ----- Run from here to include profiling of LR generation ------
# Get time of code execution
start <- Sys.time()

# Try a Logistic Regression on remaining 14 columns
LR <- glm(READMIT ~ STAY + LABS + NONLABS + MEDS + OUTVISITS + EMERGS + INVISITS
	+ DIAGS + SEX + WEIGHT
	+ DIAG1_ + MAXGLU_ + TOLBUT_
	+ PIO_ + ROSI_ + ACARB_ +  TROG_ + TOLAZ_ + INSULIN_ + GLY.MET_
	,
	family = "binomial", data = admit2.train)
	
# Show clock time elapsed during glm():
print( Sys.time() - start )
# ----- Run to here to include profiling of LR generation ------


# If glm results in the following error:
# ... contrasts can be applied only to factors with 2 or more levels
# Run this and look for values of "1" to see which factors to exclude:
#sapply(lapply(admit2, unique), length)
# Excluded for this (with subset of rows):
# EXA_, CITO_, ACETO_, NATE_, CHLOR_, MIGLIT_, GLY.MET_, GLI.MET_, GLI.PIO_, MET.ROSI_
# As we expand the subset up to our desired training set,
# check this again and see if any of these can be re-added.


#summary(LR)

# Significant variables/factors are:
# *** (Intercept), LABS, NONLABS, OUTVISITS, EMERGS, INVISITS, DIAGS
# *** Quite a few DIAG1_'s
# *** ROSI_
# *** INSULIN_Down, INSULIN_Up,

# **  PIO_, 

# *   SEXMale, INSULIN_Steady, GLY.MET_Steady


#head(admit)


# Calculate Variable Inflation Factors (VIFs)
# to check for multicollinearity:
library(car)

vif(LR)

# Results:
# All between 1 and 1.4


# ----- Run from here to include profiling of LR generation ------
# Get time of code execution
start <- Sys.time()

# Try a reduced Logistic Regression on the significant variables:
LR2 <- glm(READMIT ~ LABS + NONLABS + OUTVISITS + EMERGS + INVISITS
	+ DIAGS + SEX + WEIGHT
	+ DIAG1_
	+ PIO_ + ROSI_ + INSULIN_ + GLY.MET_
	,
	family = "binomial", data = admit2.train)
	
# Show clock time elapsed during glm():
print( Sys.time() - start )
# ----- Run to here to include profiling of LR generation ------
# glm() for  80K rows with 20 variables took 12.8 minutes on a 4-core 11th Gen Intel i7-1185G7 @ 3GHz


summary(LR2)


# Compare the 2 models with a Likelihood Ratio Test:
anova(LR, LR2, test="LRT")

# p-value = 0.6424
# Difference is very significant!!
# Proceed with the reduced model.


# Testing the model:
# Use the function predict, applied to the model and the training set,
# to calculate a probability for every test observation:

admit2.test$LRprob <- predict(LR2, admit2.test, type = "response")
# Error: factor DIAG1_ has new levels 143, 145, 334, 360, etc.

admit2.test2 <- admit2.test
# Change DIAG1_ values not present in training dataset to NA:
admit2.test2$DIAG1_[which(!(admit2.test2$DIAG1_ %in% unique(admit2.train$DIAG1)))] <- NA

head(admit2.test2)

admit2.test2$LRprob <- predict(LR2, admit2.test2, type = "response")

class(admit2.test2$LRprob)	# "numeric"
mean(admit2.test2$LRprob[which(!is.na(admit2.test2$LRprob))])	# 0.4612521

# Save those probabilities to the testing set under the variable “LRpred”.
admit2.test2$LRpred <- apply(admit2.test2, 1,
	FUN = function(x) if(!is.na(x["LRprob"]) && x["LRprob"] > 0.50) TRUE else FALSE)
# ALL TRUE   ^^^^^^!?!?! (except the 20 NA's)


admit2.test2$LRprob[which(is.na(admit2.test2$DIAG1_))]

table(admit2.test2$LRpred)
table(admit2.test2$READMIT)




