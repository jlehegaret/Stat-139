##########################################
# Stat E139 Fall 2015
# Group Project
# Kaggle team name: Chisquared Beta Normal
# Stephen Camera-Murray,
# Jennifer Le Hégaret,
# Desirée Koh,
# Elizabeth Wilson-Milne
##########################################

##################
# Step 1 - Setup #
##################

# Load libraries

library ( ggplot2 )
library ( MASS )

# Read the files

#f <- file.choose()
f <- "RossmannStoresInfo.csv"
dataStores <- read.csv(f, header=T)
#f <- file.choose()
f <- "RossmannTraining.csv"
dataSales <- read.csv(f, header=T)
#f <- file.choose()
f <- "RossmannTest.csv"
dataTest <- read.csv(f, header=T)
# Submission filename
submissionFilename <- "submission.csv"

###############################################
# Step 2 - Massage the data and add variables #
###############################################
# Note: We didn't use attach because new variables don't always show up

##################
# Common section #
##################

# Average sales by store
dataStores$AvgSales = tapply ( dataSales$Sales, dataSales$Store, FUN = mean )

# Merge the data
dataTraining <- merge ( dataSales, dataStores, by = "Store" )

## Add vars

# Add the Competition Flag
dataTraining$CompetitionOpen <- as.integer ( as.Date ( paste ( dataTraining$CompetitionOpenSinceYear, dataTraining$CompetitionOpenSinceMonth, "1", sep = "-" ), "%Y-%m-%d" ) <= as.Date ( dataTraining$Date ) )
dataTraining$CompetitionOpen [ is.na ( dataTraining$CompetitionOpen ) ] <- 0

# Add the Promo 2 flag
# Reorder the factors so we can do math to determine if we're in the promo 2 month
# Note: the factor order may be different in the test dataset
dataTraining$PromoInterval <- factor ( dataTraining$PromoInterval, levels ( dataTraining$PromoInterval )[c(1,4,3,2)] )
# Flag to determine eligibility for promo 2
dataTraining$Promo2Active <- as.integer (
  ( ( as.integer ( format ( as.Date ( dataTraining$Date ), "%Y%U" ) ) > ( ( dataTraining$Promo2SinceYear * 100 ) + dataTraining$Promo2SinceWeek ) )
    & ( ( as.integer ( format ( as.Date ( dataTraining$Date ), "%m" ) ) %% 3 ) == ( as.integer ( dataTraining$PromoInterval ) - 2 ) ) )
)

# Transform Sales and Competition Distance
dataTraining$LogSales <- log ( dataTraining$Sales )
# Only include competition distance when it's open?
dataTraining$LogCompDistance <- log ( dataTraining$CompetitionDistance )
#dataTraining$LogCompDistance [ is.na ( dataTraining$LogCompDistance ) ] <- 0

# Remove sales rows when there are no sales that day (usually due to being closed, but not always)
dataTraining <- dataTraining [ dataTraining$Sales != 0, ]

######################
# Jennifer's section #
######################

###################
# Libby's section #
###################

#####################
# Desiree's section #
#####################

###################
# Steve's section #
###################

##########################################################################
# Final vars: add any vars you wish to use for the final prediction here #
##########################################################################
dataSubmission <- merge ( dataTest, dataStores, by = "Store" )
dataSubmission$CompetitionOpen <- as.integer ( as.Date ( paste ( dataSubmission$CompetitionOpenSinceYear, dataSubmission$CompetitionOpenSinceMonth, "1", sep = "-" ), "%Y-%m-%d" ) <= as.Date ( dataSubmission$Date ) )
dataSubmission$CompetitionOpen [ is.na ( dataSubmission$CompetitionOpen ) ] <- 0
dataSubmission$PromoInterval <- factor ( dataSubmission$PromoInterval, levels ( dataSubmission$PromoInterval )[c(1,4,3,2)] )
dataSubmission$Promo2Active <- as.integer (
  ( ( as.integer ( format ( as.Date ( dataSubmission$Date ), "%Y%U" ) ) > ( ( dataSubmission$Promo2SinceYear * 100 ) + dataSubmission$Promo2SinceWeek ) )
    & ( ( as.integer ( format ( as.Date ( dataSubmission$Date ), "%m" ) ) %% 3 ) == ( as.integer ( dataSubmission$PromoInterval ) - 2 ) ) )
)
dataSubmission$LogCompDistance <- log ( dataSubmission$CompetitionDistance )

#############################
# Step 3 - Create the model #
#############################

######################
# Jennifer's section #
######################

###################
# Libby's section #
###################

#####################
# Desiree's section #
#####################

###################
# Steve's section #
###################
# Fit the regression model
#fit <- lm ( LogSales ~ AvgSales + DayOfWeek + Promo + StateHoliday + StoreType + Assortment + LogCompDistance + CompetitionOpen + Promo2Active, data = dataTraining )
fit <- lm ( LogSales ~ AvgSales + DayOfWeek + Promo + StateHoliday + StoreType + Assortment + CompetitionOpen + Promo2Active, data = dataTraining )
stepwise <- stepAIC ( fit, direction="both" )
testFit <- predict ( stepwise, dataTraining, level = 0.95 )

##############################################################################
# Step 4 - Make predictions for the test set and export the submission file  #
##############################################################################
# Note: Replace "stepwise" with your model name for the submission prediction
dataSubmission$Sales <- exp ( predict ( stepwise, dataSubmission, level = 0.95 ) )
# Predict 0 for stores that are closed
dataSubmission$Sales [ dataSubmission$Open == 0 ] <- 0
dataSubmission$Sales [ is.na ( dataSubmission$Sales ) ] <- 0
write.table ( dataSubmission [ c ( "Id", "Sales" ) ], submissionFilename, sep=",", row.names = F )

################################################
# Step 5 - Analysis stuff for the final report #
################################################

# Histograms
qplot ( dataTraining$Sales, geom="density", fill=1 )
qplot ( log ( dataTraining$Sales ), geom="density", fill=1 )
# Note: # of customers is unavailable when we do the prediction-- duh! Took me a while to figure that out
qplot ( dataTraining$Customers, geom="density", fill=1 )
qplot ( log ( dataTraining$Customers ), geom="density", fill=1 )
qplot ( dataTraining$CompetitionDistance, geom="density", fill=1 )
qplot ( dataTraining$LogCompDistance , geom="density", fill=1 )
