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
#install.packages("ggplot2")
library ( ggplot2 )
#install.packages("MASS")
library ( MASS )
#install.packages("lubridate")
library (lubridate)

# Set the pwd

# for Kaggle
pwd <- "../input/"

# for Steve

# for Desiree

# for Libby

# for Jennifer
#pwd <- "C:\\Users\\Jennifer\\Documents\\COURSES\\STAT 139 Modelling\\project\\Stat-139\\"

# Read the files
dataStores <- read.csv(paste(pwd, "store.csv", sep=""), header=T)
dataSales <- read.csv(paste(pwd, "train.csv", sep=""), header=T)
dataTest <- read.csv(paste(pwd, "test.csv", sep=""), header=T)
# Submission filename
submissionFilename <- "submission.csv"

##to free up memory?
gc() ##insert object

write.csv(dataTraining)

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

# Convert weekdays into actual names, both sets
dataTraining$DayOfWeek_Named <- format(as.Date(dataTraining$Date),"%a")
dataTest$DayOfWeek_Named <- format(as.Date(dataTest$Date),"%a")

# Add a season term

dataTraining$Season <- ""
dataTraining$Season[dataTraining$SchoolHoliday == 1 & (month(as.Date(dataTraining$Date)) == 12 | month(as.Date(dataTraining$Date)) == 1)] <- "Christmas"
dataTraining$Season[dataTraining$SchoolHoliday == 1 & (month(as.Date(dataTraining$Date)) == 3 | month(as.Date(dataTraining$Date)) == 4)] <- "Easter"
dataTraining$Season[dataTraining$SchoolHoliday == 1 & (month(as.Date(dataTraining$Date)) == 7 | month(as.Date(dataTraining$Date)) == 8 | month(as.Date(dataTraining$Date)) == 9)] <- "Summer_Break"
dataTraining$Season[dataTraining$SchoolHoliday == 1 & month(as.Date(dataTraining$Date)) == 10 ] <- "Fall_Break"
dataTraining$Season[dataTraining$Season == ""] <- format(as.Date(dataTraining$Date),"%b")

dataTest$Season <- ""
dataTest$Season[dataTest$SchoolHoliday == 1 & (month(as.Date(dataTest$Date)) == 12 | month(as.Date(dataTest$Date)) == 1)] <- "Christmas"
dataTest$Season[dataTest$SchoolHoliday == 1 & (month(as.Date(dataTest$Date)) == 3 | month(as.Date(dataTest$Date)) == 4)] <- "Easter"
dataTest$Season[dataTest$SchoolHoliday == 1 & (month(as.Date(dataTest$Date)) == 7 | month(as.Date(dataTest$Date)) == 8 | month(as.Date(dataTest$Date)) == 9)] <- "Summer_Break"
dataTest$Season[dataTest$SchoolHoliday == 1 & month(as.Date(dataTest$Date)) == 10 ] <- "Fall_Break"
dataTest$Season[dataTest$Season == ""] <- format(as.Date(dataTest$Date),"%b")

## Add a "recently reopened" flag
addReopenedFlag <- function(dset)
{
    # the vector to return
    Reopened <- rep(0, length(dset$Open))
    
    # make a vector of true/false according to if there is a change or not
    diffs <- dset$Open[-1L] != dset$Open[-length(dset$Open)]; diffs
    
    # make a vector of the last indexes of each particular run 
    idx <- c(which(diffs), length(diffs)); idx
    
    # this vector counts how long each run lasted
    runs <- diff(c(1, idx)); runs
    
    # find index values of the last day of each long run and length of each run
    # NOTE: IF HAVE PROBLEMS, MAYBE JUST FLAG 14 DAYS AND UP HERE RATHER THAN 2
    poss <- idx[runs > 2]; poss
    lengths <- runs[runs > 2]; lengths
    
    # check each long run to see if it is a run of closures
    numRuns <- length(poss); numRuns
    for(i in 1:numRuns)
    {
        if(dset$Open[poss[i]] == 0) # this run was of closed days
        {
            # mark the first 7 days of REOPENING as "bump"
            currStore <- dset$Store[poss[i]]
            #print(paste("Found one for Store #", currStore, "at index # ", poss[i]))
            if(lengths[i] < 5) # long weekend, maybe just one day's bump
            {
                cap <- 1
            }
            else if(lengths[i] < 31) # a month or less closed gives it several days
            { 
                cap <- 5
            }
            else # after longer than a month, give the excitement two weeks
            {
                cap <- 14
            }
            # don't go past our dataset, though
            cap <- min(cap, length(dset$Open) - poss[i])
            j <-1
            while(j <= cap & dset$Store[poss[i] + j] == currStore)
            {
                Reopened[poss[i] + j] <- 1
                j <- j + 1
            }
        }   
    }    
    return(Reopened)
}

# first, sort, then call function
dataTraining <- dataTraining[order(dataTraining$Store, dataTraining$Date),]
dataTraining$Reopened <- addReopenedFlag(dataTraining)

# first, sort, then call function
dataTest <- dataTest[order(dataTest$Store, dataTest$Date),]
dataTest$Reopened <- addReopenedFlag(dataTest)

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

##11/29/15
dataTraining$month=months(as.Date(dataTraining$Date))
dataTraining$Year=years(as.Date(dataTraining$Date))

##12/1/16 
dataTraining$DayOfWeekDummy<-as.character(dataTraining$DayOfWeek)
# just make dataset without zeros a different name in case we want to revisit with zeros? 
#FROM STEVE Remove sales rows when there are no sales that day (usually due to being closed, but not always)
train <- dataTraining [ dataTraining$Sales != 0, ]
train$DayOfWeekDummy0<-as.character(train$DayOfWeek)
###to save progress
write.csv(dataTraining, file = "dataTraining-merged12.1.15.csv")

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

#FROM STEVE fit <- lm ( Sales ~ AvgSales + DayOfWeek + Promo + StateHoliday + StoreType + Assortment + LogCompDistance + CompetitionOpen + Promo2Active, data = dataTraining )
fit <- lm ( Sales ~ Promo + StateHoliday + StoreType + Assortment + CompetitionOpen + Promo2Active, data = train )
stepwise <- stepAIC ( fit, direction="both" )
testFit <- predict ( stepwise, train, level = 0.95 )
summary(fit)

##add effect of different stores using Steve's model above (different intercepts for each store)
#could not run full model due to memory issues so as test run see my version of "fit" above and modification below - also had to rewrite
require(lme4)
fit1<-lmer( Sales ~ Promo + StateHoliday + StoreType + Assortment + CompetitionOpen + Promo2Active + (1|Store), data = train )
stepwise <- stepAIC ( fit1, direction="both" )
testFit1 <- predict ( stepwise, train, level = 0.95 )
summary(fit1)

fitdumb<-lm( Sales ~ DayOfWeekDummy0, data = train )
summary(fitdumb) ## it works now as dummy

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

