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

rm(list = ls())

# Choose your data
desired_data <- "dataModel_pruned_w2013.csv"

## Set the pwd
pwd <- "C:\\Users\\Jennifer\\Documents\\COURSES\\STAT_139_Modelling\\project\\Stat-139\\"

#install.packages("lubridate")
library (lubridate)
#install.packages("lme4")
library (lme4)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("moments")
library(moments)
#install.packages("reshape2")
library(reshape2)
#install.packages("MASS")
library(MASS) # critical for the qqplots with lines!


source(paste(pwd, "RossmannProject_GetData.R", sep=""))

######################################
# Step 2 - Transform and Derive Data #
######################################       

if(!file.exists(paste(pwd, desired_data, sep=""))) 
{
    getFullData(pwd)
}
dataTraining <- read.csv(paste(pwd, desired_data, sep=""), header=T)
dataTraining <- dataTraining[,-1]
dataTraining$YearFactor <- as.factor(dataTraining$YearFactor)

### Here are graphs to explain why we transformed, and other initial data explorations

###################
# Steve's section #
###################

# Histograms
qplot ( dataTraining$Sales, geom="density", fill=1 )
qplot ( log ( dataTraining$Sales ), geom="density", fill=1 )

qplot ( dataTraining$CompetitionDistance, geom="density", fill=1 )
qplot ( dataTraining$LogCompDistance , geom="density", fill=1 )

#####################
# Desiree's section #
#####################


###################
# Libby's section #
###################

######################
# Jennifer's section -- see supplementary graphs.rmd #
######################

                     
#############################
# Step 3 - Models           #
#############################

###################
# Libby's section #
###################

##add effect of different stores using Steve's model above (different intercepts for each store)
#could not run full model due to memory issues so as test run see my version of "fit" above and modification below - also had to rewrite

fit1<-lmer( LogSales ~ month + Year + DayOfWeekDummy + Promo + StateHoliday + StoreType + Assortment + CompetitionOpen + Promo2Active + (1|Store), data = dataTraining )
#stepwise <- stepAIC ( fit1, direction="both" )
summary(fit1)

#####################
# Desiree's section #
#####################

dklm <- lm ( LogSales ~ SummerMonthDummy*SummerBoost + DayOfWeek + Promo + StateHoliday + StoreType + Assortment + CompetitionOpen + Promo2Active, data = dataTraining )
summary(dklm)

# box plot example
boxplot(LogSales ~ DayOfWeek, data=dataTraining)

###################
# Steve's section #
###################

# Fit the regression model
#fit <- lm ( LogSales ~ AvgSales + DayOfWeek + Promo + StateHoliday + StoreType + Assortment + LogCompDistance + CompetitionOpen + Promo2Active, data = dataTraining )
fit <- lm ( LogSales ~ AvgSales + DayOfWeek + Promo + StateHoliday + StoreType + Assortment + CompetitionOpen + Promo2Active, data = dataTraining )
stepwise <- stepAIC ( fit, direction="both" )
testFit <- predict ( stepwise, dataTraining, level = 0.95 )
summary (fit)
fitRandomEffects<-lmer( LogSales ~ month + Year + DayOfWeekDummy + AvgSales + Promo + StateHoliday + StoreType + Assortment + CompetitionOpen + Promo2Active + (1|Store), data = dataTraining )




######################
# Jennifer's section #
######################
                         
# Here is the code to make that neat football plot I'd made before - this is all it takes. :-)

MODEL_VAR <- lm(LogSales ~ YearFactor + Season + NumPromos + Reopened + DayOfWeek_Named + LogPriorSales, data = dataTraining)
summary(my_model)
ggplot(ggplot2::fortify(MODEL_VAR), aes(.fitted, .resid)) + geom_point() + stat_smooth() + labs(title="Residual Plot")


################################################
# Step 4 - Analysis stuff for the final report #
################################################






##############################################################################
# Step 5 - Make predictions for the test set and export the submission file  #
##############################################################################

# SUBSTITUTE IN YOUR FAVORITE MODEL FOR THE "MODEL" VARIABLE HERE 
# (and change the filename if you like):

model <- my_model

makeSubmissionFile(model, pwd, "submit.csv")





