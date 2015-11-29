###############################
# Stat E139 Fall 2015
# Group Project
# Author: Stephen Camera-Murray
###############################

#########
# Setup #
#########

# Load libraries

library(ggplot2)

# Read the files

f <- file.choose()
dataStores <- read.csv(f, header=T)
f <- file.choose()
dataSales <- read.csv(f, header=T)

##################
# Scrub the data #
##################

# Merge the data
dataTraining <- merge ( dataSales, dataStores, by = "Store" )
test <- head ( dataTraining )
attach ( dataTraining )

# Add the Competition Flag
dataTraining$CompetitionOpen <- as.integer ( as.Date ( paste ( CompetitionOpenSinceYear, CompetitionOpenSinceMonth, "1", sep = "-" ), "%Y-%m-%d" ) <= as.Date ( Date ) )
dataTraining$CompetitionOpen [ is.na ( CompetitionOpen ) ] <- 0
# Note: this var needs work-- only shows eligibility for promo right now, need to layer on promo2 month
dataTraining$Promo2Active <- as.integer ( ( as.integer ( format ( as.Date ( Date ), "%Y%U" ) ) > ( ( Promo2SinceYear * 100 ) + Promo2SinceWeek ) ) & ( T ) )
# Note: this is a test to see if month matches -- still working on it!!
dataTraining$Promo2Month <- as.integer ( grepl ( format ( as.Date ( Date ), "%b" ), as.character( PromoInterval ) ) )

# Histograms
qplot ( dataTraining$Sales, geom="density", fill=1 )
qplot ( log ( dataTraining$Sales ), geom="density", fill=1 )
# Note: # of customers is unavailable when we do the prediction-- duh! Took me a while to figure that out
qplot ( dataTraining$Customers, geom="density", fill=1 )
qplot ( log ( dataTraining$Customers ), geom="density", fill=1 )
qplot ( dataTraining$CompetitionDistance, geom="density", fill=1 )
qplot ( log ( dataTraining$CompetitionDistance ), geom="density", fill=1 )
