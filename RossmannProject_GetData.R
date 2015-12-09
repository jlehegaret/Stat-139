# Stat E139 Fall 2015
# Group Project
# Kaggle team name: Chisquared Beta Normal
# Stephen Camera-Murray,
# Jennifer Le Hégaret,
# Desirée Koh,
# Elizabeth Wilson-Milne
##########################################

# This module provides the functions we need to 
#    obtain our data,
#    transform some variables
#    add derived data
#    and create a new data source file

getFullData <- function(pwd)
{   
    # Read the files
    print("Reading files")
    dataStores <- read.csv(paste(pwd, "store.csv", sep=""), header=T)
    dataSales <- read.csv(paste(pwd, "train.csv", sep=""), header=T)
    
    # Mark which dates are reopening dates before we delete the zero data
    print("Adding reopened data")
    dataSales$Reopened <- addReopenedFlag(dataSales)
    
    # Only keep rows for open stores with positive sales
    dataSales <- dataSales [ dataSales$Sales > 0 & dataSales$Open != 0, ]
    
    # Add store-level derived data
    print("Adding store-level derived data")
    dataStores <- addStoreDerived(dataStores, dataSales)
        
    # Merge the data
    print("Merging the datasets")
    dataTraining <- merge ( dataSales, dataStores, by = "Store" )

    # add date-level derived data
    print("Adding date-level derived data")
    dataTraining <- addDateDerived(dataTraining)
    dataTraining$LogSales <- log(dataTraining$Sales) 
    
    # export new files by given names
    print("Exporting full Stores data")
    write.csv(dataStores, paste(pwd, "dataStores_full.csv", sep=""))    
    print("Exporting full Training data")
    write.csv(dataTraining, paste(pwd, "dataTraining_full.csv", sep=""))
    print("Done!")
}

makeTestFile <- function(pwd)
{
    # Read the files
    dataStores <- read.csv(paste(pwd, "dataStores_full.csv", sep=""), header=T)
    dataSkeleton <- read.csv(paste(pwd, "test.csv", sep=""), header=T)

    print("Adding reopened flag")
    dataSkeleton$Reopened <- addReopenedFlag(dataSkeleton)
    
    # Merge the data
    dataTest <- merge ( dataSkeleton, dataStores, by = "Store" )
    
    # add date-level derived data
    dataTest <- addDateDerived(dataTest)

    # export new file for testing
    write.csv(dataTest, paste(pwd, "dataTesting_full.csv", sep=""))    
}

makeSubmissionFile <- function(model, pwd, filename)
{
    if(!file.exists(paste(pwd, "dataTesting_full.csv", sep=""))) 
    {
        print("Making test file")
        makeTestFile(pwd)
    }
    
    # Read the file
    print("Reading in test file")
    dataSubmission <- read.csv(paste(pwd, "dataTesting_full.csv", sep=""), header=T)
    
    print("Making predictions")
    dataSubmission$Sales <- exp ( predict ( model, dataSubmission, level = 0.95 ) )
    # Predict 0 for stores that are closed
    print("Cleaning out closures with zeros")
    dataSubmission$Sales [ dataSubmission$Open == 0 ] <- 0
    dataSubmission$Sales [ is.na ( dataSubmission$Sales ) ] <- 0
    dataSubmission$Sales [ is.nan ( dataSubmission$Sales ) ] <- 0
    
    print("Exporting")
    write.table ( dataSubmission [ c ( "Id", "Sales" ) ], paste(pwd, filename, sep=""), sep=",", row.names = F )  
    print("Done!")
}

addStoreDerived <- function(dataStores, dataSales)
{
    # Average sales by store
    dataStores$AvgSales = tapply ( dataSales$Sales, dataSales$Store, FUN = mean )
    dataStores$LogAvgSales <- log( dataStores$AvgSales )
    
    # Determine which stores are seasonal stores (high summer sales)
    
    # Average summer/winter sales by store
    dataSales$month = month(as.Date(dataSales$Date))
    dataSales$SummerMonthDummy <- (dataSales$month== 6)|(dataSales$month==7)|(dataSales$month==8)
    dataSales$WinterMonthDummy <- (dataSales$month==12)|(dataSales$month==1)|(dataSales$month==2)
    dataStores$AvgSummerSales = tapply ( dataSales$Sales[dataSales$SummerMonthDummy==1], dataSales$Store[dataSales$SummerMonthDummy==1], FUN = mean )
    dataStores$AvgWinterSales = tapply ( dataSales$Sales[dataSales$WinterMonthDummy==1], dataSales$Store[dataSales$WinterMonthDummy==1], FUN = mean )
    dataStores$SeasonSalesRatio <- dataStores$AvgSummerSales/dataStores$AvgWinterSales
    dataStores$SummerBoost <- (dataStores$SeasonSalesRatio > 1.5)
    
    return(dataStores)
}

addDateDerived <- function(dataTraining)
{    
    print("Adding quick calculations")
    dataTraining$Month = months(as.Date(dataTraining$Date))
    dataTraining$Year = format ( as.Date ( dataTraining$Date ), '%Y' )
    dataTraining$DayOfWeekDummy <- as.character(dataTraining$DayOfWeek)
    dataTraining$DayOfWeek_Named <- format(as.Date(dataTraining$Date),"%a")
    dataTraining$MonFri <- dataTraining$DayOfWeek_Named == "Mon" | dataTraining$DayOfWeek_Named == "Fri"
    
    print("Adding competition info")
    dataTraining <- addCompetitionFlag(dataTraining) #$CompetitionOpen
    dataTraining$CompetitionNONE <- abs(dataTraining$CompetitionOpen - 1) 
    dataTraining$LogCompDistance <- log ( dataTraining$CompetitionDistance + .01)
    
    print("Adding promotion info")
    dataTraining <- addPromo2Flag(dataTraining) #$Promo2
    dataTraining$NumPromos <- dataTraining$Promo + dataTraining$Promo2
    
    print("Adding season data")
    dataTraining <- addSeason(dataTraining) # $Season   
    
    return(dataTraining)
}

addCompetitionFlag <- function(dataTraining)
{
    dataTraining$CompetitionOpen <- as.integer ( as.Date ( paste ( dataTraining$CompetitionOpenSinceYear, dataTraining$CompetitionOpenSinceMonth, "1", sep = "-" ), "%Y-%m-%d" ) <= as.Date ( dataTraining$Date ) )
    dataTraining$CompetitionOpen [ is.na ( dataTraining$CompetitionOpen ) ] <- 0 
    
    return(dataTraining)
}    
 
addPromo2Flag <- function(dataTraining)
{
    # Reorder factors for math to determine if we're in the promo2 month
    # Note: the factor order may be different in the test dataset
    
    dataTraining$PromoInterval <- factor ( dataTraining$PromoInterval, levels ( dataTraining$PromoInterval )[c(1,4,3,2)] )
    # Flag to determine eligibility for promo 2
    dataTraining$Promo2Active <- as.integer (
        ( ( as.integer ( format ( as.Date ( dataTraining$Date ), "%Y%U" ) ) > ( ( dataTraining$Promo2SinceYear * 100 ) + dataTraining$Promo2SinceWeek ) )
          & ( ( as.integer ( format ( as.Date ( dataTraining$Date ), "%m" ) ) %% 3 ) == ( as.integer ( dataTraining$PromoInterval ) - 2 ) ) )
    )
    return(dataTraining)
}
addSeason <- function(dataTraining)
{
    dataTraining$Season <- ""
    dataTraining$Season[dataTraining$SchoolHoliday == 1 & (month(as.Date(dataTraining$Date)) == 12 | month(as.Date(dataTraining$Date)) == 1)] <- "Christmas_Break"
    dataTraining$Season[dataTraining$SchoolHoliday == 1 & (month(as.Date(dataTraining$Date)) == 3 | month(as.Date(dataTraining$Date)) == 4)] <- "Easter_Break"
    dataTraining$Season[dataTraining$SchoolHoliday == 1 & (month(as.Date(dataTraining$Date)) == 7 | month(as.Date(dataTraining$Date)) == 8 | month(as.Date(dataTraining$Date)) == 9)] <- "Summer_Break"
    dataTraining$Season[dataTraining$SchoolHoliday == 1 & month(as.Date(dataTraining$Date)) == 10 ] <- "Fall_Break"
    dataTraining$Season[dataTraining$Season == "" & (month(as.Date(dataTraining$Date)) == 12 | month(as.Date(dataTraining$Date)) == 1 | month(as.Date(dataTraining$Date)) == 2)] <- "Winter"
    dataTraining$Season[dataTraining$Season == "" & (month(as.Date(dataTraining$Date)) == 3 | month(as.Date(dataTraining$Date)) == 4 | month(as.Date(dataTraining$Date)) == 5)] <- "Spring"
    dataTraining$Season[dataTraining$Season == "" & (month(as.Date(dataTraining$Date)) == 6 | month(as.Date(dataTraining$Date)) == 7 | month(as.Date(dataTraining$Date)) == 8)] <- "Summer"
    dataTraining$Season[dataTraining$Season == "" & (month(as.Date(dataTraining$Date)) == 9 | month(as.Date(dataTraining$Date)) == 10 | month(as.Date(dataTraining$Date)) == 11)] <- "Fall"
    
    dataTraining$SummerMonthDummy <- 0
    dataTraining$SummerMonthDummy[ dataTraining$Season == "Summer" | dataTraining$Season == "Summer_Break" ] <- 1
    dataTraining$WinterMonthDummy <- 0 
    dataTraining$WinterMonthDummy[ dataTraining$Season == "Winter" | dataTraining$Season == "Christmas_Break" ] <- 1
  
    return(dataTraining)
}

addReopenedFlag <- function(dset)
{

    #print("Sorting table")
    dset <- dset[order(dset$Store, dset$Date),]
    
    # the vector to return
    Reopened <- rep(0, length(dset$Open))
    
    # make a vector of true/false according to if there is a change or not
    diffs <- dset$Open[-1L] != dset$Open[-length(dset$Open)]; #diffs
    
    # make a vector of the last indexes of each particular run 
    idx <- c(which(diffs), length(diffs)); #idx
    
    # this vector counts how long each run lasted
    runs <- diff(c(1, idx)); #runs
    
    # find index values of the last day of each long run and length of each run
    # NOTE: IF HAVE PROBLEMS, MAYBE JUST FLAG 14 DAYS AND UP HERE RATHER THAN 2
    poss <- idx[runs > 2]; #poss
    lengths <- runs[runs > 2]; #lengths
    
    # check each long run to see if it is a run of closures
    numRuns <- length(poss); #numRuns
    #print("Checking runs")
    for(i in 1:numRuns)
    {
        if(dset$Open[poss[i]] == 0) # this run was of closed days
        {
            # mark the first days of REOPENING as "bump"   
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
            #print(paste("For a run of", lengths[i], "cap is", cap))
            j <- 1
            while(j <= cap & dset$Store[poss[i] + j] == currStore)
            {
                #print(paste("Put a 1 in", (poss[i] + j)))
                Reopened[poss[i] + j] <- 1
                j <- j + 1
            }
        }   
    } 
    #print(paste("done.  Reopened has values of"))
    #print(unique(Reopened))
    return(Reopened)
}    





