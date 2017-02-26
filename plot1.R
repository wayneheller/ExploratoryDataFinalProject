################################################################################
# Exploratory Data Analysis - John's Hopkins University | Coursera             #
# Week 4 Course Project                                                        #
# Wayne Heller                                                                 #
# 2/25/2017                                                                    #
# Assignment- to recreate plots from the EPA National Emissions Inventory      #
#                                                                              #
#                                                                              #
# Key Assumtion: assumes data file summarySCC_PM25.rds is in the               #
# working directory                                                            #
################################################################################

# Loads the data.frame from the source file
# First checks for the existence of the file and prints an error message if
# not found
readEmissionsData <- function() {
    if(file.exists("summarySCC_PM25.rds")){
        NEI <- readRDS("summarySCC_PM25.rds")
        return(NEI)
    }
    else {
        print("Set Working Directory to location of summarySCC_PM25.rds")
        return(FALSE)
    }
}

readSourceClassificationCodes <- function() {
    
    if(file.exists("Source_Classification_Code.rds")){
        SCC <- readRDS("Source_Classification_Code.rds")
        return(SCC)
    }
    else {
        print("Set Working Directory to location of Source_Classification_Code.rds")
        return(FALSE)
    }
}


# Assignment: Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? 
# createPlot1 makes a plot using the base plotting system showing the total 
# PM2.5 emission from all sources for each of the years:
# 1999, 2002, 2005, and 2008.

createPlot1 <- function() {
    
    # Read in the dataset
    dfNEI <- readEmissionsData()
    
    # check for error: data file not found
    if(class(dfNEI)!="data.frame") {
        stop()
    }
    
    # summarize emissions by year across all sources
    emissionsByYear <- with(dfNEI, tapply(Emissions, year, sum))
    
    # close any open graphics devices
    # this clears out any existing settings
    while(length(dev.list()>1)) {
        dev.off()
    }
    
    # create PNG plotting device
    devPNG <- png("plot1.png", width=650)
    
    par(las=1) # Set Y axis label to print horizontally
    
    # Plot the total amount of emissions by year in Megatons
    plot(names(emissionsByYear), emissionsByYear / 1000000.0, 
         pch=20, type = "p", xlab="Year", ylab="PM2.5 Emissions (Megatons)")
    
    abline(lm(emissionsByYear / 1000000.0 ~ as.integer(names(emissionsByYear))), lwd = 2, col="red")

    title(main = "Total Emissions from PM2.5 have Decreased in the United States from 1999 to 2008")
    
    dev.off()
}