################################################################################
# Exploratory Data Analysis - John's Hopkins University | Coursera             #
# Week 4 Course Project                                                        #
# Wayne Heller                                                                 #
# 2/25/2017                                                                    #
# Assignment- to recreate plots from the EPA National Emissions Inventory      #
#                                                                              #
#                                                                              #
# Key Assumptions:                                                              #
#     Data file summarySCC_PM25.rds is in the working directory                #
#     Libaries ggplot2 and dplyr have been loaded                              #
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


# Assignment: Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008? 

createPlot4 <- function() {
    
    dfSCC <- readSourceClassificationCodes()
    # check for error: data file not found
    if(class(dfSCC)!="data.frame") {
        stop()
    }
    
    # Find the SCC codes where the EI.Sector contains "Coal"
    coalSCCCodes <- dfSCC$SCC[grep("Coal", dfSCC$EI.Sector, fixed = TRUE)]
    
    # Read in the dataset
    dfNEI <- readEmissionsData()
    
    # check for error: data file not found
    if(class(dfNEI)!="data.frame") {
        stop()
    }
    
    # filter the results to just those from the Coal sectors
    dfNEICoal <- dfNEI[dfNEI$SCC %in% coalSCCCodes, ]
    
    # Group By Year 
    byYear <- group_by(dfNEICoal, year)
    # Sum up all the Emissions
    dfSum <- summarize(byYear, sum(Emissions))
    # Rename columns for convenience
    names(dfSum) <- c('year', 'emissions')
    
    
    # Plot the total amount of emissions by year in Kilotons breaking up each
    # Type into a facet
    
    p <- ggplot(dfSum, aes(year, emissions / 1000)) + geom_point() + 
         geom_smooth(method = "lm", col="red", se=FALSE) +
        labs(y="PM2.5 Emissions (Kilotons)", x="Year", 
             title="PM2.5 Emissions Have Decreased Across the United States from 1999 to 2008",
             subtitle="From All Coal Combustion Related Sources")
    
    ggsave("plot4.png", plot = p, units= "in", width = 11.0, height= 5.5)
    
}