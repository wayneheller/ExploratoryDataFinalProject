################################################################################
# Exploratory Data Analysis - John's Hopkins University | Coursera             #
# Week 4 Course Project                                                        #
# Wayne Heller                                                                 #
# 2/25/2017                                                                    #
# Assignment- to recreate plots from the EPA National Emissions Inventory      #
#                                                                              #
#                                                                              #
# Key Assumtions:                                                              #
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


# Assignment: Of the four types of sources indicated by the type (point, 
# nonpoint, onroad, nonroad) variable, which of these four sources have seen 
# decreases in emissions from 1999–2008 for Baltimore City? Which have seen 
# increases in emissions from 1999–2008? 

createPlot3 <- function() {
    
    # Read in the dataset
    dfNEI <- readEmissionsData()
    
    # check for error: data file not found
    if(class(dfNEI)!="data.frame") {
        stop()
    }
    
    # filter the results to just Baltimore City
    dfNEIBaltimoreCity <- dfNEI[dfNEI$fips=="24510", ]
    
    # Group By Year and Type
    byYearType <- group_by(dfNEIBaltimoreCity, year, type)
    # Sum up all the Emissions
    dfSum <- summarize(byYearType, sum(Emissions))
    # Rename columns for convenience
    names(dfSum) <- c('year', 'type', 'emissions')
    

    # Plot the total amount of emissions by year in Kilotons breaking up each
    # Type into a facet
    p <- ggplot(dfSum, aes(year, emissions / 1000)) + geom_point() + 
        facet_grid(.~type) + geom_smooth(method = "lm", col="red", se=FALSE) +
        labs(y="PM2.5 Emissions (Kilotons)", x="Year", 
             title="PM2.5 Emissions Have Decreased in Baltimore City from 1999 to 2008 for All Source Types Except POINT")
    ggsave("plot3.png", plot = p, units= "in", width = 11.0, height= 5.5)
    
}