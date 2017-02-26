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


# Assignment: How have emissions from motor vehicle sources changed from 
# 1999–2008 in Baltimore City?

createPlot5 <- function() {
    
    dfSCC <- readSourceClassificationCodes()
    # check for error: data file not found
    if(class(dfSCC)!="data.frame") {
        stop()
    }
    
    # Find the SCC codes where the SCC.Level.One contains "Mobile Sources"
    motorVehicleSCCCodes <- dfSCC$SCC[grep("Mobile Sources", dfSCC$SCC.Level.One, 
                                         fixed = TRUE)]
    
    # Read in the dataset
    dfNEI <- readEmissionsData()
    
    # check for error: data file not found
    if(class(dfNEI)!="data.frame") {
        stop()
    }
    
    # filter the results to just those from Motor Vehicles in Baltimore City
    dfNEIFiltered <- dfNEI[dfNEI$SCC %in% motorVehicleSCCCodes & 
                               dfNEI$fips=="24510", ]
    
    # Group By Year 
    byYear <- group_by(dfNEIFiltered, year)
    # Sum up all the Emissions
    dfSum <- summarize(byYear, sum(Emissions))
    # Rename columns for convenience
    names(dfSum) <- c('year', 'emissions')
    
    
    # Plot the total amount of emissions by year in Kilotons breaking up each
    # Type into a facet
    
    p <- ggplot(dfSum, aes(year, emissions / 1000)) + geom_point() + 
        geom_smooth(method = "lm", col="red", se=FALSE) +
        labs(y="PM2.5 Emissions (Kilotons)", x="Year", 
             title="PM2.5 Emissions Have Decreased in Baltimore City, MD from 1999 to 2008",
             subtitle="From All Motor Vehicle Sources")
    
    ggsave("plot5.png", plot = p, units= "in", width = 11.0, height= 5.5)
    
}