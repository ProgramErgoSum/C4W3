# Project 2 - Fine particulate matter (PM2.5) analysis
#  
# Q: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#    (fips == "24510") from 1999 to 2008?
# A: YES, there is a declining trend - with a pike in between - as also seen by the linear regression model
#
# USAGE on command line:
#   Rscript plot2.R
# 
# Note: Path to data and PNG files are hard-coded
#
# ------------------------------------------------
# Load libraries
library(dplyr)
#
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# Summarise the emissions for years
neiDf<-tbl_df(NEI)
baltimoreEmissions<-
  filter(neiDf,fips=="24510")
emissionsYearly<-
  summarise(group_by(baltimoreEmissions,year),     # Group on year
            totalEmissions=sum(Emissions)/1000)    # Divide by 10^3 for clarity in the plot

# Open a PNG device
png(filename="plot2.png")
# Plot the summary
plot(emissionsYearly$year,                          # X-axis is year
     emissionsYearly$totalEmissions,                # Y-axis is pollutant mass in thousands of tons
     xlab="Year",                                   # X-axis label
     ylab="PM2.5 Pollutant (thousand tons)",        # Y-axis label
     main="Total PM2.5 emissions from all sources", # Chart title
     sub="Balitmore, Maryland",                     # Sub-title
     col.main="orangered2",                         # Set orange color for the title
     col.sub="orangered2",                          # Set orange color for the sub-title
     pch=16,                                        # Plot character is diamond
     col="blue"                                     # and blue in colour
)
# Fit a linear model to see the trend
abline(lm(emissionsYearly$totalEmissions~emissionsYearly$year))
# Copy to PNG
dev.off()