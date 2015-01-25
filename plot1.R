# Project 2 - Fine particulate matter (PM2.5) analysis
#  
# Q: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# A: YES, there is a definite declining trend as also seen by the linear regression model
#
# USAGE on command line:
#   Rscript plot1.R
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
emissionsYearly<-
  summarise(group_by(neiDf,year),                  # Group on year
            totalEmissions=sum(Emissions)/1000000) # Divide by 10^6 for clarity in the plot

# Open a PNG device
png(filename="plot1.png")
# Plot the summary
plot(emissionsYearly$year,                          # X-axis is year
     emissionsYearly$totalEmissions,                # Y-axis is pollutant mass in millions of tons
     xlab="Year",                                   # X-axis label
     ylab="PM2.5 Pollutant (million tons)",         # Y-axis label
     main="Total PM2.5 emissions from all sources", # Chart title
     sub="All US counties",                         # Chart sub-title
     col.main="orangered2",                         # Set orange color for the title
     col.sub="orangered2",                          # Set orange color for the sub-title
     pch=16,                                        # Plot character is diamond
     col="blue"                                     # and blue in colour
)
# Fit a linear model to see the trend
abline(lm(emissionsYearly$totalEmissions~emissionsYearly$year))
# Copy to PNG
dev.off()