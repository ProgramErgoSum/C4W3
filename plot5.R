# Project 2 - Fine particulate matter (PM2.5) analysis
#  
# Q: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? 
# A: THere is a general decreasing trend in the emissions.
#
# USAGE on command line:
#   Rscript plot3.R
# 
# Note: Path to data and PNG files are hard-coded
#
# ------------------------------------------------
# Load libraries
library(dplyr)
library(ggplot2)
#
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# Get NEI data for Baltimore
neiDf<-tbl_df(NEI)
baltimoreEmissions<-filter(neiDf,fips=="24510")
# Get SCC data relevant to motor vehicles
sccMotorVehicles<-filter(SCC,Data.Category=='Onroad')
# Get Baltimore motor vehicle emissions
baltioreMvEmissions<-merge(baltimoreEmissions,
                           sccMotorVehicles,
                           by.x='SCC',
                           by.y='SCC')
# Summarise emissions
emissionsYearly<-
  summarise(group_by(baltioreMvEmissions,year), # Group on year
            totalEmissions=sum(Emissions))      

# Plot the summary
p<- ggplot(emissionsYearly,
           aes(x=year,                          # X-axis is year
               y=totalEmissions))               # Y-axis is pollutant mass in thousands of tons
p<- p + geom_point(shape=16,colour="blue")      # Points of plot
p<- p + geom_smooth(method=lm)
p<- p + xlab("Year")                            # X-axis labesl
p<- p + ylab("Total emissions")                 # Y-axis labels
p<- p + ggtitle("PM2.5 motor vehicle emissions in\nBaltimore, Maryland")
#
ggsave("plot5.png",
       p,
       width=8,
       height=10,
       units="in")            