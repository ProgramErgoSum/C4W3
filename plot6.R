# Project 2 - Fine particulate matter (PM2.5) analysis
#  
# Q: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#    Which city has seen greater changes over time in motor vehicle emissions?
# A: Baltimore City, Maryland
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
fipsEmissions<-filter(neiDf,
                      fips=="24510" | fips == "06037")
# Get SCC data relevant to motor vehicles
sccMotorVehicles<-filter(SCC,
                         Data.Category=='Onroad')
# Get motor vehicle emissions
mvEmissions<-merge(fipsEmissions,
                   sccMotorVehicles,
                   by.x='SCC',
                   by.y='SCC')
# Summarise emissions
emissionsYearly<-
  summarise(group_by(mvEmissions,year,fips), # Group on year and fips
            totalEmissions=sum(Emissions)/1000)      
# Change names of fips
levels(emissionsYearly$fips)[levels(emissionsYearly$fips)=="06037"] <- c("Los Angeles County, California")
levels(emissionsYearly$fips)[levels(emissionsYearly$fips)=="24510"] <- c("Baltimore City, Maryland")
# Plot the summary
p<- ggplot(emissionsYearly,
           aes(x=year,                          # X-axis is year
               y=totalEmissions))               # Y-axis is pollutant mass in thousands of tons
p<- p + geom_point(shape=16,colour="blue")      # Points of plot
p<- p + geom_smooth(method=lm)
p<- p + xlab("Year")                            # X-axis labesl
p<- p + ylab("Total emissions (thousand tons)") # Y-axis labels
p<- p + ggtitle("PM2.5 motor vehicle emissions")
p<- p + facet_grid(fips ~ .)
#
ggsave("plot6.png",
       p,
       width=8,
       height=10,
       units="in")            