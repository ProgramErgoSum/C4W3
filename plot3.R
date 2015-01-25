# Project 2 - Fine particulate matter (PM2.5) analysis
#  
# Q: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources 
# have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008?#
# A: 
#
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
# Summarise the emissions for years
neiDf<-tbl_df(NEI)
baltimoreEmissions<-
  filter(neiDf,fips=="24510")
emissionsYearly<-
  summarise(group_by(baltimoreEmissions,type,year), # Group on type and year
            totalEmissions=sum(Emissions)/1000)     # Divide by 10^3 for clarity in the plot

# Plot the summary
p<- ggplot(emissionsYearly,
           aes(x=year,                          # X-axis is year
               y=totalEmissions))               # Y-axis is pollutant mass in thousands of tons
p<- p + geom_point(shape=16,colour="blue")      # Points of plot
p<- p + geom_smooth(method=lm)
p<- p + xlab("Year")                            # X-axis labesl
p<- p + ylab("Total emissions (thousand tons)") # Y-axis labels
p<- p + ggtitle("Total PM2.5 emissions by all sources\nBaltimore, Maryland")
                                                # Chart title
# Add the facet now  
p + facet_grid(type ~ .)
#
ggsave("plot3.png")