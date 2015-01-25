# Project 2 - Fine particulate matter (PM2.5) analysis
#  
# Q: Across the United States, how have emissions 
#    from coal combustion-related sources changed from 1999–2008?
#
# A: The following can be inferred about the emissions from coal based combustion sources.       
#    * Most states have emissions well below the average from 1999 to 2008.      
#    * Few states have had close to average emissions in 1999 but have since improved to below average levels by 2008.
#    * Only one state has had an increasing trend from below average to close to average from 1999 to 2008.
#
#
# NOTES:
# US County codes
#   https://www.census.gov/geo/reference/codes/cou.html
# US State codes
#   https://www.census.gov/geo/reference/docs/state.txt
#
# USAGE on command line:
#   Rscript plot4.R
# 
# Note: Path to data and PNG files are hard-coded
#
# ------------------------------------------------
# Load libraries
library(dplyr)
library(tidyr)
library(maps)
library(mapproj)
library(ggplot2)
#
# Read the air-quality files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#
# Read FIPS file for county and state codes
COU <- read.csv("national_county.txt",
                header=TRUE,
                colClasses=rep("character",5))
STA <- read.csv("state.txt",
                header=TRUE,
                sep="|",
                colClasses=rep("character",4))
#
# Load into data frames for dplyr to work with
couDf<-tbl_df(COU)
staDf<-tbl_df(STA)
#
# Add a new column for fips to match with the fips column in NEI data
fipsCode<-mutate(couDf,
                 fips=paste(couDf$State.ANSI,
                            couDf$County.ANSI,
                            sep=""))
#
# Merge with data set of states to get full state names from state codes in fips dataset
fipsDf<-merge(fipsCode,
              staDf,
              by.x='State',
              by.y='STUSAB')
#
# Load into data frames for dplyr to work with
neiDf<-tbl_df(NEI)
sccDf<-tbl_df(SCC)
#
# Create a SCC file for coal combustion based emissions by looking for string Coal
# and combustion in SCC file. A purist would look for all versions (upper and lower case)
# of the string; but, in this file a quick browse showed that the string Coal
# exists in no other form.
coalScc<-filter(sccDf,
                grepl('Combustion',SCC.Level.One) & 
                  grepl('Coal',Short.Name))
# Merge with NEI data set to get NEI data set for Coal based emissions only
coalNei<-merge(neiDf,
               coalScc,
               by.x="SCC",
               by.y="SCC")
#
# Merge with fips dataset to get coressponding state name
coalNeiStates<-merge(coalNei,
                     fipsDf,
                     by.x='fips',
                     by.y='fips')
# Create summary files for NEI coal based emissions
emissionsYearly<-
  summarise(
    group_by(coalNeiStates,year,STATE_NAME),        # Group on year and state names
    totalEmissions=sum(Emissions)/1000)             # Sum the Emissions amount
                                                    # Divide the sum by 10^3 for clarity on map
#
# We now have a data set with yearly emissions for every state in US.
#
# Assign emissions output to US state code from maps dataset.
statesMap<-map_data("state")
emissionsYearly<-mutate(emissionsYearly,
                        STATE_NAME=tolower(STATE_NAME))         # Lower case is needed to merge with dataset
                                                                # in map_data
emissionsMap<-merge(emissionsYearly,
                    statesMap,
                    by.x="STATE_NAME",
                    by.y="region")
emissionsMap<-arrange(emissionsMap,                             # Re-arrange because merging changes the order
                      year,
                      group,
                      order)
#
# We now have a dataset with yearly emissions for every state in US matched with states code in US
#
# Plot the emissions on US map 
# Code tips :
#   R-Graphics Cookbook - http://www.cookbook-r.com/Graphs/
#   http://stackoverflow.com/a/13888731/919480 
#   http://html-color-codes.info/ 
#
p <- ggplot(emissionsMap,
            aes(x=long,y=lat,
                map_id=STATE_NAME,
                fill=totalEmissions))                                          +
            geom_map(map = statesMap, 
                     colour="black")                                           +
            scale_fill_gradientn(colours=c("#05FFE7","#FFE305","#FF0505"),
                                 limits=c(0.0,200.0),
                                 breaks=c(0.0,100.0,200.0),
                                 labels=format(c(0.0,100.0,200.0)))            +
            expand_limits(x=emissionsMap$long,
                          y=emissionsMap$lat)                                  +
            coord_map("polyconic")                                             +
            ggtitle("PM2.5 emissions by coal based combustion sources\n")
# Add the facet now  
p <- p + facet_grid(year ~ .)
#
# Save image to PNG file
ggsave("plot4.png",
       p,
       width=8,
       height=10,
       units="in")            
