NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#head(NEI)

#head(SCC)

library(ggplot2)
library(plyr)

## Converting "year", "type", "Pollutant", "SCC", "fips" to factor
colToFactor <- c("year", "type", "Pollutant","SCC","fips")
NEI[,colToFactor] <- lapply(NEI[,colToFactor], factor)

head(levels(NEI$fips))

## The levels have NA as "   NA", so converting that level back to NA
levels(NEI$fips)[1] = NA
NEIdata<-NEI[complete.cases(NEI),]
colSums(is.na(NEIdata))

## The levels have NA as "   NA", so converting that level back to NA
levels(NEI$fips)[1] = NA
NEIdata<-NEI[complete.cases(NEI),]
colSums(is.na(NEIdata))

## plot
totalEmission <- aggregate(Emissions ~ year, NEIdata, sum)
totalEmission
barplot(
  (totalEmission$Emissions)/10^6,
  names.arg=totalEmission$year,
  xlab="Year",
  ylab="PM2.5 Emissions (10^6 Tons)",
  main="Total PM2.5 Emissions From All US Sources"
)
