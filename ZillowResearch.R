library(zoo)
library(ggmap)
library(Rcpp)
library(zipcode)

setwd("c:/users/reeta/downloads")

data1 <- read.csv(file="Zip_MedianSoldPrice_AllHomes.csv")
MedianSoldPrice <- data1[c(2:7,seq(175,250,by=3))]
# MedianSoldPrice <- data1[c(1:7,seq(10,250,by=6))]

data2 <- read.csv(file="Zip_MedianListingPrice_AllHomes.csv")
MedianListingPrice <- data2[c(1:6, seq(9,84,by=3))]

head(MedianSoldPrice)
head(MedianListingPrice)


dim(MedianSoldPrice)
dim(MedianListingPrice)

  library(sqldf)

ListingToSoldRatio <- sqldf("select s.RegionName, 
                            s.City as City, 
                            s.State as State, 
                            s.Metro as Metro, 
                            s.CountyName as CountyName, 
                            s.SizeRank as SizeRank ,
                            l.[X2010.03]/s.[X2010.03] as [X2010.03],
                            l.[X2010.06]/s.[X2010.06] as [X2010.06],
                            l.[X2010.09]/s.[X2010.09] as [X2010.09],
                            l.[X2010.12]/s.[X2010.12] as [X2010.12],
                            l.[X2011.03]/s.[X2011.03] as [X2011.03],
                            l.[X2011.06]/s.[X2011.06] as [X2011.06],
                            l.[X2011.09]/s.[X2011.09] as [X2011.09],
                            l.[X2011.12]/s.[X2011.12] as [X2011.12],
                            l.[X2012.03]/s.[X2012.03] as [X2012.03],
                            l.[X2012.06]/s.[X2012.06] as [X2012.06],
                            l.[X2012.09]/s.[X2012.09] as [X2012.09],
                            l.[X2012.12]/s.[X2012.12] as [X2012.12],
                            l.[X2013.03]/s.[X2013.03] as [X2013.03],
                            l.[X2013.06]/s.[X2013.06] as [X2013.06],
                            l.[X2013.09]/s.[X2013.09] as [X2013.09],
                            l.[X2013.12]/s.[X2013.12] as [X2013.12],
                            l.[X2014.03]/s.[X2014.03] as [X2014.03],
                            l.[X2014.06]/s.[X2014.06] as [X2014.06],
                            l.[X2014.09]/s.[X2014.09] as [X2014.09],
                            l.[X2014.12]/s.[X2014.12] as [X2014.12],
                            l.[X2015.03]/s.[X2015.03] as [X2015.03],
                            l.[X2015.06]/s.[X2015.06] as [X2015.06],
                            l.[X2015.09]/s.[X2015.09] as [X2015.09],
                            l.[X2015.12]/s.[X2015.12] as [X2015.12],
                            l.[X2016.03]/s.[X2016.03] as [X2016.03],
                            l.[X2016.06]/s.[X2016.06] as [X2016.06]
                            from MedianSoldPrice s inner join MedianListingPrice l on s.RegionName= l.RegionName ")

head(ListingToSoldRatio)


MedianListingPrice98075 <- sqldf("select * from MedianListingPrice where RegionName=98075")
MedianSoldPrice98075 <- sqldf("select * from MedianSoldPrice where RegionName=98075")

a <- as.numeric(t(MedianListingPrice98075[1,-(1:6)]))
acol <- colnames(MedianListingPrice98075[1,-(1:6)])
acol <- substr(acol,2,8)
aDate <- as.yearmon(acol,"%Y.%m")
z.MedianListingPrice98075 <- zoo(a, order.by=aDate)

b <- as.numeric(t(MedianSoldPrice98075[1,-(1:6)]))
bcol <- colnames(MedianSoldPrice98075[1,-(1:6)])
bcol <- substr(bcol,2,8)
bDate <- as.yearmon(bcol,"%Y.%m")
z.MedianSoldPrice98075 <- zoo(b, order.by=bDate)


plot(z.MedianListingPrice98075, type="l" , col="blue",ylim=c(550000,1000000),yaxt="n", lwd=2)
myTicks = axTicks(2)
axis(2, at = myTicks, labels = formatC(myTicks, format = 'd'))
#axis(2,at=myTicks,labels=format(myTicks,scientific=FALSE))
lines(z.MedianSoldPrice98075, col="red", lwd=2)
legend("topleft", c("List Price", "Sold Price"), col=c("blue","red"),lty=1, lwd=2)

ListingToSoldRatio_June2016 <- sqldf("select RegionName, 
                              City, 
                              State, 
                              [X2016.06]
                            from ListingToSoldRatio")

ListingToSoldRatio_June2016 <- na.omit(ListingToSoldRatio_June2016)

#ListingToSoldRatio_June2016 <- cbind(ListingToSoldRatio_June2016, geocode(ListingToSoldRatio_June2016$RegionName))

ListingToSoldRatio_June2016_WA <- ListingToSoldRatio_June2016[which(ListingToSoldRatio_June2016$State=="WA"),]

ListingToSoldRatio_June2016_WA <- na.omit(ListingToSoldRatio_June2016_WA)

data(zipcode)
ListingToSoldRatio_June2016_WA <- merge(ListingToSoldRatio_June2016_WA, zipcode, by.x="RegionName", by.y="zip")


#ListingToSoldRatio_June2016_WA <- cbind(ListingToSoldRatio_June2016_WA,geocode(as.character(ListingToSoldRatio_June2016_WA$RegionName)))
ListingToSoldRatio_June2016_WA$Cat <- 
  ifelse(ListingToSoldRatio_June2016_WA$X2016.06 < 0.95, "Less than 0.95",   
         ifelse(ListingToSoldRatio_June2016_WA$X2016.06 < 1.00, "0.95 - 1",  
                ifelse(ListingToSoldRatio_June2016_WA$X2016.06 < 1.05,"1 - 1.05", "Greater than 1.05")))
  

WashingtonMap <-qmap("Redmond",maptype="terrain", zoom = 9, color="bw") 

WashingtonMap +                          
   geom_point(aes(x = longitude, y = latitude,  colour=X2016.06, size=X2016.06), data = ListingToSoldRatio_June2016_WA)    ##This adds the points to it

WashingtonMap+ 
geom_point(aes(x = longitude, y = latitude, colour=Cat, size=4), data = ListingToSoldRatio_June2016_WA)    ##This adds the points to it

