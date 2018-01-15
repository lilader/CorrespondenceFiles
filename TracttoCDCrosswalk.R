##Creating a crosswalk that allocates tract level data to community districts in NYC
#The output file indiates the percentage of the population of a tract that lives in each CD it is part of

#The basic outline is:
#build 'inhabited area' Block Groups out of a Block shapefile by taking out the blocks that were uninhabited in the most recent decenial census Block level population counts
#calculate the proportion of the 'inhabited area' of each block group that falls into each CD.
#Use this area fraction to allocate the most recent 5 year ACS Block Group level population estimates
#aggregate by tract to arrive at statements such as this: 47% of the population of Tract X lives in CD Y, and 53% lives in CD Z


library(rgdal)
library(ggplot2)
library(ggmap)
library(RgoogleMaps)
library(broom)
library(RColorBrewer)
library(sp)
library(maptools)
library(raster)
library(dplyr)

options(scipen=999)


#pull in a google maps basemap so you can look at your shapefiles with some context
CenterOfMap <- geocode("Brooklyn,NY")

BK <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 11, maptype = "terrain", source = "google")
NYMap <- ggmap(BK)
NYMap


#read the Block shapefile
#readOGR takes two arguments: the directory your shapefile is located in and the name of the shapefile without any extension

Block <- readOGR("C:/Users/laderman/Documents/Reference Shapefiles", "NYC_Block")
#we need all the shapefiles to be in the same projection to do analysis
Block_WGS84<-spTransform(Block, CRS("+proj=longlat +datum=WGS84"))
#tidy is a command for telling R how to draw polygons from a shapefile
Block_WGS84_map<-tidy(Block_WGS84)
Block_Map <- NYMap + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.5,color='red', data=Block_WGS84_map, alpha=0)
Block_Map

#Add the block populations from 2010.  Download table P1 from FactFinder
Block_pop <- read.csv("C:/Users/laderman/Documents/Reference Shapefiles/DEC_10_SF1_P1_with_ann.csv")
Block_pop_tomerge <- Block_pop[,c(2,4)]
Block_pop_tomerge <- Block_pop_tomerge[-1,]
colnames(Block_pop_tomerge) <- c("GeoID","Population")

#merge the population into the Block shapefile
Block_WGS84<- merge(Block_WGS84,Block_pop_tomerge,by.x="GEOID10",by.y="GeoID")


#now we want to remove the area of the no pop blocks to create psudo block groups that are made up of only inhabited area
PopBlocks <- Block_WGS84[Block_WGS84@data$Population != 0,]
PopBlocks@data$BG <- substring(PopBlocks@data$BLOCKCE10,1,1)
PopBlocks@data$GeoID <- paste0(PopBlocks@data$STATEFP10,PopBlocks@data$COUNTYFP10,PopBlocks@data$TRACTCE10,PopBlocks@data$BG)

#its important to use the raster package rather than other spatial analysis packages because others drop all the data associated with a shapefile when you perform operations
PopBGs<-raster::aggregate(PopBlocks, by = 'GeoID', dissolve=TRUE)

#Add the block group populations 2015 5year from FactFinder
BG_pop <- read.csv("C:/Users/laderman/Documents/D2GNYC/2018/ACS_16_5YR_B01003_with_ann.csv")

#just grab the columns we need
BG_pop_tomerge <- BG_pop[,c(2,4)]
colnames(BG_pop_tomerge) <- c("GeoID","Population")

#merge the population into the BG shapefile
PopBGs<- merge(PopBGs,BG_pop_tomerge,by.x="GeoID",by.y="GeoID")

#read the Community Districts shapefile
CD <- readOGR("C:/Users/laderman/Documents/Reference Shapefiles", "NYC_CD_SP")
CD_WGS84<-spTransform(CD, CRS("+proj=longlat +datum=WGS84"))
CD_WGS84_map<-tidy(CD_WGS84)
CD_Map <- BG_Map + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.5,color='blue', data=CD_WGS84_map, alpha=0)
CD_Map


#OK now we are ready to start the process of interpolating the CD population

#first we want to drop the parks, cemetary, airport CDs so that we don't assign people to live there.  
CD59_WGS84 <- CD_WGS84[which(!(CD_WGS84@data$boro_cd %in% c(164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))),]
CD59_WGS84_map<-tidy(CD59_WGS84)
CD_Map <- BG_Map + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.5,color='blue', data=CD59_WGS84_map, alpha=0)
CD_Map

#intersect the BG and CD files to create pieces of BG that are in each CD
intersection<-raster::intersect(PopBGs,CD59_WGS84)

#then we need to know the area of the whole block group and each of its pieces, so add a column for the piece area
num.of.polys <- length(intersection@polygons)
all.polys <- 1:num.of.polys
areas.list <- sapply(all.polys, function(x) {
       my.area <- intersection@polygons[[x]]@area # the OS data contains area
       return(my.area)
   })
intersection@data$pieceAREA<-areas.list


#next we need to know the total area of the block group, so add the area of all the pieces in each BG and add that as a column as well
BG_total_area <- summarise(group_by(intersection@data, GeoID), BGArea= sum(pieceAREA))
intersection_w_area<-merge(intersection,BG_total_area, by.x="GeoID", by.y="GeoID")

#calculate the area fraction of each BG piece (ie what fraction of the inhabited BG area does the piece in CD X make up?)
intersection_w_area@data$AreaFrac <- intersection_w_area@data$pieceAREA/intersection_w_area$BGArea

#some pieces are super tiny slivers caused by differences in the boundaries lines drawn in the CD and Block shapefiles, we need to drop them and then recalculate area & area fraction
#We use a 1% area threshold - if a piece is less than 1% of the inhabited area of the BG it is considered an artifact of shapefile boundaries and not a real split
#we drop these pieces as essentially no-man's land - they tend to be along highways etc

intersection_wo_slivers <- intersection_w_area[which(intersection_w_area@data$AreaFrac>0.01 ), ]
BG_total_area_wo_slivers <- summarise(group_by(intersection_wo_slivers@data, GeoID), BGArea_wo_slivers= sum(pieceAREA))
intersection_w_area_wo_slivers<-merge(intersection_wo_slivers,BG_total_area_wo_slivers, by.x="GeoID", by.y="GeoID")
intersection_w_area_wo_slivers@data$AreaFrac <- intersection_w_area_wo_slivers@data$pieceAREA/intersection_w_area_wo_slivers$BGArea_wo_slivers


#calculate the population in each piece
intersection_w_area_wo_slivers@data$piecePop <- intersection_w_area_wo_slivers@data$Population*intersection_w_area_wo_slivers@data$AreaFrac


#now we want to calculate the fraction of the population of each tract that lives in each CD

#first extract county tract ID
intersection_w_area_wo_slivers@data$Tract<- substring(intersection_w_area_wo_slivers@data$GeoID,3,11)

#then find the total tract population and the population of each piece of the tract in a diferent CD, and calculate the fraction for each piece
TractPop<-summarise(group_by(intersection_w_area_wo_slivers@data,Tract), population = sum(piecePop))
TractCDPop<-summarise(group_by(intersection_w_area_wo_slivers@data,Tract,boro_cd), population = sum(piecePop))
TractCDPop<-merge(TractCDPop,TractPop,by="Tract")

TractCDPop$PopFrac<- TractCDPop$population.x/TractCDPop$population.y


#And finally add NY Boro numbers to tract numbers instead of FIPS in order to match with some NYC data files (ie voting data)
TractCDPop$TractID <- substring(TractCDPop$Tract,4,9)


TractCDPop$Boro[substring(TractCDPop$Tract,1,3) == "005"] <- "2"
TractCDPop$Boro[substring(TractCDPop$Tract,1,3) == "047"] <- "3"
TractCDPop$Boro[substring(TractCDPop$Tract,1,3) == "061"] <- "1"
TractCDPop$Boro[substring(TractCDPop$Tract,1,3) == "081"] <- "4"
TractCDPop$Boro[substring(TractCDPop$Tract,1,3) == "085"] <- "5"

TractCDPop$NYCTract <- paste0(TractCDPop$Boro,TractCDPop$TractID)

#and rename population.x and population.y
colnames(TractCDPop$population.x)<-colnames()

write.csv(TractCDPop, "TractCDCrosswalk_popweighted.csv")
