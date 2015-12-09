setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/Final_Paper")

load("EVdata1.rda")

library(dplyr)
library(plyr)
library(maptools)
library(ggmap)
library(ggplot2)
library(RColorBrewer)

# Regional Summary of Interest
Region <- c('NorthEast','NorthWest','YorkshireHumber','EastMidlands', 'WestMidrands',
            'EastofEngland','London','SouthEast','SouthWest','Wales', 'Scotland')
IntRegion <- c(mean(EVINTEREST$EVinterest[EVINTEREST$NorthEast==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$NorthWest==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$YorkshireHumber==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$EastMidlands==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$WestMidrands==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$EastofEngland==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$London==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$SouthEast==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$SouthWest==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$Wales==1]),
               mean(EVINTEREST$EVinterest[EVINTEREST$Scotland==1]))
RegionDF <- data.frame(OID=c(1,2,3,4,5,6,7,8,9,10,11),
                       Region=Region, Interest=IntRegion)

barplot(IntRegion, names.arg=Region)


# shapefile
tmp_dir = tempdir()
url_data = "https://geoportal.statistics.gov.uk/Docs/Boundaries/Regions_(GB)_2014_Boundaries_(Generalised_Clipped).zip"
zip_file = sprintf("%s/shpfile.zip", tmp_dir)
download.file(url_data, zip_file)
unzip(zip_file, exdir = tmp_dir)
gor=readShapeSpatial(sprintf('%s/RGN_DEC_2014_GB_BGC.shp', tmp_dir))

# object ID to merge with EV interest data
gor@data$OID <- c(1,2,3,4,5,6,7,8,9,11,10)

gor@data$id <- rownames(gor@data)
sh.df <- as.data.frame(gor)
sh.fort <- fortify(gor)
sh.line<- left_join(sh.fort, sh.df , by = "id" )
sh.line <- sh.line[ order( sh.line$order ) , ]

mapdf <- merge( sh.line , RegionDF , by.x= "OID", by.y="OID" , sort = FALSE)
mapdf <- mapdf[ order( mapdf$order ) , ]

# plotting map
map1 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = Interest, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()

# save the plot in the folder
png(filename='./map1.png')
plot(map1)
dev.off()