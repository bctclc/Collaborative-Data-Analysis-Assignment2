### Create maps for each group ###
### Claire & Noriko ###

### set working directory
setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/Shiny")

load("data4web.rda")

library(dplyr)
library(plyr)
library(maptools)
library(ggmap)
library(ggplot2)
library(RColorBrewer)

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


### create 18 maps

# total mean
map1 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = Interest, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
# save the plot in the folder
png(filename='ExportedMaps/map1.png')
plot(map1)
dev.off()

# female mean
map2 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntFemales, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map2.png')
plot(map2)
dev.off()

# male mean
map3 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntMales, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map3.png')
plot(map3)
dev.off()

# Young mean
map4 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntYoung, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map4.png')
plot(map4)
dev.off()

# Middle mean
map5 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntMiddleage, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map5.png')
plot(map5)
dev.off()

# Old mean
map6 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntOldage, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map6.png')
plot(map6)
dev.off()

# Low income mean
map7 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntLow, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map7.png')
plot(map7)
dev.off()

# Low-mid income mean
map8 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntLowmid, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map8.png')
plot(map8)
dev.off()

# High-mid income mean
map9 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntHighmid, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map9.png')
plot(map9)
dev.off()

# High income mean
map10 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntHigh, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map10.png')
plot(map10)
dev.off()

# College mean
map11 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntCollege, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map11.png')
plot(map11)
dev.off()

# Non College mean
map12 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntNocollege, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map12.png')
plot(map12)
dev.off()

# Licence mean
map13 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntLicence, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map13.png')
plot(map13)
dev.off()

# No Licence mean
map14 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntNolicence, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map14.png')
plot(map14)
dev.off()

# No Car mean
map15 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntNocar, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map15.png')
plot(map15)
dev.off()

# One Car mean
map16 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntOnecar, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map16.png')
plot(map16)
dev.off()

# Two Car mean
map17 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntTwocars, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map17.png')
plot(map17)
dev.off()

# Three Car mean
map18 <- ggplot(mapdf, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = IntThreecars, group = group), colour = "grey50")+
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) )+
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_equal()
png(filename='ExportedMaps/map18.png')
plot(map18)
dev.off()

