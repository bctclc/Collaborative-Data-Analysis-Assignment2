### Load libraries
library(shiny)
library(leaflet)
library(httr)
library(dplyr)
library(XML)
library(maptools)
library(sp)
library(rgdal)
library(gsubfn)
library(proj4)

setwd("C:/Users/noriko/Desktop/Collaborative-Data-Analysis-Assignment2/Shiny")

### the dataframe...

### Prepare Shapefiles
# RegionSHP <- readOGR(dsn = "./Region", layer = "RGN_DEC_2014_GB_BGC")

# shapefile
tmp_dir = tempdir()
url_data = "https://geoportal.statistics.gov.uk/Docs/Boundaries/Regions_(GB)_2014_Boundaries_(Generalised_Clipped).zip"
zip_file = sprintf("%s/shpfile.zip", tmp_dir)
download.file(url_data, zip_file)
unzip(zip_file, exdir = tmp_dir)
gor=readShapeSpatial(sprintf('%s/RGN_DEC_2014_GB_BGC.shp', tmp_dir))

# object ID to merge with EV interest data
gor@data$OID <- c(1,2,3,4,5,6,7,8,9,11,10)


### Independent variables
Data <- read.csv("Data/data.csv")
Shapes_krs$numb.ini <- (Data$numb.ini-min(Data$numb.ini))/(max(Data$numb.ini)-min(Data$numb.ini))
Shapes_krs$unemployment <- (Data$unemployment-min(Data$unemployment, na.rm = TRUE))/(max(Data$unemployment, na.rm = TRUE)-min(Data$unemployment, na.rm = TRUE))
Shapes_krs$gender.ratio <- (Data$gender.ratio-min(Data$gender.ratio))/(max(Data$gender.ratio)-min(Data$gender.ratio))
Shapes_krs$young.per <- (Data$young.per-min(Data$young.per))/(max(Data$young.per)-min(Data$young.per))
Shapes_krs$abitur.per <- (Data$abitur.per-min(Data$abitur.per, na.rm = TRUE))/(max(Data$abitur.per, na.rm = TRUE)-min(Data$abitur.per, na.rm = TRUE))
Shapes_krs$GDP.cap <- (Data$GDP.cap-min(Data$GDP.cap, na.rm = TRUE))/(max(Data$GDP.cap, na.rm = TRUE)-min(Data$GDP.cap, na.rm = TRUE))
Shapes_krs$pop.dens <- (Data$pop.dens-min(Data$pop.dens, na.rm = TRUE))/(max(Data$pop.dens, na.rm = TRUE)-min(Data$pop.dens, na.rm = TRUE))
Shapes_krs$east <- (Data$east-min(Data$east))/(max(Data$east)-min(Data$east))
Shapes_krs$refugee.ratio <- (Data$refugee.ratio-min(Data$refugee.ratio, na.rm = TRUE))/(max(Data$refugee.ratio, na.rm = TRUE)-min(Data$refugee.ratio, na.rm = TRUE))

### Set color (for dots?)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("Refugee map"),
  
  p("The markers are refugee initiatives in Germany. Click on the marker to 
    see additional information. You can color the administrative districts (Landkreise)
    depending on the variable you select in the dropdown menu. The map was created by
    Christopher Cosler and Lisa Schmid and is part of a larger project trying 
    identify the determinants of refugee initiatives in Germany
    (https://github.com/ChristopherCosler/CSSR_DataAnalysis)"),
  
  selectInput("data", "Data per district:",
              choices = 
                c("Unemployment ratio",
                  "Gender ratio",
                  "Young people ratio",
                  "Abitur ratio",
                  "GDP per capita",
                  "Population density",
                  "East-West",
                  "Refugee ratio",
                  "Refugee initiatives"
                )),
  
  leafletOutput("mymap")
  
  
  )

server <- function(input, output, session) {
  
  datasetInput <- reactive({
    switch(input$data,
           "Refugee initiatives" = Shapes_krs$numb.ini,
           "Unemployment ratio" = Shapes_krs$unemployment,
           "Gender ratio" = Shapes_krs$gender.ratio,
           "Young people ratio" = Shapes_krs$young.per,
           "Abitur ratio" = Shapes_krs$abitur.per,
           "GDP per capita" = Shapes_krs$GDP.cap,
           "Population density" = Shapes_krs$pop.dens,
           "East-West" = Shapes_krs$east,
           "Refugee ratio" = Shapes_krs$refugee.ratio
    )
  })
  
  points <- eventReactive(input$recalc, {
    cbind(data$Longitude, data$Latitude)
  }, ignoreNULL = FALSE)
  
  pop <- eventReactive(input$recalc, {
    paste(data$Names)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data=Shapes_krs, weight=2, fillOpacity = 0.8, 
                  smoothFactor = 0.5, 
                  color = ~colorBin("YlOrBr", bins = c(0,0.1,0.2,0.3,0.5,0.7,0.9,1), pretty = TRUE,
                                    na.color = "white", Shapes_krs)
                  (datasetInput() )) %>%
      
      addMarkers(data=points(), popup= paste(data$Names)) 
    
    
  })
}

shinyApp(ui, server)