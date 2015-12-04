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
load("data4web.rda")

### Prepare Shapefiles
# shapefile
tmp_dir = tempdir()
url_data = "https://geoportal.statistics.gov.uk/Docs/Boundaries/Regions_(GB)_2014_Boundaries_(Generalised_Clipped).zip"
zip_file = sprintf("%s/shpfile.zip", tmp_dir)
download.file(url_data, zip_file)
unzip(zip_file, exdir = tmp_dir)
gor=readShapeSpatial(sprintf('%s/RGN_DEC_2014_GB_BGC.shp', tmp_dir))

# object ID to merge with EV interest data
gor@data$OID <- c(1,2,3,4,5,6,7,8,9,11,10)

gor@data = data.frame(gor@data, RegionDF[match(gor@data$OID, RegionDF$OID),])


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
                c("Females",
                  "Males",
                  "Young People",
                  "Middle-Age People",
                  "Old-Age People",
                  "Low-Income People",
                  "Lower-Middle Income People",
                  "Higher-Middle Income People",
                  "High-Income People",
                  "College Graduates",
                  "Non-College Graduates",
                  "Having Driver's Licence",
                  "Not Having Driver's Licence",
                  "People with No Car",
                  "People with One Car",
                  "People with Two Cars",
                  "People with Three or More Cars"
                )),
  
  leafletOutput("mymap")
  
  
  )

server <- function(input, output, session) {
  
  datasetInput <- reactive({
    switch(input$data,
           "Females"=gor@data$IntFemales,
           "Males"=gor@data$IntMales,
           "Young People"=gor@data$IntYoung,
           "Middle-Age People"=gor@data$IntMiddleage,
           "Old-Age People"=gor@data$IntOldage,
           "Low-Income People"=gor@data$IntLow,
           "Lower-Middle Income People"=gor@data$IntLowmid,
           "Higher-Middle Income People"=gor@data$IntHighmid,
           "High-Income People"=gor@data$IntHigh,
           "College Graduates"=gor@data$IntCollege,
           "Non-College Graduates"=gor@data$IntNocollege,
           "Having Driver's Licence"=gor@data$IntLicence,
           "Not Having Driver's Licence"=gor@data$IntNolicence,
           "People with No Car"=gor@data$IntNocar,
           "People with One Car"=gor@data$IntOnecar,
           "People with Two Cars"=gor@data$IntTwocars,
           "People with Three or More Cars"=gor@data$IntThreecars
    )
  })
  
  pop <- eventReactive(input$recalc, {
    paste(data$Names)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data=gor@data, weight=2, fillOpacity = 0.8, 
                  smoothFactor = 0.5, 
                  color = ~colorBin("YlOrBr", bins = c(0,0.1,0.2,0.3,0.5,0.7,0.9,1), pretty = TRUE,
                                    na.color = "white", gor@data)
                  (datasetInput() )) %>%
      
      addMarkers(data=points(), popup= paste(data$Names)) 
    
    
  })
}

shinyApp(ui, server)