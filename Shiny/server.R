# server.R


shinyServer(function(input, output, session) {
  
  
  output$text1 <- renderText({
    
    paste("% of ", input$var, "who have interested in EVs by regions")
    
  })
  
  
  output$preImage <- renderImage({
    
    selectedgroup <- switch(input$var, 
                            "Whole Population" = 'map1.png', 
                            "Females" = 'map2.png',
                            "Males" = 'map3.png',
                            "Young People" = 'map4.png',
                            "Middle-Age People" = 'map5.png',
                            "Old-Age People" = 'map6.png',
                            "Low-Income People" = 'map7.png',
                            "Lower-Middle Income People" = 'map8.png',
                            "Higher-Middle Income People" = 'map9.png',
                            "High-Income People" = 'map10.png',
                            "College Graduates" = 'map11.png',
                            "Non-College Graduates" = 'map12.png',
                            "Having Driver's Licence" = 'map13.png',
                            "Not Having Driver's Licence" = 'map14.png',
                            "People with No Car" = 'map15.png',
                            "People with One Car" = 'map16.png',
                            "People with Two Cars" = 'map17.png',
                            "People with Three or More Cars" = 'map18.png')
    
    filename <- normalizePath(file.path('./ExportedMaps', selectedgroup))
    
    list(src = filename,
         alt = paste("Image number"))
    
  }, deleteFile = FALSE)

})


shinyServer(function(input, output, session) {
  
  
  output$text2 <- renderText({
    
    paste("Relative weights given to different factors by", input$factor)
    
  })
  
  
  output$factorbar <- renderImage({
    
    selectedgroup2 <- switch(input$factor, 
                             "age"="factor_age.png",
                             "sex"="factor_sex.png",
                             "income level"="factor_income.png",
                             "education level"="factor_college.png",
                             "driver's licence status"="factor_licence.png",
                             "number of cars owned"="factor_carnumber.png",
                             "region"="factor_scotland.png")
    
    filename2 <- normalizePath(file.path('./ExportedMaps', selectedgroup2))
    
    list(src = filename2,
         alt = paste("Image number"))
    
  }, deleteFile = FALSE)
  
})