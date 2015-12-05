# ui.R

shinyUI(fluidPage(
  titlePanel("Interests in EVs by Regions"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("var", 
                  label = "Choose a group to display",
                  choices = c("Whole Population", 
                              "Females",
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
                              "People with Three or More Cars"),
                  selected = "Whole Population")
      
      ),
    
    mainPanel(
      textOutput("text1"),
      imageOutput("preImage")
    )
    
  )
))