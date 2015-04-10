
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Clinical Trail Randomization Tool (Demo Version)"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    numericInput("id", "Patient Number:", value=0, min=0, max=9999),
    selectInput("type", "Surgery Type:", 
                choices=c("type 1" = 1, "type 2" = 2,
                          "type 3" = 3, "type 4" = 4,
                          "type 5" = 5, "type 6" = 6,
                          "type 7" = 7, "type 8" = 8), selected=1),
    gsub("label class=\"radio\"", "label class=\"radio inline\"",
         radioButtons("gender", "Gender:", 
                      choices=c("Male" = 1, "Female" = 2), selected=1)
    ),
    numericInput("age", "Age:", min = 70, max = 120, value = 83),
    selectInput("site", "Treatment Site:", 
                choices=c("site 1" = 1, "site 2" = 2,
                          "site 3" = 3, "site 4" = 4,
                          "site 5" = 5, "site 6" = 6,
                          "site 7" = 7, "site 8" = 8,
                          "site 9" = 9, "site 10" = 10,
                          "site 11" = 11, "site 12" = 12,
                          "site 13" = 13, "site 14" = 14,
                          "site 15" = 15, "site 16" = 16,
                          "site 17" = 17, "site 18" = 18,
                          "site 19" = 19, "site 20" = 20), selected=1),
    helpText("Please choose covariate levels and press the", 
             strong("submit"), "button."),
    actionButton("submit", "Submit")
    
#     tags$hr(),
#     downloadButton("dl", "Download Historical Data")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    dataTableOutput("dt")
    
#     h4(textOutput("text"))
  )
))
