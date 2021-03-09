# Predict next word price - ShinyApp - server engine (server.R)
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# author: Marko Intihar

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Next word prediction"),
    
    # Sidebar with sliders & radio buttons (for user input data - diamond features)
    sidebarLayout(
        sidebarPanel(
            
            # user input - a phrase or a single word
            textInput("phrase_user",
                      "Please provide a phrase or a single word",
                       value = ""),
            
            # master submit button - to run server calculations when inputs are defined
            submitButton("Submit")
            
        ),
        
        # show predicted word(s) by model
        mainPanel(
            h4("Predicted word (from provided phrase)"),
            textOutput("predicted_word"),
            # CSS styling for price prediction (shown as text)
            tags$head(tags$style("#pred_USD{color: red;
                                 font-size: 80px;
                                 font-style: bold;
                                 }"))
        )
    )
    
    
))
