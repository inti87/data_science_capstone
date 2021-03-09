# Predict next word price - ShinyApp - server engine (server.R)
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# author: Marko Intihar


# load essential libraries
library(shiny)
library(dplyr)
library(tm)
library(data.table)

# load functions
source("func_corpus_cleaning.R")
source("func_predict_next_word.R")

# load data
source("freq_2.R")
source("freq_3.R")
source("freq_4.R")



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Predict next word using phrase (provided by user)
    wordpred <- reactive({
        # input diamond features
        phraseInput <- input$phrase_user
   
        # word prediction
        predict_next_word(phraseInput)
    })
    
     # Predicted price as string (added USD)
     output$predicted_word <- renderText({wordpred()
         })

})
