### testslope

###############
### INSTALL ###
###############

###install.packages( "shiny" )
###install.packages( "rsconnect" )

###############
### LIBRARY ###
###############

library(shiny)
library(rsconnect)

##########
### UI ###
##########

ui = fluidPage( 
  titlePanel("Pitch Outcome Predictions"),
  
  ### INPUT FROM MOST ACCURATE MODEL BASED ON TESTING
  
  selectInput( inputId = "bc", ##Input balls in AB
               label = "Choose the number of balls in this at bat.",
               choices = c( "Zero" = "0", "One" = "1", "Two" = "2", "Three" = "3" ) ),
  
  selectInput( inputId = "sc", ##Input strikes in AB
               label = "Choose the number of strikes in this at bat.",
               choices = list( "Zero" = "0", "One" = "1", "Two" = "2" ) ),
  
  selectInput( inputId = "stand", ##Input batter stance
               label = "Choose the batter stance. If the batter is in the right-handed batter's box, mark right. If he is in the left-handed batter's box mark left.",
               choices = list( "Left" =  0, "Right" = 1 ) ),
  
  ### SUBMIT BUTTON
  
  submitButton( "Submit", icon("refresh") ),
  
  ### OUTPUT
  
  textOutput( "prob" )
  
)

##############
### SERVER ###
##############

server = function( input, output ) {
  
  output$prob = renderText({
    
    sc = as.numeric(input$sc)
    bc = as.numeric(input$bc)
    stand = as.numeric(input$stand)
    
    prob = exp(0.4778743 - 0.0521756*sc + 0.0272700*bc + 0.0114960*stand)/(1 + exp(0.4778743 - 0.0521756*sc + 0.0272700*bc + 0.0114960*stand))
    
    if(prob > 0.63) {text = "Strike" }
    if(prob <= 0.63) {text = "Ball" }
    
    prob2 = formatC( prob, format="f", digit=3 )
    
    paste( "Estimated probability of a strike:", prob2, "; Prediction:", text )
    
  })
  
} 

##############
### SHINY! ###
##############

shinyApp( ui=ui, server=server )




