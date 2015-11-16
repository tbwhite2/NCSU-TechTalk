

shinyUI(
fluidPage(
                      # column(6,
##~~~~~~~~~~~~~~~~~~~~~~~~~~#Add age inputs here
#                             sliderInput("odo", label = ("Vehicle Mileage"), min = 0, 
#                                         max = 100, value = 50),

##~~~~~~~~~~~~~~~~~~~~~~~~~~#Add cartype inputs here
#                             selectInput("engine", label = ("Engine Type"), 
#                                         choices = list("loading" = 0), 
#                                         selected = 0),
#                             selectInput("national", label = ("Nationality of Car"), 
#                                         choices = list("loading" = 0), 
#                                         selected = 0),

##~~~~~~~~~~~~~~~~~~~~~~~~~~#Add door inputs here
      
#                             selectInput("auction", label = ("Auction Location"), 
#                                         choices = list("loading" = 0), 
#                                         selected = 0),
                         
 
##~~~~~~~~~~~~~~~~~~~~~~~~~~#Add gogo Action button here
            

                                    # )
# ,
#                             column(6,
#                                    sliderInput("tolerance", label = h3("Lemon Tolerance"), min = 0, 
#                                                max = 100, value = 50),
#                                    h4("With this tolerance you risk:"),
#                                    hr(),
#                                    uiOutput("lemonytoleranceFP"),
#                                    hr(),
#                                    uiOutput("lemonytoleranceFN"),
#                                    uiOutput("lemonyResponse")
#                             )
                            
                            
                         
                            
                            
                  # ),
#                    tabPanel("Auction Payment Amount",
#                             fluidRow(
#                               column(6,h3("Historic Price Density Plot")),
#                               column(6,h3("Historic Margin Density Plot"))
#                               ),
#                            fluidRow(
#                              column(6,uiOutput("buypriceplot")),
#                              column(6,uiOutput("sellpriceplot"))
#                              ),
#                             hr(),
#                             uiOutput("pricepoints1"),
#                             hr(),
#                             uiOutput("pricepoints2"),
#                             hr()
#                             ,
#                             uiOutput("pricepoints3")
#                             )
)
)