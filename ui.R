
library(shiny)
library(markdown)
library(shinythemes)
library(dplyr)
library(magrittr)
library(dygraphs)


shinyUI(navbarPage("Lemon-AID",
                   tabPanel("Lemon Identification",
                            column(6,
                            sliderInput("tolerance", label = h3("Lemon Tolerance"), min = 0, 
                                        max = 100, value = 50),
                            h4("With this tolerance you risk:"),
                            hr(),
                            uiOutput("lemonytoleranceFP"),
                            hr(),
                            uiOutput("lemonytoleranceFN"),
                            uiOutput("lemonyResponse")
                            ),
                            column(6,
                            sliderInput("age", label = ("Vehicle Age"), min = 0, 
                                        max = 100, value = 50),
                            sliderInput("odo", label = ("Vehicle Mileage"), min = 0, 
                                        max = 100, value = 50),
                            selectInput("cartype", label = ("Car Style"), 
                                        choices = list("loading" = 0), 
                                        selected = 0),
                            selectInput("engine", label = ("Engine Type"), 
                                        choices = list("loading" = 0), 
                                        selected = 0),
                            selectInput("national", label = ("Nationality of Car"), 
                                        choices = list("loading" = 0), 
                                        selected = 0),
                            radioButtons("door", label = ("Number of Doors"),
                                         choices = list("2" = "2D", "4" = "4D", "Other" = "O"), 
                                         selected = "2D"),
                            selectInput("auction", label = ("Auction Location"), 
                                        choices = list("loading" = 0), 
                                        selected = 0),
                         
                            actionButton("gogo",label = "Submit Car")
                            
                            
                         
                            
                            
                   )),
                   tabPanel("Auction Payment Amount",
                            uiOutput("buypriceplot"),
                            uiOutput("sellpriceplot"),
                            hr(),
                            uiOutput("pricepoints1"),
                            hr(),
                            uiOutput("pricepoints2"),
                            hr(),
                            uiOutput("pricepoints3"))
))