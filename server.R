library(dplyr)



shinyServer(function(input, output, session) {
  
  updateSliderInput(session,"age", min = min(car$VehicleAge),
                    max = max(car$VehicleAge),value = min(car$VehicleAge),
                    step = 1)
  updateSliderInput(session,"odo", min = min(car$VehOdo),
                    max = max(car$VehOdo),value = min(car$VehOdo),
                    step = 100)
  updateSelectInput(session,"engine",
                    choices = unique(car$engine)[order(as.numeric(str_extract(unique(car$engine),"[0-9].[0-9]")))],selected = 1)
  updateSelectInput(session,"auction",
                    choices = unique(car$Auction),selected = 1)
  storage = reactiveValues()

observeEvent(input$gogo,{
if(input$auction != "" && 
   input$engine != ""){
  
  storage$sourpatch<-NULL
  
  proposed_car <- car_mod %>%
    select(-IsBadBuy,-RefId) %>% 
    sample_n(1) %>%
    mutate_each(funs(.*0)) %>% 
    mutate(VehOdo = ((input$odo - min(car$VehOdo))/(max(car$VehOdo) - min(car$VehOdo)))) %>% 
    mutate(VehicleAge = ifelse(input$age>3,1,0)) %>% 
    mutate(engine2.4L = ifelse("2.4L" == input$engine,1,0)) %>% 
    mutate(engine3.8L = ifelse("3.8L" == input$engine,1,0)) %>% 
    mutate(engine4.0L = ifelse("4.0L" == input$engine,1,0)) %>% 
    mutate(engine5.4L = ifelse("5.4L" == input$engine,1,0)) %>% 
    mutate(AuctionMANHEIM = ifelse("MANHEIM" == input$auction,1,0)) %>% 
    mutate(doors4D = ifelse("4D" == input$door,1,0)) %>% 
    mutate(doors2D = ifelse("2D" == input$door,1,0)) %>% 
    mutate(doorsother  = ifelse("O" == input$door,1,0))
    
  lemony<-as.numeric(predict(logreg, newdata = proposed_car, type = "response"))
  storage$lemony<-((lemony - min(car_test$prediction))/(max(car_test$prediction) - min(car_test$prediction)))
}else{
  storage$sourpatch<-1
}
})

output$lemonytoleranceFP <- renderUI({

      h3(paste("Not bidding on",
               round(diag(table(car_test$prediction<quantile(car_test$prediction,input$tolerance/100),car_test$IsBadBuy))[1]/nrow(car_test),3)*100,
               "% of cars due to incorrectly calling them a lemon!"))
  
})  
output$lemonytoleranceFN <- renderUI({
  
  h3(paste("Bidding on",
           round(diag(table(car_test$prediction<quantile(car_test$prediction,input$tolerance/100),car_test$IsBadBuy))[2]/nrow(car_test),3)*100,
           "% of cars that are lemons in disguse!"))
  
})  

output$lemonyResponse <- renderUI({
  if(is.null(storage$sourpatch)){
  if(!is.null(storage$lemony)){
    if(storage$lemony>quantile(car_test$prediction,input$tolerance/100)){
    h1("Great Scott, A LEMON")
    }else{
      h1("Probably not a lemon, let's bid!")
    }
  }else{
    h5("Select model specifications and click the Submit button on the Welcome page.")
  }
  }else{
    h4("Pick some parameters, dummy!")
  }
})  
#   selectInput("engine", label = ("Engine Type"), 
#               choices = list("loading" = 0), 
#               selected = 0),
#   selectInput("auction", label = ("Auction Location"), 
#               choices = list("loading" = 0), 
#               selected = 0),

})