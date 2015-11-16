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
  
  updateSelectInput(session,"cartype",
                    choices = unique(car$cartype),selected = 1)
  
  updateSelectInput(session,"national",
                    choices = unique(car$Nationality),selected = 1)
  
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
    mutate(engineother = ifelse("3.5L" == input$engine,1,0)) %>% 
    mutate(engine3.5L = ifelse("other" == input$engine,1,0)) %>%
    mutate(AuctionMANHEIM = ifelse("MANHEIM" == input$auction,1,0)) %>% 
    mutate(AuctionOTHER = ifelse("OTHER" == input$auction,1,0)) %>%
    mutate(doors4D = ifelse("4D" == input$door,1,0)) %>% 
    mutate(doors2D = ifelse("2D" == input$door,1,0)) %>% 
    mutate(doorsother  = ifelse("O" == input$door,1,0)) %>% 
    mutate(cartypeVAN  = ifelse("VAN" == input$cartype,1,0)) %>%
    mutate(cartypeWAGON  = ifelse("WAGON" == input$cartype,1,0)) %>% 
    mutate(NationalityAMERICAN  = ifelse("AMERICAN" == input$national,1,0)) %>% 
    mutate(NationalityOTHERASIAN  = ifelse("OTHERASIAN" == input$national,1,0))
  
  storage$proposed_car<-proposed_car  
  
  lemony<-as.numeric(predict(logreg, newdata = proposed_car, type = "response"))
  
  storage$price_levels<-as.numeric(predict(lm, newdata =proposed_car, type = "response", interval = "prediction", level = .99))
  
  storage$lemony<-((lemony - min(car_test_log$prediction))/(max(car_test_log$prediction) - min(car_test_log$prediction)))
  
}else{
  storage$sourpatch<-1
}
})

observe({
  if(!is.null(storage$proposed_car)){
    
storage$priceinfo<-car %>% 
      filter(VehOdo <= input$odo + 30000) %>% 
      filter(VehOdo >= input$odo - 30000) %>% 
      filter(VehicleAge == input$age) %>% 
      filter(cartype == input$cartype) %>% 
#       filter(engine == input$engine) %>% 
#       filter(doors == input$door)%>% 
      select(price_a,sell_r) %>% 
      mutate_each(funs(as.numeric)) %>% 
      mutate(margin = (sell_r - price_a))


# test<<-storage$priceinfo
  }
})

output$lemonytoleranceFP <- renderUI({

      h3(paste("Not bidding on",
               round(diag(table(car_test_log$prediction<quantile(car_test_log$prediction,input$tolerance/100),car_test_log$IsBadBuy))[1]/nrow(car_test_log),3)*100,
               "% of cars due to incorrectly calling them a lemon!"))
  
})  
output$lemonytoleranceFN <- renderUI({
  
  h3(paste("Bidding on",
           round(diag(table(car_test_log$prediction<quantile(car_test_log$prediction,input$tolerance/100),car_test_log$IsBadBuy))[2]/nrow(car_test_log),3)*100,
           "% of cars that are lemons in disguse!"))
  
})  

output$lemonyResponse <- renderUI({
  if(is.null(storage$sourpatch)){
  if(!is.null(storage$lemony)){
    if(storage$lemony>quantile(car_test_log$prediction,input$tolerance/100)){
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


output$buypriceplot <- renderUI({
  if(!is.null(storage$priceinfo)){
    if(nrow(storage$priceinfo)<3){
      "Not Enough Data To Compare Price History"
    }else{
      
    
    output$priceplot = renderPlot({
      
      ggplot(storage$priceinfo, aes(x=price_a)) + 
        
        geom_density( fill = "#91cf60",alpha=.75) +
        ylab("Density") +
        xlab("Historical Average Price for Similar Vehicles") + 
        theme(axis.text.x=element_text(angle=40,size=12,vjust=.5, face = "bold"),
              panel.grid.major = element_line(colour = "grey73"),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              legend.position="none") +
        geom_vline(aes(xintercept=median(price_a, na.rm=T)),   # Ignore NA values for med
                   color="#fc8d59", linetype="dashed", size=1.5)
      
    })
    plotOutput("priceplot")
    }
  }
})  

output$sellpriceplot <- renderUI({
  if(!is.null(storage$priceinfo)){
    if(nrow(storage$priceinfo)<3){
      "Not Enough Data To Compare Price History"
    }else{
      
      
      output$marginplot = renderPlot({
        
        ggplot(storage$priceinfo, aes(x=margin)) + 
          geom_density( fill = "#fee08b",,alpha=.75) +
          ylab("Density") +
          xlab("Potential Profit") + 
          theme(axis.text.x=element_text(angle=40,size=12,vjust=.5, face = "bold"),
                panel.grid.major = element_line(colour = "grey73"),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.position="none") 
        
      })
      plotOutput("marginplot")
    }
  }
})

output$pricepoints2 <- renderUI({
  if(!is.null(storage$priceinfo)){
    if(nrow(storage$priceinfo)<3){
      "Not Enough Data To Compare Price History"
    }else{
      
        h3(paste("Safest High Bid: ", round(median(storage$priceinfo$price_a) + storage$price_levels[1],0)))

      # storage$priceinfo
    }
  }
})  

output$pricepoints3 <- renderUI({
  if(!is.null(storage$priceinfo)){
    if(nrow(storage$priceinfo)<3){
      "Not Enough Data To Compare Price History"
    }else{
      
      h3(paste("Never go above: ", round(median(storage$priceinfo$price_a) + storage$price_levels[3],0)))

      # storage$priceinfo
    }
  }
})

output$pricepoints1 <- renderUI({
  if(!is.null(storage$priceinfo)){
    if(nrow(storage$priceinfo)<3){
      "Not Enough Data To Compare Price History"
    }else{
      
      h3(paste("Always bid when price is below: ", round(median(storage$priceinfo$price_a) + storage$price_levels[2],0)))
   
      # storage$priceinfo
    }
  }
})

})