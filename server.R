shinyServer(
  function(input, output) {
    
    assignmentArableLand <- read.csv("~/R Projects/InteractiveVisualisation/Assignment/data/arableland.csv")
    assignmentLandUse <- read.csv("~/R Projects/InteractiveVisualisation/Assignment/data/LandUse.csv")
    
    output$graph<- renderPlot({
        colourChoice<-input$colour
        smoothFlag<-input$isSmoothed 
        data<-input$info
        pos<-input$pos #dodge or stacked
        facetFlag<-FALSE
        alignText<-theme(axis.text.x = element_text(angle=90, vjust=1))#theme(text = element_text(size=10),axis.text.x = element_text(angle=90, vjust=1))
        
        
        if(data == 1)
          {
             if(input$graphType == "Bar")
             {
                graph<-geom_bar(stat="identity",fill=colourChoice)
          
             }
             else if(input$graphType == "Line")
             {
                graph<-geom_line(stat="identity",colour=colourChoice)
          
             }
             else if(input$graphType == "Point")
             {
                graph<-geom_point(stat="identity",colour=colourChoice)
          
             }
             else #if(input$graphType == "Density")
             {
                graph<-geom_density(stat="identity",fill=colourChoice)
          
             }
        }
        else
        {
          #graph<-geom_bar(stat="identity",fill=colourChoice)
          if(input$Facet=="LandUse")
          {
            facetSelection<-facet_grid(. ~ LandUse) 
            facetFlag<-TRUE
          }
          else if(input$Facet=="Year")
          {
            facetSelection<-facet_grid(. ~ Year)
            facetFlag<-TRUE
          }
          else if(input$Facet=="State")
          {
            facetSelection<-facet_grid(. ~ State)
            facetFlag<-TRUE
          }
          else
          {
            facetFlag<-FALSE
          }
        }
        
        if(data==1)
        {
          if(input$y == 1) #arable percent
          {
            if(smoothFlag == FALSE)
            {
              qplot(Year,
                    ArableLandPercent,
                    data=assignmentArableLand
                    )+graph
            }
            else
            {
              qplot(Year,
                    ArableLandPercent,
                    data=assignmentArableLand
                    )+graph+stat_smooth()
            }
          }
          else if(input$y == 2) #population
          {
            
            if(smoothFlag == FALSE)
            {
              qplot(Year,
                    Population,
                    data=assignmentArableLand               
                    )+scale_y_continuous(labels = comma)+graph
            }
            else
            {
              qplot(Year,
                    Population,
                    data=assignmentArableLand               
              )+scale_y_continuous(labels = comma)+graph+stat_smooth() 
            }
           
          }
          else if(input$y == 3) #actual arable amount
          {
            
            if(smoothFlag == FALSE)
            {
              qplot(Year,
                    ArableLand,
                    data=assignmentArableLand 
                    )+scale_y_continuous(labels = comma)+graph
            }
            else
            {
              qplot(Year,
                    ArableLand,
                    data=assignmentArableLand 
                    )+scale_y_continuous(labels = comma)+graph+stat_smooth()
            }               
          }
        }
        else if(data == 2)#landuse
        {
          if(input$dispGraph == "LandUse and Area")
          {
            
            if(facetFlag==FALSE)#no facet selection
            {
              if(smoothFlag == FALSE)
              {              
                qplot(LandUse,
                      Area,
                      data=assignmentLandUse,
                      fill=State,
                      geom="bar",
                      position=pos,stat="identity")+scale_y_continuous(labels = comma)+ylab("Area ('000 ha)")+alignText 
              }
              else
              {
                qplot(LandUse,
                      Area,
                      data=assignmentLandUse,
                      fill=State,
                      geom="bar",
                      position=pos,
                      stat="identity")+scale_y_continuous(labels = comma)+ylab("Area ('000 ha)")+geom_smooth(aes(group = 1))+alignText 
              }
            }
            else
            {
              if(smoothFlag == FALSE)
              {              
                qplot(LandUse,
                      Area,
                      data=assignmentLandUse,
                      fill=State,
                      geom="bar",
                      position=pos,stat="identity")+scale_y_continuous(labels = comma)+ylab("Area ('000 ha)")+facetSelection+alignText 
              }
              else
              {
                qplot(LandUse,
                      Area,
                      data=assignmentLandUse,
                      fill=State,
                      geom="bar",
                      position=pos,
                      stat="identity")+ylab("Area ('000 ha)")+scale_y_continuous(labels = comma)+facetSelection+geom_smooth(aes(group = 1))+alignText 
              }
            } 
          }
          else if(input$dispGraph == "Year and Area")
          {
            
            if(facetFlag==FALSE)
            {
              if(smoothFlag == FALSE)
              {     
                qplot(Year,
                      Area,
                      data=assignmentLandUse,
                      fill=LandUse,
                      geom="bar",
                      position=pos,
                      stat="identity")+ylab("Area ('000 ha)")+scale_y_continuous(labels = comma)+alignText 
              }
              else
              {
                qplot(Year,
                      Area,
                      data=assignmentLandUse,
                      fill=LandUse,
                      geom="bar",
                      position=pos,
                      stat="identity")+ylab("Area ('000 ha)")+scale_y_continuous(labels = comma)+geom_smooth(aes(group=1))+alignText
              }
            }
            else
            {
              if(smoothFlag == FALSE)
              {     
                qplot(Year,
                      Area,
                      data=assignmentLandUse,
                      fill=LandUse,
                      geom="bar",
                      position=pos,
                      stat="identity")+ylab("Area ('000 ha)")+scale_y_continuous(labels = comma)+facetSelection+alignText 
              }
              else
              {
                qplot(Year,
                      Area,
                      data=assignmentLandUse,
                      fill=LandUse,
                      geom="bar",
                      position=pos,
                      stat="identity")+ylab("Area ('000 ha)")+scale_y_continuous(labels = comma)+facetSelection+geom_smooth(aes(group=1))+alignText
              }
            } 
          }
          else if(input$dispGraph == "State and Area")
          {
            
            if(facetFlag==FALSE)
            {
              if(smoothFlag == FALSE)
              {
                qplot(State,
                      Area,
                      data=assignmentLandUse,
                      fill=LandUse,
                      geom="bar",
                      position=pos,stat="identity")+scale_y_continuous(labels = comma)+ylab("Area ('000 ha)")+alignText
              }
              else
              {
                qplot(State,
                      Area,
                      data=assignmentLandUse,
                      fill=LandUse,
                      geom="bar",
                      position=pos,stat="identity")+scale_y_continuous(labels = comma)+ylab("Area ('000 ha)")+stat_smooth(aes(group=1))+alignText
              }
            }
            else
            {
              if(smoothFlag == FALSE)
              {
                qplot(State,
                      Area,
                      data=assignmentLandUse,
                      fill=LandUse,
                      geom="bar",
                      position=pos,stat="identity")+scale_y_continuous(labels = comma)+ylab("Area ('000 ha)")+facetSelection+alignText
              }
              else
              {
                qplot(State,
                      Area,
                      data=assignmentLandUse,
                      fill=LandUse,
                      geom="bar",
                      position=pos,stat="identity")+scale_y_continuous(labels = comma)+ylab("Area ('000 ha)")+facetSelection+stat_smooth(aes(group=1))+alignText
              }
            }
          } 
        }
        })
})
