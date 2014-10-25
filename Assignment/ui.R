shinyUI(fluidPage(
  titlePanel("Assignment"),
  
  sidebarLayout(
    sidebarPanel(
         
       selectInput("info", 
                   label = ("Select Information to Display"), 
                   choices = list("Population and Arable Land" = 1,
                                  "Agricultural Landuse" = 2), 
                   selected = 2),
       conditionalPanel(condition = "input.info == 1",
                        selectInput("y", 
                        label = ("Select Graph to Display"), 
                        choices = list("% of Arable Land vs Year" = 1,
                                       "Population vs Year" = 2, 
                                       "Actual Arable Land vs Year" = 3), 
                        selected = 1),
                        selectInput("graphType", 
                                    label = ("Select Graph Type"), 
                                    choices = c("Bar","Line","Point","Density"), 
                                    selected = "Bar"),
                        selectInput("colour", 
                                    label = ("Select Colour"), 
                                    choices = list("Black" = "black",
                                                   "Dark Blue" = "darkblue", 
                                                   "Red" = "red",
                                                   "Dark Green" = "darkgreen",
                                                   "Gray" = "gray"), 
                                    selected = "black")),
       conditionalPanel(condition = "input.info == 2",
                        selectInput("dispGraph",
                                    label=("Select Graph to Display"),
                                    choices = c("LandUse and Area",
                                                "Year and Area",
                                                "State and Area"),
                                    selected = "LandUse and Area"),
                        selectInput("Facet",
                                    label=("Facet Graph"),
                                    choices = c("None",
                                                "Year",
                                                "State",
                                                "LandUse"),
                                    selected = "None"),
                        radioButtons("pos", label = "Position",
                                     choices = c("stack","dodge"),selected = "dodge")
       ),
       
       checkboxInput("isSmoothed", "Add Smoothing", value = FALSE)
      
       
       
      ),
    mainPanel(plotOutput("graph"))
  )
))