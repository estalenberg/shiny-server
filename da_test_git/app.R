#DA test app for structure with simple graph

library(shiny)

library(RColorBrewer) #for map colours
library(shinyjs)
library(shinythemes)


# import data ----

#setwd("C:/R/shiny/DA_testapp")



# Define UI for slider demo app ----
ui <- tagList(
  useShinyjs(),
  navbarPage(fluid = T,theme = shinytheme("yeti"),
             tabPanel(h2(" ")),
             tabPanel(h4("Research"),
                      fluidPage(
                        mainPanel(
                          h2("Modelling electricity price change"),
                          h3("DEMO VERSION")
                        )
                      )
             ),
             tabPanel(h4("The model"),
                      fluidPage(
                        titlePanel(h4("The dynamic model")),
                        sidebarLayout(
                          sidebarPanel( 
                            selectInput(inputId = "year",
                                        label = "Reporting year",
                                        choices = seq(2050,2050, by=10),
                                        selected = 2050),
                            hr(),
                            
                            sliderInput("age", "Age of assets (years):",
                                        min = 60, max = 80,
                                        value = 60, step=5),
                            
                            sliderInput("sales", "Annual energy sales change (%):",
                                        min = -1, max = 1,
                                        value = -0.5, step = 0.1),
                            
                            sliderInput("size", "Size of network (% change from today):",
                                        min = -20, max = 10,
                                        value = 0, step = 1),
                            
                            sliderInput("product", "Annual productivity (%):",
                                        min = -1, max = 2,
                                        value = 0, step = 0.1),
                            
                            sliderInput("interest", "RBA cash rate (%):",
                                        min = 1.5, max = 3.5,
                                        value = 3.5, step = 0.1)
                          ),
                          mainPanel(
                            plotOutput("graph", height = "300px")
                          )
                        )
                      )
             )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  #graph ----
  
  output$graph<-renderPlot({
    
    #inputs
    a=as.numeric(input$age)
    sa=as.numeric(input$sales)
    si=as.numeric(input$size)
    pr=as.numeric(input$product)
    int=as.numeric(input$interest)
    
    

    inputs=c(a,sa,si,pr,int)
    inputsname=c("age","sales","size","productivity","interest")
    
    
    
    saeq=(80-a)+((10*(0.5-sa))+(si+10)+((10*(1.5-pr))+(10*(int-1.5))))
    nsweq=-30+saeq
    viceq=-10+saeq
    taseq=-25+saeq
    qldeq=-20+saeq
    nteq=-30+saeq
    
    price=c(nsweq,qldeq,saeq,viceq,nteq,taseq)
    
    states=c("NSW", "QLD", "SA", "VIC", "NT", "TAS")
    
    data=cbind.data.frame(price, states)
    
    inputsdf=cbind.data.frame(inputs, inputsname)
    
    barplot(data$price, names = data$states,
            xlab = "Australian states", ylab = "Percent price growth (%)",
            main = "Percent price growth (% of 2018 prices)", col=data$states)
            #col = c("yellow", "yellow","red","red", "orange","orange"))
    })
  
}




# Create Shiny app ----
shinyApp(ui, server)

# end ----

