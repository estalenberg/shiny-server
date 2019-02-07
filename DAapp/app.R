#DA_APP - final

#libraries ----
library(shiny)
#library(maptools)
#library(maps)
#library(mapdata)
#library(lattice)
library(sp)  # classes for spatial data
#library(rgeos)
library(dismo) #need this to load map
#library(gtools)
#library(GISTools)
library(rgdal) #loading polygon shapefile
library(RColorBrewer) #for map colours
library(shinyjs)
library(shinythemes)


# import data ----

#setwd("C:/R/shiny/DA_app")


#stateshp<-readOGR(dsn = "./data",layer =  "stateshp")


stateshp=readRDS("./data/states.rds")

#colours ----
#manually colour scheme for both plots
#spectral can only take 11 categories, ylorred takes 9
percent=seq(-100,-5,by=1)
scalefac=as.factor(percent)
scalefac = levels(scalefac)

colour = rev(brewer.pal(9, "BuGn")) 
#add more tones to this palette :
colour = colorRampPalette(colour)(96)
#pie(rep(1, length(colour)), col = colour , main="")
labels=paste(as.numeric(percent),"%")
labels=gsub(" ", "", labels, fixed = TRUE)

scolours <- cbind.data.frame(percent,scalefac,colour,labels)
scolours$colour=as.character(scolours$colour)

percent=seq(0,100,by=1)
scalefac=as.factor(percent)
scalefac = levels(scalefac)

colour = rev(heat.colors(9)) 
colour = colorRampPalette(colour)(101)

labels=paste("+",as.numeric(percent),"%")
labels=gsub(" ", "", labels, fixed = TRUE)

scolours2 <- cbind.data.frame(percent,scalefac,colour, labels)
scolours2$colour=as.character(scolours2$colour)

scol=rbind.data.frame(scolours, scolours2)
scol=scol[order(scol$percent),] 


#pie(rep(1, length(scol$colour)), col = scol$colour , main="")

#for legend
subdf=seq(-100,100,by=20)

df.new <- scol[scol$percent %in% subdf, ]

#df.new = scol[seq(1, nrow(scol), 10), ]


# code states with new percents



# Define UI  ----
ui <- tagList(
  useShinyjs(),
  navbarPage(uiOutput("tab",style="float:right; padding-right:0px"),
             fluid = T,theme = shinytheme("yeti"),
             tabPanel(h4("The Model"),
                     # uiOutput("tab2",style="float:right; padding-right:0px"), 
                      # img(src="DAlogo.png", height=50, style="float:right; padding-right:0px"),
                      fluidPage(
                        titlePanel(h4("")),
                        sidebarLayout(
                          sidebarPanel( 
                            selectInput(inputId = "year",
                  label = "Reporting year",
                  choices = seq(2050,2050, by=10),
                  selected = 2050),
      hr(),
      
      sliderInput("age", "Age of assets (years):",
                  min = 60, max = 80,
                  value = 61, step=1),
      
      sliderInput("sales", "Annual energy sales change (%):",
                  min = -1, max = 1,
                  value = -0.5, step = 0.1),
      
      sliderInput("size", "Size of network (% change from today):",
                  min = -20, max = 10,
                  value = 10, step = 1),
      
      sliderInput("product", "Annual productivity (%):",
                  min = -1, max = 2,
                  value = 0, step = 0.1),
      
      sliderInput("interest", "RBA cash rate (%):",
                  min = 1.5, max = 3.5,
                  value = 3.3, step = 0.1)
                          ),
      mainPanel(
        plotOutput("map", height = "900px")
      )
                        )
                      )
             ),
      tabPanel(h4("The Research"),
                 uiOutput("tab1",style="float:right; padding-right:0px"), 
               #img(src="DAlogo.png", height=50, style="float:right; padding-right:0px"),
               fluidPage(
                 mainPanel(
                   h2("The Dynamic Model"),
                   em(h3("Lowering energy prices for consumers")),
                   br(),
                   div("Our research aims to identify 'operational targets' to reduce network prices over time."),
                   br(),
                   div("The model on this website is an interactive and graphical way to identify the issues that our industry faces over the next 30 years. 
                       This builds on our research agenda which shows we are facing impending headwinds that are likely to significantly drive prices higher by 2050:"),
                   br(),
                   tags$li("A steep and protracted uplift in replacement capex due as a wave of network assets become too old to provide a reliable energy service."),
                   tags$li("An expected increase from today's low interest rates, which will increase the cost of financing capital investments."),
                   tags$li("Falling energy sales as consumers increasingly use their own energy from solar and battery, particularly as prices rise."),
                   br(),
                   div("Our model will use replacement modelling techniques that use sustainable age to predict replacement capital. It will then use the AER's revenue calculator to pinpoint the increase in prices by 2050. The model will incorporate other scenarios such as interest rates, energy sales, productivity, and size of the network."),
                   br(),
                   div("What we are trying to offer is a 'blueprint' for the industry to avoid the impending headwinds. Similar to an 'emissions target', it provides a quantifiable measure of what we need to do to keep a lid on prices. The model identifies key variables that a network can target that can reduce prices for customers over time. These include:"),
                   br(),
                   tags$li("Keeping assets in service longer by using better risk frameworks and new technology."),
                   tags$li("Increasing energy sales by having infrastructure in place for electric vehicles."),
                   tags$li("Reducing the footprint and size of today's networks, by finding ways to retire assets rather than 'like for like' replacement. 
                        This would occur by acting on the recommendations of the ENA Transformation roadmap to utilise the power of DER to steadily reduce the need of networks."),
                   tags$li("Driving sustained productivity through targets in expenditure allowances, and more targeted incentives."),
                   br(),
                   div("We want to display our results in an interactive form that allows participants and stakeholders to openly discuss and challenge the long term strategies of networks. The model also opens us deeper conversation on how those targets can be achieved.")
                 )
               )
      )
     )
)



# Define server  ----
server <- function(input, output) {
  url <- a(img(src="DAlogo.png", height=50), href="http://www.dynamicanalysis.com.au")
  url2 <- a(img(src="DAlogowhitesml.png", height=50), href="http://www.dynamicanalysis.com.au")
  
  #model ----
  output$map<-renderPlot({
    
    #inputs
    a=as.numeric(input$age)
    sa=as.numeric(input$sales)
    si=as.numeric(input$size)
    pr=as.numeric(input$product)
    int=as.numeric(input$interest)
    
    #testing
    #a=80
    #sa=1
    #si=-20
    #pr=2
    #int=1.5
    
    
    saeq=(80-a)+((10*(0.5-sa))+(si+10)+((10*(1.5-pr))+(10*(int-1.5))))
    nsweq=-30+saeq
    viceq=-10+saeq
    taseq=-25+saeq
    qldeq=-20+saeq
    nteq=-30+saeq
    
    price=c(nsweq,viceq,qldeq,saeq,taseq,nteq)
    
    name=c("NSW","VIC","QLD","SA", "TAS","NT")
    
  
    labels=paste(as.numeric(price),"%")
    labels=gsub(" ", "", labels, fixed = TRUE)
    
    data <- cbind.data.frame(price,labels,name)
    
    model.plot=merge(stateshp, data, by="name", all.x=TRUE)
    
    
    #plot ----
    
   # layout(matrix(c(1, 1, 1,
    #                1, 1, 1,
     #               2, 3, 4), nrow=3, byrow=TRUE))
    
    layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
    
    par(mar=c(0,2,0,0))#sets margins of plotting area
    
    #to float in the ocean
    # par(bg = 'light blue')
    #map("worldHires", "Australia", fill=TRUE,col="grey", xlim=c(125,165),
     #        ylim=c(-45,-15))
    #need to add=T into the next line
    #layout(matrix(1:2,nrow=2), width = c(1,1),height = c(2,1))
    
    
    #layout.show(n=4)
    legend_image <- as.raster(matrix(rev(df.new$colour), ncol=1))
    
    plot(model.plot, col = scol$colour[match(model.plot@data$price, scol$percent)]) #,xlim=c(140,150), ylim=c(-50,-10))
    text(model.plot, labels = as.character(model.plot@data$labels),cex=1.5)
    
    par(mar=c(25,0,10,4))#sets margins of plotting area
    
    plot(c(0,2),c(0,11),type = 'n', axes = F,xlab = '', ylab = '', main = '\n % Change \n in price')
    text(x=1.5, y = seq(0,11,l=11), labels = df.new$labels)
    
    rasterImage(legend_image, 0.5, 0, 1,11, angle=0)
    #text(y=-0.5, x = seq(0,11,l=11), labels = df.new$labels)
    
    #plot.new()
    
           
   # legend(x = "bottom", horiz=T, nrow=2,title="% Change \n in price",text.font=0.8,xpd=T,
    #       legend = rev((df.new$labels)),
     #      col = rev(df.new$colour), pch = 19, bty = 'n',xjust = -1,cex=1.4)
    
    
  })
  
   
  output$tab <- renderUI({
    tagList(url2)
  })
  
  output$tab1 <- renderUI({
    tagList(url)
  })
  
 # output$tab2 <- renderUI({
  #  tagList(url)
#  })
 # output$tab3 <- renderUI({
  #  tagList(url)
#  })
  
  
}




# Create Shiny app ----
shinyApp(ui, server)

# end ----

