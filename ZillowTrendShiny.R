library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(sqldf)

setwd("c:/users/reeta/downloads")

data1 <- read.csv(file="Zip_MedianSoldPrice_AllHomes.csv")
MedianSoldPrice <- data1[c(2:7,seq(175,250,by=3))]
# MedianSoldPrice <- data1[c(1:7,seq(10,250,by=6))]

data2 <- read.csv(file="Zip_MedianListingPrice_AllHomes.csv")
MedianListingPrice <- data2[c(1:6, seq(9,84,by=3))]



# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Zillow Research"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "zipcode", label = strong("Zip Code"),
                                choices = unique(MedianListingPrice$RegionName), selected="98075")
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "500px")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$zipcode)
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    
    MedianListingPrice_new <- sqldf(paste("select * from MedianListingPrice where RegionName=",input$zipcode))
    MedianSoldPrice_new <- sqldf(paste("select * from MedianSoldPrice where RegionName=",input$zipcode))
    a <- as.numeric(t(MedianListingPrice_new[1,-(1:6)]))
    acol <- colnames(MedianListingPrice_new[1,-(1:6)])
    acol <- substr(acol,2,8)
    aDate <- as.yearmon(acol,"%Y.%m")
    z.MedianListingPrice_new <- zoo(a, order.by=aDate)
    
    b <- as.numeric(t(MedianSoldPrice_new[1,-(1:6)]))
    bcol <- colnames(MedianSoldPrice_new[1,-(1:6)])
    bcol <- substr(bcol,2,8)
    bDate <- as.yearmon(bcol,"%Y.%m")
    z.MedianSoldPrice_new <- zoo(b, order.by=bDate)
    
    y1 <- min(min(na.omit(a)),min(na.omit(b)))
    y2 <- max(max(na.omit(a)),max(na.omit(b)))
    desc1 <- paste("Zillow price Trend for", MedianSoldPrice_new$City,MedianSoldPrice_new$State, MedianSoldPrice_new$RegionName)
    
    plot(z.MedianListingPrice_new, type="l" , col="blue",ylim=c(y1,y2),yaxt="n", lwd=2, main=desc1) #, main=paste("Zipcode ",z.MedianSoldPrice_new$RegonName))
    myTicks = axTicks(2)
    axis(2, at = myTicks, labels = formatC(myTicks, format = 'd'))
    #axis(2,at=myTicks,labels=format(myTicks,scientific=FALSE))
    lines(z.MedianSoldPrice_new, col="red", lwd=2)
    legend("topleft", c("List Price", "Sold Price"), col=c("blue","red"),lty=1, lwd=2)

  })
  
 
}

# Create Shiny object
shinyApp(ui = ui, server = server)
