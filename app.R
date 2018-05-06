library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)

#load data
data = read.csv("trip_length.csv")
datayellow=read.csv("09-15.csv")
datagreen=read.csv("green13-15.csv")
yellowData = data[data['color'] == 'yellow', ]
greenData = data[data['color'] == 'green', ]
trip_area = read.csv("trip_area.csv")
trip_area$Airports = 1000 * trip_area$Airports
trip_area_melt <- melt(trip_area, id="Year") 


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "The NYC Taxi"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("yellow_taxi_trip_length", tabName = "yellow"),
      menuItem("green_taxi_trip_length", tabName = "green"),
      menuItem("Airport_and_Mahattan", tabName = "areas")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "yellow",
              fluidRow(
                box(selectInput("a_mode",
                                "Mode:",
                                choices = list("yellow_taxi_time_series", "yellow_taxi_by_year")), 
                    plotOutput("plot1"), width = 12)
              ),
              p("According to the yellow taxi time series,trip lengths increased dramaticly in the beginning of 2010 and the middle of 2013, I guess its because the weather of these two periods are too cold or too hot, so people used Taxi more, or there were some events in these two periods. 
Also, people used taxi more in 2015 compared to the previous years. <br> <br>
                According to the yellow taxi trip lengths by year, we can tell that the yellow taxi trip lengths are generally increased, but ups and downs frequently, ex. yellow taxi trip lengths decreased from 2013 to 2014, increased from 2014 to 2015, and then decreased from 2015 to 2016.")
      ),
    
        tabItem(tabName = "green",
                fluidRow(
                  box(selectInput("b_mode",
                                  "Mode:",
                                  choices = list("green_taxi_time_series", "green_taxi_by_year")), 
                      plotOutput("plot2"), width = 12)
                ),
                p("For the green taxi Time Series, because we do not have the data before 08/01/2013, I use zero to replace the trip length. According to the green taxi plot, we can see that there is no dramatic peak for green taxi, which is different from the yellow taxi.<br> <br>
                   For the gree taxi the green taxi trip lengths by year, we can see the trip length incresed from 2013 to 2015 and decresed from 2015 to 2016.")
      ),
        tabItem(tabName = "areas",
                fluidRow(
                  box(selectInput("c_mode",
                                  "Mode:",
                                  choices = list("yellow_taxi_in_different_areas")),
                      plotOutput("plot3"), width=12)
                ),
                p("According to the plot, the trip lengths of Yellow taxi has different trending in Manhattan and Airport from 2009 to 2015. The Airport trip lengths dicreased from 2010, but the trip lengths of Manhattan generally incresed from 2010 to 2015, especially from 2014 to 2015. However, the trip lengths of Manhattan decresed dramaticlly from the beginning of 2015. 
")
      )
    )
  ))

# Define server logic
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    if (input$a_mode == "yellow_taxi_time_series") {
      
      graph <- ts.plot(ts(datayellow[,-1], start = c(2009,1), frequency = 12), gpars = list(xlab = " ", ylab = " ", lty=1))
      print(graph)
    }
    
    if (input$a_mode == "yellow_taxi_by_year") {
      
      graph<-ggplot(data=yellowData, aes(x=year, y=trip_length, group=1)) +
        geom_line()+
        geom_point() + 
        labs(title="Plot of Yellow Trip Length",x="Year", y = "Trip Length")
      print(graph)
    }  
  })
  
  output$plot2 <- renderPlot({
    if (input$b_mode == "green_taxi_time_series") {
      graph <- ts.plot(ts(datagreen[,-1], start = c(2013,1), frequency = 12), gpars = list(xlab = " ", ylab = " ", lty=1))
      print(graph)
    }
    
    if (input$b_mode == "green_taxi_by_year") {
      
      graph <- ggplot(data=greenData, aes(x=year, y=trip_length, group=1)) +
        geom_line()+
        geom_point() + 
        labs(title="Plot of Green Trip Length",x="Year", y = "Trip Length")
      print(graph)
    }
  })
  output$plot3 <- renderPlot({
      if (input$c_mode == "yellow_taxi_in_different_areas") {
        
        graph <- ggplot(data=trip_area_melt,
                        aes(x=Year, y=value, colour=variable)) +
          geom_line() + geom_point() +
          labs(title="Plot of Yellow Trip Length of different areas",x="Year", y = "Trip Length")
        print(graph)
       }
  })
}

#Run the application
shinyApp(ui = ui, server = server)
      

