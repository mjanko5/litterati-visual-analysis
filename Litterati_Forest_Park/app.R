# Matt Jankowski
# Project 1 - R and Shiny

#libraries to include

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)

#FILE READING

# # assume all of the tsv files in this directory are data of the same kind that I want to visualize

# temp = list.files(pattern="*.tsv")
# allData2 <- lapply(temp, read.delim)
# allData3 <- do.call(rbind, allData2)

# #some of the temperature readings are 32 (invalid value) so lets remove any row that has one of those
# allData <- subset(allData3, S1 != "32" & S2 != "32" & S3 != "32" & S4 != "32" & S5 != "32" & S6 != "32" & S7 != "32" )

# otherwise I could load them all in independantly and combine them afterwards
# evl2005 <- read.table(file = "history_2005.tsv", sep = "\t", header = TRUE) # etc for each one then combine them
# allData <- rbind(evl2005, evl2006, evl2007, evl2008, evl2009, evl2010, evl2011, evl2012, evl2013, evl2014, evl2015, evl2016)




#get the data in
rawdata <- read.table(file = "https://www.evl.uic.edu/aej/424/litterati challenge-65.csv", sep = ",", header = TRUE)

#some of the locations are not in Forest Park, so let's get rid of those data points
filtered <- subset(rawdata, lat > 41.84 & lon < -87.80 & lon > -87.83)



#DATE MANIPULATION

# # convert the dates to the internal format
# allData$newDate <- as.Date(allData$Date, "%m/%d/%Y")
# allData$Date <- NULL

#dates
newDate <- ymd_hms(filtered$litterTimestamp)   #ex: 2020-01-07 22:14:20 UTC
date <- date(newDate)                          #only hours
hour <- hour(newDate)
weekday <- weekdays(newDate)

filtered$date <- date       #add new data to table
filtered$hour <- hour       #add new data to table
filtered$weekday <- weekday
filtered$litterTimestamp <- NULL   #remove old data from table





#CONVERSION TO NUMERIC (?)

# # convert the temperatures from strings to numbers
# allData$S1 <- as.numeric(as.character(allData$S1))
# allData$S2 <- as.numeric(as.character(allData$S2))
# allData$S3 <- as.numeric(as.character(allData$S3))
# allData$S4 <- as.numeric(as.character(allData$S4))
# allData$S5 <- as.numeric(as.character(allData$S5))
# allData$S6 <- as.numeric(as.character(allData$S6))
# allData$S7 <- as.numeric(as.character(allData$S7))


#CONVERSION TO MEMORABLE (?)

# # convert the room codes to more memorable room names
# names(allData)[names(allData)=="S6"] <- "Meeting Room"
# names(allData)[names(allData)=="S5"] <- "Main Lab"
# names(allData)[names(allData)=="S7"] <- "Machine Room"
# names(allData)[names(allData)=="S3"] <- "Thin Rooms"
# names(allData)[names(allData)=="S4"] <- "Ph.D. Room"
# names(allData)[names(allData)=="S2"] <- "Classroom"
# names(allData)[names(allData)=="S1"] <- "Work Room"


# <A> 

# # Create the menu items to select the different years and the different rooms
# listNames <- c(colnames(filtered))
# 
# listNamesGood <- listNames[listNames != "Hour" & listNames != "newDate"]
# years<-c(2005:2019)

# get top 10 pickers:
pickerFreq <- as.data.frame(table(filtered$username))
top10pickers <- head(pickerFreq[order(-pickerO$Freq),], 10)

# pickerOccurences <-table(unlist(filtered$username))         #get frequency of each and convert df to table 
# top10pickers <- sort(pickerOccurences, decreasing=T)[1:10] #sort and get top 10

# top10tags <- ?



#SHINY DASHBOARD

# Create the shiny dashboard
ui <- dashboardPage(
    dashboardHeader(title = "Litterati Forest Park Challenge - Data Analysis"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     
                     # sidebarMenu(
                     #     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     #     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     #     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     #     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     #     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                     # menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     
                     selectInput("Picker", "Top 10 Pickers", top10pickers$Var1, selected = 'julieta'),
                     selectInput("Tag", "Top 10 Tags", top10pickers, selected = 'plastic')
    ),
    dashboardBody(
        fluidRow(
            column(2,
                   fluidRow(
                       box(title = "Map of Forest Park", solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("leaf", height = 300)
                       )
                   ),
                   fluidRow(
                       box(title = "Total Amount Litter Pieces Picked", solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("total", height = 200)
                       )
                   ),
                   fluidRow(
                       box(title = "Top 10 Pickers", solidHeader = TRUE, status = "primary", width = 12,
                           leafletOutput("top10pickers", height = 200)
                       )
                   )
            ),
            
            column(8,
                   fluidRow(
                       box( title = "Litter Picked Over Time", solidHeader = TRUE, status = "primary", width = 12,
                            plotOutput("bar1", height = 200)
                       )
                   ), 
                   fluidRow(
                       box( title = "Litter Picked by Hour of Day", solidHeader = TRUE, status = "primary", width = 12,
                            plotOutput("bar2", height = 200)
                       )
                   ),
                   fluidRow(
                       box( title = "Litter Picked by Day of the Week", solidHeader = TRUE, status = "primary", width = 12,
                            plotOutput("bar3", height = 200)
                       )
                   ),
                   fluidRow(
                       box( title = "Litter Picked by Category (Tag)", solidHeader = TRUE, status = "primary", width = 12,
                            plotOutput("bar4", height = 200)
                       )
                   )
            ),
            column(2,
                   fluidRow(
                       box(title = "Litter Picked Over Time", solidHeader = TRUE, status = "primary", width = 12,
                           dataTableOutput("table1", height = 200)
                       )
                   ),
                   fluidRow(
                       box(title = "Litter Picked by Hour of Day", solidHeader = TRUE, status = "primary", width = 12,
                           dataTableOutput("table2", height = 200)
                       )
                   ),
                   fluidRow(
                       box(title = "Litter Picked by Day of the Week", solidHeader = TRUE, status = "primary", width = 12,
                           dataTableOutput("table3", height = 200)
                       )
                   ),
                   fluidRow(
                       box(title = "Litter Picked by Category (Tag)", solidHeader = TRUE, status = "primary", width = 12,
                           dataTableOutput("table4", height = 200)
                       )
                   )                  
            )
        )
    ))

server <- function(input, output) {
    
    # increase the default font size
    theme_set(theme_grey(base_size = 18)) 
    
    
    #GET REACTIVE DATA FROM INPUT:
        
    # # calculate the values one time and re-use them in multiple charts to speed things up
    # justOneYearReactive <- reactive({subset(allData, year(allData$newDate) == input$Year)})
    # newNoonsReactive <- reactive({subset(allData, year(allData$newDate) == input$Year & Hour == 12)})
    # oneRoomNoonReactive <- reactive({subset(allData$input$Room, year(allData$newDate) == input$Year & Hour == 12)})
    
    justOnePickerReactive <- reactive({subset(filtered, filtered$username == input$Picker)}) #contatins just the data about selected picker
    
    
      observeEvent(input$Picker, {
        print(paste0("You have chosen: ", input$Picker))
      })
    
    #?
    # in 2017 it was y=justOneYear["Hour"] - needed to make a change for 2018
    
    # # <F> create heat map for the given year and room and play with the colors to make it more readable
    # output$hist0 <- renderPlot({
    #     justOneYear <- justOneYearReactive()
    #     ggplot(justOneYear, aes(x=newDate, color=justOneYear[,input$Room], y=justOneYear$Hour)) + 
    #         labs(x=paste("Day in", input$Year), y = "Hour of the Day") +  geom_point(alpha = 1/2, size = 4, shape=15) + scale_y_continuous() + 
    #         
    #         theme_dark(18) + theme(plot.background = element_rect(fill = "gray50")) + theme(axis.title = element_text(colour = "white")) +
    #         theme(axis.text = element_text(colour = "white")) + theme(panel.grid.major = element_line(colour = "white")) + 
    #         theme(panel.grid.minor = element_line(colour = "white")) +
    #         theme(legend.background = element_rect(fill="gray50")) +
    #         theme(legend.text = element_text(colour = "white"))  +
    #         theme(legend.title = element_text(colour = "white"))  +
    #         
    #         scale_colour_gradient2(low = "green", high = "red", limits=c(60, 90), midpoint=70, mid="yellow") +
    #         theme(legend.position="top") + labs(colour = "Temperature (F) ") +
    #         scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0))
    # })
    # 
    
    
    
    # <D> show all of the temperatures for a given room for a given year
    # output$bar1 <- renderPlot({
    #     justOneYear <- justOneYearReactive()
    #     ggplot(justOneYear, aes(x=newDate, y=justOneYear[,input$Room])) +
    #         labs(x=paste("Day in", input$Year), y = "Temperature (F)") + geom_point(alpha = 1/12) + ylim(55,90) +
    #         scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0))
    # })
    
    
    output$bar1 <- renderPlot({
    justOnePicker <- justOnePickerReactive()
        ggplot(justOnePicker, aes(x=factor(date))) +
            labs(x="date", y = "Amount per Day") + geom_bar(stat="count", width=0.7, fill="steelblue")
    })
    
    
    
    # # <E> show a line graph of the temperatures at noon for a given room for a given year
    # output$hist2 <- renderPlot({
    #     newNoons <- newNoonsReactive()
    #     ggplot(newNoons, aes(x=newDate, y=newNoons[,input$Room])) +
    #         labs(x=paste("Day in", input$Year), y = "Temperature (F)") + geom_point() + geom_line() + ylim(55,90) +
    #         scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0))
    # })
    
    output$bar2 <- renderPlot({
    justOnePicker <- justOnePickerReactive()
        ggplot(justOnePicker, aes(x=hour)) + 
            labs(x="hour", y = "Amount per Hour") + 
            geom_bar(stat="count", width=0.7, fill="steelblue")

    })
    
    
    # 
    # 
    # # <H> show a bar chart of the temperatures at noon for a given room for a given year
    # output$hist3 <- renderPlot({
    #     newNoons <-  newNoonsReactive()
    #     temperatures <- as.data.frame(table(newNoons[,input$Room]))
    #     temperatures$Var1 <- as.numeric(as.character(temperatures$Var1))
    #     
    #     ggplot(temperatures, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="steelblue") +
    #         labs(x="Temperature (F)", y = "Count") + xlim(60,90)
    # })
    # 
    # 
    # # <G> show box plot of the temperatures at noon for a given room for a given year
    # output$hist4 <- renderPlot({
    #     newTemps2 <- justOneYearReactive()
    #     ggplot(newTemps2, aes(x = "", y = newTemps2[,input$Room])) + geom_boxplot() + labs(y="Temperature (F)", x="") + ylim(55,90)
    # })
    # 
    # 
    # # <I> use DT to help out with the tables - https://datatables.net/reference/option/
    # output$tab1 <- DT::renderDataTable(
    #     DT::datatable({ 
    #         newNoons <-  newNoonsReactive()
    #         temperatures <- as.data.frame(table(newNoons[,input$Room], dnn = list("Temperature")), responseName = "Count")
    #     }, 
    #     options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
    #     ), rownames = FALSE 
    #     )
    # )
    # 
    # # <B> read in a jpeg map of the lab to show the room layout and plot some text on it
    # output$jpeg <- renderPlot({
    #     # read in a jpg image
    #     jp <- jpeg::readJPEG('evl_2nd_floor.jpg')
    #     
    #     df <- data.frame(x = 1:10, y = 1:10) # set the range to be 1 to 10 in x and y for the image
    #     
    #     markerX = 0
    #     markerY = 0
    #     
    #     if (input$Room == "Classroom")
    #     {
    #         markerX = 3
    #         markerY = 4.5
    #     }
    #     else if (input$Room == "Ph.D. Room")
    #     {
    #         markerX = 5.25
    #         markerY = 4.5
    #     }
    #     else if (input$Room == "Thin Rooms")
    #     {
    #         markerX = 6.7
    #         markerY = 4.5
    #     }
    #     else if (input$Room == "Machine Room")
    #     {
    #         markerX = 8.75
    #         markerY = 4.5
    #     }
    #     else if (input$Room == "Work Room")
    #     {
    #         markerX = 3
    #         markerY = 7.1
    #     }
    #     else if (input$Room == "Meeting Room")
    #     {
    #         markerX = 6
    #         markerY = 7.1
    #     }
    #     else if (input$Room == "Main Lab")
    #     {
    #         markerX = 8.75
    #         markerY = 7.1
    #     }
    #     else
    #     {
    #         markerX = 0
    #         markerY = 0
    #     }
    #     
    #     ggplot(df, aes(x,y)) + geom_blank() + labs(x="", y = "") +
    #         annotation_custom(rasterGrob(jp, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) + 
    #         
    #         annotate("text", x = 3, y = 4.5, label = "Classroom") +
    #         annotate("text", x = 5.25, y = 4.5, label = "Ph.D \n Room") +
    #         annotate("text", x = 6.7, y = 4.5, label = "Thin Rooms") +
    #         annotate("text", x = 8.75, y = 4.5, label = "Machine Room") +
    #         annotate("text", x = 3, y = 7.1, label = "Work \n Room") +
    #         annotate("text", x = 6, y = 7.1, label = "Meeting \n Room") +
    #         annotate("text", x = 8.75, y = 7.1, label = "Main Lab") +
    #         
    #         # show the selected room with a colored overlay based on input$Room
    #         annotate("rect", xmin = markerX-0.9, xmax = markerX+0.9, ymin = markerY-0.5, ymax = markerY+0.5, fill="green", alpha=0.3) +
    #         
    #         theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    #         theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
    # })
    # 
    # # <C> add a leaflet map and put a marker on it at the location of the lab
    # # while not overly useful this can ceratinly be expnded upon
    # output$leaf <- renderLeaflet({
    #     map <- leaflet()
    #     map <- addTiles(map)
    #     map <- setView(map, lng = -87.647998, lat = 41.870, zoom = 18)
    #     map <- addMarkers(map, lng = -87.6477, lat = 41.8698, popup = "evl")
    #     map
    # })
    # 
    
}

shinyApp(ui = ui, server = server)
