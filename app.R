############
SHINY APP

#load necessary labels 
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(sp)
library(rsconnect)
library(rgeos)
library(readr)
library(RColorBrewer)
library(leaflet.extras)
library(plotly)


#Crime contains all crime counts per month
crime <- readOGR("./c_all",layer="c_all")
#gUnary to dissolve crime: for outline of Liverpool
LSOA <- gUnaryUnion(crime, id=NULL)

#read in plotly csv
all <- read_csv("./all/all.csv")
all$Month <- factor(all$Month, levels=unique(all$Month))

#load all labels
labels <- read.csv("./labels/labels.csv")

#select columns
jan <- labels[c(4,5,6,7,8)]
feb <- labels[c(10,11,12,13,14)]
mar <- labels[c(16,17,18,19,20)]
apr <- labels[c(22,23,24,25,26)]
may <- labels[c(28,29,30,31,32)]
jun <- labels[c(34,35,36,37,38)]
jul <- labels[c(40,41,42,43,44)]
aug <- labels[c(46,47,48,49,50)]
sep <- labels[c(52,53,54,55,56)]
oct <- labels[c(58,59,60,61,62)]
nov <- labels[c(64,65,66,67,68)]
dec <- labels[c(70,71,72,73,74)]

#change names of columns
names(jan) <- c("anti","burg", "vio", "shop", "crim")
names(feb) <- c("anti","burg", "vio", "shop", "crim")
names(mar) <- c("anti","burg", "vio", "shop", "crim")
names(apr) <- c("anti","burg", "vio", "shop", "crim")
names(may) <- c("anti","burg", "vio", "shop", "crim")
names(jun) <- c("anti","burg", "vio", "shop", "crim")
names(jul) <- c("anti","burg", "vio", "shop", "crim")
names(aug) <- c("anti","burg", "vio", "shop", "crim")
names(sep) <- c("anti","burg", "vio", "shop", "crim")
names(oct) <- c("anti","burg", "vio", "shop", "crim")
names(nov) <- c("anti","burg", "vio", "shop", "crim")
names(dec) <- c("anti","burg", "vio", "shop", "crim")

#create label function with html
labs <- function(df4){
  sprintf("<span style='color: #858585; font: Arial;font-size: 12pt'><strong>Crime Type</strong><br/>
          <span style='color: #000000; font: Arial;font-size: 8pt'><strong>Anti Social Behaviour: %g</strong><br/> 
          <span style='color: #000000; font: Arial; font-size: 8pt'><strong>Burglary: %g</strong><br/>
          <span style='color: #000000; font: Arial; font-size: 8pt'><strong>Violent Crime: %g</strong><br/>
          <span style='color: #000000; font: Arial; font-size: 8pt'><strong>Shoplifting: %g</strong><br/>
          <span style='color: #000000; font: Arial; font-size: 8pt'><strong>Criminal Damage: %g</strong>",
          df4$anti, df4$burg, df4$vio, df4$shop, df4$crim) %>% 
    lapply(htmltools::HTML)
}

#apply label function
janL <- labs(jan)
febL <- labs(feb)
marL <- labs(mar)
aprL <- labs(apr)
mayL <- labs(may)
junL <- labs(jun)
julL <- labs(jul)
augL <- labs(aug)
sepL <- labs(sep)
octL <- labs(oct)
novL <- labs(nov)
decL <- labs(dec)

type = c("Anti Social Behaviour" = "Anti Social Behaviour", "Burglary" = "Burglary", "Violent Crime" = "Violent Crime", 
         "Shoplifting" = "Shoplifting", "Criminal Damage" = "Criminal Damage")
#custom tiles
MBaccessToken <- "pk.eyJ1IjoiaWJyZWNraGUiLCJhIjoidVNHX1VpRSJ9.9fPQ1A3rdxyCAzPkeYSYEQ"
MBurlTemplate <- "https://api.mapbox.com/styles/v1/mapbox/dark-v9/tiles/{z}/{x}/{y}?access_token="
MBTemplate <- paste(MBurlTemplate,MBaccessToken,sep="")


#define UI
header <- dashboardHeader(title = "Crime Reported in Liverpool 2017", titleWidth = 450,
                          dropdownMenuOutput("menu"),
                          dropdownMenu(
                            type = "notifications", 
                            icon = icon("question-circle"),
                            badgeStatus = NULL,
                            headerText = "See also:",
                            
                            notificationItem("Merseyside Police", icon("info-sign", lib = "glyphicon"),
                                             href = "https://www.merseyside.police.uk/"),
                            notificationItem("Github Repository", icon("github-square"),
                                             href = "https://github.com/tomstatham/"),
                            notificationItem("Data Police UK", icon("info-sign", lib = "glyphicon"),
                                             href = "https://data.police.uk/docs/")))


body <- dashboardBody(
  
  fluidRow(
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("mymap", height = "500px")),
           box(width=NULL,
               plotlyOutput("plot", height= "280px"))
    ),
    column(width=3,
           
           box(width = NULL,title = "About Crime Reported in Liverpool 2017", solidHeader = T, collapsible = T, status = 'danger',
               strong("Crime Reported in Liverpool 2017"),"is an interactive application built on shiny and leaflet, which allows you to visualize how the spatial pattern of crime in Liverpool changes over time.You can filter out crime reported each month, as well as the type of crime reported in 2017."
               
           ),
           
           
           
           box(width = NULL, title =tagList(shiny::icon("filter",class = 'fa-lg'), "Filter Data") ,
               solidHeader = T, collapsible = T, status = 'danger',
               selectizeInput('crimeType','Crime Type', choices = type,
                              selected = c('Anti Social Behaviour', "Burglary","Violent Crime",
                                           "Shoplifting","Criminal Damage"),multiple = T),
               sliderInput("range", "Crime Count", 
                           min(crime$jan), max(crime$jan),
                           value = range(crime$jan), step = 10)),
           
           box(width = NULL,
               icon('link', class = 'fa-lg'), a('Police Data UK', href = 'https://data.police.uk/docs/', target = '_blank'),
               br(),
               br(),
               icon('link', class = 'fa-lg'), a('Merseyside Police', href = 'https://www.merseyside.police.uk/', target = "_blank"),
               br(),
               br(),
               icon('github', class = 'fa-lg'), a('Source Code', href = 'https://github.com/tomstatham/', target = "_blank"))
           
           
    )
  )
)

#define user interface
ui <- dashboardPage(skin="black",
                    header,
                    dashboardSidebar(disable = TRUE),
                    body
)


#define server
server <- function(input, output, session) {
  
  #fileData <- reactiveFileReader(1000, session, 'labels.csv', read.csv)
  #filtered data
  filteredData <- reactive({
    crime[crime$jan >= input$range[1] & crime$jan<= input$range[2],]
  }) #range[1] = min #range[2]= max
  
  
  #define colour schemes
  colorpal <- reactive({
    pal <- colorBin("Reds", crime$jan, bins = c(0, 10,19, 31, 48, 99, 333))
  })
  
  colorpal1 <- reactive({
    pal1 <- colorBin("Reds", crime$feb, bins = c(0,10, 22,39, 65, 105, 321))
  })
  
  colorpal2 <- reactive({
    pal2 <- colorBin("Reds", crime$mar, bins = c(0,13, 27,44, 64, 135, 362))
  })
  
  colorpal3 <- reactive({
    pal3 <- colorBin("Reds", crime$apr, bins = c(0,12, 23,38, 60, 94, 349))
  })
  
  colorpal4 <- reactive({
    pal4 <- colorBin("Reds", crime$may, bins = c(0,13, 26,42, 60, 96, 341))
  })
  
  colorpal5 <- reactive({
    pal5 <- colorBin("Reds", crime$jun, bins = c(0,10,19, 31,48, 83, 280))
  })
  
  colorpal6 <- reactive({
    pal6 <- colorBin("Reds", crime$jul, bins = c(0,11,20, 30,48, 88, 292))
  })
  
  colorpal7 <- reactive({
    pal7 <- colorBin("Reds", crime$aug, bins = c(0,10,20, 33,51, 79, 328))
  })
  
  colorpal8 <- reactive({
    pal8 <- colorBin("Reds", crime$sep, bins = c(0,10,20,33,50, 91, 308))
  })
  
  colorpal9 <- reactive({
    pal9 <- colorBin("Reds", crime$oct, bins = c(0,14,27, 45,65, 107, 363))
  })
  
  colorpal10 <- reactive({
    pal10 <- colorBin("Reds", crime$nov, bins = c(0,10,19, 31,51, 88, 317))
  })
  
  colorpal11 <- reactive({
    pal11 <- colorBin("Reds", crime$dec, bins = c(0,10,18, 29,50, 102, 352))
  })
  
  output$mymap <- renderLeaflet({
    leaflet(crime) %>%
      addTiles(MBTemplate, options=tileOptions(minZoom=11, maxZoom=18)) %>%
      setView(-2.959761, 53.404049, 11)
    
  })
  
  observe({
    
    pal <- colorpal()
    pal1 <- colorpal1()
    pal2 <- colorpal2()
    pal3 <- colorpal3()
    pal4 <- colorpal4()
    pal5 <- colorpal5()
    pal6 <- colorpal6()
    pal7 <- colorpal7()
    pal8 <- colorpal8()
    pal9 <- colorpal9()
    pal10 <- colorpal10()
    pal11 <- colorpal11()
    
    
    leafletProxy("mymap", data = filteredData())  %>%
      clearShapes() %>%
      clearPopups() %>%
      
      addPolygons(
        fillColor = ~pal(jan),
        fillOpacity = 0.6, 
        highlight = highlightOptions( #highlight works but labels don't
          weight = 5,
          color = "#666",
          dashArray = "",
          bringToFront = TRUE),
        label = janL, #as.character returns values but not correctly
        labelOptions = labelOptions(noHide = TRUE),
        group = "January", #groups need "" for shiny to work
        color = "#BDBDC3",
        weight = 1) %>%
      
      addPolygons(
        fillColor = ~pal1(feb),
        fillOpacity = 0.6, 
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          bringToFront = TRUE),
        label = febL,
        group = "February", #groups need "" for shiny to work
        color = "#BDBDC3",
        weight = 1) %>%
      
      addPolygons(
        fillColor = ~pal2(mar),
        fillOpacity = 0.6, 
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          bringToFront = TRUE),
        label = marL,
        group = "March", #groups need "" for shiny to work
        color = "#BDBDC3",
        weight = 1) %>%
      
      addPolygons(
        fillColor = ~pal3(apr),
        fillOpacity = 0.6, 
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          bringToFront = TRUE),
        label = aprL,
        group = "April", #groups need "" for shiny to work
        color = "#BDBDC3",
        weight = 1) %>%
      
      addPolygons(
        fillColor = ~pal4(may),
        fillOpacity = 0.6, 
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          bringToFront = TRUE),
        label = mayL,
        group = "May", #groups need "" for shiny to work
        color = "#BDBDC3",
        weight = 1) %>%
      
      addPolygons(
        fillColor = ~pal5(jun),
        fillOpacity = 0.6, 
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          bringToFront = TRUE),
        label = junL,
        group = "June", #groups need "" for shiny to work
        color = "#BDBDC3",
        weight = 1) %>%
      
      addPolygons(
        fillColor = ~pal6(jul),
        fillOpacity = 0.6, 
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          bringToFront = TRUE),
        label = julL,
        group = "July", #groups need "" for shiny to work
        color = "#BDBDC3",
        weight = 1) %>%
      
      
      addPolygons(
        fillColor = ~pal7(aug),
        fillOpacity = 0.6, 
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          bringToFront = TRUE),
        label = augL,
        group = "August", #groups need "" for shiny to work
        color = "#BDBDC3",
        weight = 1) %>%
      
      addPolygons(
        fillColor = ~pal8(sep),
        fillOpacity = 0.6, 
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          bringToFront = TRUE),
        label = sepL,
        group = "September", #groups need "" for shiny to work
        color = "#BDBDC3",
        weight = 1) %>%
      
      addPolygons(
        fillColor = ~pal9(oct),
        fillOpacity = 0.6, 
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          bringToFront = TRUE),
        label = octL,
        group = "October", #groups need "" for shiny to work
        color = "#BDBDC3",
        weight = 1) %>%
      
      addPolygons(
        fillColor = ~pal10(nov),
        fillOpacity = 0.6, 
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          bringToFront = TRUE),
        label = novL,
        group = "November", #groups need "" for shiny to work
        color = "#BDBDC3",
        weight = 1) %>%
      
      addPolygons(
        fillColor = ~pal11(dec),
        fillOpacity = 0.6, 
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          bringToFront = TRUE),
        label = decL,
        group = "December", #groups need "" for shiny to work
        color = "#BDBDC3",
        weight = 1) %>%
      
      #layer to visualize outline of liverpool
      addPolygons(data = LSOA, 
                  stroke = T, 
                  smoothFactor = 0.05, 
                  fillOpacity = 0.05, 
                  color = "red",
                  weight = 1) %>%
      
      addLegend("bottomleft",pal = pal, title = "Crimes reported (2017)",values = crime$jan, opacity = 1) %>%
      addScaleBar(position = c("bottomright"), options = scaleBarOptions(metric = TRUE)) %>%
      addFullscreenControl(pseudoFullscreen = TRUE) %>%
      
      addLayersControl(baseGroups = c("January","February","March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), options = layersControlOptions(collapsed = FALSE))
    
  })
  
  
  observe({ 
    proxy <- leafletProxy("mymap", data = crime) 
  })
  
  #define title 
  t <- list(
    title = "Type of Crime Reported per Month in Liverpool (2017)",
    family = "sans-serif",
    size=20,
    color = "#c7c5c5")
  
  #define x label
  x <- list(
    title = "Month",
    gridcolor = "rgb(255, 255, 255)",
    family = "sans-serif",
    size=14,
    color="grey")
  
  #define y label
  y <- list(
    title = "Crime Reported",
    gridcolor = "rgb(255, 255, 255)",
    family = "sans-serif",
    size=14,
    color="grey")
    
  #define legend
  l <- list(
    font = list(
      family = "sans-serif",
      size = 12,
      color = "#c7c5c5"))
  
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    plot_ly(all, x =~Month) %>%
      add_lines(y=~Anti_social_behaviour, name = "Anti Social Behaviour",line=list(shape="linear", color="#bd0026")) %>%
      add_lines(y=~Violent_crime, name = "Violent Crime", line=list(shape="linear", color="#f03b20")) %>%
      add_lines(y=~Criminal_damage, name = "Criminal Damage", line=list(shape="linear", color="#fd8d3c")) %>%
      add_lines(y=~Burglary, name = "Burglary", line=list(shape="linear",color="#fecc5c")) %>%
      add_lines(y=~Shoplifting, name = "Shoplifting", line=list(shape="linear",color="#ffffb2")) %>%
      layout(title = "Type of Crime Reported per Month in Liverpool (2017)", 
             titlefont = list(color = "rgb(204, 204, 204)", size = 18),
             xaxis = x, yaxis = y,
             margin = list(r = 20, t = 35, b = 35, l = 60, pad = 1),
             legend=l) %>%
      layout(plot_bgcolor='#747474') %>%
      layout(paper_bgcolor='#404040') 
  })
  
}


#run shiny app
shinyApp(ui, server)
