library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(rstudioapi)

#script.dir <- dirname(sys.frame(1)$ofile)
script.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

df = read.csv(paste(script.dir,"hurricanes_data_2017.csv",sep = "/"))
#df$Datetime = as.datetime(0,lubridate::ymd_hms(df$Datetime))

# Define UI #
ui <- fluidPage(
  
  # App title #
  titlePanel("Atlantic Hurricanes"),
  
  # fluidRow to split the top in one slider and two select #
  fluidRow(
  
    # Input: Animation with custom interval (in steps) #
    # to control speed, 1 second... #
    column(4,
           sliderInput("animation", "Animation:",
                       min = 1, max = nrow(df),
                       value = 1, step = 1,
                       animate =
                         animationOptions(interval = 500, loop = T))
           
           # fluidRow(
           #   column(6, df$Step),
           #   column(6,"colunm 6"))

    ),
    
    
    # Input: Years of the hurricanes #
    column(4,
    selectInput("ano", "Year:",
                 c("All",
                  unique(as.character(df$Year))),
                  selected = 2017)
    ),
      
    # Input: Names of Hurricanes #
    column(4,
    selectInput("nome", "Name:",
                 c("All",
                  unique(as.character(df$Name))),
                  selected = "ARLENE")
    ),
    # to create a UI element (map) #  
    leafletOutput("MapPlot1", height = 600)
    #),
    
    
    # Main panel we didn't need #
    #mainPanel()

  )
)


# Define server logic #
server = function(session, input, output) {
  
  # Define the icons that we use #
  # hurriIcon = makeIcon(
  #   iconUrl = "C:/Users/DELL/Desktop/hurricane.png",
  #   iconWidth = 10.32,
  #   iconAnchorX = 16,
  #   iconHeight = 14,
  #   iconAnchorY = 16
  # )
  # hurriIcon1 = makeIcon(
  #   iconUrl = "C:/Users/DELL/Desktop/hurricane1.png",
  #   iconWidth = 16.95,
  #   iconAnchorX = 16,
  #   iconHeight = 23,
  #   iconAnchorY = 16
  # )
  # hurriIcon2 = makeIcon(
  #   iconUrl = "C:/Users/DELL/Desktop/hurricane2.png",
  #   iconWidth = 23.58,
  #   iconAnchorX = 16,
  #   iconHeight = 32,
  #   iconAnchorY = 16
  # )
  # hurriIcon3 = makeIcon(
  #   iconUrl = "C:/Users/DELL/Desktop/hurricane3.png",
  #   iconWidth = 30.22,
  #   iconAnchorX = 16,
  #   iconHeight = 41,
  #   iconAnchorY = 16
  # )
  # hurriIcon4 = makeIcon(
  #   iconUrl = "C:/Users/DELL/Desktop/hurricane4.png",
  #   iconWidth = 38.85,
  #   iconAnchorX = 16,
  #   iconHeight = 50,
  #   iconAnchorY = 16
  # )
  # Define the icons that we use, but others size #
  hurriIcon = makeIcon(
    iconUrl = paste(script.dir,"hurricane.png",sep="/"),
    iconWidth = 17.69,
    iconAnchorX = 16,
    iconHeight = 24,
    iconAnchorY = 16
  )
  hurriIcon1 = makeIcon(
    iconUrl = paste(script.dir,"hurricane1.png",sep="/"),
    iconWidth = 20.64,
    iconAnchorX = 16,
    iconHeight = 28,
    iconAnchorY = 16
  )
  hurriIcon2 = makeIcon(
    iconUrl = paste(script.dir,"hurricane2.png",sep="/"),
    iconWidth = 23.58,
    iconAnchorX = 16,
    iconHeight = 32,
    iconAnchorY = 16
  )
  hurriIcon3 = makeIcon(
    iconUrl = paste(script.dir,"hurricane3.png",sep="/"),
    iconWidth = 26.53,
    iconAnchorX = 16,
    iconHeight = 36,
    iconAnchorY = 16
  )
  hurriIcon4 = makeIcon(
    iconUrl = paste(script.dir,"hurricane4.png",sep="/"),
    iconWidth = 29.48,
    iconAnchorX = 16,
    iconHeight = 40,
    iconAnchorY = 16
  )

  # we tried to filter the dataset #
  #dfs <- reactive({
  ##  df[df$Year == input$ano,]
  #  df[df$Name == input$nome,]
  #})
  #dfs <- reactive({
  #  df[df$Name == input$nome,]
  #})

  # map output with setView defined #
  output$MapPlot1 <- renderLeaflet({
    leaflet() %>% 
    addProviderTiles("OpenStreetMap.Mapnik") %>% 
    setView(lng = -54.5678, lat = 34.144, zoom = 3)
  })
  
  # update the select name based on year #
  observeEvent(input$ano,
               updateSelectInput(session, "nome", "Name",
                                 choices = df$Name[df$Year==input$ano]))
  # update the slider animation based on name #
  observeEvent(input$nome,
               updateSliderInput(session, "animation", "Animation:",
                                 max = as.numeric(df$Days[df$Name==input$nome][1])))

  # Create a reactive observer #
  observe({
    
    # Create a function to define which icon we will use #
    sIze <- function(X) {
      icon <- if (X <= 28){
        return(hurriIcon)
      } else if (X <= 56){
        return(hurriIcon1)
      } else if (X <= 84){
        return(hurriIcon2)
      } else if (X <= 112){
        return(hurriIcon3)
      } else return(hurriIcon4)
      return(icon)
    }
    # we tried to filter the dataset #
    #df <- df %>% 
    #  filter(df$Year == input$ano)
    #df <- df %>% 
    #  filter(df$Name == input$nome)
    
    # filter the dataset #
    dfs <- df[df$Year == input$ano,]
    dfs <- dfs[dfs$Name == input$nome,]

    # Creates a map-like object #
    leafletProxy("MapPlot1") %>%
      # Clear markers #  
      clearMarkers() %>%
      # Add markers # 
      addMarkers(lng = dfs$LongitudeHemisphere[input$animation:input$animation],
                 lat = dfs$LatitudeHemisphere[input$animation:input$animation],
                 icon = sIze(dfs$Wind.knots.[input$animation:input$animation]),
                 #label = dfs$Name,
                 popup=sprintf("<b>%s</b><br/><i>Wind speed</i>: %d km/h<br/><i>Radius</i>: %d km",
                               format(as_datetime(dfs$Datetime[input$animation:input$animation],"%Y-%m-%d %H:%M:%S"), "%d %B %Y %H:%M"),
                               as.integer(dfs$Wind.knots.[input$animation:input$animation]*1.852+0.5),
                               as.integer(dfs$Radius[input$animation:input$animation]*1.852+0.5)
                 )
                 #popup = paste(sep='','<b>',
                #              format(as.Date(dfs$Datetime[input$animation:input$animation],"%Y-%m-%d %H:%M:%S"), "%-d %B %Y %H:%M"),
                 #              '</b></br>')
                 )%>%
      # Add circles # 
      addCircles(lng = dfs$LongitudeHemisphere[1:input$animation],
                 lat = dfs$LatitudeHemisphere[1:input$animation],
                 radius = 5,
                 label = dfs$Name)
  })
  
}


# Create Shiny app ----
shinyApp(ui, server)