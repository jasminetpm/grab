library(shiny)
library(leaflet)
library(magrittr)
library(sp)
library(rgdal)
library(RColorBrewer)

# load data
route_data <- read.csv("./data/test_assignmentv2.csv", stringsAsFactors=FALSE)

# sort data based on the hour (ascending) and number of rides (descending)
route_data <- route_data[order(route_data$hour_of_day, -route_data$Rides),]
# View(route_data)

# unique pickup and dropoff locations
# note that the pickup and dropoff locations are similar (54 unique locations each)
route_data_unique <- unique(route_data[c("pickup", "latitude", "longitude")])

# Define UI for application
ui <- fluidPage(
   
   # application title
   titlePanel("Most Popular Routes in Singapore"),
   
   # sidebar with a slider input for the hour of the day
   sidebarLayout(
      sidebarPanel(
         sliderInput("hour",
                     "Hour of Day:",
                     min = 0,
                     max = 23,
                     value = 12)
      ),
      
      # show a plot of the generated map
      mainPanel(
         leafletOutput("map")
      )
   )
)




# Define server logic required to plot map
server <- function(input, output) {
  
  m = leaflet() %>% 
    
    # set map background
    addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
    setView(lng = 103.816964, lat = 1.368545, zoom = 11) %>%
    
    # set location Markers
    addCircles(data = route_data_unique, lat = ~ latitude, lng = ~ longitude) %>%
    addMarkers(data = route_data_unique,
               lng= ~longitude, lat= ~latitude,
               popup = ~as.character(pickup), label = ~as.character(pickup),
               group = "Location Markers")
  
  
  #render leaflet
  output$map = renderLeaflet({
    
    # since we want the most popular routes, we subset the data to Rides>9000 to declutter map
    route_data_hr <- route_data[route_data$hour_of_day==input$hour & route_data$Rides > 9000,]
    
    # make a new column for the group so that we can plot the Polylines
    route_data_hr$group <- c(1:nrow(route_data_hr))
    
    # define a new data frame for ease of plotting Polylines 
    # (data is based on hour of day, provided by slider input)
    df_hr <- data.frame(group = route_data_hr$group,
                       origin_lat = route_data_hr$latitude,
                       origin_long = route_data_hr$longitude,
                       dest_lat = route_data_hr$dropoff_latitude,
                       dest_long = route_data_hr$dropoff_longitude,
                       rides = route_data_hr$Rides,
                       stringsAsFactors = FALSE)
    
    # colour palette for Polylines
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = df_hr$rides)
    
    # draw a Polyline for each row of data frame
    for(i in 1:nrow(df_hr)){
      m <- addPolylines(m, lat = as.numeric(df_hr[i, c("origin_lat", "dest_lat")]), 
                        lng = as.numeric(df_hr[i, c("origin_long", "dest_long")]), 
                        weight = 0.7 , opacity = 0.5,
                        color = pal(df_hr[i, "rides"]))
    }
    
    # set legend: colour gets darker as no. of rides increases
    m <- addLegend(m, "bottomright", pal = pal, values = df_hr$rides,
                  title = "Number of Rides",
                  labFormat = labelFormat(prefix = " "),
                  opacity = 0.75
    )
    
    # set CircleMarkers to show the popularity of pickup points:
    # size of circle increases as no. of rides at pickup location increases
    m <- addCircleMarkers(m, data = df_hr, ~origin_long, ~origin_lat, 
                      # re-express no. of rides so radius can be better sized
                       radius = ~exp(rides/1000)/1000, stroke = FALSE,
                       color = "red",
                       fillColor = "red", fillOpacity = 0.05,
                       group = "Popular Pickups") 
    
    # set CircleMarkers to show the popularity of dropoff points:
    # size of circle increases as no. of rides at dropoff location increases
    m <- addCircleMarkers(m, data = df_hr, ~dest_long, ~dest_lat, 
                       radius = ~exp(rides/1000)/1000, stroke = FALSE, 
                       color = "green",
                       fillColor = "green", fillOpacity = 0.05,
                       group = "Popular Dropoffs") %>%
      
      # set LayerControl to toggle visibility of diff. layers
      addLayersControl(
        baseGroups = c("Light", "Dark"),
        overlayGroups = c("Popular Pickups", "Popular Dropoffs", "Location Markers"),
        options = layersControlOptions(collapsed = FALSE))
    
    # render map
    m
  })
  
  

}


# Run the application 
shinyApp(ui = ui, server = server)

