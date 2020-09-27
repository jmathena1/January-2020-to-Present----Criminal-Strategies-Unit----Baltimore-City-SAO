#get the functions 
source("helper.R")

#set crime descriptions and neighborhoods for incidents ui
x <- get_UI_list(incident_url, "neighborhood")
x[[length(x)+1]] <- "DISTRICT-WIDE"
names(x)[length(x)] <- "DISTRICT-WIDE"

z <- get_UI_list(incident_url, "description")

#define crime_types
crime_types <- as.character(z)

#define districts
districts <- c("CENTRAL",
               "SOUTHEAST",
               "EASTERN",
               "NORTHEAST",
               "NORTHERN",
               "NORTHWEST",
               "WESTERN",
               "SOUTHWEST",
               "SOUTHERN",
               "CITYWIDE")

# Define UI for application 
ui <- fluidPage(
  #create Title for top of app
  navbarPage("Baltimore City Victim-based Crime Data",
      #create tabs to view various maps
                  #map tab containing the map output
                  tabPanel("Incident Map",
                           #create place for user to input date range
                           sidebarPanel(dateRangeInput("dates1", label = h4("Date range"),
                                                       start = paste(as.character(year(Sys.Date())), "-01", "-01", sep=""),
                                                       end = Sys.Date()),
                                        #district input
                                        selectInput("district",
                                                    label = h4("Choose a Police District in Baltimore City"),
                                                    choices = districts, selected = "CITYWIDE"),
                                        #neighborhood input 
                                        selectInput("neighborhood",
                                                    label = h4("Choose a neighborhood in the selected Police District, if applicable"),
                                                    choices = x, selected = "DISTRICT-WIDE"),
                                        #list of checkboxes to select the crimes to be viewed
                                        checkboxGroupInput("crimes", h4("Select the crimes you want mapped"),
                                                           choices = z, selected = z)),
                           mainPanel(leafletOutput('incident_map'),
                                     #outputs summary count of data in map
                                     tableOutput("map_description"),
                                     #button downloads a csv of the data used to create the map
                                     downloadButton('downloadcsv', "Download map points as CSV"))
                           )
  )
)
# Define server logic ----
server <- function(input, output) {
  #filters the raw OB incident data based on user input
  #if statement determines if the map should be city-wide or focussed on a single neighborhood
  filteredData <- reactive({
    withProgress(message='Gathering data for map points',
    if (input$district == 'CITYWIDE') {
       test <- get_new_data1(incident_url, input$dates1[1], input$dates1[2])
       test1 <- test[which(test$description %in% as.character(input$crimes)),]
    } else if (input$district != 'CITYWIDE' & input$neighborhood == 'DISTRICT-WIDE') {
      test <- get_new_data2(incident_url, input$dates1[1], input$dates1[2], input$district)
      test1 <- test[which(test$description %in% input$crimes),]
    } else if (input$district != 'CITYWIDE' & input$neighborhood != 'DISTRICT-WIDE') {
      test <- get_new_data2(incident_url, input$dates1[1], input$dates1[2], input$district)
      test1 <- test[which(test$neighborhood == input$neighborhood &
                            test$description %in% input$crimes
      ),]
    } else {
      print("ERROR!")
    }
    )
  })
  
  #builds map to be output into 'Incident Map' tab in UI
  output$incident_map <- renderLeaflet({
    #builds color palette for markers based on the crimes that the user wants to see
    colors <- c(topo.colors(length(crime_types)))
    pal <- colorFactor(palette = colors, domain = crime_types)
    
    #builds the map
    leaflet(filteredData()) %>% 
      #adds a basemap
      addTiles(group = "basemap") %>%
      #adds the markers for the map baed on the 'filteredData()' variable
      addCircles(lng = filteredData()$longitude, 
                 lat = filteredData()$latitude,
                 radius = 10,
                 color = ~pal(filteredData()$description),
                 #creates a popup which appears upon clicking on a circle
                 popup = paste("Date:", filteredData()$crimedate, "<br>", 
                               "Offense:", filteredData()$description,"<br>",
                               "Address:", filteredData()$location, "<br>",
                               "District:", filteredData()$district, "<br>", 
                               "Post:", filteredData()$post),
                 fill = TRUE,
                 fillOpacity = 0.75,
                 group = "Incident markers") %>%
      #create heat map layer
      addHeatmap(lng = filteredData()$longitude,
                 lat = filteredData()$latitude,
                 radius = 8,
                 group = "heatmap") %>%
      #adds Baltimore City Police District boundaries
      addKML(kml,
             group = "basemap",
             opacity = 0.2,
             popupProperty = 'description',
             fillColor = 'gray',
             fillOpacity = 0.01) %>%
      #adjusts the bounds of map view to fit all of the marks of the selection
      fitBounds(lng1 = min(filteredData()$longitude), 
                lat1 = min(filteredData()$latitude), 
                lng2 = max(filteredData()$longitude), 
                lat2 = max(filteredData()$latitude)) %>%
      #adds a legend
      addLegend("bottomleft", 
                pal = pal, 
                values = filteredData()$description, 
                title = "Crime Types", 
                opacity = 1,
                group = "legend") %>%
      #add map options for users
      addLayersControl(
        baseGroups = c("basemap"),
        overlayGroups = c("Incident markers", "heatmap", "legend"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      #hides the heatmap initially so the map isn't too cluttered
      hideGroup(c("heatmap", "legend"))
  })
  
  output$map_description <- renderTable({
    
    
    #create pivot table to display counts of various crimes
    pt1 <- PivotTable$new()
    pt1$addData(filteredData())
    pt1$addRowDataGroups("description")
    pt1$defineCalculation(calculationName = "Number of Incidents", summariseExpression = "n()")
    pt1$evaluatePivot()
    pt2 <- pt1$asDataFrame()
    pt3 <- pt2[order(-pt2$`Number of Incidents`), drop = FALSE,]
  }, rownames = TRUE)
  
  #let users download map data
  output$downloadcsv <- downloadHandler(
    filename = function() {
      paste("map_data", ".csv", sep = "")
    },
    content = function(filename) {
      write.csv(filteredData(), filename, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

