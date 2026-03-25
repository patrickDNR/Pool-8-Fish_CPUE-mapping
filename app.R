#Try leaflet?

library(leaflet)
library(tidyverse)
library(lubridate)
library(sf)
library(paletteer)
library(shiny)
library(bslib)
library(viridis)

#load data -- get from github
#load data -- grab from github
data_url <- 'https://raw.githubusercontent.com/patrickDNR/Pool-8-Fish_CPUE-mapping/refs/heads/main/Data/CPUE_all.csv'
download.file(data_url, 'CPUE_all.csv')

cpue <- read.csv('CPUE_all.csv') %>%
  dplyr::mutate(Year = as.numeric(format(mdy(sdate), '%Y'))) %>%
  dplyr::mutate(Month = month(mdy(sdate), label = T)) %>%
  dplyr::mutate(DATE = as.Date(format(mdy(sdate), '%Y-%m-%d')))

#convert UTM to lat lon
utm_crs <- 32615

#Convert to sf object
utm_sf <- st_as_sf(cpue, coords = c('zone15e', 'zone15n'), crs = utm_crs)

#transform to geographic coordinates
latlon_sf <- st_transform(utm_sf, crs = 4326)

#make into data frame
latlon_df <- as.data.frame(st_coordinates(latlon_sf))
colnames(latlon_df) <- c('lng', 'lat')

#combine with water quality data
cpue <- cbind(cpue, latlon_df)

# Define UI for water quality map app ----
ui <- fluidPage(
  
  bslib::page_navbar(
    
    #set up ID
    id = 'nav', 
    
    
    #make a title for the app
    title = 'Fish CPUE Time Series and Map', 
    
    
    #Set background color
    bg = 'darkgreen', 
    
    #Set theme
    theme = bslib::bs_theme(version = 5), 
    
    #Window title 
    
    window_title = 'Fish CPUE', 
    
    #make it mobile friendly?
    
  fillable_mobile = TRUE, 
  
  #Now for the good stuff, make a sidebar first...
  
  # Sidebar layout with input and output definitions ----
  sidebar = bslib::sidebar(
    id = 'sidebar', 
    width = 600, 
    position = 'left', 
    open = TRUE,
    
    # Sidebar panel for inputs ----
    conditionalPanel(
      condition = 'input.nav' == 'Time series plot' ||
        'input.nav' == 'Sample point map',
      width = 2/3,
      
      # Input: Maybe a slider for time series?
      sliderInput(inputId = 'date_range', 
                     label = 'Select Date Range:', 
                     min = min(cpue$Year), 
                     max = max(cpue$Year),
                     step = 1,
                     value = c(min(cpue$Year), max(cpue$Year)), 
                      sep = '', 
                  ticks = F),
      
      #Select months of interest
      checkboxGroupInput(inputId = 'months', 
                         label = 'Select Sampling Months:', 
                         choices = c('June' = 'Jun', 
                                     'July' = 'Jul', 
                                     'August' = 'Aug', 
                                     'September' = "Sep", 
                                     'October' = 'Oct', 
                                     'November' = 'Nov'), 
                         selected = c('Jun', 'Jul', 'Aug', 'Sep', 
                                      'Oct', 'Nov')),
      
      # Input: Enter gear type
      selectInput(inputId = 'gear_type', 
                     label = 'Select Gear Type:', 
                     choices = c('Large Fyke' = 'F', 
                                 'Mini Fyke' = 'M', 
                                 'Day electroshocking' = 'D')),
      
      #Input: Select constituent
      selectInput(inputId = 'fish', 
                  label = 'Select Species:', 
                  choices = c('Bigmouth buffalo', 
                              'Black bullhead', 'Black crappie', 
                              'Bluegill', 'Bowfin', 'Brown bullhead', 
                              'Burbot', 'Central mudminnow', 'Channel catfish', 'Common carp', 
                              'Emerald shiner', 'Fathead minnow', 'Freshwater drum', 
                              'Gizzard shad', 'Golden redhorse', 'Golden shiner', 'Green sunfish', 
                              'Largemouth bass', 'Longnose gar', 'Mimic shiner', 'Mooneye', 
                              'Northern pike', 'Orangespotted sunfish', 'Pugnose minnow', 
                              'Pumpkinseed', 'Quillback', 'River carpsucker', 'River darter', 
                              'River shiner', 'Rock bass', 'Sand shiner', 'Sauger', 'Shorthead redhorse', 
                              'Shortnose gar', 'Shovelnose sturgeon', 'Silver chub', 'Silver redhorse', 
                              'Smallmouth bass', 'Smallmouth buffalo', 'Spotfin shiner', 
                              'Spottail shiner', 'Spotted sucker', 'Stonecat', 'Tadpole madtom', 
                              'Walleye', 'Warmouth', 'Weed shiner', 'Western sand darter', 
                              'White bass', 'White crappie', 'White sucker', 'Yellow bass', 
                              'Yellow bullhead', 'Yellow perch')),
      
      #Add input for habitat
      checkboxGroupInput(inputId = 'habitat', 
                    label = 'Select Stratum:', 
                  choices = c('Wing Dam' = 'MCB-W', 
                              'Main Channel Border - Unstructured' = 'MCB-U', 
                              'Backwater' = 'BWC-S', 
                              'Impounded - Shoreline' = 'IMP-S', 
                              'Impounded-Offshore' = 'IMP-O', 
                              'Side Channel' = 'SCB', 
                              'Tailwater'  = 'TWZ'), 
                  selected = c('MCB-W', 'MCB-U', 'BWC-S', 'IMP-S', 'IMP-O', 
                               'SCB', 'TWZ')),
      
      #checkbox to select if you want to show outliers or not
      checkboxInput(inputId = 'outliers', 
                    label = 'Show outliers:', 
                    value = TRUE),
      
      #Add the option to download the selected dataset
      downloadButton(outputId = 'downloadData', 
                     label = 'Download CSV')

    )
    ),
      
      # Output: Formatted text for caption ----
    #  h3(textOutput("caption")),
      
      nav_panel(title = "Time series plot",
                
                bslib::layout_column_wrap(
                  width = '600px', 
                  fill = FALSE,
                
        plotOutput('fishBoxes', height = 600, width = 1000)
      )
      ),
      
      # Output: Map of fish variable ----
      nav_panel(title = 'Sample point map',
                bslib::layout_column_wrap(
                
              width = '600px', 
              fill = FALSE,
        leafletOutput("fishMap", height = 800))
      )
    )
  
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste(input$fish)
  })
  
  filtered_data <- reactive({
      cpue %>%
        filter(Year >= input$date_range[1] & Year <= input$date_range[2]) %>%
      filter(gear == input$gear_type) %>%
      filter(Fishname == input$fish) %>%
      filter(!is.na(CPUE)) %>%
      filter(stratum %in% input$habitat) %>%
      filter(Month %in% input$months)
  
  })
  
  colorpal <- reactive({
    df <- filtered_data()
    
    colorNumeric('RdYlBu', domain = as.numeric(df$CPUE), reverse = T)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  #Generate a boxplot across habitat classes
 output$fishBoxes <- renderPlot({
    df <- filtered_data()
    
    validate(
      need(nrow(df) > 0, 'No data available to display. Please adjust stratum or gear filters.')
    )
    
      boxplot(
      as.numeric(df$CPUE) ~ as.numeric(df$Year), 
      xlab = 'Year',
      ylab = paste(input$fish, unique(df$lab)), 
      outline = input$outliers, 
      cex.lab = 1.5, 
      cex.axis = 1.5
    )

  })
  
  # Generate a plot of the requested variable in a pool 8 map
  output$fishMap <- renderLeaflet({
    df <- filtered_data()
    
    pal <- colorpal()
    
    chart <- df %>%
      leaflet() %>%
      addTiles() %>%
      setView(lng = -91.24, lat = 43.75, zoom = 12) %>%
      addCircleMarkers(data = df,
                       color = ~pal(df$CPUE), 
                       popup = paste(df$DATE, '\n','CPUE',' = ', as.character(round(df$CPUE, 2))),
                       fillOpacity = 0.8, 
                       lat = df$lat, 
                       lng = df$lng) %>%
      addLegend(
        position = 'bottomright', 
        pal = pal, 
        values = ~df$CPUE, 
        title = paste(input$fish, ' CPUE'), 
        opacity = 1
      )
    
    chart
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste('FISH_CPUEdata-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
}


shinyApp(ui, server)



