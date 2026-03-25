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

#Now do the same with summaries data
summary_url <- 'https://raw.githubusercontent.com/patrickDNR/Pool-8-Fish_CPUE-mapping/refs/heads/main/Data/summaries_allGear.csv'
download.file(summary_url, 'summaries_allGear.csv')

sum.all <- read.csv('summaries_allGear.csv') %>%
  dplyr::mutate(date = as.Date(format(mdy(sdate), '%Y-%m-%d'))) %>%
  dplyr::mutate(Month = month(mdy(sdate), label = T)) %>%
  select(code, date, Month, gear,zone15e, zone15n, temp, do, vegd, stratum, richness, shannon, year)

#Fix the location data just like with cpue
#Convert to sf object
sum.utm_sf <- st_as_sf(sum.all, coords = c('zone15e', 'zone15n'), crs = utm_crs)

#transform to geographic coordinates
sum.latlon_sf <- st_transform(sum.utm_sf, crs = 4326)

#make into data frame
sum.latlon_df <- as.data.frame(st_coordinates(sum.latlon_sf))
colnames(sum.latlon_df) <- c('lng', 'lat')

#combine with water quality data
sum.all <- cbind(sum.all, sum.latlon_df)


# Define UI for water quality map app ----
ui <-  bslib::page_navbar(
    
    #set up ID
    id = 'nav', 
    
    
    #make a title for the app
    title = 'LTRM Fish Data', 
    
    
    #Set background color
    bg = 'darkgreen', 
    
    #Set theme
    theme = bslib::bs_theme(version = 5), 
    
    #Window title 
    
    window_title = 'Fish CPUE', 
    
    #make it mobile friendly?
    
  fillable_mobile = TRUE, 
  fillable = TRUE,
  fluid = TRUE,
  
  #Now for the good stuff, start with the first page as Fish CPUE plot
#Page 1 -- CPUE time series plot
  nav_panel(
    title = 'CPUE time series plot', 
    
    layout_sidebar(
      
      position = 'left',
    sidebar = bslib::sidebar(
      width = 600, 
      open = TRUE,

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
    
    bslib::layout_column_wrap(
      width = '600px', 
      fill = FALSE,
      
      plotOutput('fishBoxes', height = 600, width = 1000)
    )
  ),

  #Page 2 -- sample point map for fish CPUE
    nav_panel(
      title = 'CPUE sample point map', 
      
      layout_sidebar(
      sidebar = bslib::sidebar( 
        width = 600, 
        position = 'left', 
        open = TRUE,
        
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
        downloadButton(outputId = 'CPUE_downloadData', 
                       label = 'Download CSV')
      )
      ),
      
        bslib::layout_column_wrap(
          
          width = '600px', 
          fill = FALSE,
          leafletOutput("fishMap", height = 800)),
    ),
    
    nav_panel(
      title = 'Diversity time series plot', 

      layout_sidebar(
      sidebar = bslib::sidebar( 
        width = 600,
        open = TRUE,
      sliderInput(inputId = 'sum_date_range', 
                  label = 'Select Date Range:', 
                  min = min(sum.all$year), 
                  max = max(sum.all$year),
                  step = 1,
                  value = c(min(sum.all$year), max(sum.all$year)), 
                  sep = '', 
                  ticks = F),
      
      #Select months of interest
      checkboxGroupInput(inputId = 'sum_months', 
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
      selectInput(inputId = 'sum_gear_type', 
                  label = 'Select Gear Type:', 
                  choices = c('Large Fyke' = 'F', 
                              'Mini Fyke' = 'M', 
                              'Day electroshocking' = 'D')),
      
      #Add input for habitat
      checkboxGroupInput(inputId = 'sum_habitat', 
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
      checkboxInput(inputId = 'sum_outliers', 
                    label = 'Show outliers:', 
                    value = TRUE),
      
      #Add the option to download the selected dataset
      downloadButton(outputId = 'sum_downloadData', 
                     label = 'Download CSV')
      
    )
    ),
    bslib::layout_column_wrap(
      width = '600px', 
      fill = TRUE,
      
      plotOutput('divBoxes', height = 300, width = 600)
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
  
  #filter summary data based on input parameters
  filtered_summary <- reactive({
    sum.all %>%
      filter(year >= input$sum_date_range[1] & year <= input$sum_date_range[2]) %>%
      filter(gear == input$sum_gear_type) %>%
      filter(!is.na(shannon)) %>%
      filter(stratum %in% input$sum_habitat) %>%
      filter(Month %in% input$sum_months)
    
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
  
  output$CPUE_downloadData <- downloadHandler(
    filename = function(){
      paste('FISH_CPUEdata-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  #Generate a boxplot across habitat classes
  output$divBoxes <- renderPlot({
    df <- filtered_summary()
    
    validate(
      need(nrow(df) > 0, 'No data available to display. Please adjust stratum or gear filters.')
    )
    
    boxplot(
      as.numeric(df$shannon) ~ as.numeric(df$year), 
      xlab = 'Year',
      ylab = 'Shannon index (H)', 
      outline = input$sum_outliers, 
      cex.lab = 1.5, 
      cex.axis = 1.5
    )
    
  })
  
  output$sum_downloadData <- downloadHandler(
    filename = function(){
      paste('FISH_Diversitydata-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(filtered_summary(), file, row.names = FALSE)
    }
  )
  
}


shinyApp(ui, server)



