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

#Now do the same with lengths data
length_url <- 'https://raw.githubusercontent.com/patrickDNR/Pool-8-Fish_CPUE-mapping/refs/heads/main/Data/Lengths_allgear.csv'
download.file(length_url, 'Lengths_allGear.csv')

length.all <- read.csv('Lengths_allGear.csv') %>%
  dplyr::mutate(date = as.Date(format(mdy(sdate), '%Y-%m-%d'))) %>%
  dplyr::mutate(Month = month(mdy(sdate), label = T)) %>%
  select(code,Fishname,sdate, X0., X25., X50., X75., X100., date, Month, gear,zone15e, zone15n, temp, do, vegd, stratum, year) %>%
  rename('0%' = 'X0.', '25%' = 'X25.', '50%' = 'X50.', 
         '75%' = 'X75.', '100%' = 'X100.')

#Fix the location data just like with cpue
#Convert to sf object
length.utm_sf <- st_as_sf(length.all, coords = c('zone15e', 'zone15n'), crs = utm_crs)

#transform to geographic coordinates
length.latlon_sf <- st_transform(length.utm_sf, crs = 4326)

#make into data frame
length.latlon_df <- as.data.frame(st_coordinates(length.latlon_sf))
colnames(length.latlon_df) <- c('lng', 'lat')

#combine with water quality data
length.all <- cbind(length.all, length.latlon_df)

#change the direction of the data...
length.all <- pivot_longer(length.all, 
                     cols = c('0%', '25%', '50%', '75%', '100%'), 
                     names_to = 'Metric', 
                     values_to = 'Measurement')

# Define UI for water quality map app ----
ui <- 
  
  bslib::page_navbar(
    
    #set up ID
    id = 'nav', 
    
    
    #make a title for the app
    title = 'LTRM Fish data', 
    
    
    #Set background color
    bg = 'darkgreen', 
    
    #Set theme
    theme = bslib::bs_theme(version = 5), 
    
    #Window title 
    
    window_title = 'Fish data', 
    
    #make it mobile friendly?
    
    fillable_mobile = TRUE, 
    
    #Now for the good stuff, make a sidebar first...
    
    nav_panel(
      title = "CPUE Time series",
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
          
          # Input: Maybe a slider for time series?
          sliderInput(inputId = 'CPUEts_date_range', 
                      label = 'Select Date Range:', 
                      min = min(cpue$Year), 
                      max = max(cpue$Year),
                      step = 1,
                      value = c(min(cpue$Year), max(cpue$Year)), 
                      sep = '', 
                      ticks = F),
          
          #Select months of interest
          checkboxGroupInput(inputId = 'CPUEts_months', 
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
          selectInput(inputId = 'CPUEts_gear_type', 
                      label = 'Select Gear Type:', 
                      choices = c('Large Fyke' = 'F', 
                                  'Mini Fyke' = 'M', 
                                  'Day electroshocking' = 'D')),
          
          #Input: Select constituent
          selectInput(inputId = 'CPUEts_fish', 
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
          checkboxGroupInput(inputId = 'CPUEts_habitat', 
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
          checkboxInput(inputId = 'CPUEts_outliers', 
                        label = 'Show outliers:', 
                        value = TRUE),
          
          #Add the option to download the selected dataset
          downloadButton(outputId = 'CPUE_downloadData', 
                         label = 'Download CSV')
          
        )
        ,
        mainPanel(
          plotOutput('fishBoxes', height = 600, width = 1000), 
          "Annual boxplots of fish CPUE for each catch using LTRM SRS sampling."
        )
      )
    ),
    
    # Output: Map of fish variable ----
    nav_panel(title = 'CPUE map',
              # Sidebar layout with input and output definitions ----
              sidebarLayout(
                # Sidebar panel for inputs ----
                sidebarPanel(
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
                  downloadButton(outputId = 'CPUE2_downloadData', 
                                 label = 'Download CSV')
                  
                )
                ,
                mainPanel(
                  leafletOutput("fishMap", height = 800)
                )
              )
    ), 
    
    #Page 3 -- plot diversity data...
    nav_panel(
      title = 'Diversity time series',
      
      sidebarLayout(
        sidebarPanel(
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
        ,
        mainPanel(
          plotOutput('divBoxes', , height = 600, width = 1000), 
          
          "Annual boxplots of Shannon diversity index measured for each catch from LTRM SRS sampling."
        )
      )
    ), 
    
    #Page 4....make a plot of fish lengths
    nav_panel(
      title = "Median fish lengths time series",
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
          
          # Input: Maybe a slider for time series?
          sliderInput(inputId = 'length_date_range', 
                      label = 'Select Date Range:', 
                      min = min(length.all$year), 
                      max = max(length.all$year),
                      step = 1,
                      value = c(min(length.all$year), max(length.all$year)), 
                      sep = '', 
                      ticks = F),
          
          #Select months of interest
          checkboxGroupInput(inputId = 'length_months', 
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
          selectInput(inputId = 'length_gear_type', 
                      label = 'Select Gear Type:', 
                      choices = c('Large Fyke' = 'F', 
                                  'Mini Fyke' = 'M', 
                                  'Day electroshocking' = 'D')),
          
          #Input: Select constituent
          selectInput(inputId = 'length_fish', 
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
          checkboxGroupInput(inputId = 'length_habitat', 
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
          checkboxInput(inputId = 'length_outliers', 
                        label = 'Show outliers:', 
                        value = TRUE),
          
          #Add the option to download the selected dataset
          downloadButton(outputId = 'length_downloadData', 
                         label = 'Download CSV')
          
        )
        ,
        mainPanel(
          plotOutput('lengthPlot', height = 600, width = 1000),
          
          "Annual averages of the median (50%), 25%, and 75% quantiles of measured length for fish collected from LTRM SRS sampling."
        )
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
  
  filtered_data_CPUEts <- reactive({
    cpue %>%
      filter(Year >= input$CPUEts_date_range[1] & Year <= input$CPUEts_date_range[2]) %>%
      filter(gear == input$CPUEts_gear_type) %>%
      filter(Fishname == input$CPUEts_fish) %>%
      filter(!is.na(CPUE)) %>%
      filter(stratum %in% input$CPUEts_habitat) %>%
      filter(Month %in% input$CPUEts_months)
    
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
  
  #filter length data based on input parameters
  filtered_length <- reactive({
    length.all %>%
      filter(year >= input$length_date_range[1] & year <= input$length_date_range[2]) %>%
      filter(gear == input$length_gear_type) %>%
      filter(Fishname == input$length_fish) %>%
      filter(stratum %in% input$length_habitat) %>%
      filter(Month %in% input$length_months) %>%
      group_by(year, Metric) %>%
      summarise(mean.measure = mean(Measurement, na.rm = T))
    
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
    df <- filtered_data_CPUEts()
    
    validate(
      need(nrow(df) > 0, 'No data available to display. Please adjust stratum or gear filters.')
    )
    
    boxplot(
      as.numeric(df$CPUE) ~ as.numeric(df$Year), 
      xlab = 'Year',
      ylab = paste(input$fish, unique(df$lab)), 
      outline = input$CPUEts_outliers, 
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
  
  output$CPUE_downloadData <- downloadHandler(
    filename = function(){
      paste('FISH_CPUEdata-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$CPUE2_downloadData <- downloadHandler(
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
      need(nrow(df) > 0,'No data available to display. Please adjust stratum or gear filters.')
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
  
  output$lengthPlot <- renderPlot({
    df.length <- filtered_length()
    
    validate(
      need(nrow(df.length) > 0,'No data available to display. Please adjust stratum or gear filters.')
    )
    
    ggplot(data = df.length[df.length$Metric == '25%' |
                              df.length$Metric == '50%' |
                              df.length$Metric == '75%',], aes(x = year, y = mean.measure, color = Metric)) + 
      geom_point(size = 6) + 
      scale_color_manual('Quantile', values = c('25%' = 'darkblue', '50%' = 'black', '75%' = 'red')) +
      theme_bw() + 
      theme(text = element_text(size = 25))+
      labs(x = 'Year', 
           y = 'mean Length (mm)')
    
  })
  
  output$length_downloadData <- downloadHandler(
    filename = function(){
      paste('FISH_Lengthdata-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(filtered_length(), file, row.names = FALSE)
    }
  )
  
}


shinyApp(ui, server)