####### IMPORTANT #######

# 1. This scipt is just for quickly launching the app.  
# 2. The R-Markdown script has much more detailed steps on the process followed.
# 3. Please run the script first so that variables can be loaded and then the aplication.
# 4. Otherwise aplication will not run :)

#########################

# Steps:

# 1. Installing packages used for analysis:

# 1.1 General use

#install.packages("tidyverse")
#install.packages("remotes")
#install.packages("rgdal")
#install.packages("devtools")
#install.packages("RColorBrewer")
#install.packages("ggedit")
#install.packages("caTools")
#install.packages("plotly")
#install.packages("gridExtra")
#install.packages("egg")
#install.packages("cowplot")
#install.packages("ggrepel")

library(ggrepel)
library(cowplot)
library(tidyverse)
library(remotes)
library(rgdal)
library(devtools)
library(RColorBrewer)
library(ggedit)
library(caTools)
library(plotly)
library(gridExtra)
library(egg)

# 1.2 Mapping data and working with spatial objects

#install.packages("spdplyr")
#install.packages("sf")
#install.packages("countrycode")
#install.packages("ggmap")
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("mapdata")
#intall,packages("leaflet")
#install.packages("rgeos")
#install.packages("rmapshaper")

library(spdplyr)
library(sf)
library(countrycode)
library(ggmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(mapdata)
library(rgeos)
library(rmapshaper)

# 1.3 Creating animations

#install.packages("gganimate")
#install.packages("gifski")


library(gifski)
library(gganimate)

# 1.4 Creating Shiny dashboard


#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("DT")
#install.packages("shinyWidgets")
#install.packages("shinybusy")
#install.packages("dygraphs")
#install.packages("xts")


library(shinybusy)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(shiny)
library(dygraphs)
library(xts)

# Loading Covid 19 data up to the 15/04/2020 ((Most code written for this data set))

ncov_new <- read.csv("https://raw.githubusercontent.com/Goodstuff88/Assignment4UPDATED/master/time_series_19_covid_combined_10-05_2020.csv",
                     stringsAsFactors = FALSE)

# Remove instances with NA values

ncov_new <- na.omit(ncov_new)


# Changing the format of the "Date" variable

ncov_new$Date <- as.Date(ncov_new$Date,)

# Adding a variable "Infected" which is the number of accounts of people that are still undergoing treatment.

ncov_new <- ncov_new %>% 
  mutate(Infected = Confirmed - Recovered - Deaths)


# Renaming some of the variables to make it more intuitive

ncov_new <- ncov_new %>% 
  rename(Cum_Confirmed = Confirmed,
         Province_State = Province.State,
         Country_Region = Country.Region)


# Loading of the world data (spatial object) 

World <- ne_countries(scale = "medium", returnclass = "sf")


# Note: The "Country_Region" variable will be used to join the spatial data frame ("World") and the ncov_new data frame. Therfore, a geometry variable will be added to the ncov_new data frame.  

# Steps:

# Testing to see which country names do NOT coincide between the two data frames.

# Creating a data frame (World_df) that list all the countries of the natural earth data.The "pop_est" variable will be used later on for testing purpose.

World_df <- data.frame( Country_Region = World$name_long,
                        Pop_Estimate = World$pop_est)


# Joining the ncov_new data frame with the World_df date frame to investigate cases where "pop_est" is NA (i.e to investigate where there is a mis match in country names)

Country_Test_df <- left_join(ncov_new, World_df, by = "Country_Region" )

# Investigation of the countries that present NA values

Mismatch_Countries <- Country_Test_df %>%  
  filter(is.na(Pop_Estimate)) %>%
  group_by(Country_Region) %>%
  summarise(count = n())


ncov_new$Country_Region[ncov_new$Country_Region == "Brunei"] <- "Brunei Darussalam"

ncov_new$Country_Region[ncov_new$Country_Region == "Congo (Brazzaville)"] <- "Republic of Congo"

ncov_new$Country_Region[ncov_new$Country_Region == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"

ncov_new$Country_Region[ncov_new$Country_Region == "Cote d'Ivoire"] <- "Côte d'Ivoire"

ncov_new$Country_Region[ncov_new$Country_Region == "Diamond Princess"] <- "Japan"

ncov_new$Country_Region[ncov_new$Country_Region == "Czechia"] <- "Czech Republic"

ncov_new$Country_Region[ncov_new$Country_Region == "Eswatini"] <- "Swaziland"

ncov_new$Country_Region[ncov_new$Country_Region == "Gambia"] <- "The Gambia"

ncov_new$Country_Region[ncov_new$Country_Region == "Holy See"] <- "Vatican"

ncov_new$Country_Region[ncov_new$Country_Region == "Korea, South"] <- "Republic of Korea"

ncov_new$Country_Region[ncov_new$Country_Region == "Russia"] <- "Russian Federation"

ncov_new$Country_Region[ncov_new$Country_Region == "US"] <- "United States"

ncov_new$Country_Region[ncov_new$Country_Region == "Cabo Verde"] <- "Cape Verde"

ncov_new$Country_Region[ncov_new$Country_Region == "North Macedonia"] <- "Macedonia"

ncov_new$Country_Region[ncov_new$Country_Region == "Taiwan*"] <- "Taiwan" 

ncov_new$Country_Region[ncov_new$Country_Region == "Burma"] <- "Myanmar"

ncov_new$Country_Region[ncov_new$Country_Region == "Laos"] <- "Lao PDR" 

ncov_new$Country_Region[ncov_new$Country_Region == "MS Zaandam"] <- "Bahamas" 

ncov_new$Country_Region[ncov_new$Country_Region == "West Bank and Gaza"] <- "Palestine" 

ncov_new$Country_Region[ncov_new$Country_Region == "Sao Tome and Principe"] <- "São Tomé and Principe" 


# Creating Spatial data frame that only contains the important features from the world data set and storing in "World_Merge_df". This spatial data frame will be used in the merging operation in the next code chunk.

World_Merge_df <- World %>% 
  select(name_long,continent) %>%
  rename(Country_Region  = name_long)


# Creating a data frame that summarises the data for each country per day. 

ncov_new_FINAL_data <-  ncov_new %>% 
  group_by(Date, Country_Region) %>% 
  summarise(Cum_Confirmed_Country = sum(Cum_Confirmed),
            Recovered_Country = sum(Recovered),
            Deaths_Country = sum(Deaths),
            Infected_Country = sum(Infected)) %>% 
  as.data.frame()


# Merge data frame (ncov_new_FINAL_data) with Spatial data frame (World_Merde_df)

ncov_new_Spatial <- left_join(ncov_new_FINAL_data, World_Merge_df , by = "Country_Region")

# Classifying the geometry feature in data frame.The data frame will then have a class "data frame" and "sf" instead of just a "data frame"

st_geometry(ncov_new_Spatial) <- ncov_new_Spatial$geometry

# Saving static variables that are going to be used in the shiny application

# Creating customised icons for the shiny dashboard 

Death_icon <- awesomeIcons(
  icon = "times-circle",
  library = "fa",
  iconColor = "black",
  markerColor = "red"
)

Recovery_icon <- awesomeIcons(
  icon = "ambulance",
  library = "fa",
  iconColor = "black",
  markerColor = "green"
)

# Adding the continent variable to the existing ncov_new_Final_data data frame

# Retrieving continents data from World_Merge_df (used earlier)

World_Merge_No_Geom <- World_Merge_df %>% 
  st_set_geometry(value = NULL)

# Joining the two data frames

Continents_data <- left_join(ncov_new_FINAL_data, World_Merge_No_Geom, by = "Country_Region") %>%
  group_by(Date, continent) %>% 
  summarise(Cum_Confirmed_Country = sum(Cum_Confirmed_Country),
            Recovered_Country = sum(Recovered_Country),
            Deaths_Country = sum(Deaths_Country),
            Infected_Country = sum(Infected_Country)) %>% 
  rename(Country_Region = continent) %>% 
  as.data.frame()

# Adding the continents data to the original data frame

ncov_new_FINAL_data_Plot <- rbind(Continents_data, ncov_new_FINAL_data)



# Extracting country and continent names (for select input)


Country_Names <- levels(as.factor(ncov_new_FINAL_data$Country_Region))

Continent_Names <- levels(as.factor(Continents_data$Country_Region))

Country_Region_Names <- c(Continent_Names, Country_Names)

# Filtering to select non-zero values for each variable

Country_Zero_Less_df <- ncov_new_FINAL_data %>% 
  filter(Cum_Confirmed_Country != 0,
         Deaths_Country != 0,
         Recovered_Country != 0,
         Infected_Country != 0)

# Creating colour pallete for the world map


pal_Cum <- colorNumeric(palette = "YlOrRd", domain = c(0,log(Country_Zero_Less_df$Cum_Confirmed_Country)), na.color = "lightgoldenrodyellow" )


# Frequency data set of ncov cases

# Bind continents data with rest of data


Complete_Country_Region_df <- rbind(Continents_data, ncov_new_FINAL_data) 

# Create "day counter" (ref) for data set

ref <- data.frame("Date" = unique(Complete_Country_Region_df$Date), 
                  "Day" = c(1:109))

# Merge ref with ncov_new_FINAL_data

merge <- left_join(Complete_Country_Region_df, ref, by = "Date")

# Create new columns for individual frequency counts

ncov_freq_df <- add_column(merge,
                           "freq_Confirmed" = rep(0,nrow(merge)),
                           "freq_Deaths" = rep(0,nrow(merge)),
                           "freq_Recovered" = rep(0,nrow(merge)),
                           "freq_Infected" = rep(0,nrow(merge)),
)

Data_freq_Plot <- ncov_freq_df[order(ncov_freq_df$Country_Region),]

for(i in 1:nrow(Data_freq_Plot)){
  
  if(Data_freq_Plot$Day[i] == 1){
    Data_freq_Plot$freq_Confirmed[i] <- Data_freq_Plot$Cum_Confirmed_Country[i]
    Data_freq_Plot$freq_Deaths[i] <- Data_freq_Plot$Deaths_Country[i]
    Data_freq_Plot$freq_Recovered[i]<- Data_freq_Plot$Recovered_Country[i]
    Data_freq_Plot$freq_Infected[i] <- Data_freq_Plot$Infected_Country[i]
    
  }else{
    Data_freq_Plot$freq_Confirmed[i] <- Data_freq_Plot$Cum_Confirmed_Country[i]-Data_freq_Plot$Cum_Confirmed_Country[i-1]
    Data_freq_Plot$freq_Deaths[i] <- Data_freq_Plot$Deaths_Country[i] - Data_freq_Plot$Deaths_Country[i-1]
    Data_freq_Plot$freq_Recovered[i]<- Data_freq_Plot$Recovered_Country[i] - Data_freq_Plot$Recovered_Country[i-1]
    Data_freq_Plot$freq_Infected[i] <- Data_freq_Plot$Infected_Country[i] - Data_freq_Plot$Infected_Country[i-1]
  }
}

#  Selecting ony the freqeuncy columns

Data_freq_Plot <- Data_freq_Plot %>% 
  select(-Cum_Confirmed_Country, -Deaths_Country, -Recovered_Country, -Infected_Country)


# ACTUAL BEGINNING OF THE APP!

# Note: The shinydashboard package was used to construct the layout of the dashboard. 

# Steps:

# 1. Defining structure of the UI

# 1.1 Adding dashboard header 

header <- dashboardHeader(
  title = "Covid 19 - 09/05/2020",
  titleWidth = 250
)

# 1.2 Adding a sidebar to dashboard

sidebar <- dashboardSidebar(
  
  # Adding tabs to the sidebar in the dashboard
  
  collapsed = TRUE, # Setting the deafault of the sidebar to close
  
  # Adding tabs to the sidebar in the dashboard
  
  sidebarMenu( 
    menuItem("Word Data",
             tabName = "world_data", icon = icon("globe")),
    menuItem("Country/Region Data",
             tabName = "country_region_data", icon = icon("chart-bar"))
  )
)

# 1.3 Adding body to dashboard

body <- dashboardBody(
  
  # 1.3.1 Linking the tabs created in the sidebar to reflect in the body of the dashboard  
  
  tabItems(
    tabItem(tabName = "world_data", # Elements in included in the first tab
            
            # Value boxes will display the number of cases, deaths, recoveries and infected variables
            
            fluidRow(
              column( width = 3,
                      valueBoxOutput("G_Confirmed_Cases",
                                     width = NULL)
              ),
              
              column( width = 3,
                      valueBoxOutput("G_deaths",
                                     width = NULL)
              ),
              
              column(width = 3,
                     valueBoxOutput("G_Recoveries",
                                    width = NULL)
              ),
              
              column(width = 3,
                     valueBoxOutput("G_Infected",
                                    width = NULL))
            ),
            
            fluidRow(
              
              # Space for World Map output
              
              leafletOutput("World_Map", height = 550)
            ),
            
            fluidRow(
              
              panel(
                
                # Adding date-slider to dashboard
                column(width = 4,
                       setSliderColor("red", 1),
                       sliderInput("date","Select data extraction date:", # Creating slider date input
                                   min = as.Date(min(ncov_new_FINAL_data$Date),"%Y-%m-%d"),
                                   max = as.Date(max(ncov_new_FINAL_data$Date),"%Y-%m-%d"),
                                   value=as.Date(max(ncov_new_FINAL_data$Date)),
                                   timeFormat="%Y-%m-%d")
                ),
                
                # Adding radio buttons to select the top ten areas with the higest recoveries and deaths
                
                column(width = 4,
                       prettyRadioButtons(inputId = "topTen", # Creating radio button input
                                          label = "Additional information w.r.t chosen date:",
                                          choices = c("Normal world map", "Top ten regions with highest death count", "Top ten regions with the higest recovery count"),
                                          shape = "round", 
                                          status = "danger",
                                          fill = TRUE, 
                                          animation = "pulse")
                       
                ),
                
                # Adding radio buttons that filter the table output
                
                column(width = 4,
                       prettyRadioButtons(inputId = "table", # Creating radio button input
                                          label = "Select the variable for data table:",
                                          choices = c("None", "Cases", "Deaths", "Recoveries", "Infections"),
                                          shape = "round", 
                                          status = "danger",
                                          fill = TRUE, 
                                          animation = "pulse")
                )
                
              )
              
            ),
            
            # Making space for table output
            
            fluidRow(
              tableOutput("Data_frame")
            )
            
    ),
    
    # Creating new tab for the plotting of graphs
    
    tabItem(tabName = "country_region_data",
            
            column(width = 4,
                   panel( heading = "Data selection",
                          footer = p(strong("Note:"), "The graphs are fully interactive with a variety of options such as zooming, hovering of mouse to view and compare data values, panning etc."),
                          status = "danger",
                          
                          # Adding drop down list                    
                          
                          selectInput(inputId = "country_continent", # For country/region input
                                      "Select Country/Continent:",
                                      choices = Country_Region_Names ,
                                      selected = "South Africa",
                                      multiple = TRUE,
                                      width = NULL),
                          
                          # Adding radio buttons for variable selection
                          
                          prettyRadioButtons(inputId = "DataSelection", # For variable input
                                             label = "Select data interest:",
                                             icon = icon("check-circle"),
                                             choiceNames = c("Cumulative cases", "Deaths", "Recoveries", "Infections"),
                                             choiceValues = c("Cum_Confirmed_Country", "Deaths_Country", "Recovered_Country", "Infected_Country"),
                                             selected = "Cum_Confirmed_Country",
                                             animation = "tada",
                                             status = "danger",
                                             width = NULL),
                          
                          # Adding switch for log scale
                          
                          materialSwitch(inputId = "logScale", # To enable log-scale
                                         label = "Activate log scale:",
                                         status = "danger",
                                         width = NULL
                          ),
                          
                          # Adding switch for area graph
                          
                          materialSwitch(inputId = "area", # To enable area graph
                                         label = "Activate area graph:",
                                         status = "danger",
                                         width = NULL
                          )
                   )
            ),
            
            column(width = 8,
                   
                   # Adding plotly graphs
                   
                   fluidRow(
                     h3("Cumulative graph"),
                     plotlyOutput("countryPlot", height = "300px" ), # Interactive plot of country/region
                   ),
                   fluidRow(
                     h3("Frequency graph"),
                     plotlyOutput("FreqPlot", height = "300px") # Interactive plot of daily frequencies
                   )
            )
    )
  )
)


# 1.4 Saving all elements created to the UI

ui <- dashboardPage(header, sidebar, body, skin = "black")

# 2. Define elements in server

server <- function(input, output){
  
  # 2.1. Saving reactive variables (minimising duplication of code)
  
  # 2.1.1 Data set used for value box inputs
  
  ncov_reactive_data <-  reactive({
    ncov_new_FINAL_data %>% 
      filter(Date == input$date) %>% 
      as.data.frame()
  })
  
  # 2.1.2 New variable intorduced ncov_final_data_plot (includes continent data)
  
  data <- reactive({  
    ncov_new_FINAL_data_Plot %>%
      filter(Country_Region %in% input$country_continent)
  })
  
  # 2.1.3 Frequency data that will be used with the dygraph ()
  
  freq_Data <- reactive({
    Data_freq_Plot %>% 
      filter(Country_Region %in% input$country_continent)
    
  }) 
  
  
  # 2.2 Specifying data to use for value box output
  
  # 2.2.1 Value box 1 output 
  
  output$G_Confirmed_Cases <- renderValueBox({
    Confirmed <-  ncov_reactive_data() %>% 
      select(Cum_Confirmed_Country) %>% 
      sum()
    
    valueBox(value = Confirmed,
             subtitle = "Global confirmed cases",
             icon = icon("globe", lib = "glyphicon"), color = "red")
  })
  
  # 2.2.2 Value box 2 output
  
  output$G_deaths <- renderValueBox({
    Deaths <- ncov_reactive_data() %>%
      select(Deaths_Country) %>% 
      sum()
    
    valueBox(value = Deaths,
             subtitle = "Global deaths",
             icon = icon("remove-sign", lib = "glyphicon"), color = "red")
  })
  
  # 2.2.3 Value box 3 output
  
  output$G_Recoveries <- renderValueBox({
    
    Recoveries <- ncov_reactive_data() %>% 
      select(Recovered_Country) %>% 
      sum()
    
    valueBox(value = Recoveries,
             subtitle = "Global recoveries",
             icon = icon("heart", lib = "glyphicon"), color = "red")
  })
  
  # 2.2.4 Value box 4 output
  
  output$G_Infected <- renderValueBox({
    
    Infected <- ncov_reactive_data() %>% 
      select(Infected_Country) %>% 
      sum()
    
    valueBox(value = Infected,
             subtitle = "Global Infections",
             icon = icon("stethoscope", lib = "font-awesome"), color = "red")
  })
  
  # 2.3 Data for World map output
  
  output$World_Map <- renderLeaflet({ 
    
    
    L_World_Map <- ncov_new_Spatial %>%
      filter(Date == input$date) %>%
      leaflet(options = leafletOptions(minZoom = 2.5)) %>% 
      addProviderTiles(provider = "CartoDB.Positron", options = providerTileOptions( noWrap = TRUE) ) %>%
      addPolygons(weight = 1,
                  fillOpacity = 0.5,
                  color = ~pal_Cum(log(Cum_Confirmed_Country)) ,
                  popup = ~paste0("<b>", Country_Region, "</b>","<br/>",
                                  "Cases:", Cum_Confirmed_Country,"<br/>",
                                  "Deaths:", Deaths_Country, "<br/>",
                                  "Recoveries:", Recovered_Country,"<br/>",
                                  "Infections:", Infected_Country),
                  highlight = highlightOptions(weight = 3,
                                               color = "red",
                                               bringToFront = TRUE)) %>%
      setMaxBounds(lat1 = -90, lat2 = 90, lng1 = -180, lng2 = 180) %>%
      setView(lat = 8.0817, lng = 17.6078, zoom = 2.5)
    
    # Option that displays the normal world map    
    
    if(input$topTen == "Normal world map") {
      
      L_World_Map
      
    }
    
    # Option that displays the top 10 locations with the highest death count
    
    if(input$topTen == "Top ten regions with highest death count") {
      
      # Accessing relavant data
      
      Top10Deaths <-  ncov_new %>%
        filter(Date == input$date) %>%
        arrange(desc(Deaths)) %>%
        head(10) %>%
        mutate(Rank = seq(1,10, by = 1)) %>%
        as.data.frame()
      
      # Plotting of markers on world map
      
      L_World_Map <- L_World_Map %>%
        addAwesomeMarkers(lng = Top10Deaths$Long,
                          lat = Top10Deaths$Lat,
                          icon = Death_icon,
                          popup = ~paste0("<b>", "Rank:", Top10Deaths$Rank, "</b>", "<br/>", "<b>", "Deaths:", "</b>", Top10Deaths$Deaths))
      
    }
    
    # Option that displays the top 10 locations with the highest recovery count
    
    if(input$topTen == "Top ten regions with the higest recovery count") {
      
      # Accessing relavant data
      
      Top10Recoveries <- ncov_new %>%
        filter(Date == input$date) %>%
        arrange(desc(Recovered)) %>%
        head(10) %>%
        mutate(Rank = seq(1,10, by = 1)) %>%
        data.frame()
      
      # Plotting of markers on world map
      
      L_World_Map <- L_World_Map %>%
        addAwesomeMarkers(lng = Top10Recoveries$Long,
                          lat = Top10Recoveries$Lat,
                          icon = Recovery_icon,
                          popup = ~paste0("<b>", "Rank:", Top10Recoveries$Rank, "</b>", "<br/>", "<b>", "Recoveries:", "</b>", Top10Recoveries$Recovered))
      
    }
    
    L_World_Map
    
  })
  
  
  # 2.4 Data table output
  
  # Option that displays no data table
  
  output$Data_frame <-  renderTable({
    
    if(input$table == "None"){
      
      Table <- NULL
    }  
    
    # Option that displays top 20 countries with the highest cases   
    
    if(input$table == "Cases" ){
      
      Table <- ncov_new_FINAL_data %>%
        filter(Date == input$date) %>%
        arrange(desc(Cum_Confirmed_Country)) %>%
        head(20) %>%
        mutate(Rank = seq(1,20, by = 1)) %>%
        rename(Country = Country_Region,
               Cases = Cum_Confirmed_Country,
               Deaths = Deaths_Country,
               Recoveries = Recovered_Country,
               Infected = Infected_Country) %>% 
        as.data.frame()
      
      Table$Rank <- as.integer(Table$Rank)
      
      Table$Date <- as.character(Table$Date)
      
      Table <- Table[, c(7, 1, 2, 3, 4, 5, 6)]
      
    }
    
    # Option that displays top 20 countries with the highest deaths   
    
    if(input$table == "Deaths" ){
      
      Table <- ncov_new_FINAL_data %>%
        filter(Date == input$date) %>%
        arrange(desc(Deaths_Country)) %>%
        head(20) %>%
        mutate(Rank = seq(1,20, by = 1)) %>%
        rename(Country = Country_Region,
               Cases = Cum_Confirmed_Country,
               Deaths = Deaths_Country,
               Recoveries = Recovered_Country,
               Infected = Infected_Country) %>% 
        as.data.frame()
      
      Table$Rank <- as.integer(Table$Rank)
      
      Table$Date <- as.character(Table$Date)
      
      Table <- Table[, c(7, 1, 2, 3, 4, 5, 6)]
      
    }
    
    # Option that displays top 20 countries with the highest recoveries   
    
    if(input$table == "Recoveries" ){
      
      Table <- ncov_new_FINAL_data %>%
        filter(Date == input$date) %>%
        arrange(desc(Recovered_Country)) %>%
        head(20) %>%
        mutate(Rank = seq(1,20, by = 1)) %>%
        rename(Country = Country_Region,
               Cases = Cum_Confirmed_Country,
               Deaths = Deaths_Country,
               Recoveries = Recovered_Country,
               Infected = Infected_Country) %>% 
        as.data.frame()
      
      Table$Rank <- as.integer(Table$Rank)
      
      Table$Date <- as.character(Table$Date)
      
      Table <- Table[, c(7, 1, 2, 3, 4, 5, 6)] 
      
    }
    
    # Option that displays top 20 countries with the highesy infections    
    
    if(input$table == "Infections" ){
      
      Table <- ncov_new_FINAL_data %>%
        filter(Date == input$date) %>%
        arrange(desc(Infected_Country)) %>%
        head(20) %>%
        mutate(Rank = seq(1,20, by = 1)) %>%
        rename(Country = Country_Region,
               Cases = Cum_Confirmed_Country,
               Deaths = Deaths_Country,
               Recoveries = Recovered_Country,
               Infected = Infected_Country) %>% 
        as.data.frame()
      
      Table$Rank <- as.integer(Table$Rank)
      
      Table$Date <- as.character(Table$Date)
      
      Table <- Table[, c(7, 1, 2, 3, 4, 5, 6)]
      
    }
    
    Table
    
  })
  
  
  # 2.5 Plotly output - Cummalative country/region plot
  
  output$countryPlot <- renderPlotly({
    
    # Plotting Cummalative confirmed cases of country/region  
    
    if(input$DataSelection == "Cum_Confirmed_Country") {
      
      data <-  data() %>% 
        filter(Cum_Confirmed_Country != 0)
      
      y <- data() %>%
        select(Cum_Confirmed_Country)
      
      Plot <- ggplot(data, aes(x = Date, y = Cum_Confirmed_Country , fill = Country_Region)) +
        geom_col(aes(text = paste0('</br> Cumalative cases: ', Cum_Confirmed_Country,'</br> Date: ', Date, '</br> Country: ', Country_Region)), position = "dodge") +
        labs(y = "Cummalative cases", fill = "Region") +
        theme_bw()
    }
    
    # Plotting Deaths of country/region 
    
    if(input$DataSelection == "Deaths_Country") {
      
      data <-  data() %>% 
        filter(Deaths_Country != 0)
      
      y <- data() %>% 
        select(Deaths_Country)
      
      Plot <- ggplot(data, aes(x = Date, y = Deaths_Country, fill = Country_Region)) +
        geom_col(aes(text = paste0('</br> Deaths: ', Deaths_Country,'</br> Date: ', Date, '</br> Country: ', Country_Region)), position = "dodge") +
        labs(y = "Deaths", fill = "Region") +
        theme_bw()
    }
    
    # Plotting recoveries of country/region 
    
    if(input$DataSelection == "Recovered_Country") {
      
      data <-  data() %>% 
        filter(Recovered_Country != 0)
      
      y <- data() %>% 
        select(Recovered_Country)
      
      Plot <- ggplot(data, aes(x = Date, y = Recovered_Country, fill = Country_Region)) +
        geom_col(aes(text = paste0('</br> Recoveries: ', Recovered_Country,'</br> Date: ', Date, '</br> Country: ', Country_Region)), position = "dodge") +
        labs(y = "Recoveries", fill = "Region") +
        theme_bw()
      
    }
    
    # Plotting those still Infected of country/region 
    
    if(input$DataSelection == "Infected_Country") {
      
      data <-  data() %>% 
        filter(Infected_Country != 0)
      
      
      y <- data() %>%
        select(Infected_Country)
      
      Plot <- ggplot(data, aes(x = Date, y = Infected_Country, fill = Country_Region)) +
        geom_col(aes(text = paste0('</br> Infections: ', Infected_Country,'</br> Date: ', Date, '</br> Country: ', Country_Region)), position = "dodge") +
        labs(y = "Infections", fill = "Region") +
        theme_bw()
    }
    
    # Inserting optional log scale output   
    
    if(input$logScale){
      
      Plot <-  Plot + scale_y_log10() 
      
    }
    
    # Inserting optional area graph output  
    
    if(input$area){
      
      Plot <- Plot %>% 
        remove_geom(geom = "col")
      
      Plot <- Plot + geom_area(alpha = 0.6, 
                               position = "identity")
      
      #aes(fill = fct_reorder(Country_Region, y, .desc = TRUE))
      
    }
    
    # Actual plotting of plot_ly graph  
    
    if(input$area)  {
      
      ggplotly(Plot, tooltip = c("y","x")) %>% 
        layout(hovermode = "compare")
    }
    else{
      ggplotly(Plot, tooltip = "text") %>% 
        layout(hovermode = "compare")
    }
    
  })
  
  # 2.6 Plotly output - Frequecy plot of counntry/region
  
  output$FreqPlot <- renderPlotly({
    
    
    # Frequency plot of the total daily cases of the country/region selected
    
    if(input$DataSelection == "Cum_Confirmed_Country"){
      
      Data_Freq <- freq_Data()
      
      y <- freq_Data() %>%
        select(freq_Confirmed)
      
      Plot_2 <- ggplot(Data_Freq, aes(x = Date, y = freq_Confirmed, color = Country_Region, fill = Country_Region )) + 
        geom_point(aes(text = paste0('</br> Cases: ', freq_Confirmed,'</br> Date: ', Date, '</br> Country: ', Country_Region)), size = 1) + 
        geom_line()  +
        labs(y = "Cases (per day)", fill = "Region") +
        theme_bw() +
        theme(legend.position = "none") 
      
    }
    
    # Frequency plot of the daily deaths of the country/region
    
    if(input$DataSelection == "Deaths_Country"){
      
      Data_Freq <- freq_Data()
      
      y <- freq_Data() %>%
        select(freq_Deaths)
      
      Plot_2 <- ggplot(Data_Freq, aes(x = Date, y = freq_Deaths, color = Country_Region, fill = Country_Region )) + 
        geom_point(aes(text = paste0('</br> Deaths: ', freq_Deaths,'</br> Date: ', Date, '</br> Country: ', Country_Region)), size = 1) + 
        geom_line()  +
        labs(y = "Deaths (per day)", fill = "Region") +
        theme_bw() +
        theme(legend.position = "none") 
      
    }
    
    # Frequency plot of the daily recoveries of the country/region  
    
    if(input$DataSelection == "Recovered_Country"){
      
      Data_Freq <- freq_Data()
      
      y <- freq_Data() %>%
        select(freq_Recovered)
      
      Plot_2 <- ggplot(Data_Freq, aes(x = Date, y = freq_Recovered, color = Country_Region, fill = Country_Region )) + 
        geom_point(aes(text = paste0('</br> Recoveries: ', freq_Recovered,'</br> Date: ', Date, '</br> Country: ', Country_Region)), size = 1) + 
        geom_line()  +
        labs(y = "Recoveries (per day) ", fill = "Region") +
        theme_bw() +
        theme(legend.position = "none") 
      
    }
    
    # Frequency plot of the people infected (tested positive) of the country/region   
    
    if(input$DataSelection == "Infected_Country"){
      
      Data_Freq <- freq_Data()
      
      y <- freq_Data() %>%
        select(freq_Infected)
      
      Plot_2 <- ggplot(Data_Freq, aes(x = Date, y = freq_Infected, color = Country_Region, fill = Country_Region )) + 
        geom_point(aes(text = paste0('</br> Infections:', freq_Infected,'</br> Date: ', Date, '</br> Country: ', Country_Region)), size = 1) + 
        geom_line()  +
        labs(y = "Infections (per day) ") +
        theme_bw() +
        theme(legend.position = "none") 
      
    }
    
    # Inserting optional area graph output  
    
    if(input$area){
      
      Plot_2 <- Plot_2 + geom_area(alpha = 0.3, 
                                   position = "identity")
      
    }
    
    # Actual plotting of plot_ly graph  
    
    ggplotly(Plot_2, tooltip = "text") %>% 
      layout(hovermode = "compare")
    
  })
  
  
}

shinyApp(ui = ui, server = server )



