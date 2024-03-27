library(shiny)
library(dplyr)
library(CDCPLACES)
library(shinylive)
library(leaflet)
library(readr)
library(sf)
library(data.table)
library(shinythemes)

# mi_census <- get_places(geography = "census", state = "MI", geometry = T) %>%
#   mutate(locationname = as.character(locationname),
#          data_value_lab = data_value / 100) %>%
#   select(category, locationname, measure, data_value, year, low_confidence_limit, high_confidence_limit, data_value_lab)

# mi_county <- get_places(geography = "county", state = "MI", geometry = T) %>%
#   mutate(locationname = as.character(locationname),
#          data_value_lab = data_value / 100) %>%
#   select(category, locationname, measure, data_value, year, low_confidence_limit, high_confidence_limit, data_value_lab)
# 
# sf::st_write(mi_census, "data/mi_census.shp")
# 
# sf::st_write(mi_county, "data/mi_county.shp")

# dict <- get_dictionary() |> select(measure_full_name, category_name)
# 
# write_csv(dict, "data/dict.csv")

dict <- read_csv("data/dict.csv") |> 
  as.data.table()



# mi_county <- sf::st_read("data/mi_county.shp")

measures_select <-  unique(dict$measure_full_name)

category_select <-  unique(dict$category_name)

mi_county <- sf::st_read("data/mi_county.shp")

#mi_census <- sf::st_read("data/mi_census.shp") #|> 
#  st_transform(mi_census, '+proj=longlat +datum=WGS84')

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("yeti"),
  
  # Application title
  titlePanel("MI PLACES Data Explorer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("category",
                  h3("Select a category:"),
                  choices = category_select),
      uiOutput("measures"),
      h3("About"),
      h4("This app was created to help explore data from the Centers for Disease Control and Prevention's 'PLACES' data set. This app shows PLACES data in Michigan Counties."),
      h4("This data was pulled from the", tags$a(href = "https://github.com/brendensm/CDCPLACES", "'CDCPLACES' R Package."), "Use the menus below to select a category and measure of interest."),
      # selectInput("measure",
      #             h3("Select a measure:"),
      
      #choices = measures_select),
      h5("Rate shown is crude and unadjusted for age. Confidence limits are shown in parentheses.")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map", height = "675", width = "800"),
      textOutput("datasource")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # filtered_measures <- reactive({
  #   filter(lansing, category == input$category) %>% unique()
  # })
  
  output$measures = renderUI({
    measures_select1 <- unique(dict[category_name == input$category])
    measures_select2 <- measures_select1$measure_full_name
    selectInput('measure', h3('Select a measure:'), measures_select2)
  })
  
  group_to_map <- reactive({
    # filter(mi_census, measure == input$measure)
    mi_county[mi_county$measure == input$measure,]
  })
  
  # Initialize the map object, centered on the Michigan counties
  output$map <- renderLeaflet({
    
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addTiles() %>%
      #  addProviderTiles("Stamen.TonerHybrid") %>%
      setView(lng = -84.752663,
              lat = 43.989807,
              zoom = 7)
    #, 
  })
  
  output$datasource <- renderText({
    release <- unique(group_to_map()$year)
    
    paste0("Data source: CDC PLACES release 2023. Behavioral Risk Factor Surveillance Survey (BRFSS), ", release, ".")
  })
  
  observeEvent(input$measure, {
    
    pal <- colorNumeric("viridis", group_to_map()$dt_vl_l)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = group_to_map(),
                  color = ~pal(dt_vl_l),
                  weight = 0.5,
                  fillOpacity = 0.5,
                  smoothFactor = 0.2,
                  label = paste0(group_to_map()$data_vl, ", (", group_to_map()$lw_cnf_, "-", group_to_map()$hgh_cn_, ")")
      ) %>%
      # leaflegend::addLegendNumeric(pal= pal,
      #                 shape = "rect",
      #                 orientation = "horizontal",
      #                 values = group_to_map()$data_value,
      #                 width = 250,
      #                 height = 25,
      #                 
      #                 title = paste0(unique(group_to_map()$measure), " (%)"),
      #                fillOpacity = 1)
      addLegend(
        position = "bottomright",
        pal = pal,
        bins = 9,
        values = group_to_map()$dt_vl_l,
        labFormat = scales::percent
        # title = paste0(unique(group_to_map()$measure), " (%)")
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)