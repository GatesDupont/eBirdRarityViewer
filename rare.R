
# TODO:
" 1) SELECT species input - figure out how to get this to work how I want
  2) All species input - upload file listing all species in eBird to use as default
  3) Region NAME - figure out how to translate from region name input to region code"

### GLOBAL SPACE ### ---------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(jsonlite)

# Opening connection to pull functions from external file
source('./Functions.R')

# Pulling region code choices from external file
choices = as.character(read.csv("./data/choices.csv")$x)

# Fetching custom map tiles and adding citation
custom_map = "https://api.mapbox.com/styles/v1/heliornis/cjboo3dac64w02srud7535eec/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiaGVsaW9ybmlzIiwiYSI6ImNqNGtjZjE1cjBoYmcycXAzbmlmNmJieGEifQ.ERz0DjQKEE1PBd7myLKwZA"
mb_attribution <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a> © <a href='http://ebird.org/content/ebird/about/'>eBird / Cornell Lab of Ornithology</a> © <a href='https://www.gatesdupont.com/'>Gates Dupont</a>"

# Making my location icon
uloc = makeIcon(iconUrl = "./uloc.png", iconHeight = 25, iconWidth = 25)


### USER INTERFACE ### -------------------------------------------------------------------
ui <- bootstrapPage(
 
  # Adding dynamically updating USER LOC
  tags$script(geoloc()),
  
  # Add Google Analytics data
  tags$head(HTML(gtag())),
  
  # Setting THEME
  theme = shinytheme("superhero"),
  
  # Setting map to FULL-SCREEN
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  
  # Initializing LEAFLET output
  leafletOutput("myMap", width="100%", height="100%"),
  
  # Adding TITLE overlayed on leaflet map
  absolutePanel(top = 1, left = 50, draggable = F, 
                titlePanel("eBird Rarity Viewer"),
                helpText("by Gates Dupont")),
  
  # Adding SLIDER input overlayed on leaflet map
  absolutePanel(bottom = 1, left = 45, draggable = F, 
                sliderInput("slider_in", "Days Back", 
                            min = 1, max = 30, value = 3, round = T)),
  
  # Adding REGION INPUT overlayed on leaflet map
  absolutePanel(top = 1, right = 45, draggable = F,
                selectInput("region_in", "Region Code", choices = choices, 
                            selected = "US-MA", multiple = F, width  = 130)),
  
  # Adding SELECT SPECIES INPUT overlayed on leaflet map
  absolutePanel(bottom = 205, left = 45, width = 170, draggable = T,
                selectInput("species_in", "Species", choices = "", 
                            selected = "", multiple = F, width  = 170))
  
)


### SERVER ### ---------------------------------------------------------------------------
server <- function(input, output, session) {

  ## Rendering data frame from API with slider input -------------------------------------
  APIdata <- reactive({
    
    # Initial fetch of data from eBird API, with conditionals to reject errant input
    a <- try(api2(regionCode = as.character(input$region_in), 
                  back = as.numeric(input$slider_in)))
    if(class(a) == "try-error" ||length(a) == 0){return(NULL)}
    
    ### --- ### --- ### --- ### --- ### --- ### --- ### --- ###
    # Trying species input
    if(length(input$species_in) < 2){
      updateSelectInput(session, "species_in", 
                        choices = unique(a$comName),
                        selected = "")
    }else{
      updateSelectInput(session, "species_in", 
                      choices = "",
                      selected = "")
    }

    # Jittering lat/lon points to fix point overlap
    a$lat = jitter(a$lat, factor = 3) 
    
    # Changing review status from logical to numeric
    cols <- sapply(a, is.logical)
    a[,cols] <- lapply(a[,cols], as.numeric)
    
    # Initializing new date column
    a["date"] <- format(strptime(a$obsDt, format = "%Y-%m-%d"), "%b %d")
    
    # Initializing new color grouping column
    a["group"] <- NA
    
    # Assigning colors by review status
    idx<-  (a$obsReviewed == 0) # Not reviewed
    a$group[idx] <- "white"
    idx<- (a$obsReviewed == 1) & (a$obsValid == 1) # Reviewed and accepted
    a$group[idx] <- "green"
    
    # Adding url for list popups
    a["url"] <- NA
    a$url = sapply(a$subId, subIDurl)
    
    ### THIS IS DUPLICATE TO ABOVE< WORKING WEIRDLY ###
    # Species search filtering
    if(input$species_in %in% a$comName){
      #a = subset(a, a$comName == as.character(input$species_in))
      a = a[a$comName == as.character(input$species_in),]
      return(a)
    }else{return(a)}

    return(a)
  })

  ## -------------------------------------------------------------------------------------
  # Dynamically updating user location
  observe({
    if(!is.null(input$lat)){
      
      ulat <- input$lat
      ulng <- input$long
      acc <- input$accuracy
      time <- input$time
      
      proxy <- leafletProxy("myMap")
      
      proxy  %>% 
        clearGroup(group="pos") %>% 
        addMarkers(icon = uloc,lng=ulng, lat=ulat, label = "My Location", 
                   popup=paste("My location is:","<br>", 
                               ulng,"Longitude","<br>", ulat,"Latitude", 
                               "<br>", "My accuracy is:",  "<br>", acc, "meters"), 
                   group="pos") %>%
        addCircles(lng=ulng, lat=ulat, radius=acc, group="pos") %>%
        addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me",
                                 onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    }
  })
  
  ## -------------------------------------------------------------------------------------
  # Leaflet map
  output$myMap = renderLeaflet({
    if(is.null(APIdata()))
    {
      # Rendering leaflet map
      return(leaflet() %>% addTiles(urlTemplate = custom_map, attribution = mb_attribution)) %>%
        addSearchOSM(options = searchOSMOptions(zoom = 8)) %>%
        setView(-19.451108, 30.479968, 2)
    }
    else
    {
      # Splitting up by review status in order to show reviewed on top
      notReviewed = APIdata()[APIdata()$group == "white",]
      accepted = APIdata()[APIdata()$group == "green",]
      
      # Rendering leaflet map
      leaflet() %>% addTiles(urlTemplate = custom_map, attribution = mb_attribution) %>%
        addCircleMarkers(group = "Not reviewed", data = notReviewed, 
                         color = "#f5f5dc", opacity = 0.7, popup = notReviewed$url,
                         label = paste(notReviewed$comName,", ",notReviewed$date, ", ",
                                       notReviewed$locName,sep = "")) %>%
        addCircleMarkers(group = "Accepted", data = accepted, 
                         color = "#00FF33", opacity = 0.7, popup = accepted$url, 
                         label = paste(accepted$comName,", ",accepted$date, ", ", 
                                       accepted$locName, sep = "")) %>%
        addLegend(position = "bottomright", 
                  colors = c("#f5f5dc", "#00FF33"), 
                  labels = c("Not reviewed", "Accepted"),
                  title = "Legend: review status", opacity = 1) %>%
        addLayersControl(overlayGroups = c("Not reviewed", "Accepted"), position = "bottomright") %>%
        addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me",
                                 onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

