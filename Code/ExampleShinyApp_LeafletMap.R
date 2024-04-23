# 
# title: Pohatu Penguins Adoptions Dashboard
# author: Dr. Rachel Hickcox
# date: 2023
# email: rphickcox@gmail.com
# ---

# Packages
library("data.table") 
library("reshape2") 
library("knitr") 
library("here") 
library("leaflegend") 
library("leaflet") 
library("leaflet.esri")
library("fresh")
library("shinycssloaders") 
library("shiny")
library("shinythemes")
library("googlesheets4")
library("googledrive")
library("readxl")
library("graphics")
library("lubridate")
library("tidyverse")
library("dplyr")
library("DT")
library("sf")
library("htmlwidgets")
library("rvest")
library("shinyWidgets")

############################## Data processing #################################
# Connecting to Google Drive
#drive_deauth()
# google_client <- gargle::gargle_oauth_client_from_json(
#   path = here(".secrets", "googleauth.json"),
#   name = "googleauth"
# )
# drive_auth(google_client)


# Connecting to Google Drive
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets", 
  gargle_oob_default = TRUE, 
  gargle_oauth_client_type = "web", 
  scopes = "drive.readonly",
  warn = 0,
  dplyr.summarise.inform = FALSE,
  tidyverse.quiet = TRUE)

drive_auth(email = TRUE)

drive_download(file = "https://docs.google.com/spreadsheets/d/1mEcD3r8sAauUH_rE5dDMkvpPlYNVqrEXawA0CYAg34E", overwrite = TRUE, 
               path = "data/monitoring.xlsx")
monitor <- read_excel("data/monitoring.xlsx", 
                      col_types = c("date", rep("text", 5), "numeric", rep("text", 8),
                                    "numeric", "numeric",	
                                    "text",	"text", rep("numeric", 2), 
                                    "text",	"text",  rep("numeric", 2), 	
                                    "text", "text", rep("numeric", 2),	
                                    "text", "text", rep("numeric", 1), 
                                    rep("text", 6)))  

drive_download(file = "https://docs.google.com/spreadsheets/d/1qh6zAXgVokfdx4WjjSls8qh4tAfEOL-bO3xg3CvRY44", overwrite = TRUE,
               path = "data/nestid.xlsx")
nestid <- read_excel("data/nestid.xlsx")

drive_download(file = "https://docs.google.com/spreadsheets/d/16c4c5nXbz_PFf7iyN2DG7fExsi4P5uP-NSYAbCDX1rc", overwrite = TRUE, 
               path = "data/birdid.xlsx")
birdid <- read_excel("data/birdid.xlsx")
drive_download(file = "https://docs.google.com/spreadsheets/d/1d9Yt3PXLqYgpCHaKaQX2PHvrCXxrq8AEZ8sDiUDoBk8", overwrite = TRUE, 
               path = "data/adoptions.xlsx")
adoptions <- read_excel("data/adoptions.xlsx", sheet = "Nest Boxes")
nbhood <- read_excel("data/adoptions.xlsx", sheet = "Neighbourhoods")

# Last monitoring status
nests <- monitor %>%
  group_by(`Nest ID`) %>%
  arrange(`Nest ID`, DateTime) %>%
  mutate(n = row_number()) %>%
  dplyr::filter(DateTime == max(DateTime, na.rm = T)) %>%
  dplyr::select(DateTime, `Nest ID`, `Nest activity`, `Number of adults`, `Adult 1`, `Adult 2`, `Image.http`) %>%
  mutate(Monitored = format(DateTime, "%d-%m-%Y"), .before = 1) %>% 
  mutate(Status = ifelse(`Nest activity` == "Eggs/Chicks present", "Breeding", `Nest activity`), 
         #Status = ifelse(Status == "Loafing adult(s)", "Non-breeding", Status), 
         Status = ifelse(Status == "FAILED)", "Breeding", Status), 
         Status = ifelse(Status == "Not visible", "Unknown", Status), .after = `Nest activity`) %>% 
  mutate(across(c(`Adult 1`, `Adult 2`), ~ifelse(!is.na(.), "Marked", "Not marked"))) %>%
  ungroup()

# Active nestboxes
activenests <- nestid %>% 
  separate(col = Location, into = c("Lat", "Lon"), sep = ",", remove = FALSE) %>% 
  mutate(Lat = ifelse(str_detect(`Nest ID`, "^DUD"), NA, Lat), 
         Lon = ifelse(str_detect(`Nest ID`, "^DUD"), NA, Lon), 
         Lat = as.numeric(Lat), 
         Lon = as.numeric(Lon)) %>% 
  mutate(Neighborhood = str_replace_all(Neighborhood, "Dagobah System", "The Dagobah System"), 
         Neighborhood = str_replace_all(Neighborhood, "Unnamed Land", "The Unnamed Land")) %>%  
  dplyr::filter(!str_detect(`Nest ID`, "^DUD"), 
                !is.na(Lat)) %>% 
  arrange(Neighborhood, `Nest ID`) %>%
  select(Date, Neighborhood, `Nest ID`, Lat, Lon, Image.http) %>%
  left_join(nests, by = "Nest ID") %>%
  mutate(Status = ifelse(Status == "Empty", "Not currently breeding", Status), 
         Image.http.x = str_extract(Image.http.x, ".*\\s|.*"))
names(activenests) <- make.names(names(activenests))

# Active nests/neighbourhood
activenests_sum <- activenests %>% 
  group_by(Neighborhood) %>% 
  summarise(n = n()) %>%
  filter(!is.na(Neighborhood))

# Adoptions
names(adoptions) <- make.names(names(adoptions))

adoptions_all <- adoptions %>%
  filter(rowSums(is.na(.)) != length(.)) %>%
  select(Nest.ID, Neighborhood, Box.name:Active) %>%
  group_by(Nest.ID) %>%
  arrange(First.adoption.date, Renewal.date, Due.date)

adoptions_current <- adoptions_all %>% 
  filter(Active == "Active") %>% 
  select(Nest.ID, Box.name, Active) %>%
  mutate(n = row_number(), 
         n = paste0("Name ", n))

adoptions_past <- adoptions_all %>% 
  filter(Active == "Inactive" | is.na(Active)) %>%
  group_by(Nest.ID) %>%
  mutate(Box.name = replace_na(Box.name, ""),
         Box.name.past = paste(Box.name, collapse = ", "), 
         Box.name.past = str_replace_all(Box.name.past, pattern = "^, ", "")) %>% 
  select(Nest.ID, Box.name.past)

adopt_sum <- adoptions_current %>%
  #full_join(adoptions_past, by = "Nest.ID") %>% 
  group_by(Nest.ID) %>% 
  mutate(Active = replace_na(Active, "Inactive"), 
         n = row_number()) %>%
  arrange(Nest.ID) %>%
  pivot_wider(names_from = n, values_from = Box.name) %>% 
  mutate(`1` = ifelse(is.na(`1`), `2`, `1`), 
         #`2` = ifelse(`1` == `2`, NA, `2`), 
         `1`= replace_na(`1`, "AVAILABLE FOR ADOPTION"), 
         `2`= replace_na(`2`, "AVAILABLE FOR ADOPTION"), 
         Active = ifelse(Active == "Active" & (`1` == "AVAILABLE FOR ADOPTION" | `2` == "AVAILABLE FOR ADOPTION" ), 'Active- 1 name', Active), 
         Active = ifelse(Active == 'Active', 'Active- 2 names', Active))

activenests2 <- activenests %>% 
  left_join(adopt_sum, by = 'Nest.ID') %>%
  mutate(#Box.name.past = na_if(Box.name.past, ""), 
    Active = ifelse(is.na(Active), "Inactive", Active))

adopt_active <- activenests2[activenests2$Active != "Inactive",]
adopt_active_1 <- adopt_active[adopt_active$Active == "Active- 1 name",]
adopt_active <- adopt_active[adopt_active$Active != "Active- 1 name",]
adopt_inactive <- activenests2[activenests2$Active == "Inactive",]

nest_list <- sort(unique(c(adopt_sum$`1`, adopt_sum$`2`)))

shp_nests <- st_as_sf(activenests2, coords = c("Lon", "Lat"), crs = 2193)
shp_nests_wgs <- st_transform(shp_nests, crs = 4326)

# Neighbourhoods
names(nbhood) <- make.names(names(nbhood))

nbhood_all <- nbhood %>%
  filter(rowSums(is.na(.)) != length(.)) %>%
  select(Neighborhood:Business.name) %>%
  group_by(Neighborhood) %>%
  arrange(Neighborhood, Renewal.date, Due.date)

nbhood_names <- nbhood_all$Neighborhood

nbhood_current <- nbhood_all %>% 
  ungroup() %>% 
  filter(Active != "Inactive" & !is.na(Active)) %>% 
  mutate(Sponsor.date = ifelse(!is.na(Renewal.date), format(Renewal.date, "%d-%m-%Y"), format(First.adoption.date, "%d-%m-%Y"))) %>%
  select(Neighborhood, Business.name, Status, Active, Sponsor.date) %>%
  group_by(Neighborhood) %>% 
  mutate(n = row_number(), 
         n = paste0("Name ", n)) %>%
  distinct()

nbhood_not <- data.frame("Neighborhood" = nbhood_names[!nbhood_names %in% nbhood_current$Neighborhood]) %>% 
  mutate(Business.name = "AVAILABLE FOR ADOPTION", 
         Status = "Not adopted", 
         Active = "Inactive", 
         Sponsor.date = format(as.POSIXct("1900-01-01"), "%d-%m-%Y"), 
         n = "Name 1")

nbhood_sum <- nbhood_current %>%
  bind_rows(nbhood_not) %>%
  arrange(Neighborhood) %>% 
  left_join(activenests_sum, by = "Neighborhood") 

nbhoods_wgs <- st_read(here("data/all_neighbourhoods_wgs.shp"))
nbhoods_wgs <- st_zm(nbhoods_wgs, drop = T, what = "ZM") #drop extra dimensions
nbhoods_wgs <- left_join(nbhoods_wgs, activenests_sum, by = c("layer" = "Neighborhood")) 
nbhoods_wgs <- left_join(nbhoods_wgs, nbhood_sum, by = c("layer" = "Neighborhood")) %>%
  filter(!is.na(layer))
nbhoods_df <- st_coordinates(nbhoods_wgs)

############################## UI ##############################################
ui <- bootstrapPage(
  theme = shinytheme("flatly"),  # Use the custom CSS class for the navbar
  tags$link(
    rel = "stylesheet",
    href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"
  ), 
  tags$style(HTML( '
    @media (max-width: 767px) {
      /* Hide social links on smaller screens */
      .social-links {
        display: none;
      }
    }
    
    .leaflet-left .leaflet-control {
      visibility: hidden;
    }
    
    .navbar-default {
      margin: 0em;
      width: auto;
      height: auto;
    }
    
    .navbar-nav > li > a,
    .navbar-brand {
      height: auto;
      right: 2em;
      left: -1em;
    }
    
    ul {
      color: #fff;
    }
    
    .navbar .nav li.active a {
      background-color: #09213d; /* Change this to your desired color */
        color: #fff; /* Change this to the text color you prefer */
    }
    
    .navbar .nav > li > a:hover {
      background-color: #09213d; /* Change this to your desired color */
        color: #fff; /* Change this to the text color you prefer */
    }
    
    .panel.panel-default .form-group.shiny-input-container {
      padding: 0.25em; 
      top: 1em;
    }
    
    .selectize-input {
      line-height: 1em;
      min-height: 0;
    } 
    
    .selectize-dropdown {
      line-height: 1em;
    }
   '
                   
  )),
  tags$nav(
    class = "navbar navbar-default",
    tags$ul(
      class = "nav navbar-nav navbar-right social-links",
      tags$li(
        tags$a(
          href = "https://www.facebook.com/PohatuPenguinsplunge",
          class = "fab fa-facebook-square fa-lg",
          " Facebook"
        )
      ),
      tags$li(
        tags$a(
          href = "https://www.youtube.com/PohatupenguinsPlungeNZLtdAkaroa",
          class = "fab fa-youtube fa-lg",
          " YouTube"
        )
      ),
      tags$li(
        tags$a(
          href = "https://www.instagram.com/pohatu_penguins",
          class = "fab fa-instagram fa-lg",
          " Instagram"
        ))),
    # Custom navbar header with social media links
    tags$ul(tags$img(src = 'korora-color.png', height = '60em'),
            tags$a(href = "https://www.pohatu.co.nz/Our+blog/Penguin+Adoption.html",
                   HTML('<span style="font-size: 2em; display: inline; color: white">P\u14dhatu Adopt-A-Penguin </span>'))),
    tabsetPanel(
      tabPanel("Nest Box Adoptions", 
               sidebarPanel(tags$style(type = "text/css", "#map {height: calc(75vh - 1em) !important;}"),
                            #tags$style(".leaflet-popup-content { font-size: 16px; }"),
                            id = "controls", 
                            class = "panel panel-default",
                            width = 3,
                            #fixed = TRUE,
                            #top = "10%", height = "13em", width = "10em", left = "1em", 
                            #draggable = TRUE,
                            # Input fields
                            selectInput("nest_box_number", 
                                        label = "Nest box number", 
                                        choices = c("All", sort(activenests2$Nest.ID)), 
                                        selected = "All", multiple = FALSE, 
                                        # options = list(placeholder = 'Select a nest box number', 
                                        #                searchField = c(nestid$`Nest ID`))
                            ),
                            #textInput("nest_box_name", "Nest box name", placeholder = "Enter box name"),
                            selectInput("neighbourhood", 
                                        label = "Neighbourhood", 
                                        choices = c("All", nbhood_names),  
                                        selected = "All", multiple = FALSE, 
                                        #options = list(placeholder = 'Select a neighbourhood', 
                                        #               searchField = c(nbhood_names))
                            )),
               mainPanel(leafletOutput("map")),
               absolutePanel(id = "contacts", class = "card", 
                             bottom = "1em", left = "50%", width = "auto", 
                             fixed = TRUE, draggable = FALSE, height = "auto",
                             a(actionButton(inputId = "email", label = "Contact Helps P\u14dhatu Conservation Trust!",
                                            style='padding: 0.5em; background-color: #2c3e50;', 
                                            icon = icon("envelope", lib = "font-awesome")),
                               href = "mailto:conservation@pohatu.co.nz")),
               absolutePanel(id = "cc", class = "card", 
                             bottom = "1em", left = "0.5em", width = "auto", 
                             fixed = TRUE, draggable = FALSE, height = "auto",
                             HTML(paste0(
                               "<script>",
                               "var today = new Date();",
                               "var yyyy = today.getFullYear();",
                               "</script>",
                               "<p style = 'text-align: center;'><small>&copy; - <a href = 'https://rphickcox.com' target = '_blank'>Dr. Rachel P. Hickcox</a> - <script>document.write(yyyy);</script></small></p>")
                             ))),
      tabPanel("About", 
               fluidRow(
                 column(width = 8,
                        mainPanel(id = "About",
                                  style = "color: #ffffff;",
                                  HTML('<h3><center>Ngā mihi nui! Thank you for your generous contribution to the <br><a href="https://www.pohatu.co.nz/Trust.html">Helps Pōhatu Conservation Trust</a>.</h3>
        <ul>
        <p>By adopting a penguin, your donation supports a number of conservation projects, including:</p>
        <ul>
          <li><p>Monitoring and research of the Pōhatu kororā/little penguin colony</p>
            <ul>
              <li><p><a href="https://www.pohatu.co.nz/what-we-do/monitoring-of-a-subset-of-the-colony">Marking penguins</a> with individual ID microchips/transponders</p></li>
              <li><p><a href="https://www.pohatu.co.nz/what-we-do/tracking-at-sea">GPS tracking at sea</a></p></li>
              <li><p><a href="https://www.pohatu.co.nz/what-we-do/supporting-research">Foraging, diet, and breeding biology studies</a></p></li>
              <li><p><a href="https://www.pohatu.co.nz/what-we-do/yearly-survey">Breeding pair surveys</a></p></li>
              </ul></li>
            <li><p><a href="https://www.pohatu.co.nz/what-we-do/providing-habitat">Building and maintaining nest boxes</a> for penguins and other native animals</p></li>
          <li><p><a href="https://www.pohatu.co.nz/what-we-do/penguin-rehabilitation">Penguin rehabilitation</a></p>
            <ul>
              <li><p>Fish</p></li>
              <li><p>Transport costs to the vet</p></li>
              <li><p>Rehabilitation facility maintenance.</p></li>
            </ul></li>
          <li><p><a href="https://www.pohatu.co.nz/what-we-do/trapping">Predator control</a></p></li>
          <li><p>Reforestation of Pōhatu’s covenants (protected areas)</p></li>
          <li><p>Colony maintenance, track building</p></li>
          <li><p><a href="https://www.pohatu.co.nz/what-we-do/education-and-awareness">Outreach</a> (education programmes, conference presentations, etc.)</p></li>
        </ul>
        </ul>
        <hr />
        <h4><center>Thank you so much to our <a href="https://www.pohatu.co.nz/what-we-do/our-funders">donors, funders, sponsors, and partners</a>, without whom this work would not be possible.</h4>
        <hr />'))), 
                 column(width = 4,
                        br(),
                        shinydashboard::box(background = "aqua", solidHeader = TRUE, width = 10,
                                            align = "center", status = "info",
                                            title = h3(strong("Interested in adopting a penguin?"), style = "color: aqua;",  align = "center"), 
                                            p("We have multiple package options for you to choose from to symbolically name a penguin in a nestbox for one year.", style = "align: center; color: white;"),
                                            actionButton("btn_link", "Learn more!", icon = icon("link"), style = "background-color: aqua; color: black",
                                                         onclick = "window.open('https://www.pohatu.co.nz/Trust/adopt-a-penguin', '_blank')"),
                                            h3(""),
                                            img(src = "nb.png", width = "75%", height = "75%", style = "align: center;"), 
                                            img(src = "penguins.png", width = "75%", height = "75%", style = "align: center;")))
               ), 
               fluidRow(h3("")),
               fluidRow(
                 absolutePanel(id = "cc", class = "card", 
                               bottom = "1em", left = "100%", width = "auto", 
                               fixed = TRUE, draggable = FALSE, height = "auto",
                               HTML(paste0(
                                 "<script>",
                                 "var today = new Date();",
                                 "var yyyy = today.getFullYear();",
                                 "</script>",
                                 "<p style = 'text-align: center;'><small>&copy; - <a href = 'https://rphickcox.com' target = '_blank'>Dr. Rachel P. Hickcox</a> - <script>document.write(yyyy);</script></small></p>")
                               )))
      )
    ))
)


############################## Server ########################################
# Define server logic required
server <- function(input, output, session){
  # Add server logic for the popup panel (right panel)
  # You can use observeEvent() to handle clicks on the map and update the popup panel accordingly.
  # Depending on how you want to filter the information, you might need to use filtered data and/or
  # reactive functions to update the popup content.
  
  colours2 <- colorFactor(
    #palette = topo.colors(20), 
    palette = rainbow(20), 
    domain = nbhoods_wgs$layer)
  
  # FUNCTION label nests
  popup_nests <- function(df){
    paste0("<table style='width: 100%;'><tr>",
           "<td style='width: 60%;'>",
           "<h3 style='color: red'> Nest: <b>", df$Nest.ID, "</b></h3>",
           "<b> Neighbourhood: </b>", df$Neighborhood, "<br>", "<br>",
           "<b> Adoption status: </b>", df$Active, "<br>",
           "<b> Adopted name 1: </b>", df$`1`, "<br>",
           "<b> Adopted name 2: </b>", df$`2`, "<br>","<br>",
           #"<b> TEST: </b>", df$Image.http.x, "<br>", "<br>",
           # "<b> Adopted name previous: </b>", df$Box.name.past, "<br>", "<br>",
           "<b> Current nest status: </b>", df$Status, "<br>",
           "<b> Current breeding pair: </b>",
           paste0("Adult 1 ", tolower(df$Adult.1), "; Adult 2 ", 
                  tolower(df$Adult.2)), "<br>",
           "</td>",
           "<td style='width: 40%; text-align: right;'>",
           # paste0("<img src=\"",df$Image.http.x, "\">"),
           #"<img src=",df$Image.http.x, ", width='50'>",
           "<a href=\"",df$Image.http.x, "\">", "Click here to view photo", "</a>",
           "</td>",
           "</tr></table>"
    )
  }
  
  ### Static map
  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(173.0035,-43.871,173.01,-43.86694) %>% 
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>% 
      #Neighbourhoods
      addPolygons(data = nbhoods_wgs, 
                  group = "Neighbourhoods",
                  fillColor = "transparent",
                  color = ~colours2(nbhoods_wgs$layer),
                  opacity = 1,
                  fillOpacity = 0,
                  weight = 1, 
                  highlightOptions = highlightOptions(color = "white", weight = 3, bringToFront = FALSE), 
                  popup = ~paste("<h3 style='color: red'>",layer,"</h3>",
                                 "<b> Sponsor:</b>", Business.name, "<br>",
                                 "<b> Status:</b>", Status, "<br>",
                                 "<b>Number of nests:</b>",n.y,"<br>","<b>", sep=" "), 
                  label = ~nbhoods_wgs$layer) %>% 
      # Nestboxes
      # Active/onhold/na for both nests = not available for adoption
      addCircleMarkers(data = adopt_active, 
                       lat = ~Lat, lng = ~Lon, 
                       group = "Adopted nests",
                       radius = 1.5, 
                       label = ~adopt_active$Nest.ID,
                       popup =  popup_nests(adopt_active), 
                       fillColor = ~colours2(adopt_active$Neighborhood),
                       fillOpacity = 1, 
                       color ="transparent", stroke = TRUE, weight = 6, 
                       options = popupOptions(closeButton = TRUE, keepInView = TRUE, autoPan = FALSE, maxHeight = "auto")) %>% 
      # Active for 1 nest
      addCircleMarkers(data = adopt_active_1, 
                       lat = ~Lat, lng = ~Lon, 
                       group = "Adopted nests (one name available)",
                       radius = 1.5, 
                       label = ~adopt_active_1$Nest.ID,
                       popup =  popup_nests(adopt_active_1), 
                       fillColor = ~colours2(adopt_active_1$Neighborhood),
                       fillOpacity = 1, 
                       color ="transparent", stroke = TRUE, weight = 6, 
                       options = popupOptions(closeButton = TRUE, keepInView = TRUE, autoPan = FALSE, maxHeight = "auto")) %>% 
      # Inactive = available for adoption
      addCircleMarkers(data = adopt_inactive, 
                       lat = ~Lat, lng = ~Lon, 
                       group = "Nests available for adoption",
                       radius = 1.5, 
                       label = ~adopt_inactive$Nest.ID,
                       popup =  popup_nests(adopt_inactive), 
                       fillColor = ~colours2(adopt_inactive$Neighborhood),
                       fillOpacity = 1, 
                       color ="transparent", stroke = TRUE, weight = 6) %>% 
      addLayersControl(overlayGroups = c("Neighbourhoods", "Adopted nests", "Adopted nests (one name available)", "Nests available for adoption"),    
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      addMiniMap(zoomLevelOffset = -6, 
                 position = "bottomleft", 
                 toggleDisplay = TRUE) %>%   
      addResetMapButton() %>%
      addScaleBar(position = "bottomright") %>% 
      onRender( # moves the zoom control to the bottom right
        "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }")
  })  
  
  toclear <- c("newmark", "newmark1", "newmark2", "newmark3")
  
  ### Dropdown observeEvents
  ######################## Nestbox  #############################################
  reactive_searchnbnum <- reactive({ 
    input$nest_box_number
  })
  
  
  # Add observeEvent to handle changes in the selectizeInput for nestbox number
  observeEvent(input$nest_box_number, {
    # Get the selected nestbox ID from the input
    leafletProxy("map") %>% clearGroup(toclear) %>%  clearPopups()
    
    if(reactive_searchnbnum() != "All"){
      nn <- reactive_searchnbnum()
      df <- activenests2[activenests2$Nest.ID == nn,]
      
      leafletProxy("map") %>% 
        addCircleMarkers(data = df, 
                         lat = ~Lat, lng = ~Lon, 
                         layerId = "newmark",
                         radius = 10, 
                         label = ~df$Nest.ID,
                         fillColor = "yellow",
                         fillOpacity = 0.5, 
                         stroke = TRUE,
                         color ="transparent", 
                         weight = 6) %>% 
        addPopups(lat = df$Lat, lng = df$Lon,
                  popup =  popup_nests(df), 
                  options = popupOptions(closeButton = TRUE, keepInView = TRUE, autoPan = TRUE, maxHeight = "auto")) %>% 
        setView(lng = df$Lon, lat = df$Lat, zoom = 18)
    } 
  }) 
  
  ######################## Neighbourhood  ######################################
  # Filter the data for the selected neighbourhood
  reactive_searchnbhood <- reactive({ 
    input$neighbourhood
  })
  
  # Add observeEvent to handle changes in the selectizeInput
  observeEvent(input$neighbourhood, {
    # Get the selected nestbox ID from the input
    leafletProxy("map") %>% clearGroup(toclear) %>%  clearPopups()
    
    if(reactive_searchnbhood() != "All"){
      nn <- reactive_searchnbhood()
      df <- activenests2[activenests2$Neighborhood == nn,] %>%
        filter(!is.na(Neighborhood))
      dff <- nbhood_sum[nbhood_sum$Neighborhood == nn,]
      df_poly <- nbhoods_wgs[nbhoods_wgs$layer == nn,]
      lat.1 <- st_coordinates(st_centroid(df_poly))[, "Y"]
      long.1 <- st_coordinates(st_centroid(df_poly))[, "X"]
      
      leafletProxy("map") %>% 
        # Highlight searched polygon
        addPolygons(data = df_poly, 
                    group = "Neighbourhoods",
                    fillColor = "transparent",
                    color = "yellow",
                    opacity = 1.5,
                    #fillOpacity = 0,
                    weight = 3, 
                    layerId = "newmark2",
                    label = ~df_poly$layer) %>% 
        # Add all markers so that they are still clickable
        # Active/onhold/na = not available for adoption
        addCircleMarkers(data = adopt_active, 
                         lat = ~Lat, lng = ~Lon, 
                         group = "Adopted nests",
                         radius = 1.5, 
                         label = ~adopt_active$Nest.ID,
                         popup =  popup_nests(adopt_active), 
                         fillColor = ~colours2(adopt_active$Neighborhood),
                         fillOpacity = 1, 
                         color ="transparent", stroke = TRUE, weight = 6, 
                         options = popupOptions(closeButton = TRUE, keepInView = TRUE, autoPan = FALSE, maxHeight = "auto")) %>% 
        # Active for 1 nest
        addCircleMarkers(data = adopt_active_1, 
                         lat = ~Lat, lng = ~Lon, 
                         group = "Adopted nests (one name available)",
                         radius = 1.5, 
                         label = ~adopt_active_1$Nest.ID,
                         popup =  popup_nests(adopt_active_1), 
                         fillColor = ~colours2(adopt_active_1$Neighborhood),
                         fillOpacity = 1, 
                         color ="transparent", stroke = TRUE, weight = 6, 
                         options = popupOptions(closeButton = TRUE, keepInView = TRUE, autoPan = FALSE, maxHeight = "auto")) %>% 
        # Inactive = available for adoption
        addCircleMarkers(data = adopt_inactive, 
                         lat = ~Lat, lng = ~Lon, 
                         group = "Nests available for adoption",
                         radius = 1.5, 
                         label = ~adopt_inactive$Nest.ID,
                         popup =  popup_nests(adopt_inactive), 
                         fillColor = ~colours2(adopt_inactive$Neighborhood),
                         fillOpacity = 1, 
                         color ="transparent", stroke = TRUE, weight = 6,
                         options = popupOptions(closeButton = TRUE, keepInView = TRUE, autoPan = FALSE, maxHeight = "auto")) %>% 
        addPopups(lng = long.1, lat = lat.1,
                  popup = paste("<h3 style='color: red'>", dff$Neighborhood,"</h3>",
                                "<b> Sponsor:</b>", dff$Business.name, "<br>",
                                "<b> Status:</b>", dff$Status, "<br>",
                                "<b>Number of nests:</b>", dff$n.y,"<br>","<b>", sep=" "),
                  options = popupOptions(closeButton = TRUE, keepInView = TRUE, autoPan = FALSE, maxHeight = "auto")) %>% 
        setView(lng = long.1, lat = lat.1, zoom = 18)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)





