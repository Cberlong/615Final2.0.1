library(shiny)
library(bslib)
library(shinypanels)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(readr)


### Don't forget to comment your code!!


PNGui <- navbarPage(id = "tabs",
                    title = "Papua New Guinea",
                    
                    ## a comment for each interface component sounds like
                    ## a good idea.
                    
                    navbarMenu(title = "The Pacific",
                               
                               tabPanel("Regions of the Pacific",
                                        img(src="Oceania_UN_Geoscheme_-_Map_with_Zones.svg",
                                            height="600",
                                            width="800")
                               ),
                               
                               tabPanel("where?",
                                        img(src="PNG location.jpg",
                                            height="600",
                                            width="800")
                                        #actionButton("recalc", "New points")
                               )
                               
                    ),
                    
                    ## general information tab
                    
                    navbarMenu(
                      title = "General Information",
                      tabPanel("About Kiribati",
                               img(src="KR-flag.jpg", width="200"),
                               p(style="font-size:12pt", "In the Republic of Kiribati, more than 119,000 people
              live across 33 islands stretching over
              3.5 million square kilometers of the Pacific.
              The World Bank’s support to Kiribati is focused on
              improving roads, sustainable fisheries,
              health system strengthening, access to clean,
              safe drinking water and economic growth. ")
                      ),
                      
                      tabPanel("Kiribati world location"),
                      tabPanel("Regional Map"),
                      tabPanel("Country Map",
                               leafletOutput("mycountrymap", height = 700),
                               p()
                      )
                    ),
                    
                    nav_panel("Demographics", p("Tables and plots go here."),
                              
                              shinypanels::box("box", colapsed=F)
                    ),
                    
                    nav_panel("Pacific Region",
                              p("A regional map goes here.
              Be sure to include comparisons -- maybe a table,
              definitely different graphs"),
                              
                              shinydashboardPlus::box(
                                solidHeader = FALSE,
                                title = "Status summary",
                                background = NULL,
                                width = 4,
                                status = "danger",
                                footer = fluidRow(
                                  column(
                                    width = 6,
                                    descriptionBlock(
                                      number = "17%",
                                      numberColor = "green",
                                      numberIcon = icon("caret-up"),
                                      header = "$35,210.43",
                                      text = "TOTAL REVENUE",
                                      rightBorder = TRUE,
                                      marginBottom = FALSE
                                    )
                                  )
                                )
                              )
                    ),
                    
                    nav_panel("SWOT", p("Analysis",
                                        
                                        panelsPage(
                                          panel(title = "Strengths",
                                                width = 300,
                                                hidden = TRUE,
                                                body = h1("Nothing here")
                                          ),
                                          panel(
                                            title = "Weaknesses",
                                            width = 300,
                                            color = "magenta",
                                            hidden = FALSE,
                                            body = div(
                                              h3("Here is some info"),
                                              hr(),
                                              verbatimTextOutput("debug"),
                                              hr(),
                                              p("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                                            ),
                                            hr(),
                                            h2("More info")
                                          ),
                                          panel(
                                            title = "Opportunities",
                                            width = 350,
                                            collapsed = TRUE,
                                            body = div(
                                              plotOutput("plot")
                                            )
                                          ),
                                          panel(
                                            title = "Threats",
                                            can_collapse = FALSE,
                                            body = div(
                                            ),
                                            footer = HTML("Footer")
                                          )
                                        )
                                        
                    )
                    
                    
                    ),
                    
                    navbarMenu(
                      title = "Bibliogrphy",
                      align = "right",
                      tabPanel(tags$a(href="https://en.wikipedia.org/wiki/Kiribati",
                                      "Wikipedia/Kiribati")),
                      tabPanel(tags$a(href="https://www.shinyapps.io/",
                                      "shinyapps.io for publishing")),
                      
                      
                      tabPanel(tags$a(href="https://www.un.org/ohrlls/mvi/documents",
                                      "Multidimensional Vulnerability Index")),
                      
                      tabPanel(tags$a(href = "https://kiribati.gov.ki/",
                                      "Kiribati_gov")),
                      tabPanel(tags$a(href="https://www.forumsec.org/",
                                      "Pacific Islands Forum")),
                      tabPanel(tags$a(href="https://rstudio.github.io/leaflet/",
                                      "Leaflet doc")),
                      tabPanel(tags$a(href="https://www.un.org/ohrlls/mvi/documents",
                                      "Multidimensional Vulnerability Index"))
                      
                      
                    )
                    
)


# Define server logic for Papua New Guinea Shiny App
PNGserver <- function(input, output, session) {
  
  # Assuming you have a CSV with Papua New Guinea city data
  png_cities <- read_csv(file = "pg.csv", show_col_types = FALSE)
  
  # Render map of Papua New Guinea with markers for cities
  output$pngMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 145.778055, lat = -6.314993, zoom = 6) %>%
      addMarkers(data = png_cities, popup = ~city)
  })
  
  # Other outputs such as plots or tables can be added here
  # using renderPlot, renderTable, etc.
}


# Run the application 
shinyApp(ui = PNGui, server = PNGserver)

