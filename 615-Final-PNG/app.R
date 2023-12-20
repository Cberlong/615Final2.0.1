library(shiny)
library(bslib)
library(shinypanels)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(readr)
library(ggplot2)


### Don't forget to comment your code!!


PNGui <- navbarPage(id = "tabs",
                    title = "Papua New Guinea",
                    
                    ## a comment for each interface component sounds like
                    ## a good idea.
                    
                    navbarMenu(title = "The Pacific",
                               
                               tabPanel("Regions of the Pacific",
                                        img(src="PNG-pacific.png",
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
                      tabPanel("About Papua New Guinea",
                               img(src="PNG flag.jpg", width="200"),
                               p(style="font-size:12pt", "In the Independent State of Papua New Guinea, 
                                 a diverse population of close to 17 million people resides across the eastern half of New Guinea and numerous offshore islands, 
                                 spanning an area of 462,840 square kilometers. the World Bank is currently supporting 11 active projects 
                                 across the country worth approximately US$503 million, including areas like transportation, food safety, health and sanitation.")
                      ),
                      
                      tabPanel("Kiribati world location",
                               img(src="PNG-around.gif",
                                   height="600",
                                   width="800")),
                      tabPanel("Country Map",
                               img(src="PNG map.gif",
                                   height="600",
                                   width="800")
                      )
                    ),
                    
                    nav_panel("Demographics",
                              p("Population Trends"),
                                       # This will display the plot from the server logic
                                       plotOutput("populationPlot")
                              ,
                              p(style="font-size:12pt", "As shown above, the plot suggests that there is
                                a growing trend in Papua New Guinea.")
                              
                    ),
                    
                    nav_panel("Pacific Region",
                              p("And here is a GDP growth rate of Papua New Guinea and other two countries near it, 
                                including Soloman Islands and Indonesia."),
                              img(src="PNG-comparison.jpg",
                                  height="600",
                                  width="800")
                        
                    ),
                    
                    nav_panel("SWOT", p("Analysis",
                                        
                                        panelsPage(
                                          panel(title = "Strengths",
                                                width = 300,
                                                hidden = TRUE,
                                                body = div(
                                                  hr(),
                                                  verbatimTextOutput("debug"),
                                                  hr(),
                                                  p("Papua New Guinea has abundant natural resources which offer substantial revenue potential.
With over 800 languages, PNG's cultural heritage is a significant asset, potentially attracting cultural tourism and enhancing social resilience.
PNG also has a rich biodiversity, with vast rainforests and marine ecosystems that are important for both the environment and ecotourism.")
                                                )),
                                          panel(
                                            title = "Weaknesses",
                                            width = 300,
                                            hidden = FALSE,
                                            body = div(
                                              hr(),
                                              verbatimTextOutput("debug"),
                                              hr(),
                                              p("Inadequate infrastructure, particularly in transportation and healthcare, hinders economic development and access to services.
And heavy reliance on extractive industries and agriculture makes the economy vulnerable to commodity price fluctuations and environmental challenges.
Also the Issues with governance, corruption, and political instability can deter investment and hinder effective policy implementation. Besides, there is 
limited access to quality education and healthcare services affects human capital development.")
                                            ),
                                            hr(),
                                            h2("More info")
                                          ),
                                          panel(
                                            title = "Opportunities",
                                            width = 300,
                                            hidden = FALSE,
                                            body = div(
                                              hr(),
                                              verbatimTextOutput("debug"),
                                              hr(),
                                              p("Opportunities to develop sustainable industries, such as eco-tourism and sustainable agriculture, can drive economic growth while preserving the environment.
PNG can leverage international partnerships for economic development, technological transfer, and climate change mitigation strategies.
As mentioned in demographics, a young and growing population can provide a dynamic workforce if coupled with education and skill development.")
                                            )
                                          ),
                                          panel(
                                            title = "Threats",
                                            hidden = FALSE,
                                            body = div(
                                              hr(),
                                              verbatimTextOutput("debug"),
                                              hr(),
                                              p("PNG is highly vulnerable to climate change impacts, including sea-level rise, which threaten its ecosystems and communities.
Global economic shifts, particularly in commodity markets, can significantly impact PNG's economy.
Public health issues, including the threat of pandemics and endemic diseases like malaria, pose significant challenges.")
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
  
  # Reading datasets about cities and population
  png_cities <- read_csv(file = "pg.csv", show_col_types = FALSE)
  
  png_pop <- read_csv("fusion_GLOBAL_DATAFLOW_UNICEF_1.0_PNG.DM_POP_TOT. (1).csv")
  
  # Render map of Papua New Guinea with markers for cities
  output$pngMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 145.778055, lat = -6.314993, zoom = 6) %>%
      addMarkers(data = png_cities, popup = ~city)
  })
  
  output$populationPlot <- renderPlot({
    # Create the ggplot object
    ggplot(png_pop, aes(x = png_pop$`TIME_PERIOD:Time period`, y = png_pop$`OBS_VALUE:Observation Value`)) +
            geom_line() +
            labs(title = "Population Trend in Papua New Guinea", x = "Year", y = "Population")
  })
  
  # Other outputs such as plots or tables can be added here
  # using renderPlot, renderTable, etc.
}


# Run the application 
shinyApp(ui = PNGui, server = PNGserver)

