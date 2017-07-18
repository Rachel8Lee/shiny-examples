library(leaflet)

# Choices for drop-downs
record_length <- c(
  "Full" = "full",
  "Post Impairment" = "imp"
)

site_type <- c(
  "Impaired" = "impaired",
  "Unimpaired" = "unimpaired",
  "Include both" = "full"
)

metric <- c(
  "Magnitude" = "magnitude",
  "Duration" = "duration",
  "Inter-Annual Frequency" = "interannual_frequency",
  "Intra-Annual Frequency" = "intraannual_frequency",
  "Timing" = "timing"
)

year_type <- c(
  "All" = "all",
  "Above Normal" = "AN",
  "Below Normal" = "BN",
  "Critical" = "C",
  "Dry" = "D"
) 

navbarPage("Availability of high-magnitude streamflow for groundwater banking in the Central Valley, California", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Site Manager"),
        selectInput("record_length","Record Length", record_length), 
        selectInput("sites", "Sites Included", site_type),
        selectInput("year_type", "Year Type", year_type),
        selectInput("metric", "Metric", metric),

        plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250),
        resize = "both"
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('UC Davis Depart of Land, Air and Water Resource, 2017') 
      )  
    )
  )
)
