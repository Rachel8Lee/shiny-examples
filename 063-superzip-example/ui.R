library(leaflet)

# Choices for drop-downs
site_type <- c(
  "Impaired" = "impaired",
  "Unimpaired" = "unimpaired",
  "All Sites" = "both"
)

vars <- c(
  "Magnitude" = "avg",
  "Duration" = "centile",
  "Inter-Annual Frequency" = "college",
  "Intra-Annual Frequency" = "income",
  "Timing" = "adultpop"
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
        selectInput("sites", "Sites Included", site_type),
		selectInput("metric", "Metric", vars),
#        selectInput("color", "Color", vars),
#        selectInput("size", "Size", vars, selected = "adultpop"),
        conditionalPanel("input.color == 'avg' || input.size == 'avg'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),

        plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250),
        resize = "both"
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('UC Davis Depart of Land, Air and Water Resource, 2017') 
      )  
    )
  )

  
