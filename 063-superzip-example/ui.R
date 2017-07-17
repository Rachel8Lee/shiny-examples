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
  "Magnitude" = "avg",
  "Duration" = "duration",
  "Inter-Annual Frequency" = "college",
  "Intra-Annual Frequency" = "intraannual_frequency",
  "Timing" = "adultpop"
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
        selectInput("record length","Record Length", record_length), 
        selectInput("sites", "Sites Included", site_type),
        selectInput("yeartype", "Year Type", year_type),
        selectInput("metric", "Metric", metric, selected = "avg"),

        plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250),
        resize = "both"
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('UC Davis Depart of Land, Air and Water Resource, 2017') 
      )  
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All sites"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=100, value=100)
      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),

  conditionalPanel("false", icon("crosshair"))
)
