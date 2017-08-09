library(leaflet)
library(shinydashboard)

# Choices for drop-downs
record_length <- c("Full" = "full", "Post-Impairment" = "post-impairment")	

site_type <- c("Impaired" = "impaired", "Unimpaired" = "unimpaired")

metric <- c("Magnitude" = "magnitude", "Duration" = "duration", "Inter-Annual Frequency" = "interannual frequency", 
					"Intra-Annual Frequency" = "intraannual frequency", "Timing" = "timing")
 
period <- c("January"= "January", "February" = "February", "March" = "March", "April" = "April", 
						"November" = "November", "December" = "December", "November to April" = "November to April", 
						"December to February" = "December to February", "Hydrologic Year" = "Hydrologic Year")

year_type <- c("All" = "all", "Above Normal" = "AN", "Below Normal" = "BN", "Critical" = "C", "Dry" = "D", "Wet" = "W")

sites <- paste("USGS ", gauge_data$site_no, ", ", gauge_data$station_nm, sep="")
siteblank <- c(" ", sites)
includeallsites <- c("All Sites", sites)

#header
header <- dashboardHeader(titleWidth=150, title = "Menu")

sidebar <- dashboardSidebar(width=150,
				sidebarMenu(
					menuItem("Interactive Map", tabName="interactivemap", icon=icon("globe")),
					menuItem("Data Explorer", tabName = "dataexplorer", icon=icon("info-circle"))
				)
)

bodies <- dashboardBody( 
  tags$script(HTML('
                   $(document).ready(function() {
                   $("header").find("nav").append(\'<div class="myClass"> Availability of High-magnitude Streamflow for Groundwater Banking </div>\');
                   })
                   ')),
  tabItems(
    tabItem(tabName="interactivemap",
				
            fluidRow(
              column(width=6,
                     box(width=NULL, height=NULL,
                         tags$head(
                           # Include our custom CSS
                           includeCSS("styles.css"),
                           includeScript("gomap.js")
                         ),
                         tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
                         
                         leafletOutput("map")
                         
                     )
              ),
              column(width=6,
                     fluidRow(tags$head(tags$style(HTML('
                                                        .form-group, .selectize-control {
                                                        margin-bottom: 5px;
                                                        }'))), 
									div(
									column(width=4,
											box(id="selectbox",width=NULL, 
													selectInput("record","Record Length", record_length),
													selectInput("metric", "Metric", metric)
												)
										
									),
									column(width=4,
											box(id="selectbox",width=NULL, #collapsible=TRUE,
													selectInput("period","Time Period", period),
													selectInput("yeartype", "Year Type", year_type)
											)
									
									),
									column(width=4,
											box(id="selectbox",width=NULL, 
													 checkboxGroupInput("sitetype", "Select Site Type:", c("Impaired" = "impaired", "Unimpaired" = "unimpaired"), selected = c("impaired", "unimpaired")
													                   )
												)
										
									), style="font-size:small;")),
							fluidRow(column(width=12),
									box(id="selectbox2",width=NULL, #collapsible=TRUE,
											selectInput("site","Site Selection", sites)
									)
									),
									fluidRow(column(width=12,
									                box(width=NULL,
									                    tags$style(type = "text/css", "#testplot {height: calc(100vh - 410px) !important;}"),
									                    plotOutput("testplot")
									                )
									)
									)
                     )
              )
    ),
    tabItem(tabName= "dataexplorer",
            fluidRow(column(width=4,
                            box(id="selectbox3",width=NULL, #collapsible=TRUE,
                                selectInput("record","Record Length", record_length),
                                selectInput("metric", "Metric", metric),
                                selectInput("period","Time Period", period),
                                selectInput("site1","Site", sites),
                                selectInput("site2","Site", siteblank, selected = " "),
                                selectInput("site3","Site", siteblank, selected = " ")
                            )
            ),
										 
            column(width=8,
                   box(id="plotboxexplorer", width=NULL,
                       tags$style(type = "text/css", "#testplot {height: calc(100vh - 410px) !important;}"),
                       plotOutput("testplot2")
                   )
            )
            ),
						fluidRow(column(title="Download Data Set",width=12,
                            box(id="selectbox4",width=NULL, 
																selectInput("yeartype", "Year Type", year_type),
																downloadButton("downloadData", "Download")
                            )
            ))
    )))




dashboardPage(
		title="Flow Availability",
		header=header,
		sidebar=sidebar,
		body=bodies,
		skin="black"
)
