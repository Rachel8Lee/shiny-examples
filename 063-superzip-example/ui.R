library(leaflet)
library(shinydashboard)

# Choices for drop-downs
record_length <- c("Full" = "full", "Post-Impairment" = "post_imp")	

site_type <- c("Impaired" = "impaired", "Unimpaired" = "unimpaired", 
							 "All Sites" = "both")

vars <- c("Magnitude" = "avg", "Duration" = "centile", "Inter-Annual Frequency" = "college", 
					"Intra-Annual Frequency" = "income", "Timing" = "adultpop")
 
period <- c("January"= "jan", "February" = "feb", "March" = "mar", "April" = "apr", 
						"November" = "nov", "December" = "dec", "November to April" = "novtapr", 
						"December to February" = "dectfeb", "Hydrologic Year" = "hy")

year_type <- c("Above Normal" = "AN", "Below Normal" = "BN", "Critical" = "C", "Dry" = "dry", "All" = "all")

#header
header <- dashboardHeader(titleWidth=150,
		title = "Menu"
)

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
					column(width=12,
							fluidRow(tags$head(tags$style(HTML('
																	.form-group, .selectize-control {
																	margin-bottom: 5px;
																	}'))), 
									div(
									column(width=6,
											box(id="selectbox",width=NULL, #collapsible=TRUE,
													selectInput("record","Record Length", record_length),
													selectInput("metric", "Metric", vars)
												)
										
									), 
										column(width=6,
											box(id="selectbox2",width=NULL, #collapsible=TRUE,
													selectInput("period","Time Period", period),
													selectInput("yeartype", "Year Type", year_type)
												)
										
									), style="font-size:small;")),	 

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
#					fluidRow(column(width=12,
#									infoBox(div("Availability of high-magnitude streamflow for groundwater banking in the Central Valley, CA",
#													style = "white-space: normal; word-wrap: break-word"), 
#											value = NULL,
#											subtitle="Data Explorer",
#											icon = shiny::icon("info-circle"), color = "aqua", width = NULL,
#											href = NULL, fill = FALSE) 
#							)),
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
			)
		)

)

dashboardPage(
		title="Flow Availability",
		header=header,
		sidebar=sidebar,
		body=bodies,
		skin="black"
)
