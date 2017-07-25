library(leaflet)
library(shinydashboard)

# Choices for drop-downs
record_length <- c("Full" = "full", "Post-Impairment" = "post_imp")	

site_type <- c("Impaired" = "impaired", "Unimpaired" = "unimpaired", 
							 "All Sites" = "both")

metric <- c("Magnitude" = "magnitude", "Duration" = "duration", "Inter-Annual Frequency" = "interaanual frequency", 
					"Intra-Annual Frequency" = "intraannual frequency", "Timing" = "timing")
 
period <- c("January"= "jan", "February" = "feb", "March" = "mar", "April" = "apr", 
						"November" = "nov", "December" = "dec", "November to April" = "novtapr", 
						"December to February" = "dectfeb", "Hydrologic Year" = "hy")

year_type <- c("Above Normal" = "AN", "Below Normal" = "BN", "Critical" = "C", "Dry" = "dry", "All" = "all")

sites <- paste("USGS ", allsites$zipcode, ", ", allsites$station_nm, sep="")

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
					column(width=12,
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
													selectInput("metric", "Metric", metric)
												)
										
									), 
										column(width=6,
											box(id="selectbox2",width=NULL, #collapsible=TRUE,
													selectInput("period","Time Period", period),
													selectInput("yeartype", "Year Type", year_type)
												)
										
									), style="font-size:small;")),	 
								 	
								 div(width=12,
									 column(width=12,
										 box(id="selectbox3", width=NULL,
												selectInput("site","Site", sites)
												)
										)
									),

							fluidRow(column(width=12,
											box(id="plotboxinteractive", width=NULL,
													tags$style(type = "text/css", "#testplot {height: calc(100vh - 410px) !important;}"),
													plotOutput("testplot")
											)
										)
									)
							)
						)
				),

			tabItem(tabName= "dataexplorer",
			fluidRow(column(width=6,
											box(id="selectbox",width=NULL, #collapsible=TRUE,
													selectInput("record","Record Length", record_length),
													selectInput("metric", "Metric", metric),
													selectInput("period","Time Period", period),
													selectInput("yeartype", "Year Type", year_type),
													selectInput("site1","Site", sites),
													selectInput("site2","Site", sites),
													selectInput("site3","Site", sites)
												)
										)
									),
							
			fluidRow(column(width=12,
				       box(id="plotboxexplorer", width=NULL,
					         tags$style(type = "text/css", "#testplot {height: calc(100vh - 410px) !important;}"),
						                  plotOutput("testplot")
						      	         )
					         )
			        )
		)))


dashboardPage(
		title="Flow Availability",
		header=header,
		sidebar=sidebar,
		body=bodies,
		skin="black"
)
