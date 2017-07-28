library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(gplots)
source("website_barplots.R")
#source("convert.js")

sitedata <- allsites

function(input, output, session) {
## Interactive Map ###########################################
# Create the map

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -120.51, lat = 38.06, zoom = 6)
  })
	
  #sitedata <- reactive({(paste(input$record, input$metric, input$yeartype, input$period,sep="_"))})
 
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  #siteInBounds <- reactive({
   # if (is.null(input$map_bounds))
    #  return(sitedata[FALSE,])
    #bounds <- input$map_bounds
    #latRng <- range(bounds$north, bounds$south)
    #lngRng <- range(bounds$east, bounds$west)
    
    ##subset(sitedata,
      #latitude >= latRng[1] & latitude <= latRng[2] &
      #longitude >= lngRng[1] & longitude <= lngRng[2])
  #})


  output$testplot <- renderPlot({
 	  my_barplot(imp.full, "vol MAF", monthly = TRUE, full = TRUE)
  })
	
	output$testplot2 <- renderPlot({
 	  my_barplot(imp.full, "vol MAF", monthly = TRUE, full = TRUE)
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
   
	##add if statement for  metric variables
  if (input$metric == "magnitude") {
	colorlist <-  c("black","orangered","khaki1","olivedrab1","chartreuse3","green4","aquamarine2","deepskyblue4","blue","royalblue4","navyblue")
	bounds <- c(0,1000,10000,50000,125000,200000,400000,800000,1500000,2500000,3500000)
	labs <-  c("0","1 AF - 1 TAF","1TAF - 10TAF","10TAF- 50TAF","50TAF - 125TAF","125TAF - 200TAF","200TAF - 400TAF","400TAF - 800TAF","800TAF - 1.5MAF","1.5MAF - 2.5MAF","2.5MAF - 3.5MAF")
	legendTitle <- "Magnitude (HMF Volume)"
	zoomsize <- input$map_zoom
  sizes <- c(1,3,6,9,12,15,18,21,24,27,30) }
  
  else if (input$metric == "duration") {
	colorlist <- c("black", "darkmagenta", "magenta", "blueviolet", "royalblue", "turquoise")
	bounds <- c(0,1,10,20,40,60,80)
	labs <-	c("0","1 - 10","10 - 20","20 - 40","40 - 60", "60 - 80")
	legendTitle <- "Duration (HMF Days)"
  sizes <- c(1,3,6,9,12,15) }
		
	else{	
  colorlist <- c("black","yellow","darkorange","deeppink","dark violet","navy")
	bounds <- c(0,1,4,8,12,16,20)
	labs <- c("0", "1 - 4","4 - 8", "8 - 12", "12 - 16","16 - 20")
  legendTitle <- "No. 1-Day Peaks"
	sizes <- c(1,3,6,9,12,15) }

	  # size for legend icons
    sizes <- sizes + (input$map_zoom - 6)
    
    dom <- seq(1,length(bounds),1)  
	  
	  #unimp_sites <- subset(sitedata(), sitedata()[,5]=="unimpaired")
	  #sitedata()[which(sitedata()$status=="unimpaired"),] 
	  #imp_sites <- 
	  
    colorData <- sitedata$avg
	  classdata <- rep(NA,length(colorData))
	  classdata[which(colorData== bounds[[1]])] <- 1
	  for(i in 2:length(bounds)){
		  classdata[which(colorData > bounds[[i-1]] & colorData <= bounds[[i]] )] <- i
	  }
     
     pal <- colorFactor(palette=colorlist, domain=dom, na.color="black")
	  
     radius <- 10000*sitedata$avg/max(sitedata$avg) + 3000

	  colorlist <- col2hex(colorlist)
  
  colorAdditions <- paste0(colorlist, "; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labs, "</div>")
	  
    	  
    leafletProxy("map", data = sitedata) %>%
      clearShapes() %>% 
      addCircles(~longitude, ~latitude, radius=radius, layerId=~site_no,
        stroke=TRUE, weight = 1, color ="#000000", fillOpacity=0.85, fillColor=pal(classdata)) %>%
      addLegend("bottomleft", values=dom, colors=colorAdditions, title=legendTitle,
        layerId="colorLegend", opacity=0.85, labels=labelAdditions)
	
  })

  # Show a popup at the given location
  showSitePopup <- function(site_no, lat, lng) {
    selectedSite <- allsites[allsites$site_no == site_no,]
    content <- as.character(tagList(
      tags$h4("Site Number:", as.integer(selectedSite$site_no)),
      tags$br(),
      sprintf("Station Name: %s", selectedSite$statname), tags$br(),
      sprintf("Longitude: %s", selectedSite$longitude), tags$br(),
      sprintf("Latitude: %s", selectedSite$latitude), tags$br(),
      sprintf("Status: %s", selectedSite$status), tags$br(),
      sprintf("Average: %s", selectedSite$metric), tags$br()	    
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = site_no)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    isolate({
      showSitePopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################
  
  output$downloadData <- downloadHandler(filename = "temp.csv", content = allsites)
  
}
