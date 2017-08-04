library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(gplots)
source("barplots2.R")

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
	
  #sitedata <- reactive({
	  #subset(allsites, allsites$record == input$record, allsites$yeartype == input$yeartype, allsites$period, 
		#allsites$metric == input$metric)
	  #paste(input$record, input$metric, input$yeartype, input$period,sep="_")
  #})
 
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  # siteInBounds <- reactive({
   # if (is.null(input$map_bounds))
    # return(sitedata[FALSE,])
    # bounds <- input$map_bounds
    # latRng <- range(bounds$north, bounds$south)
    # lngRng <- range(bounds$east, bounds$west)
    
    # subset(sitedata,
    # latitude >= latRng[1] & latitude <= latRng[2] &
    # longitude >= lngRng[1] & longitude <= lngRng[2])
  # })

  output$testplot <- renderPlot({
    # isolate site ID
	  gauge <- strsplit(input$site, " ")[[1]][2]
	  gauge <- strsplit(gauge, ",")[[1]][1]
    # full, nested monthly
	  if (input$record == "full") {
	    d <- gauge_select_plot(gauge, full = TRUE) 
      if (input$period == "January" |  input$period == "February" | 
		      input$period == "March" | input$period == "April" | input$period == "November" | input$period == "December") {
		      my_barplot(d, "vol MAF", monthly = TRUE, full = TRUE) }
      else {
		    my_barplot(d, "vol MAF", monthly = FALSE, full = TRUE) }
	  } 
    # post-impairment
	  else {
	    d <- gauge_select_plot(gauge, full = FALSE) 
      if (input$period == "January" |  input$period == "February" | 
		      input$period == "March" | input$period == "April" | input$period == "November" | input$period == "December") {
		      my_barplot(d, "vol MAF", monthly = TRUE, full = FALSE) }
      else {
		    my_barplot(d, "vol MAF", monthly = FALSE, full = FALSE) }
	  }
  })
	
	output$testplot2 <- renderPlot({
		first_site <- strsplit(input$site1, " ")[[1]][2]
		first_site <- strsplit(first_site, ",")[[1]][1]
		sec_site <- strsplit(input$site2, " ")[[1]][2]
		sec_site <- strsplit(sec_site, ",")[[1]][1]
		third_site <- strsplit(input$site3, " ")[[1]][2]
		third_site <- strsplit(third_site, ",")[[1]][1]
    
	  d2 <- gauge_select_plot(c(first_site, sec_site, third_site), full = TRUE)
 	  my_barplot(d2, "vol MAF", monthly = TRUE, full = TRUE)
 })
  

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
		# checkbox for site types
    if (length(input$sitetype) == 1) {
		  sitedata <- sitedata[which(sitedata[5] == input$sitetype),]
		}
		
	##add if statement for  metric variables
    if (input$metric == "magnitude") {
	    colorlist <-  c("black","orangered","khaki1","olivedrab1","chartreuse3","green4","aquamarine2","deepskyblue4","blue","royalblue4","navyblue")
	    bounds <- c(0,1000,10000,50000,125000,200000,400000,800000,1500000,2500000,3500000)
	    labs <-  c("0","1 AF - 1 TAF","1TAF - 10TAF","10TAF- 50TAF","50TAF - 125TAF","125TAF - 200TAF","200TAF - 400TAF","400TAF - 800TAF","800TAF - 1.5MAF","1.5MAF - 2.5MAF","2.5MAF - 3.5MAF")
	    legendTitle <- "Magnitude (HMF Volume)"
	    zoomsize <- input$map_zoom
      sizes <- c(1,3,6,9,12,15,18,21,24,27,30) 
	    scalar <- 10000/sitedata$avg[1]
	    rad <- scalar*sitedata$avg + 3000
	  }
  
    else if (input$metric == "duration") {
	    colorlist <- c("black","maroon","magenta","darkslateblue","royalblue","turquoise")
	    bounds <- c(0,10,20,40,60,80)
	    labs <- c("0","1 - 10","10 - 20","20 - 40","40 - 60", "60 - 80")
	    legendTitle <- "Duration (HMF Days)"
      sizes <- c(15,18,21,24,27,30) 
	    scalar <- 10000/sitedata$avg[1]
	    rad <- scalar*sitedata$avg + 3000
	  }
		
		else if (input$metric == "intraannual frequency") {
	    colorlist <- c("black","yellow","darkorange","deeppink","darkviolet","navy")
	    bounds <- c(0,4,8,12,16,20)
	    labs <- c("0", "1 - 4","4 - 8", "8 - 12", "12 - 16","16 - 20")
      legendTitle <- "No. 1-Day Peaks"
	    sizes <- c(15,18,21,24,27,30) 
	    scalar <- 10000/sitedata$avg[1]
	    rad <- scalar*sitedata$avg + 3000
	  }
		
		# havent decided on inter freq and timing yet
	  else { 	
      colorlist <- c("black","yellow","darkorange","deeppink","darkviolet","navy")
	    bounds <- c(0,4,8,12,16,20)
	    labs <- c("0", "1 - 4","4 - 8", "8 - 12", "12 - 16","16 - 20")
      legendTitle <- "No. 1-Day Peaks"
	    sizes <- c(15,18,21,24,27,30) 
	    rad <- 4000
	  }  

	  # size for legend icons
    sizes <- sizes + (input$map_zoom - 6)
    dom <- seq(1,length(bounds),1)  
	 
    colorData <- sitedata$avg
    classdata <- rep(NA,length(colorData))
    classdata[which(colorData == bounds[[1]])] <- 1
    classdata[which(colorData == "NA")] <- 1  
    for(i in 2:length(bounds)){
      classdata[which(colorData > bounds[[i-1]] & colorData <= bounds[[i]] )] <- i
    }
     
    pal <- colorFactor(palette=colorlist, domain=dom, na.color="black")
    colorlist <- col2hex(colorlist)
    # modify legend size 
    colorAdditions <- paste0(colorlist, "; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labs, "</div>")
    	  
    leafletProxy("map", data = sitedata) %>%
      clearShapes() %>% 
      addCircles(~longitude, ~latitude, radius=rad, layerId=~site_no, stroke=TRUE, 
                 weight = 1, color ="#000000", fillOpacity=0.9, fillColor=pal(classdata)) %>%
      addLegend("bottomleft", values=dom, colors=colorAdditions, title=legendTitle, layerId="colorLegend", 
                opacity=0.9, labels=labelAdditions)
  })

  # Show a popup at the given location
  showSitePopup <- function(site_no, lat, lng) {
    selectedSite <- allsites[allsites$site_no == site_no,]
    content <- as.character(tagList(
      tags$h4("Site Number:", as.integer(selectedSite$site_no)),
      sprintf("Station Name: %s", selectedSite$statname), tags$br(),
      sprintf("Longitude: %s", selectedSite$longitude), tags$br(),
      sprintf("Latitude: %s", selectedSite$latitude), tags$br(),
      sprintf("Status: %s", selectedSite$status), tags$br(),
      sprintf("Average: %s", selectedSite$avg), tags$br()	    
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
  
  output$downloadData <- downloadHandler(filename = "temp.csv", content = function(file) {write.csv(sitedata, file)})  
}
