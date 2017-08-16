library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(gplots)
library(ggplot2)
library(leaflet)
source("barplots2.R")
source("intERplotscode.R")
source("timing_plot_code.R")

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
	
 # reactive data set 
  sitedata <- reactive({	      
	  if (input$metric == "magnitude") {
	    temp <- subset(allsites, allsites$tag == input$record & allsites$yeartype == input$yeartype & allsites$period == input$period & allsites$valtype == "vol AF") }
          else if (input$metric == "duration") {
	    temp<- subset(allsites, allsites$tag == input$record & allsites$yeartype == input$yeartype & allsites$period == input$period & allsites$valtype == "duration_days") }
	  else if (input$metric == "intraannual frequency"){
	    temp<- subset(allsites, allsites$tag == input$record & allsites$yeartype == input$yeartype & allsites$period == input$period & allsites$valtype == "intraannual_frequency_numpeaks") }
	  else if (input$metric == "interannual frequency"){
			temp<- subset(allsites, allsites$tag == input$record & allsites$yeartype == input$yeartype & allsites$period == input$period & allsites$valtype == "intERannual_frequency_fraction_of_years") }
		else {temp <- subset(allsites, allsites$tag == input$record & allsites$yeartype == input$yeartype & allsites$valtype == "timing")}
	  temp <- temp[order(temp$avg, decreasing = TRUE),]
	  if (length(input$sitetype) == 1) {
		  temp <- temp[which(temp$status == input$sitetype),]}
	  return(temp)
	})
	
	# to download
	sitedataDE <- reactive({	      
	  if (input$metricDE == "magnitude") {
	    temp <- subset(allsites, allsites$tag == input$recordDE & allsites$yeartype == input$yeartypeDE & allsites$period == input$periodDE & allsites$valtype == "vol AF") }
          else if (input$metricDE == "duration") {
	    temp <- subset(allsites, allsites$tag == input$recordDE & allsites$yeartype == input$yeartypeDE & allsites$period == input$periodDE & allsites$valtype == "duration_days") }
	  else if (input$metricDE == "intraannual frequency"){
	    temp <- subset(allsites, allsites$tag == input$recordDE & allsites$yeartype == input$yeartypeDE & allsites$period == input$periodDE & allsites$valtype == "intraannual_frequency_numpeaks") }
	  else if (input$metricDE == "interannual frequency"){
			temp <- subset(allsites, allsites$tag == input$recordDE & allsites$yeartype == input$yeartypeDE & allsites$period == input$periodDE & allsites$valtype == "intERannual_frequency_fraction_of_years") }
	  else {temp <- subset(allsites, allsites$tag == input$recordDE & allsites$yeartype == input$yeartypeDE & allsites$valtype == "timing")}
	  temp <- temp[order(temp$avg, decreasing = TRUE),]
	  return(temp)
	})
	
  output$IMplot <- renderPlot({
    # isolate site ID
	  gauge <- strsplit(input$site, " ")[[1]][2]
	  gauge <- strsplit(gauge, ",")[[1]][1]
	  full_bool <- (input$record == "full")
    monthly_bool <- !(input$period == "November to April" | input$period == "December to February" | input$period == "Hydrologic Year")
    if (input$metric == "interannual frequency"){interplot(gauges=gauge, monthly = monthly_bool, full = full_bool)}
		else if (input$metric == "timing") {
			if (input$sitetiming == "All Sites"){ timingplot(c(), full = full_bool, all = TRUE)}
			else {
			  gauge <- strsplit(input$sitetiming, " ")[[1]][2]
	  		gauge <- strsplit(gauge, ",")[[1]][1]
			  timingplot(gauge, full = full_bool, all = FALSE)
		  }
		}
    else {
      if (input$metric == "magnitude") {yvar <- "vol MAF"}
      else if (input$metric == "duration") {yvar <- "duration_days"}
      else {yvar <- "intraannual_frequency_numpeaks"}
	    d <- gauge_select_plot(gauge, full = full_bool) 
      my_barplot(d, yvar, monthly = monthly_bool, full = full_bool) 
    }
  })
	
	output$DEplot <- renderPlot({
		first_site <- strsplit(input$site1, " ")[[1]][2]
		first_site <- strsplit(first_site, ",")[[1]][1]
		sec_site <- strsplit(input$site2, " ")[[1]][2]
		sec_site <- strsplit(sec_site, ",")[[1]][1]
		third_site <- strsplit(input$site3, " ")[[1]][2]
		third_site <- strsplit(third_site, ",")[[1]][1]
		full_boolDE <- (input$recordDE == "full")
		monthly_boolDE <- !(input$periodDE == "November to April" | input$periodDE == "December to February" | input$periodDE == "Hydrologic Year")
    if (input$metricDE == "interannual frequency") {interplot(gauges=c(first_site, sec_site, third_site), monthly = monthly_boolDE, full = full_boolDE)}
		else if (input$metricDE == "timing") {
			if (input$site1timing == "All Sites") {timingplot(c(), full = full_boolDE, all = TRUE)}
			else {
				first_sitetim <- strsplit(input$site1timing, " ")[[1]][2]
		    first_sitetim <- strsplit(first_sitetim, ",")[[1]][1]
				sec_sitetim <- strsplit(input$site2timing, " ")[[1]][2]
		    sec_sitetim <- strsplit(sec_sitetim, ",")[[1]][1]
		    third_sitetim <- strsplit(input$site3timing, " ")[[1]][2]
		    third_sitetim <- strsplit(third_sitetim, ",")[[1]][1]
			  timingplot(c(first_sitetim, sec_sitetim, third_sitetim), full = full_boolDE, all = FALSE)
			}
	  }		
		else{
      if (input$metricDE == "magnitude") {yvar <- "vol MAF"}
      else if (input$metricDE == "duration") {yvar <- "duration_days"}
      else {yvar <- "intraannual_frequency_numpeaks"}		
	    d2 <- gauge_select_plot(c(first_site, sec_site, third_site), full = full_boolDE)
 	    my_barplot(d2, yvar, monthly = monthly_boolDE, full = full_boolDE)
	  }
 })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    if (input$metric == "magnitude") {
	    colorlist <-  c("black","orangered","khaki1","olivedrab1","chartreuse3","green4","aquamarine2","deepskyblue4","blue","royalblue4","navyblue")
	    bounds <- c(0,1000,10000,50000,125000,200000,400000,800000,1500000,2500000,3500000)
	    labs <-  c("0","1 AF - 1 TAF","1TAF - 10TAF","10TAF- 50TAF","50TAF - 125TAF","125TAF - 200TAF","200TAF - 400TAF","400TAF - 800TAF","800TAF - 1.5MAF","1.5MAF - 2.5MAF","2.5MAF - 3.5MAF")
	    legendTitle <- "Magnitude (HMF Volume)"
	    sizes <- c(5,7,9,11,13,15,17,19,21,23,25) 
	    nonscalesize <- c(5000,5500,6000,6500,7000,7500,8000,8500,9000,9500,10000)
	    #rad <- sitedata()$avg/120 + 3000
	  }
  
    else if (input$metric == "duration") {
	    colorlist <- c("black","maroon","magenta","darkslateblue","royalblue","turquoise")
	    bounds <- c(0,10,20,40,60,80)
	    labs <- c("0","1 - 10","10 - 20","20 - 40","40 - 60", "60 - 80")
	    legendTitle <- "Duration (HMF Days)"
      sizes <- c(12,14,16,18,20,22) 
			nonscalesize <- c(5000,6000,7000,8000,9000,10000)
	    #rad <- 150*sitedata()$avg + 3000
	  }
		
		else if (input$metric == "intraannual frequency") {
	    colorlist <- c("black","yellow","darkorange","deeppink","darkviolet","navy")
	    bounds <- c(0,5,11,17,23,27)
	    labs <- c("0", "1 - 5","6 - 11", "12 - 17", "18 - 23","23 - 27")
      legendTitle <- "No. 1-Day Peaks"
	    sizes <- c(12,14,16,18,20,22) 
			nonscalesize <- c(5000,6000,7000,8000,9000,10000)
	    #rad <- 300*sitedata()$avg + 3000
	  }
		
    else if (input$metric == "interannual frequency"){
      colorlist <- c("black", "aquamarine", "darkturquoise", "steelblue", "mediumblue", "navy")
      bounds <- c(0,20,40,60,80,100)
      labs <- c("0%", "1 - 20%", "20- 40%", "40 - 60%", "60 - 80%", "80 - 100%")
      legendTitle <- "% of Years with HMF"
      sizes <- c(12,14,16,18,20,22)
			nonscalesize <- c(5000,6000,7000,8000,9000,10000)
      #rad <- 100*150*sitedata()$avg + 3000
    }
		# timing 
	  else { 	
      colorlist <- c("deeppink", "orangered", "yellow", "mediumspringgreen", "deepskyblue", "darkviolet", "black")
	    bounds <- c(1,2,3,4,5,6,7)
	    labs <- c("January", "February","March", "April", "May","June", "July")
      legendTitle <- "COM Date of HMF"
	    sizes <- c(21,21,21,21,21,21,21) 
			nonscalesize <- c(10000,10000,10000,10000,10000,10000,10000)
	    #rad <- 10000
	  }  
		
	  # size for legend icons
    #sizes <- sizes + (input$map_zoom - 6)
    dom <- seq(1,length(bounds),1)  
	  
		if(input$metric == "interannual frequency") {colorData <- sitedata()$avg*100}
		else {colorData <- sitedata()$avg}
    classdata <- rep(NA,length(colorData))
		classsize <- rep(NA,length(colorData))
    classdata[which(colorData == bounds[[1]])] <- 1
    classdata[which(colorData == "NA")] <- 1  
    classdata[which(colorData == bounds[[1]])] <- nonscalesize[1]
    classsize[which(colorData == "NA")] <- nonscalesize[1]  
    for(i in 2:length(bounds)){
      classdata[which(colorData > bounds[[i-1]] & colorData <= bounds[[i]] )] <- i    
      classsize[which(colorData > bounds[[i-1]] & colorData <= bounds[[i]] )] <- nonscalesize[i]
    }
		#if( input$metric == "timing")	{classsize <- rad}						 
     
    pal <- colorFactor(palette=colorlist, domain=dom, na.color="black")
    colorlist <- col2hex(colorlist)
    # modify legend size 
    colorAdditions <- paste0(colorlist, "; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labs, "</div>")
    	  
    leafletProxy("map", data = sitedata()) %>%
      clearShapes() %>% 
      addCircles(~longitude, ~latitude, radius=classsize, layerId=~site_no, stroke=TRUE, 
                 weight = 1, color ="#000000", fillOpacity=0.9, fillColor=pal(classdata)) %>%
      addLegend("bottomleft", values=dom, colors=colorAdditions, title=legendTitle, layerId="colorLegend", 
                opacity=0.9, labels=labelAdditions)
  })

  # Show a popup at the given location
  showSitePopup <- function(site_no, lat, lng) {
    selectedSite <- sitedata()[sitedata()$site_no == site_no,]
	  if(selectedSite$valtype[1]=="vol AF")	{
		  valuename<-"Magnitude"
			unitMeasure <-"TAF"}
		else if(selectedSite$valtype[1]=="duration_days")	{
			valuename<-"Duration"
		  unitMeasure <-"Days"}
	  else if(selectedSite$valtype[1]=="intraannual_frequency_numpeaks")	{
			valuename<-"Intra-Anual Frequency"
		  unitMeasure<-"Number of Peaks"}
	  else if(selectedSite$valtype[1]=="intERannual_frequency_fraction_of_years")	{
			valuename<-"Inter-Annual Frequency"
		  unitMeasure<-"Fraction of Years"}
	  else{
			valuename<-"Timing"
			unitMeasure<-"Timing"	}
    content <- as.character(tagList(
      tags$h4("Site Number:", as.integer(selectedSite$site_no)),
      sprintf("Station Name: %s", selectedSite$station_nm), tags$br(),
      sprintf("Longitude: %s", selectedSite$longitude), tags$br(),
      sprintf("Latitude: %s", selectedSite$latitude), tags$br(),
      sprintf("Status: %s", selectedSite$status), tags$br(),
      sprintf("%s : %.2f %s", valuename, selectedSite$avg, unitMeasure), tags$br()	    
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
  
  output$downloadData <- downloadHandler(filename = "temp.csv", content = function(file) {write.csv(sitedataDE(), file)})  
}
