library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(gplots)
zipdata<- allzips
#zipdata <- allzips[order(-allzips$avg),]

function(input, output, session) {
  ## Interactive Map ###########################################
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -120.51, lat = 38.06, zoom = 7)
  })

  # decide on working data set 
	# full record
  working_data_set <- reactive({if (input$record_length == "full"){
	  # chose set by metric
		working_set <- 
		switch(input$metric, 
					"avg" = full_vol,
					"duration" = full_dur,
					"intraannual_frequency" = full_intra,
					"interannual_frequency" = full_vol,
					"timing" = full_vol)	
  }
	# post-impairment record
  else {
		working_set <- 
		switch(input$metric, 
					"avg" = imp_vol,
					"duration" = imp_dur,
					"intraannual_frequency" = imp_intra,
					"interannual_frequency" = imp_vol,
					"timing" = imp_vol)	
	}
	
	# filter set by year type
	working_set_yt <- working_set[which(working_set$yeartype == input$yeartype),]
	# filter by period
	working_set_yt_period <- working_set_yt[which(working_set_yt$period == input$period),]
	# merge status, filter by site type
	working_set_status <- merge(working_set_yt_period, gauge_data , by.x = "gauge", by.y = "site_no", all.x = TRUE)
	final_working_set <- 
	switch(input$site_type, 
				 "impaired" = working_set_yt_period[which(working_set_yt_period$status=="impaired"),],
				 "unimpaired" = working_set_yt_period[which(working_set_yt_period$status=="unimpaired"),],																					
			   "both" = final_working_set)})

	# A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(working_data_set[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(working_data_set,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  })

   observe({
    colorBy <- input$metric
    sizeBy <- input$metric
	if (sizeBy == "avg") {
	colorlist <-  c("black","orangered","khaki1","olivedrab1","chartreuse3","green4","aquamarine2","deepskyblue4","blue","royalblue4","navyblue")
	bounds <- c(0,1000,10000,50000,125000,200000,400000,800000,1500000,2500000,3500000)
	labs <-  c("0","1 AF - 1 TAF","1000 - 10000","10000 - 50000","50000 - 125000","125000 - 200000","200000 - 400000","400000 - 800000","800000 - 1500000","1500000 - 2500000","2500000 - 3500000")

    colorData <- working_data_set[[colorBy]]
	  classdata <- rep(NA,length(colorData))
	  classdata[which(colorData== bounds[[1]])] <- 1
	  for(i in 2:length(bounds)){
		  classdata[which(colorData > bounds[[i-1]] & colorData <= bounds[[i]] )] <- i
	  }

    pal <- colorFactor(palette=colorlist, domain=seq(1,11,1), na.color="black")

	  # sort in descending order so larger points rendered first
		# for some reason site info on pop up lost
    #temp <- zipdata[order(-zipdata$avg),]
	   
    #if (sizeBy == "avg") {
      # Radius is treated specially in the "superzip" case.
     # radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    #} else {
      radius <- 500 + (working_data_set[[sizeBy]] / max(working_data_set[[sizeBy]]) * 30000)
    #}
   
    	   
    leafletProxy("map", data = working_data_set) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~site_no,
        stroke=TRUE, fillOpacity=0.85, weight= 1, color ="#000000", fillColor=pal(classdata)) %>%
      addLegend("bottomleft", values=seq(1,11,1), colors=col2hex(colorlist), title=colorBy,
        layerId="colorLegend", opacity=0.85, labels=labs)
	}
  })

  # Show a popup at the given location
  showZipcodePopup <- function(site_no, lat, lng) {
    selectedZip <- allzips[allzips$site_no == site_no,]
    content <- as.character(tagList(
      tags$h4("Site Number:", as.integer(selectedZip$site_no)),
      tags$br(),
      sprintf("Station Name: %s", selectedZip$statname), tags$br(),
      sprintf("Longitude: %s", selectedZip$longitude), tags$br(),
      sprintf("Latitude: %s", selectedZip$latitude), tags$br(),
      sprintf("Status: %s", selectedZip$status), tags$br()
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
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
      selected = stillSelected)
  })

  observe({
    site_nos <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Site_No') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$site_nos[input$site_nos %in% site_nos])
    updateSelectInput(session, "site_nos", choices = site_nos,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      site <- input$goto$site
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(site, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$site_nos) | Site_No %in% input$site_nos
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-site="', Site_No, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
