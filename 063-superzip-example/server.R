library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(gplots)

zipdata <- allzips

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

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    hist(zipsInBounds()$centile,
      breaks = centileBreaks,
      main = "Histogram Title",
      xlab = "X Lavel",
      xlim = range(allzips$centile),
      col = '#00DD00',
      border = 'white')
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
	#add if statement for  metric variables
	colorlist <-  c("black","orangered","khaki1","olivedrab1","chartreuse3","green4","aquamarine2","deepskyblue4","blue","royalblue4","navyblue")
	bounds <- c(0,1000,10000,50000,125000,200000,400000,800000,1500000,2500000,3500000)
	labs <-  c("0","1 AF - 1 TAF","1000 - 10000","10000 - 50000","50000 - 125000","125000 - 200000","200000 - 400000","400000 - 800000","800000 - 1500000","1500000 - 2500000","2500000 - 3500000")

    colorData <- zipdata[[colorBy]]
	  classdata <- rep(NA,length(colorData))
	  classdata[which(colorData== bounds[[1]])] <- 1
	  for(i in 2:length(bounds)){
		  classdata[which(colorData > bounds[[i-1]] & colorData <= bounds[[i]] )] <- i
	  }
#		levs <- factor(seq(1,11,1), levels=seq(1,11,1), labels=as.character(bounds))
      pal <- colorFactor(palette=colorlist, domain=seq(1,11,1), na.color="black")
#    }
    zipdata <- zipdata[with(zipdata, order(-avg))]
    #if (sizeBy == "avg") {
      # Radius is treated specially in the "superzip" case.
     # radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    #} else {
      radius <- 500 + (zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000)
    #}
   
    	   
    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
        stroke=TRUE, fillOpacity=0.85, weight= 1, color ="#000000", fillColor=pal(classdata)) %>%
      addLegend("bottomleft", values=seq(1,11,1), colors=col2hex(colorlist), title=colorBy,
        layerId="colorLegend", opacity=0.85, labels=labs)
	
  })

  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allzips[allzips$site_no == site_no,]
    content <- as.character(tagList(
      tags$h4("Site Number:", as.integer(selectedZip$site_no)),
      tags$br(),
      sprintf("Station Name: %s", selectedZip$statname), tags$br(),
      sprintf("Longitude: %s", selectedZip$longitude), tags$br(),
      sprintf("Latitude: %s", selectedZip$latitude), tags$br(),
      sprintf("Status: %s", selectedZip$status), tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
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
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
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
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Site_No, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
