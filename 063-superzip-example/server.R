library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
source("website_barplots.R")

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
 
  #zipdata <- reactive({paste(input$record, input$vars, input$yeartype, input$period, input$sitetype,sep="_")})
	
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
    my_barplot(imp.full, "vol MAF", monthly = TRUE, full = TRUE)
  })

  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$metric
    sizeBy <- input$metric
	##add if statement for  metric variables
	colorlist <-  c("black","orangered","khaki1","olivedrab1","chartreuse3","green4","aquamarine2","deepskyblue4","blue","royalblue4","navyblue")
	bounds <- c(0,1000,10000,50000,125000,200000,400000,800000,1500000,2500000,3500000)
	labs <-  c("0","1 AF - 1 TAF","1000 - 10000","10000 - 50000","50000 - 125000","125000 - 200000","200000 - 400000","400000 - 800000","800000 - 1500000","1500000 - 2500000","2500000 - 3500000")
#    if (colorBy == "avg") {
#      # Color and palette are treated specially in the "superzip" case, because
#      # the values are categorical instead of continuous.
#      colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
#      pal <- colorFactor("viridis", colorData)
#    } else {
      colorData <- zipdata[[colorBy]]
	  classdata <- rep(NA,length(colorData))
	  classdata[which(colorData== bounds[[1]])] <- 1
	  for(i in 2:length(bounds)){
		  classdata[which(colorData > bounds[[i-1]] & colorData <= bounds[[i]] )] <- i
	  }
#		levs <- factor(seq(1,11,1), levels=seq(1,11,1), labels=as.character(bounds))
      pal <- colorFactor(palette=colorlist, domain=seq(1,11,1), na.color="black")
#    }

#    if (sizeBy == "avg") {
#      # Radius is treated specially in the "superzip" case.
#      radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
#    } else {
     radius <- 10000*zipdata[[sizeBy]]/max(zipdata[[sizeBy]]) + 3000
#    }
library(gplots)
    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>% 
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
        stroke=TRUE, weight = 1, color ="#000000", fillOpacity=0.85, fillColor=pal(classdata)) %>%
      addLegend("bottomleft", values=seq(1,11,1), colors=col2hex(colorlist), title=colorBy,
        layerId="colorLegend", opacity=0.85, labels=labs)
	
  })

  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allzips[allzips$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Site Number:", as.integer(selectedZip$zipcode)),
      tags$br(),
      sprintf("Station Name: %s", selectedZip$statname), tags$br(),
      sprintf("Longitude: %s", selectedZip$longitude), tags$br(),
      sprintf("Latitude: %s", selectedZip$latitude), tags$br(),
      sprintf("Status: %s", selectedZip$status), tags$br(),
      sprintf("Average: %s", selectedZip$avg), tags$br()	    
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
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
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
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
