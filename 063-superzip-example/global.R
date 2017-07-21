library(dplyr)

# read in site #, name, location
gauge_location <- read.csv("data/gauge_locations.csv")
gauge_status <- read.csv("data/gauge_classification.csv")
gauge_data <- merge(gauge_location, gauge_status, by.x = "site_no", by.y = "gauge", all.x = TRUE)

allsites <- data.frame(site_no = gauge_data$site_no, station_nm = gauge_data$station_nm, latitude = gauge_data$dec_lat_va, longitude = gauge_data$dec_long_v, status = gauge_data$status)

# zipdata <- reactive({paste(input$record, input$vars, input$yeartype, input$period, input$sitetype,sep="_")})
# subset data 
# FULL VOL
full_avg <- read.csv("data/redo_simp_data_full_vol_90.csv")
full_avg_all <- full_avg[which(full_avg$yeartype == "all"),]
full_avg_all_April <-full_avg_all[which(full_avg_all$period == "April"),]
allsites <- merge(allsites, full_avg_all_April, by.x="zipcode", by.y="gauge", all.y=TRUE)
allsites <- allsites[order(allsites$avg, decreasing = TRUE),]


# POST-IMP VOL
postimp_vol <- read.csv("data/redo_simp_data_imp_vol_90.csv")

row.names(allsites) <- allsites$site_no
