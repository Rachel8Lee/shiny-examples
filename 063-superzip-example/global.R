library(dplyr)

# read in site #, name, location
gauge_location <- read.csv("data/gauge_locations.csv")
gauge_status <- read.csv("data/gauge_classification.csv")
gauge_data <- merge(gauge_location, gauge_status, by.x = "site_no", by.y = "gauge", all.x = TRUE)

allsites <- data.frame(site_no = gauge_data$site_no, station_nm = gauge_data$station_nm, latitude = gauge_data$dec_lat_va, longitude = gauge_data$dec_long_v, status = gauge_data$status)

# zipdata <- reactive({paste(input$record, input$vars, input$yeartype, input$period, input$sitetype,sep="_")})
# subset data 
# FULL VOL
full_magnitude <- read.csv("data/redo_simp_data_full_vol_90.csv")
full_magnitude_all <- full_magnitude[which(full_magnitude$yeartype == "all"),]
full_magnitude_all_apr <-full_magnitude_all[which(full_magnitude_all$period == "January"),]

imp_magnitude <- read.csv("data/redo_simp_data_imp_vol_90.csv")
imp_magnitude_all <- imp_magnitude[which(imp_magnitude$yeartype == "all"),]
imp_magnitude_all_apr <- imp_magnitude_all[which(imp_magnitude_all$period == "April"),]

allsites <- merge(allsites, full_magnitude_all_apr, by.x="site_no", by.y="gauge", all.y=TRUE)
allsites <- allsites[order(allsites$avg, decreasing = TRUE),]
row.names(allsites) <- allsites$site_no

# TODO: Add comment
# 
# Author: tiffnk
###############################################################################

