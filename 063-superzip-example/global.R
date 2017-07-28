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
full_magnitude_all_apr <-full_magnitude_all[which(full_magnitude_all$period == "April"),]

allsites <- merge(allsites, full_magnitude_all_apr, by.x="site_no", by.y="gauge", all.y=TRUE)
allsites <- allsites[order(allsites$avg, decreasing = TRUE),]
row.names(allsites) <- allsites$site_no

# TODO: Add comment
# 
# Author: tiffnk
###############################################################################


redo_full_vol <- read.csv("data/redo_simp_data_full_vol_90.csv")
redo_full_vol <- merge(redo_full_vol, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
redo_full_dur <- read.csv("data/simp_data_full_vol_90_duration.csv")
redo_full_dur <- merge(redo_full_vol, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
redo_full_nmpks <- read.csv("data/simp_data_full_vol_90_intraannual_frequency.csv")
redo_full_nmpks <- merge(redo_full_vol, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)

redo_imp_vol <- read.csv("data/redo_simp_data_imp_vol_90.csv")
redo_imp_vol <- merge(redo_imp_vol, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
redo_imp_dur <- read.csv("data/simp_data_imp_vol_90_duration.csv")
redo_imp_dur <- merge(redo_imp_vol, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
redo_imp_nmpks <- read.csv("data/simp_data_imp_vol_90_intraannual_frequency.csv")
redo_imp_nmpks <- merge(redo_imp_vol, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)

vol.frame <- redo_full_vol
vol.frame[is.na(vol.frame)] <- 0 
dur.frame<- redo_full_dur
dur.frame[is.na(dur.frame)] <- 0 
nmpks.frame<- redo_full_nmpks
nmpks.frame[is.na(nmpks.frame)] <- 0 
imp.vol.frame<- redo_full_vol
imp.vol.frame[is.na(imp.vol.frame)] <- 0 
imp.dur.frame<- redo_imp_dur
imp.dur.frame[is.na(imp.dur.frame)] <- 0 
imp.nmpks.frame<- redo_imp_nmpks
imp.nmpks.frame[is.na(imp.nmpks.frame)] <- 0 
vol.frame <- vol.frame[,2:length(vol.frame)]
dur.frame<- dur.frame[,2:length(dur.frame)]
nmpks.frame<- nmpks.frame[,2:length(nmpks.frame)]
imp.vol.frame<- imp.vol.frame[,2:length(imp.vol.frame)]
imp.dur.frame<- imp.dur.frame[,2:length(imp.dur.frame)]
imp.nmpks.frame<- imp.nmpks.frame[,2:length(imp.nmpks.frame)]
