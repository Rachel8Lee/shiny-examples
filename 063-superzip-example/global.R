library(dplyr)

bigset<- readRDS("data/superzip.rds")
allzips<-bigset[1:93,]

# read in site #, name, location
gauge_data <- read.csv("data/gauge_locations.csv")
gauge_status <- read.csv("data/gauge_classification.csv")
allzips$zipcode <- gauge_data$site_no
allzips$statname <- gauge_data$station_nm
allzips$latitude <- gauge_data$dec_lat_va
allzips$longitude <- gauge_data$dec_long_v


# zipdata <- reactive({paste(input$record, input$vars, input$yeartype, input$period, input$sitetype,sep="_")})
# subset data 
# FULL VOL
full_avg <- read.csv("data/redo_simp_data_full_vol_90.csv")
full_avg <- merge(full_avg, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
full_avg_all <- full_avg[which(full_avg$yeartype == "all"),]
full_avg_all_April <-full_avg_all[which(full_avg_all$period == "April"),]
allzips <- merge(allzips, full_avg_all_April, by.x="zipcode", by.y="gauge", all.y=TRUE)
allzips <- allzips[order(allzips$avg, decreasing = TRUE),]
#allzips$avg <- redo_full_vol_all_April$avg
#allzips$sd <- redo_full_vol_all_April$sd

# POST-IMP VOL
postimp_vol <- read.csv("data/redo_simp_data_imp_vol_90.csv")

allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=7, format="d", flag="0")
# try as.char
row.names(allzips) <- allzips$zipcode
allzips

cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income
  )
