library(dplyr)

bigset<- readRDS("data/superzip.rds")
allsites<-bigset[1:93,]

# read in site #, name, location
gauge_data <- read.csv("data/gauge_locations.csv")
gauge_status <- read.csv("data/gauge_classification.csv")
allsites$site_no <- gauge_data$site_no
allsites$statname <- gauge_data$station_nm
allsites$latitude <- gauge_data$dec_lat_va
allsites$longitude <- gauge_data$dec_long_v


# zipdata <- reactive({paste(input$record, input$vars, input$yeartype, input$period, input$sitetype,sep="_")})
# subset data 
# FULL VOL
full_avg <- read.csv("data/redo_simp_data_full_vol_90.csv")
full_avg <- merge(full_avg, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
full_avg_all <- full_avg[which(full_avg$yeartype == "all"),]
full_avg_all_April <-full_avg_all[which(full_avg_all$period == "April"),]
allsites <- merge(allsites, full_avg_all_April, by.x="zipcode", by.y="gauge", all.y=TRUE)
allsites <- allsites[order(allsites$avg, decreasing = TRUE),]


# POST-IMP VOL
postimp_vol <- read.csv("data/redo_simp_data_imp_vol_90.csv")

allsites$college <- allsites$college * 100
allsites$zipcode <- formatC(allsites$zipcode, width=7, format="d", flag="0")
# try as.char
row.names(allsites) <- allsites$zipcode

cleantable <- allsites %>%
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
