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

# subset data 
# FULL VOL
redo_full_vol <- read.csv("data/redo_simp_data_full_vol_90.csv")
redo_full_vol <- merge(redo_full_vol, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
redo_full_vol_all <- redo_full_vol[which(redo_full_vol$yeartype == "all"),]
redo_full_vol_all_April <-redo_full_vol_all[which(redo_full_vol_all$period == "April"),]
allzips <- merge(allzips, redo_full_vol_all_April, by.x="zipcode", by.y="gauge", all.y=TRUE)
#allzips$avg <- redo_full_vol_all_April$avg
#allzips$sd <- redo_full_vol_all_April$sd

# POST-IMP VOL
redo_imp_vol <- read.csv("data/redo_simp_data_imp_vol_90.csv")

allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=7, format="d", flag="0")
# try as.char
row.names(allzips) <- allzips$zipcode

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
