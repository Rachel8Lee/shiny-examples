library(dplyr)

# read in site #, name, location, status
gauge_locations <- read.csv("data/gauge_locations.csv")
gauge_status <- read.csv("data/gauge_classification.csv")
gauge_data <- merge(gauge_locations, gauge_status, by.x = "site_no", by.y = "gauge", all.x = TRUE)

allzips$site_no <- gauge_data$site_no
allzips$statname <- gauge_data$station_nm
allzips$latitude <- gauge_data$dec_lat_va
allzips$longitude <- gauge_data$dec_long_v

# FULL YEARS DATA 
# MAGNITUDE
full_vol <- read.csv("data/redo_simp_data_full_vol_90.csv")
full_vol <- merge(full_vol, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)

# DURATION
full_dur <- read.csv("data/simp_data_full_vol_90_duration.csv")

# INTRAANNUAL 
full_intra <- read.csv("data/simp_data_full_vol_90_intraannual_frequency.csv")

full_vol_all <- full_vol[which(full_vol$yeartype == "all"),]
full_vol_all_April <-full_vol_all[which(redo_full_vol_all$period == "April"),]
#allzips <- merge(allzips, full_vol_all_April, by.x="site_no", by.y="gauge", all.y=TRUE)

# POST-IMP DATA
# VOLUME
imp_vol <- read.csv("data/redo_simp_data_imp_vol_90.csv")

# DURATION
imp_dur <- read.csv("data/simp_data_full_vol_90_duration.csv")

# INTRA
imp_intra <- read.csv("data/simp_data_imp_vol_90_intraannual_frequency.csv")

allzips$site_no <- formatC(allzips$site_no, width=7, format="d", flag="0")
row.names(allzips) <- allzips$site_no
