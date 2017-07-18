library(dplyr)

bigset<- readRDS("data/superzip.rds")
allzips<-bigset[1:93,]

# read in site #, name, location, status
gauge_data <- read.csv("data/gauge_locations.csv")
gauge_status <- read.csv("data/gauge_classification.csv")
allzips$site_no <- gauge_data$site_no
allzips$statname <- gauge_data$station_nm
allzips$latitude <- gauge_data$dec_lat_va
allzips$longitude <- gauge_data$dec_long_v

# FULL YEAR VOL
redo_full_vol <- read.csv("data/redo_simp_data_full_vol_90.csv")
redo_full_vol <- merge(redo_full_vol, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)


redo_full_vol_all <- redo_full_vol[which(redo_full_vol$yeartype == "all"),]
redo_full_vol_all_April <-redo_full_vol_all[which(redo_full_vol_all$period == "April"),]
allzips <- merge(allzips, redo_full_vol_all_April, by.x="site_no", by.y="gauge", all.y=TRUE)


# POST-IMP VOL
redo_imp_vol <- read.csv("data/redo_simp_data_imp_vol_90.csv")
redo_imp_vol <- merge(redo_full_vol, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)

# FULL YEAR DUR
simp_full_dur <- read.csv("data/simp_data_full_vol_90_duration.csv")

# POST IMP DUR
simp_imp_dur <- read.csv("data/simp_data_full_vol_90_duration.csv")

# FULL YEAR INTRA
simp_full_intra <- read.csv("data/simp_data_full_vol_90_intraannual_frequency.csv")
# POST IMP INTRA
simp_imp_intra <- read.csv("data/simp_data_imp_vol_90_intraannual_frequency.csv")

allzips$college <- allzips$college * 100
allzips$site_no <- formatC(allzips$site_no, width=7, format="d", flag="0")
# try as.char
row.names(allzips) <- allzips$site_no
