# read in site #, name, location
gauge_location <- read.csv("data/gauge_locations.csv")
gauge_location <- gauge_location[,2:length(gauge_location)]
gauge_status <- read.csv("data/gauge_classification.csv")
gauge_status<- gauge_status[,2:length(gauge_status)]
gauge_data <- merge(gauge_location, gauge_status, by.x = "site_no", by.y = "gauge", all.x = TRUE)

allsites <- data.frame(site_no = gauge_data$site_no, station_nm = gauge_data$station_nm, latitude = gauge_data$dec_lat_va, longitude = gauge_data$dec_long_v, status = gauge_data$status)

redo_full_vol <- read.csv("data/redo_simp_data_full_vol_90.csv")
redo_full_vol <- redo_full_vol[,2:length(redo_full_vol)]
redo_full_vol <- merge(redo_full_vol, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
redo_full_dur <- read.csv("data/simp_data_full_vol_90_duration.csv")
redo_full_dur <- redo_full_dur[,2:length(redo_full_dur)]
redo_full_dur <- merge(redo_full_dur, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
redo_full_nmpks <- read.csv("data/simp_data_full_vol_90_intraannual_frequency.csv")
redo_full_nmpks <- redo_full_nmpks[,2:length(redo_full_nmpks)]
redo_full_nmpks <- merge(redo_full_nmpks, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
fracywf.full <- read.csv("data/simp_data_full_vol_90_intERannual_frequency.csv")
fracywf.full[is.na(fracywf.full)] <- 0
fracywf.full$X <- NULL
fracywf.full <-  merge(fracywf.full, gauge_data, by.x="gauge", by.y="site_no", all.x=TRUE)
fracywfFG <- fracywf.full[c(1,2,3,5,6,8,13,14)]
colnames(fracywfFG)[4]<- "avg"
colnames(fracywfFG)[5]<- "sd"
fracywfFG$sd <-"NA"

redo_imp_vol <- read.csv("data/redo_simp_data_imp_vol_90.csv")
redo_imp_vol <- redo_imp_vol[,2:length(redo_imp_vol)]
redo_imp_vol <- merge(redo_imp_vol, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
redo_imp_dur <- read.csv("data/simp_data_imp_vol_90_duration.csv")
redo_imp_dur <- redo_imp_dur[,2:length(redo_imp_dur)]
redo_imp_dur <- merge(redo_imp_dur, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
redo_imp_nmpks <- read.csv("data/simp_data_imp_vol_90_intraannual_frequency.csv")
redo_imp_nmpks <- redo_imp_nmpks[,2:length(redo_imp_nmpks)]
redo_imp_nmpks <- merge(redo_imp_nmpks, gauge_status, by.x="gauge", by.y="gauge", all.x=TRUE)
fracywf.imp <- read.csv("data/simp_data_imp_vol_90_intERannual_frequency.csv")
fracywf.imp[is.na(fracywf.imp)] <- 0
fracywf.imp <-  merge(fracywf.imp, gauge_data, by.x="gauge", by.y="site_no", all.x=TRUE)
fracywf.imp$X <- NULL
fracywfIG <- fracywf.imp[c(1,2,3,5,6,8,13,14)]
colnames(fracywfIG)[4]<- "avg"
colnames(fracywfIG)[5]<- "sd"
fracywfIG$sd <-"NA"

vol.frame <- redo_full_vol
vol.frame[is.na(vol.frame)] <- 0 
dur.frame<- redo_full_dur
dur.frame[is.na(dur.frame)] <- 0 
nmpks.frame<- redo_full_nmpks
nmpks.frame[is.na(nmpks.frame)] <- 0 

imp.vol.frame<- redo_imp_vol
imp.vol.frame[is.na(imp.vol.frame)] <- 0 
imp.dur.frame<- redo_imp_dur
imp.dur.frame[is.na(imp.dur.frame)] <- 0 
imp.nmpks.frame<- redo_imp_nmpks
imp.nmpks.frame[is.na(imp.nmpks.frame)] <- 0 

fullG <- rbind.data.frame(dur.frame, vol.frame, nmpks.frame, fracywfFG)
fullG["tag"] <- "full" 
postG <- rbind.data.frame(imp.dur.frame, imp.vol.frame, imp.nmpks.frame, fracywfIG)
postG["tag"] <- "post-impairment"
alldatG <- rbind.data.frame(fullG, postG)
allsites <- merge(alldatG, gauge_location, by.x = "gauge", by.y = "site_no", all.x = TRUE)
colnames(allsites)[1] <- "site_no"
colnames(allsites)[12] <- "latitude"
colnames(allsites)[13] <- "longitude"

sites <- paste("USGS ", gauge_data$site_no, ", ", gauge_data$station_nm, sep="")
siteblank <- c(" ", sites)
includeallsites <- c("All Sites", sites)
