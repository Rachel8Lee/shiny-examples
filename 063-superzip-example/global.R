library(leaflet)
library(shinydashboard)

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
timing.fullcsv <- read.csv("data/redo_COM90_mag_date_sd_full.csv")
timing.full.all <- data.frame(gauge=timing.fullcsv$gauge, avg=timing.fullcsv$avg_DOHY, sd=timing.fullcsv$sd_DOHY, yeartype="All")
timing.full.C <- data.frame(gauge=timing.fullcsv$gauge, avg=timing.fullcsv$C_avg, sd=timing.fullcsv$C_sd, yeartype="C")
timing.full.D <- data.frame(gauge=timing.fullcsv$gauge, avg=timing.fullcsv$D_avg, sd=timing.fullcsv$D_sd, yeartype="D")
timing.full.BN <- data.frame(gauge=timing.fullcsv$gauge, avg=timing.fullcsv$BN_avg, sd=timing.fullcsv$BN_sd, yeartype="BN")
timing.full.AN <- data.frame(gauge=timing.fullcsv$gauge, avg=timing.fullcsv$AN_avg, sd=timing.fullcsv$AN_sd, yeartype="AN")
timing.full.W <- data.frame(gauge=timing.fullcsv$gauge, avg=timing.fullcsv$W_avg, sd=timing.fullcsv$W_sd, yeartype="W")
timing.full.blanks <- data.frame(gauge=timing.fullcsv$gauge, avg=NA, sd=NA, yeartype=" ")
timing.full <- rbind.data.frame(timing.full.all,timing.full.C,timing.full.D,
		timing.full.BN,timing.full.AN,timing.full.W, timing.full.blanks)
timing.full <-  merge(timing.full, gauge_data, by.x="gauge", by.y="site_no", all.x=TRUE)
timing.full$X <- NULL
timing.full$period <-"NA"
timing.full$valtype <- "timing"
timing.full$avg <- as.character(timing.full$avg)
month <- strsplit(timing.full$avg, "-")
for(i in 1:length(month)){timing.full$avg[i]<-as.numeric(month[[i]][1])}
time.frame <- timing.full[c(1,4,11,2,3,12,9,10)]

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
timing.impcsv <- read.csv("data/redo_COM90_mag_sd_date_imp_updated.csv")
timing.imp.all <- data.frame(gauge=timing.impcsv$gauge, avg=timing.impcsv$avg_DOHY, sd=timing.impcsv$sd_DOHY, yeartype="All")
timing.imp.C <- data.frame(gauge=timing.impcsv$gauge, avg=timing.impcsv$C_avg, sd=timing.impcsv$C_sd, yeartype="C")
timing.imp.D <- data.frame(gauge=timing.impcsv$gauge, avg=timing.impcsv$D_avg, sd=timing.impcsv$D_sd, yeartype="D")
timing.imp.BN <- data.frame(gauge=timing.impcsv$gauge, avg=timing.impcsv$BN_avg, sd=timing.impcsv$BN_sd, yeartype="BN")
timing.imp.AN <- data.frame(gauge=timing.impcsv$gauge, avg=timing.impcsv$AN_avg, sd=timing.impcsv$AN_sd, yeartype="AN")
timing.imp.W <- data.frame(gauge=timing.impcsv$gauge, avg=timing.impcsv$W_avg, sd=timing.impcsv$W_sd, yeartype="W")
timing.imp.blanks <- data.frame(gauge=timing.impcsv$gauge, avg=NA, sd=NA, yeartype=" ")
timing.imp <- rbind.data.frame(timing.imp.all,timing.imp.C,timing.imp.D,
		timing.imp.BN,timing.imp.AN,timing.imp.W, timing.imp.blanks)
timing.imp <-  merge(timing.imp, gauge_data, by.x="gauge", by.y="site_no", all.x=TRUE)
timing.imp$X <- NULL
timing.imp.blanks <- data.frame(gauge=timing.impcsv$gauge, avg=NA, sd=NA, yeartype=" ", agency_cd=timing.imp$agency_cd,
		station_nm=timing.imp$station_nm,dec_lat_va=timing.imp$dec_lat_va, dec_long_v=timing.imp$dec_long_v, 
		basin=timing.imp$basin, status=timing.imp$status)
timing.imp$period <-"NA"
timing.imp$valtype <- "timing"
timing.imp$avg <- as.character(timing.imp$avg)
month <- strsplit(timing.imp$avg, "-")
for(i in 1:length(month)){timing.imp$avg[i]<-as.numeric(month[[i]][1])}
imp.time.frame <- timing.imp[c(1,4,11,2,3,12,9,10)]

vol.frame <- redo_full_vol
vol.frame[is.na(vol.frame)] <- 0 
dur.frame<- redo_full_dur
dur.frame[is.na(dur.frame)] <- 0 
nmpks.frame <- redo_full_nmpks
nmpks.frame[is.na(nmpks.frame)] <- 0 
#time.frame[is.na(time.frame)] <- 0

imp.vol.frame<- redo_imp_vol
imp.vol.frame[is.na(imp.vol.frame)] <- 0 
imp.dur.frame<- redo_imp_dur
imp.dur.frame[is.na(imp.dur.frame)] <- 0 
imp.nmpks.frame<- redo_imp_nmpks
imp.nmpks.frame[is.na(imp.nmpks.frame)] <- 0 
#imp.time.frame[is.na(imp.time.frame)] <- 0

fullG <- rbind.data.frame(dur.frame, vol.frame, nmpks.frame, fracywfFG, time.frame)
fullG["tag"] <- "full" 
postG <- rbind.data.frame(imp.dur.frame, imp.vol.frame, imp.nmpks.frame, fracywfIG, imp.time.frame)
postG["tag"] <- "post-impairment"
alldatG <- rbind.data.frame(fullG, postG)
allsites <- merge(alldatG, gauge_location, by.x = "gauge", by.y = "site_no", all.x = TRUE)
colnames(allsites)[1] <- "site_no"
colnames(allsites)[12] <- "latitude"
colnames(allsites)[13] <- "longitude"

sites <- paste("USGS ", gauge_data$site_no, ", ", gauge_data$station_nm, sep="")
siteblank <- c(" ", sites)
includeallsites <- c("All Sites", sites)
