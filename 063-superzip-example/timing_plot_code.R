# TODO: Add comment
# 
# Author: tiffnk
###############################################################################
###add to global####
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


### gauges should be whatever gauge is selected, if all=TRUE, it displays the all sites and it doesn't matter what is given to gauges.
timingplot <- function(gauges, full=TRUE, all=FALSE){

	gauge_select <- gauges
	
	if(full==TRUE & all==FALSE){
		rec <- "Full Record of Data"
		timing.final <- timing.full %>% filter(gauge %in% gauge_select)
		timing.final$yeartype <- factor(timing.final$yeartype, levels=c("C","D","BN","AN","W"," ","All"))
		timing.final$avg <- as.character(timing.final$avg)
		timing.final.month <- as.character(as.Date(timing.final$avg, format="%m-%d"), format="%m")
		for(i in 1:length(timing.final.month)){
			if(is.na(timing.final.month[[i]])){
				next
			}else if(timing.final.month[[i]]>=10){
				timing.final$avg[[i]] <- paste0(as.character(timing.final$avg[[i]]),"-2016")				
			}else if(timing.final.month[[i]]<10){
				timing.final$avg[[i]] <- paste0(as.character(timing.final$avg[[i]]),"-2017")
				
			}
		}
		timing.final$avg <- as.Date(timing.final$avg, format="%m-%d-%Y")
		timing.final$ymin <- timing.final$avg - timing.final$sd
		timing.final$ymax <- timing.final$avg + timing.final$sd
	}else if (full==FALSE & all==FALSE){
		rec <- "Post Impariment Record of Data"
		timing.final <- timing.imp %>% filter(gauge %in% gauge_select)
		timing.final$yeartype <- factor(timing.final$yeartype, levels=c("C","D","BN","AN","W"," ","All"))
		timing.final$avg <- as.character(timing.final$avg)
		timing.final.month <- as.character(as.Date(timing.final$avg, format="%m-%d"), format="%m")
		for(i in 1:length(timing.final.month)){
			if(is.na(timing.final.month[[i]])){
				next
			}else if(timing.final.month[[i]]>=10){
				timing.final$avg[[i]] <- paste0(as.character(timing.final$avg[[i]]),"-2016")				
			}else if(timing.final.month[[i]]<10){
				timing.final$avg[[i]] <- paste0(as.character(timing.final$avg[[i]]),"-2017")
				
			}
		}
		timing.final$avg <- as.Date(timing.final$avg, format="%m-%d-%Y")
		timing.final$ymin <- timing.final$avg - timing.final$sd
		timing.final$ymax <- timing.final$avg + timing.final$sd
	}
	if(all==TRUE & full==TRUE){
			rec <- "Full Record of Data"
			timing.final <- timing.full 
			timing.final <- timing.final[which(timing.final$yeartype!=" "),]
			timing.final$yeartype <- factor(timing.final$yeartype, levels=c("C","D","BN","AN","W"," ","All"), labels=c("Critical","Dry","Below Normal","Above Normal","Wet"," ","All"))
			timing.final$basin <- factor(timing.final$basin, levels=c("sac","sj"), labels=c("Sacramento Basin","San Joaquin-Tulare Basins"))
	
			timing.final$avg <- as.character(timing.final$avg)
			timing.final$month <- as.numeric(as.character(as.Date(timing.final$avg, format="%m-%d"), format="%m"))
			timing.final.month <- as.character(as.Date(timing.final$avg, format="%m-%d"), format="%m")
			for(i in 1:length(timing.final.month)){
				if(is.na(timing.final.month[[i]])){
					next
				}else if(timing.final.month[[i]]>=10){
					timing.final$avg[[i]] <- paste0(as.character(timing.final$avg[[i]]),"-2016")				
				}else if(timing.final.month[[i]]<10){
					timing.final$avg[[i]] <- paste0(as.character(timing.final$avg[[i]]),"-2017")
					
				}
			}
			timing.final$avg <- as.Date(timing.final$avg, format="%m-%d-%Y")
	}else if (all==TRUE & full==FALSE){
			rec <- "Post Impariment Record of Data"
			timing.final <- timing.imp 
			timing.final <- timing.final[which(timing.final$yeartype!=" "),]
			timing.final$yeartype <- factor(timing.final$yeartype, levels=c("C","D","BN","AN","W"," ","All"), labels=c("Critical","Dry","Below Normal","Above Normal","Wet"," ","All"))
			timing.final$basin <- factor(timing.final$basin, levels=c("sac","sj"), labels=c("Sacramento Basin","San Joaquin-Tulare Basins"))
			
			timing.final$avg <- as.character(timing.final$avg)
			timing.final$month <- as.numeric(as.character(as.Date(timing.final$avg, format="%m-%d"), format="%m"))
			timing.final.month <- as.character(as.Date(timing.final$avg, format="%m-%d"), format="%m")
			for(i in 1:length(timing.final.month)){
				if(is.na(timing.final.month[[i]])){
					next
				}else if(timing.final.month[[i]]>=10){
					timing.final$avg[[i]] <- paste0(as.character(timing.final$avg[[i]]),"-2016")				
				}else if(timing.final.month[[i]]<10){
					timing.final$avg[[i]] <- paste0(as.character(timing.final$avg[[i]]),"-2017")
					
				}
			}
			timing.final$avg <- as.Date(timing.final$avg, format="%m-%d-%Y")
	}
	
	if(all==FALSE){
		numgauges <- length(as.character(unique(timing.final$station_nm)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(timing.final$station_nm))[[i]])){
				gauges2[[i]] <-	strsplit(as.character(unique(timing.final$station_nm))[[i]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(timing.final$station_nm))[[i]])){
				gauges2[[i]] <-	strsplit(as.character(unique(timing.final$station_nm))[[i]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(timing.final$station_nm))[[i]])){
				gauges2[[i]] <-	strsplit(as.character(unique(timing.final$station_nm))[[i]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	as.character(unique(timing.final$station_nm))[[i]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(timing.final$gauge))[[i]], " on the ", gauges2[[i]],".",sep="")
				
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(timing.final$gauge))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
	}

	
	if(full==FALSE & all==FALSE){
		cap = paste("Average timing of HMF \nover the post-impairment record of data by year type\nfor ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(full==TRUE & all==FALSE){
		cap = paste("Average timing of HMF \nover the full record of data by year type\nfor ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(full==TRUE & all==TRUE){
		cap = paste("Average timing of HMF \nover the full record of data by year type\nfor all sites in the Central Valley.",
				"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(full==FALSE & all==TRUE){
		cap = paste("Average timing of HMF \nover the full record of data by year type\nfor all sites in the Central Valley.",
				"\n Source: Kocis & Dahlke 2017", sep="")
	}
	timing.final$namelab <- paste0("USGS ", timing.final$gauge)
	
	if(all==FALSE){
		p <- ggplot(data=timing.final)+geom_errorbar(aes(x=yeartype,ymin=ymin, ymax=ymax), color="black", width=.1, size=.2) +geom_point(aes(x=yeartype, y=avg, fill=yeartype),color="black",pch=21, size=rel(3))+
				facet_grid(namelab ~.)+
				scale_y_date(limits=c(as.Date("1-10-2016", format="%d-%m-%Y"),
								as.Date("30-9-2017", format="%d-%m-%Y")), date_breaks="1 month", date_labels="%b", minor_breaks=NULL)+ 
				scale_x_discrete(labels = c("C", "D", "BN", "AN", "W"," ", "All"), 
						drop = FALSE) +
				scale_fill_manual(NULL, breaks=c("C", "D", "BN", "AN", "W", "All"),
						values = c("C" = "lightcoral", "D" = "lemonchiffon", 
								"BN" = "mediumaquamarine", "AN" = "dodgerblue3", 
								"W" = "darkblue"," "= "white","All" = "maroon2"), 
						labels = c("Critical", "Dry", "Below Normal", "Above Normal", 
								"Wet","All year types"))+
				labs(title="Timing", 
						subtitle=rec,
						x="\n Year Type", y="Date of center of mass over 90th percentile\n",
						caption=cap)+
				theme_light()+
				theme(plot.title = element_text(hjust=0.5, size=rel(.9)),
						axis.text.x = element_text(color="black", size=rel(.6)),
						axis.text.y = element_text(color="black", size=rel(.6)),
						axis.title.x = element_text(color="black", size=rel(.8)),
						axis.title.y = element_text(color="black", size=rel(.8)),
						legend.position = "bottom",
						legend.title = element_text(color="black", size=rel(.6)),
						legend.text = element_text(color="black", size=rel(.6)),
						strip.text = element_text(color="black", size=rel(.6)),
						plot.subtitle = element_text(hjust=0.5, size=rel(.8)),
						plot.caption = element_text(size=rel(.5)))+
				guides(fill = guide_legend(reverse = FALSE, nrow = 1)) 	
	}else{
		p <- ggplot(data=timing.final) + geom_histogram(aes(x=avg, fill=yeartype), color="black", binwidth=14) + 
				facet_grid(basin ~ yeartype)+
				scale_x_date(limits = c(as.Date("2016-10-01", format="%Y-%m-%d"), 
								as.Date("2017-09-30",format="%Y-%m-%d")),
						date_labels="%b",date_minor_breaks="1 month") + 
				scale_y_continuous(limits=c(0,20))+
				scale_fill_manual(NULL,
						values = c("Critical" = "lightcoral", "Dry" = "lemonchiffon", 
								"Below Normal" = "mediumaquamarine", "Above Normal" = "dodgerblue3", 
								"Wet" = "darkblue","All" = "maroon2"), 
						labels = c("Critical", "Dry", "Below Normal", "Above Normal", 
								"Wet","All year types"))+
				labs(title="Timing", 
						subtitle=rec,
						x="Date", y="Count of Sites",
						caption=cap)+
				guides(fill = guide_legend(reverse = FALSE, nrow = 1)) +
				theme(plot.title = element_text(hjust=0.5, size=rel(.9)),
						axis.text.x = element_text(color="black", size=rel(.6)),
						axis.text.y = element_text(color="black", size=rel(.6)),
						axis.title.x = element_text(color="black", size=rel(.8)),
						axis.title.y = element_text(color="black", size=rel(.8)),
						legend.position = "bottom",
						legend.title = element_text(color="black", size=rel(.6)),
						legend.text = element_text(color="black", size=rel(.6)),
						strip.text = element_text(color="black", size=rel(.6)),
						plot.subtitle = element_text(hjust=0.5, size=rel(.8)),
						plot.caption = element_text(size=rel(.5)))
				
	
	}
		


	return(p)
}
