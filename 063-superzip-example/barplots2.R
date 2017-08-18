# TODO: Add comment
# 
# Author: tiffnk
###############################################################################
gauge_select_plot <- function(gauges, full_rec=TRUE){
	six.gauges = gauges[which(gauges != " ")]
# bundle data
	blanks = data.frame(gauge = six.gauges, yeartype = " ", period = NA, avg = NA, 
			sd = NA, valtype = NA, basin=NA, status=NA)
	full = do.call(rbind.data.frame, list(dur.frame, vol.frame,
					nmpks.frame)) %>% rbind.data.frame(blanks)
	full["tag"] = "full" 
	post = do.call(rbind.data.frame, list(imp.dur.frame, imp.vol.frame,
					imp.nmpks.frame)) %>% rbind.data.frame(blanks)
	post["tag"] = "post-impairment"
	alldat = rbind.data.frame(full, post) %>% filter(gauge %in% six.gauges)  
# format levels
	alldat$yeartype <- factor(alldat$yeartype, levels = c("C", "D", "BN", "AN", 
					"W", " ", "all"))
	levels(alldat$yeartype) = c("Critical", "Dry",
			"Below Normal","Above Normal","Wet"," ","All")
	alldat$period <- factor(alldat$period, levels = c("November", "December", 
					"January", "February","March", "April", "December to February", 
					"November to April", "Hydrologic Year"))
# convert AF to cubic km
	alldat[alldat$valtype %in% "vol AF", "avg"] = 1e-6*
			alldat[alldat$valtype %in% "vol AF", "avg"]
	alldat[alldat$valtype %in% "vol AF", "sd"] = 1e-6*
			alldat[alldat$valtype %in% "vol AF", "sd"]  
	levels(alldat$valtype)[levels(alldat$valtype) %in% "vol AF"] = "vol MAF"
# add sd
	alldat$ymin <- alldat$avg - alldat$sd
	alldat$ymax <- alldat$avg + alldat$sd
	##add station name
	#stationname <- gauge_data
	stationname <- data.frame(site=gauge_data$site_no, station_name=gauge_data$station_nm)
	alldat <- merge(alldat,stationname,by.x="gauge",by.y="site")
	
	if(full_rec==TRUE){
		imp.full = alldat %>% filter(gauge %in% six.gauges, tag == "full")
		lab.full <- rep(NA, length(six.gauges))
		for(i in 1:length(six.gauges)){
			lab.full[[i]] <- paste("USGS ",six.gauges[[i]],"\n",as.character(alldat$station_name[which(alldat$gauge==six.gauges[[i]])[[1]]]),sep="")
#			lab.full[[i]] <- paste("USGS ",six.gauges[[i]],sep="")
			
		}
		names(lab.full) = paste(c(six.gauges))
		imp.full$station = imp.full$gauge
		imp.full$gauge = factor(imp.full$gauge, levels = names(lab.full))
		levels(imp.full$gauge) = lab.full
		d <- imp.full
	}else{
		imp.post = alldat %>% filter(gauge %in% six.gauges, tag == "post-impairment")
		lab.imp <- rep(NA, length(six.gauges))
		for(i in 1:length(six.gauges)){
			lab.imp[[i]] <- paste("USGS ",six.gauges[[i]],"\n",as.character(alldat$station_name[which(alldat$gauge==six.gauges[[i]])[[1]]]),sep="")
#			lab.imp[[i]] <- paste("USGS ",six.gauges[[i]],sep="")
		
	}
		names(lab.imp) = paste(c(six.gauges))
		imp.post$station = imp.post$gauge
		imp.post$gauge = factor(imp.post$gauge, levels = names(lab.imp))
		levels(imp.post$gauge) = lab.imp
		d <- imp.post
	}
	return(d)
}


### function to dynamically create plots for magnitude, duration, and intra-annual frequency
my_barplot = function(d, yvar, monthly = TRUE, full = TRUE){
	if(yvar == "vol MAF"){
		ylabel = "High Magnitude Flow Volume  (MAF)"
		tlabel = "Average Magnitude (Volume) Above 90th Percentile\n"
		allcolor = "chartreuse4"    
	} else if(yvar == "duration_days"){
		ylabel = "Number of Days Above 90th Percentile\n"
		tlabel = "Average Number Of Days Above 90th Percentile"
		allcolor = "magenta"
	} else if(yvar == "intraannual_frequency_numpeaks"){
		ylabel = "Number of Peaks Above 90th Percentile\n"
		tlabel = "Average Number Of Peaks Above 90th Percentile"
		allcolor = "turquoise2"  
	} else {
		stop('value of argument "yvar" not recognized.')
	}
	if(monthly){
		p = c("November", "December", "January", "February", "March", "April")
		plabel = "Monthly (November to April) By Year Type"
	} else {
		p = c("December to February", "November to April", "Hydrologic Year")
		plabel = "3-Month Period, 6-Month Period, Hydrologic Year By Year Type"
	}
	if(full){
		rlabel = "Full Record of Available Data, Zero-Deflated"
	} else {
		rlabel = "Post-Impairment Record of Available Data, Zero-Deflated"
	}
	if(yvar=="vol MAF" & full==TRUE & monthly==FALSE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],".",sep="")
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average magnitude (volume) of HMF over the full record of data by period and year typefor ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(yvar=="duration_days" & full==TRUE & monthly==FALSE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],".",sep="")
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average duration (days) of HMF over the full record of data by period and year type for ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(yvar=="intraannual_frequency_numpeaks" & full==TRUE & monthly==FALSE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],".",sep="")
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average intra-annual frequency (# of peaks) of HMF over the full record of data by period and year type for ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	} else 	if(yvar=="vol MAF" & full==FALSE & monthly==FALSE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],".",sep="")
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average magnitude (volume) of HMF over the post-impairment record of data by period and year type for ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(yvar=="duration_days" & full==FALSE & monthly==FALSE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],".",sep="")
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average duration (days) of HMF over the post-impairment record of data by period and year type for ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(yvar=="intraannual_frequency_numpeaks" & full==FALSE & monthly==FALSE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],".",sep="")
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average intra-annual frequency (# of peaks) of HMF over the post-impairment record of data by period and year type for ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	} else if(yvar=="vol MAF" & full==TRUE & monthly==TRUE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],".",sep="")
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average magnitude (volume) of HMF over the full record of data by month and year type for ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(yvar=="duration_days" & full==TRUE & monthly==TRUE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],".",sep="")
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average duration (days) of HMF over the full record of data by month and year type for ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(yvar=="intraannual_frequency_numpeaks" & full==TRUE & monthly==TRUE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],".",sep="")
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average intra-annual frequency (# of peaks) of HMF over the full record of data by month and year type for ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	} else 	if(yvar=="vol MAF" & full==FALSE & monthly==TRUE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],".",sep="")
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average magnitude (volume) of HMF over the post-impairment record of data by month and year type for ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(yvar=="duration_days" & full==FALSE & monthly==TRUE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],".",sep="")
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average duration (days) of HMF over the post-impairment record of data by month and year type for ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(yvar=="intraannual_frequency_numpeaks" & full==FALSE & monthly==TRUE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, numgauges)
		for(i in 1:length(gauges2)){
			if(grepl(" A ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(grepl(" BL ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(grepl(" NR ",as.character(unique(d$gauge))[[i]])){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],".",sep="")
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$station))[[i]], " on the ", gauges2[[i]],",\n",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average intra-annual frequency (# of peaks) of HMF over the post-impairment record of data by month and year type for ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}
	plottitle = eval(parse(text = paste0('ggtitle(expression(atop("', tlabel, 
							'", atop("', plabel, '", atop(italic("', rlabel, '"))))))')))
	subd = d[d$period %in% p & d$valtype %in% yvar,]
	ggplot(subd, aes(x = yeartype, fill = yeartype, y = avg))+ ylab(ylabel) +
			facet_grid(gauge ~ period, scales = "free_y") + xlab("\nYear Type") + 
			geom_bar(stat = "identity", color = "black") + plottitle +
			geom_errorbar(aes(ymin = avg,ymax = ymax), width = 0.3)  +
			scale_x_discrete(labels = c("C", "D", "BN", "AN", "W"," ", "All"), 
					drop = FALSE) +
			scale_fill_manual(NULL, 
					values = c("Critical" = "lightcoral", "Dry" = "lemonchiffon", 
							"Below Normal" = "mediumaquamarine", "Above Normal" = "dodgerblue3", 
							"Wet" = "darkblue", " " = "white", "All" = allcolor), 
					labels = c("Critical\t", "Dry\t", "Below Normal\t", "Above Normal\t", 
							"Wet\t", "All\t")) + 
			guides(fill = guide_legend(reverse = FALSE, nrow = 1)) +
			labs(caption=cap)+
			theme(
					axis.text.x = element_text(color="black", size=rel(1)),
					axis.text.y = element_text(color="black", size=rel(1)),
					axis.title.x = element_text(color="black", size=rel(1)),
					axis.title.y = element_text(color="black", size=rel(1)),
					title = element_text(color="black", size=rel(1)),
					legend.position = "bottom",
					legend.title = element_text(color="black", size=rel(1)),
					legend.text = element_text(color="black", size=rel(.8)),
					strip.text = element_text(color="black", size=rel(.8)),
					legend.key = element_rect(color = 'black'),
					plot.caption = element_text(size=rel(1))
			)
}
