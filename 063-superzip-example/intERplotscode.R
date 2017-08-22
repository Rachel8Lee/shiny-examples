# TODO: Add comment
# 
# Author: tiffnk
###############################################################################

##plot function
interplot <- function(gauges, monthly=TRUE, full=TRUE){
	if(monthly==FALSE){
		period2 <- c("December to February","November to April", "Hydrologic Year")
	}else{
		period2 <- c("November","December","January","February","March","April")	
	}
	gauge_select <- gauges
	
	if(full==TRUE){
		rec <- "Full Record of Data"
		fracywf.full.gauges <- fracywf.full %>% filter(gauge %in% gauge_select)
		fracywf.full.period <- fracywf.full.gauges %>% filter(period %in% period2)
		temp1 <- fracywf.full.period[,c("gauge","yeartype","period","num_zero", "station_nm")]
		temp1$var <- "num_zero"
		names(temp1)[which(names(temp1)=="num_zero")] <- "bar"
		temp2 <- fracywf.full.period[,c("gauge","yeartype","period","num_nonzero", "station_nm")]
		temp2$var <- "num_nonzero"
		names(temp2)[which(names(temp2)=="num_nonzero")] <- "bar"
		temp.full <- rbind.data.frame(temp2,temp1)
		addin <- data.frame(gauge=temp.full$gauge,yeartype=" ",period=temp.full$period, bar=0,var=temp.full$var, station_nm=temp.full$station_nm)
		temp.final <- rbind.data.frame(temp.full,addin)
		temp.final$var <- factor(temp.final$var,levels=c("num_zero","num_nonzero"), labels=c("years without HMF", "years with HMF"), ordered=TRUE)
		temp.final$yeartype <-factor(temp.final$yeartype, levels=c("C","D","BN","AN","W"," ","all"), ordered=TRUE)
		temp.final$period <- factor(temp.full$period,levels=period2, ordered=TRUE)
	}else{
		rec <- "Post Impariment Record of Data"
		fracywf.imp.gauges <- fracywf.imp %>% filter(gauge %in% gauge_select)
		fracywf.imp.period <- fracywf.imp.gauges %>% filter(period %in% period2)
		temp1 <- fracywf.imp.period[,c("gauge","yeartype","period","num_zero", "station_nm")]
		temp1$var <- "num_zero"
		names(temp1)[which(names(temp1)=="num_zero")] <- "bar"
		temp2 <- fracywf.imp.period[,c("gauge","yeartype","period","num_nonzero", "station_nm")]
		temp2$var <- "num_nonzero"
		names(temp2)[which(names(temp2)=="num_nonzero")] <- "bar"
		temp.imp <- rbind.data.frame(temp2,temp1)
		addin <- data.frame(gauge=temp.imp$gauge,yeartype=" ",period=temp.imp$period, bar=0,var=temp.imp$var, station_nm=temp.imp$station_nm)
		temp.final <- rbind.data.frame(temp.imp,addin)
		temp.final$var <- factor(temp.final$var,levels=c("num_zero","num_nonzero"), labels=c("years without HMF", "years with HMF"), ordered=TRUE)
		temp.final$yeartype <-factor(temp.final$yeartype, levels=c("C","D","BN","AN","W"," ","all"), ordered=TRUE)
		temp.final$period <- factor(temp.imp$period,levels=period2, ordered=TRUE)
	}
	numgauges <- length(as.character(unique(temp.final$station_nm)))
	gauges2 <- rep(NA, numgauges)
	for(i in 1:length(gauges2)){
		if(grepl(" A ",as.character(unique(temp.final$station_nm))[[i]])){
			gauges2[[i]] <-	strsplit(as.character(unique(temp.final$station_nm))[[i]]," A ")[[1]][[1]]
		}else if(grepl(" BL ",as.character(unique(temp.final$station_nm))[[i]])){
			gauges2[[i]] <-	strsplit(as.character(unique(temp.final$station_nm))[[i]]," BL ")[[1]][[1]]
		}else if(grepl(" NR ",as.character(unique(temp.final$station_nm))[[i]])){
			gauges2[[i]] <-	strsplit(as.character(unique(temp.final$station_nm))[[i]]," NR ")[[1]][[1]]
		}else{
			gauges2[[i]] <-	as.character(unique(temp.final$station_nm))[[i]]
		}
	}
	captext <- rep(NA, length(gauges2))
	for(i in 1:length(gauges2)){
		if(i == length(gauges2)){
			captext[[i]] <- paste("USGS ",as.character(unique(temp.final$gauge))[[i]], " on the ", gauges2[[i]],".",sep="")
			
		}else {
			captext[[i]] <- paste("USGS ",as.character(unique(temp.final$gauge))[[i]], " on the ", gauges2[[i]],",\n",sep="")
		}
	}
	captext2 <- paste(captext, collapse=" ")
	
	if(full==FALSE & monthly==FALSE){
		cap = paste("Average inter-annual frequency of HMF \nover the post-impairment record of data by period and year type\nfor ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	} else if(full==TRUE & monthly==FALSE){
		cap = paste("Average inter-annual frequency of HMF \nover the full record of data by period and year type\nfor ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(full==TRUE & monthly==TRUE){
		cap = paste("Average inter-annual frequency of HMF \nover the full record of data by month and year type\nfor ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}else if(full==FALSE & monthly==TRUE){
		cap = paste("Average inter-annual frequency of HMF \nover the post-impairment record of data by month and year type\nfor ", numgauges, " site(s) in the Central Valley:\n",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
	}
	temp.final$namelab <- paste0("USGS ", temp.final$gauge)
	
	p <- ggplot(data=temp.final) +geom_bar(aes(x=yeartype,y=bar,fill=var), stat="identity") + 
			facet_grid(namelab~period, scales="free_y")+
			scale_fill_manual(values=c("gray61","deepskyblue","purple"))+
			labs(title="Interannual Frequency", 
					subtitle=rec,
					x="\n Year Type", y="Number of Years \n",
					caption=cap)+
			theme(axis.text.x = element_text(color="black", size=rel(1)),
					axis.text.y = element_text(color="black", size=rel(1)),
					axis.title.x = element_text(color="black", size=rel(1.2)),
					axis.title.y = element_text(color="black", size=rel(1.2)),
					legend.text = element_text(color="black", size=rel(1)),
					strip.text = element_text(color="black", size=rel(1)),
					legend.key = element_rect(colour = 'black'),
					plot.title = element_text(hjust=0.5, size=rel(1.5)),
					plot.subtitle=element_text(hjust=0.5, size=rel(1)),
					legend.position = "bottom",
					plot.caption = element_text(size=rel(1)),
					legend.key.width = unit(0.03,"npc"),
					legend.key.height = unit(0.03,"npc"))+
			guides(fill=guide_legend(title=" ", reverse=TRUE))+
			scale_x_discrete(labels=c("C", "D", "BN", "AN","W"," ","All"), drop=FALSE)
	return(p)
}


