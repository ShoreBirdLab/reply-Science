rm( list = ls() )	

# set working and output directories
wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
source(paste(wd, 'Constants_Functions.R',sep=""))
source(paste(wd, 'Prepare_Data.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script

# FIGURE 1G loess
{# prepare
d$trans = ifelse(d$DPR_trans == 'YES',1,0)
d = d[order(d$mean_year),]
plx<-predict(loess(trans*100~mean_year,d), se=T)
lwr = (plx$fit - qt(0.975,plx$df)*plx$se)
upr = (plx$fit + qt(0.975,plx$df)*plx$se)
upr = ifelse(upr>100,100,upr)
		#ggplot(d,aes(x= mean_year, y = trans))+stat_smooth() + ylab('Beintam used (yes = 1, no = 0)')								
		#ggsave(paste(outdir,"Figure_1G.png", sep=""),width=1.85*2, height=1.85*2, units='in',dpi=600)
		#ggplot(d,aes(x= mean_year, y = trans, col = Belt))+stat_smooth(se = F) + ylab('Beintam used (yes = 1, no = 0)')	
# raw data
  d$int = ifelse(d$mean_year<1945, 1940, ifelse(d$mean_year<1955, 1950, ifelse(d$mean_year<1965, 1960, ifelse(d$mean_year<1975, 1970, ifelse(d$mean_year<1985,1980, ifelse(d$mean_year<1995, 1990, ifelse(d$mean_year<2005, 2000, 2010)))))))
  
  d$int5 = ifelse(d$mean_year<1943, 1940, ifelse(d$mean_year<1948, 1945,ifelse(d$mean_year<1953, 1950,ifelse(d$mean_year<1958, 1955, ifelse(d$mean_year<1963, 1960, ifelse(d$mean_year<1968, 1965, ifelse(d$mean_year<1973, 1970, ifelse(d$mean_year<1978, 1975, ifelse(d$mean_year<1983,1980, ifelse(d$mean_year<1988,1985, ifelse(d$mean_year<1993, 1990, ifelse(d$mean_year<1998, 1995, ifelse(d$mean_year<2003, 2000, ifelse(d$mean_year<2008, 2005,  ifelse(d$mean_year<2013, 2010, 2016)))))))))))))))
  
  d$n = 1
  ddr = ddply(d,('int'), summarise, med = median (trans),mea = mean(trans), n = sum(n))	
  ddr5 = ddply(d,('int5'), summarise, med = median (trans),mea = mean(trans), n = sum(n))	
}
{# plot
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_2A_loess.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,2,0.5,0.2), ps=12, mgp=c(1.2,0.15,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(d$trans ~ d$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
		axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
			mtext('Year',side=1,line=0.3, cex=0.6, las=1, col='grey30')
		axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Population estimates\ntransformed by Beintema',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		text(x = 2016, y =100, labels= expression(bold("G")), col='black', cex = 0.7, adj = 0)
	
		polygon(c(d$mean_year, rev(d$mean_year)), c(lwr, 
		  rev(upr)), border=NA, col = adjustcolor('black',alpha.f = 0.1), xpd = FALSE)#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(d$mean_year,plx$fit, col='black',lwd=1)
		
		#symbols((ddr$int),(ddr$mea)*100, circles=sqrt(ddr$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
		symbols((ddr5$int5),(ddr5$mea)*100, circles=sqrt(ddr5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
		
			if(PNG == TRUE) {dev.off()}
}
