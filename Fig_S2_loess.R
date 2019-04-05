rm( list = ls() )	

# set working and output directories
wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
source(paste(wd, 'Constants_Functions.R',sep=""))
source(paste(wd, 'Prepare_Data.R',sep="")) # generates same warnings as Kubelke et al's script

d$trans = ifelse(d$DPR_trans == 'YES',1,0)
d = d[order(d$mean_year),] 
d2 = d
d2$Belt = 'ALL'
d = rbind(d2,d)

 d$int5 = ifelse(d$mean_year<1943, 1940, ifelse(d$mean_year<1948, 1945,ifelse(d$mean_year<1953, 1950,ifelse(d$mean_year<1958, 1955, ifelse(d$mean_year<1963, 1960, ifelse(d$mean_year<1968, 1965, ifelse(d$mean_year<1973, 1970, ifelse(d$mean_year<1978, 1975, ifelse(d$mean_year<1983,1980, ifelse(d$mean_year<1988,1985, ifelse(d$mean_year<1993, 1990, ifelse(d$mean_year<1998, 1995, ifelse(d$mean_year<2003, 2000, ifelse(d$mean_year<2008, 2005,  ifelse(d$mean_year<2013, 2010, 2016)))))))))))))))
  
  d$n = 1
  ddr5 = ddply(d,.(Belt,int5), summarise, trans = mean (trans), n = sum(n))	
  
ggplot(d,aes(x = mean_year, y = trans, col = Belt))+stat_smooth() + 
		geom_point(data = ddr5, mapping = aes(x = int5, y = trans, size = n)) +
		facet_wrap(~Belt, ncol = 6)+
		scale_y_continuous(name = "% of populations\nwith estimated exposure", breaks = c(0,0.25,0.5,0.75,1), labels = c(0,25,50,75,100))+
		coord_cartesian(xlim = c(1940, 2025),ylim = c(0,1)) +
		xlab("Year")+
		guides(col=FALSE)

ggsave(file=paste(outdir,'Fig_SA.png',sep=''),dpi = 600, width = 33, height = 7.5, units = 'cm')

ggplot(d,aes(x = mean_year, y = trans, col = Belt))+stat_smooth()+facet_wrap(~Belt, ncol = 6)  + ylab("% of populations\nwith estimated exposure")+xlab("Year")
ggsave(file=paste(outdir,'loess_Belt_trans.png',sep=''),dpi = 600)
  
# Arctic only
a = d[order(d$mean_year) & d$Belt == 'Arctic',]
a$trans = ifelse(a$DPR_trans == 'YES',1,0)
plx<-predict(loess(trans*100~mean_year,a), se=T)
a$fit = plx$fit
df_ =plx$df
a$lwr = (plx$fit - qt(0.975,df_)*plx$se)
a$upr = (plx$fit + qt(0.975,df_)*plx$se)
a$upr = ifelse(a$upr>100,100,a$upr)
		#ggplot(d,aes(x= mean_year, y = trans))+stat_smooth() + ylab('Beintam used (yes = 1, no = 0)')								
		#ggsave(paste(outdir,"Figure_1G.png", sep=""),width=1.85*2, height=1.85*2, units='in',dpi=600)
		#ggplot(d,aes(x= mean_year, y = trans, col = Belt))+stat_smooth(se = F) + ylab('Beintam used (yes = 1, no = 0)')	
  # raw data
  a$int = ifelse(a$mean_year<1945, 1940, ifelse(a$mean_year<1955, 1950, ifelse(a$mean_year<1965, 1960, ifelse(a$mean_year<1975, 1970, ifelse(a$mean_year<1985,1980, ifelse(a$mean_year<1995, 1990, ifelse(a$mean_year<2005, 2000, 2010)))))))
  
  a$int5 = ifelse(a$mean_year<1943, 1940, ifelse(a$mean_year<1948, 1945,ifelse(a$mean_year<1953, 1950,ifelse(a$mean_year<1958, 1955, ifelse(a$mean_year<1963, 1960, ifelse(a$mean_year<1968, 1965, ifelse(a$mean_year<1973, 1970, ifelse(a$mean_year<1978, 1975, ifelse(a$mean_year<1983,1980, ifelse(a$mean_year<1988,1985, ifelse(a$mean_year<1993, 1990, ifelse(a$mean_year<1998, 1995, ifelse(a$mean_year<2003, 2000, ifelse(a$mean_year<2008, 2005,  ifelse(a$mean_year<2013, 2010, 2016)))))))))))))))
  
  a$n = 1
  ddr = ddply(a,('int'), summarise, med = median (trans),mea = mean(trans), n = sum(n))	
  ddr5 = ddply(a,('int5'), summarise, med = median (trans),mea = mean(trans), n = sum(n))	
  ddr5$int5[ ddr5$int5 == 2016] = 2015	

  # plot
  if(PNG == TRUE) {
   png(paste(outdir,"loess_Arctic_trans.png", sep=""), width=1.45,height=1.45,units="in",res=600) 
	}else{dev.new(width=1.45,height=1.45)
  }
	par(mar=c(0.5,1.5,0,0),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=FALSE)
   par(oma = c(1.0, 1.5, 0.55, 0.25),mgp=c(1.2,0.15,0), 
	ps=12,font.main = 1, las=1, lwd = 0.5, tcl=-0.05,
	cex=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5,
	col.lab="black", col.main="162", fg="grey45",
	bty="n",xpd=TRUE)
  plot(a$trans ~ a$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
  
  axis(1, at=c(1944,1960,1980,2000,2016),labels=TRUE,cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
  mtext('Year',side=1,line=0.3, cex=0.6, las=1, col='grey30')
  axis(2, at=seq(0,100, by = 20), labels=seq(0,100, by = 20), lwd = 0.5)
  mtext('% of populations\nwith estimated exposure',side=2,line=1.1, cex=0.6, las=3, col='black')
  #text(x = 2016, y =100*0.98, labels= expression(bold("G")), col='black', cex = 0.7, xpd = TRUE)
  #text(x = 2016, y =100, labels= expression(bold("G")), col='black', cex = 0.7, adj = 0)
	
  polygon(c(a$mean_year, rev(a$mean_year)), c(a$lwr, 
	  rev(a$upr)), border=NA, col = adjustcolor('black',alpha.f = 0.1), xpd = TRUE)#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
  lines(a$mean_year,a$fit, col='black',lwd=1)
		
  #symbols((ddr$int),(ddr$mea)*100, circles=sqrt(ddr$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
  symbols((ddr5$int5),(ddr5$mea)*100, circles=sqrt(ddr5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
  #mtext(expression(bold("D")),col='black', cex = 0.7,  side = 2, line=-0.5, padj = -7.25)	
	
  text(x = c(1950), y =c(37), labels= substitute(paste(italic('N'), " populations:", sep="")), col='grey18', cex = 0.5, xpd = TRUE, adj = 0)
	#symbols(c(1950,1960,1970, 1900),c(20,20,20,-20), circles=sqrt(c(10, 20,30,33)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE)
  if(PNG == TRUE){dev.off()}
  symbols(c(1970,1970, 1970, 1900),c(29,19.5,7,-20), circles=sqrt(c(5, 15,30,33)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE)
		
	text(x = c(1970,1970, 1970, 1900)+9,c(29,19.5,7,-20), labels= c(5,15,30,10), col='grey18', cex = 0.5, xpd = TRUE, adj=c(0.5,0.5))  

