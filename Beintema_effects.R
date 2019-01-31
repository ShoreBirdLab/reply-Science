rm( list = ls() )	

# set working and output directories
wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
source(paste(wd, 'Constants_Functions.R',sep=""))
source(paste(wd, 'Prepare_Data.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script
#source(paste(wd, 'Prepare_predictions_Fig1AB.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script
#img = readPNG(paste(outdir,'fig1aic_smaller.png',sep='')) # load AIC TABLE data


u=b[b$DPRtrans=='NO',]
u=u[-which(is.na(u$obs_time)| is.na(u$other_failed)),]
u$hatched[u$N.nests == 86 & u$Locality == 'South Uist, Hebrides'] = 86-19
u$infertile[u$N.nests == 86 & u$Locality == 'South Uist, Hebrides'] = 0
u = u[order(u$DPR_orig),]	

# for supplement
l = list()
for(i in 1:9){
	r = seq(0.1,0.9, by =0.1)[i]
	u$expMB = (r*u$Incubation_days*(u$other_failed +u$predated)/2)+(r*u$Incubation_days*(u$hatched+u$infertile))
    u$DPR_MB = u$predated/u$expMB
	u$beintema = paste(r, 'of nesting observed')
	
	l[[i]] = u[,c('DPR_orig','DPR_MB','beintema')]
	print(r)
	}
uu = do.call(rbind,l)	


ggplot(uu,aes(x = DPR_orig, y = DPR_MB))+stat_smooth()+geom_point(alpha = 0.5)+geom_abline(intercept = 0, slope = 1, col = 'red', lty = 2)+	
		#geom_point(data = df2, aes(x = DPR_orig, y = DPR_MB), colour = "white")+
		facet_wrap(~beintema, ncol = 3,scales="free_y") + 
		ylab('Estimated daily predation rate') + xlab('True daily predation rate') 
		 #scale_x_continuous(trans = "log10")+scale_y_continuous(trans = "log10")

ggsave(paste(outdir,'Figure SB beintrema_effects.png'),width = 7, height = 6, dpi = 600)

# main text
 u = u[order(u$DPR_orig),]
 cols = brewer.pal(n = 9, name = "Spectral")	
if(PNG == TRUE) {
	png(paste(outdir,"Figure_Beintema.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		}else{dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
	}	
 par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
 plot(u$DPR_MB~u$DPR_orig, pch=19,xlim=c(0,0.25), ylim=c(0,0.8),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')  
  axis(1, at=seq(0,0.25,by = 0.05),labels=seq(0,0.25,by = 0.05),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
	mtext('Daily nest predation [literature]',side=1,line=0.2, cex=0.6, las=1, col = 'black')
  axis(2, at=seq(0,0.8, by = 0.2), labels=seq(0,0.8, by = 0.2), lwd = 0.5)
			mtext('Daily nest predation [computed]',side=2,line=1.1, cex=0.6, las=3, col='grey30')
  #text(x = 0.25, y =0.75, labels= c('Beintema:'), col='grey18', cex = 0.5, adj = 1)
  #for(i in 1:length(seq(0.1,0.9,by=0.1))){
	#		text(x = 0.25, y =0.75-(i)*0.06923077*0.75, labels= seq(0.1,0.9,by=0.1)[i], col=cols[i], cex = 0.5, adj = 1)
	#		}			

for(i in 1:9){
	r = seq(0.1,0.9, by =0.1)[i]
	u$expMB = (r*u$Incubation_days*(u$other_failed +u$predated)/2)+(r*u$Incubation_days*(u$hatched+u$infertile))
    u$DPR_MB = u$predated/u$expMB
	u$beintema = paste(r, 'of nesting observed')
	plx = predict(loess(DPR_MB~DPR_orig,u), se=T)
	lwr = (plx$fit - qt(0.975,plx$df)*plx$se)
	upr = (plx$fit + qt(0.975,plx$df)*plx$se)
	polygon(c(u$DPR_orig, rev(u$DPR_orig)), c(lwr, 
	  rev(upr)), border=NA, col = adjustcolor(cols[i],alpha.f = 0.2), xpd = FALSE)
    lines(u$DPR_orig,plx$fit, col=cols[i],lwd=1)
	
	print(r)
	}
abline(0,1, lwd = 0.5, lty = 3, col = 'darkgrey', xpd=FALSE)
if(PNG == TRUE) {dev.off()}

