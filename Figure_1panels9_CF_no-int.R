# FIGURES info 
	#5.5 cm (2.25 inches or 1 column) or 12.0 cm (4.75 inches or 2 columns)
	#Symbols and lettering should be large enough to be legible after reduction [a reduced size of about 7 points (2 mm) high, and not smaller than 5 points].
rm( list = ls() )	

# set working and output directories
wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
source(paste(wd, 'Constants_Functions.R',sep=""))
source(paste(wd, 'Prepare_Data.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script
source(paste(wd, 'Prepare_predictions_Fig1_CF-no-int.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script
#img = readPNG(paste(outdir,'fig1aic_smaller.png',sep='')) # load AIC TABLE data
summary(factor(d$DPR_trans))
# plot
if(PNG == TRUE) {
   png(paste(outdir,"Figure_1panels9_CF_no-int.png", sep=""), width=3*1.6,height=3*1.45,units="in",res=600) 
	}else{dev.new(width=3*1.6,height=3*1.45)
  }
#par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.15,0), las=1,  tcl=-0.05,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)  
	
par(oma = c(1.05, 1.5, 0.55, 0.5),mgp=c(1.2,0.15,0), 
	ps=12,font.main = 1, las=1, lwd = 0.5, tcl=-0.05,
	cex=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5,
	col.lab="black", col.main="162", fg="grey45",
	bty="n",xpd=TRUE) #
	
layout(mat = matrix(c(1,2,3,4,
						  5,6,7,8,9),nrow = 3), 
		  widths = c(1.6,1.6,2), 
		  heights = c(1.6,1.6,1.6))
		  
layout.show(n=9)

# A interaction
  pp = dprA
  par(mar=c(0.1,0.5,0,0.1),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=FALSE)
   
  plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')#xaxs="i",yaxs="i")
  
  axis(1, at=c(1944,1960,1980,2000,2016),labels=NA,cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
 # axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
	#mtext('Year',side=1,line=0.3, cex=0.6, las=1, col='grey30')
  axis(2, at=seq(0,0.07, by = 0.01), labels=seq(0,0.07, by = 0.01), lwd = 0.5)
	mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col = 'black')
  text(x = 2016, y =0.07*0.98, labels= expression(bold("A")), col='black', cex = 0.7,  xpd = TRUE)
  #mtext(expression(bold("A")),col='black', cex = 0.7,  side = 2, line=-0.5, padj = -7.25)
		for(i in unique(pp$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr)-0.01, 
								rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, exp(ppi$pred)-0.01, col=ppi$line_col,lwd=1)
						}	
		text(x = c(1944), y =0.068, labels= c(col_$Belt[col_$Belt%in%c('Arctic')]), col=c(col_$line_col[col_$Belt%in%c('Arctic')]), cex = 0.5, adj = 0)
		text(x = c(1944), y =0.063, labels= col_$Belt[col_$Belt%in%c('North temperate')], col=col_$line_col[col_$Belt%in%c('North temperate')], cex = 0.5, adj = 0)
		text(x = c(1944), y =0.058, labels= col_$Belt[col_$Belt%in%c('North tropics')], col=col_$line_col[col_$Belt%in%c('North tropics')], cex = 0.5, adj = 0)
		text(x =  c(1944), y =0.053, labels= col_$Belt[col_$Belt%in%c('South tropics')], col=col_$line_col[col_$Belt%in%c('South tropics')], cex = 0.5, adj = 0)
		text(x =  c(1944), y =0.048, labels= col_$Belt[col_$Belt%in%c('South temperate')], col=col_$line_col[col_$Belt%in%c('South temperate')], cex = 0.5, adj = 0)

# A interaction
  pp = dprB
  par(mar=c(0.1,0.5,0,0.1),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=FALSE)
   
  plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')#xaxs="i",yaxs="i")
  
  axis(1, at=c(1944,1960,1980,2000,2016),labels=NA,cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
	#mtext('Year',side=1,line=0.2, cex=0.6, las=1, col = 'black')
  axis(2, at=seq(0,0.07, by = 0.01), labels=seq(0,0.07, by = 0.01), lwd = 0.5)
	mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col = 'black')
  text(x = 2016, y =0.07*0.98, labels= expression(bold("B")), col='black', cex = 0.7, xpd = TRUE)
  for(i in unique(pp$Belt)){
		#i ="Arctic"
		print(i)
		ppi = pp[pp$Belt==i,]
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr)-0.01, 
			rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, exp(ppi$pred)-0.01, col=ppi$line_col,lwd=1)
						}	

# A limited
  pp = dprC
  par(mar=c(0.1,0.5,0,0.1),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=FALSE)
   
  plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')#xaxs="i",yaxs="i")
  
  axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
	mtext('Year',side=1,line=0.2, cex=0.6, las=1, col = 'black')
  axis(2, at=seq(0,0.07, by = 0.01), labels=seq(0,0.07, by = 0.01), lwd = 0.5)
	mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col = 'black')
  text(x = 2016, y =0.07*0.98, labels= expression(bold("C")), col='black', cex = 0.7,  xpd = TRUE)
  for(i in unique(pp$Belt)){
		#i ="Arctic"
		print(i)
		ppi = pp[pp$Belt==i,]
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr)-0.01, 
			rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, exp(ppi$pred)-0.01, col=ppi$line_col,lwd=1)
						}	
						
# B period
  pp = dprAA
  par(mar=c(0.1,0.3,0,0.3),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=FALSE)
  
  plot(exp(pp$pred)-0.01~pp$Latitude, pch=19,xlim=c(-80,80), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
  
  axis(1, at = seq(-80,80, by = 20),labels=F,cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
  #axis(1, at = seq(-80,80, by = 40),labels=paste(seq(-80,80, by = 40),'°',sep=''),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
	#mtext('Latitude',side=1,line=0.3, cex=0.6, las=1, col='black')
  axis(2, at=seq(0,0.07, by = 0.01), labels=NA, lwd = 0.5)
	#mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
    #text(x = 80, y =100, labels= expression(bold("E")), col='black', cex = 0.7, adj = 0)
  text(x = 80, y =0.07*0.98, labels= expression(bold("D")), col='black', cex = 0.7, xpd = TRUE)
  #mtext(expression(bold("B")),col='black', cex = 0.7,  side = 2, line=-0.5, padj = -7.25)	
  for(i in unique(pp$period_orig)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$period_orig==i,]
							polygon(c(ppi$Latitude, rev(ppi$Latitude)), c(exp(ppi$lwr)-0.01, 
								rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$Latitude, exp(ppi$pred)-0.01, col=ppi$line_col,lwd=1)
						}	
	#text(x = c(-80), y =0.058, labels= 'Period:', col='grey30', cex = 0.5, adj = 0)
	text(x = c(-80), y =0.063, labels= 'before year 2000', col='#5b6ea3', cex = 0.5, adj = 0)		
	text(x = c(-80), y =0.068, labels= 'after year 2000', col='#f74b45', cex = 0.5, adj = 0)		
    
# B interaction
  pp = dprBB
  par(mar=c(0.1,0.3,0,0.3),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=FALSE)
  
  plot(exp(pp$pred)-0.01~pp$Latitude, pch=19,xlim=c(-80,80), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
  
  axis(1, at = seq(-80,80, by = 20),labels=F,cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
  #axis(1, at = seq(-80,80, by = 40),labels=paste(seq(-80,80, by = 40),'°',sep=''),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
	#mtext('Latitude',side=1,line=0.3, cex=0.6, las=1, col='black')
  axis(2, at=seq(0,0.07, by = 0.01), labels=NA, lwd = 0.5)
  text(x = 80, y =0.07*0.98, labels= expression(bold("E")), col='black', cex = 0.7, xpd = TRUE)
	#mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
    #text(x = 80, y =100, labels= expression(bold("E")), col='black', cex = 0.7, adj = 0)
  #mtext(expression(bold("B")),col='black', cex = 0.7,  side = 2, line=-0.5, padj = -7.25)	
  for(i in 1:length(unique(pp$mean_year))){
			year_ = c(unique(pp$mean_year))[i]
			text(x = c(-80), y =0.048+(i-1)*0.004846154, labels= year_, col=col_$year_col[col_$year_%in%year_], cex = 0.5, adj = 0)
			}
  for(i in unique(pp$mean_year)){
		#i ="Arctic"
		print(i)
		ppi = pp[pp$mean_year==i,]
		polygon(c(ppi$Latitude, rev(ppi$Latitude)), c(exp(ppi$lwr)-0.01, 
		rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$Latitude, exp(ppi$pred)-0.01, col=ppi$line_col,lwd=1)
		}	

# F interaction limited
  pp = dprCC
  par(mar=c(0.1,0.3,0,0.3),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=FALSE)
  
  plot(exp(pp$pred)-0.01~pp$Latitude, pch=19,xlim=c(-80,80), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
  
  axis(1, at = seq(-80,80, by = 20),labels=paste(seq(-80,80, by = 20),'°',sep=''),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
	mtext('Latitude',side=1,line=0.2, cex=0.6, las=1, col = 'black')
  #axis(1, at = seq(-80,80, by = 40),labels=paste(seq(-80,80, by = 40),'°',sep=''),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
	#mtext('Latitude',side=1,line=0.3, cex=0.6, las=1, col='black')
  axis(2, at=seq(0,0.07, by = 0.01), labels=NA, lwd = 0.5)
  text(x = 80, y =0.07*0.98, labels= expression(bold("F")), col='black', cex = 0.7, xpd = TRUE)
	#mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
    #text(x = 80, y =100, labels= expression(bold("E")), col='black', cex = 0.7, adj = 0)
  #mtext(expression(bold("B")),col='black', cex = 0.7,  side = 2, line=-0.5, padj = -7.25)
 
  #for(i in 1:length(unique(pp$mean_year))){
	#		year_ = c(unique(pp$mean_year))[i]
	#		text(x = c(-80), y =0.048+(i-1)*0.004846154, labels= year_, col=col_$year_col[col_$year_%in%year_], cex = 0.5, adj = 0)
	#		}
  for(i in unique(pp$mean_year)){
		#i ="Arctic"
		print(i)
		ppi = pp[pp$mean_year==i,]
		polygon(c(ppi$Latitude, rev(ppi$Latitude)), c(exp(ppi$lwr)-0.01, 
		rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$Latitude, exp(ppi$pred)-0.01, col=ppi$line_col,lwd=1)
		}	
	
# G Proportion of transformed
  par(mar=c(0.1,2.5,0,0),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=FALSE)
  
  plot(a$trans ~ a$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
  
  axis(1, at=c(1944,1960,1980,2000,2016),labels=F,cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
	#mtext('Year',side=1,line=0.3, cex=0.6, las=1, col='grey30')
  axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
  mtext('Population\nwith assumed exposure',side=2,line=1.1, cex=0.6, las=3, col='black')
  text(x = 2016, y =100*0.98, labels= expression(bold("G")), col='black', cex = 0.7, xpd = TRUE)
  #text(x = 2016, y =100, labels= expression(bold("G")), col='black', cex = 0.7, adj = 0)
	
  polygon(c(a$mean_year, rev(a$mean_year)), c(lwr, 
	  rev(upr)), border=NA, col = adjustcolor('black',alpha.f = 0.1), xpd = FALSE)#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
  lines(a$mean_year,plx$fit, col='black',lwd=1)
		
  #symbols((ddr$int),(ddr$mea)*100, circles=sqrt(ddr$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
  symbols((ddr5$int5),(ddr5$mea)*100, circles=sqrt(ddr5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
  #mtext(expression(bold("D")),col='black', cex = 0.7,  side = 2, line=-0.5, padj = -7.25)	

  
# H Simulation
  cols = brewer.pal(n = 9, name = "Spectral")	
  g = b[b$DPRtrans=='NO' | (b$DPRtrans=='YES' & b$mean_year>1999),]
  g$DPR = g$DPR_orig
  h = b[b$DPRtrans=='YES' & b$mean_year<2000,]
  
  par(mar=c(.1,2.5,0,0),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=FALSE)
  plot(g$DPR~g$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.16),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
  
  axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
	mtext('Year',side=1,line=0.2, cex=0.6, las=1, col = 'black')
  axis(2, at=seq(0,0.16, by = 0.02), labels=seq(0,0.16, by = 0.02), lwd = 0.5)
			mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='black')
  text(x = c(2003), y =0.157-(1)*0.06923077*0.157, labels= c('Observed'), col='grey18', cex = 0.5, adj = 1)
  text(x = c(2003), y =0.157-(2)*0.06923077*0.157, labels= c('nesting'), col='grey18', cex = 0.5, adj = 1)
  text(x = c(2003), y =0.157-(3)*0.06923077*0.157, labels= c('period'), col='grey18', cex = 0.5, adj = 1)
  #text(x = c(2005), y =0.157-(2)*0.06923077*0.157, labels= c('Observed\nnesting\nperiod'), col='grey18', cex = 0.5, adj = 1)
  for(i in 1:length(seq(0.1,0.9,by=0.1))){
			text(x = 2016, y =0.157-(i)*0.06923077*0.157, labels= paste(100*seq(0.1,0.9,by=0.1)[i],'%',sep=''), col=cols[i], cex = 0.5, adj = 1)
			}
  text(x = 2016, y =0.16*0.98, labels= expression(bold("H")), col='black', cex = 0.7, xpd = TRUE)		
for(i in 1: length(seq(0.1,0.9, by = 0.1))){
 #i=4
 r = seq(0.1,0.9, by = 0.1)[i]
 h$expMB = (r*h$Incubation_days*(h$other_failed +h$predated)/2)+(r*h$Incubation_days*(h$hatched+h$infertile))
 h$expMB = ifelse(grepl('Moit',h$'References.and.notes'),(r*h$Incubation_days*h$"Failed_together."/2)+(r*h$Incubation_days*(h$hatched+h$infertile)), h$expMB)
 h$expMB = ifelse(grepl('Favero',h$'References.and.notes'),(r*h$Incubation_days*h$"Failed_together."/2)+(r*h$Incubation_days*(h$hatched+h$infertile)), h$expMB)
 h$DPR = h$predated/h$expMB
 xx = rbind(g[,c('DPR','ln_N_nests','mean_year','species','site')],h[,c('DPR','ln_N_nests','mean_year','species','site')])
 xx$beintema = paste(100*r, '% of nesting observed', sep ='')
 
 m = lmer(log(DPR+0.01) ~ ln_N_nests + mean_year + (1|mean_year) +(1|species)+(1|site),  data = xx)  
 #plot(allEffects(m))
 #summary(glht(m))
 bsim <- sim(m, n.sim=nsim)  
 v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
 # values to predict for		
   newD=data.frame(ln_N_nests = mean(xx$ln_N_nests),mean_year = seq(min(xx$mean_year),max(xx$mean_year), length.out=300))
 # exactly the model which was used has to be specified here 
   X <- model.matrix(~ ln_N_nests + mean_year,data=newD)	
 # calculate predicted values and creditability intervals
   newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
   predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
   for(j in 1:nsim) predmatrix[,j] <- X%*%bsim@fixef[j,]
					newD$lwr <- exp(apply(predmatrix, 1, quantile, prob=0.025))-0.01
					newD$upr <- exp(apply(predmatrix, 1, quantile, prob=0.975))-0.01
					newD$pred <- exp(newD$pred)-0.01
		ppi = newD
		ppi$upr = ifelse(ppi$upr>0.16,0.16,ppi$upr)
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
		  rev(ppi$upr)), border=NA, col = adjustcolor(cols[i],alpha.f = 0.2), xpd = FALSE)#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, ppi$pred, col=cols[i],lwd=1)
print(i)
	}			

# J
 par(mar=c(0.1,2.5,1,0),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=FALSE)
 #par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
 plot(u$N_nests~u$DPR_orig, pch=19,xlim=c(0,0.2), ylim=c(0,0.8),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')  
  axis(1, at=seq(0,0.2,by = 0.05),labels=seq(0,0.20,by = 0.05),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
	mtext('True daily nest predation',side=1,line=0.2, cex=0.6, las=1, col = 'black')
  axis(2, at=seq(0,0.8, by = 0.2), labels=seq(0,0.8, by = 0.2), lwd = 0.5)
			mtext('Estimated daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='black')
  
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
abline(0,1, lwd = 1, lty = 3, col = 'black', xpd=FALSE)
text(x = 0.2, y =0.8*0.98, labels= expression(bold("J")), col='black', cex = 0.7, xpd = TRUE)	
 
if(PNG == TRUE) {dev.off()}

    
  					