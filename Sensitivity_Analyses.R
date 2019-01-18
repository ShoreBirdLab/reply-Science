rm( list = ls() )	
# set working and output directories
wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
		source(paste(wd, 'Constants_Functions.R',sep=""))
		source(paste(wd, 'Prepare_Data.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script

cols = brewer.pal(n = 9, name = "Blues")	
b$other_failed0 = ifelse(is.na(b$other_failed),0,b$other_failed)
{# all years
if(PNG == TRUE) {
		png(paste(outdir,"sensitivity_anal.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
plot(b$DPR~b$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.1),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0), lwd = 0.5)
			mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
axis(2, at=seq(0,0.1, by = 0.01), labels=seq(0,0.1, by = 0.01), lwd = 0.5)
			mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
text(x = c(1980,2000, 2006, 2010), y =0.1, labels= c('Beintema:','0.1','-','0.9'), col=c('darkgrey',cols[2], 'darkgrey', cols[9]), cex = 0.5, adj = 0)			
for(i in 1: length(seq(0.1,0.9, by = 0.1))){
 #i=5
 r = seq(0.1,0.9, by = 0.1)[i]
 b$expMB_r = (r*b$Incubation_days*b$predated/2)+(r*b$Incubation_days*(b$hatched+b$infertile))
 b$expMB_r = ifelse(grepl('Moit',b$'References.and.notes'),(r*b$Incubation_days*b$"Failed_together."/2)+(r*b$Incubation_days*(b$hatched+b$infertile)), b$expMB_r)
 b$expMB_r = ifelse(grepl('Favero',b$'References.and.notes'),(r*b$Incubation_days*b$"Failed_together."/2)+(r*b$Incubation_days*(b$hatched+b$infertile)), b$expMB_r)
 b$DPR_MB_r = b$predated/b$expMB_r
 b$DPR = ifelse(b$DPRtrans=="NO", b$DPR_orig, ifelse(b$mean_year<2000, b$DPR_MB_r, b$DPR_MB))
 
 m = lmer(log(DPR+0.01) ~ ln_N_nests + mean_year + (1|mean_year) +(1|Latitude)+(1|species),  data = b)  
 #plot(allEffects(m))
 #summary(glht(m))
 bsim <- sim(m, n.sim=nsim)  
 v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
 # values to predict for		
   newD=data.frame(ln_N_nests = mean(b$ln_N_nests),mean_year = seq(min(b$mean_year),max(b$mean_year), length.out=300))
 # exactly the model which was used has to be specified here 
   X <- model.matrix(~ ln_N_nests + mean_year,data=newD)	
 # calculate predicted values and creditability intervals
   newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
   predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
   for(j in 1:nsim) predmatrix[,j] <- X%*%bsim@fixef[j,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
	
		ppi = newD
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr)-0.01, 
		  rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor(cols[i],alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, exp(ppi$pred)-0.01, col=cols[i],lwd=1)
print(i)
	}
if(PNG == TRUE) {dev.off()}
}
{# >1970
if(PNG == TRUE) {
		png(paste(outdir,"sensitivity_anal_1970+.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
plot(b$DPR~b$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.1),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0), lwd = 0.5)
			mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
axis(2, at=seq(0,0.1, by = 0.01), labels=seq(0,0.1, by = 0.01), lwd = 0.5)
			mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
text(x = c(1980,2000, 2006, 2010), y =0.1, labels= c('Beintema:','0.1','-','0.9'), col=c('darkgrey',cols[2], 'darkgrey', cols[9]), cex = 0.5, adj = 0)
			
for(i in 1: length(seq(0.1,0.9, by = 0.1))){
 #i =5
 r = seq(0.1,0.9, by = 0.1)[i]
 b$expMB_r = (r*b$Incubation_days*b$predated/2)+(r*b$Incubation_days*(b$hatched+b$infertile))
 b$expMB_r = ifelse(grepl('Moit',b$'References.and.notes'),(r*b$Incubation_days*b$"Failed_together."/2)+(r*b$Incubation_days*(b$hatched+b$infertile)), b$expMB_r)
 b$expMB_r = ifelse(grepl('Favero',b$'References.and.notes'),(r*b$Incubation_days*b$"Failed_together."/2)+(r*b$Incubation_days*(b$hatched+b$infertile)), b$expMB_r)
 b$DPR_MB_r = b$predated/b$expMB_r
 b$DPR = ifelse(b$DPRtrans=="NO", b$DPR_orig, ifelse(b$mean_year<2000, b$DPR_MB_r, b$DPR_MB))
 
 m = lmer(log(DPR+0.01) ~ ln_N_nests + mean_year + (1|mean_year) +(1|Latitude)+(1|species),  data = b[b$mean_year>1970,])  
 #plot(allEffects(m))
 #summary(glht(m))
 
 bsim <- sim(m, n.sim=nsim)  
 v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
 # values to predict for		
   newD=data.frame(ln_N_nests = mean(b$ln_N_nests[b$mean_year>1970]),mean_year = seq(min(b$mean_year[b$mean_year>1970]),max(b$mean_year[b$mean_year>1970]), length.out=300))
 # exactly the model which was used has to be specified here 
   X <- model.matrix(~ ln_N_nests + mean_year,data=newD)	
 # calculate predicted values and creditability intervals
   newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
   predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
   for(j in 1:nsim) predmatrix[,j] <- X%*%bsim@fixef[j,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
	
		ppi = newD
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr)-0.01, 
		  rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor(cols[i],alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, exp(ppi$pred)-0.01, col=cols[i],lwd=1)
print(i)
	}
if(PNG == TRUE) {dev.off()}
}
{# <2000
if(PNG == TRUE) {
		png(paste(outdir,"sensitivity_anal_less2000.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
plot(b$DPR~b$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.1),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0), lwd = 0.5)
			mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
axis(2, at=seq(0,0.1, by = 0.01), labels=seq(0,0.1, by = 0.01), lwd = 0.5)
			mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
text(x = c(1980,2000, 2006, 2010), y =0.1, labels= c('Beintema:','0.1','-','0.9'), col=c('darkgrey',cols[2], 'darkgrey', cols[9]), cex = 0.5, adj = 0)
			
for(i in 1: length(seq(0.1,0.9, by = 0.1))){
 #i=6
 r = seq(0.1,0.9, by = 0.1)[i]
 b$expMB_r = (r*b$Incubation_days*b$predated/2)+(r*b$Incubation_days*(b$hatched+b$infertile))
 b$expMB_r = ifelse(grepl('Moit',b$'References.and.notes'),(r*b$Incubation_days*b$"Failed_together."/2)+(r*b$Incubation_days*(b$hatched+b$infertile)), b$expMB_r)
 b$expMB_r = ifelse(grepl('Favero',b$'References.and.notes'),(r*b$Incubation_days*b$"Failed_together."/2)+(r*b$Incubation_days*(b$hatched+b$infertile)), b$expMB_r)
 b$DPR_MB_r = b$predated/b$expMB_r
 b$DPR = ifelse(b$DPRtrans=="NO", b$DPR_orig, ifelse(b$mean_year<2000, b$DPR_MB_r, b$DPR_MB))
 
 m = lmer(log(DPR+0.01) ~ ln_N_nests + mean_year + (1|mean_year) +(1|Latitude)+(1|species),  data = b[b$mean_year<2000,])  
 #plot(allEffects(m))
 #summary(glht(m))
 bsim <- sim(m, n.sim=nsim)  
 v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
 # values to predict for		
   newD=data.frame(ln_N_nests = mean(b$ln_N_nests[b$mean_year<2000]),mean_year = seq(min(b$mean_year[b$mean_yea<2000]),max(b$mean_year[b$mean_year<2000]), length.out=300))
 # exactly the model which was used has to be specified here 
   X <- model.matrix(~ ln_N_nests + mean_year,data=newD)	
 # calculate predicted values and creditability intervals
   newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
   predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
   for(j in 1:nsim) predmatrix[,j] <- X%*%bsim@fixef[j,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
	
		ppi = newD
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr)-0.01, 
		  rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor(cols[i],alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, exp(ppi$pred)-0.01, col=cols[i],lwd=1)
print(i)
	}
if(PNG == TRUE) {dev.off()}		
}
# using only data with exposure from literature
bb =b[which(b$DPRtrans=='NO'),]
m = lmer(log(DPR_orig+0.01) ~ ln_N_nests + mean_year + (1|mean_year) +(1|Latitude)+(1|species),  data = bb)  
plot(allEffects(m))
summary(glht(m))

b[b$mean_year<1970,'References.and.notes']
b[b$mean_year<1970,'Locality']

{## all years - adjusting for all data (not juse <2000) # only with 0.3 starts to be significant
if(PNG == TRUE) {
		png(paste(outdir,"sensitivity_anal_all.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
plot(b$DPR_orig~b$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.1),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0), lwd = 0.5)
			mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
axis(2, at=seq(0,0.1, by = 0.01), labels=seq(0,0.1, by = 0.01), lwd = 0.5)
			mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
text(x = c(1980,2000, 2006, 2010), y =0.1, labels= c('Beintema:','0.1','-','0.9'), col=c('darkgrey',cols[2], 'darkgrey', cols[9]), cex = 0.5, adj = 0)			
for(i in 1: length(seq(0.1,0.9, by = 0.1))){
 #i=2
 r = seq(0.1,0.9, by = 0.1)[i]
 b$expMB_r = (r*b$Incubation_days*(b$predated+b$other_failed0)/2)+(r*b$Incubation_days*(b$hatched+b$infertile))
 b$expMB_r = ifelse(grepl('Moit',b$'References.and.notes'),(r*b$Incubation_days*b$"Failed_together."/2)+(r*b$Incubation_days*(b$hatched+b$infertile)), b$expMB_r)
 b$expMB_r = ifelse(grepl('Favero',b$'References.and.notes'),(r*b$Incubation_days*b$"Failed_together."/2)+(r*b$Incubation_days*(b$hatched+b$infertile)), b$expMB_r)
 b$DPR_MB_r = b$predated/b$expMB_r
 b$DPR = ifelse(b$DPRtrans=="NO", b$DPR_orig,  b$DPR_MB_r)
 
 m = lmer(log(DPR+0.01) ~ ln_N_nests + mean_year + (1|mean_year) +(1|Latitude)+(1|species),  data = b)  
 #plot(allEffects(m))
 #summary(glht(m))
 bsim <- sim(m, n.sim=nsim)  
 v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
 # values to predict for		
   newD=data.frame(ln_N_nests = mean(b$ln_N_nests),mean_year = seq(min(b$mean_year),max(b$mean_year), length.out=300))
 # exactly the model which was used has to be specified here 
   X <- model.matrix(~ ln_N_nests + mean_year,data=newD)	
 # calculate predicted values and creditability intervals
   newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
   predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
   for(j in 1:nsim) predmatrix[,j] <- X%*%bsim@fixef[j,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
	
		ppi = newD
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr)-0.01, 
		  rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor(cols[i],alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, exp(ppi$pred)-0.01, col=cols[i],lwd=1)
print(i)
	}
if(PNG == TRUE) {dev.off()}

}