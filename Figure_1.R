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

# FIGURE 1A - TPR poly
{# predictions
     dd =d
		m <- lmer(TPR ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i, stringsAsFactors=FALSE)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2)*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]
		
	m <- lmer(TPR ~ ln_N_nests + poly(mean_year,2) + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(dd$ln_N_nests),mean_year = seq(min(dd$mean_year),max(dd$mean_year), length.out=300))
		
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2),data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
}
{# plot
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_1A_TPR_mean.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
		axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
			mtext('Year',side=1,line=0.3, cex=0.6, las=1, col='grey30')
		axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Total predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		text(x = 2016, y =100, labels= expression(bold("A")), col='black', cex = 0.7, adj = 0)
		for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(100*(ppi$lwr), 
								rev(100*(ppi$upr))), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, 100*(ppi$pred), col=ppi$line_col,lwd=1)
						}	
		ppi = ps
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(100*(ppi$lwr), 
		  rev(100*(ppi$upr))), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, 100*(ppi$pred), col='black',lwd=1)
		
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
		text(x = c(1944,1980), y =100, labels= c('General',col_$Belt[col_$Belt%in%c('Arctic')]), col=c('black',col_$line_col[col_$Belt%in%c('Arctic')]), cex = 0.5, adj = 0)
		text(x = c(1944,1980), y =90, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
		text(x =  c(1944,1980), y =80, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}

{# plot - without general
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_1b_all_orig_poly.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
			axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0), lwd = 0.5)
				mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
			axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Total predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(100*(ppi$lwr), 
								rev(100*(ppi$upr))), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, 100*(ppi$pred), col=ppi$line_col,lwd=1)
						}	
						
		text(x = c(1944), y =100, labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
		text(x = c(1944,1980), y =90, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
		text(x =  c(1944,1980), y =80, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}

# FIGURE 1B - TPR 
{# predictions
     dd =d
		m <- lmer(TPR ~ ln_N_nests + mean_year+Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i, stringsAsFactors=FALSE)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + mean_year+Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]
		
	m <- lmer(TPR ~ ln_N_nests + mean_year + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(dd$ln_N_nests),mean_year = seq(min(dd$mean_year),max(dd$mean_year), length.out=300))
		
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + mean_year,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
}
{# plot
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_1B_TPR_mean.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
		axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
			mtext('Year',side=1,line=0.6, cex=0.3, las=1, col='grey30')
		axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Total predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		text(x = 2016, y =100, labels= expression(bold("B")), col='black', cex = 0.7, adj = 0)
		for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(100*(ppi$lwr), 
								rev(100*(ppi$upr))), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, 100*(ppi$pred), col=ppi$line_col,lwd=1)
						}	
		ppi = ps
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(100*(ppi$lwr), 
		  rev(100*(ppi$upr))), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, 100*(ppi$pred), col='black',lwd=1)
		
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
		text(x = c(1944,1980), y =100, labels= c('General',col_$Belt[col_$Belt%in%c('Arctic')]), col=c('black',col_$line_col[col_$Belt%in%c('Arctic')]), cex = 0.5, adj = 0)
		text(x = c(1944,1980), y =90, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
		text(x =  c(1944,1980), y =80, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}

# FIGURE 1C - TPR limited 
 {# predictions
     dd =d[d$mean_year>2000,]
		m <- lmer(TPR ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i, stringsAsFactors=FALSE)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2)*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]
		
}
{# plot
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_1C_TPR_2000.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(2000,2016), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
		axis(1, at=c(2000,2005,2010,2015),labels=c(2000,2005,2010,2015),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
			mtext('Year',side=1,line=0.3, cex=0.6, las=1, col='grey30')
		axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Total predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		text(x = 2016, y =100, labels= expression(bold("C")), col='black', cex = 0.7, adj = 0)
		for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(100*(ppi$lwr), 
								rev(100*(ppi$upr))), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, 100*(ppi$pred), col=ppi$line_col,lwd=1)
						}	
		
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
		text(x = c(1944), y =100, labels= c(col_$Belt[col_$Belt%in%c('Arctic')]), col=c(col_$line_col[col_$Belt%in%c('Arctic')]), cex = 0.5, adj = 0)
		text(x = c(1944,1980), y =90, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
		text(x =  c(1944,1980), y =80, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}
	

# FIGURE 1D - TPR only OK data 
{# predictions
    dd =d[d$DPR_trans == 'NO',]
		summary(factor(dd$Belt))
	m = lmer(TPR ~ ln_N_nests + mean_year + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(dd$ln_N_nests),mean_year = seq(min(dd$mean_year),max(dd$mean_year), length.out=300))
		
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + mean_year,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
}
{# raw data
	dd$int = ifelse(dd$mean_year<1975, 1970, ifelse(dd$mean_year<1985, 1980, ifelse(dd$mean_year<1995, 1990, ifelse(dd$mean_year<2005, 2000, 2010))))
	dd$n = 1
	ddr = ddply(dd,('int'), summarise, med = median (TPR), n = sum(n))
}
{# plot
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_1D_TPR_noTransData.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.15,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
		axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
			mtext('Year',side=1,line=0.3, cex=0.6, las=1, col='grey30')
		axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Total predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		text(x = 2016, y =100, labels= expression(bold("D")), col='black', cex = 0.7, adj = 0)
		ppi = ps
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(100*(ppi$lwr), 
		  rev(100*(ppi$upr))), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, 100*(ppi$pred), col='black',lwd=1)
		
		symbols((ddr$int),(ddr$med)*100, circles=sqrt(ddr$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
			
			if(PNG == TRUE) {dev.off()}
}

# FIGURE 1Ddpr - DPR only OK data 
{# predictions
    dd =d[d$DPR_trans == 'NO',]
		
	m = lmer(TPR ~ ln_N_nests + mean_year + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(dd$ln_N_nests),mean_year = seq(min(dd$mean_year),max(dd$mean_year), length.out=300))
		
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + mean_year,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
}
{# raw data
	dd$int = ifelse(dd$mean_year<1975, 1970, ifelse(dd$mean_year<1985, 1980, ifelse(dd$mean_year<1995, 1990, ifelse(dd$mean_year<2005, 2000, 2010))))
	dd$n = 1
	ddr = ddply(dd,('int'), summarise, med = median (TPR), n = sum(n))
}
{# plot
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_1D_TPR_noTransData.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.15,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
		axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
			mtext('Year',side=1,line=0.3, cex=0.6, las=1, col='grey30')
		axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Total predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		text(x = 2016, y =100, labels= expression(bold("D")), col='black', cex = 0.7, adj = 0)
		ppi = ps
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(100*(ppi$lwr), 
		  rev(100*(ppi$upr))), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, 100*(ppi$pred), col='black',lwd=1)
		
		symbols((ddr$int),(ddr$med)*100, circles=sqrt(ddr$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
			
			if(PNG == TRUE) {dev.off()}
}


# FIGURE 1E latitude TPR
  {# predictions
     dd =d
		m <- lmer(TPR ~ ln_N_nests + poly(Latitude,3)*scale(mean_year) + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in c(1970,1980,1990,2000,2010)){
		 #i ="South temperate"
	  	 print(i)
		  l[[i]]=data.frame(ln_N_nests = mean(d$ln_N_nests),mean_year= i, Latitude = seq(min(d$Latitude[d$mean_year>i-6 & d$mean_year<i+5]),max(d$Latitude[d$mean_year>i-6 & d$mean_year<i+5]), length.out=300),stringsAsFactors = FALSE)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(Latitude,3)*scale(mean_year),data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$year_col[match(pp$mean_year, col_$year_)]
			pp$poly_col = col_$year_col[match(pp$mean_year, col_$year_)]
	
	dd$mean_year_s=scale(dd$mean_year)
	m <- lmer(TPR ~ ln_N_nests + poly(Latitude,3)+mean_year_s + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(d$ln_N_nests),Latitude = seq(min(dd$Latitude),max(dd$Latitude), length.out=300),mean_year_s= mean(dd$mean_year_s))
	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(Latitude,3)+mean_year_s,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
	
}
  {# plot 
	general = FALSE
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_4E_TPR_col.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.15,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(pp$pred~pp$Latitude, pch=19,xlim=c(-80,80), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
		axis(1, at = seq(-80,80, by = 20),labels=paste(seq(-80,80, by = 20),'°',sep=''),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
				mtext('Latitude',side=1,line=0.3, cex=0.6, las=1, col='grey30')
		axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Total predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		text(x = 80, y =100, labels= expression(bold("E")), col='black', cex = 0.7, adj = 0)
		
		if(general == TRUE){
		ppi = ps
		polygon(c(ppi$Latitude, rev(ppi$Latitude)), c(100*(ppi$lwr), 
		  rev(100*(ppi$upr))), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$Latitude, 100*(ppi$pred), col='black',lwd=1)	
		
		for(i in 1:length(unique(pp$mean_year))+1){
			year_ = c(unique(pp$mean_year),'General')[i]
			if(year_=='General'){
			text(x = c(2), y =65+(i-1)*7, labels= year_, col='black', cex = 0.5, adj = 0)
			}else{
			text(x = c(2), y =65+(i-1)*7, labels= year_, col=col_$line_col[col_$year_%in%year_], cex = 0.5, adj = 0)
			}
			}	
			}else{
			for(i in 1:length(unique(pp$mean_year))){
			year_ = c(unique(pp$mean_year))[i]
			text(x = -80, y =65+(i-1)*7, labels= year_, col=col_$year_col[col_$year_%in%year_], cex = 0.5, adj = 0)
			}
			}
			
		for(i in unique(pp$mean_year)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$mean_year==i,]
							polygon(c(ppi$Latitude, rev(ppi$Latitude)), c(100*(ppi$lwr), 
								rev(100*(ppi$upr))), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$Latitude, 100*(ppi$pred), col=ppi$line_col,lwd=1)
						}	
		
		
		
			if(PNG == TRUE) {dev.off()}
}

# FIGURE 1E latitude DPR
  {# predictions
     dd =d
		m <- lmer(log(DPR) ~ ln_N_nests + poly(Latitude,3)*scale(mean_year) + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in c(1970,1980,1990,2000,2010)){
		 #i ="South temperate"
	  	 print(i)
		  l[[i]]=data.frame(ln_N_nests = mean(d$ln_N_nests),mean_year= i, Latitude = seq(min(d$Latitude[d$mean_year>i-6 & d$mean_year<i+5]),max(d$Latitude[d$mean_year>i-6 & d$mean_year<i+5]), length.out=300),stringsAsFactors = FALSE)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(Latitude,3)*scale(mean_year),data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$year_col[match(pp$mean_year, col_$year_)]
			pp$poly_col = col_$year_col[match(pp$mean_year, col_$year_)]
	
	dd$mean_year_s=scale(dd$mean_year)
	m <- lmer(log(DPR) ~ ln_N_nests + poly(Latitude,3)+mean_year_s + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(d$ln_N_nests),Latitude = seq(min(dd$Latitude),max(dd$Latitude), length.out=300),mean_year_s= mean(dd$mean_year_s))
	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(Latitude,3)+mean_year_s,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
	
}
  {# plot 
	general = FALSE
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_4E_DPR_col2.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.15,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)

		plot(exp(pp$pred)-0.01~pp$Latitude, pch=19,xlim=c(-80,80), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
		axis(1, at = seq(-80,80, by = 20),labels=F,cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
		axis(1, at = seq(-80,80, by = 40),labels=paste(seq(-80,80, by = 40),'°',sep=''),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
				mtext('Latitude',side=1,line=0.3, cex=0.6, las=1, col='grey30')
		axis(2, at=seq(0,0.07, by = 0.01), labels=seq(0,0.07, by = 0.01), lwd = 0.5)
				mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		text(x = 80, y =100, labels= expression(bold("E")), col='black', cex = 0.7, adj = 0)
		
		if(general == TRUE){
		ppi = ps
		polygon(c(ppi$Latitude, rev(ppi$Latitude)), c(exp(ppi$lwr)-0.01, 
		  rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$Latitude, exp(ppi$pred)-0.01, col='black',lwd=1)	
		
		for(i in 1:length(unique(pp$mean_year))+1){
			year_ = c(unique(pp$mean_year),'General')[i]
			if(year_=='General'){
			text(x = c(-80), y =0.045+(i-1)*0.004846154, labels= year_, col='black', cex = 0.5, adj = 0)
			}else{
			text(x = c(-80), y =0.045+(i-1)*0.004846154, labels= year_, col=col_$year_col[col_$year_%in%year_], cex = 0.5, adj = 0)
			}
			}	
			}else{
			for(i in 1:length(unique(pp$mean_year))){
			year_ = c(unique(pp$mean_year))[i]
			text(x = c(-80), y =0.045+(i-1)*0.004846154, labels= year_, col=col_$year_col[col_$year_%in%year_], cex = 0.5, adj = 0)
			}
			}
			
		for(i in unique(pp$mean_year)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$mean_year==i,]
							polygon(c(ppi$Latitude, rev(ppi$Latitude)), c(exp(ppi$lwr)-0.01, 
								rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$Latitude, exp(ppi$pred)-0.01, col=ppi$line_col,lwd=1)
						}	
		
		
		
			if(PNG == TRUE) {dev.off()}
}


# FIGURE 1F latitude
  {# predictions
     dd =d
	dd$mean_year_s=scale(dd$mean_year)
	m <- lmer(TPR ~ ln_N_nests + poly(lat_abs,3)+mean_year_s + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(d$ln_N_nests),lat_abs = seq(min(dd$lat_abs),max(dd$lat_abs), length.out=300),mean_year_s= mean(dd$mean_year_s))
	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(lat_abs,3)+mean_year_s,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
	
}
  {# plot 
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_4F_TPR_best.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.15,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(ps$pred~ps$lat_abs, pch=19,xlim=c(0,80), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
		axis(1, at = seq(0,80, by = 20),labels=paste(seq(0,80, by = 20),'°',sep=''),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
				mtext('Latitude',side=1,line=0.3, cex=0.6, las=1, col='grey30')
		axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Total predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		text(x = 80, y =100, labels= expression(bold("F")), col='black', cex = 0.7, adj = 0)
		
		
		ppi = ps
		polygon(c(ppi$lat_abs, rev(ppi$lat_abs)), c(100*(ppi$lwr), 
		  rev(100*(ppi$upr))), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$lat_abs, 100*(ppi$pred), col='black',lwd=1)	
		
		if(PNG == TRUE) {dev.off()}
}
  {# plot - without general
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_2b_all_orig_poly.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(pp$pred~pp$lat_abs, pch=19,xlim=c(0,80), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
			axis(1, at = seq(0,80, by = 20),labels=paste(seq(0,80, by = 20),'°',sep=''),cex.axis=0.5,mgp=c(0,-0.15,0), lwd = 0.5)
				mtext('Latitude',side=1,line=0.6, cex=0.6, las=1, col='grey30')
			axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Total predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		for(i in unique(pp$mean_year)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$mean_year==i,]
							polygon(c(ppi$lat_abs, rev(ppi$lat_abs)), c(100*(ppi$lwr), 
								rev(100*(ppi$upr))), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$lat_abs, 100*(ppi$pred), col=ppi$line_col,lwd=1)
						}	
						
		for(i in 1:length(unique(pp$mean_year))){
			year_ = unique(pp$mean_year)[i]
			text(x = c(2), y =72+(i-1)*7, labels= year_, col=col_$line_col[col_$year_%in%year_], cex = 0.5, adj = 0)
			}
		
			if(PNG == TRUE) {dev.off()}
}

# FIGURE 1Falt latitude
  {# predictions
     dd =d
	dd$mean_year_s=scale(dd$mean_year)
	m <- lmer(TPR ~ ln_N_nests + hemisphere + lat_abs +mean_year_s + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newDs=data.frame(hemisphere = 'Southern',ln_N_nests = mean(d$ln_N_nests),lat_abs = seq(min(dd$lat_abs),max(dd$lat_abs), length.out=300),mean_year_s= mean(dd$mean_year_s), stringsAsFactors = FALSE)
		newDn=data.frame(hemisphere = 'Northern',ln_N_nests = mean(d$ln_N_nests),lat_abs = seq(min(dd$lat_abs),max(dd$lat_abs), length.out=300),mean_year_s= mean(dd$mean_year_s), stringsAsFactors = FALSE)
		newD = rbind(newDs, newDn)			
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + hemisphere + lat_abs +mean_year_s ,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
	
}
  {# plot 
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_4Falt_TPR_best.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.15,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(ps$pred~ps$lat_abs, pch=19,xlim=c(0,80), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
		axis(1, at = seq(0,80, by = 20),labels=paste(seq(0,80, by = 20),'°',sep=''),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
				mtext('Latitude',side=1,line=0.3, cex=0.6, las=1, col='grey30')
		axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Total predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		text(x = 80, y =100, labels= expression(bold("F")), col='black', cex = 0.7, adj = 0)
		
		ppi = ps[ps$hemisphere == 'Southern',]
		polygon(c(ppi$lat_abs, rev(ppi$lat_abs)), c(100*(ppi$lwr), 
		  rev(100*(ppi$upr))), border=NA, col = adjustcolor('orange',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$lat_abs, 100*(ppi$pred), col='orange',lwd=1)	
		
		ppi = ps[ps$hemisphere == 'Northern',]
		polygon(c(ppi$lat_abs, rev(ppi$lat_abs)), c(100*(ppi$lwr), 
		  rev(100*(ppi$upr))), border=NA, col = adjustcolor('slateblue2',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$lat_abs, 100*(ppi$pred), col='slateblue2',lwd=1)
		text(x = 0, y =100+(1-1)*7, labels= 'Northern hemisphere', col='slateblue2', cex = 0.5, adj = 0)
		text(x = 0, y =100-(2-1)*7, labels= 'Southern hemisphere', col='orange', cex = 0.5, adj = 0)
		
		if(PNG == TRUE) {dev.off()}
}
  			

# belt - simple
# all data poly + log(DPR)
  {# predictions
     dd =d
		m <- lmer(log(DPR) ~ ln_N_nests + mean_year+Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(as.character(dd$Belt))){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i, stringsAsFactors=FALSE)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + mean_year + Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]
		
	m <- lmer(log(DPR) ~ ln_N_nests + mean_year + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(dd$ln_N_nests),mean_year = seq(min(dd$mean_year),max(dd$mean_year), length.out=300))
		
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + mean_year,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
}
  {# on log 
		if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_all_log.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(pp$pred~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=log(c(0.006,1)),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
					axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0))
						mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
						#mtext('Tundra temperature [°C]',side=1,line=0.4, cex=0.6, las=1, col='grey30')
						
					axis(2, at=log(c(0.006,0.2,0.4,0.6,0.8,1)), labels=c(0,0.2,0.4,0.6,0.8,1))
						mtext('Daily nest predation',side=2,line=1, cex=0.6, las=3, col='grey30')
					
					
					
					# treated
					for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, ppi$pred, col=ppi$line_col,lwd=1)
						}	
						
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
						text(x = c(2000), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
						text(x = c(2000,2008), y =log(0.6), labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
						text(x = c(2000,2008), y =log(0.4), labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}
  {# back transformed
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_1a_all_simple_mean.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
			axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0), lwd = 0.5)
				mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
			axis(2, at=seq(0,0.07, by = 0.01), labels=seq(0,0.07, by = 0.01), lwd = 0.5)
				mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		for(i in unique(pp$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr), 
								rev(exp(ppi$upr))), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, exp(ppi$pred), col=ppi$line_col,lwd=1)
						}	
		ppi = ps
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr), 
		  rev(exp(ppi$upr))), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, exp(ppi$pred), col='black',lwd=1)
		
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
		text(x = c(1944,1980), y =0.07, labels= c('General',col_$Belt[col_$Belt%in%c('Arctic')]), col=c('black',col_$line_col[col_$Belt%in%c('Arctic')]), cex = 0.5, adj = 0)
		text(x = c(1944,1980), y =0.065, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
		text(x =  c(1944,1980), y =0.06, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}

	
# FIGURE 1Balt poly(Belt DPR) - COL correct
# all data poly + log(DPR)
  {# predictions
     dd =d
		m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i,stringsAsFactors=FALSE)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2)*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]
		
	m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,2) + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(dd$ln_N_nests),mean_year = seq(min(dd$mean_year),max(dd$mean_year), length.out=300))
		
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2),data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
}
  {# on log 
		if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_all_log.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(pp$pred~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=log(c(0.006,1)),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
					axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0))
						mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
						#mtext('Tundra temperature [°C]',side=1,line=0.4, cex=0.6, las=1, col='grey30')
						
					axis(2, at=log(c(0.006,0.2,0.4,0.6,0.8,1)), labels=c(0,0.2,0.4,0.6,0.8,1))
						mtext('Daily nest predation',side=2,line=1, cex=0.6, las=3, col='grey30')
					
					
					
					# treated
					for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, ppi$pred, col=ppi$line_col,lwd=1)
						}	
						
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
						text(x = c(2000), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
						text(x = c(2000,2008), y =log(0.6), labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
						text(x = c(2000,2008), y =log(0.4), labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}
  {# back transformed
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_1Balt_DPRpoly_col.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.15,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.05,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')#xaxs="i",yaxs="i")
									
			axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.35,0), lwd = 0.5)
				mtext('Year',side=1,line=0.3, cex=0.6, las=1, col='grey30')
			axis(2, at=seq(0,0.07, by = 0.01), labels=seq(0,0.07, by = 0.01), lwd = 0.5)
				mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr)-0.01, 
								rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, exp(ppi$pred)-0.01, col=ppi$line_col,lwd=1)
						}	
		#ppi = ps
		#polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr)-0.01, 
		 # rev(exp(ppi$upr)-0.01)), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		#lines(ppi$mean_year, exp(ppi$pred)-0.01, col='black',lwd=1)
		
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
		#text(x = c(1944,1980), y =0.07, labels= c('General',col_$Belt[col_$Belt%in%c('Arctic')]), col=c('black',col_$line_col[col_$Belt%in%c('Arctic')]), cex = 0.5, adj = 0)
		text(x = c(1944), y =0.07, labels= c(col_$Belt[col_$Belt%in%c('Arctic')]), col=c(col_$line_col[col_$Belt%in%c('Arctic')]), cex = 0.5, adj = 0)
		text(x = c(1944), y =0.065, labels= col_$Belt[col_$Belt%in%c('North temperate')], col=col_$line_col[col_$Belt%in%c('North temperate')], cex = 0.5, adj = 0)
		text(x = c(1944), y =0.06, labels= col_$Belt[col_$Belt%in%c('North tropics')], col=col_$line_col[col_$Belt%in%c('North tropics')], cex = 0.5, adj = 0)
		text(x =  c(1944), y =0.055, labels= col_$Belt[col_$Belt%in%c('South tropics')], col=col_$line_col[col_$Belt%in%c('South tropics')], cex = 0.5, adj = 0)
		text(x =  c(1944), y =0.05, labels= col_$Belt[col_$Belt%in%c('South temperate')], col=col_$line_col[col_$Belt%in%c('South temperate')], cex = 0.5, adj = 0)
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}

# all data poly + TPR
  {# predictions
     dd =d
		m <- lmer(TPR ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2)*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]
		
	m <- lmer(TPR ~ ln_N_nests + poly(mean_year,2) + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(dd$ln_N_nests),mean_year = seq(min(dd$mean_year),max(dd$mean_year), length.out=300))
		
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2),data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
}
{# plot
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_1b_all_orig_poly_mean.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
			axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0), lwd = 0.5)
				mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
			axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Total predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(100*(ppi$lwr), 
								rev(100*(ppi$upr))), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, 100*(ppi$pred), col=ppi$line_col,lwd=1)
						}	
		ppi = ps
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(100*(ppi$lwr), 
		  rev(100*(ppi$upr))), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, 100*(ppi$pred), col='black',lwd=1)
		
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
		text(x = c(1944,1980), y =100, labels= c('General',col_$Belt[col_$Belt%in%c('Arctic')]), col=c('black',col_$line_col[col_$Belt%in%c('Arctic')]), cex = 0.5, adj = 0)
		text(x = c(1944,1980), y =90, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
		text(x =  c(1944,1980), y =80, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}
{# plot - without general
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_1b_all_orig_poly.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,100),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
			axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0), lwd = 0.5)
				mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
			axis(2, at=seq(0,100, by = 20), labels=paste(seq(0,100, by = 20),'%',sep=''), lwd = 0.5)
				mtext('Total predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(100*(ppi$lwr), 
								rev(100*(ppi$upr))), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, 100*(ppi$pred), col=ppi$line_col,lwd=1)
						}	
						
		text(x = c(1944), y =100, labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
		text(x = c(1944,1980), y =90, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
		text(x =  c(1944,1980), y =80, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}
	
# lat	
# ?? all data poly + log(DPR)
  {# predictions
     dd =d
		m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2)*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]
		
	m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,2) + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(dd$ln_N_nests),mean_year = seq(min(dd$mean_year),max(dd$mean_year), length.out=300))
		
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2),data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
}
  {# on log 
		if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_all_log.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(pp$pred~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=log(c(0.006,1)),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
					axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0))
						mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
						#mtext('Tundra temperature [°C]',side=1,line=0.4, cex=0.6, las=1, col='grey30')
						
					axis(2, at=log(c(0.006,0.2,0.4,0.6,0.8,1)), labels=c(0,0.2,0.4,0.6,0.8,1))
						mtext('Daily nest predation',side=2,line=1, cex=0.6, las=3, col='grey30')
					
					
					
					# treated
					for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, ppi$pred, col=ppi$line_col,lwd=1)
						}	
						
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
						text(x = c(2000), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
						text(x = c(2000,2008), y =log(0.6), labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
						text(x = c(2000,2008), y =log(0.4), labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}
  {# back transformed
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_1a_all_orig_poly_back_mean.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", lwd = 0.5)
		plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
			axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0), lwd = 0.5)
				mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
			axis(2, at=seq(0,0.07, by = 0.01), labels=seq(0,0.07, by = 0.01), lwd = 0.5)
				mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
		for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr), 
								rev(exp(ppi$upr))), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, exp(ppi$pred), col=ppi$line_col,lwd=1)
						}	
		ppi = ps
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr), 
		  rev(exp(ppi$upr))), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, exp(ppi$pred), col='black',lwd=1)
		
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
		text(x = c(1944,1980), y =0.07, labels= c('General',col_$Belt[col_$Belt%in%c('Arctic')]), col=c('black',col_$line_col[col_$Belt%in%c('Arctic')]), cex = 0.5, adj = 0)
		text(x = c(1944,1980), y =0.065, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
		text(x =  c(1944,1980), y =0.06, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
		#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}

	
	
# poly and DPR - all
  {# predictions
		dd =d
		m <- lmer(DPR ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2)*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]
			
	dd =d
		m <- lmer(DPR ~ ln_N_nests + poly(mean_year,2) + Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(dd$ln_N_nests),mean_year = seq(min(dd$mean_year),max(dd$mean_year), length.out=300))
		
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2),data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
				
}
  {# plot
		if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_all_DPR_poly_mean.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(pp$pred~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
		axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0))
				mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
			axis(2, at=seq(0,0.07, by = 0.01), labels=seq(0,0.07, by = 0.01))
				mtext('Daily nest predation',side=2,line=1, cex=0.6, las=3, col='grey30')
					
		for(i in unique(dd$Belt)){
						 #i ="South temperate"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, ppi$pred, col=ppi$line_col,lwd=1)
						}
							
		ppi = ps
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
		  rev(ppi$upr)), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, ppi$pred, col='black',lwd=1)
							
	text(x = c(1944), y =0.07, labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
						text(x = c(1944,1975), y =0.065, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
						text(x =  c(1944,1975), y =0.06, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
				
			if(PNG == TRUE) {dev.off()}
}
	
# poly and DPR - all
  {# predictions
		dd =d
		m <- lmer(DPR ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2)*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]
			
	dd =d
		m <- lmer(DPR ~ ln_N_nests + poly(mean_year,2) + Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(dd$ln_N_nests),mean_year = seq(min(dd$mean_year),max(dd$mean_year), length.out=300))
		
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2),data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
				
}
  {# plot
		if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_all_DPR_poly_mean.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(pp$pred~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
		axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0))
				mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
			axis(2, at=seq(0,0.07, by = 0.01), labels=seq(0,0.07, by = 0.01))
				mtext('Daily nest predation',side=2,line=1, cex=0.6, las=3, col='grey30')
					
		for(i in unique(dd$Belt)){
						 #i ="South temperate"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, ppi$pred, col=ppi$line_col,lwd=1)
						}
							
		ppi = ps
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
		  rev(ppi$upr)), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, ppi$pred, col='black',lwd=1)
							
	text(x = c(1944), y =0.07, labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
						text(x = c(1944,1975), y =0.065, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
						text(x =  c(1944,1975), y =0.06, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
				
			if(PNG == TRUE) {dev.off()}
}
		
	
	
	
m_ass(name = 'test', mo = m0, dat = d, fixed = 3, categ = 'Belt', spatial = TRUE, temporal = FALSE, PNG = TRUE)
   
   m0 = lmer(log(DPR) ~ log( N_nests) + mean_year+Belt + (N_nests|mean_year) +(1|site)+(1|species),  data = d)  
   
m3 = lmer(log(DPR) ~ log( N_nests) + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$mean_year>2000,])
	plot(allEffects(m3))
	summary(glht(m3))
	
  m4 = lmer(log(DPR) ~ log( N_nests) + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$mean_year>2000 & d$Belt%in%c('North temperate','Arctic'),])
	plot(allEffects(m4))
	summary(glht(m4))
	
# TABLE S1 - TPR
# testing for the difference between belts
  # using Kubelka et al model from their Table S2A
  Model_7 = lmekin( TPR ~ (1|species ) + log( N_nests)+ mean_year*Belt  , varlist = list( I, phyloMat , distanceMatrix ), data = d )
  Model_7
  # using lmer package	(same results)	
  m = lmer(TPR ~ log( N_nests) + mean_year*Belt +(1|species),  data = d)  

# controlling for site and year
  # simple linear
  m0 = lmer(TPR ~ log( N_nests) + mean_year+Belt + (1|mean_year) +(1|site)+(1|species),  data = d)  
  # interaction linear
  m1 = lmer(TPR ~ log( N_nests) + mean_year*Belt + (1|mean_year) +(1|site)+(1|species),  data = d)
  # interaction 2nd polynomial
  m2 = lmer(TPR ~ log( N_nests) + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data = d)
   
  # comparing the models
  AIC(m0,m1,m2)  
 
  m3 = lmer(TPR ~ log( N_nests) + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$mean_year>2000,])
	plot(allEffects(m3))
  
  m4 = lmer(TPR ~ log( N_nests) + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$mean_year>2000 & d$Belt%in%c('North temperate','Arctic'),])
	plot(allEffects(m4))
	summary(glht(m4))

### START


 m3 = lmer(TPR ~ log( N_nests) + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data = d)
m <- lmer((DPR) ~ log( N_nests) + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data = d)
m <- lmer(log(DPR) ~ log( N_nests) + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data = d)

m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$mean_year>2000,])

m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$mean_year>2000 & d$Belt%in%c('North temperate','Arctic'),])
#####

m <- lmer(log(DPR) ~ ln_N_nests + mean_year*Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$mean_year>2000,])
m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data =d )
m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,3)*Belt + (1|mean_year) +(1|site)+(1|species),  data =d )
m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,4)*Belt + (1|mean_year) +(1|site)+(1|species),  data =d )
plot(allEffects(m))
summary(glht(m))
{# plot - on log - >2000
	# predictions
		dd =d[d$mean_year>2000,]
		m <- lmer(log(DPR) ~ ln_N_nests + mean_year*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + mean_year*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]

			
	# plot
		if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_all.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(pp$pred~pp$mean_year, pch=19,xlim=c(2000,2016), ylim=log(c(0.006,1)),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
					axis(1, at=seq(2000,2016, by = 4),labels=seq(2000,2016, by = 4),cex.axis=0.5,mgp=c(0,-0.15,0))
						mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
						#mtext('Tundra temperature [°C]',side=1,line=0.4, cex=0.6, las=1, col='grey30')
						
					axis(2, at=log(c(0.006,0.2,0.4,0.6,0.8,1)), labels=c(0,0.2,0.4,0.6,0.8,1))
						mtext('Daily nest predation',side=2,line=1, cex=0.6, las=3, col='grey30')
					
					
					
					# treated
					for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, ppi$pred, col=ppi$line_col,lwd=1)
						}	
						
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
						text(x = c(2000), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
						text(x = c(2000,2008), y =log(0.6), labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
						text(x = c(2000,2008), y =log(0.4), labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}

# all data poly + log(DPR)
  {# predictions
dd =d
		m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2)*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]

}
  {# on log 
		if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_all_log.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(pp$pred~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=log(c(0.006,1)),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
					axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0))
						mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
						#mtext('Tundra temperature [°C]',side=1,line=0.4, cex=0.6, las=1, col='grey30')
						
					axis(2, at=log(c(0.006,0.2,0.4,0.6,0.8,1)), labels=c(0,0.2,0.4,0.6,0.8,1))
						mtext('Daily nest predation',side=2,line=1, cex=0.6, las=3, col='grey30')
					
					
					
					# treated
					for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, ppi$pred, col=ppi$line_col,lwd=1)
						}	
						
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
						text(x = c(2000), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
						text(x = c(2000,2008), y =log(0.6), labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
						text(x = c(2000,2008), y =log(0.4), labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}
  {# back transformed
	if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_all_orig_poly_back.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(exp(pp$pred)~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
			axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0))
				mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
			axis(2, at=seq(0,0.07, by = 0.01), labels=seq(0,0.07, by = 0.01))
				mtext('Daily nest predation',side=2,line=1, cex=0.6, las=3, col='grey30')
					
					
					
					# treated
					for(i in unique(dd$Belt)){
						 #i ="Arctic"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(exp(ppi$lwr), 
								rev(exp(ppi$upr))), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, exp(ppi$pred), col=ppi$line_col,lwd=1)
						}	
						
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
						text(x = c(1944), y =0.07, labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
						text(x = c(1944,1975), y =0.065, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
						text(x =  c(1944,1975), y =0.06, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}

# poly and DPR - all
  {# predictions
		dd =d
		m <- lmer(DPR ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2)*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]
			
	dd =d
		m <- lmer(DPR ~ ln_N_nests + poly(mean_year,2) + Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(dd$ln_N_nests),mean_year = seq(min(dd$mean_year),max(dd$mean_year), length.out=300))
		
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2),data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			ps=newD	
				
}
  {# plot
		if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_all_DPR_poly_mean.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(pp$pred~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(0,0.07),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
		axis(1, at=c(1944,1960,1980,2000,2016),labels=c(1944,1960,1980,2000,2016),cex.axis=0.5,mgp=c(0,-0.15,0))
				mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
			axis(2, at=seq(0,0.07, by = 0.01), labels=seq(0,0.07, by = 0.01))
				mtext('Daily nest predation',side=2,line=1, cex=0.6, las=3, col='grey30')
					
		for(i in unique(dd$Belt)){
						 #i ="South temperate"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, ppi$pred, col=ppi$line_col,lwd=1)
						}
							
		ppi = ps
		polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
		  rev(ppi$upr)), border=NA, col = adjustcolor('black',alpha.f = 0.2))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
		lines(ppi$mean_year, ppi$pred, col='black',lwd=1)
							
	text(x = c(1944), y =0.07, labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
						text(x = c(1944,1975), y =0.065, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
						text(x =  c(1944,1975), y =0.06, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
				
			if(PNG == TRUE) {dev.off()}
}


{# plot - on log - arctic temp
	# predictions
		dd =d[d$mean_year>2000 & d$Belt%in%c('North temperate','Arctic'),]
		m <- lmer(log(DPR) ~ ln_N_nests + mean_year*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + mean_year*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]

			
	# plot
		if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_Arc_Temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(pp$pred~pp$mean_year, pch=19,xlim=c(2000,2016), ylim=log(c(0.006,1)),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
					axis(1, at=seq(2000,2016, by = 4),labels=seq(2000,2016, by = 4),cex.axis=0.5,mgp=c(0,-0.15,0))
						mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
						#mtext('Tundra temperature [°C]',side=1,line=0.4, cex=0.6, las=1, col='grey30')
						
					axis(2, at=log(c(0.006,0.2,0.4,0.6,0.8,1)), labels=c(0,0.2,0.4,0.6,0.8,1))
						mtext('Daily nest predation',side=2,line=1, cex=0.6, las=3, col='grey30')
					
					
					
					# treated
					for(i in unique(dd$Belt)){
						 #i ="South temperate"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, ppi$pred, col=ppi$line_col,lwd=1)
						}	
						
						text(x = c(2000,2008), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('Arctic','North temperate')], col=col_$line_col[col_$Belt%in%c('Arctic','North temperate')], cex = 0.5, adj = 0)
						
				
			if(PNG == TRUE) {dev.off()}
}
{# plot - original
	# plot
		if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_all_orig.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(pp$pred~pp$mean_year, pch=19,xlim=c(2000,2016), ylim=c(-0.03,0.2),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
					axis(1, at=seq(2000,2016, by = 4),labels=seq(2000,2016, by = 4),cex.axis=0.5,mgp=c(0,-0.15,0))
						mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
						#mtext('Tundra temperature [°C]',side=1,line=0.4, cex=0.6, las=1, col='grey30')
						
					axis(2, at=seq(0,0.2,by = 0.05), labels=seq(0,0.2,by = 0.05))
						mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
					
					
					
					# treated
					for(i in unique(dd$Belt)){
						 #i ="South temperate"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, ppi$pred, col=ppi$line_col,lwd=1)
						}	
						
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
						text(x = c(2000), y =0.2, labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
						text(x = c(2000,2008), y =0.18, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
						text(x = c(2000,2008), y =0.16, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}

{# plot original - arctic temp - TO DO
	# predictions
		dd =d[d$mean_year>2000 & d$Belt%in%c('North temperate','Arctic'),]
		m <- lmer(log(DPR) ~ ln_N_nests + mean_year*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + mean_year*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]

			
	# plot
		if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_Arc_Temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(pp$pred~pp$mean_year, pch=19,xlim=c(2000,2016), ylim=log(c(0.006,1)),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
					axis(1, at=seq(2000,2016, by = 4),labels=seq(2000,2016, by = 4),cex.axis=0.5,mgp=c(0,-0.15,0))
						mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
						#mtext('Tundra temperature [°C]',side=1,line=0.4, cex=0.6, las=1, col='grey30')
						
					axis(2, at=log(c(0.006,0.2,0.4,0.6,0.8,1)), labels=c(0,0.2,0.4,0.6,0.8,1))
						mtext('Daily nest predation',side=2,line=1, cex=0.6, las=3, col='grey30')
					
					
					
					# treated
					for(i in unique(dd$Belt)){
						 #i ="South temperate"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, ppi$pred, col=ppi$line_col,lwd=1)
						}	
						
						text(x = c(2000,2008), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('Arctic','North temperate')], col=col_$line_col[col_$Belt%in%c('Arctic','North temperate')], cex = 0.5, adj = 0)
						
				
			if(PNG == TRUE) {dev.off()}
}
{# plot - original + poly - all data also before 
	# predictions
		dd =d
		dd$mean_year_pol1 = poly(dd$mean_year,2)[,1]
		dd$mean_year_pol2 = poly(dd$mean_year,2)[,2]
		#m <- lmer(DPR ~ ln_N_nests + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data =dd )
		m <- lmer(DPR ~ ln_N_nests + mean_year_pol1*Belt +mean_year_pol2*Belt+ (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in unique(dd$Belt)){
		 #i ="South temperate"
	  	 print(i)
		 di = dd[dd$Belt==i,]
		 l[[i]]=data.frame(ln_N_nests = mean(di$ln_N_nests),mean_year = seq(min(di$mean_year),max(di$mean_year), length.out=300), Belt = i)
		 }
		
		newD = do.call(rbind,l)	
			 newD$mean_year_pol1 = poly(newD$mean_year,2)[,1]
			newD$mean_year_pol2 = poly(newD$mean_year,2)[,2]		
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + mean_year_pol1*Belt +mean_year_pol2*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD	
			pp$line_col = col_$line_col[match(pp$Belt, col_$Belt)]
			pp$poly_col = col_$poly_col[match(pp$Belt, col_$Belt)]

			
	# plot
		if(PNG == TRUE) {
					
			png(paste(outdir,"Figure_1a_all_orig_allyears.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
		#	png(paste(outdir,"Figure_1a_Arc_temp.png", sep=""), width=1.85,height=1.85,units="in",res=600)	 
					#jpeg(paste(outdir,"Figure_3.jpeg", sep=""), width=3.5,height=1.85,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.85) #dev.new(width=3.5, height=1.97)
					}	
		par(mar=c(2.2,1.7,0.5,0.5), ps=12, mgp=c(1.2,0.35,0), las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black")
		plot(pp$pred~pp$mean_year, pch=19,xlim=c(1944,2016), ylim=c(-0.03,0.2),ylab=NA,xlab=NA, xaxt='n',yaxt='n', type='n')
									
					axis(1, at=c(1944,1960,1980,2000,2015),labels=c(1944,1960,1980,2000,2015),cex.axis=0.5,mgp=c(0,-0.15,0))
						mtext('Year',side=1,line=0.6, cex=0.6, las=1, col='grey30')
						#mtext('Tundra temperature [°C]',side=1,line=0.4, cex=0.6, las=1, col='grey30')
						
					axis(2, at=seq(0,0.2,by = 0.05), labels=seq(0,0.2,by = 0.05))
						mtext('Daily nest predation',side=2,line=1.1, cex=0.6, las=3, col='grey30')
					
					
					
					# treated
					for(i in unique(dd$Belt)){
						 #i ="South temperate"
						 print(i)
						 ppi = pp[pp$Belt==i,]
							polygon(c(ppi$mean_year, rev(ppi$mean_year)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col = adjustcolor(ppi$line_col,alpha.f = 0.1))#adjustcolor(col_t ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							
							lines(ppi$mean_year, ppi$pred, col=ppi$line_col,lwd=1)
						}	
						
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)						
						text(x = c(1944), y =0.2, labels= col_$Belt[col_$Belt%in%c('Arctic')], col=col_$line_col[col_$Belt%in%c('Arctic')], cex = 0.5, adj = 0)
						text(x = c(1944,1980), y =0.18, labels= col_$Belt[col_$Belt%in%c('North temperate','North tropics')], col=col_$line_col[col_$Belt%in%c('North temperate','North tropics')], cex = 0.5, adj = 0)
						text(x = c(1944,1980), y =0.16, labels= col_$Belt[col_$Belt%in%c('South tropics','South temperate')], col=col_$line_col[col_$Belt%in%c('South tropics','South temperate')], cex = 0.5, adj = 0)
						#text(x = c(2004,2012), y =log(0.9), labels= col_$Belt[col_$Belt%in%c('North temperate','Arctic')], col=col_$line_col[col_$Belt%in%c('North temperate','Arctic')], cex = 0.5)
						#text(x = seq(2000,2016, by = 4), y =log(0.9), labels= col_$Belt, col=col_$line_col, cex = 0.5)
				
			if(PNG == TRUE) {dev.off()}
}


					points(bt$t_ambient_med, bt$inc_eff, col=col_t,bg=adjustcolor(col_t ,alpha.f = 0.4), pch=21,cex=0.5)
					mtext(expression(bold("B")),side=3,line=-0.45, cex=0.7, las=1,adj=1, col="grey30")			
					
# Fig 3
m=  lmer( log(DPR) ~ log(N_nests) + period_orig*lat_abs +(1|mean_year/site) + (1|species), data = d )
m=  lmer( log(DPR) ~ log(N_nests) + period_orig*poly(lat_abs,2) +(1|mean_year/site) + (1|species), data = d )
m=  lmer( log(DPR) ~ log(N_nests) + period_orig*poly(lat_abs,3) +(1|mean_year/site) + (1|species), data = d )

m=  lmer( log(DPR) ~ log(N_nests) + period_orig*lat_abs +(1|mean_year/site) + (1|species), data = d[d$lat_abs>60,] )
m=  lmer( log(DPR) ~ log(N_nests) + period_orig*poly(lat_abs,2) +(1|mean_year/site) + (1|species), data = d[d$lat_abs>60,] )

m=  lmer( log(DPR) ~ log(N_nests) + lat_abs*scale(mean_year) +(1|mean_year/site) + (1|species), data = d )
m=  lmer( log(DPR) ~ log(N_nests) + poly(lat_abs,2)*scale(mean_year) +(1|mean_year/site) + (1|species), data = d )
summary(glht(m))
summary(m)
plot(allEffects(m))

#### TESTING
  bind.tip<-function(tree,tip.label,edge.length=NULL,where=NULL){
  if(is.null(where)) where<-length(tree$tip)+1
  tip<-list(edge=matrix(c(2,1),1,2),
            tip.label=tip.label,
            edge.length=edge.length,
            Nnode=1)
  class(tip)<-"phylo"
  obj<-bind.tree(tree,tip,where=where)
  return(obj)
}
  addInTip <- function( phylo, where, newname) {
  idx <- which( phylo$tip == where )
  np <- nodepath( phylo)[[idx]]
  to <- np[ length(np) -1 ]
  ed <- which( phylo$edge[,2] == idx )
  newphylo <- bind.tip(tree = phylo, tip.label = newname, edge.length = phylo$edge.length[ed], where = to )
  return( newphylo)
}

where = "Charadrius_alexandrinus"

tree2 <- addInTip( phylo = trees[[25]],where = "Charadrius_alexandrinus", newname =  "Charadrius_nivosus" )
tree2 <- addInTip( phylo = trees[[42]],where = "Gallinago_gallinago", newname =  "Gallinago_delicata" )
plotTree(tree2, node.numbers = TRUE, fsize = 0.5)

treex <- trees[[5]]
idx <- which( treex$tip == "Charadrius_alexandrinus")
np <- nodepath(treex)[[idx]]
to <- np[ length( np ) - 1] 
ed <- which( treex$edge[,2] == idx)

tree1 <- bind.tip(tree= treex, tip.label="Charadrius_nivosus", edge.length = tree$edge.length[ed], where = to )

  bind.tip<-function(tree,tip.label,edge.length=NULL,where=NULL){
  if(is.null(where)) where<-length(tree$tip)+1
  tip<-list(edge=matrix(c(2,1),1,2),
            tip.label=tip.label,
            edge.length=edge.length,
            Nnode=1)
  class(tip)<-"phylo"
  obj<-bind.tree(tree,tip,where=where)
  return(obj)
}
					
# not used
{# model assumptions
   m = m0	
   dev.new(width=6,height=9)
   par(mfrow=c(4,3))
   			
   scatter.smooth(fitted(m),resid(m),col='orange');abline(h=0, lty=2)
   scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='orange')
   qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='orange');qqline(resid(m))
   qqnorm(unlist(ranef(m)$species[1]), main = "species",col='orange');qqline(unlist(ranef(m)$species[1]))
   qqnorm(unlist(ranef(m)$site[1]), main = "site",col='orange');qqline(unlist(ranef(m)$site[1]))					
   qqnorm(unlist(ranef(m)$mean_year[1]), main = "mean_year",col='orange');qqline(unlist(ranef(m)$mean_year[1]))					
   
   scatter.smooth(resid(m)~d$mean_year, col='orange');abline(h=0, lty=2, col = 'deepskyblue3')
   plot(resid(m)~d$Belt, , col='orange');abline(h=0, lty=2, lwd=2, col = 'deepskyblue3')
   scatter.smooth(resid(m)~log(d$N_nests), col='orange');abline(h=0, lty=2, col = 'deepskyblue3')
						
	#acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
						
	# spatial autocorrelations - nest location
	spdata=data.frame(resid=resid(m), x=d$Longitude, y=d$Latitude)
		spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
		#cex_=c(1,2,3,3.5,4)
		cex_=c(1,1.5,2,2.5,3)
		spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
	  plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
		legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
	  plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals (<0)', cex=0.8))
	  plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals (>=0)', cex=0.8))

} 

# OLD VERSION
  m_ass = function(name = 'define', mo = m0, dat = d, fixed = 3, categ = NULL, spatial = TRUE, temporal = TRUE, PNG = TRUE){
   l=data.frame(summary(mo)$varcor)
   l = l[is.na(l$var2),]
   if(PNG == TRUE){
	png(paste(outdir,name, ".png", sep=""), width=6,height=9,units="in",res=600)
	 }else{dev.new(width=6,height=9)}
   
   n = nrow(l)-1+fixed + 6
   par(mfrow=c(ceiling(n/3),3))
   
   scatter.smooth(fitted(mo),resid(mo),col='grey');abline(h=0, lty=2, col ='red')
   scatter.smooth(fitted(mo),sqrt(abs(resid(mo))), col='grey')
   qqnorm(resid(mo), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey');qqline(resid(mo))
   unique(l$grp[l$grp!="Residual"])
   for(i in unique(l$grp[l$grp!="Residual"])){
	#i = "mean_year"
	ll=ranef(mo)[names(ranef(mo))==i][[1]]
	if(ncol(ll)==1){
	 qqnorm(ll[,1], main = paste(i,names(ll)[1]),col='grey');qqline(ll[,1], col ='red')
	 }else{
	  qqnorm(ll[,1], main = paste(i,names(ll)[1]),col='grey');qqline(ll[,1], col ='red')
	  qqnorm(ll[,2], main = paste(i,names(ll)[2]),col='grey');qqline(ll[,2], col ='red')
	 }
	}
	
   # variables
   scatter={} 
   for (i in rownames(summary(mo)$coef)) {
      j=sub("\\).*", "", sub(".*\\(", "",i)) 
      scatter[length(scatter)+1]=j
    }
    x = data.frame(scatter=unique(scatter)[2:length(unique(scatter))],
					log_ = grepl("log",rownames(summary(mo)$coef)[2:length(unique(scatter))]), stringsAsFactors = FALSE)
    for (i in 1: nrow(x)){
      jj = x$scatter[i]
	  if ( jj %in% names(dat)){
        variable=dat[,names(dat)==jj]
		if(x$log_[i]==TRUE){
        scatter.smooth(resid(mo)~log(variable),xlab=paste('log(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
		}else{
        scatter.smooth(resid(mo)~variable,xlab=jj,col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
      }
     }
	} 
	if(length(categ)>0){
	  for(i in categ){
		 variable=dat[,names(dat)==i]
		  plot(resid(m0)~variable, medcol='grey', whiskcol='grey', staplecol='grey', boxcol='grey', outcol='grey');abline(h=0, lty=3, lwd=1, col = 'red')
		 }
	}	  
		  
	if(temporal == TRUE){
		acf(resid(mo), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
		}
	if(spatial == TRUE){	
	spdata=data.frame(resid=resid(mo), x=d$Longitude, y=d$Latitude)
		spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
		#cex_=c(1,2,3,3.5,4)
		cex_=c(1,1.5,2,2.5,3)
		spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
	  plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
		legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
	  plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals (<0)', cex=0.8))
	  plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals (>=0)', cex=0.8))
		}
   
   mtext(paste(slot(mo,"call")[1],'(',slot(mo,"call")[2],sep=''), side = 3, line = -1, cex=0.7,outer = TRUE)
  if(PNG==TRUE){dev.off()}
  }
    
  					