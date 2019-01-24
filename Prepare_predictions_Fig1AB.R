# Fig1A DPR
    dd =d
		m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,2)*Belt +(1|site)+(1|species),  data =dd )
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
			newD$line_col = col_$line_col[match(newD$Belt, col_$Belt)]
	dprA=newD
	
# Fig1A TPR 
     dd =d
		m <- lmer(TPR ~ ln_N_nests + poly(mean_year,2)*Belt + (1|site)+(1|species),  data =dd )
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
			newD$line_col = col_$line_col[match(newD$Belt, col_$Belt)]
	tprA=newD	

# Fig1B DPR
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
			newD$line_col = col_$year_col[match(newD$mean_year, col_$year_)]
	dprB=newD
	
# Fig1B TPR
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
			newD$line_col = col_$year_col[match(newD$mean_year, col_$year_)]
	tprB=newD

# D loes
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
	