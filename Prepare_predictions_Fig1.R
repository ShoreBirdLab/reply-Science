# Fig1A DPR
    dd =d
		m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,2)*Belt +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
			#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5,0.975))	
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

# Fig1B DPR
    dd =d
		m <- lmer(log(DPR) ~ ln_N_nests + poly(mean_year,2)+Belt +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
			#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5,0.975))
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
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2)+Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			newD$line_col = col_$line_col[match(newD$Belt, col_$Belt)]
	dprB=newD	

# Fig1C DPR
    dd =d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),]
	nrow(dd)
	#summary(dd$Belt)	
	m = lmer(log(DPR) ~ ln_N_nests + poly(mean_year,2)*Belt+(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5,0.975))
		# values to predict for		
		newD=data.frame(ln_N_nests = mean(dd$ln_N_nests),mean_year = seq(min(dd$mean_year),max(dd$mean_year), length.out=900), Belt = c('Arctic', 'North temperate'))
		
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(mean_year,2)*Belt,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			newD$line_col = col_$line_col[match(newD$Belt, col_$Belt)]
	dprC=newD		
	
# Fig1AA DPR
    dd =d
		m <- lmer(log(DPR) ~ ln_N_nests + poly(Latitude,3)*period_orig + (1|mean_year) +(1|site)+(1|species),  data =dd )
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)  
		
		# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
		# values to predict for		
		l = list()
		for(i in c('before 2000','after 2000')){
		 #i ="South temperate"
	  	 print(i)
		  l[[i]]=data.frame(ln_N_nests = mean(d$ln_N_nests),period_orig= i, Latitude = seq(min(d$Latitude[d$period_orig == i]),max(d$Latitude[d$period_orig == i]), length.out=300),stringsAsFactors = FALSE)
		 }
		newD = do.call(rbind,l)	
					
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ ln_N_nests + poly(Latitude,3)*period_orig,data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			newD$line_col = newD$line_col = ifelse(newD$period_orig == 'before 2000',historic, recent)
	dprAA=newD
	
# Fig1BB DPR
    dd =d
		m <- lmer(log(DPR) ~ ln_N_nests + poly(Latitude,3)+scale(mean_year) + (1|mean_year) +(1|site)+(1|species),  data =dd )
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
			X <- model.matrix(~ ln_N_nests + poly(Latitude,3)+scale(mean_year),data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			newD$line_col = col_$year_col[match(newD$mean_year, col_$year_)]
	dprBB=newD

# Fig1CC DPR	
	dd =d[d$DPR_trans == "NO",]
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
		dprCC=newD	
			
# D loess
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
  
# proportion of transfomred
a = d[order(d$mean_year),]
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
	
# DD sensitivity (within text)
 
# Beintema effects
#g[,c('obs_time','N.nests','other_failed','predated','infertile','hatched')]

u=b[b$DPRtrans=='NO',]
u=u[-which(is.na(u$obs_time)| is.na(u$other_failed)),]
u$hatched[u$N.nests == 86 & u$Locality == 'South Uist, Hebrides'] = 86-19
u$infertile[u$N.nests == 86 & u$Locality == 'South Uist, Hebrides'] = 0
u = u[order(u$DPR_orig),]	