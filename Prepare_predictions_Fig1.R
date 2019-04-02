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
	
# Fig1D DPR
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
	
# Fig1E DPR
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

# Fig1F DPR	
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
			
# Fig 1G loess
# proportion of transfomred
d$n = 1
d$trans = ifelse(d$DPR_trans == 'YES',1,0)
d$int5 = ifelse(d$mean_year<1943, 1940, ifelse(d$mean_year<1948, 1945,ifelse(d$mean_year<1953, 1950,ifelse(d$mean_year<1958, 1955, ifelse(d$mean_year<1963, 1960, ifelse(d$mean_year<1968, 1965, ifelse(d$mean_year<1973, 1970, ifelse(d$mean_year<1978, 1975, ifelse(d$mean_year<1983,1980, ifelse(d$mean_year<1988,1985, ifelse(d$mean_year<1993, 1990, ifelse(d$mean_year<1998, 1995, ifelse(d$mean_year<2003, 2000, ifelse(d$mean_year<2008, 2005,  ifelse(d$mean_year<2013, 2010, 2015)))))))))))))))

a = d[order(d$mean_year),]
plx<-predict(loess(trans*100~mean_year,a), se=T)
a$fit = plx$fit
df_ =plx$df
a$lwr = (plx$fit - qt(0.975,df_)*plx$se)
a$lwr = ifelse(a$lwr<0,0,a$lwr)
a$upr = (plx$fit + qt(0.975,df_)*plx$se)
a$upr = ifelse(a$upr>100,100,a$upr)
		#ggplot(d,aes(x= mean_year, y = trans))+stat_smooth() + ylab('Beintam used (yes = 1, no = 0)')								
		#ggsave(paste(outdir,"Figure_1G.png", sep=""),width=1.85*2, height=1.85*2, units='in',dpi=600)
		#ggplot(d,aes(x= mean_year, y = trans, col = Belt))+stat_smooth(se = F) + ylab('Beintam used (yes = 1, no = 0)')	

# arctic proportion of transformed
ac = d[d$Belt == 'Arctic',]
ac = ac[order(ac$mean_year),]
plx<-predict(loess(trans*100~mean_year,ac), se=T)
ac$fit = plx$fit
df_ =plx$df
ac$lwr = (plx$fit - qt(0.975,df_)*plx$se)
ac$lwr = ifelse(ac$lwr<0,0,ac$lwr)
ac$upr = (plx$fit + qt(0.975,df_)*plx$se)
ac$upr = ifelse(ac$upr>100,100,ac$upr)

# rest   
re = d[d$Belt != 'Arctic',]
re = re[order(re$mean_year),]
plx<-predict(loess(trans*100~mean_year,re), se=T)
re$fit = plx$fit
df_ =plx$df
re$lwr = (plx$fit - qt(0.975,df_)*plx$se)
re$lwr = ifelse(re$lwr<0,0,re$lwr)
re$upr = (plx$fit + qt(0.975,df_)*plx$se)
re$upr = ifelse(re$upr>100,100,re$upr)

# raw data
  ddr5 = ddply(a,('int5'), summarise, mea = mean(trans), n = sum(n))	
  ddr5ac = ddply(ac,('int5'), summarise, mea = mean(trans), n = sum(n))
	ddr5ac$col_ = col_$line_col[col_$Belt=="Arctic"]  
	ddr5ac$col_bg = arct_bg
  ddr5r = ddply(re,('int5'), summarise, mea = mean(trans), n = sum(n))	
	ddr5r$col_ = rest
	ddr5r$col_bg = rest_bg
  acr = rbind(ddr5r,ddr5ac)
	#acx = data.frame(int5 = 1900, med = 0, mea = 0, n = 33, stringsAsFactors= FALSE)
     #ddr5ac = rbind(ddr5ac,acx)


# Fig 1H -  sensitivity
 g = b[b$DPRtrans=='NO' | (b$DPRtrans=='YES' & b$mean_year>1999),]
  g$DPR = g$DPR_orig
  h = b[b$DPRtrans=='YES' & b$mean_year<2000,]
ll = list()
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
		ppi$B = r
ll[[i]] = ppi		
print(i)
	}			
ppl = do.call(rbind,ll)  
  
# I - Beintema effects
#g[,c('obs_time','N.nests','other_failed','predated','infertile','hatched')]
u=b[b$DPRtrans=='NO',]
u=u[-which(is.na(u$obs_time)| is.na(u$other_failed)),]
u$hatched[u$N.nests == 86 & u$Locality == 'South Uist, Hebrides'] = 86-19
u$infertile[u$N.nests == 86 & u$Locality == 'South Uist, Hebrides'] = 0
u = u[order(u$DPR_orig),]	