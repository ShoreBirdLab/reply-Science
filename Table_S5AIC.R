rm( list = ls() )	

# set working and output directories
wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
source(paste(wd, 'Constants_Functions.R',sep=""))
source(paste(wd, 'Prepare_Data.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script


#AIC
# DPR - BELTS
  m0a = lmer(log(DPR) ~ log( N_nests) + mean_year+Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)  
  m1a = lmer(log(DPR) ~ log( N_nests) + mean_year*Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
  m2a = lmer(log(DPR) ~ log( N_nests) + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
  m3a = lmer(log(DPR) ~ log( N_nests) + poly(mean_year,2)+Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
	 o1=data.frame(response = 'log(DPR)',
					model=c('year+belt','year*belt','poly2(year)*belt','poly2(year)+belt'), 
					K = c(length(fixef(m0a)),length(fixef(m1a)),length(fixef(m2a)),length(fixef(m3a))),
					AIC=AIC(m0a,m1a,m2a,m3a)$AIC)
		o=o1
		o$delta=o$AIC-min(o$AIC)
		o = o[order(o$delta),]
		o$prob=exp(-0.5*o$delta)/sum(exp(-0.5*o$delta))
		o$cums = cumsum(o$prob)
		o$ER=round(max(o$prob)/o$prob,2)
		o$prob=round(o$prob,2)
		o$cums=round(o$cums,2)
	 od = o
	 
# TPR - BELTS
  m0at = lmer(TPR ~ log( N_nests) + mean_year+Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)  
  m1at = lmer(TPR ~ log( N_nests) + mean_year*Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
  m2at = lmer(TPR~ log( N_nests) + poly(mean_year,2)*Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
  m3at = lmer(TPR ~ log( N_nests) + poly(mean_year,2)+Belt + (1|mean_year) +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
	 o1t=data.frame(response = 'TPR',
			model=c('year+belt','year*belt','poly2(year)*belt','poly2(year)+belt'), 
			K = c(length(fixef(m0at)),length(fixef(m1at)),length(fixef(m2at)),length(fixef(m3at))),
			AIC=AIC(m0at,m1at,m2at, m3at)$AIC)
		o=o1t
		o$delta=o$AIC-min(o$AIC)
		o = o[order(o$delta),]
		o$prob=exp(-0.5*o$delta)/sum(exp(-0.5*o$delta))
		o$cums = cumsum(o$prob)
		o$ER=round(max(o$prob)/o$prob,2)
		o$prob=round(o$prob,2)
		o$cums=round(o$cums,2)
	 ot = o
	 
# DPR - latitude
	m0x = lmer(log(DPR) ~ log( N_nests) + scale(mean_year)+scale(abs(Latitude)) + (1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
	m1x = lmer(log(DPR) ~ log( N_nests) + scale(mean_year)*scale(abs(Latitude))  +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE) 
	m3x = lmer(log(DPR) ~ log(N_nests) + scale(mean_year)*poly(abs(Latitude),2)  +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
	m55x = lmer(log(DPR) ~ log(N_nests) + scale(mean_year)+poly(abs(Latitude),2) + (1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
	
	m6x = lmer(log(DPR) ~ log( N_nests) + period_orig+scale(abs(Latitude))  +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
	m66x = lmer(log(DPR) ~ log( N_nests) + period_orig*scale(abs(Latitude)) +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
	m7x = lmer(log(DPR) ~ log( N_nests) + period_orig+poly(abs(Latitude),2)  +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
	m8x = lmer(log(DPR) ~ log( N_nests) + period_orig*poly(abs(Latitude),2)  +(1|site)+(1|species),  data = d[d$DPR_trans == 'NO' & d$Belt %in% c('Arctic','North temperate'),], REML = FALSE)
	
	
 o2=data.frame(response = 'ln(DPR)',
	model=c('year+abs(lat)','year*abs(lat)',
			'year*pol2(lat)','year+pol2(lat))',
			'+period+(lat)','*period*(lat)', #'hem+period+abs(lat)','hem*period+abs(lat)',
			'period+pol2(lat)','period*pol2(lat)'),#'period+pol3(abs(lat))','period+pol2(abs(lat))'
	K = c(length(fixef(m0x)),length(fixef(m1x)),
		length(fixef(m3x)),length(fixef(m55x)),
		length(fixef(m6x)),length(fixef(m66x)),
		length(fixef(m7x)),length(fixef(m8x))),
	AIC=AIC(m0x,m1x,
			m3x,m55x,
			m6x,m66x,
			m7x,m8x)$AIC)	
	o=o2
	o$delta=o$AIC-min(o$AIC)
		o = o[order(o$delta),]
		o$prob=exp(-0.5*o$delta)/sum(exp(-0.5*o$delta))
		o$cums = cumsum(o$prob)
		o$ER=round(max(o$prob)/o$prob,2)
		o$prob=round(o$prob,2)
		o$cums=round(o$cums,2)		
		odl = o
# o2=data.frame(response = 'ln(DPR)',
#	model=c('hem+year+abs(lat)','hem+year*abs(lat)','hem*year*abs(lat)','hem*year++hem*ab(lat)',
#			'year*pol2(abs(lat))','year*pol3(abs(lat))','year+pol3(abs(lat))','year+pol2(abs(lat))',
#			'hem+period*abs(lat)','hem*period*abs(lat)', #'hem+period+abs(lat)','hem*period+abs(lat)',
#			'period*pol3(abs(lat))','period*pol2(abs(lat))'),#'period+pol3(abs(lat))','period+pol2(abs(lat))'
#	AIC=AIC(m0x,m00x,m1x,m2x,
#			m3x,m4x,m5x,m55x,
#			m66x,m8x,
#			m9x,m11x)$AIC)	
#		o=o2		
		

	#plot(allEffects(m2x))
	#summary(glht(m2x))

# TPR - latitude
	m0x = lmer(TPR ~ log( N_nests) + hemisphere + scale(mean_year)+scale(abs(Latitude)) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	m00x = lmer(TPR ~ log( N_nests) + hemisphere + scale(mean_year)*scale(abs(Latitude)) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	m1x = lmer(TPR ~ log( N_nests) + hemisphere*scale(mean_year)*scale(abs(Latitude)) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE) 
 	m2x = lmer(TPR ~ log( N_nests) + hemisphere+scale(mean_year)+scale(abs(Latitude))+hemisphere*scale(mean_year)+hemisphere*scale(abs(Latitude))+  (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)

	m3x = lmer(TPR ~ log(N_nests) + scale(mean_year)*poly(abs(Latitude),2) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	m4x = lmer(TPR ~ log(N_nests) + scale(mean_year)*poly(Latitude,3) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	m5x = lmer(TPR ~ log(N_nests) + scale(mean_year)+poly(Latitude,3) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	m55x = lmer(TPR ~ log(N_nests) + scale(mean_year)+poly(Latitude,2) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	
	m6x = lmer(TPR ~ log( N_nests) + hemisphere+period_orig+scale(abs(Latitude)) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	m66x = lmer(TPR ~ log( N_nests) + hemisphere+period_orig*scale(abs(Latitude)) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	m7x = lmer(TPR ~ log( N_nests) + hemisphere*period_orig+scale(abs(Latitude)) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	m8x = lmer(TPR ~ log( N_nests) + hemisphere*period_orig*scale(abs(Latitude)) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	
	m9x = lmer(TPR ~ log( N_nests) + period_orig*poly(Latitude,3) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	m10x = lmer(TPR ~ log( N_nests) + period_orig+poly(Latitude,3) +(1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	m11x = lmer(TPR ~ log( N_nests) + period_orig*poly(Latitude,2) + (1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	m12x = lmer(TPR ~ log( N_nests) + period_orig+poly(Latitude,2) +(1|mean_year) +(1|site)+(1|species),  data = d, REML = FALSE)
	
 o2t=data.frame(response = 'TPR',
	model=c('hem+year+abs(lat)','hem*year*abs(lat)',
			'year*pol3(lat)','year+pol3(abs(lat))',
			'hem+period+abs(lat)','hem*period*abs(lat)', #'hem+period+abs(lat)','hem*period+abs(lat)',
			'period*pol3(lat)','period+pol3(lat)'),#'period+pol3(abs(lat))','period+pol2(abs(lat))'
	K = c(length(fixef(m0x)),length(fixef(m1x)),
		length(fixef(m4x)),length(fixef(m5x)),
		length(fixef(m6x)),length(fixef(m8x)),
		length(fixef(m9x)),length(fixef(m10x))),
	AIC=AIC(m0x,m1x,
			m4x,m5x,
			m6x,m8x,
			m9x,m10x)$AIC)	
		o=o2t
		o$delta=round(o$AIC-min(o$AIC),2)
		o = o[order(o$delta),]
		o$prob=exp(-0.5*o$delta)/sum(exp(-0.5*o$delta))
		o$cums = cumsum(o$prob)
		o$ER=round(max(o$prob)/o$prob,2)
		o$prob=round(o$prob,2)
		o$cums=round(o$cums,2)
	otl = o
	#plot(allEffects(m2x))
	#summary(glht(m2x))

# DPR - ALL together

o=rbind(o1,o2)
	o$delta=o$AIC-min(o$AIC)
	o = o[order(o$delta),]
	o$prob=exp(-0.5*o$delta)/sum(exp(-0.5*o$delta))
	o$cums = cumsum(o$prob)
	o$ER=round(max(o$prob)/o$prob,2)
	o$AIC=round(o$AIC,2)
	o$prob=round(o$prob,2)
	o$cums=round(o$cums,2)
	o$delta=round(o$delta,2)
oall = 	o

# TPR - ALL together
o=rbind(o1t,o2t)
	o$delta=round(o$AIC-min(o$AIC),2)
	o = o[order(o$delta),]
	o$prob=exp(-0.5*o$delta)/sum(exp(-0.5*o$delta))
	o$cums = cumsum(o$prob)
	o$ER=round(max(o$prob)/o$prob,2)
	o$AIC=round(o$AIC,2)
	o$prob=round(o$prob,2)
	o$cums=round(o$cums,2)
oallt = o

# EXPORT
	sname = tempfile(fileext='.xls')
		wb = loadWorkbook(sname,create = TRUE)	
		createSheet(wb, name = "DPR")
		writeWorksheet(wb, rbind(od,odl), sheet = "DPR")
		createSheet(wb, name = "TPR")
		writeWorksheet(wb, rbind(ot,otl), sheet = "TPR")
		
		createSheet(wb, name = "DPRall")
		writeWorksheet(wb, oall, sheet = "DPRall")
		createSheet(wb, name = "TPRall")
		writeWorksheet(wb, oallt, sheet = "TPRall")
	saveWorkbook(wb, paste(outdir,'TABLE_S4AIC.xls'))
