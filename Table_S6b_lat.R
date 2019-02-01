# we do not find strong and consistent statistical evidance for arctic being different from other belts or temperate belt... instead, models without the interaction seem to be fitting the data better.

rm( list = ls() )	

# set working and output directories
wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
source(paste(wd, 'Constants_Functions.R',sep=""))
source(paste(wd, 'Prepare_Data.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script

	dd_ = d[d$DPR_tran == 'NO',]
	summary(factor(dd_$Belt))

# TABLE S6 - control for year and study site
# DPR non-transfomred data
   	m0 = lmer(log(DPR) ~ log( N_nests) + hemisphere + scale(mean_year)+scale(lat_abs) +(1|site)+(1|species),  data =  dd_)
	m_ass(name = 'DPR_hem+year+abslatTRUE', mo = m0, dat = dd_, fixed = c('N_nests','mean_year', 'lat_abs'),categ = 'hemisphere', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om0 = m_out(name = "DPR_hem+year+abslatTRUE", model = m0, round_ = 3, nsim = 5000, aic = FALSE)
	
	m0p = lmer(log(DPR) ~ log( N_nests) + scale(mean_year)+poly(Latitude,3) +(1|site)+(1|species),  data =  dd_)
	m_ass(name = 'DPR_year+Poly3LatTRUE', mo = m0p, dat = dd_, fixed = c('N_nests','mean_year', 'Latitude'),categ = NULL, trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om0p = m_out(name = "DPR_year+Poly3LatTRUE'", model = m0p, round_ = 3, nsim = 5000, aic = FALSE)
	
	m1 = lmer(log(DPR) ~ log( N_nests) + hemisphere*scale(mean_year)*scale(lat_abs) +(1|site)+(1|species),  data =  dd_)
	m_ass(name = 'DPR_HemYearAbslatTRUE', mo = m1, dat = dd_, fixed = c('N_nests','mean_year', 'lat_abs'),categ = 'hemisphere', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om1 = m_out(name = "DPR_HemYearAbslatTRUE", model = m1, round_ = 3, nsim = 5000, aic = FALSE)
	
	m1p = lmer(log(DPR) ~ log( N_nests) + scale(mean_year)*poly(Latitude,3) +(1|site)+(1|species),  data = dd_)
	m_ass(name = 'DPR_yearIntPoly3LatTRUE', mo = m1p, dat = dd_, fixed = c('N_nests','mean_year', 'Latitude'),categ = NULL, trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om1p = m_out(name = "DPR_yearIntPoly3LatTRUE", model = m1p, round_ = 3, nsim = 5000, aic = FALSE)

# TPR
   	m0 = lmer(TPR ~ log( N_nests) + hemisphere + scale(mean_year)+scale(lat_abs) +(1|site)+(1|species),  data =  dd_)
	m_ass(name = 'TPR_hem+year+abslatTRUE', mo = m0, dat = dd_, fixed = c('N_nests','mean_year', 'lat_abs'),categ = 'hemisphere', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om0t = m_out(name = "TPR_hem+year+abslatTRUE", model = m0, round_ = 3, nsim = 5000, aic = FALSE)
	
	m0p = lmer(TPR ~ log( N_nests) + scale(mean_year)+poly(Latitude,3) +(1|site)+(1|species),  data =  dd_)
	m_ass(name = 'DTPR_year+Poly3LatTRUE', mo = m0p, dat = dd_, fixed = c('N_nests','mean_year', 'Latitude'),categ = NULL, trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om0pt = m_out(name = "TPR_year+Poly3LatTRUE'", model = m0p, round_ = 3, nsim = 5000, aic = FALSE)
	
	m1 = lmer(TPR ~ log( N_nests) + hemisphere*scale(mean_year)*scale(lat_abs) +(1|site)+(1|species),  data =  dd_)
	m_ass(name = 'TPR_HemYearAbslatTRUE', mo = m1, dat = dd_, fixed = c('N_nests','mean_year', 'lat_abs'),categ = 'hemisphere', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om1t = m_out(name = "TPR_HemYearAbslatTRUE", model = m1, round_ = 3, nsim = 5000, aic = FALSE)
	
	m1p = lmer(TPR ~ log( N_nests) + scale(mean_year)*poly(Latitude,3) +(1|site)+(1|species),  data = dd_)
	m_ass(name = 'TPR_yearIntPoly3LatTRUE', mo = m1p, dat = dd_, fixed = c('N_nests','mean_year', 'Latitude'),categ = NULL, trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om1pt = m_out(name = "TPR_yearIntPoly3LatTRUE", model = m1p, round_ = 3, nsim = 5000, aic = FALSE)

# EXPORT
	sname = tempfile(fileext='.xls')
		wb = loadWorkbook(sname,create = TRUE)	
		createSheet(wb, name = "DPR")
		writeWorksheet(wb, rbind(om0,om1,om0p,om1p), sheet = "DPR")
	createSheet(wb, name = "TPR")
	writeWorksheet(wb, rbind(om0t,om1t, om0pt,om1pt), sheet = "TPR")
	saveWorkbook(wb, paste(outdir,'TABLE_S6lat.xls'))
