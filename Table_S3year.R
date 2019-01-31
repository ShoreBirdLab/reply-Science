rm( list = ls() )	

# set working and output directories
wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
source(paste(wd, 'Constants_Functions.R',sep=""))
source(paste(wd, 'Prepare_Data.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script

# TABLE S3year - latitude
# DPR
	# simple 	
     m0 = lmer(log(DPR) ~ log( N_nests) +hemisphere+ scale(mean_year)+lat_abs + (1|site)+(1|species),  data = d)  
   
	# interaction linear
     m1 = lmer(log(DPR) ~ log( N_nests) + hemisphere*scale(mean_year)*lat_abs +(1|site)+(1|species),  data = d)
	
	# simple 3rd polynomial
    m4 = lmer(log(DPR) ~ log( N_nests) + scale(mean_year)+poly(Latitude,3) +(1|site)+(1|species),  data = d) 
	
	# interaction 3rd polynomial
    m5 = lmer(log(DPR) ~ log( N_nests) + scale(mean_year)*poly(Latitude,3) +(1|site)+(1|species),  data = d)
  
	# model assumptions
	m_ass(name = 'DPR_year+hem+abs(lat)', mo = m0, dat = d, fixed = c('N_nests','mean_year', 'lat_abs'),categ = 'hemisphere', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'DPR_year-hem-abs(lat)', mo = m1, dat = d, fixed = c('N_nests','mean_year', 'lat_abs'),categ = 'hemisphere', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'DPR_year+poly3(lat)', mo = m4, dat = d, fixed = c('N_nests','mean_year', 'Latitude'),categ = NULL, trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'DPR_year-poly3(lat)', mo = m5, dat = d, fixed = c('N_nests','mean_year', 'Latitude'),categ = NULL, trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	
	
	# model outputs
	 om0 = m_out(name = "simple linear", model = m0, round_ = 3, nsim = 5000, aic = FALSE)
	 om1 = m_out(name = "interaction linear", model = m1, round_ = 3, nsim = 5000, aic = FALSE)
	 om4 = m_out(name = "simple poly3", model = m4, round_ = 3, nsim = 5000, aic = FALSE)
	 om5 = m_out(name = "interaction poly3", model = m5, round_ = 3, nsim = 5000, aic = FALSE)

    
# TPR
	# simple 	
     m0 = lmer(TPR ~ log( N_nests) +hemisphere+ scale(mean_year)+lat_abs + (1|site)+(1|species),  data = d)  
   
	# interaction linear
     m1 = lmer(TPR ~ log( N_nests) + hemisphere*scale(mean_year)*lat_abs +(1|site)+(1|species),  data = d)
	
	# simple 3rd polynomial
    m4 = lmer(TPR ~ log( N_nests) + scale(mean_year)+poly(Latitude,3) +(1|site)+(1|species),  data = d) 
	
	# interaction 3rd polynomial
    m5 = lmer(TPR ~ log( N_nests) + scale(mean_year)*poly(Latitude,3) +(1|site)+(1|species),  data = d)
  
	# model assumptions
	m_ass(name = 'TPR_year+hem+abs(lat)', mo = m0, dat = d, fixed = c('N_nests','mean_year', 'lat_abs'),categ = 'hemisphere', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'TPR_year-hem-abs(lat)', mo = m1, dat = d, fixed = c('N_nests','mean_year', 'lat_abs'),categ = 'hemisphere', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'TPR_year+poly3(lat)', mo = m4, dat = d, fixed = c('N_nests','mean_year', 'Latitude'),categ = NULL, trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'TPR_year-poly3(lat)', mo = m5, dat = d, fixed = c('N_nests','mean_year', 'Latitude'),categ = NULL, trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	
	# model outputs
	 om0t = m_out(name = "simple linear", model = m0, round_ = 3, nsim = 5000, aic = FALSE)
	 om1t= m_out(name = "interaction linear", model = m1, round_ = 3, nsim = 5000, aic = FALSE)
	 om4t = m_out(name = "simple poly3", model = m4, round_ = 3, nsim = 5000, aic = FALSE)
	 om5t = m_out(name = "interaction poly3", model = m5, round_ = 3, nsim = 5000, aic = FALSE)
	 #om3$AIC = NA
	 #om4 = m_out(name = "interaction poly >2000 A+T", model = m4, round_ = 3, nsim = 5000, aic = FALSE)
	 #om4$AIC = NA
    

# EXPORT
	sname = tempfile(fileext='.xls')
		wb = loadWorkbook(sname,create = TRUE)	
		createSheet(wb, name = "DPR")
		writeWorksheet(wb, rbind(om0,om1, om4,om5), sheet = "DPR")
	createSheet(wb, name = "TPR")
	writeWorksheet(wb, rbind(om0t,om1t, om4t,om5t), sheet = "TPR")
	saveWorkbook(wb, paste(outdir,'TABLE_S3year.xls'))
