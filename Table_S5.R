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

	dd = d[d$mean_year>1999,]
	summary(factor(dd$Belt))
	
# TABLE S5 - control for year and study site
# DPR >1999
    m1 = lmer(log(DPR) ~ log( N_nests) + mean_year*Belt +(1|site)+(1|species),  data = d[d$mean_year>1999,])
	m_ass(name = 'DPR_yearIntBeltmore1999', mo = m1, dat = dd, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om1 = m_out(name = "DPR_year*Belt>1999", model = m1, round_ = 3, nsim = 5000, aic = FALSE)
	

# TPR
	m1 = lmer(TPR ~ log( N_nests) + mean_year*Belt +(1|site)+(1|species),  data = d[d$mean_year>1999,])
	m_ass(name = 'TPR_yearIntBeltmore1999', mo = m1, dat = dd, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om1t = m_out(name = "TPR_year*Belt>1999", model = m1, round_ = 3, nsim = 5000, aic = FALSE)
	
# EXPORT
	sname = tempfile(fileext='.xls')
		wb = loadWorkbook(sname,create = TRUE)	
		createSheet(wb, name = "DPR")
		writeWorksheet(wb, rbind(om1), sheet = "DPR")
	createSheet(wb, name = "TPR")
	writeWorksheet(wb, rbind(om1t), sheet = "TPR")
	saveWorkbook(wb, paste(outdir,'TABLE_S5.xls'))
