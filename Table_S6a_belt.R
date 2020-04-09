# we do not find strong and consistent statistical evidence for arctic being different from other belts or temperate belt... instead, models without the interaction seem to be fitting the data better.

rm( list = ls() )	

# set working and output directories
wd = "/Users/martinbulla/Dropbox/Science/ms_published/Kubelka_et_al_rebuttal/Analyses/"
outdir = '/Users/martinbulla/Dropbox/Science/ms_published/Kubelka_et_al_rebuttal/Outputs/'
#wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
#outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
source(paste(wd, 'Constants_Functions.R',sep=""))
source(paste(wd, 'Prepare_Data.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script

	dd = d[d$DPR_tran == 'NO',]
	summary(factor(dd$Belt))
	dd_ = dd[dd$Belt %in% c('Arctic','North temperate'),]
# TABLE S6 - control for year and study site
# DPR non-transfomred data
   	m0 = lmer(log(DPR) ~ log( N_nests) + scale(mean_year)+Belt +(1|site)+(1|species),  data =  dd[dd$Belt %in% c('Arctic','North temperate'),])
	m_ass(name = 'DPR_year+BeltTRUE_AT', mo = m0, dat = dd_, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om0 = m_out(name = "S6aA DPR_year+BeltTRUE_AT", model = m0, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	
	m0p = lmer(log(DPR) ~ log( N_nests) + poly(mean_year,2)+Belt +(1|site)+(1|species),  data =  dd[dd$Belt %in% c('Arctic','North temperate'),])
	m_ass(name = 'DPR_polyyear+BeltTRUE_AT', mo = m0p, dat = dd_, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om0p = m_out(name = "S5aC DPR_polyyear+BeltTRUE_AT'", model = m0p, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	
	m1 = lmer(log(DPR) ~ log( N_nests) + scale(mean_year)*Belt +(1|site)+(1|species),  data =  dd[dd$Belt %in% c('Arctic','North temperate'),])
	m_ass(name = 'DPR_yearIntBeltTRUE_AT', mo = m1, dat = dd_, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om1 = m_out(name = "S5aB DPR_yearIntBeltTRUE_AT", model = m1, round_ = 3, nsim = 5000, aic = FALSE)
	
	m1p = lmer(log(DPR) ~ log( N_nests) + poly(mean_year,2)*Belt +(1|site)+(1|species),  data = dd[dd$Belt %in% c('Arctic','North temperate'),])
	m_ass(name = 'DPR_polyyearIntBeltTRUE_AT', mo = m1p, dat = dd_, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om1p = m_out(name = "S6aD DPR_polyyearIntBeltTRUE_AT", model = m1p, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))

# TPR
	m0 = lmer(TPR ~ log( N_nests) + scale(mean_year)+Belt +(1|site)+(1|species),  data =  dd[dd$Belt %in% c('Arctic','North temperate'),])
	m_ass(name = 'TPR_year+BeltTRUE_AT', mo = m0, dat = dd_, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om0t = m_out(name = "S6aA TPR_year+BeltTRUE_AT", model = m0, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	
	m0p = lmer(TPR ~ log( N_nests) + poly(mean_year,2)+Belt +(1|site)+(1|species),  data =  dd[dd$Belt %in% c('Arctic','North temperate'),])
	m_ass(name = 'TPR_polyyear+BeltTRUE_AT', mo = m0p, dat = dd_, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om0pt = m_out(name = "S6aC TPR_polyyear+BeltTRUE_AT'", model = m0p, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	
	m1 = lmer(TPR ~ log( N_nests) + mean_year*Belt +(1|site)+(1|species),  data =  dd[dd$Belt %in% c('Arctic','North temperate'),])
	m_ass(name = 'TPR_yearIntBeltTRUE_AT', mo = m1, dat = dd_, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om1t = m_out(name = "S6aB TPR_yearIntBeltTRUE_AT", model = m1, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	
	m1p = lmer(TPR ~ log( N_nests) + poly(mean_year,2)*Belt +(1|site)+(1|species),  data = dd[dd$Belt %in% c('Arctic','North temperate'),])
	m_ass(name = 'TPR_polyyearIntBeltTRUE_AT', mo = m1p, dat = dd_, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	om1pt = m_out(name = "S6aD TPR_polyyearIntBeltTRUE_AT", model = m1p, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))

# EXPORT model output
  l = list()
  l[['dpr']] = rbind(om0,om1,om0p,om1p)
  l[['tpr']] = rbind(om0t,om1t, om0pt,om1pt)
           
  sname = 'Table_S6a_belt'
  tmp = write_xlsx(l, paste0(outdir,sname,'.xlsx'))
  #openFile(tmp)   
  #shell(sname)
