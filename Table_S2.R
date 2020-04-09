# we do not find strong and consistent statistical evidance for arctic being different from other belts or temperate belt... instead, models without the interaction seem to be fitting the data better.

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

# TABLE S2 - control for year and study site
# DPR
	# simple	
     m0 = lmer(log(DPR) ~ log( N_nests) + scale(mean_year)+Belt   +(1|site)+(1|species),  data = d)  
		#m0 = lmer(log(DPR) ~ log( N_nests) + mean_year+Belt   +(1|site)+(1|species),  data = d[d$mean_year>1970,])  
		#m0 = lmer(log(DPR) ~ log( N_nests) + mean_year+Belt   +(1|site)+(1|species),  data = d[d$DPR_trans=='NO',])  
		#m0 = lmer(log(DPR) ~ log( N_nests) + mean_year+Belt   +(1|site)+(1|species),  data = d[d$DPR_trans=='NO' & d$mean_year>1970,])  
		#m = lmer(log(DPR) ~ log( N_nests) + mean_year+Belt   +(1|site)+(1|species),  data = d[d$years_nr<11 & d$mean_year>1970,])   
		
	# interaction linear
    m1 = lmer(log(DPR) ~ log( N_nests) + scale(mean_year)*Belt +(1|site)+(1|species),  data = d)
 
    # interaction 2nd polynomial
    m2 = lmer(log(DPR) ~ log( N_nests) + poly(mean_year,2)*Belt + (1|site)+(1|species),  data = d)

	# simple 2nd polynomial
    m3 = lmer(log(DPR) ~ log( N_nests) + poly(mean_year,2)+Belt +(1|site)+(1|species),  data = d)
  
	# >2000
	m4 = lmer(log(DPR) ~ log( N_nests) + poly(mean_year,2)*Belt +(1|site)+(1|species),  data = d[d$mean_year>2000,])
	
	# >2000 & only Arctic and N. Temperate
	m5 = lmer(log(DPR) ~ log( N_nests) + poly(mean_year,2)*Belt +(1|site)+(1|species),  data = d[d$mean_year>2000 & d$Belt%in%c('North temperate','Arctic'),])
	
	# only Arctic and N. Temperate
	m6 = lmer(log(DPR) ~ log( N_nests) + poly(mean_year,2)*Belt +(1|site)+(1|species),  data = d[d$Belt%in%c('North temperate','Arctic'),])
	
	# model assumptions
	m_ass(name = 'DPR_year+Belt', mo = m0, dat = d, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'DPR_yearIntBelt', mo = m1, dat = d, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'DPR_POLYyearIntBelt', mo = m2, dat = d, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'),spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'DPR_polyyear+Belt', mo = m2, dat = d, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'),spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'DPR_yearIntBelt2000', mo = m4, dat = d[d$mean_year>2000,], fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'DPR_yearIntBelt2000AT', mo = m5, dat = d[d$mean_year>2000 & d$Belt%in%c('North temperate','Arctic'),], fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'),spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'DPR_yearIntBelt-AT', mo = m6, dat = d[d$Belt%in%c('North temperate','Arctic'),], fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'),spatial = TRUE, temporal = TRUE, PNG = TRUE)
	# model outputs
	 om0 = m_out(name = "S2A DPR simple linear", model = m0, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	 om1 = m_out(name = "S2B DPR interaction linear", model = m1, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	 om2 = m_out(name = "S2D DPR interaction poly", model = m2, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
		bsim <- sim(m2, n.sim=nsim)  	 
		sum(bsim@fixef[,c('poly(mean_year, 2)2:BeltNorth temperate')]<0)/nsim #	bsim@fixef[6,7]
	 om3 = m_out(name = "S2C DPR simple poly", model = m3, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	 om5 = m_out(name = "interaction poly >2000 A+T", model = m5, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
		bsim <- sim(m2, n.sim=nsim)  	 
		sum(bsim@fixef[,c('poly(mean_year, 2)2:BeltNorth temperate')]<0)/nsim #	bsim@fixef[6,7]
	 om6 = m_out(name = "interaction poly all A+T", model = m5, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
		bsim <- sim(m6, n.sim=nsim)  	 
		sum(bsim@fixef[,c('poly(mean_year, 2)2:BeltNorth temperate')]>0)/nsim #	bsim@fixef[6,7]
# TPR
	# simple	
     m0 = lmer(TPR ~ log( N_nests) + scale(mean_year)+Belt   +(1|site)+(1|species),  data = d)  
   
	# interaction linear
    m1 = lmer(TPR ~ log( N_nests) + scale(mean_year)*Belt +(1|site)+(1|species),  data = d)
 
    # interaction 2nd polynomial
    m2 = lmer(TPR ~ log( N_nests) + poly(mean_year,2)*Belt +(1|site)+(1|species),  data = d)
	
	# simple 2nd polynomial
    m3 = lmer(TPR ~ log( N_nests) + poly(mean_year,2)+Belt +(1|site)+(1|species),  data = d)
  
	# >2000
	m4 = lmer(TPR ~ log( N_nests) + poly(mean_year,2)*Belt +(1|site)+(1|species),  data = d[d$mean_year>2000,])
	
	# >2000 & only Arctic and N. Temperate
	m5 = lmer(TPR ~ log( N_nests) + poly(mean_year,2)*Belt +(1|site)+(1|species),  data = d[d$mean_year>2000 & d$Belt%in%c('North temperate','Arctic'),])

	# model assumptions
	m_ass(name = 'TPR_year+Belt', mo = m0, dat = d, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'TPR_yearIntBelt', mo = m1, dat = d, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'TPR_POLYyearIntBelt', mo = m2, dat = d, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'),spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'TPR_polyyear+Belt', mo = m2, dat = d, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'),spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'TPR_yearIntBelt2000', mo = m4, dat = d[d$mean_year>2000,], fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
	m_ass(name = 'TPR_yearIntBelt2000AT', mo = m5, dat = d[d$mean_year>2000 & d$Belt%in%c('North temperate','Arctic'),], fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'),spatial = TRUE, temporal = TRUE, PNG = TRUE)
	
	# model outputs
	 om0t = m_out(name = "S2A TPR simple linear", model = m0, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	 om1t = m_out(name = "S2B TPR interaction linear", model = m1, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	 om2t = m_out(name = "S2D TPR interaction poly", model = m2, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	 om3t = m_out(name = "S2C TPR simple poly", model = m3, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
		bsim <- sim(m2, n.sim=nsim)  	 
		sum(bsim@fixef[,c('poly(mean_year, 2)2:BeltNorth temperate')]<0)/nsim #	bsim@fixef[6,7]
# EXPORT
  l = list()
  l[['dpr']] = rbind(om0,om1, om3,om2)
  l[['tpr']] = rbind(om0t,om1t, om3t,om2t)
           
  sname = 'Table_S2'
  tmp = write_xlsx(l, paste0(outdir,sname,'.xlsx'))
  #openFile(tmp)   
  #shell(sname)

