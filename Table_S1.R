rm( list = ls() )	

# set working and output directories
#wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
wd = "/Users/martinbulla/Dropbox/Science/ms_published/Kubelka_et_al_rebuttal/Analyses/"
outdir = '/Users/martinbulla/Dropbox/Science/ms_published/Kubelka_et_al_rebuttal/Outputs/'
#outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
source(paste(wd, 'Constants_Functions.R',sep=""))
source(paste(wd, 'Prepare_Data.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script

# TABLE S1AB- testing for the difference between belts using same model as Kubelka and comparing it with lmer output
    # DPR 
      # using Kubelka et al model from their Table S2A
        Model_1 = lmekin( log(DPR) ~ (1|species ) + log( N_nests)+ scale(mean_year)*Belt  , varlist = list( I, phyloMat , distanceMatrix ), data = d )
    	oi=data.frame(name='S2+int', type='fixed',effect=names(fixef(Model_1)),estimate=fixef(Model_1), lwr=fixef(Model_1)-1.96*extractSE(Model_1), upr=fixef(Model_1)+1.96*extractSE(Model_1))
    			rownames(oi) = NULL
    			oi$estimate_r=round(oi$estimate,3)
    			oi$lwr_r=round(oi$lwr,3)
    			oi$upr_r=round(oi$upr,3)
    	oii=oi[c('name','type',"effect", "estimate_r","lwr_r",'upr_r')]	
    	ri=data.frame(name='S2+int', type='random (var)',effect=c('# nest rec mat','phylo','distance','resid'), estimate_r=round(100*c(Model_1$vcoef$species,Model_1$sigma)/sum(c(Model_1$vcoef$species,Model_1$sigma))), lwr_r=NA, upr_r=NA)
    	ri$estimate_r = paste(ri$estimate_r,'%',sep='')
    	oo=rbind(oii,ri)
    	
      # using lmer package gives same results
        m = lmer(log(DPR) ~ log( N_nests) + scale(mean_year)*Belt +(1|species),  data = d)  
    	om = m_out(name = 'S1B_DPR',  model = m, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
    	m_ass(name = 'S1B_DPR_year-Belt', mo = m, dat = d, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
    	
    # TPR 
      # using Kubelka et al model from their Table S2A
        Model_7 = lmekin( TPR ~ (1|species ) + log( N_nests)+ scale(mean_year)*Belt  , varlist = list( I, phyloMat , distanceMatrix ), data = d )
    	oi=data.frame(name='S2+int',  type='fixed',effect=names(fixef(Model_7)),estimate=fixef(Model_7), lwr=fixef(Model_7)-1.96*extractSE(Model_7), upr=fixef(Model_7)+1.96*extractSE(Model_7))
    			rownames(oi) = NULL
    			oi$estimate_r=round(oi$estimate,3)
    			oi$lwr_r=round(oi$lwr,3)
    			oi$upr_r=round(oi$upr,3)
    	oii=oi[c('name','type',"effect", "estimate_r","lwr_r",'upr_r')]
    	ri=data.frame(name='S2+int',  type='random (var)',effect=c('# nest rec mat','phylo','distance','resid'), estimate_r=round(100*c(Model_7$vcoef$species,Model_7$sigma)/sum(c(Model_7$vcoef$species,Model_7$sigma))), lwr_r=NA, upr_r=NA)
    	ri$estimate_r = paste(ri$estimate_r,'%',sep='')
    	ot=rbind(oii,ri)
     
       # using lmer package gives same results)
        m = lmer(TPR ~ log( N_nests) + scale(mean_year)*Belt +(1|species),  data = d)  
        omt = m_out(name = 'S1B_TPR', model = m, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
    	m_ass(name = 'S1B_TPR_year-Belt', mo = m, dat = d, fixed = c('N_nests','mean_year'),categ = 'Belt', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)
        
# TABLE S1CD- testing for the difference between hemisphere,latitude and year using same model as Kubelka and comparing it with lmer output
# DPR 
  # using Kubelka et al model from their Table S6A
	model_x <- lmekin( log(DPR) ~ (1|species) + log( N_nests) + hemisphere*scale(mean_year) * scale(abs( Latitude )), varlist = list( I, phyloMat, distanceMatrix), data = d )
	oi=data.frame(name='S6+int',type='fixed',effect=names(fixef(model_x)),estimate=fixef(model_x), lwr=fixef(model_x)-1.96*extractSE(model_x), upr=fixef(model_x)+1.96*extractSE(model_x))
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
	oii=oi[c('name','type',"effect", "estimate_r","lwr_r",'upr_r')]	
	ri=data.frame(name='S6+int', type='random (var)',effect=c('# nest rec mat','phylo','distance','resid'), estimate_r=round(100*c(model_x$vcoef$species,model_x$sigma)/sum(c(model_x$vcoef$species,model_x$sigma))), lwr_r=NA, upr_r=NA)
	ol=rbind(oii,ri)
	
  # using lmer package	(same results)	
    m = lmer(log(DPR) ~ log( N_nests) + hemisphere*scale(mean_year)*scale(abs(Latitude)) +(1|species),  data = d)  
	oml = m_out(name = 'S1D_DPR',  model = m, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	m_ass(name = 'S1D_DPR_hem-year-absLat', mo = m, dat = d, fixed = c('N_nests','mean_year','Latitude'),categ = 'hemisphere', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)

# TPR 
  # using Kubelka et al model from their Table S6A
	model_y <- lmekin( TPR ~ (1|species) + log( N_nests) + hemisphere*scale(mean_year)*scale(abs( Latitude )), varlist = list( I, phyloMat, distanceMatrix), data = d )
	oi=data.frame(name='S6+int', type='fixed',effect=names(fixef(model_y)),estimate=fixef(model_y), lwr=fixef(model_y)-1.96*extractSE(model_y), upr=fixef(model_y)+1.96*extractSE(model_y))
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
	oii=oi[c('name','type',"effect", "estimate_r","lwr_r",'upr_r')]	
	ri=data.frame(name='S6+int', type='random (var)',effect=c('# nest rec mat','phylo','distance','resid'), estimate_r=round(100*c(model_y$vcoef$species,model_y$sigma)/sum(c(model_y$vcoef$species,model_y$sigma))), lwr_r=NA, upr_r=NA)
	olt=rbind(oii,ri)
  
  # using lmer package	(same results)	
    m = lmer(TPR ~ log( N_nests) + hemisphere*scale(mean_year)*scale(abs(Latitude)) +(1|species),  data = d)  
    omlt = m_out(name = 'S1D_TPR', model = m, round_ = 3, nsim = 5000, aic = FALSE, save_sim = paste0(wd, 'posteriory_simulations/'))
	m_ass(name = 'S1D_TPR_hem-year-absLat', mo = m, dat = d, fixed = c('N_nests','mean_year','Latitude'),categ = 'hemisphere', trans = c('log','none','none'), spatial = TRUE, temporal = TRUE, PNG = TRUE)	

# EXPORT model output
  l = list()
  l[['dpr']] = rbind(oo,om, ol,oml)
  l[['tpr']] = rbind(ot,omt, olt, omlt)
           
  sname = 'Table_S1'
  tmp = write_xlsx(l, paste0(outdir,sname,'.xlsx'))
  #openFile(tmp)   
  #shell(sname)