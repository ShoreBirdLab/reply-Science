rm( list = ls() )	

# set working and output directories
wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
source(paste(wd, 'Constants_Functions.R',sep=""))
source(paste(wd, 'Prepare_Data.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script

# TABLE S1- testing for the difference between belts using same model as Kubelka and comparing it with lmer output
# DPR 
  # using Kubelka et al model from their Table S2A
    Model_1 = lmekin( log(DPR) ~ (1|species ) + log( N_nests)+ mean_year*Belt  , varlist = list( I, phyloMat , distanceMatrix ), data = d )
	oi=data.frame(model='S2+int',type='fixed',effect=names(fixef(Model_1)),estimate=fixef(Model_1), lwr=fixef(Model_1)-1.96*extractSE(Model_1), upr=fixef(Model_1)+1.96*extractSE(Model_1))
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
	oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
	ri=data.frame(model='S2+int',type='random (var)',effect=c('# nest rec mat','phylo','distance','resid'), estimate_r=round(100*c(Model_1$vcoef$species,Model_1$sigma)/sum(c(Model_1$vcoef$species,Model_1$sigma))), lwr_r=NA, upr_r=NA)
	ri$estimate_r = paste(ri$estimate_r,'%',sep='')
	oo=rbind(oii,ri)
	
  # using lmer package gives same results
    m = lmer(log(DPR) ~ log( N_nests) + mean_year*Belt +(1|species),  data = d)  
	om = m_out(name = "lmer", model = m, round_ = 3, nsim = 5000, aic = FALSE)

# TPR 
  # using Kubelka et al model from their Table S2A
    Model_7 = lmekin( TPR ~ (1|species ) + log( N_nests)+ mean_year*Belt  , varlist = list( I, phyloMat , distanceMatrix ), data = d )
	oi=data.frame(model='S2+int',type='fixed',effect=names(fixef(Model_7)),estimate=fixef(Model_7), lwr=fixef(Model_7)-1.96*extractSE(Model_7), upr=fixef(Model_7)+1.96*extractSE(Model_7))
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
	oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
	ri=data.frame(model='S2+int',type='random (var)',effect=c('# nest rec mat','phylo','distance','resid'), estimate_r=round(100*c(Model_7$vcoef$species,Model_7$sigma)/sum(c(Model_7$vcoef$species,Model_7$sigma))), lwr_r=NA, upr_r=NA)
	ri$estimate_r = paste(ri$estimate_r,'%',sep='')
	ot=rbind(oii,ri)
 
   # using lmer package gives same results)
    m = lmer(TPR ~ log( N_nests) + mean_year*Belt +(1|species),  data = d)  
	omt = m_out(name = "lmer", model = m, round_ = 3, nsim = 5000, aic = FALSE)	
  
# EXPORT model outputs
  sname = tempfile(fileext='.xls')
  wb = loadWorkbook(sname,create = TRUE)	
  createSheet(wb, name = "DPR")
  writeWorksheet(wb, rbind(oo,om), sheet = "DPR")
  createSheet(wb, name = "TPR")
  writeWorksheet(wb, rbind(ot,omt), sheet = "TPR")
  saveWorkbook(wb, paste(outdir,'TABLE_S1.xls'))
  #shell(sname)