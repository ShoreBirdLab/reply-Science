rm( list = ls() )	

# set working and output directories
wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
source(paste(wd, 'Constants_Functions.R',sep=""))
source(paste(wd, 'Prepare_Data.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script
#source(paste(wd, 'Prepare_predictions_Fig1AB.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script
#img = readPNG(paste(outdir,'fig1aic_smaller.png',sep='')) # load AIC TABLE data


u=b[b$DPRtrans=='NO',]
u=u[-which(is.na(u$obs_time)| is.na(u$other_failed)),]
u$hatched[u$N.nests == 86 & u$Locality == 'South Uist, Hebrides'] = 86-19
u$infertile[u$N.nests == 86 & u$Locality == 'South Uist, Hebrides'] = 0
u = u[order(u$DPR_orig),]	

l = list()

# specific proportion of nesting used
for(i in 1:9){
	r = seq(0.1,0.9, by =0.1)[i]
	u$expMB = (r*u$Incubation_days*(u$other_failed +u$predated)/2)+(r*u$Incubation_days*(u$hatched+u$infertile))
    u$DPR_MB = u$predated/u$expMB
	u$beintema = paste(r, 'of nesting observed')
	
	l[[i]] = u[,c('DPR_orig','DPR_MB','beintema')]
	print(r)
	}
# variable proportion of nesting (as in Kubelka et al)	
	u$expMB = (u$obs_time*u$Incubation_days*(u$other_failed +u$predated)/2)+(u$obs_time*u$Incubation_days*(u$hatched+u$infertile))
    u$DPR_MB = u$predated/u$expMB
	u$beintema = 'variable as in Kubelka et al'
	l[[10]] = u[,c('DPR_orig','DPR_MB','beintema')]
uu = do.call(rbind,l)	

ggplot(uu,aes(x = DPR_orig, y = DPR_MB))+stat_smooth()+geom_point(alpha = 0.5)+geom_abline(intercept = 0, slope = 1, col = 'red', lty = 2)+	
		#geom_point(data = df2, aes(x = DPR_orig, y = DPR_MB), colour = "white")+
		facet_wrap(~beintema, ncol = 5,scales="free_y") + 
		ylab('Estimated daily predation rate') + xlab('True daily predation rate') 
		 #scale_x_continuous(trans = "log10")+scale_y_continuous(trans = "log10")

ggsave(paste(outdir,'Fig_SB_beintema_effects.png'),width = 10, height = 4, dpi = 600)
#ggsave(paste(outdir,'Fig_SB_beintema_effects.png'),width = 7, height = 6, dpi = 600)

