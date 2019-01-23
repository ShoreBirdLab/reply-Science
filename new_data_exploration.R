rm( list = ls() )	

# set working and output directories
wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'
outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Outputs/'

# print figures in PNG or not
PNG = TRUE

# load packages, constants and data
source(paste(wd, 'Constants_Functions.R',sep=""))
source(paste(wd, 'Prepare_Data.R',sep="")) # generates 18 warnings, same way as Kubelke et al's script

# Emily's new pooled
d = read.csv(paste(wd,"Rebuttal_data_with_DPR_yrs_pooled_and_Kubelka.csv",sep=""), h = T, sep=",",stringsAsFactors = FALSE)
d$pk = 1:nrow(d)
d$Belt = as.factor(d$Belt)
d$abs_lat = abs(d$Latitude)
d$hemisphere =as.factor(ifelse(d$Latitude > 0, "Northern", "Southern"))
#d = d[d$mean_year>1900,]

m = lmer(log(DPR+0.01) ~ log(N.nests) + scale(mean_year)+ Belt +(1|Locality)+(1|species),  data =d )  
m = lmer(log(DPR_k+0.01) ~ log(N.nests) + scale(mean_year)+ Belt +(1|Locality)+(1|species),  data =d )  

m = lmer(log(DPR+0.01) ~ log(N.nests) + scale(mean_year)*Belt +(1|Locality)+(1|species),  data =d )  
m = lmer(log(DPR_k+0.01) ~ log(N.nests) + scale(mean_year)*Belt +(1|Locality)+(1|species),  data =d )

m = lmer(log(DPR+0.01) ~ log(N.nests) + poly(mean_year,2)*Belt +(1|Locality)+(1|species),  data =d )  
m = lmer(log(DPR_k+0.01) ~ log(N.nests) + poly(mean_year,2)*Belt +(1|Locality)+(1|species),  data =d )
summary(glht(m))
plot(allEffects(m))

# Emily's new yearly
d = read.csv(paste(wd,"Rebuttal_data_with_DPR_by_yr.csv",sep=""), h = T, sep=",",stringsAsFactors = FALSE)
d$pk = 1:nrow(d)
d$Belt = as.factor(d$Belt)
d$abs_lat = abs(d$Latitude)
d$hemisphere =as.factor(ifelse(d$Latitude > 0, "Northern", "Southern"))
d = d[- which(is.na(d$year)|is.na(d$DPR)),]
d[is.na(d$DPR),]
d$year_=as.numeric(ifelse(d$year=='all', ((d$start_year+d$end_year)/2), d$year))
d = d[d$N.nests>11,]

# relative year effects - CHECK DATA # of years
b = d[which(d$year!='all'),]
b$year = as.numeric(b$year)
b$n=1
b$pop = paste(b$source_id,b$species,b$locality)
x = ddply(b,.(pop), summarise, n =sum(n))
summary(factor(x$n))
bb= b[b$pop%in%x$pop[x$n>1],]
bb$years_nr = x$n[match(bb$pop,x$pop)]

nrow(bb)
length(unique(bb$pop))
bb = ddply(bb,.(pop), transform, year_relative = year-min(year))
bb[is.na(bb$year_relative),]
ggplot(bb,aes(y = DPR, x = year_relative)) + geom_point() + geom_smooth()
m = lmer(log(DPR+0.01)~year_relative +(1|species) + (1|locality),bb)
m = lmer(log(DPR+0.01)~year_relative*years_nr +(1|species) + (1|locality),bb)
m = lmer(log(DPR+0.01)~poly(year_relative,2)*years_nr +(1|species) + (1|locality),bb)
summary(glht(m))
plot(allEffects(m))

# Emily's data
d = read.csv(paste(wd,"Rebuttal_data_with_DPR.csv",sep=""), h = T, sep=",",stringsAsFactors = FALSE)
d$pk = 1:nrow(d)
d$Belt = as.factor(d$Belt)
d$abs_lat = abs(d$Latitude)
d$hemisphere =as.factor(ifelse(d$Latitude > 0, "Northern", "Southern"))
d = d[!is.na(d$year),]
d$year_=as.numeric(ifelse(d$year=='all', ((d$start_year+d$end_year)/2), d$year))
d = d[d$N.nests>11,]
nrow(d[d$DPRtrans=='NO',])
dd = d[d$DPRtrans=='NO',]
dd = dd[!is.na(log(dd$DPR+0.01)),]
densityplot(d$DPR)
ggplot(d,aes(x=DPR, y=N.nests))+geom_point()
#dd[,c("species","start_year","year","mean_year","end_year")]
d[which(d$DPR>0.4),]
m = lmer(log(DPR+0.01) ~ log(N.nests) + scale(year_)+ Belt +(1|locality)+(1|species),  data =dd )  
m = lmer(log(DPR+0.01) ~ log(N.nests) + scale(year_)+ Belt +(scale(year_)|locality)+(1|species),  data =dd )  
m = lmer(log(DPR+0.01) ~ log(N.nests) + scale(year_)*Belt +(1|locality)+(1|species),  data =dd[dd$Belt %in% c('Arctic','North temperate'),] )  
m = lmer(log(DPR+0.01) ~ log(N.nests) + scale(year_)*Belt +(scale(year_)|locality)+(1|species),  data = dd[dd$Belt %in% c('Arctic','North temperate'),] )   
plot(allEffects(m))
summary(glht(m))
summary(m)

m = lmer(log(DPR+0.01) ~ log(N.nests) + hemisphere*scale(abs_lat)*scale(year_) +(1|locality)+(1|species),  data =dd )
m = lmer(log(DPR+0.01) ~ log(N.nests) + poly(Latitude,3)*scale(year_) +(1|locality)+(1|species),  data =dd[!is.na(dd$Latitude),] )
ggplot(dd[dd$Belt %in% c('Arctic','North temperate'),],aes(x=year_, y = DPR, col = Belt)) + stat_smooth() + geom_point()

# Mine
d = read.csv(paste(wd,"Kubelka_et_al_rebuttal - lit_team.csv",sep=""), h = T, sep=",",stringsAsFactors = FALSE)
#d = readWorksheetFromFile(paste(wd, 'Kubelka_et_al_rebuttal.xlsx',sep = ""), colTypes = 'character', sheet = "lit_team")	
d$exposure_days= as.numeric(d$exposure_days)
d$overall_predation_rate= as.numeric(d$overall_predation_rate)
d$nests_predated = as.numeric(d$nests_predated)
d$nests_total = as.numeric(d$nests_total)
d$nests_total = as.numeric(d$nests_total)
d$start_year = as.numeric(d$start_year)
d$end_year = as.numeric(d$end_year)
d$year_=as.numeric(ifelse(d$year=='all', round((d$start_year+d$end_year)/2), d$year))
#d$daily_predation_rate= as.numeric(d$daily_predation_rate)
d = d[which(!is.na(d$daily_predation_rate)|!is.na(d$exposure_days)),]
d = d[!is.na(d$nests_total) & d$nests_total>9,]
d = d[-which(d$exposure_days==0),]
length(unique(d$locality))
d[,c('nests_predated','daily_predation_rate','overall_predation_rate','exposure_days')]
d[is.na(d$exposure_days),c('species','daily_predation_rate','overall_predation_rate','exposure_days')]

d$daily_predation_rate = ifelse(is.na(d$exposure_days), d$daily_predation_rate, d$nests_predated/d$exposure_days)
d = d[!is.na(d$daily_predation_rate),]
summary(d$daily_predation_rate)
d[,c("species","start_year","year","end_year")]

ggplot(d,aes(y =log(daily_predation_rate+0.01), x = year_))+stat_smooth()+geom_point()
ggplot(d,aes(y =log(daily_predation_rate+0.01), x = year_))+stat_smooth()+geom_point()
m = lmer(log(daily_predation_rate+0.01) ~ scale(year_) + (1|species) + (scale(year_)|locality),d)
m = lmer(log(daily_predation_rate+0.01) ~ scale(year_) + (1|species) + (scale(year_)|locality),d)
plot(allEffects(m))
summary(glht(m))




Sys.setenv(TZ="UTC") # set time to UTC to have always same time
require("anytime")
require("data.table")
require("googlesheets")

for_gs = gs_title("Kubelka_et_al_rebuttal")
d = data.table(gs_read(for_gs, ws = 'nests'))
a = data.table(gs_read(for_gs, ws = 'authors'))
sp = data.table(gs_read(for_gs, ws = 'species'))
s = data.table(gs_read(for_gs, ws = 'site'))
so = data.table(gs_read(for_gs, ws = 'source'))

# check predefined variables
nrow(d[!(author %in% unique(a[,abbreviation]))])
	summary(factor(a$abbreviation))
summary(factor(d$year))
nrow(d[!(species %in% unique(sp[,abbreviation]))])
	summary(factor(d$species))
	
nrow(d[!(site %in% unique(s[,abbreviation]))])
summary(factor(d$site))

summary(factor(d$first_egg_meth))
summary(factor(d$f_exp))
summary(factor(d$m_exp))
summary(factor(d$nest_exp))
summary(factor(d$study_site_exp))

# check continuous
summary(abs(as.numeric(d$lat)))
d[abs(lat)<10] 
summary(as.numeric(d$lon))
d[is.na(lon)]
summary(as.numeric(d$eggs))
summary(factor(d$eggs)) # how 0 or NA or 5-8 eggs possible?
nrow(d[is.na(eggs)])


# check
	d[, nest_found_t := anydate(nest_found, asUTC=TRUE)]
	d[, first_egg_t := anydate(first_egg, asUTC=TRUE)]
	d[, last_visit_before_end_t := anydate(last_visit_before_end, asUTC=TRUE)]
	d[, end_t := anydate(end_, asUTC=TRUE)]
d[is.na(nest_found_t)]
d[is.na(first_egg_t)]
d[is.na(end_t)]
d[is.na(last_visit_before_end_t)]

d[nest_found_t<first_egg_t & !is.na(first_egg)]
d[nest_found_t>last_visit_before_end_t]
d[nest_found_t>end_t]
d[first_egg_t>end_t]


