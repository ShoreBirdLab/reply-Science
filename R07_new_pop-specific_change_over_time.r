## New figures like Fig 2EF in Kubelka:

# Within each population where we have data before and after 2000, how does DPR change over time?

rm(list=ls(all=TRUE))
options(scipen=10,stringsAsFactors=FALSE)
memory.limit(size=8000000000)
setwd("C:/Users/emily/Documents/Work and research/ASDN/Science rebuttal/")
library(gplots)

# Also pull in their data to check which they already had:
kdat1 <- read.csv("Data from Kubelka et al/Kubelka et al._2018_Science_additional datafile_1.csv", header=TRUE)
kdat2 <- read.csv("Data from Kubelka et al/Before+After2000_final.csv", header=TRUE)

# Kubelka et al plotted mean +- SE across years, so get those from our yearly data:
bdat1 <- read.csv("Bulla et al database/Rebuttal_data_with_DPR_by_yr.csv", header=TRUE)	 # includes all our pops, even those excluded by Kubelka
bdat <- bdat1
bdat$start_year <- as.numeric(bdat$start_year)
bdat$end_year <- as.numeric(bdat$end_year)
bdat <- bdat[which(!is.na(bdat$DPR)),]
bdat[which(bdat$source_id==113),]


## Merge some sites following Kubelka:
bdat$locality[which(bdat$locality=="La Perouse Bay, Manitoba")] <- "Churchill, Manitoba"	
bdat$locality[which(bdat$locality=="Marambitsy Bay")] <- "Tsimanampetsotsa "
bdat$locality[which(bdat$locality %in% c("Central & S England", "N England", "Scotland"))] <-  "England, Scotland, Wales"

# add locality to kdat2, and use kdat2 from here down:
kdat2$Locality <- NA
for(i in 1:nrow(kdat2)){
	loc <- unique(kdat1$Locality[which(round(kdat1$Latitude,3)==round(kdat2$Latitude[i],3) & round(kdat1$Longitude,3)==round(kdat2$Longitude[i],3))])
	if(length(loc)>1) 	loc <- unique(kdat1$Locality[which(round(kdat1$Latitude,3)==round(kdat2$Latitude[i],3) & round(kdat1$Longitude,3)==round(kdat2$Longitude[i],3) & kdat1$species==kdat2$species[i])])
	kdat2$Locality[i] <- loc
}
any(is.na(kdat2$Locality))	# FALSE = ok!!

#################
# for our data:
# for each pop and period, get the mean and SE across years:
bdat2 <- bdat[,which(colnames(bdat) %in% c('species','locality','period','Belt','DPRtrans'))]
bdat2 <- unique(bdat2); nrow(bdat2)	

bdat2$DPR_mean <- bdat2$DPR_SE <- bdat2$TPR_mean <- bdat2$TPR_SE <- bdat2$Nyr <- bdat2$Nnests <- bdat2$nests_predated <- bdat2$ nests_abandoned <- bdat2$nests_infertile <- bdat2$nests_hatched <- bdat2$nests_failed_other_cause <- bdat2$Latitude <- bdat2$Longitude <- bdat2$mean_yr <-bdat2$obs_time <- bdat2$exposure_days <- bdat2$source_id <- bdat2$Incubation_days <- NA
bdat2[which(bdat2$species=="Vanellus_vanellus" & bdat2$locality=="Ceskobudejovicko, South Bohemia"),]

for(i in 1:nrow(bdat2)){
	mydat <- bdat[which(bdat$species==bdat2$species[i] & bdat$locality==bdat2$locality[i] & bdat$period==bdat2$period[i]),]
	bdat2$DPR_mean[i] <- mean(mydat$DPR)
	if(!any(is.na(mydat$exposure_days)) & !any(is.na(mydat$nests_predated))){
		bdat2$DPR_SE[i] <- sqrt(1 / 
			((sum(mydat$exposure_days)^3 /
			((sum(mydat$exposure_days)-
				sum(mydat$nests_predated))*sum(mydat$nests_predated)))))
				# following Johnson (ref 52)
	}
	bdat2$TPR_mean[i] <- mean(mydat$TPR)
	bdat2$TPR_SE[i] <- sd(mydat$TPR)/sqrt(nrow(mydat))
	bdat2$Nyr[i] <- sum(mydat$years_nr)
	bdat2$Nnests[i] <- sum(mydat$N.nests)
	bdat2$exposure_days[i] <- sum(mydat$exposure_days)
	bdat2$nests_hatched[i] <- sum(mydat$nests_hatched)
	bdat2$nests_predated[i] <- sum(mydat$nests_predated)
	bdat2$nests_abandoned[i] <- sum(mydat$nests_abandoned)
	bdat2$nests_infertile[i] <- sum(mydat$nests_infertile)
	bdat2$nests_failed_other_cause[i] <- sum(mydat$nests_failed_other_cause)
	bdat2$Latitude[i] <- mean(unique(mydat$Latitude))
	bdat2$Longitude[i] <- mean(unique(mydat$Longitude))
	bdat2$Incubation_days[i] <- mean(unique(mydat$Incubation_days))
	bdat2$source_id[i] <- paste(sort(unique(mydat$source_id)), collapse=",")
	if(!is.na(mydat$year[1])) if(mydat$year[1]=="all") bdat2$mean_yr[i] <- mean(unique(mydat$mean_year))
	if(!is.na(mydat$year[1])) if(mydat$year[1]!="all") bdat2$mean_yr[i] <- mean(unique(as.numeric(mydat$year)))
	bdat2$obs_time[i] <- paste(unique(mydat$obs_time),collapse=",")
#	if(length(unique(mydat$mean_year))>1) print(mydat)
}	
bdat2[which(is.na(bdat2$mean_yr)),]	# can ignore warning - we don't use that population here
# only pops with >=2 yrs per period, as mentioned in Kubelka supplement:
hist(bdat2$Nyr, breaks=20); nrow(bdat2)
bdat2 <- bdat2[which(bdat2$Nyr>=2),]; nrow(bdat2)
# and 12 nests TOTAL within each period:
bdat2 <- bdat2[which(bdat2$Nnests>=12),]; nrow(bdat2)


# Keep only the species-sites with data for both periods:
bdat2$pds <- NA
for(i in 1:nrow(bdat2)) bdat2$pds[i] <- length(unique(bdat2$period[which(bdat2$species==bdat2$species[i] & bdat2$locality==bdat2$locality[i])]))
bdat2 <- bdat2[which(bdat2$pds==2),]

# Note that Numenius_phaeopus at Churchill has 2 old datasets, only one of which needs transf:
bdat2[which(bdat2$species=="Numenius_phaeopus" & bdat2$locality=="Churchill, Manitoba"),]
# I have calculated the within-period DPR already so it is identical for both the "before" records, so we should remove one (part of "before" required trans, so keep trans=="YES"):
bdat2 <- bdat2[-which(bdat2$species=="Numenius_phaeopus" & bdat2$locality=="Churchill, Manitoba" & bdat2$period=="before" & bdat2$DPRtrans=="NO"),]
# Same for Calidris_alpina Lower Khatanga River (both "yes" and "no" after 2000, so we keep "yes"):
bdat2[which(bdat2$species=="Calidris_alpina" & bdat2$locality=="Lower Khatanga River"),]
bdat2 <- bdat2[-which(bdat2$species=="Calidris_alpina" & bdat2$locality=="Lower Khatanga River" & bdat2$period=="after" & bdat2$DPRtrans=="NO"),]
# and Calidris_melanotos, minua, Phalaropus_fulicarius, Philomachus_pugnax at LKRI:
bdat2 <- bdat2[-which(bdat2$species=="Calidris_melanotos" & bdat2$locality=="Lower Khatanga River" & bdat2$period=="after" & bdat2$DPRtrans=="NO"),]
bdat2 <- bdat2[-which(bdat2$species=="Calidris_minuta" & bdat2$locality=="Lower Khatanga River" & bdat2$period=="after" & bdat2$DPRtrans=="NO"),]
bdat2 <- bdat2[-which(bdat2$species=="Phalaropus_fulicarius" & bdat2$locality=="Lower Khatanga River" & bdat2$period=="after" & bdat2$DPRtrans=="NO"),]
bdat2 <- bdat2[-which(bdat2$species=="Philomachus_pugnax" & bdat2$locality=="Lower Khatanga River" & bdat2$period=="after" & bdat2$DPRtrans=="NO"),]

# How many pops total?
nrow(bdat2)/2	# 23! They had only 9.
# Also note that they excluded a bunch of data for some pops, so we might get different answers.


## Mark which of those were included by Kubelka, before and after:
# They have SESA at Kuparuk in their file, but with insufficient data (1 yr) so they did not actually include it:
kdat2 <- kdat2[-which(kdat2$years_nr==1),]
bdat2$K_incl <- 0
for(i in 1:nrow(bdat2)){
	myk <- kdat2[which(kdat2$species==bdat2$species[i] & kdat2$Locality==bdat2$locality[i]),]
	if(length(unique(myk$Period))==2) bdat2$K_incl[i] <- 1
}
bdat2$K_incl
# check what they have that we do not:
temp <- unique(cbind(bdat2$species, bdat2$locality, bdat2$K_incl, bdat2$Nnests))
temp[order(temp[,1]),]	# shows 1 row per period
# They have 2 that we do not (from paper):
	# Vanellus_vanellus, Ceskobudejovicko, South Bohemia 1988-2015: The "after" data were Kubelka's unpubl data that he chose not to share with us.
	# Calidris_melanotos at Kuparuk: Kubelka had 123 nests 1989-1993 in Moitoret and 18 in 2010-2013 in ASDN. BUT 7 of the 18 did not have "last day" and thus exp_days could not be directly calculated. Kubelka excluded those 7 from all other analyses (using direct exposure days for all ASDN nests) but used Beintema to include them here. This is such a small sample that I feel it should probably be excluded anyway - we have plenty of others to consider now.
length(which(bdat2$K_incl==0))/2
# We have 16 pops that they do not.

# Remove tropics:
bdat2 <- bdat2[which(bdat2$Latitude > 30),]



##########################
## Assumptions:

## How many required Beintema transformation in at least one period?
pops <- bdat2[,which(colnames(bdat2) %in% c('species', 'locality'))]
pops <- unique(pops)
pops$DPRtrans <- NA
for(i in 1:nrow(pops)) pops$DPRtrans[i] <- length(which(bdat2$species==pops$species[i] & bdat2$locality==pops$locality[i] & bdat2$DPRtrans=="YES"))
length(which(pops$DPRtrans==2))	# 8 required in both
length(which(pops$DPRtrans==1))	# 9 required in only one (this is "before" in all cases)
nrow(pops)	# out of 23 total, so 6 required no trans in either period. 6+8 = 14 are consistent (always transformed, or never transformed) while 9 are inconsistent (require trans before only).
# save this to bdat2 for later use:
bdat2$TransPer <- NA	# num periods transformed
for(i in 1:nrow(bdat2)){
	bdat2$TransPer[i] <- pops$DPRtrans[which(pops$species==bdat2$species[i] & pops$locality==bdat2$locality[i])]
}


#########################
## PLOT:	Pops included vs excluded by Kubelka:
dev.new(width=4, height=6)
par(mfrow=c(2,1), oma=c(3,3.5,0.1,0.5), mai=rep(0.1,4), mgp=c(2,0.7,0))

# Figure showing ONLY the pops they INCLUDED, aside from the two for which we don't have data. These are our estimates for these pops::
bdat3 <- bdat2[which(bdat2$K_incl==1),]
pops <- bdat3[,which(colnames(bdat3) %in% c('species', 'locality'))]
pops <- unique(pops)

plot(c(0,0), type="n", xlim=c(1960,2020), ylim=c(0,0.1), las=1, xaxs="i", yaxs="i", xaxt="n")
axis(side=1, at=seq(1960,2020,10), labels=FALSE)
mtext("Daily nest predation", side=2, line=3)
mtext(" a) Included by Kubelka et al.", side=3, line=-1.2, adj=0)
for(i in 1:nrow(pops)){
	b <- bdat3[which(bdat3$species==pops$species[i] & bdat3$locality==pops$locality[i] & bdat3$period=="before"),]
	a <- bdat3[which(bdat3$species==pops$species[i] & bdat3$locality==pops$locality[i] & bdat3$period=="after"),]
	lines(x=c(b$mean_yr, a$mean_yr), y=c(b$DPR_mean, a$DPR_mean))
	plotCI(x=c(b$mean_yr, a$mean_yr), y=c(b$DPR_mean, a$DPR_mean), uiw=c(b$DPR_SE, a$DPR_SE), gap=0, pch=21, pt.bg=c("cornflowerblue","red"), lty=1, add=TRUE,xpd=NA)
	text(x=a$mean_yr-0.5, y=a$DPR_mean, labels=paste0(round(a$Latitude,0),"°"), pos=4,cex=0.7)
}	# note that our values are very different from theirs in a few cases - they showed no declines in any pop.

	
# Figure showing ONLY the pops they EXCLUDED:
bdat4 <- bdat2[which(bdat2$K_incl==0),]
pops <- bdat4[,which(colnames(bdat4) %in% c('species', 'locality'))]
pops <- unique(pops)

plot(c(0,0), type="n", xlim=c(1960,2020), ylim=c(0,0.1), las=1, xaxs="i", yaxs="i")
mtext("Daily nest predation", side=2, line=3)
mtext("Year", side=1, line=2)
mtext(" b) Excluded by Kubelka et al.", side=3, line=-1.2, adj=0)
for(i in 1:nrow(pops)){
	b <- bdat4[which(bdat4$species==pops$species[i] & bdat4$locality==pops$locality[i] & bdat4$period=="before"),]
	a <- bdat4[which(bdat4$species==pops$species[i] & bdat4$locality==pops$locality[i] & bdat4$period=="after"),]
	lines(x=c(b$mean_yr, a$mean_yr), y=c(b$DPR_mean, a$DPR_mean))
	plotCI(x=c(b$mean_yr, a$mean_yr), y=c(b$DPR_mean, a$DPR_mean), uiw=c(b$DPR_SE, a$DPR_SE), gap=0, pch=21, pt.bg=c("cornflowerblue","red"), lty=1, add=TRUE, xpd=NA)
}


###########

## Test for an effect of period using all populations vs. only those they included:

## LME: DPR ~ period + latitude + (1|Species)
library(lme4)
# Kubelka used log(DPR)
hist(bdat2$DPR_mean)
hist(log(bdat2$DPR_mean+0.01))

# Only pops they included, but with our DPR; excluding the 2 for which we have no info (Kubelka Vanellus) or shouldn't be included (Kuparuk melanotos)
# make pd alphabetical first, second:
bdat3$period[which(bdat3$period=="before")] <- "1before"
bdat3$period[which(bdat3$period=="after")] <- "2after"
bdat3$period <- as.factor(bdat3$period)

mod <- lmer(log(DPR_mean+0.01) ~ period + scale(Latitude) + (1|species) + (1|locality), data=bdat3)	# they DID include locality in their model, though Table S5 footnote says only species.
drop1(mod,~.,test="Chisq")	# Shows what happens if you DROP each variable. Dropping 'period' isn't a great idea.
summary(mod) 	# NO LONGER any sig effect of period. Also, the effect of latitude is now in the opposite direction (though theirs was nowhere near sig and neither is ours).

## ALL POPS that were available to them (minus the 2 that we exclude):
bdat2$period[which(bdat2$period=="before")] <- "1before"
bdat2$period[which(bdat2$period=="after")] <- "2after"
mod2 <- lmer(log(DPR_mean+0.01) ~ as.factor(period) + as.numeric(Latitude) + (1|species) + (1|locality), data=bdat2)
drop1(mod2,~.,test="Chisq")	# Definitely don't drop period.
summary(mod2) 	# With all pops (except those 2), the period effect IS significant, with higher DPR after 2000.
2*pt(2.397, nrow(bdat3), lower=FALSE)

# Try with year (just for my interest):
mod2 <- lmer(log(DPR_mean+0.01) ~ as.numeric(scale(mean_yr)) + scale(Latitude) + (1|species) + (1|locality), data=bdat2)
summary(mod2) 
	# period*yr is not sig, with or without lat, so drop. With year, period is not sig, so keep only year, which is sig.

################

## same model as above, but ADD INTERACTION BT PERIOD AND TRANS GROUP:
bdat2$transgroup <- "consistent"
bdat2$transgroup[which(bdat2$TransPer==1)] <- "inconsistent"
summary(lmer(log(DPR_mean+0.01) ~ as.factor(period)*as.factor(bdat2$transgroup) + as.numeric(Latitude) + (1|species) + (1|locality), data=bdat2))


######################################################

## What if we use ONLY the pops that have a consistent transformation (NOT transformed in ONLY ONE period):

bdat2a <- bdat2[which(bdat2$TransPer!=1),]
# drop the low latitudes:
mod2a <- lmer(log(DPR_mean+0.01) ~ as.factor(period) + as.numeric(Latitude) + (1|species) + (1|locality), data=bdat2a)
summary(mod2a)	# NO period effect! (with or without the low latitude one)
2*pt(1.114, nrow(bdat2a), lower=FALSE)
nrow(unique(cbind(bdat2a$species, bdat2a$locality)))	
length(unique(bdat2a$locality))
unique(bdat2a$Latitude)

# Okay, so how about only the ones WITH transformation in only ONE period?
bdat2b <- bdat2[which(bdat2$TransPer==1),]	# all high latitudes
mod2b <- lmer(log(DPR_mean+0.01) ~ as.factor(period) + as.numeric(Latitude) + (1|species) + (1|locality), data=bdat2b)
summary(mod2b)	# YES period effect!
2*pt(2.498, nrow(bdat2a), lower=FALSE)
nrow(unique(cbind(bdat2b$species, bdat2b$locality)))
length(unique(bdat2b$locality))
# Is WHEN the transformation was needed consistent?
mydat <- bdat2[which(bdat2$species %in% bdat2b$species & bdat2$locality %in% bdat2b$locality),]
nrow(unique(cbind(mydat$species, mydat$locality)))
mydat$DPRtrans[which(mydat$period=="1before")]
mydat$DPRtrans[which(mydat$period=="2after")]
# Yes, all required the transformation BEFORE 2000.
mydat$obs_time[which(mydat$period=="1before")]	 # We used 0.5 or 0.6 always. If we'd used something higher, the increase over time would be exaggerated.
bdat2b[which(bdat2b$obs_time==0.6 & bdat2b$period=="1before"),]


# How much do we need to decrease obs_time to make the period effect disappear?
Bs <- seq(0.1,0.4,0.1)
bdat2b[,paste0("exp",Bs)] <- NA
bdat2b[,paste0("DPR",Bs)] <- bdat2b$DPR_mean	# copy the normal value - will retain this for "after 2000"

# calculate each exp from each B:
for(i in 1:nrow(bdat2b)){
	if(bdat2b$period[i]=="1before"){
	for(j in 1:length(Bs)){
		b <- Bs[j]
		# but in one case, we found info on methods, so retain that obs_time:
		if(bdat2b$obs_time[i]==0.6) b <- 0.6

		bdat2b[i,which(colnames(bdat2b)==paste0("exp",b))] <- sum(
			# hatched nests: obs_time*(inc_days/2) per nest
			bdat2b$nests_hatched[i]* (b*(bdat2b$Incubation_days[i])), 
			# infertile nests: obs_time*(inc_days/2)
			bdat2b$nests_infertile[i]* (b*(bdat2b$Incubation_days[i])), 
			# predated nests: obs_time*(inc_days/4)
			bdat2b$nests_predated[i]* (b*(bdat2b$Incubation_days[i]/2)), 
			# other_failed nests: obs_time*(inc_days/4)
			bdat2b$nests_failed_other_cause[i]*(b*(bdat2b$Incubation_days[i]/2)), na.rm=TRUE)
		bdat2b[i,which(colnames(bdat2b)==paste0("DPR",b))] <- bdat2b$nests_predated[i]/bdat2b[i,which(colnames(bdat2b)==paste0("exp",b))]
	}}
}
bdat2b[which(is.na(bdat2b$DPR0.1)),]	# oh... we have 2 old datasets for Churchill. Jehl provided nest numbers and required Beintema, but Skeel did not provide numbers - only a DSR. So I think we should exclude Churchill Numenius from this assessment of Beintema sensitivity. (The other Churchill species are only from Jehl, not Skeel.)
bdat2c <- bdat2b[-which(bdat2b$species=="Numenius_phaeopus" & bdat2b$locality=="Churchill, Manitoba"),]

## Run the model with various Beintema coefficients. Where is there a period effect?:
summary(lmer(log(DPR0.1+0.01) ~ as.factor(period) + as.numeric(Latitude) + (1|species) + (1|locality), data=bdat2c))	# NO, in fact, it is negative! (mean, nonsig)
2*pt(1.522, nrow(bdat2c), lower=FALSE)
summary(lmer(log(DPR0.2+0.01) ~ as.factor(period) + as.numeric(Latitude) + (1|species) + (1|locality), data=bdat2c))	# NO
2*pt(0.277, nrow(bdat2c), lower=FALSE)
summary(lmer(log(DPR0.3+0.01) ~ as.factor(period) + as.numeric(Latitude) + (1|species) + (1|locality), data=bdat2c))	# NO
2*pt(0.725, nrow(bdat2c), lower=FALSE)
summary(lmer(log(DPR0.4+0.01) ~ as.factor(period) + as.numeric(Latitude) + (1|species) + (1|locality), data=bdat2c))	# NO
2*pt(1.548, nrow(bdat2c), lower=FALSE)



################################


#### Plot the populations: one panel where the transformation was consistent (applied in both periods, or never applied) and one panel where the transformation was inconsistent (applied before 2000 only):

dev.new(width=8, height=4)
par(mfrow=c(1,2), oma=c(3,3.5,0.1,0.5), mai=rep(0.1,4), mgp=c(2,0.7,0))

# consistent pops:
pops <- bdat2a[,which(colnames(bdat2a) %in% c('species', 'locality'))]
pops <- unique(pops)

plot(c(0,0), type="n", xlim=c(1960,2019), ylim=c(0,0.1), las=1, xaxs="i", yaxs="i", xaxt="n")
axis(side=1, at=seq(1960,2020,10), labels=TRUE)
mtext("Daily nest predation", side=2, line=3)
mtext(" a) Consistent", side=3, line=-1.2, adj=0)
for(i in 1:nrow(pops)){
	b <- bdat2a[which(bdat2a$species==pops$species[i] & bdat2a$locality==pops$locality[i] & bdat2a$period=="1before"),]
	a <- bdat2a[which(bdat2a$species==pops$species[i] & bdat2a$locality==pops$locality[i] & bdat2a$period=="2after"),]
	lines(x=c(b$mean_yr, a$mean_yr), y=c(b$DPR_mean, a$DPR_mean))
	plotCI(x=c(b$mean_yr, a$mean_yr), y=c(b$DPR_mean, a$DPR_mean), uiw=c(b$DPR_SE, a$DPR_SE), gap=0, pch=21, pt.bg=c("cornflowerblue","red"), lty=1, add=TRUE,xpd=NA)
	text(x=a$mean_yr-0.5, y=a$DPR_mean, labels=paste0(round(a$Latitude,0),"°"), pos=4,cex=0.7)
	i <- i+1
}

	
# inconsistent pops:
pops <- bdat2b[,which(colnames(bdat2b) %in% c('species', 'locality'))]
pops <- unique(pops)

plot(c(0,0), type="n", xlim=c(1960,2019), ylim=c(0,0.1), las=1, xaxs="i", yaxs="i", yaxt="n")
mtext("Year", side=1, line=2)
mtext(" b) Transformed <2000 only", side=3, line=-1.2, adj=0)
for(i in 1:nrow(pops)){
	b <- bdat2b[which(bdat2b$species==pops$species[i] & bdat2b$locality==pops$locality[i] & bdat2b$period=="1before"),]
	a <- bdat2b[which(bdat2b$species==pops$species[i] & bdat2b$locality==pops$locality[i] & bdat2b$period=="2after"),]
	lines(x=c(b$mean_yr, a$mean_yr), y=c(b$DPR_mean, a$DPR_mean))
	plotCI(x=c(b$mean_yr, a$mean_yr), y=c(b$DPR_mean, a$DPR_mean), uiw=c(b$DPR_SE, a$DPR_SE), gap=0, pch=21, pt.bg=c("cornflowerblue","red"), lty=1, add=TRUE, xpd=NA)
	text(x=a$mean_yr-0.5, y=a$DPR_mean, labels=paste0(round(a$Latitude,0),"°"), pos=4,cex=0.7)
	axis(side=2, at=seq(0,0.1,0.02), labels=FALSE)
}

# Well, the consistent pops were all from 1990s while inconsistent were much older. So maybe the consistent pops simply haven't had time to show the trend.
sort(unique(bdat2b$source_id))
# Moitoret and Jehl were likely searching < weekly (Jehl combines data from many people and many methods and some nests could have been found opportunistically - who knows).


### For the inconsistent pops, show how DPR estimate (and change over time) depends on Beintema:
# one panel per pop:
pops <- bdat2c[,which(colnames(bdat2c) %in% c('species', 'locality'))]
pops <- unique(pops)
nrow(pops)
# shorter label for plot:
pops$loc2 <- splab <- NA
pops$loc2[grep("Kuparuk",pops$locality)] <- "Alaska"
pops$loc2[grep("Chaun",pops$locality)] <- "Russia"
pops$loc2[grep("Churchill",pops$locality)] <- "Canada"
pops$splab[grep("pusilla",pops$species)] <- "Ca. pusilla"
pops$splab[grep("alpina",pops$species)] <- "Ca. alpina"
pops$splab[grep("melanotos",pops$species)] <- "Ca. melanotos"
pops$splab[grep("minutilla",pops$species)] <- "Ca. minutilla"
pops$splab[grep("lobatus",pops$species)] <- "Ph. lobatus"
pops$splab[grep("semipalm",pops$species)] <- "Ch. semipalm."
pops$splab[grep("Limosa",pops$species)] <- "Li. haemastica"


dev.new(width=8.2, height=4)
par(mfrow=c(2,4), oma=c(3,3.5,0.1,0.5), mai=rep(0.1,4), mgp=c(2,0.7,0), cex=0.8)
Bs <- c(0.2,0.3,0.4) 	

for(i in 1:nrow(pops)){
	b <- bdat2c[which(bdat2c$species==pops$species[i] & bdat2c$locality==pops$locality[i] & bdat2c$period=="1before"),]
	a <- bdat2c[which(bdat2c$species==pops$species[i] & bdat2c$locality==pops$locality[i] & bdat2c$period=="2after"),]
#	ymax <- max(as.numeric(c(a$DPR_mean, as.vector(as.matrix(b[,grep("DPR", colnames(b))])))), na.rm=TRUE)
	plot(c(0,3000),rep(a$DPR_mean,2), type="n", lty=3, col="gray50", xlim=c(1955,2019), ylim=c(0,0.13), las=1, xaxs="i", yaxs="i", yaxt="n", xaxt="n")
	if(i==7) mtext("Year", side=1, line=2, at=par('usr')[1]-4)
	if(i==1) mtext("Daily predation rate", side=2, at=-0.01, line=3)
	mtext(paste0(" ", letters[i], ") ", pops$loc2[i],", ", pops$splab[i]), side=3, line=-1.2, adj=0,cex=0.8)
	axis(side=1, at=seq(1960,2020,10), labels=(i > 4))
	axis(side=2, at=seq(0,0.1,0.02), labels=(i %in% c(1,5)), las=2)
	# Original DPR, with its B value:
	plotCI(x=c(b$mean_yr, a$mean_yr), y=c(b$DPR_mean, a$DPR_mean), uiw=c(b$DPR_SE, a$DPR_SE), gap=0, pch=21, pt.bg=c("cornflowerblue","red"), lty=1, type="o", xpd=NA, add=TRUE, cex=1.5)
	if(b$nests_predated>0 & b$obs_time!=0.6) text(x=b$mean_yr-0.5, y=b$DPR_mean, labels=b$obs_time, pos=2,cex=0.8)
	# with other B values - only if num pred > 0 "before", else all estimates are the same (0):
	if(b$nests_predated>0){
		for(j in 1:length(Bs)){
			plotCI(x=c(b$mean_yr, a$mean_yr), y=c(b[,which(colnames(b)==paste0("DPR",Bs[j]))], a[,which(colnames(b)==paste0("DPR",Bs[j]))]), uiw=c(b$DPR_SE, a$DPR_SE), gap=0, pch=21, pt.bg=c("cornflowerblue","red"), lty=1, type="o", xpd=NA, add=TRUE, cex=1.5)
			if(j==1 & b$obs_time!=0.6) text(x=b$mean_yr-0.5, y=b[,which(colnames(b)==paste0("DPR",Bs[j]))], labels=Bs[j], pos=2,cex=0.8)
		}
		# add points again over error bars:
		for(j in 1:length(Bs)){
			points(x=c(b$mean_yr, a$mean_yr), y=c(b[,which(colnames(b)==paste0("DPR",Bs[j]))], a[,which(colnames(b)==paste0("DPR",Bs[j]))]), pch=21, bg=c("cornflowerblue","red"), xpd=NA, cex=1.5)
		}
		points(x=c(b$mean_yr, a$mean_yr), y=c(b$DPR_mean, a$DPR_mean), pch=21, bg=c("cornflowerblue","red"), xpd=NA, cex=1.5)

	}
}

# table will show same as Kubelka Table S4:
colnames(bdat2)
out <- data.frame(Species=bdat2$species, Location=bdat2$locality, Latitude=round(bdat2$Latitude,3), Longitude=round(bdat2$Longitude,3), Period=bdat2$period, DPR=round(bdat2$DPR_mean,3), SEM=round(bdat2$DPR_SE,3), Years=bdat2$Nyr, Mean_year=round(bdat2$mean_yr,0), N_nests=bdat2$Nnests, Exposure=round(bdat2$exposure_days,1), K_incl=bdat2$K_incl, B=bdat2$obs_time, TransPer=bdat2$TransPer, source_id=bdat2$source_id)
head(out)
out <- out[order(out$Period),]
out <- out[order(out$Location),]
out <- out[order(out$Species),]
out$B[which(out$TransPer==0)] <- NA
out$B[which(out$TransPer==1 & out$Period=="2after")] <- NA

# Add the reference:
out$Ref <- NA
for(i in 1:nrow(out)){
	out$Ref[i] <- paste(sort(unique(bdat$ref_abb[which(bdat$source_id %in% unlist(strsplit(out$source_id[i],",")))])),collapse=", ")
}

write.csv(out, "Table_within-pop_comparisons.csv", row.names=FALSE)




##############################################

########## NOT USED: we have only 2 pops here ###############

## YEARLY DATA: Is there a difference in the trend?

# for our data:
# only >=12 nests per year:

bdat5 <- bdat#[which(bdat$N.nests>=12),]	# they used >=12 total across all years within each period, but I really don't think we want small yearly samples here.
bdat5[which(bdat5$locality=="Zackenberg"),which(colnames(bdat5) %in% c('species','year','N.nests','period'))]	# Zberg has only 1 yr <2000 for each of 2 species, so is eliminated below


# Keep only the species-sites with 2+ yrs of data for both periods:
bdat5 <- bdat5[-which(bdat5$year=="all"),]
bdat5$pds <- bdat5$yrs <- NA
for(i in 1:nrow(bdat5)){
	bdat5$yrs[i] <- length(unique(bdat5$year[which(bdat5$species==bdat5$species[i] & bdat5$locality==bdat5$locality[i] & bdat5$period==bdat5$period[i])]))
}
bdat5 <- bdat5[which(bdat5$yrs>=2),]
bdat5[which(bdat5$locality=="Zackenberg"),]

for(i in 1:nrow(bdat5)){
	bdat5$pds[i] <- length(unique(bdat5$period[which(bdat5$species==bdat5$species[i] & bdat5$locality==bdat5$locality[i])]))
}
bdat5 <- bdat5[which(bdat5$pds==2),]
nrow(bdat5)	# num species-site-year
nrow(unique(cbind(bdat5$species, bdat5$locality)))	# 8 species-sites (with yearly data, before and after)


## Mark which of those were included by Kubelka, before and after:
bdat5$K_incl <- 0
for(i in 1:nrow(bdat5)){
	myk <- kdat2[which(kdat2$species==bdat5$species[i] & kdat2$Locality==bdat5$locality[i]),]
	if(length(unique(myk$Period))==2) bdat5$K_incl[i] <- 1
}
bdat5$K_incl
temp <- unique(cbind(bdat5$species, bdat5$locality, bdat5$K_incl, bdat5$Nnests))
temp[order(temp[,1]),]
# we only share Calidris_mauri at Nome - the others that they included do not have annual data.
	

##########
## LME: DPR ~ period [or year] + latitude + (1|Species)
# note there are only two locations here - lkri and nome:
unique(bdat5$locality)
unique(bdat5$species)	# but several species
# make pd alphabetical first, second:
bdat5$period[which(bdat5$period=="before")] <- "1before"
bdat5$period[which(bdat5$period=="after")] <- "2after"
bdat5$period <- as.factor(bdat5$period)
bdat5$year <- as.numeric(bdat5$year)

# First, check this subset of pops for an effect of period (mean DPR in each period):
keep <- rep(0, nrow(bdat2))
for(i in 1:length(keep)){
	mydat <- bdat5[which(bdat5$locality==bdat2$locality[i] & bdat5$species==bdat2$species[i]),]
	if(nrow(mydat)>0) keep[i] <- 1
}
bdat6 <- bdat2[which(keep==1),]
mod <- lmer(log(DPR_mean+0.01) ~ period + (Latitude) + (1|species) + (1|locality), data=bdat6)
summary(mod) 
# definitely no effect of period.


###### What about an effect of year?:
mod2 <- lmer(log(DPR+0.01) ~ as.numeric(year) + (Latitude) + (1|species) + (1|locality), data=bdat5)
summary(mod2) 
# no... unless we include Zackenberg (few nests per year), then YES
#### year + period?
mod2 <- lmer(log(DPR+0.01) ~ period*as.numeric(year) + (Latitude) + (1|species) + (1|locality), data=bdat5)
summary(mod2) 
# Well, WITH Zackenberg, we now see LOWER predn AFTER 2000 if we also include a year effect (increasing across years within each period)


## PLOT the ANNUAL data:
bdat5$locality[grep("Nome",bdat5$locality)] <- "Nome, Alaska"
bdat5$locality[grep("Khatanga",bdat5$locality)] <- "Lower Khatanga"
pops <- bdat5[,which(colnames(bdat5) %in% c('species', 'locality'))]
pops <- unique(pops)
nrow(pops)
range(bdat5$DPR)
range(bdat5$year)
bdat5$cols <- "cornflowerblue"
bdat5$cols[which(bdat5$year>=2000)] <- "red"
dev.new(width=8, height=7)
par(mfrow=c(4,4), oma=c(3,3.5,0.1,0.5), mai=rep(0.1,4), mgp=c(2,0.7,0),cex=1, cex.axis=0.8)

for(i in 1:nrow(pops)){
	plot(c(0,0), type="n", xlim=c(1990,2020), ylim=c(0,0.3), las=1, xaxs="i", yaxs="i", xaxt="n", yaxt="n")
	axis(side=1, at=seq(1990,2020,5), labels=(i>12))
	axis(side=2, at=seq(0,0.3,.05), labels=(i %in% seq(1,16,4)),las=2)
	if(i==5) mtext("Daily nest predation", side=2, line=3, at=0.32)
	b <- bdat5[which(bdat5$species==pops$species[i] & bdat5$locality==pops$locality[i]),]
	plotCI(x=b$year, y=b$DPR, uiw=0, gap=0, pch=21, pt.bg=b$cols, add=TRUE,xpd=NA)
	mtext(paste0(" ", pops$locality[i]), side=3, at=1990, adj=0, line=-1.1,cex=0.8)
	mtext(paste0(" ", pops$species[i]), side=3, at=1990, adj=0, line=-2.1,cex=0.8)
}	# ignore warnings for small SEs




