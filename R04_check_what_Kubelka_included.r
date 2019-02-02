
## Check what populations Kubelka included vs excluded, against our own literature extraction.
# < 1 min to run.

rm(list=ls(all=TRUE))
options(scipen=10,stringsAsFactors=FALSE)
memory.limit(size=8000000000)

# our data, from script R03
bdat <- read.csv("Rebuttal_data_with_DPR_yrs_pooled.csv", header=TRUE)
# Kubelka's data:
kdat <- read.csv("Kubelka et al._2018_Science_additional datafile_1.csv", header=TRUE)

# Exclude pops with <12 nests (over all years pooled):
bdat <- bdat[which(bdat$N.nests>=12),]

# Before we did NOT lump locations that Kubelka did, but should not have (>40 km away). Here we do this for the sake of comparison:

bdat$Locality[which(bdat$Locality=="La Perouse Bay, Manitoba")] <- "Churchill, Manitoba"
bdat$Locality[which(bdat$Locality=="Marambitsy Bay")] <- "Tsimanampetsotsa "
bdat$Locality[which(bdat$Locality %in% c("Central & S England", "N England", "Scotland"))] <-  "England, Scotland, Wales"

## Mark which species/localities/periods were included by Kubelka:
bdat$K_incl <- rep(NA, nrow(bdat))
for(i in 1:nrow(bdat)){
	myk <- kdat[which(kdat$Locality==bdat$Locality[i] & kdat$species==bdat$species[i]),]
	if(is.na(bdat$period[i])) bdat$K_incl[i] <- 1
	if(!is.na(bdat$period[i])){
		if(nrow(myk)>0){
			if(bdat$period[i]=="before") myk <- myk[which(myk$before=="yes"),]
			if(bdat$period[i]=="after") myk <- myk[which(myk$after=="yes"),]
		}
		bdat$K_incl[i] <- nrow(myk)
	}
}
bdat$K_incl	# many not included...:
length(which(bdat$K_incl==0 & !is.na(bdat$DPR)))
# we found data (and DPR) for 69 records that they missed! (includes before vs after; all >=10 nests)

#################################################################3

#### Check our data extraction against Kubelka's:

# From our dataset, remove all with DPR = NA; we either couldn't access these references (few cases) or deemed them insufficient to calculate DPR, so below they'll be identified as "Kubelka has but we do not":
bdat <- bdat[which(!is.na(bdat$DPR)),]	# remove NAs

# Check what Kubelka excluded:
excl <- bdat[which(bdat$K_incl==0),]
sort(excl$N.nests)	# many with very large samples
sort(excl$References.and.notes)	# Many, but not all, of the remainder excluded were ASDN and Zackenberg (Hansen).
sort(excl$Locality)	# various locations. Normally we will exclude Barrow 2005-2014, but Kubelka included those years for the 3 spp they included, so assume they mean to include all.

excl$obs_time	# most excluded allowed 0.6 assumption
excl$DPR_assumption	# most provided exp_days. Even where an assumption was required, it was always the same across pops within a source, and all of these are from sources K used.
excl$Incubation_days	# missing for only 1
excl$years_nr	# nearly all ran for >1 year
mean(excl$DPR[which(excl$period=="before")])	
mean(bdat$DPR[which(bdat$period=="before")])
	# excluded pops had HIGHER dpr BEFORE 2000 than those that were included...
mean(excl$DPR[which(excl$period=="after")])
mean(bdat$DPR[which(bdat$period=="after")])
	# excluded pops had LOWER dpr AFTER 2000 than those that were included (at least by our calculations).
	
# for each excluded pop, note whether they had the other time period:
excl$Other_per_incl <- NA
for(i in 1:nrow(excl)){
	mydat <- kdat[which(kdat$species==excl$species[i] & kdat$Locality==excl$Locality[i]),]
	if(nrow(mydat)>0) excl$Other_per_incl[i] <- "yes"
}
excl$Other_per_incl
length(which(excl$Other_per_incl=="yes"))

write.csv(excl, "Pops_Kubelka_excluded.csv", row.names=FALSE)
nrow(excl)	

write.csv(bdat, "Rebuttal_data_checked_K_incl.csv", row.names=FALSE)
