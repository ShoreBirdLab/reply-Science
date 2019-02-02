### From our extraction from the literature, use the processed dataset (from script R04a - format data) to calculate DPR (or assume DPR = 1-DSR where needed).

# This script calculates DPR by site, for YEARS POOLED to compare with Kubelka. They say "When data were available for >1 yr, the sum of depredated nests and overall exposure were pooled over years to obtain mean predation values." If only DSR is given, I will assume they averaged DSR across years.
# Saves a file that includes DPR for each population, across years pooled.
# < 1 min to run.


rm(list=ls(all=TRUE))
options(scipen=10,stringsAsFactors=FALSE)
memory.limit(size=8000000000)

bdat <- read.csv("Rebuttal_data_processed.csv", header=TRUE)	# from R01 script

# Kubelka's data:
kdat <- read.csv("Kubelka et al._2018_Science_additional datafile_1.csv", header=TRUE)
	# Identical to the data tab from the .xlsx file of the same name, as provided by Kubelka; copied to csv for ease of use in R.


## Summarize our data by species/locality (across sources and years):
# Set up a blank table to receive the summary numbers, using colnames to match kdat (some columns can be ignored for now; unimportant to check):
sdat <- data.frame(species=bdat$species, Locality=bdat$locality, Latitude=bdat$Latitude, Longitude=bdat$Longitude, Belt=bdat$Belt, period=bdat$period, daily_survival_rate=NA, daily_predation_rate=NA, DPRtrans=NA, DPR_assumption=NA, Incubation_days=bdat$Incubation_days, source_id=NA)
sdat <- unique(sdat)
sdat[,colnames(kdat)[-which(colnames(kdat) %in% c('species','Locality','Latitude', 'Longitude', 'State', 'before', 'after', 'family','Incubation_days','TPR_orig','DPR_orig'))]] <- NA
colnames(sdat)

# For each species/locality, summarize the data we have, across years and sources:
i <- 1	# to test loop
for(i in 1:nrow(sdat)){
	# data for this species-site-period:
	mydat <- bdat[which(bdat$species==sdat$species[i] & bdat$locality==sdat$Locality[i] & bdat$period==sdat$period[i] & bdat$who!="bib_await"),]
	if(nrow(mydat)>0){
		# Nest numbers:
		# make sure we fill 0s only when we truly extracted zeros (not all NAs):
		if(any(!is.na(mydat$nests_predated))) sdat$predated[i] <- sum(mydat$nests_predated, na.rm=TRUE)
		if(any(!is.na(mydat$nests_hatched))) sdat$hatched[i] <- sum(mydat$nests_hatched, na.rm=TRUE)
		if(any(!is.na(mydat$nests_infertile))) sdat$infertile[i] <- sum(mydat$nests_infertile, na.rm=TRUE)
		if(any(!is.na(mydat$nests_failed_other_cause)) | any(!is.na(mydat$nests_abandoned))) sdat$other_failed[i] <- sum(mydat$nests_failed_other_cause, mydat$nests_abandoned, na.rm=TRUE)
		if(any(!is.na(c(sdat$predated[i], sdat$infertile[i], sdat$other_failed[i])))){
			sdat$Failed_together[i] <- sum(c(sdat$predated[i], sdat$infertile[i], sdat$other_failed[i]), na.rm=TRUE)
		} else sdat$Failed_together[i] <- sum(mydat$Failed_together)
		sdat$N.nests[i] <- sum(c(sdat$hatched[i], sdat$Failed_together[i]), na.rm=TRUE)	# excluding abandoned and unknown-fate
		sdat$Exposure_days[i] <- sum(mydat$exposure_days,na.rm=TRUE)
		
		# overall rates (must take the average; these will be used only if we don't have nest numbers):
		sdat$daily_survival_rate[i] <- mean(mydat$daily_survival_rate, na.rm=TRUE)
		sdat$daily_predation_rate[i] <- mean(mydat$daily_predation_rate,na.rm=TRUE) 
		
		# Study methods:
		sdat$obs_time[i] <- min(mydat$obs_time, na.rm=TRUE)
		sdat$mean_year[i] <- mean(mydat$mean_year, na.rm=TRUE)
		if("all" %in% mydat$year) ny <- length(c(min(mydat$start_year,na.rm=TRUE):max(mydat$end_year,na.rm=TRUE)))
		if(!("all" %in% mydat$year)) ny <- length(unique(mydat$year))
		sdat$years_nr[i] <- ny
		if("all" %in% mydat$year) sdat$years[i] <- paste0(min(mydat$start_year,na.rm=TRUE),"-", max(mydat$end_year,na.rm=TRUE))
		if(!("all" %in% mydat$year)){
			if(length(unique(mydat$year))==sdat$years_nr[i]) sdat$years[i] <- paste0(min(mydat$start_year,na.rm=TRUE),"-", max(mydat$end_year,na.rm=TRUE))
			if(length(unique(mydat$year))!=sdat$years_nr[i]) sdat$years[i] <- paste0(sort(unique(mydat$year)), collapse=", ")
		}

		sdat$DPR_assumption[i] <- paste(unique(mydat$DPR_assumption),collapse=", ")
		sdat$DPRtrans[i] <- paste(unique(mydat$DPRtrans),collapse=", ")

		# Source:
		sdat$References.and.notes[i] <- paste(sort(unique(mydat$ref_abb)),collapse=", ")
		sdat$source_id[i] <- paste(unique(mydat$source_id),collapse=", ")
	}
}	# records with no data (yet) are excluded here
head(sdat)
sdat$N.nests[which(sdat$N.nests==0)] <- NA
sdat <- sdat[which(!is.na(sdat$source_id)),]	# these were the bib_await, ready, etc. They were populated when we created sdat, but have no data.

#############################################################

### Now, calculate DPR across all years, based on the pooled nest numbers and exposure days:

sdat$DPR <- NA
sdat$DPR <- sdat$predated/sdat$Exposure_days
sdat$DPR[which(sdat$DPR==Inf)] <- NA	# these are where we don't have inc per to get exp days
# some will be NA where we don't have predated and/or exp:
sdat[which(is.na(sdat$DPR)),]
# most of those have daily_predation_rate or daily_survival_rate, so we'll use those below to fill DPR.


# When the source gave DPR, does it match ours?
temp2 <- sdat[which(!is.na(sdat$daily_predation_rate)),]
data.frame(Source=temp2$daily_predation_rate, Ours=temp2$DPR, Beintema=temp2$DPRtrans)
# Very close in most cases. If we did not use Beintema transformation, keep our DPR because they might have used a different calculation.
# Of those where we have both our estimate and theirs, only one study used Beintema transformation. Use their value instead, because we have little confidence in the transformation:
sdat$DPR[which(!is.na(sdat$daily_predation_rate) & !is.na(sdat$DPR) & sdat$DPRtrans=="YES")] <- sdat$daily_predation_rate[which(!is.na(sdat$daily_predation_rate) & !is.na(sdat$DPR) & sdat$DPRtrans=="YES")]
# In a few cases, daily_predation_rate is provided with no additional information, so we have not calculated DPR ourselves. In this case, we'll use their DPR:
x <- which(!is.na(sdat$daily_predation_rate) & is.na(sdat$DPR))
sdat$DPR[x] <- sdat$daily_predation_rate[x]
sdat$DPRtrans[x] <- "NO"	# no transformation needed when DPR is from source
sdat$DPR_assumption[x] <- "DPR_direct_from_source"

## Kubelka must have assumed DPR = 1-DSR when no other info available:
x <- which(is.na(sdat$DPR) & !is.na(sdat$daily_survival_rate))
sdat$DPR[x] <- 1- sdat$daily_survival_rate[x]
sdat$DPR_assumption[x] <- "1-DSR"

#########################

## Add TPR = 1 - ((1-DPR)^inclay)
# this will be NA for the species without inc+lay period (only the species we added that Kubelka excluded)

sdat$TPR <- 1- ((1-sdat$DPR)^sdat$Incubation_days)

#############################

# Note that Kubelka included nothing with <12 nests. Ultimately we will want to exclude the same, but keep for now.
range(sdat$N.nests, na.rm=TRUE)
length(which(sdat$N.nests>=12))	# will leave us with 277 pops with data.

# Save all of these data, including rows where DPR is blank and nests < 12:

write.csv(sdat,"Rebuttal_data_with_DPR_yrs_pooled.csv", row.names=FALSE) 

