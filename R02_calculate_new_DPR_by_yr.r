### From our literature extraction, use the processed dataset (from script R04a - format data) to calculate DPR (or assume DPR = 1-DSR where needed).

# This script calculates DPR by site and by year (when available by year in the source) - the finest scale available in our dataset.
	# To compare with Kubelka, we'll use a different script to calculate DPR, because they say "When data were available for >1 yr, the sum of depredated nests and overall exposure were pooled over years to obtain mean predation values." If only DSR is given, I will assume they averaged DSR across years.
# Saves a file that includes DPR for each population-year.
# < 1 min to run.


rm(list=ls(all=TRUE))
options(scipen=10,stringsAsFactors=FALSE)
memory.limit(size=8000000000)

bdat <- read.csv("Rebuttal_data_processed.csv", header=TRUE)	# as created by R01 script

bdat$DPR <- NA
bdat$DPR <- bdat$nests_predated/bdat$exposure_days
bdat$DPR[which(bdat$DPR==Inf)] <- NA	# shouldn't be any, but run this just in case.
# some will be NA where we don't have predated and/or exp:
bdat[which(is.na(bdat$DPR)),]
# but all of those have daily_predation_rate or daily_survival_rate, so we'll use those below to fill DPR.


# When the source gave DPR, does it match ours?
temp2 <- bdat[which(!is.na(bdat$daily_predation_rate)),]
data.frame(Source=temp2$daily_predation_rate, Ours=temp2$DPR, Beintema=temp2$DPRtrans)
# Very close in most cases. If we did not use Beintema transformation, keep our DPR because they might have used a different calculation.
# Of those where we have both our estimate and theirs, only one study used Beintema transformation. Use their value instead, because we have little confidence in the transformation:
bdat$DPR[which(!is.na(bdat$daily_predation_rate) & !is.na(bdat$DPR) & bdat$DPRtrans=="YES")] <- bdat$daily_predation_rate[which(!is.na(bdat$daily_predation_rate) & !is.na(bdat$DPR) & bdat$DPRtrans=="YES")]
# In a few cases, daily_predation_rate is provided with no additional information, so we have not calculated DPR ourselves. In this case, we'll use their DPR:
x <- which(!is.na(bdat$daily_predation_rate) & is.na(bdat$DPR))
bdat$DPR[x] <- bdat$daily_predation_rate[x]
bdat$DPRtrans[x] <- "NO"	# no transformation needed when DPR is from source
bdat$DPR_assumption[x] <- "DPR_direct_from_source"

## Kubelka must have assumed DPR = 1-DSR when no other info available:
x <- which(is.na(bdat$DPR) & !is.na(bdat$daily_survival_rate))
bdat$DPR[x] <- 1- bdat$daily_survival_rate[x]
bdat$DPR_assumption[x] <- "1-DSR"

####################

## Some quick checks of DPR:
length(which(is.na(bdat$DPR)))
head(unique(sort(bdat$DPR)))	# no negatives; ok
tail(sort(bdat$DPR))	# none >1; ok
bdat[which(bdat$DPR==1),]	# yes that's accurate, 2 nests depredated in 1 day
# others with weird DPR are likely to be small samples:
sort(bdat$N.nests[which(bdat$DPR > 0.3)])	# yes, very small
sort(bdat$N.nests[which(bdat$DPR > 0.2)])	# yes, most very small. Check the largest one:
bdat[which(bdat$DPR>0.2 & bdat$N.nests>100),]	# DPR was 1-DSR, which was very low; nest numbers not given so this is the best we can do.

#########################

## Add TPR = 1 - ((1-DPR)^inclay)
# this will be NA for the species without inc+lay period (only the species we added that Kubelka excluded)

bdat$TPR <- 1- ((1-bdat$DPR)^bdat$Incubation_days)


##########################
	
# Note that Kubelka included nothing with <12 nests. Ultimately we will want to exclude the same, but keep for now.
range(bdat$N.nests, na.rm=TRUE)

# Save all of these data, including rows where DPR is blank and nests < 12:

write.csv(bdat,"Rebuttal_data_with_DPR_by_yr.csv", row.names=FALSE) 

