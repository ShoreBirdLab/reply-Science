
rm(list=ls(all=TRUE))
options(scipen=10,stringsAsFactors=FALSE)
memory.limit(size=8000000000)
setwd("C:/Users/emily/Documents/Work and research/ASDN/Science rebuttal/")	# change as needed
library("googlesheets")

# Sign in to Google in your browser, if needed:
gs_ls()
# See what you have:
#as.data.frame(gs_ls())$sheet_title
# Read from the Google sheet:
bdb <- gs_title("Kubelka_et_al_rebuttal")
gs_ws_ls(bdb)
bdat <- gs_read(ss=bdb, ws = "lit_team", col_names=TRUE)
bdat <- as.data.frame(bdat)
# I got some download warnings, but it looks okay (maybe special characters are a problem?); check nrow against what is currently in Google Sheet (minus 1 for the column headers):
nrow(bdat)

kdat <- read.csv("Data from Kubelka et al/Kubelka et al._2018_Science_additional datafile_1.csv", header=TRUE)
	# Identical to the data tab from the .xlsx file of the same name, as provided by Kubelka; copied to csv for ease of use in R.
	
# Clean up our database - u or unknown --> NA, numbers --> numeric:
head(bdat)	
bdat[bdat=="u" | bdat=="U" | bdat=="Unknown" | bdat=="unknown"] <- NA
# These are the columns that we'll use here and should be numeric:
numcols <- c("nests_total", "nests_hatched", "nests_predated", "nests_abandoned", "nests_infertile", "nests_failed_other_cause", "nests_unknown_fate", "nests_total_failed", "prop_hatched", "prop_predated", "daily_survival_rate", "daily_predation_rate", "overall_nest_success", "overall_predation_rate", "exposure_days")	# prop_hatched, daily_survival_rate
for(i in 1:length(numcols)){
	w <- which(colnames(bdat)==numcols[i])
	startblank <- which(bdat[,w]=="" | is.na(bdat[,w]))
	bdat[,w] <- as.numeric(bdat[,w])
	endblank <- which(bdat[,w]=="" | is.na(bdat[,w]))
	if(length(endblank)>length(startblank)) print(numcols[i])
	
}	# prints name of column where NAs coerced
# If NAs are coerced, we wouldn't have been able to use that value anyway; the only one is ">1" in nests_predated (I've fixed all the other problems in these columns in Drive).

# Drop a few cases where all nests had unknown fate (mostly ASDN, non-focal species):
bdat <- bdat[-which(!is.na(bdat$nests_total) & bdat$nests_total==bdat$nests_unknown_fate),]


# all proportions need to be <1!:
any(bdat$prop_hatched > 1, na.rm=TRUE)
any(bdat$prop_predated > 1, na.rm=TRUE)
any(bdat$daily_predation_rate > 1, na.rm=TRUE)
# I went back into Google Drive to change these, because sometimes the number predated had been put into prop_predated, and other times a percentage (1-100) was given instead of proportion (0-1). But this check will make sure all is still okay.


######## 

# Clean up site names

# To compare with Kubelka, we need to get the localities and species names to match. It's still worth running this even if not comparing directly with Kubelka, to ensure that sites are pooled into populations as they should be. (Many site names were changed in the lit_team tab from the original, often arbitrary changes like adding the country name or removing a space.)

bdat$locality[which(bdat$locality=="California, Eden Landing Ecological Reserve" )] <- " California, Eden Landing Ecological Reserve" 
bdat$locality[which(bdat$locality=="Audubon National Wildlife Refuge, North Dacota")] <- "Audubon National Wildlife Refuge, North Dacota " 
bdat$locality[which(bdat$locality=="Cass river Valley, New Zealand")] <- "Cass river Valley + surrounding" 
bdat$locality[which(bdat$locality=="Elmley marshes, England, grazed marshes")] <- "Emley marshes, England" 
bdat$locality[which(bdat$locality=="Elmley marshes, England, ungrazed marshes")] <- "Emley marshes, England" 
bdat$locality[which(bdat$locality=="England, Scotland, Wales (Central & S England, N England, Scotland)")] <- "England, Scotland, Wales" 
bdat$locality[grep("Freshwater lake and surrounding, Alberta", bdat$locality)] <- "Freshwarter lake and surrounding, Alberta"
bdat$locality[which(bdat$locality=="Hardangervidda, Norway")] <- "Hardangervidda "
bdat$locality[grep("Great Salt Lake", bdat$locality)] <- "Great Salt Lake, Utah"
bdat$locality[which(bdat$locality=="Kaliningrad region")] <- "Kaliningrad"
bdat$locality[grep("Khao Sam Roi", bdat$locality)] <- "Khao Sam Roi Yod National Park"
bdat$locality[which(bdat$locality=="Long Bay Regional Park and Okura estuary, Auckland")] <- "Long Bay Regional Park, Auckland"
bdat$locality[grep("Maio", bdat$locality)] <- "Maio "
bdat$locality[which(bdat$locality=="Medusa Bay, Taimyr, 18km south of Dikson")] <- "Medusa Bay, Taimyr" 
bdat$locality[which(bdat$locality=="Mestersvig, Scoresby Land, Greenland" )] <- "Mestersvig, Scoresby Land,"
bdat$locality[which(bdat$locality=="North Dakota, South Dakota, Manitoba, Montana")] <- "North Dacota + some data also from South Dacota, Manitoba and Montana"
bdat$locality[grep("Puerto Madryn",bdat$locality)] <- "Puerto Madryn, Patagonia"
bdat$locality[which(bdat$locality=="St Cyrus, Scotland")] <- "Cyrus NNR, Scotland" 
bdat$locality[which(bdat$locality=="Trebonsko")] <- "T?ebo?sko" 
bdat$locality[which(bdat$locality=="Tsimanampetsotsa")] <- "Tsimanampetsotsa "
bdat$locality[which(bdat$locality=="Vembanur lake, India")] <- "Vembanur" 
bdat$locality[which(bdat$locality=="Lough Erne, Northern Ireland")] <- "Lough Erne,  Northern Ireland"
bdat$locality[grep("Snares Islands Nature reserve", bdat$locality)] <- "Snares Islands Nature reserve"
bdat$locality[grep("Lustenau", bdat$locality)] <- "Lustenau, Austrian Rhine Valley"
bdat$locality[which(bdat$locality=="Hiraethog, North Wales")] <- "Hiraethog,  North Wales"
bdat$locality[which(bdat$locality=="Nag Valley")] <- "Nag valley"
bdat$locality[which(bdat$locality=="Farne Islands, Northumberland")] <- "Lindisfarne NNR, North Englang"
bdat$locality[grep("Yakutia", bdat$locality)] <- "Yakutia, Lena River"
bdat$locality[grep("Anadyr ", bdat$locality)] <- "Anadyr surrounding"
bdat$locality[which(bdat$locality=="Bahia de Ceuta, Mexico")] <- "Ceuta"
bdat$locality[which(bdat$locality=="Kazakhstan")] <- "Korgalzhyn"
bdat$locality[which(bdat$locality=="Rangatira (South East) island New Zealand")] <- "Rangatira (South East) island"


# Some issues:
	# Kubelka inappropriately lumped several locations (>40 km apart):
		# For at least one species, Taimyr, Chaun, and/or Koljuchinskaja (all 100s of km apart).
		# La Perouse Bay with Churchill (>300 km apart)
		# Marambitsy Bay and Tsimanampetsotsa in Madagascar (700 km apart!) from Zefania et al 2008; another location in Madagascar, between these two, was NOT pooled.
		# England, Scotland, and Wales average >40 km apart! One study included all of them, so maybe we could lump for a before-after for the relevant species - but in general, we should keep them separate.
		# We should keep these separate. If running a comparison with Kubelka, though, we need to pool:
#		bdat$locality[which(bdat$locality=="La Perouse Bay, Manitoba")] <- "Churchill, Manitoba"
#		bdat$locality[which(bdat$locality=="Marambitsy Bay")] <- "Tsimanampetsotsa "
#		bdat$locality[which(bdat$locality %in% c("Central & S England", "N England", "Scotland"))] <-  "England, Scotland, Wales"

# Check if Kubelka still has something we're missing:
kdat[which(!(kdat$locality %in% bdat$locality)),]
# no, all good 

# What about the localities that we have, but Kubelka doesn't? Lots of them are ASDN pops that Kubelka missed; check the rest:
scheck <- sort(unique(bdat$species[which(!(bdat$locality %in% kdat$locality) & bdat$source_id!=132)]))
length(scheck)	# only 1 remaining
for(i in 1:length(scheck)){
	# Step through to assess each one:
	bloc <- sort(unique(bdat$locality[which(bdat$species==scheck[i]  & bdat$source_id!=132)]))
	kloc <- sort(unique(kdat$locality[which(kdat$species==scheck[i])]))
	print(bloc[which(!(bloc %in% kloc))])	# missing from Kubelka. Probably legitimately missing unless something similar prints below.
	print(kloc)	# Show all localities present in Kubelka for this species. If any are similar to the above (missing from Kubelka), we probably have the wrong name in bdat - change above.
}
# The ones Kubelka is missing, that we have, are two lapwing studies that are cited in the table in the main manuscript, but are NOT included in the Kubelka data table. Maybe the ref was omitted in data table, or maybe the data were truly not used. Locations in bdat are Bohemia and Netherlands.
bdat[which(bdat$locality %in% c("Bohemia","Netherlands")),]
	# we haven't yet extracted data for Kragten, but noted no study anomaly for Zámečník, so it should be included (ultimately).

###############

# Check and fix some species names so they agree with Kubelka:
sort(unique(bdat$species[which(!(bdat$species %in% kdat$species))]))
bdat$species[which(bdat$species=="Calidris_falcinellus")] <- "Limicola_falcinellus"
bdat$species[which(bdat$species=="Calidris_alpina_schinzii")] <- "Calidris_alpina"
bdat$species[which(bdat$species=="Calidris_pugnax")] <- "Philomachus_pugnax"
bdat$species[which(bdat$species=="Calidris_subruficollis")] <- "Tryngites_subruficollis"
bdat$species[which(bdat$species=="Charadrius_alexandrinus_nivosus")] <- "Charadrius_nivosus"
bdat$species[which(bdat$species=="Himantopus_himantopus_leucoephalus")] <- "Himantopus_leucocephalus"
# check again:
sort(unique(bdat$species[which(!(bdat$species %in% kdat$species))]))
# check against any that Kubelka have that we don't:
sort(unique(kdat$species[which(!(kdat$species %in% bdat$species))]))
# We have all Kubelka's, plus we've added some species that they missed (from the same refs). Later we will note which ones they missed.

# For the species that match, get incubation days used by Kubelka (for now, I'm ignoring inc_days from the source because it was so rarely provided):
bdat$Incubation_days <- NA
for(i in 1:nrow(bdat)){
	bdat$Incubation_days[i] <- mean(kdat$Incubation_days[which(kdat$species==bdat$species[i])])
}
# the species missing inc:
sort(unique(bdat$species[which(is.na(bdat$Incubation_days))]))
# should match the species missing from Kubelka:
sort(unique(bdat$species[which(!(bdat$species %in% kdat$species))]))
# yes - great, we have inc_days for everything included by Kubelka.
		
###################

## To calculate DPR, we need to infer num predated from other values in some cases.

# Here, check failed nests:
# Sometimes we have num predated and nests_total_failed, but the other failed nests aren't broken down by cause. Assign the remainder to nests_failed_other_cause:
temp1 <- temp2 <- matrix(NA, nrow=0, ncol=8)
for(i in 1:nrow(bdat)){

	# calculate total failed, including aband and unknown, which we'll ultimately exclude:
	tf <- sum(c(bdat$nests_predated[i], bdat$nests_abandoned[i], bdat$nests_infertile[i], bdat$nests_failed_other_cause[i]),na.rm=TRUE)
	# Usually when nests_total_failed is filled, tf will be 0, except sometimes the predated nests are given.
	if(is.na(bdat$nests_total_failed[i]) & !is.na(tf)) if(tf>0) bdat$nests_total_failed[i] <- tf
	# if we have a value for total failed:
	if(!is.na(bdat$nests_total_failed[i])){
		# and if a value is given in nests_predated (if nothing is given, we know nothing about cause of failure, so keep totals in total_failed only)
		# and if total failed doesn't equal tf
		# and if failed_other is blank:
		if(bdat$nests_total_failed[i] != tf & !is.na(bdat$nests_predated[i]) & (is.na(bdat$nests_failed_other_cause[i]) | bdat$nests_failed_other_cause[i]==0)){
			# if total_failed is LESS than the sum of the failure causes, we probably have an error in the record; save these to check below:
			if(bdat$nests_total_failed[i] < tf){
				temp1 <- rbind(temp1, bdat[i,which(colnames(bdat) %in% c('source_id','nests_total','nests_hatched', 'nests_total_failed','nests_predated','nests_abandoned','nests_infertile','nests_failed_other_cause'))])
			}
			# if total_failed includes more nests than tf, the remainder are failed to unknown cause, so put into failed_other:
			if(bdat$nests_total_failed[i] > tf){
				temp2 <- rbind(temp2, bdat[i,which(colnames(bdat) %in% c('source_id','nests_total','nests_hatched', 'nests_total_failed','nests_predated','nests_abandoned','nests_infertile','nests_failed_other_cause'))])
				ntf <- bdat$nests_total_failed[i] - tf
				# in addition to any nests already in failed_other:
				if(!is.na(bdat$nests_failed_other_cause[i])) ntf <- ntf + bdat$nests_failed_other_cause[i]
				bdat$nests_failed_other_cause[i] <- ntf
			}
		}
	}

}
# Double-check records where we made changes:
# temp1 records might have errors:
temp1	# but there are none!
# temp2: if nests_hatched or nests_predated is NA, the source probably simply didn't give those numbers; we can calculate them below.
	# check the other records for errors:
temp2[which(!is.na(temp2$nests_hatched) & !is.na(temp2$nests_predated)),]
# source_id 133, 171, and 186 seem to have nests where cause of failure wasn't indicated. We'll make some assumptions about them below.

# Kubelka et al assumed sources 57 and 58 had all nests failed to predation:
head(bdat[which(bdat$source_id %in% c(57,58)),])
# well, we show 0 nests predated in 57 and no info on any failures, so I guess we assume they all hatched:
bdat$nests_hatched[which(bdat$source_id==57)] <- bdat$nests_total[which(bdat$source_id==57)]
# for 58:
bdat$nests_predated[which(bdat$source_id==58)] <- bdat$nests_total_failed[which(bdat$source_id==58)]
# Record the assumptions we're making:
bdat$DPR_assumption <- NA
bdat$DPR_assumption[which(bdat$source_id %in% c(57,58))] <- "Fail_is_depr"

# for source 95, only DSR is given, but the source states that DSR was calculated only for those that survived + those depredated. Thus, we can assume DPR = 1-DSR:
bdat$daily_predation_rate[which(bdat$source_id==95)] <- 1 - as.numeric(bdat$daily_survival_rate[which(bdat$source_id==95)])

# check where num predated is missing, but we have some other nest numbers:
temp <- bdat[which(is.na(bdat$nests_predated) & bdat$who!="bib_await"),]
head(temp[,which(colnames(temp) %in% c('nests_predated', 'nests_failed_other_cause', 'nests_hatched', 'nests_total'))])
# Kubelka et al do not say that they assumed failed=predated for these. However, I guess they must have, because we found no information on predation vs other failures. Ultimately we should exclude these due to uncertainty. Here, we'll include them for the sake of comparison.
x <- which(is.na(bdat$nests_predated) & !is.na(bdat$nests_total) & !is.na(bdat$nests_hatched))
bdat$nests_predated[x] <- bdat$nests_total[x] - bdat$nests_hatched[x]
bdat$DPR_assumption[x] <- "Fail_is_depr"

x <- which(is.na(bdat$nests_predated) & !is.na(bdat$nests_total_failed))
bdat$nests_predated[x] <- bdat$nests_total_failed[x]
bdat$nests_failed_other_cause[x] <- bdat$nests_total_failed[x]-bdat$nests_predated[x]
bdat$DPR_assumption[x] <- "Fail_is_depr"
# Similarly, if we have nests_total and prop_hatched, we can fill in other nest numbers:
x <- which(is.na(bdat$nests_predated) & !is.na(bdat$nests_total) & !is.na(bdat$prop_hatched))
bdat$nests_hatched[x] <- round(bdat$nests_total[x]*bdat$prop_hatched[x],0)
bdat$nests_predated[x] <- bdat$nests_total[x] - bdat$nests_hatched[x]
bdat$DPR_assumption[x] <- "Fail_is_depr"

# Below, we'll make some more assumptions about using DSR or total nest success - only for the sake of comparison with Kubelka. Ultimately those assumptions are probably not safe to make.

############################

## From the values we extracted, calculate some additional columns that we'll use (and Kubelka used):
bdat$Failed_together <- bdat$mean_year <- bdat$years_nr <- bdat$obs_time <- NA	# to match with kdat, excluding aband and unk
bdat$N.nests <- bdat$nests_total
i <- 1	# to test loop
for(i in 1:nrow(bdat)){
	# ignore if we haven't yet extracted:
	if(bdat$who[i]!="bib_await"){
	
		### Nest info:
		
		# We ignore abandoned and unknown, so Failed_together and N.nests exclude those:
		f <-  sum(c(bdat$nests_predated[i], bdat$nests_infertile[i], bdat$nests_failed_other_cause[i]), na.rm=TRUE)
		# or, if we only have a total failed:
		if(f==0) f <- bdat$nests_total_failed[i]
		if(!is.na(f)){
			bdat$N.nests[i] <- bdat$nests_hatched[i] + f
			bdat$Failed_together[i] <- f
		}
		# or if we have no info on the fail numbers, assume we should include all nests:
		if(f==0 | is.na(f)) bdat$N.nests[i] <- bdat$nests_total[i]
		if(is.na(bdat$N.nests[i]) & !is.na(bdat$nests_total[i])) bdat$N.nests[i] <- bdat$nests_total[i]
		
		# If we have prop_hatched but not nests_hatched, fill the number:
		if(is.na(bdat$nests_hatched[i])) bdat$nests_hatched[i] <- round(bdat$prop_hatched[i]*bdat$N.nests[i],0)
		# If we have prop_predated but not nests_predated, fill the number:
		if(is.na(bdat$nests_predated[i])) bdat$nests_predated[i] <- round(bdat$prop_predated[i]*bdat$N.nests[i],0)
		
		# If we have total + hatched, but not num failed, fill failed_together:
		if(is.na(bdat$Failed_together[i])) bdat$Failed_together[i] <- bdat$N.nests[i] - bdat$nests_hatched[i]
		
		# If hatched = total, predated must = 0:
		if(!is.na(bdat$nests_hatched[i]) & !is.na(bdat$N.nests[i]) & is.na(bdat$nests_predated[i])) if(bdat$nests_hatched[i]==bdat$N.nests[i]) bdat$nests_predated[i] <- 0

		# If failed = total, hatched must = 0:
		if(!is.na(bdat$Failed_together[i]) & !is.na(bdat$N.nests[i]) & is.na(bdat$nests_hatched[i])) if(bdat$Failed_together[i]==bdat$N.nests[i]) bdat$nests_hatched[i] <- 0

	
		# If we have total + failed, but not num hatched, fill hatched:
		if(is.na(bdat$nests_hatched[i])) bdat$nests_hatched[i] <- bdat$N.nests[i] - bdat$Failed_together[i]

		# for some, we can calculate total and hatched from prop_hatched and number failed:
		if(is.na(bdat$nests_hatched[i]) & is.na(bdat$N.nests[i]) & !is.na(bdat$prop_hatched[i]) & !is.na(bdat$nests_total_failed[i])){
			bdat$nests_hatched[i] <- round(bdat$nests_total_failed[i]/(1-bdat$prop_hatched[i]),0)
			bdat$N.nests[i] <- bdat$nests_total_failed[i] + bdat$nests_hatched[i]
		}

		
		if(!is.na(bdat$N.nests[i]) & !is.na(bdat$nests_hatched[i])) if(bdat$N.nests[i]==bdat$nests_hatched[i]) bdat$nests_predated[i] <- 0
		

		### Study info:
		
		# Fill year info:
		if(!is.na(bdat$start_year[i]) & !is.na(bdat$end_year[i])){
			bdat$mean_year[i] <- mean(c(bdat$start_year[i]:bdat$end_year[i]))
			bdat$years_nr[i] <- length(c(bdat$start_year[i]:bdat$end_year[i]))
		}
		
		# Fill obs_time based on any info about nest searching or age found:
		if(is.na(bdat$nest_search_frequency[i]) & is.na(bdat$when_most_nests_found[i])) bdat$obs_time[i] <- 0.5
		if(!is.na(bdat$nest_search_frequency[i])){
			if(bdat$nest_search_frequency[i]=="daily") bdat$obs_time[i] <- 0.9
			if(bdat$nest_search_frequency[i] %in% c("1-2 per week","once-twice per week")) bdat$obs_time[i] <- 0.6
			if(bdat$nest_search_frequency[i] %in% c("less than weekly")) bdat$obs_time[i] <- 0.5	# Kubelka used min 0.5
		}
		# if we don't have nest_search_frequency, use age found instead:
		if(is.na(bdat$nest_search_frequency[i]) & !is.na(bdat$when_most_nests_found[i])){
			if(bdat$when_most_nests_found[i]=="laying") bdat$obs_time[i] <- 0.9
			if(bdat$when_most_nests_found[i]=="early incubation") bdat$obs_time[i] <- 0.6
			if(bdat$when_most_nests_found[i]=="mid incubation") bdat$obs_time[i] <- 0.5	# Kubelka used min 0.5
		}
	}
}	# 1 sec to run

# See which nest numbers we're still missing but can be filled from our extraction:
temp <- bdat[which(is.na(bdat$N.nests) & bdat$who!="bib_await"),]
temp[which(!is.na(temp$prop_hatched)),]	# total nests not given; will use DSR below when we make more assumptions, but leave numbers blank for now.

# Drop a couple of our columns that we have replaced above, so I don't get confused about which to use:
bdat <- bdat[,-which(colnames(bdat) %in% c('nests_total', 'nests_total_failed'))]

#### Check study info:
bdat[which(is.na(bdat$mean_year) & bdat$who!="bib_await"),]	# all no data 
bdat[which(is.na(bdat$years_nr) & bdat$who!="bib_await"),]	# all no data
bdat[which(is.na(bdat$obs_time) & bdat$who!="bib_await"),]	# ok
bdat[which(is.na(bdat$locality)),]	# ok

############################


# calculate exposure_days (if not given) with Beintema and indicate DPRtrans="YES":
bdat$DPRtrans <- NA
bdat$DPRtrans[which(is.na(bdat$exposure_days))] <- "YES"
bdat$DPRtrans[which(bdat$exposure_days>0)] <- "NO"
bdat$DPRtrans[which(bdat$exposure_days==0)] <- "YES"
sort(unique(bdat$DPRtrans))	# should only be NO and YES
z <- which(bdat$exposure_days==0 | is.na(bdat$exposure_days))
for(i in 1:length(z)){
	bdat$exposure_days[z[i]] <- sum(
		# hatched nests: obs_time*(inc_days/2) per nest
		bdat$nests_hatched[z[i]]* (bdat$obs_time[z[i]]*(bdat$Incubation_days[z[i]])), 
		# infertile nests: obs_time*(inc_days/2)
		bdat$nests_infertile[z[i]]* (bdat$obs_time[z[i]]*(bdat$Incubation_days[z[i]])), 
		# predated nests: obs_time*(inc_days/4)
		bdat$nests_predated[z[i]]* (bdat$obs_time[z[i]]*(bdat$Incubation_days[z[i]]/2)), 
		# other_failed nests: obs_time*(inc_days/4)
		bdat$nests_failed_other_cause[z[i]]*(bdat$obs_time[z[i]]*(bdat$Incubation_days[z[i]]/2)), na.rm=TRUE)
}
# Check which records are still missing exp:
#bdat[which((is.na(bdat$exposure_days) | bdat$exposure_days==0) & !is.na(bdat$Incubation_days)),]
# only species for which we don't have inc_days, or sources with no nest numbers; and one ASDN with a single failed nests (which we will ignore). Some have DSR; we'll use that with an assumption below.
# change 0 exposure to NA to ensure I remember these are invalid records (we never include something that truly had 0 exposure!):
bdat$exposure_days[which(bdat$exposure_days==0)] <- NA

#################

# Now, we can finally calculate DPR:

bdat$DPR_source <- bdat$daily_predation_rate	# already given by source in a few cases
bdat$DPR <- NA
bdat$DPR <- bdat$nests_predated/bdat$exposure_days
# some will be NA where we don't have predated and/or exp:
bdat[which(is.na(bdat$DPR) & !is.na(bdat$exposure_days)),]

bdat$DPR[which(bdat$DPR==Inf)] <- NA

# In a few cases, the source gave DPR. Does it match ours?
temp2 <- bdat[which(!is.na(bdat$DPR_source)),]
cbind(temp2$DPR_source, temp2$DPR, temp2$DPRtrans)
# Pretty close in most cases. Of those where we have both our estimate and theirs, only one used Beintema transformation. We'll use our calculation for consistency (but look into these studies more later).
# But in a few cases, DPR_source is provided with no additional information, so we have no DPR yet. In this case, we'll use their DPR:
x <- which(!is.na(bdat$DPR_source) & is.na(bdat$DPR))
bdat$DPR[x] <- bdat$DPR_source[x]
bdat$DPR_assumption[x] <- "DPR_direct_from_source"

## Kubelka must have assumed DPR = 1-DSR when no other info available:
x <- which(is.na(bdat$DPR) & !is.na(bdat$daily_survival_rate))
bdat$DPR[x] <- 1- bdat$daily_survival_rate[x]
bdat$DPR_assumption[x] <- "1-DSR"

## Can we get anything more from total nest success?
x <- which(is.na(bdat$DPR) & !is.na(bdat$TSR))
bdat[x,]
# No, no data there.


# For the remainder, we did not find any info on how many nests failed (or, we're missing inc because Kubelka excluded this species):
temp <- bdat[which(is.na(bdat$DPR) & !is.na(bdat$Incubation_days)),]
temp <- temp[order(temp$source_id),]
temp[,which(colnames(temp) %in% c('N.nests','exposure_days','nests_predated','daily_survival_rate','daily_predation_rate'))]	# For all of these, we have no nest numbers, no exposure days, no DSR, and no DPR_source. Thus, we (currently) do not have the necessary info for these refs (but will want to double-check those, eventually) and others are awaiting checking.


##############################

### Add some study info, if needed:


# Is the study before or after 2000?
bdat$period <- NA
bdat$period[which(as.numeric(bdat$year)<2000)] <- "before"
bdat$period[which(as.numeric(bdat$year)>=2000)] <- "after"
bdat$period[which(is.na(bdat$year) & bdat$start_year<2000)] <- "before"
bdat$period[which(is.na(bdat$year) & bdat$start_year>=2000)] <- "after"

bdat$period[which(bdat$year=="all" & bdat$start_year<2000)] <- "before"
bdat$period[which(bdat$year=="all" & bdat$start_year>=2000)] <- "after"
bdat[which(is.na(bdat$period)),]
# I went through Google Sheet and ensured something was filled for start_year, at least, for even the refs we're currently missing (necessary to keep track of them further down the script).

###############################


# Get belts, where Kubelka also included this pop (if excluded, leave blank for now - we'll have to go back through study site location):
bdat$Latitude <- NA
for(i in 1:nrow(bdat)){
	bdat$Latitude[i] <- mean(unique(kdat$Latitude[which(kdat$Locality==bdat$locality[i])]), na.rm=TRUE)
}
range(bdat$Latitude, na.rm=TRUE)
bdat$Belt <- NA
bdat$Belt[which(bdat$Latitude <= -30)] <- "South temperate"
bdat$Belt[which(bdat$Latitude > -30 & bdat$Latitude <= 0)] <- "South tropics"
bdat$Belt[which(bdat$Latitude > 0 & bdat$Latitude <= 30)] <- "North tropics"
bdat$Belt[which(bdat$Latitude > 30 & bdat$Latitude <= 60)] <- "North temperate"
bdat$Belt[which(bdat$Latitude > 60 & bdat$Latitude <= 90)] <- "Arctic"

# Check where belt is missing:
bdat$locality[which(is.na(bdat$Belt))]	# pops excluded by Kubelka
# add their belt
bdat$Belt[which(bdat$locality %in% c("Coats Island", "La Perouse Bay, Manitoba"))] <- "Arctic"
bdat$Belt[which(bdat$locality %in% c("Central & S England", "N England", "Scotland", "Netherlands", "Bohemia"))] <- "North temperate"
bdat$Belt[which(bdat$locality=="Marambitsy Bay")] <- "South tropics"

#############################

# How many populations-years do we have DPR for? (not all included by Kubelka):
length(which(!is.na(bdat$DPR)))	# 1224
length(which(is.na(bdat$DPR)))	# 47 missing data (some not yet checked/received, others with insufficient info)
	
# Note that Kubelka included nothing with <10 nests, though they did not specify that in the paper:
range(kdat$N.nests)	

# We might want to exclude <10 too:	
range(bdat$N.nests, na.rm=TRUE)
# but for now, I'll keep them all, so don't run this:
# bdat <- bdat[-which(bdat$N.nests<10),]

# Save all of these data, including rows where DPR is blank and nests < 10:

write.csv(bdat,"Bulla et al database/Rebuttal_data_with_DPR.csv", row.names=FALSE) 
nrow(bdat)