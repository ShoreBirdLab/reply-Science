
### Reformat the data that we extracted from the literature. This includes calculating some numbers (that we need to calculate DPR) from the information that was explicitly provided in the reference.
# This involves a lot of cleaning and checking of our extracted data.
# Saves a file that is used in later scripts to calculate DPR.
# < 1 min to run.


rm(list=ls(all=TRUE))
options(scipen=10,stringsAsFactors=FALSE)
memory.limit(size=8000000000)

# Read in our newly extracted data:
bdat <- read.csv("newly_extracted.csv", header=TRUE)

# Read in Kubelka's data:
kdat <- read.csv("Kubelka et al._2018_Science_additional datafile_1.csv", header=TRUE)
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
	
}	.

# Drop a few cases where all nests had unknown fate (excluded from all analyses by Kubelka):
bdat <- bdat[-which(!is.na(bdat$nests_total) & bdat$nests_total==bdat$nests_unknown_fate),]

#####
# Drop source 113; better data (unpubl) are provided in 517:
bdat[which(bdat$source_id==517),]
bdat[which(bdat$source_id==113),]
bdat <- bdat[-which(bdat$source_id==113),]
bdat$source_id[which(bdat$source_id==517)] <- 113

#############

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

######## 

# Clean up site names to ensure they match Kubelka

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
bdat$locality[which(bdat$locality=="south part of Oland")] <- "south part of Oländ"
bdat$locality[which(bdat$locality %in% c("Alten Sorge-Scheife", "Meggerkoog"))] <- "Alke Sorge, Meggerdorf"
bdat$locality[grep("Zackenberg", bdat$locality)] <- "Zackenberg"

# Check if Kubelka still has something we're missing:
kdat[which(!(kdat$Locality %in% bdat$locality)),]
# no, all good now.

# What about the localities that we have, but Kubelka doesn't? (aside from those mentioned above as not pooled.) Lots of them are ASDN pops that Kubelka missed; check the rest:
scheck <- sort(unique(bdat$species[which(!(bdat$locality %in% kdat$Locality) & bdat$source_id!=132)]))
# Are any of those species missing entirely?
scheck[which(!scheck %in% kdat$species)]	# yes, those were legitimately missed by Kubelka (6 spp currently).
length(scheck)	# currently 4
# If the species was included by Kubelka, show 1) the site names we have that they don't, and 2) all the site names they used for that species. If they have something similar to what we used, we might need to change our site name (add to the list above):
for(i in 1:length(scheck)){
	print("#######################################################")
	if(scheck[i] %in% kdat$species){
		bloc <- sort(unique(bdat$locality[which(bdat$species==scheck[i]  & bdat$source_id!=132)]))
		kloc <- sort(unique(kdat$Locality[which(kdat$species==scheck[i])]))
		bloc <- bloc[which(!(bloc %in% kloc))]
		if(length(kloc)>0){
			print("### ours:")
			print(bloc[which(!(bloc %in% kloc))])	# missing from Kubelka. Probably legitimately missing unless something similar prints below.
			print("### Kubelka's:")
			print(kloc)	# Show all localities present in Kubelka for this species. If any are similar to the above (missing from Kubelka), we probably have the wrong name in bdat - change above.
		}
	}
}
# Ignore the ones that we already mentioned above as ones we don't want to pool.
# The other ones Kubelka is missing, that we have, are two lapwing studies that are cited in the table in the main manuscript, but are NOT included in the Kubelka data table. Maybe the ref was omitted in data table, or maybe the data were truly not used. Locations in bdat are Bohemia and Netherlands.
bdat[which(bdat$locality %in% c("Bohemia","Netherlands")),]
	# we haven't yet extracted data for Kragten, but noted no study anomaly for Zámečník, so it should be included (ultimately).

###############

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
# source_id 133, 171, and 186 seem to have nests where cause of failure wasn't indicated. We'll make some assumptions about them later in DPR script.

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
tail(temp[,which(colnames(temp) %in% c('nests_predated', 'nests_failed_other_cause', 'nests_hatched', 'nests_total'))])
# Kubelka et al do not say that they assumed failed=predated for these. However, they must have, because we found no information on predation vs other failures.
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

# Later, we'll make some more assumptions about using DSR or total nest success.

############################

## From the values we extracted, calculate some additional columns that we'll use (and Kubelka used):
bdat$Failed_together <- bdat$mean_year <- bdat$years_nr <- bdat$obs_time <- NA	# to match with kdat, excluding aband and unk
bdat$N.nests <- bdat$nests_total
i <- 1	# to test loop
for(i in 1:nrow(bdat)){
	# ignore if we haven't yet extracted:
	if(bdat$who[i]!="bib_await"){
	
		### Nest info:
		
		# We ignore unknown, so Failed_together and N.nests exclude those. Kubelka were inconsistent on their treatment of abandoned; we include them:
		f <-  sum(c(bdat$nests_predated[i], bdat$nests_abandoned[i], bdat$nests_infertile[i], bdat$nests_failed_other_cause[i]), na.rm=TRUE)
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
			if(is.na(bdat$year[i])) ny <- length(unique(c(bdat$start_year[i]:bdat$end_year[i])))
			if(!is.na(bdat$year[i])){
				if(bdat$year[i]=="all") ny <- length(unique(c(bdat$start_year[i]:bdat$end_year[i])))	
				if(bdat$year[i]!="all") ny <- 1	
			}
			bdat$years_nr[i] <- ny
		}
		
		# Fill obs_time based on any info about nest searching or age found:
		if(is.na(bdat$nest_search_frequency[i]) & is.na(bdat$when_most_nests_found[i])) bdat$obs_time[i] <- 0.5
		if(!is.na(bdat$nest_search_frequency[i])){
			if(bdat$nest_search_frequency[i]=="daily") bdat$obs_time[i] <- 0.9
			if(bdat$nest_search_frequency[i] %in% c("1-2 per week","once-twice per week")) bdat$obs_time[i] <- 0.6
			if(bdat$nest_search_frequency[i] %in% c("less than weekly", "unknown")) bdat$obs_time[i] <- 0.5	# Kubelka used min 0.5
		}
		# if we don't have nest_search_frequency, use age found instead:
		if((is.na(bdat$nest_search_frequency[i]) | bdat$nest_search_frequency[i]=="unknown") & !is.na(bdat$when_most_nests_found[i])){
			if(bdat$when_most_nests_found[i]=="laying") bdat$obs_time[i] <- 0.9
			if(bdat$when_most_nests_found[i]=="early incubation") bdat$obs_time[i] <- 0.6
			if(bdat$when_most_nests_found[i] %in% c("mid incubation","unknown")) bdat$obs_time[i] <- 0.5	# Kubelka used min 0.5
		}
	}
}	# 1 sec to run

# Drop a couple of our columns that we have replaced above, so I don't get confused about which to use:
bdat <- bdat[,-which(colnames(bdat) %in% c('nests_total', 'nests_total_failed'))]

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
# change 0 exposure to NA to ensure I remember these are invalid records (we never include something that truly had 0 exposure!):
bdat$exposure_days[which(bdat$exposure_days==0)] <- NA

##############################

### Add some study info, if needed:
# Is the year before or after 2000?
bdat$period <- NA
bdat$period[which(as.numeric(bdat$year)<2000)] <- "before"
bdat$period[which(as.numeric(bdat$year)>=2000)] <- "after"
bdat$period[which(is.na(bdat$year) & as.numeric(bdat$start_year)<2000)] <- "before"
bdat$period[which(is.na(bdat$year) & as.numeric(bdat$start_year)>=2000)] <- "after"

bdat$period[which(bdat$year=="all" & as.numeric(bdat$start_year<2000))] <- "before"
bdat$period[which(bdat$year=="all" & as.numeric(bdat$start_year>=2000))] <- "after"
bdat[which(is.na(bdat$period)),]

###############################

# Get Latitude, Longitude, and belts (regions):
# from Kubelka:
bdat$Latitude <- bdat$Longitude <- NA
for(i in 1:nrow(bdat)){
	bdat$Latitude[i] <- mean(unique(kdat$Latitude[which(kdat$Locality==bdat$locality[i])]), na.rm=TRUE)
	bdat$Longitude[i] <- mean(unique(kdat$Longitude[which(kdat$Locality==bdat$locality[i])]), na.rm=TRUE)
}
range(bdat$Latitude, na.rm=TRUE)

## Add latitude for sites that are missing it (not in Kubelka):
unique(bdat$locality[which(is.na(bdat$Latitude))])	
bdat$Latitude[which(bdat$locality=="Coats Island")] <- 62.855204
bdat$Latitude[which(bdat$locality=="Marambitsy Bay")] <- -15.966667
bdat$Latitude[which(bdat$locality=="Netherlands")] <- 52.1917324
bdat$Latitude[which(bdat$locality=="Bohemia")] <- 49.966793
bdat$Latitude[which(bdat$locality=="Central & S England")] <- 51.725193
bdat$Latitude[which(bdat$locality=="N England")] <- 53.706973
bdat$Latitude[which(bdat$locality=="Scotland")] <- 56.623632
bdat$Latitude[which(bdat$locality=="La Perouse Bay, Manitoba")] <- 58.735744

# and longitude:
bdat$Longitude[which(bdat$locality=="Coats Island")] <- -82.498505
bdat$Longitude[which(bdat$locality=="Marambitsy Bay")] <- 45.590556
bdat$Longitude[which(bdat$locality=="Netherlands")] <- 3.0367669
bdat$Longitude[which(bdat$locality=="Bohemia")] <- 14.211334
bdat$Longitude[which(bdat$locality=="Central & S England")] <- -1.405592
bdat$Longitude[which(bdat$locality=="N England")] <- 1.739997
bdat$Longitude[which(bdat$locality=="Scotland")] <- -4.263176
bdat$Longitude[which(bdat$locality=="La Perouse Bay, Manitoba")] <- -93.302804

bdat$Belt <- NA
bdat$Belt[which(bdat$Latitude <= -30)] <- "South temperate"
bdat$Belt[which(bdat$Latitude > -30 & bdat$Latitude <= 0)] <- "South tropics"
bdat$Belt[which(bdat$Latitude > 0 & bdat$Latitude <= 30)] <- "North tropics"
bdat$Belt[which(bdat$Latitude > 30 & bdat$Latitude <= 60)] <- "North temperate"
bdat$Belt[which(bdat$Latitude > 60 & bdat$Latitude <= 90)] <- "Arctic"

#############################

## Save the new, cleaned data file:
write.csv(bdat,"Rebuttal_data_processed.csv", row.names=FALSE) 
