### Check our lit extraction against Kubelka's for obs_time (for the Beintema transformation).
# < 1 min to run.

rm(list=ls(all=TRUE))
options(scipen=10,stringsAsFactors=FALSE)
memory.limit(size=8000000000)

# Our data, from script R03:
bdat <- read.csv("Rebuttal_data_with_DPR_yrs_pooled.csv", header=TRUE)	
kdat <- read.csv("Kubelka et al._2018_Science_additional datafile_1.csv", header=TRUE) # This file pools across periods for the populations that span 2000. However, their Before+After file does not provide obs_time. So I will work with this file.

# Read in our data extraction to check how often we found which info:
bdat1 <- read.csv("newly_extracted.csv", header=TRUE)
# Clean up site names: 
bdat1$locality[which(bdat1$locality=="California, Eden Landing Ecological Reserve" )] <- " California, Eden Landing Ecological Reserve" 
bdat1$locality[which(bdat1$locality=="Audubon National Wildlife Refuge, North Dacota")] <- "Audubon National Wildlife Refuge, North Dacota " 
bdat1$locality[which(bdat1$locality=="Cass river Valley, New Zealand")] <- "Cass river Valley + surrounding" 
bdat1$locality[which(bdat1$locality=="Elmley marshes, England, grazed marshes")] <- "Emley marshes, England" 
bdat1$locality[which(bdat1$locality=="Elmley marshes, England, ungrazed marshes")] <- "Emley marshes, England" 
bdat1$locality[which(bdat1$locality=="England, Scotland, Wales (Central & S England, N England, Scotland)")] <- "England, Scotland, Wales" 
bdat1$locality[grep("Freshwater lake and surrounding, Alberta", bdat1$locality)] <- "Freshwarter lake and surrounding, Alberta"
bdat1$locality[which(bdat1$locality=="Hardangervidda, Norway")] <- "Hardangervidda "
bdat1$locality[grep("Great Salt Lake", bdat1$locality)] <- "Great Salt Lake, Utah"
bdat1$locality[which(bdat1$locality=="Kaliningrad region")] <- "Kaliningrad"
bdat1$locality[grep("Khao Sam Roi", bdat1$locality)] <- "Khao Sam Roi Yod National Park"
bdat1$locality[which(bdat1$locality=="Long Bay Regional Park and Okura estuary, Auckland")] <- "Long Bay Regional Park, Auckland"
bdat1$locality[grep("Maio", bdat1$locality)] <- "Maio "
bdat1$locality[which(bdat1$locality=="Medusa Bay, Taimyr, 18km south of Dikson")] <- "Medusa Bay, Taimyr" 
bdat1$locality[which(bdat1$locality=="Mestersvig, Scoresby Land, Greenland" )] <- "Mestersvig, Scoresby Land,"
bdat1$locality[which(bdat1$locality=="North Dakota, South Dakota, Manitoba, Montana")] <- "North Dacota + some data also from South Dacota, Manitoba and Montana"
bdat1$locality[grep("Puerto Madryn",bdat1$locality)] <- "Puerto Madryn, Patagonia"
bdat1$locality[which(bdat1$locality=="St Cyrus, Scotland")] <- "Cyrus NNR, Scotland" 
bdat1$locality[which(bdat1$locality=="Trebonsko")] <- "T?ebo?sko" 
bdat1$locality[which(bdat1$locality=="Tsimanampetsotsa")] <- "Tsimanampetsotsa "
bdat1$locality[which(bdat1$locality=="Vembanur lake, India")] <- "Vembanur" 
bdat1$locality[which(bdat1$locality=="Lough Erne, Northern Ireland")] <- "Lough Erne,  Northern Ireland"
bdat1$locality[grep("Snares Islands Nature reserve", bdat1$locality)] <- "Snares Islands Nature reserve"
bdat1$locality[grep("Lustenau", bdat1$locality)] <- "Lustenau, Austrian Rhine Valley"
bdat1$locality[which(bdat1$locality=="Hiraethog, North Wales")] <- "Hiraethog,  North Wales"
bdat1$locality[which(bdat1$locality=="Nag Valley")] <- "Nag valley"
bdat1$locality[which(bdat1$locality=="Farne Islands, Northumberland")] <- "Lindisfarne NNR, North Englang"
bdat1$locality[grep("Yakutia", bdat1$locality)] <- "Yakutia, Lena River"
bdat1$locality[grep("Anadyr ", bdat1$locality)] <- "Anadyr surrounding"
bdat1$locality[which(bdat1$locality=="Bahia de Ceuta, Mexico")] <- "Ceuta"
bdat1$locality[which(bdat1$locality=="Kazakhstan")] <- "Korgalzhyn"
bdat1$locality[which(bdat1$locality=="Rangatira (South East) island New Zealand")] <- "Rangatira (South East) island"
bdat1$locality[which(bdat1$locality=="south part of Oland")] <- "south part of Oländ"

bdat1$locality[which(bdat1$locality %in% c("Alten Sorge-Scheife", "Meggerkoog"))] <- "Alke Sorge, Meggerdorf"
bdat1$locality[grep("Zackenberg", bdat1$locality)] <- "Zackenberg"

# clean up NAs --> unknown, if we have checked them:
unique(bdat1$who)
unique(bdat1$nest_search_frequency)
unique(bdat1$when_most_nests_found)
bdat1$nest_search_frequency[which(is.na(bdat1$nest_search_frequency) & bdat1$who!="bib_await")] <- "unknown"
bdat1$when_most_nests_found[which(is.na(bdat1$when_most_nests_found) & bdat1$who!="bib_await")] <- "unknown"
bdat1$nest_search_frequency[which(is.na(bdat1$nest_search_frequency) & bdat1$who=="bib_await")] <- "not checked"
bdat1$when_most_nests_found[which(is.na(bdat1$when_most_nests_found) & bdat1$who=="bib_await")] <- "not checked"

# Of Kubelka's 140 transformed populations, how many do we have info for?
kdat$B_incl <- kdat$source_id <- 0
for(i in 1:nrow(kdat)){
	mydat <- which(bdat$species==kdat$species[i] & bdat$Locality==kdat$Locality[i])
	if(length(mydat)>0) kdat$B_incl[i] <- 1
	kdat$source_id[i] <- sort(bdat$source_id[which(bdat$species==kdat$species[i] & bdat$Locality==kdat$Locality[i])])[1]
}

#############################################################

# Get Kubelka's obs_time and assumptions:

bdat$DPRtrans_k <- bdat$obs_time_k <- bdat$mean_yr_k <- bdat$Exp_k <- NA
for(i in 1:nrow(bdat)){
	k <- which(kdat$species==bdat$species[i] & kdat$Locality==bdat$Locality[i])
	bdat$mean_yr_k[i] <- kdat$mean_year[which(kdat$species==bdat$species[i] & kdat$Locality==bdat$Locality[i])]
	bdat$DPRtrans_k[i] <- paste(unique(kdat$DPRtrans[k]),collapse=(", "))
	bdat$obs_time_k[i] <- mean(unique(kdat$obs_time[k]),na.rm=TRUE)
	bdat$Exp_k[i] <- mean(unique(kdat$Exposure_days[k]),na.rm=TRUE)
}

# ONLY consider pops where DPR_trans==YES, but check if we differ there:
length(which(bdat$DPRtrans_k!=bdat$DPRtrans))/nrow(bdat)	# we often disagreed! 15% of records
# if I want to check those closer:
	bdat[which(bdat$DPRtrans_k!=bdat$DPRtrans & !(bdat$DPRtrans %in% c("NO, YES", "YES, NO")) & bdat$DPRtrans=="YES"),which(colnames(bdat) %in% c('species','Locality','period','DPRtrans','DPRtrans_k','predated','Exposure_days','obs_time','obs_time_k','source_id', "Exp_k"))]

cbind(bdat$DPRtrans_k, bdat$DPRtrans)[which(bdat$DPRtrans_k!=bdat$DPRtrans),]
# Sometimes, we disagreed because where data were available from multiple sources, we list "YES, NO" for each instead of choosing only one answer as apparently Kubelka did. For this comparison, change those to Kubelka's values (always true for one of the references!):
bdat$DPRtrans[which(bdat$DPRtrans %in% c("NO, YES", "YES, NO"))] <- bdat$DPRtrans_k[which(bdat$DPRtrans %in% c("NO, YES", "YES, NO"))]

# We still disagree in 11% of cases, though - in both directions:
length(which(bdat$DPRtrans_k!=bdat$DPRtrans))/nrow(bdat)
cbind(bdat$DPRtrans_k, bdat$DPRtrans)[which(bdat$DPRtrans_k!=bdat$DPRtrans),]
# Check the ones where we assumed Beintema but he did not: Are our exposure days really different? What about nest numbers?
dif <- bdat[which(bdat$DPRtrans_k!=bdat$DPRtrans),]
dif$Exp_k <- dif$N_k <- NA
for(i in 1:nrow(dif)){
	dif$Exp_k[i] <- sum(kdat$Exposure_days[which(kdat$species==dif$species[i] & kdat$Locality==dif$Locality[i])])
	dif$N_k[i] <- sum(kdat$N.nests[which(kdat$species==dif$species[i] & kdat$Locality==dif$Locality[i])])

}
par(mfrow=c(2,1))
plot(log(dif$N_k), log(dif$N.nests)); lines(c(0,1000), c(0,1000))# different, yes, but not shockingly so in any case... could be explained by abandoned, found as hatched, and maybe some cases of unknown-fate (we always excluded). Biggest diff is here:
dif[which(log(dif$N_k)>5 & log(dif$N.nests)<4),]	
bdat[which(bdat$species=="Calidris_pusilla" & bdat$Locality=="Kuparuk Oilfield, North Slope, Alaska"),]	# that's because our dataset separates before vs after 2000, while Kubelka do not in this datafile.
plot(log(dif$Exp_k), log(dif$Exposure_days)); lines(c(0,1000), c(0,1000))# somewhat different in some cases, but only one really stands out:
dif[which(log(dif$Exp_k)<6 & log(dif$Exposure_days)>6),]	# Hmm, they missed one whole year of data. So, we keep what we have here for sure. We also found exp_days in the ref so I am not sure why they did not.
# All the other exp_days are so close that I wonder if their trans method must have actually matched ours... They did fill obs_time for many pops that say DPRtrans=NO.

# Let's just proceed with those where we both agree that DPRtrans=YES:
nrow(bdat)
bdat <- bdat[which(bdat$DPRtrans==bdat$DPRtrans_k),]
nrow(bdat)	# only excludes 22 pops where we disagree
bdat <- bdat[which(bdat$DPRtrans=="YES"),]
nrow(bdat)	# 107 pops with Beintema trans

## For the pops included by both, how did our obs_times differ?:
both <- bdat[which(!is.na(bdat$obs_time) & !is.na(bdat$obs_time_k)),]; nrow(both)	# only 34 were checked by us TWICE, so I'm not including that here. However, our second check averaged closer to 0.5 than even our first one.
mean(both$obs_time)	# ours
mean(both$obs_time_k)	# Kubelka assumed longer obs time on avg


## How often were we different?

b <- round(both$obs_time,1)
k <- round(both$obs_time_k,1)
length(which(b!=k))/length(b)
length(which(k==0.5 & b==0.5))
length(which(k==0.5 & b==0.6))
length(which(k==0.5 & b==0.9))
length(which(k==0.6 & b==0.5))
length(which(k==0.6 & b==0.6))
length(which(k==0.6 & b==0.9))
length(which(k==0.9 & b==0.5))
length(which(k==0.9 & b==0.6))
length(which(k==0.9 & b==0.9))



######################################################

# When should we assume the default 0.5?

## How often (in first check) did we note anything known about nest-searching frequency?
sources <- unique(as.numeric(unlist(strsplit(bdat$source_id[which(!is.na(bdat$DPR) & bdat$DPRtrans=="YES")],","))))	# refs where Beintema required, and we got DPR
length(sources)
bdat1s <- bdat1[which(bdat1$source_id %in% sources),]	# where we got information AND needed Beintema
# How many REFERENCES had NO info for Beintema?
length(unique(bdat1s$source_id[which(
	(is.na(bdat1s$nest_search_frequency) | bdat1s$nest_search_frequency=="unknown")	& # missing nest search, AND:
		(is.na(bdat1s$when_most_nests_found) | bdat1s$when_most_nests_found=="unknown"))])	# missing nest age
	)/length(unique(bdat1s$source_id))	
# 49%. These should all take the default.

# How often did Kubelka assume 0.5? 
length(unique(kdat$References.and.notes[which(kdat$DPRtrans=="YES" & kdat$obs_time==0.5)]))/length(unique(kdat$References.and.notes[which(kdat$DPRtrans=="YES")]))
# Only 9% of studies!!!!!!!

## When we did NOT assume the default, how often did we match?
both96 <- both[which(both$obs_time!=0.5),]; nrow(both96)
nrow(both96[which(both96$obs_time==both96$obs_time_k),])
38/48

# Prop of populations in which we used 0.5 but Kubelka used higher:
length(which(both$obs_time==0.5 & both$obs_time_k > 0.5))/nrow(both)	# 58%



############################################

## PLOT: proportion in each group, and change over time:

# Set up function to return p-value from lm:
lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}

# Plot for each belt:
belts <- c("Arctic", "North temperate", "North tropics", "South tropics", "South temperate")

dev.new(width=9, height=6)
par(mfrow=c(2,5), oma=c(1,3,0.1,0.1), mai=c(0.8,0.1,0.1,0.1), mgp=c(3,0.7,0), xpd=FALSE, cex=1)
for(i in 1:length(belts)){
	# get proportion assigned to each obs_time by kubelka:
	k <- b <- rep(NA, 3)
	for(j in 1:3){
		k[j] <- length(which(round(both$obs_time_k[which(both$Belt==belts[i])],1)==c(0.5,0.6,0.9)[j]))/nrow(both[which(both$Belt==belts[i]),])
		b[j] <- length(which(round(both$obs_time[which(both$Belt==belts[i])],1)==c(0.5,0.6,0.9)[j]))/nrow(both[which(both$Belt==belts[i]),])
	}
	barplot(rbind(k, b), beside=TRUE, main="", xlab="", ylab="", xlim=c(0.5,9.5), ylim=c(0,1.3), yaxt="n", xaxt="n", las=1, col=c(adjustcolor("dark green",0.3),adjustcolor("dark orange", 0.2))); box()
	axis(side=1, at=c(2,5,8), labels=c(0.5,0.6,0.9))
	axis(side=2, at=seq(0,1,0.2), labels=(i==1), las=2)
	mtext(paste0(" ", letters[i], ") ", belts[i]), side=3, line=-1.2, adj=0)
	legend(x=-0.4, y=1.18, legend=c(paste0("Kubelka (m = ", round(mean(both$obs_time_k[which(both$Belt==belts[i])],na.rm=TRUE),2),")"), paste0("New (m = ", round(mean(both$obs_time[which(both$Belt==belts[i])],na.rm=TRUE),2),")")), fill=c(adjustcolor("dark green",0.3),adjustcolor("dark orange", 0.2)), bty="n", x.intersp=0.5, y.intersp=0.9,cex=0.8)
	if(i==1) mtext("Prop. populations", side=2, line=2.5)
	if(i==3) mtext("obs_time assumed", side=1, line=2)
}


# Did either of our assumptions change over time in any belt?
for(i in 1:length(belts)){
	plot(both$mean_yr[which(both$Belt==belts[i])], jitter(both$obs_time_k[which(both$Belt==belts[i])]), pch=16, cex=0.8, col=adjustcolor("dark green",0.3), ylim=c(0.4,1.2), xlim=range(both$mean_yr), yaxt="n", ylab="", xlab="")
	points(both$mean_yr[which(both$Belt==belts[i])], jitter(both$obs_time[which(both$Belt==belts[i])]), pch=17, cex=0.8, col=adjustcolor("dark orange",0.3))
	modk <- lm(both$obs_time_k[which(both$Belt==belts[i])]~both$mean_yr[which(both$Belt==belts[i])])
	abline(modk, col="dark green", lwd=2)
	modb <- lm(both$obs_time[which(both$Belt==belts[i])]~both$mean_yr[which(both$Belt==belts[i])])
	abline(modb, col="dark orange", lty=2, lwd=2)
	legend(x=0.998*par('usr')[1], y=1.15, legend=c(paste0("Kubelka (p = ", sprintf("%.2f", lmp(modk)),")"), paste0("New (p = ", sprintf("%.2f", lmp(modb)),")")), col=c("dark green","dark orange"), lty=c(1,2), lwd=2, bty="n", x.intersp=0.5, y.intersp=0.9,cex=0.8)
	axis(side=2, at=seq(0.4,1,0.1), labels=(i==1), las=2)
	mtext(paste0(" ", letters[i+5], ") ", belts[i]), side=3, line=-1.2, adj=0)
	if(i==3) mtext("Mean year", side=1, line=2)
	if(i==1) mtext("obs_time", side=2, line=2.5)

}
# We find a sig decline over time in North temperate. Otherwise, no sig changes, although our direction is different in the Arctic too (increase, while they find stable). So, if we accurately assessed obs_time in the Arctic, num pred / exp for recent years will be SMALLER in our assessment than in Kubelka's.
both[which(both$obs_time==0.9 & both$Belt=="Arctic"),]	# We found info for these pops - all from literature - to suggest nests were found very early. 2 before, 1 after 2000.

