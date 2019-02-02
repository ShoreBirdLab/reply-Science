### Check our lit extraction against Kubelka's for DPR.
## Saves a file that records where our values agree or disagree.
# < 1 min to run.

rm(list=ls(all=TRUE))
options(scipen=10,stringsAsFactors=FALSE)
memory.limit(size=8000000000)

# Read our data from script R04:
bdat <- read.csv("Rebuttal_data_checked_K_incl.csv", header=TRUE)
# Kubelka's data:
kdat1 <- read.csv("Kubelka et al._2018_Science_additional datafile_1.csv", header=TRUE)
# the above lists DPR averaged across periods for those pops where both before and after 2000 are included. To get period-specific DPR, we need to refer to this:
kdat2 <- read.csv("Before+After2000_final.csv", header=TRUE)

# add locality to kdat2, and use that from here down:
kdat2$Locality <- NA
for(i in 1:nrow(kdat2)){
	loc <- unique(kdat1$Locality[which(round(kdat1$Latitude,3)==round(kdat2$Latitude[i],3) & round(kdat1$Longitude,3)==round(kdat2$Longitude[i],3))])
	if(length(loc)>1) 	loc <- unique(kdat1$Locality[which(round(kdat1$Latitude,3)==round(kdat2$Latitude[i],3) & round(kdat1$Longitude,3)==round(kdat2$Longitude[i],3) & kdat1$species==kdat2$species[i])])
	kdat2$Locality[i] <- loc
}

# rename for simplicity:
kdat2$Period[which(kdat2$Period=="After 2000")] <- "after"
kdat2$Period[which(kdat2$Period=="Before 2000")] <- "before"
kdat <- kdat2

# Of the species/locality present in both files, where do our DPR estimates differ?
bdat <- bdat[which(bdat$K_incl==1),]

# add K DPR and TPR to each row:
# in some cases, Kubelka did not split even when possible, e.g. site "Nag valley" had 1999-2001 but they included all as "after". So we will skip those, and their DPR for the other period will not match ours.
bdat$DPR_k <- bdat$TPR_k <- NA
for(i in 1:nrow(bdat)){
	mydat <- kdat[which(kdat$species==bdat$species[i] & kdat$Locality==bdat$Locality[i] & kdat$Period==bdat$period[i]),]
	if(nrow(mydat)>0){
		bdat$DPR_k[i] <- mydat$DPR_orig
		bdat$TPR_k[i] <- mydat$TPR_orig_100
	}
}
# Exclude those pops missing DPR_k from this comparison:
nosp <- bdat$species[which(is.na(bdat$DPR_k))]
nost <- bdat$Locality[which(is.na(bdat$DPR_k))]
no <- which(bdat$species %in% nosp & bdat$Locality %in% nost)
if(length(no)>0) bdat <- bdat[-no,]	# leaves 193 pops for comparison

# We won't worry about very small differences; round the values (K's were already rounded to 5 places):
bdat$DPR <- round(bdat$DPR,4)
bdat$DPR_k <- round(bdat$DPR_k,4)
bdat$diff <- bdat$DPR-bdat$DPR_k
bdat$pdiff <- bdat$diff/bdat$DPR
range((bdat$pdiff)[which(bdat$DPR>0)], na.rm=TRUE)	# some are -870% to 100% different!

# Show:
plot(bdat$DPR~bdat$DPR_k); lines(c(0,1), c(0,1))
hist(bdat$diff)
hist(bdat$pdiff)

bdat[which(bdat$DPR_k>0.1 & bdat$DPR<0.07),]# Kubelka's very high DPRs are from a population where they excluded a lot of data that we included.
bdat[which(bdat$DPR_k<0.01 & bdat$DPR>0.05),] # Where Kubelka's are very low and ours are higher, we usually had errors. We fixed those; the remaining appear legitimate.

# save this file with both our values and Kubelka's, excluding pops we have not yet assessed:
write.csv(bdat[,-which(colnames(bdat) %in% c('diff','pdiff'))], "Rebuttal_data_with_DPR_yrs_pooled_and_Kubelka.csv", row.names=FALSE)

# Some other causes of discrepancies:
	# Kubelka seems to assume that "failed to unknown cause" = depredated, even in systems where other causes of failure are more common (flooding, farm operations). We do not make this assumption, and I think we should not. Occasionally, it seems Kubelka excluded these nests entirely (even if explicitly "failed" to unknown cause).
	# Kubelka inconsistently treated abandoned nests; sometimes included in other_failed (as we did, and should) and sometimes excluded entirely.
	# Differences in obs_time will lead to differences in DPR when DPRtrans==YES.
	# See also some specific notes below, but I did not make a complete list of the discrepancies I checked.
	
# The discrepancies are most important if they are more pronounced before vs after 2000 within each belt. Show how our values correspond to theirs in each group:	
belts <- c("Arctic", "North temperate", "North tropics", "South tropics", "South temperate")
per <- c("before","after")
library(gplots)


dev.new(width=4.5, height=8)
par(mfrow=c(5,2), oma=c(3,3,1,1), mai=c(0.1,0.1,0.1,0.1), mgp=c(3,0.7,0), xpd=FALSE, cex=1)
ymax <- max(c(bdat$DPR_k, bdat$DPR))
for(i in 1:length(belts)){
	beltdat <- bdat[which(bdat$Belt==belts[i]),]
	for(j in 1:length(per)){
		perdat <- beltdat[which(beltdat$period==per[j]),]

		plot(x=perdat$DPR_k, y=perdat$DPR, pch=16, xlim=c(0,ymax), ylim=c(0, ymax), ylab="", xlab="", xaxt="n", yaxt="n", las=2, cex.axis=0.8, col=adjustcolor("black", 0.3),cex=0.8)
		lines(c(0,1), c(0,1))
		if(i==1) mtext(paste(per[j],"2000"), side=3, line=0.1)
		if(j==2) mtext(belts[i], side=4, line=0.1, las=3)
		if(i==3 & j==1) mtext("New estimate", side=2, line=2.5)
		axis(side=1, at=seq(0,1,0.05), labels=(i==5))
		axis(side=2, at=seq(0,1,0.05), labels=(j==1), las=2)

		if(i==length(belts)) axis(side=1, at=c(1,2), labels=c("Incl.","All"))
		if(i==length(belts) & j==1) mtext("Kubelka estimate", side=1, line=2, at=0.23)
	}
}
# yes, the most pronounced discrepancies are Arctic after 2000, where the Soloviev et al. 2010 data were excluded for some species (while included for others) by Kubelka et al., resulting in much smaller sample sizes.
# some of the discrepancies are due to errors or differences of opinion in extracting values from the literature. We have fixed all the errors in our own database that we have found.
		
