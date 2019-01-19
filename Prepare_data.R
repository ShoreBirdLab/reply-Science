# load and prepare phylogenetic trees
trees = read.tree(paste(wd,"trees2.phy",sep=""))
trees1 = lapply(trees, function( t ) addInTip( t , "Charadrius_alexandrinus","Charadrius_nivosus" )  )
trees2 = lapply( trees1, function( t ) addInTip( t , "Gallinago_gallinago", "Gallinago_delicata") ) 

# load citation data
n = readWorksheetFromFile(paste(wd, 'literature_check.xlsx',sep = ""), colTypes = 'character', sheet = "Data")	
n = n[,c("ref_num","abb_ref","locality")]

# load and prepare data later shared (and added to Dryad) by Kubelka - we have split the 'species' column into two (used in Science = species, and other name species2)
b = readWorksheetFromFile(paste(wd, 'Kubelka et al._2018_Science_additional datafile_1.xlsx',sep = ""), colTypes = 'character', sheet = "DATA")	
b = b[order(b$species,b$Latitude, b$mean_year),]

 # assign citation number
 b$source_id = NA
 b$source_id = as.character(b$source_id)
 for(i in 1:nrow(n)){
	#i = 1
	ni = n[i,]
	b$source_id = ifelse(grepl(ni$abb_ref, b$References.and.notes),ifelse(is.na(b$source_id), ni$ref_num, paste(b$source_id, ni$ref_num, sep=',')), b$source_id)
	print(ni$abb_ref)
	}
#b[is.na(b$source_id),]	

b$DPRtrans[b$pk == 23]='NO' # correct a typo - the estimate for this nest was not transformed by Kubelka et al 
b$obs_time = as.numeric(b$obs_time)
b$Incubation_days = as.numeric(b$Incubation_days)
b$predated = as.numeric(b$predated)
b$hatched = as.numeric(b$hatched)
b$infertile = as.numeric(b$infertile)
b$Latitude = as.numeric(b$Latitude)
b$mean_year = as.numeric(b$mean_year)
b$DPR_orig = as.numeric(b$DPR_orig)
b$Exposure_days = as.numeric(b$Exposure_days)
b$N_nests = as.numeric(b$N.nests)
b$"Failed_together." = as.numeric(b$"Failed_together.")
b$other_failed = as.numeric(b$other_failed)

b$site = paste(b$Latitude,b$Longitude) # define site
b$lat_abs = abs(b$Latitude) # abs latitude
b$ln_N_nests = log(b$N_nests)
b$hemisphere =as.factor(ifelse(b$Latitude > 0, "Northern", "Southern"))
b$genus = gsub("\\_.*","",b$species)
	
# load and prepare predation data used in the paper
d = read.csv(paste(wd,"DATApopulations.csv",sep=""), h = T, sep=";",stringsAsFactors = FALSE)
d$source_id = b$source_id[match(paste(d$species, d$mean_year, round(d$Latitude,2)),paste(b$species, b$mean_year, round(b$Latitude,2)))] 
#d[is.na(d$source_id),c('species','mean_year','Latitude')]
#d[is.na(d$source_id),]
#x = d$species[is.na(d$source_id)]
#b[b$species%in%x,c('species','mean_year','Latitude')]
d$site = paste(d$Latitude,d$Longitude) # define site
d$lat_abs = abs(d$Latitude) # abs latitude
d$ln_N_nests = log(d$N_nests)
d$hemisphere =as.factor(ifelse(d$Latitude > 0, "Northern", "Southern"))
d$genus = gsub("\\_.*","",d$species)

# prepare phylogenetic, distance and PI matricies
tree2 = trees2[[42]]
phyloMat <- vcv.phylo( tree2 )
phyloMat <- phyloMat / max( phyloMat )

distanceMatrix <- dist.mat( d$Latitude, d$Longitude, d$species) 
diag( distanceMatrix ) <- diag(distanceMatrix) + 0.01
distanceMatrix <- distanceMatrix / 1.01

I0 <- diag(1, dim(d)[1] )
rownames(I0) <- colnames(I0) <- d$species 
 I <- diag(1 / d$N_nests )
I <- I / max(I)
rownames(I) <- colnames(I) <- d$species 

