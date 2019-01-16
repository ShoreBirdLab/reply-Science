# load and prepare phylogenetic trees
trees = read.tree(paste(wd,"trees2.phy",sep=""))
trees1 = lapply(trees, function( t ) addInTip( t , "Charadrius_alexandrinus","Charadrius_nivosus" )  )
trees2 = lapply( trees1, function( t ) addInTip( t , "Gallinago_gallinago", "Gallinago_delicata") ) 

# load and prepare predation data
d = read.csv(paste(wd,"DATApopulations.csv",sep=""), h = T, sep=";")
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

