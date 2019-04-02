rm( list = ls() )	


# set working and output directories
wd = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Kubelka_et_al_rebuttal/Analyses/'

source(paste(wd, 'Constants_Functions.R',sep=""))

d = read.csv(paste(wd,'obs_check.csv', sep =''), stringsAsFactors = FALSE)	
d$who = d$who2
d = d[,c('who','source_id','species','ref_abb','locality','nest_search_frequency',	'when_most_nests_found')]
d$trial = 2
dd = d[-which(is.na(d$who) | d$who%in%c("*in russian*","*in german(?)*",'*in spanish*',"")),]
dd = dd[!duplicated(paste(dd$source_id)),]
nrow(dd)
#dd = dd[!duplicated(paste(dd$source_id,dd$ref_abb,dd$species,dd$locality)),]

b = read.csv(paste(wd,'newly_extracted.csv', sep =''), stringsAsFactors = FALSE)
b = b[,c('who','source_id','species','ref_abb','locality','nest_search_frequency',	'when_most_nests_found')]
b$trial = 1
bb = b[b$source_id%in%dd$source_id,]
#table(bb$source_id,bb$nest_search_frequency)
bb = bb[!duplicated(bb$source_id),]
summary(factor(bb$nest_search_frequency))
summary(factor(bb$when_most_nests_found))
bb$nest_search_frequency[bb$nest_search_frequency==""] = "unknown"
bb$when_most_nests_found[bb$when_most_nests_found==""] = "unknown"
bb$nest_search_frequency[bb$nest_search_frequency=="once-twice per week"] = "1-2 per week"

#dd = dd[!(!dd$source_id%in%bb$source_id),]

summary(factor(dd$nest_search_frequency))
summary(factor(dd$when_most_nests_found))

xx = merge(bb,dd, by = 'source_id')
nrow(xx)
xx$search_same = ifelse(xx$'nest_search_frequency.y' == xx$nest_search_frequency.x,1,0)
xx$found_same = ifelse(xx$'when_most_nests_found.y' == xx$when_most_nests_found.x,1,0)

summary(xx)
sum(xx$search_same)/nrow(xx)
sum(xx$found_same)/nrow(xx)
