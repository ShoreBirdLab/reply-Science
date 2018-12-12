Sys.setenv(TZ="UTC") # set time to UTC to have always same time
require("anytime")
require("data.table")
require("googlesheets")

for_gs = gs_title("Kubelka_et_al_rebuttal")
d = data.table(gs_read(for_gs, ws = 'nests'))
a = data.table(gs_read(for_gs, ws = 'authors'))
sp = data.table(gs_read(for_gs, ws = 'species'))
s = data.table(gs_read(for_gs, ws = 'site'))
so = data.table(gs_read(for_gs, ws = 'source'))

# check predefined variables
nrow(d[!(author %in% unique(a[,abbreviation]))])
	summary(factor(a$abbreviation))
summary(factor(d$year))
nrow(d[!(species %in% unique(sp[,abbreviation]))])
	summary(factor(d$species))
	
nrow(d[!(site %in% unique(s[,abbreviation]))])
summary(factor(d$site))

summary(factor(d$first_egg_meth))
summary(factor(d$f_exp))
summary(factor(d$m_exp))
summary(factor(d$nest_exp))
summary(factor(d$study_site_exp))

# check continuous
summary(abs(as.numeric(d$lat)))
d[abs(lat)<10] 
summary(d$lon)
d[is.na(lon)]
summary(as.numeric(d$eggs))
summary(factor(d$eggs)) # how 0 or NA or 5-8 eggs possible?
nrow(d[is.na(eggs)])


# check
	d[, nest_found_t := anydate(nest_found, asUTC=TRUE)]
	d[, first_egg_t := anydate(first_egg, asUTC=TRUE)]
	d[, last_visit_before_end_t := anydate(last_visit_before_end, asUTC=TRUE)]
	d[, end_t := anydate(end_, asUTC=TRUE)]
d[is.na(nest_found_t)]
d[is.na(first_egg_t)]
d[is.na(end_t)]
d[is.na(last_visit_before_end_t)]

d[nest_found_t<first_egg & !is.na(first_egg)]
d[nest_found_t>last_visit_before_end_t]
d[nest_found_t>end_t]
d[first_egg_t>end_t]


