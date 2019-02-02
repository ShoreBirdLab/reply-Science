--------------------------------------------------------------------------------------------------------

Description of the scripts and data to generate the results and visualisations, from 
   Comment on “Global pattern of nest predation is disrupted by climate change in shorebirds.”   
   by Bulla et al 2019
  

--------------------------------------------------------------------------------------------------------

WHEN USING any of the Supporting information, PLEASE CITE both the original paper and the Supporting information

--------------------------------------------------------------------------------------------------------

CONTENT
1. Supplementary Methods & Results, Figures & Tables
2. Data
    a) csv file with newly extracted data (see column definitions below)
    b) sources.xlsx
3. Scripts
4. Model_assumptions
5. Acknowledgements

--------------------------------------------------------------------------------------------------------

1. Supplementary Methods & Results, Figures & Tables contain detailed procedures and arguments complementing the Technical Comment

--------------------------------------------------------------------------------------------------------
2a.newly_extracted.csv 
	- contains data newly extracted in standardized way from sources cited in Kubelka et al., including left out populations and yeary estimates
	- comma delineated 
who	first letter of given name and last name abbreviation of person who entered the data (in same form as in 'author' sheet - enter your details there too!) 
source_id	number of the source as per Kubelka et al's numbered reference list
species	exact species name as used by Kubelka et al (underscore between genus and species name)
ref_abb	exact short reference abbreviation as used by Kubelka et al
locality	study site. Sites within 40 km of each other may be merged
start_year	first year of the study - essential if estimates cover multiple years
year	year from which the estimates comes (if specified); 'all' indicates from 'start_year' to 'end_year'. "all" is not given when year-specific data are given.
end_year	last year of the study 
nests_total	number of nests in the study
nests_hatched	number of nests hatched (if provided)
nests_predated	number of nests depredated (if provided)
nest_abandoned	Number of nests abandoned or deserted (if provided)
nest_infertile	Number of nests with infertile eggs surviving the whole incubation period (if provided)
nest_failed_other_cause	Number of nests that failed to other causes than predation or desertion (e.g. trampled, flooded) (if provided)
nests_unknown_fate	Number of nests with unknown fate (not clear whether it hatched or failed) (if provided)
nests_total_failed	Total number of nests that failed and cause of failure not specified (just overall number of failed nests given)
prop_hatched	Only needed if number of hatched is not given. Proportion of monitored nests that hatched.
prop_predated	Only needed if number of depredated is not given. Proportion of monitored nests that failed to depredation.
daily_survival	daily survival (if given)
ds_lwr/ds_upr	lower and upper uncertainty around the daily survival estimate; if only +/- value given for lwr substract from estimate, for upr add to estimate (if given)
daily_predation_rate	daily predation rate (if given)
dp_lwr/upr	lower and upper uncertainty around the daily predation estimate; if only +/- value given for lwr substract from estimate, for upr add to estimate (if given)
overall_nest_success	only if daily survival not given - overall total survival - expected probability of hatching - not proportion of hatch = daily survival rate^incubation period (if given)
os_lwr/upr	uncertainty around the overall survival estimate - expected probability of depredation - not proportion of predated (if given)
overall_predation_rate	only if daily predation not given - overall or total predation rate (if given)
op_lwr/upr	uncertainty around the overall predation estimate (if given)
uncertainty	type of the lwr and upr uncertainty bounds: se - standard error, 95%CI, sd - standard deviation, unknown
exposure_days	total number of days, for which nests were folowed - summed across all nests, if given. (e.g. nest1 = 5days, nest2 = 2days, exposure = 7); Can be estimated from other values, but fill this value only if the study gives it explicitly.
estimation_method	if given, 'mayfield', 'bayesian','unknown'
nest_exposure_interval	whether daily survival/predation calculations included only time nest was actually 'observed' (used by Mayfield 1961), the whole incubation period ('incubation'), also laying ('lay+inc'), or unknown; if study does not specify, we write "unknown", but if the method used was mayfield, we can later assume "observed"
nest_search_frequency	How often were nest searches conducted? Choose from: "daily", "1-2 per week", "less than weekly", "unknown"
nest_check_frequency	How often were nests checked after they were found? Choose from: "daily", "every 2nd day","1-2 per week", "less than weekly", "unknown"
when_most_nests_found	during "laying", "early incubation", "mid incubation, "hatching","unknown"
incubation_period	Average number of days from start of incubation to hatching (exluding egg-laying) for the species, if given; use mean if range presented
egg_laying_period	Average number of days it take to lay a complete clutch of the species, if given; use mean if range presented
study_anomaly	"unknown","no", "yes" - something strange going on at the site that could influence estimates (predation control - shooting of foxes; invasive species) - describe in the comment column
comments	Anything to note about the values found in the reference.
Data_quality	5 = all data found easily; 4 = at least one value derived (e.g. throught the average); 3 = data difficult to obtain, e.g. some data derived interpreting a graph; 2 = data subject to interpretation or partial (to double-check); 1 = bad data (to double-check)	

2b. sources.xlsx contains informatin on sources found in Kubelka et al with unique number for each sources (as listed in Kubelka et al)

--------------------------------------------------------------------------------------------------------

3. Scripts to replicate the analyses 
	- in order to run properly, 
		a) place the scripts in the same directory as the above mentioned data files
		b) place Kubela et al's data files (provided at http://hdl.handle.net/10255/dryad.194844) in the same directory as the scripts
		   (DATApopulations.csv, trees2.phy, Kubelka et al._2018_Science_additional datafile_1.xlsx)
		b) define the path to this directory in "wd" at the top of each script
		c) define the path to the desired output directory in "outdir" at the top of each script
	- Constants_Functions.R & Prepare_data.R are necessary to run the remaining scripts 	
	- Prepare_predictions_Fig1 is necessary to run the script Fig1.R
	- R01-R07 should be run in numerical order; all deal with our newly extracted dataset (including new calculations and comparisons with Kubelka et al.'s values).
	- The names of the remaining R-scripts indicate the figure or table in the main text and supplement
	
--------------------------------------------------------------------------------------------------------

4. Model_assumptions folder - contains pngs of model assumptions for each model used int the paper or its supplement. The name indicates type of the model and data used in the model, the exact model is at top of each PNG 