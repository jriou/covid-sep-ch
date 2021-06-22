# covid-sep-ch

Code and supplementary material to the article 

Riou J., Panczak R., Althaus C.L., Junker C., Perisa D., Schneider K., Criscuolo N.G., Low N., Egger M. "Socioeconomic position and the cascade from SARS-CoV-2 testing to COVID-19 mortality: a population-based analysis of Swiss surveillance data", *Lancet Public Health* (accepted).

Observations:

* This code is not meant to be run directly (as the data cannot provided by default), but rather to better explain the approach and facilitate its adaptation to other contexts and datasets.

* We use `R` version `version 4.0.0` code and rely mainly on the following packages: `tidyverse 1.3.0`, `rstanarm 2.21.1` and `sf 1.0.0`.

* Extraction, management and formatting of the individual level data available at the Swiss Federal Office of Public health (FOPH) is done in `analyses/` folder using the R files with names starting with `FOPH-`. The objectives of these scripts are to 
1) apply exclusion criteria; 
2) create variables;
3) match individual geocodes with the closest Swiss neighbourhood index of socioeconomic position (SEP); 
4) match individual geocodes with the closest care facility (SOMED); 
5) aggregate the individual data by period, SEP decile, age group, sex, and canton.  

The original individual-level data is not available to share for confidentiality reasons.
The aggregated data is available on motivated request to the authors (julien.riou@ispm.unibe.ch). 

* The code for the main statistical analysis is in `analyses/run_models.R` file. This script is best run from  the console (e.g. using a high-performance computer cluster) with command `Rscript run_models.R i`, where `i` is the number of the dataset defined in `data_files`. The outputs are several sets of posterior samples for each relevant combination of model types, numerator and denominator. This script can be adapted to any data set, as long as it is aggregated and formatted in the same way:	

| Variable | Type | Comments |
| -------- | ---- | -------- |
| `canton`  | character | Canton or other geographical division |
| `period`  | double | First/second wave or other temporal division |
| `sex`  | double | Binary indicator 
| `age_group_f`  | factor | Age in 10-year bands |
| `age_group_f2`  | factor | Same as `age_group_f` but all age groups below 50 are pooled |
| `ssep_d`  | int | SEP index in 10 groups divided in deciles |
| `n_test`  | double | Counts of total tests |
| `n_pos`  | double | Counts of positive tests |
| `n_hospit`  | double | Counts of hospitalisations |
| `n_icu`  | double | Counts of ICU admissions |
| `n_death`  | double | Counts of deaths |
| `n_pop`  | double | Counts of population (to be used as a denominator only) |


* The scripts `paper_outputs.r` and all other scripts starting with `po_` in folder `analyses/` take the raw data and posterior samples to produce formatted results, tables and figures for the main paper and the supplementary.
