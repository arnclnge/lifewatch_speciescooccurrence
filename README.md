# LifeWatch Species Co-occurrence

Herein this repository were codes used to analyze species co-occurrence using available Passive Acoustic Monitoring (PAM) and Acoustic Telemetry (AT) data from the European Tracking Network (ETN). We created hourly presence-absence matrices for each species of interest and applied some of the most common analyses previously used to study species co-occurrence. 

# Preparation of data

a. Raw data was extracted from https://lifewatch.be/etn. Data is under moratorium and access must be granted by data owners. See https://www.lifewatch.be/etn/assets/docs/ETN-DataPolicy.pdf

b. Cleaning, organizing and merging of data is found in organising_data.Rmd. Hourly presence-absence matrices were made for each species which were then merged. This code results to the final data set (DPH_final.csv) used for the subsequent analyses. A distinction was made between true absence (DPH=0) and uncertainty of presence/absence (DPH = NA) by creating a dataframe of all hours when CPODs/receivers were both active and fish had active acoustic transmitters. 

# Data visualization

Plotting of available data for each station, mapping out the stations and plotting of heat maps for species detected from the BPAN. 

# Data analyses

a. MonthlyOccupancy.R: Pairwise species monthly occupancy using the cooccur package (Griffith et al., 2016).

b. CScore.R: Quantification of association between species pairs based on the number of shared stations using the EcoSimR package (Gotelli et al., 2015).

c. DielOverlap.R: Graphing of hourly overlap of presence of species and calculation of overlap estimates using the overlap package (Meredith & Ridout, 2021).

d. sb_porp_glmm.R, cod_porp_glmm.R, dol_porp_glmm.R:  Generalized Linear Mixed Effects/Generalized Linear Models for each species pair (Bates et al., 2022). 

# References


Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting Linear Mixed-Effects Models Using 
lme4. Journal of Statistical Software, 67(1), 1–48. https://doi.org/10.18637/jss.v067.i01

Gotelli, ., Hart, ., & Ellison, . (2015). EcoSimR: Null Model Analysis for Ecological Data. Zenodo. 
https://doi.org/10.5281/zenodo.16636

Griffith, D. M., Veech, J. A., & Marsh, C. J. (2016). Cooccur: Probabilistic species co-occurrence analysis in 
R. Journal of Statistical Software, 69(1), 1–17. https://doi.org/10.18637/jss.v069.c02

Meredith, A. M., Ridout, M., & Meredith, M. M. (2021). Package ‘overlap.’ 
https://cran.r-project.org/web/packages/overlap/overlap.pdf








