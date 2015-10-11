GTREE synthesis script descriptions.

1. scripts/readOriginalExcelFile.R
inputs:Background/GTREE_Master_30Sept2015.xlsx
Converts excel file downloaded from Dropbox to csvs for the repository and
removes whitespace and encoded formatting (line endings, etc)
1a.Requires scripts/rem_specialCharacters.R function to do the tidying
writes out csvs 

2. scripts/cleanupCSV.R
inputs:Data/plotData.csv; Data/siteData.csv
cleanup of factor coding, date formatting, typos, etc in original files
and infilling of 0s.
outputs: Data/cleanData/plotDataClean.csv; Data/cleanData/siteDataClean.csv

3.scripts/subsetPlotData.r
inputs: Data/cleanData/plotDataClean.csv; Data/cleanData/siteDataClean.csv
PREPARE#PREPARE DATA TO PLAY WITH FOR ANALYSIS, CUTTING OUT COMPLICATIONS
#TO MAKE A TESTING DATASET
#ABJ 10/11/2015
#ISSUES - nearly all needs to be redone once the final data are prepared,
#we threw out a lot.
outputs:'data/subsets/plotDatasub.csv'

4.draftLatencyModel.r
inputs: Data/subsets/plotDatasub.csv
Start of bayesian model with a latent variable of background recruitment rate
10/11/2015 This runs but the model structure isn't quite right
We at least need to background emergence rate vary by scarification
Add in transects
Marginalize over the tossocks

4b. mixed_models.R
inputs: Data/subsets/plotDatasub.csv
Tried to just run a regular overdispersed poisson - could not get convergence even trying different optimizers.
We could go back and try this with the full data.

4c. draftPoisson.R
inputs: Data/subsets/plotDatasub.csv
Bayesian ZIpoisson on subset
creates figures/treatment_effect_poisson.pdf ->a picture of the coefficients from the regular poisson

4d. draftPoissonOverdisp.R
inputs: Data/subsets/plotDatasub.csv
Bayesian ZIpoisson on subset
creates figures/treatment_effect_poisson.pdf ->a picture of the coefficients from the overdispersed
