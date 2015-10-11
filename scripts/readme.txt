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

3.