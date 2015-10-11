**Analysis notes**
Ideas?
- ZIPPoisson glmm with number seeded as on logoffset or binomial model
- Code example for ZIPP is in Zuur Owl example, starting on p27
- We think we might want to build both and check for model fit using diagnostics, either should fit the assumptions of the data but one has log transform one logit
- Do we need a mixture model?
	- Advantage: we could have different covariates for the 0/1 vs count
	- Disadvantage - more complicated model and perhaps not necessary unless we think there's different biology? 

- Do we need to model the coefficient on seeding trtmt as a function of # added.

- Day4 CSU stats notes has the mixture model, but with MCMC coded directly not in JAGS/STAN, which is probably more complicated than we want to bother with.


Complications

- What to with the plots where 2 species were seeded in?
	- ignore the 2nd species?
	- Add in the competitor species somewhere in the proportion germinated as a function of seeding density?
- Davos has subplots within plots.  So need to recode as transect, plot, subplot.
- Subplot: site:zone:transect:plot:trtmt:species
- Plot: site:zone:transect:plot EXCEPT for wolfcreek and canol trail and churchillMB- where we need to strip the species letter off the plot - double check with maika to figure out how she did it.
- What todo with the 2 different years of seed addition for Davos

What we've tried

