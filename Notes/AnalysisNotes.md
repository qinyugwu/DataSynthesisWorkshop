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

Discussion indicates that people did not do the vegetation surveys in the same way?  At least the 12mile did not?  Unsure about others.

What we've tried

**poissonGLMM - overdispersed and not**, in preliminary trials the overdispersed had better model fit (Bayesian p value)
y=count of seeds germinating in seeded-count of seeds germinating in nonseeded over all plots (or randomly discard plots til they are equal numbers)
~offset(totalviable# added at the site).

This model runs, but as we thought about it, it seemed a little illogical, because putting in the offset for the non-seeded treatments is odd. 

We think we could/should do this same model by adding the offset onto the bSeedT per plot, rather than onto the # seeds per plot.

Alternatively, we could run two separate models one on seeded&scarified vs seedednonscarified (with the offset), and the other on the two nonseeded treatments (with no offset).


**latent variable mixture model**
Sarah still thinks this might capture the biology best, though our original coding of it had a constant background emergence rate, and it should vary by scarification vs not, likely.  Tried to update that in the new model but haven't tried running it yet, likely it has some typos and other problems in it.
Also, we think technically it should be a mixture of poisson and binomial, but jags refused to run this.  If we love this idea, we could try to rewrite it in STAN.  We haven't done any assessments of model fit on this model, or model comparison vs other models, we only ran it on very preliminary data.