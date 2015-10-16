**Analysis notes**

- Data structure = poisson component (number of 'background' seedlings emerging + binomial component (# emerging per # added for the treatment)

- This data would naturally lend itself to a mixture model, with the poisson component for the nonseeded plus a binomial component for the seeding.

- You can add an overdispersion or zero inflation component to either model.

- We tried to implement this in jags but got errors.
It might be possible to move to stan and try to implement this, but another reasonable option seemed to be to deal with the variability in #s of viable seeds added to the different plots by using an offset.
- I also have notes from Day4 CSU stats notes on how to directly code the mixture model in R, but with MCMC coded directly not in JAGS/STAN, which is probably more complicated than we want to bother with but if we get desperate could rethink this.

- I haven't used offsets that much before, so did a little work with a simple example to convince myself it would work, using the script simulatePoissonDataWithOffset.  You can also read a little about it in the nest calls example in Zuur's mixed model book

- In our jags models, we tried two approaches, one using a latent model for the naturally recruiting seedlings, and a second a simpler poisson model with 4 treatments.  I think we may have finally convinced ourselves that if you use the poisson for both parts of the process (natural recruitment + seeding) they will give you the same answer; if you want the binomial for the seed addition, the latent structure makes a little more sense.

- We checked the Zuur zero inflated models book on how to make zero inflated models in lmer and using bugs (we write in jags so they will run on linux/mac too, but it's pretty much like bugs, see Zuur owl example starting on p27 if you want to reference). We did not have success in making this run.  Also, on reflection, we don't know how to do the offset correctly in this model. We had originally left it in as it is in the data, but number of seeds added should actually be 0 for all the non-seeded plots.  I think if we want to pursue doing it in lmer, we are going to need to break it into two separate datasets, one all seeded, and one all not-seeded.  We can't get a p-value on the seeded vs not seeded comparison out of these - though we can get a confidence interval on the estimate of the # seedlings in seeded trtmts, this can't be modeled with the controls in the same model (unless we ignore # of seeds added, which seems like a bad idea). 

- our plan was to write an overdispersed and not-overdispersed model, and then pick the one with better fit based on model diagnostics.

- so far our only model diagnostic is the bayesian P value.  Also, I modified the overdispersed but not yet the not overdispersed, so we'll need to make sure the models match before checking which is better.


**STILL TO DO**
- Decide whether we want the latent variable parameterization or the 4 trtmt poisson.  We were leaning toward the poisson, but on further reflection, this is a multiplicative model and I'm not sure the biology dictates that adding seeds would have any multiplicative effect on the # of seedlings emerging over a baseline of no seeds.   

- Add in more model comparisons and diagnostics:
    - DIC
    - BIC
    - G?  Need to look through my notes on this from the Bayesian course
    - Plot pdf of posteriors on priors to make sure we're not doing something silly with our priors

- Add in species effects - probably as a random effect?
- Add in the correct germination rates from the real data
- Add in covariates of interest - bare ground +temp site and anomalyTemp for year of emergence seem like the obvious ones  
- Run models at additional zones- this one is only for 'A' sites   
- Ensure no crowding term is needed - i.e. are we getting some sort of saturating effect with declining #s of emergents per seed added in very high density seedings?  
- Add in a transect within site random effect
- Add in a subplot within plot random effect (needed for Davos) 
  - Davos has subplots within plots.  So need to recode as transect, plot, subplot.
  - Subplot: site:zone:transect:plot:trtmt:species
  - Plot: site:zone:transect:plot EXCEPT for wolfcreek and canol trail and churchillMB- where we need to strip the species letter off the plot - double check
- Add in a term to marginalize the means for craggie burn over the tussock vs nontussock area

-If we want to add in any veg covariates, need to triple check that the zeros are filled in correctly.  Currently there are some sites that have all zeros for cover of some things (like forbs) simply because they did not record those data, NOT because they are 0.  

What we've tried:

**poissonGLMM - overdispersed and not**, in preliminary trials the overdispersed had better model fit (Bayesian p value)
y=count of seeds germinating in seeded-count of seeds germinating in nonseeded over all plots (or randomly discard plots til they are equal numbers)
~offset(totalviable# added at the site).

This model runs, but as we thought about it, it seemed a little illogical, because putting in the offset for the non-seeded treatments is odd. 

We think we could/should do this same model by adding the offset onto the bSeedT per plot, rather than onto the # seeds per plot.

Alternatively, we could run two separate models one on seeded&scarified vs seedednonscarified (with the offset), and the other on the two nonseeded treatments (with no offset).


**latent variable mixture model**
Sarah still thinks this might capture the biology best, though our original coding of it had a constant background emergence rate, and it should vary by scarification vs not, likely.  Tried to update that in the new model but haven't tried running it yet, likely it has some typos and other problems in it.
Also, we think technically it should be a mixture of poisson and binomial, but jags refused to run this.  If we love this idea, we could try to rewrite it in STAN.  We haven't done any assessments of model fit on this model, or model comparison vs other models, we only ran it on very preliminary data.