# LINEAR MIXED EFFECT MODELS
library(tidyverse)
library(lme4)
library(MuMIn)

d <- tibble(day=runif(100,90,120),
            year=rep(1999:2023, 4),
            wind=rnorm(100,mean=10),
            temp=rnorm(100,mean=10),
            species=rep(letters[1:4],25))

#fixed effects: effects that we care about on the outcome
#but what about random effects that might affect patterns in fixed effects? Maybe species a, b, c, d
  #have different arrival times? But we want to know if arrival time for any species are predicted by
  #the variables - we care about the OVERALL pattern
  #here, species is the RANDOM EFFECT, meaning we allow the response of arrival time to maybe vary
    #according to species

#including random effects allow us to control for this

d_fit <- lmer(day~temp*wind+(1|species), d, na.action="na.fail")
  #day predicted by temperature and wind (and the two interact between themselves: *)
  #we also allow an error term, to allow the species to have their own errors
  #notice year is not in the model

  #nested model: can remove variables from the full model and run AICs on the nested model, 
    #predicting day only on wind, or only temperature, or even the model with both variables but
    #no interactions

dredge(d_fit)
  #dredging = running AICs on all nested variations of full model

#now we can evaluate the best model
Anova(d_fit)

#repeated measures analysis: for each species, we have measured it a number of time (over years) --> 
  #each species is not independent
#so should be doing linear mixed effects modeling to deal with that

