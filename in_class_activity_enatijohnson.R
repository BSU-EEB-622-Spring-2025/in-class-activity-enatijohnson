library(tidyverse)
library(brms)
library(bayesplot)
library(marginaleffects)
library(ggplot2)
library(performance)

recordings <- read.csv("recordings.csv")
sensor <- read.csv("sensorinfo.csv")

merged <- recordings %>% left_join(sensor, by = "sensorid")

#remove 10th row
merged <- merged[-c(10), ]

merged$watertemp <- as.numeric(merged$watertemp)

#Question 1: Does sound from boats influence beluga whale song duration and frequency (total calls/hr)? 
  #Make a separate model for number of songs and for duration of songs
  #Include sensor and day as random effects
  #Leave out water depth and distance from shore because they are causal for boat activity,     which we are not interested in for this question
  #For count data (number of songs), start with poisson and use negative binomial if            overdispersion is detected
  #For continuous data (song length), use gamma

# poisson first, check for over dispersion, change to nb if over dispersed
mod1_totsongs <- brm(totsongs ~ scale(boatnoise) + scale(watertemp) + scale(boatactivity) + (1|sensorid) + (1|dayid), data = merged, family = poisson (link = "log"))

#Do a pp check

pp_check(mod1_totsongs) + xlim(c(0, 300)) # Can change number of draws using ndraws=...
## Does the posterior predictive check indicate improved fit?

# Given our choice of the poisson, we may also care about assessing the data's dispersion, which is the ratio of the variance to the mean:
dispersion <- function(x) {var(x)/mean(x)} #Create a function for dispersion

## Examine pp check for dispersion:
ppc_stat(y = merged$totsongs,  
         # Compare the dispersion in the real data (y)
         yrep = posterior_predict(mod1_totsongs, 
                                  ndraws = 1000), 
         # to dispersion in predictions from our posterior (yrep)
         stat="dispersion")


## Or, we can plot the full posterior curves:
mcmc_plot(mod1_totsongs, type="areas", prob = .90) + theme_bw()

#now do a negative binomial
mod1_totsongs <- brm(totsongs ~ scale(boatnoise) + scale(watertemp) + scale(boatactivity) + (1|sensorid) + (1|dayid), data = merged, family = negbinomial (link = "log"))

summary(mod1_totsongs)
bayes_R2(mod1_totsongs)
mae(mod1_totsongs)
mcmc_plot(mod1_totsongs, type="areas", prob = .90) + theme_bw()

#gamma
mod1_songlength <- brm(songlength ~ scale(boatnoise) + scale(watertemp) + scale(boatactivity) + (1|sensorid) + (1|dayid), data = merged, family = Gamma (link = "log"))

summary(mod1_songlength)
bayes_R2(mod1_songlength)
mae(mod1_songlength)
mcmc_plot(mod1_songlength, type="areas", prob = .90) + theme_bw()
#interpret using bayes_R2(), mae, summary table


#Question 2: Do boats have other direct impacts on whale calling behavior, outside of their noise impacts?
  #Again, separate models.
  #Leave out water temp and include water depth and distance from shore           (controlling for impacts on boat activity).
  #Same random effects.

mod2_totsongs <- brm(totsongs ~ scale(boatnoise) + scale(waterdepth) +scale(distshore) + scale(boatactivity) + (1|sensorid) + (1|dayid), data = merged, family = negbinomial (link = "log"))

summary(mod2_totsongs)
bayes_R2(mod2_totsongs)
mae(mod2_totsongs)
mcmc_plot(mod2_totsongs, type="areas", prob = .90) + theme_bw()


#gamma model
mod2_songlength <- brm(songlength ~ scale(boatnoise) + scale(waterdepth) +scale(distshore) + scale(boatactivity) + (1|sensorid) + (1|dayid), data = merged, family = Gamma (link = "log"))

summary(mod2_songlength)
bayes_R2(mod2_songlength)
mae(mod2_songlength)
mcmc_plot(mod2_songlength, type="areas", prob = .90) + theme_bw()
