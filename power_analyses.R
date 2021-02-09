# power analyses

# load packages
library(WebPower)
library(effectsize)

wp.rmanova(n=28, ng=2, nm=2, nscor=1, power=.8, type = 0) # between effect
wp.rmanova(n=28, ng=2, nm=2, nscor=1, power=.8, type = 1) # within effect
wp.rmanova(n=28, ng=2, nm=2, nscor=1, power=.8, type = 2) # inx effect

f_to_eta2(0.55)

