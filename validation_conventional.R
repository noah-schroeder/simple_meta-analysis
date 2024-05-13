##########preparation

#load metafor
library(metafor)
#name data file and read in .csv
dat1 <- read.csv("validdat_conv.csv")
#calculate overall ES, in this case standardized mean dif hedges g, and variance. intmean in this case is column headings
dat1 <- escalc(measure="SMD", m1i=Exp_mean, sd1i=Exp_sd, n1i=Exp_n,
               m2i=Ctrl_mean, sd2i=Ctrl_sd, n2i=Ctrl_n, data=dat1)
#display dataset with ES and variance
dat1



##########analyses

#run overall random effects meta-analysis
overallresult <- rma(yi, vi, data=dat1)
#display overall result
overallresult
#moderator test to calculate qbetween value - do not use means. Assumes common tau2 across subgroups
mod.ctrlq <- rma(yi, vi, mods = ~ factor(control.condition), data=dat1)
mod.ctrlq
#moderator test to get mean effect size for each group. mod.grade names the new data (mod for moderator, grade for grade range moderator). Factor is the name of the moderator variable (the column in coding form). The -1 removes intercept so it will run ANOVA type model
mod.ctrl <- rma(yi, vi, mods = ~ factor(control.condition)-1, data=dat1)
#Display moderator result
mod.ctrl
#continuous moderator test
mod.cont <- rma(yi, vi, mods = ~ cont_fake, data=dat1)
mod.cont

forest.rma(overallresult, slab = dat1$Study)


#########publication bias analyses

#standard funnel plot
funnel(overallresult)
# carry out trim-and-fill analysis
trimandfill <- trimfill(overallresult)
trimandfill
#Eggers regression
regtest(overallresult)
#Rosenthal, Orwin, & Rosenberg Fail Safe N test 
fsn(yi, vi, data=dat1)
fsn(yi, vi, data=dat1, type="Orwin")
fsn(yi, vi, data=dat1, type="Rosenberg")

#influence analysis
influence(overallresult)