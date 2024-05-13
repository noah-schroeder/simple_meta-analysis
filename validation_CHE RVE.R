###########################
#Preparation#
###########################

#load metafor
library(metafor)
library(ggplot2)
library(dplyr)
library(clubSandwich)


#name data file and read in .csv. 
dat1 <- read.csv("validdat_3lma.csv")

#set Rho
rho <- .60

V <- with(dat1, impute_covariance_matrix(vi = vi, cluster = Study, r = rho))

#multilevel model
m_multi <- rma.mv(yi,
                  V,
                  random = ~ 1 | Study/ES_number,
                  method = "REML",
                  test = "t",
                  dfs = "contain",
                  data = dat1) 
m_multi

robrob <- robust(m_multi, cluster = Study, clubSandwich = TRUE)
robrob
#calculate i2 for each level-copy paste function from https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/mlm.variance.distribution.R into console and hit enter
i2 <- var.comp(m_multi)
summary(i2)
i2

###########################
#Check for outliers#
###########################

#adapting CI calculation and plotting from https://cjvanlissa.github.io/Doing-Meta-Analysis-in-R/detecting-outliers-influential-cases.html
# Calculate CI for all observed effect sizes
dat1$upperci <- dat1$yi + 1.96 * sqrt(dat1$vi)
dat1$lowerci <- dat1$yi - 1.96 * sqrt(dat1$vi)
# Create filter variable
dat1$outlier <- dat1$upperci < m_multi$ci.lb | dat1$lowerci > m_multi$ci.ub
# Count number of outliers:
sum(dat1$outlier)
dat1
# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = dat1, aes(x = yi, colour = outlier, fill = outlier)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_multi$b[1]) +
  # Apply a black and white theme
  theme_bw()


##########################################
#influence check
#adapting method from https://wviechtb.github.io/metafor/reference/influence.rma.mv.html to calculate influence 
#cook's distance
cooks <- cooks.distance(robrob)
plot(cooks, type="o", pch=19, xlab="Observed Outcome", ylab="Cook's Distance")

#note dfbetas doesn't work with robust results so we use the 3 level model
dfbetas <-dfbetas(m_multi)
dfbetas

hatvalues <- hatvalues(robrob)
hatvalues

#################################################
#Moderator Analyses#########
################################################
####Control Condition
#calculate qb
mod.ctrlq <- rma.mv(yi,
                                V,
                                data = dat1,
                                random = ~ 1 | Study/ES_number, 
                                method = "REML",
                                test = "t",
                                dfs = "contain",
                                mods = ~ factor(control_c))
robust_q <- robust(mod.ctrlq, cluster = Study, clubSandwich = TRUE, digits = 3)
summary(robust_q)

#calculate levels
mod.ctrl <- rma.mv(yi,
                    V,
                    data = dat1,
                    random = ~ 1 | Study/ES_number, 
                    method = "REML",
                    test = "t",
                    dfs = "contain",
                    mods = ~ -1 + factor(control_c))
robust <- robust(mod.ctrl, cluster = Study, clubSandwich = TRUE, digits = 3)
summary(robust)

#run continuous
mod.cont <- rma.mv(yi,
                    V,
                    data = dat1,
                    random = ~ 1 | Study/ES_number, 
                    method = "REML",
                    test = "t",
                    dfs = "contain",
                    mods = ~ cont_fakedata)
robust_cont <- robust(mod.cont, cluster = Study, clubSandwich = TRUE, digits = 3)
summary(robust_cont)

################################################
#forest plot
forest(robrob, slab = dat1$Study)