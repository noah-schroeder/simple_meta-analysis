##########preparation

#load metafor
library(metafor)
library(ggplot2)

#name data file and read in .csv. Change the \ to /
df360 <- read.csv("validdat_3lma.csv")

#calculate overall ES, in this case standardized mean dif hedges g, and variance.
dat1 <- escalc(measure="SMD", m1i=Exp_mean, sd1i=Exp_sd, n1i=Exp_n,
               m2i=Ctrl_mean, sd2i=Ctrl_sd, n2i=Ctrl_n, data=df360)
#display dataset with ES and variance
dat1


###########################
#fitting model#
###########################
#multilevel model
m_multi <- rma.mv(yi,
                  vi,
                  random = ~ 1 | Study/ES_number,
                  test = "t", 
                  data = dat1) 
m_multi

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
cooks <- cooks.distance(m_multi)
plot(cooks, type="o", pch=19, xlab="Observed Outcome", ylab="Cook's Distance")

#dfbeta
dfbetas <-dfbetas(m_multi)
dfbetas
#hatvalue
hatvalues <- hatvalues(m_multi)
hatvalues

#########################################
#moderator analyses#
#########################################
#calcualte qb categorical moderator
mod.ctrlq <- rma.mv(yi,
                  vi,
                  data = dat1,
                  random = ~ 1 | Study/ES_number, 
                  test = "t",
                  method = "REML",
                  mods = ~ factor(control_c))
summary(mod.ctrlq)

#calcualte ES categorical moderator
mod.ctrl <- rma.mv(yi,
                    vi,
                    data = dat1,
                    random = ~ 1 | Study/ES_number, 
                    test = "t",
                    method = "REML",
                    mods = ~ factor(control_c)-1)
summary(mod.ctrl)

#continuous mod
mod.cont_fake <- rma.mv(yi,
                     vi,
                     data = dat1,
                     random = ~ 1 | Study/ES_number, 
                     test = "t",
                     method = "REML",
                     mods = ~ cont_fakedata)
summary(mod.cont_fake)


