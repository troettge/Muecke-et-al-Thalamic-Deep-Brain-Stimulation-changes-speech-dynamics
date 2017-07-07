
## Analysis of acoustics data for Muecke et al. "Thalamic Deep Brain Stimulation changes speech dynamics in patients with Essential Tremor"

## Author: Timo B. Roettger
## Contact: timo.roettger@uni-koeln.de
## Date: May 30th, 2016


## Load in necessary packages
library(lme4)

## define function for correction of multiple testing
dunn.sidak <- function(alpha,n) 1-(1-alpha)^(1/n)
dunn.sidak(0.05, 4) ## so the new threshold for significance is 0.0127


#############################################################
################## P R E P R O C E S S ######################
#############################################################


## set working directory:
setwd("~")

## load in data
xdata  <- read.csv("DBS_acoustic_overall_reduced.csv")

## scale continous predictors
xdata$syllable.scale = scale(xdata$syllable)

## scale and log-transform measurements
xdata$syll_dur_2 = scale(log(xdata$syll_dur))

## two subsets for Control vs. Off and Off vs. On
xdata.CvsOff = xdata[xdata$DBS != "ON",]
xdata.OffvsOn = xdata[xdata$DBS != "C",]
xdata.CvsOff$DBS = factor(xdata.CvsOff$DBS)
xdata.OffvsOn$DBS = factor(xdata.OffvsOn$DBS)
xdata.OffvsOn$dbs.group = factor(xdata.OffvsOn$dbs.group)

## contrast code categorical predictors
contrasts(xdata.CvsOff$POA) = contr.sum(3)/2
contrasts(xdata.CvsOff$DBS) = contr.sum(2)/2
contrasts(xdata.OffvsOn$POA) = contr.sum(3)/2
contrasts(xdata.OffvsOn$DBS) = contr.sum(2)/2
contrasts(xdata.OffvsOn$dbs.group) = contr.sum(2)/2


#############################################################
################# M O D E L I N G ###########################
#############################################################

## First, testing for possible interactions of the control variables POA with the test variable DBS.
## If we come across a significant interaction, we report the interaction model, 
## If not we drop the interaction term and test whether inclusion of DBS improves the model significantly.


#########
## syllable duration
#########

## model CvsOff
syll.CvsOff.POA  = lmer(syll_dur_2 ~ syllable.scale + DBS * POA + (1|subject), xdata.CvsOff, REML=FALSE)
syll.CvsOff = lmer(syll_dur_2 ~  syllable.scale + DBS + POA + (1|subject), xdata.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(syll.CvsOff, syll.CvsOff.POA)      ## X2(2)=10.315; p=0.005756 **    --> interaction with POA

summary(syll.CvsOff.POA)

## main effect of DBS?
#syll.CvsOff.null = lmer(syll_dur_2 ~  syllable.scale + POA + (1|subject), xdata.CvsOff, REML=FALSE)
#anova(syll.CvsOff.null, syll.CvsOff)      ## X2(1)=6.9183; p=0.008532 ** 


## model OffvsOn
syll.OffvsOn.POA  = lmer(syll_dur_2 ~ syllable.scale + DBS * POA + (1|subject), xdata.OffvsOn, REML=FALSE)
syll.OffvsOn = lmer(syll_dur_2 ~  syllable.scale + DBS + POA + (1|subject), xdata.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(syll.OffvsOn, syll.OffvsOn.POA)      ## X2(2)=15.78; p=0.0003744 *** --> interaction with POA

summary(syll.OffvsOn.POA)

## main effect of DBS?
#syll.OffvsOn.null = lmer(syll_dur_2 ~  syllable.scale + POA + (1|subject), xdata.OffvsOn, REML=FALSE)
#anova(syll.OffvsOn.null, syll.OffvsOn)      ## X2(1)=124.51; p< 2.2e-16 ***  


#########
## voi-to-syll
#########

## model CvsOff
voi_to_syll.CvsOff.POA  = lmer(voi_to_syll ~ syllable.scale + DBS * POA + (1|subject), xdata.CvsOff, REML=FALSE)
voi_to_syll.CvsOff = lmer(voi_to_syll ~  syllable.scale + DBS + POA + (1|subject), xdata.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(voi_to_syll.CvsOff, voi_to_syll.CvsOff.POA)      ## X2(2)=1.6282; p=0.443         --> no interaction with POA

## main effect of DBS?
voi_to_syll.CvsOff.null = lmer(voi_to_syll ~  syllable.scale + POA + (1|subject), xdata.CvsOff, REML=FALSE)
anova(voi_to_syll.CvsOff.null, voi_to_syll.CvsOff)      ## X2(1)=0.6292; p=0.4277

summary(voi_to_syll.CvsOff)


## model OffvsOn
voi_to_syll.OffvsOn.POA  = lmer(voi_to_syll ~ syllable.scale + DBS * POA + (1|subject), xdata.OffvsOn, REML=FALSE)
voi_to_syll.OffvsOn = lmer(voi_to_syll ~  syllable.scale + DBS + POA + (1|subject), xdata.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(voi_to_syll.OffvsOn, voi_to_syll.OffvsOn.POA)      ## X2(2)=5.1707; p=0.07537    --> no interaction with POA 

## main effect of DBS?
voi_to_syll.OffvsOn.null = lmer(voi_to_syll ~  syllable.scale + POA + (1|subject), xdata.OffvsOn, REML=FALSE)
anova(voi_to_syll.OffvsOn.null, voi_to_syll.OffvsOn)      ## X2(1)=36.481; p=1.541e-09 ***

summary(voi_to_syll.OffvsOn)


#########
## frication
#########


## model CvsOff
fri_dur_clo.CvsOff.POA  = glmer(fri_dur_clo ~ syllable.scale + POA * DBS + (1|subject), xdata.CvsOff, family=binomial)
fri_dur_clo.CvsOff = glmer(fri_dur_clo ~  syllable.scale + DBS + POA + (1|subject), xdata.CvsOff, family=binomial)

## likelihood ratio tests
anova(fri_dur_clo.CvsOff, fri_dur_clo.CvsOff.POA)      ## X2(2)=24.348; p=5.162e-06 *** --> interaction with POA

summary(fri_dur_clo.CvsOff.POA)

## main effect of DBS?
#fri_dur_clo.OffvsOn.null = glmer(fri_dur_clo ~  syllable.scale + POA + (1|subject), xdata.CvsOff, family=binomial)
#anova(fri_dur_clo.OffvsOn.null, fri_dur_clo.CvsOff)      ## X2(1)=1.4727; p=0.2249


## model OffvsOn
fri_dur_clo.OffvsOn.POA  = glmer(fri_dur_clo ~ syllable.scale + POA * DBS + (1|subject), xdata.OffvsOn, family=binomial)
fri_dur_clo.OffvsOn = glmer(fri_dur_clo ~  syllable.scale + DBS + POA + (1|subject), xdata.OffvsOn, family=binomial)

## likelihood ratio tests
anova(fri_dur_clo.OffvsOn, fri_dur_clo.OffvsOn.POA)      ## X2(2)=3.5318; p=0.171       --> no interaction with POA

## main effect of DBS?
fri_dur_clo.OffvsOn.null = glmer(fri_dur_clo ~  syllable.scale + POA + (1|subject), xdata.OffvsOn, family=binomial)
anova(fri_dur_clo.OffvsOn.null, fri_dur_clo.OffvsOn)      ## X2(1)=41.116; p=1.434e-10 ***

summary(fri_dur_clo.OffvsOn)


#########
## voi_dur_clo
#########

## model CvsOff
voi_dur_clo.CvsOff.POA  = glmer(voi_dur_clo ~ syllable.scale + POA * DBS + (1|subject), xdata.CvsOff, family=binomial)
voi_dur_clo.CvsOff = glmer(voi_dur_clo ~  syllable.scale + DBS + POA + (1|subject), xdata.CvsOff, family=binomial)

## likelihood ratio tests
anova(voi_dur_clo.CvsOff, voi_dur_clo.CvsOff.POA)      ## X2(2)=1.415; p=0.4929       --> no interaction with POA

## main effect of DBS?
voi_dur_clo.CvsOff.null = glmer(voi_dur_clo ~  syllable.scale + POA + (1|subject), xdata.CvsOff, family=binomial)
anova(voi_dur_clo.CvsOff.null, voi_dur_clo.CvsOff)      ## X2(1)=1.4341; p=0.2311 

summary(voi_dur_clo.CvsOff)


## model OffvsOn
voi_dur_clo.OffvsOn.POA  = glmer(voi_dur_clo ~ syllable.scale + POA * DBS + (1|subject), xdata.OffvsOn, family=binomial)
voi_dur_clo.OffvsOn = glmer(voi_dur_clo ~  syllable.scale + DBS + POA + (1|subject), xdata.OffvsOn, family=binomial)

## likelihood ratio tests
anova(voi_dur_clo.OffvsOn, voi_dur_clo.OffvsOn.POA)      ## X2(2)=7.9246; p=0.01902 *     -->  no interaction with POA (after correction)

## main effect of DBS?
voi_dur_clo.OffvsOn.null = glmer(voi_dur_clo ~  syllable.scale + POA + (1|subject), xdata.OffvsOn, family=binomial)
anova(voi_dur_clo.OffvsOn.null, voi_dur_clo.OffvsOn)      ## X2(1)=23.762; p=1.09e-06 *** 

summary(voi_dur_clo.OffvsOn)



