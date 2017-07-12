
## Analysis of articulatory data for Muecke et al. "Thalamic Deep Brain Stimulation changes speech dynamics in patients with Essential Tremor"

## Author: Timo B. Roettger
## Contact: timo.roettger@uni-koeln.de
## Date: May 30th, 2016


## Load in necessary packages
library(lme4)

## define function for correction of multiple testing
dunn.sidak<-function(alpha,n) 1-(1-alpha)^(1/n)
dunn.sidak(0.05, 4) ## so the new threshold for significance is 0.0127


#############################################################
################## P R E P R O C E S S ######################
#############################################################


## set working directory:
setwd("~")

## load in data
xdata  <- read.csv("DBS_articulation_overall_reduced.csv")

## scale continous predictors
xdata$syllable.scale = scale(xdata$syllable)

## scale and log-transform measurements
xdata$act_int = xdata$acc+xdata$dec   
xdata$act_int_2 = scale(log(xdata$act_int))
xdata$acc_2 = scale(log(xdata$acc))
xdata$dec_2 = scale(log(xdata$dec))
xdata$displ_2 = scale(log(xdata$displ))
xdata$pvel_yvel_2 = scale(log(xdata$pvel_yvel))
xdata$stiff_2 = scale(xdata$stiff)

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

## First, testing for possible interactions of the control variables POA and syllable with the test variable DBS.
## If we come across interactions, we report the interaction model, 
## If not we drop the interaction term and test whether inclusion of DBS improves model significantly.


#########
## acceleration
#########

## model CvsOff
acc.CvsOff.POA  = lmer(acc_2 ~ syllable.scale + DBS * POA + (1|subject), xdata.CvsOff, REML=FALSE)
acc.CvsOff = lmer(acc_2 ~  syllable.scale + DBS + POA + (1|subject), xdata.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(acc.CvsOff, acc.CvsOff.POA)      ## X2(2)=9.6888 ; p=0.007872 **   --> interaction with POA

summary(acc.CvsOff.POA)

## main effect of DBS?
#acc.CvsOff.null = lmer(acc_2 ~  syllable.scale + POA + (1|subject), xdata.CvsOff, REML=FALSE)
#anova(acc.CvsOff.null, acc.CvsOff)      ## X2(1)=6.5825 ; p=0.0103 * 


## model OffvsOn
acc.OffvsOn.POA  = lmer(acc_2 ~ syllable.scale + DBS * POA + (1|subject), xdata.OffvsOn, REML=FALSE)
acc.OffvsOn = lmer(acc_2 ~  syllable.scale + DBS + POA + (1|subject), xdata.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(acc.OffvsOn, acc.OffvsOn.POA)      ## X2(2)=6.7131 ; p=0.03486 *    --> no interaction with POA (after correction)

## main effect of DBS?
acc.OffvsOn.null = lmer(acc_2 ~  syllable.scale + POA + (1|subject), xdata.OffvsOn, REML=FALSE)
anova(acc.OffvsOn.null, acc.OffvsOn)      ## X2(1)=52.215 ; p=4.974e-13 ***  

summary(acc.OffvsOn)


#########
## deceleration
#########


## model CvsOff
dec.CvsOff.POA  = lmer(dec_2 ~ syllable.scale + DBS * POA + (1|subject), xdata.CvsOff, REML=FALSE)
dec.CvsOff = lmer(dec_2 ~  syllable.scale + DBS + POA + (1|subject), xdata.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(dec.CvsOff, dec.CvsOff.POA)      ## X2(2)=3.2811 ; p=0.1939       --> no interaction with POA

## main effect of DBS?
dec.CvsOff.null = lmer(dec_2 ~  syllable.scale + POA + (1|subject), xdata.CvsOff, REML=FALSE)
anova(dec.CvsOff.null, dec.CvsOff)      ## X2(1)=8.3334 ; p=0.003892 **

summary(dec.CvsOff)


## model OffvsOn
dec.OffvsOn.POA  = lmer(dec_2 ~ syllable.scale + DBS * POA + (1|subject), xdata.OffvsOn, REML=FALSE)
dec.OffvsOn = lmer(dec_2 ~  syllable.scale + DBS + POA + (1|subject), xdata.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(dec.OffvsOn, dec.OffvsOn.POA)      ## X2(2)=0.5322 ; p=0.7664        --> no interaction with POA

## main effect of DBS?
dec.OffvsOn.null = lmer(dec_2 ~  syllable.scale + POA + (1|subject), xdata.OffvsOn, REML=FALSE)
anova(dec.OffvsOn.null, dec.OffvsOn)      ## X2(1)=37.59 ; p=8.728e-10 ***  

summary(dec.OffvsOn)


#########
## displacement
#########

## model CvsOff
displ.CvsOff.POA  = lmer(displ_2 ~ syllable.scale + DBS * POA + (1|subject), xdata.CvsOff, REML=FALSE)
displ.CvsOff = lmer(displ_2 ~  syllable.scale + DBS + POA + (1|subject), xdata.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(displ.CvsOff, displ.CvsOff.POA)      ## X2(2)=6.5426 ; p=0.03796 *  --> no interaction with POA (after correction)

## main effect of DBS?
displ.CvsOff.null = lmer(displ_2 ~  syllable.scale + POA + (1|subject), xdata.CvsOff, REML=FALSE)
anova(displ.CvsOff.null, displ.CvsOff)      ## X2(1)=5.5316 ; p= 0.01868 * not significant after correction 

summary(displ.CvsOff)


## model OffvsOn
displ.OffvsOn.POA  = lmer(displ_2 ~ syllable.scale + DBS * POA + (1|subject), xdata.OffvsOn, REML=FALSE)
displ.OffvsOn = lmer(displ_2 ~  syllable.scale + DBS + POA + (1|subject), xdata.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(displ.OffvsOn, displ.OffvsOn.POA)      ## X2(2)=5.329 ; p=0.06963 .    --> no interaction with POA

## main effect of DBS?
displ.OffvsOn.null = lmer(displ_2 ~  syllable.scale + POA + (1|subject), xdata.OffvsOn, REML=FALSE)
anova(displ.OffvsOn.null, displ.OffvsOn)      ## X2(1)=37.311 ; p=1.007e-09 *** 

summary(displ.OffvsOn)


#########
## pvel_yvel
#########

## model CvsOff
pvel.CvsOff.POA  = lmer(pvel_yvel_2 ~ syllable.scale + DBS * POA + (1|subject), xdata.CvsOff, REML=FALSE)
pvel.CvsOff = lmer(pvel_yvel_2 ~  syllable.scale + DBS + POA + (1|subject), xdata.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(pvel.CvsOff, pvel.CvsOff.POA)      ## X2(2)=2.1844 ; p=0.3355 --> no interaction with POA

## main effect of DBS?
pvel.CvsOff.null = lmer(pvel_yvel_2 ~  syllable.scale + POA + (1|subject), xdata.CvsOff, REML=FALSE)
anova(pvel.CvsOff.null, pvel.CvsOff)      ## X2(1)=2.5327 ; p= 0.1115 

summary(pvel.CvsOff)


## model OffvsOn
pvel.OffvsOn.POA  = lmer(pvel_yvel_2 ~ syllable.scale + DBS * POA + (1|subject), xdata.OffvsOn, REML=FALSE)
pvel.OffvsOn = lmer(pvel_yvel_2 ~  syllable.scale + DBS + POA + (1|subject), xdata.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(pvel.OffvsOn, pvel.OffvsOn.POA)      ## X2(2)=4.9895 ; p= 0.08252 .  -->no interaction with POA

## main effect of DBS?
pvel.OffvsOn.null = lmer(pvel_yvel_2 ~  syllable.scale + POA + (1|subject), xdata.OffvsOn, REML=FALSE)
anova(pvel.OffvsOn.null, pvel.OffvsOn)      ## X2(1)=104.1 ; p<2.2e-16 ***  

summary(pvel.OffvsOn)


#########
## stiffness
#########

## model CvsOff
stiff.CvsOff.POA  = lmer(stiff_2 ~ syllable.scale + DBS * POA + (1|subject), xdata.CvsOff, REML=FALSE)
stiff.CvsOff = lmer(stiff_2 ~  syllable.scale + DBS + POA + (1|subject), xdata.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(stiff.CvsOff, stiff.CvsOff.POA)      ## X2(2)=10.322 ; p=0.005737 **  --> interaction with POA

summary(stiff.CvsOff.POA)

## main effect of DBS?
#stiff.CvsOff.null = lmer(stiff_2 ~  syllable.scale + POA + (1|subject), xdata.CvsOff, REML=FALSE)
#anova(stiff.CvsOff.null, stiff.CvsOff)      ## X2(1)=3.2634 ; p=0.07084 . 

## model OffvsOn
stiff.OffvsOn.POA  = lmer(stiff_2 ~ syllable.scale + DBS * POA + (1|subject), xdata.OffvsOn, REML=FALSE)
stiff.OffvsOn = lmer(stiff_2 ~  syllable.scale + DBS + POA + (1|subject), xdata.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(stiff.OffvsOn, stiff.OffvsOn.POA)      ## X2(2)=1.2037; p=0.5478  --> no interaction with POA

## main effect of DBS?
stiff.OffvsOn.null = lmer(stiff_2 ~  syllable.scale + POA + (1|subject), xdata.OffvsOn, REML=FALSE)
anova(stiff.OffvsOn.null, stiff.OffvsOn)      ## X2(1)=68.17 ; p< 2.2e-16 ***  

summary(stiff.OffvsOn)
