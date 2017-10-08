## Analysis of articulatory data for Muecke et al. "Thalamic Deep Brain Stimulation changes speech dynamics in patients with Essential Tremor"
## As requested by the reviewers we test the effect of stimulation order, since some patients arrived in DBS OFF and some arrived in DBS ON
## Author: Timo B. Roettger
## Contact: timo.roettger@uni-koeln.de
## Date: October 1st, 2017

## Load in necessary packages
library(lme4)

## Load in data
setwd("~")
acoustic  <- read.csv("DBS_acoustic_overall_reduced.csv")
articulation <- read.csv("DBS_articulation_overall_reduced.csv")

## scale continous predictors
acoustic$syllable.scale = scale(acoustic$syllable)
articulation$syllable.scale = scale(articulation$syllable)

## scale and log-transform measurements
acoustic$syll_dur_2 = scale(log(acoustic$syll_dur))
articulation$act_int = articulation$acc + articulation$dec   
articulation$act_int_2 = scale(log(articulation$act_int))
articulation$acc_2 = scale(log(articulation$acc))
articulation$dec_2 = scale(log(articulation$dec))
articulation$displ_2 = scale(log(articulation$displ))
articulation$pvel_yvel_2 = scale(log(articulation$pvel_yvel))
articulation$stiff_2 = scale(articulation$stiff)

## create order vector
acoustic$order.DBS <- ifelse(acoustic$order == "OFF-ON", "OFF-ON",
                             ifelse(acoustic$order == "ON-OFF", "ON-OFF", "Control"))

articulation$order.DBS <- ifelse(articulation$order == "OFF-ON", "OFF-ON",
                                 ifelse(articulation$order == "ON-OFF", "ON-OFF", "Control"))

## two subsets for Control vs. Off and Off vs. On
articulation.CvsOff = articulation[articulation$DBS != "ON",]
articulation.OffvsOn = articulation[articulation$DBS != "C",]
articulation.CvsOff$DBS = factor(articulation.CvsOff$DBS)
articulation.OffvsOn$DBS = factor(articulation.OffvsOn$DBS)

acoustic.CvsOff = acoustic[acoustic$DBS != "ON",]
acoustic.OffvsOn = acoustic[acoustic$DBS != "C",]
acoustic.CvsOff$DBS = factor(acoustic.CvsOff$DBS)
acoustic.OffvsOn$DBS = factor(acoustic.OffvsOn$DBS)

## two subsets for Control vs. Off and Off vs. On
acoustic.CvsOff = acoustic[acoustic$DBS != "ON",]
acoustic.OffvsOn = acoustic[acoustic$DBS != "C",]
acoustic.CvsOff$DBS = factor(acoustic.CvsOff$DBS)
acoustic.OffvsOn$DBS = factor(acoustic.OffvsOn$DBS)
acoustic.CvsOff$dbs.group = factor(acoustic.CvsOff$dbs.group)
acoustic.OffvsOn$dbs.group = factor(acoustic.OffvsOn$dbs.group)
articulation.CvsOff = articulation[articulation$DBS != "ON",]
articulation.OffvsOn = articulation[articulation$DBS != "C",]
articulation.CvsOff$DBS = factor(articulation.CvsOff$DBS)
articulation.OffvsOn$DBS = factor(articulation.OffvsOn$DBS)
articulation.CvsOff$dbs.group = factor(articulation.CvsOff$dbs.group)
articulation.OffvsOn$dbs.group = factor(articulation.OffvsOn$dbs.group)

## contrast code categorical predictors
contrasts(acoustic.CvsOff$POA) = contr.sum(3)/2
contrasts(acoustic.CvsOff$DBS) = contr.sum(2)/2
contrasts(acoustic.CvsOff$dbs.group) = contr.sum(3)/2
contrasts(acoustic.OffvsOn$POA) = contr.sum(3)/2
contrasts(acoustic.OffvsOn$DBS) = contr.sum(2)/2
contrasts(acoustic.OffvsOn$dbs.group) = contr.sum(2)/2
contrasts(articulation.CvsOff$POA) = contr.sum(3)/2
contrasts(articulation.CvsOff$DBS) = contr.sum(2)/2
contrasts(articulation.CvsOff$dbs.group) = contr.sum(3)/2
contrasts(articulation.OffvsOn$POA) = contr.sum(3)/2
contrasts(articulation.OffvsOn$DBS) = contr.sum(2)/2
contrasts(articulation.OffvsOn$dbs.group) = contr.sum(2)/2


## aggregate for table
acoustic.agg <- acoustic %>%
  group_by(DBS, order.DBS) %>%
  summarise(mean_syll_dur = mean(syll_dur, na.rm =T), 
            mean_voi_to_syll = mean(voi_to_syll, na.rm =T),
            mean_fri_dur_clo = mean(fri_dur_clo, na.rm =T),
            mean_voi_dur_clo = mean(voi_dur_clo, na.rm =T))
            
articulation.agg <- articulation %>%
  group_by(DBS, order.DBS) %>%
  summarise(mean_acc = mean(acc, na.rm =T), 
            mean_dec = mean(dec, na.rm =T),
            mean_displ = mean(displ, na.rm =T),
            mean_pvel_yvel = mean(pvel_yvel, na.rm =T),
            mean_stiff = mean(stiff, na.rm =T))

all.agg <- full_join(acoustic.agg,articulation.agg)
write.table(all.agg, file="mean.table.order.csv")
         

#############################################################
######### Additional Modeling for revision ##################
#############################################################

## We now want to access whether the order of DBS stimulation (on-off vs. off-on) affects the results of the main analysis.
## As a departure point, we take the specified models of the main analysis without the interaction terms between DBS and POA to simplify the interpretation.
## For the comparison of Control vs. DBS Off, we test whether a model with DBS order (i.e. control vs. OFF(order OFF-ON) vs. OFF(order ON-OFF)) 
## does improve the model fit significantly compared to a model with DBS only (i.e. control vs. OFF)
## For the comparison of DBS OFF vs. DBS ON, we test whether there is either an interaction of DBS order and DBS or a main effect of DBS order.

######### Acoustics ###########

#########
## syllable duration
#########

## model CvsOff
syll.CvsOff.POA.order  = lmer(syll_dur_2 ~ syllable.scale + order.DBS + POA + (1|subject), acoustic.CvsOff, REML=FALSE)
syll.CvsOff.POA  = lmer(syll_dur_2 ~ syllable.scale + DBS + POA + (1|subject), acoustic.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(syll.CvsOff.POA, syll.CvsOff.POA.order)      ## X2(1)=6.2422; p=0.01247 *    --> order does improve the model significantly
summary(syll.CvsOff.POA.order)


## model OffvsOn
syll.OffvsOn.POA.orderint  = lmer(syll_dur_2 ~ syllable.scale + order.DBS * DBS + DBS + POA + (1|subject), acoustic.OffvsOn, REML=FALSE)
syll.OffvsOn.POA.order = lmer(syll_dur_2 ~  syllable.scale + order.DBS + DBS + POA + (1|subject), acoustic.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(syll.OffvsOn.POA.order, syll.OffvsOn.POA.orderint)      ## X2(1)=23.989 ; p=9.687e-07 ***    --> interaction with order significantly improves model 
summary(syll.OffvsOn.POA.orderint)

#########
## voi-to-syll
#########

## model CvsOff
voi_to_syll.CvsOff.POA.order  = lmer(voi_to_syll ~ syllable.scale + order.DBS + POA + (1|subject), acoustic.CvsOff, REML=FALSE)
voi_to_syll.CvsOff.POA  = lmer(voi_to_syll ~ syllable.scale + DBS + POA + (1|subject), acoustic.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(voi_to_syll.CvsOff.POA, voi_to_syll.CvsOff.POA.order)      ## X2(1)=3.7468; p= 0.05291 .    --> order does NOT improve the model significantly
summary(voi_to_syll.CvsOff.POA.order)


## model OffvsOn
voi_to_syll.OffvsOn.POA.orderint  = lmer(voi_to_syll ~ syllable.scale + order.DBS * DBS + DBS + POA + (1|subject), acoustic.OffvsOn, REML=FALSE)
voi_to_syll.OffvsOn.POA.order = lmer(voi_to_syll ~  syllable.scale + order.DBS + DBS + POA + (1|subject), acoustic.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(voi_to_syll.OffvsOn.POA.order, voi_to_syll.OffvsOn.POA.orderint)      ## X2(1)=31.49 ; p=2.004e-08 ***   --> interaction with order significantly improves model 
summary(voi_to_syll.OffvsOn.POA.orderint)

#########
## frication
#########

## model CvsOff
fri_dur_clo.CvsOff.POA.order  = glmer(fri_dur_clo ~ syllable.scale + order.DBS + POA + (1|subject), acoustic.CvsOff, family = binomial)
fri_dur_clo.CvsOff.POA  = glmer(fri_dur_clo ~ syllable.scale + DBS + POA + (1|subject), acoustic.CvsOff, family = binomial)

## likelihood ratio tests
anova(fri_dur_clo.CvsOff.POA, fri_dur_clo.CvsOff.POA.order)      ## X2(1)=3.0821; p=0.07916 .    --> order does NOT improve the model significantly 
summary(fri_dur_clo.CvsOff.POA.order)


## model OffvsOn
fri_dur_clo.OffvsOn.POA.orderint = glmer(fri_dur_clo ~ syllable.scale + order.DBS * DBS + DBS + POA + (1|subject), acoustic.OffvsOn, family=binomial)
fri_dur_clo.OffvsOn.POA.order = glmer(fri_dur_clo ~  syllable.scale + order.DBS + DBS + POA + (1|subject), acoustic.OffvsOn, family=binomial)

## likelihood ratio tests
anova(fri_dur_clo.OffvsOn.POA.order, fri_dur_clo.OffvsOn.POA.orderint)      ## X2(1)=8.341 ; p=0.003876 **   --> interaction with order significantly improves model 
summary(fri_dur_clo.OffvsOn.POA.orderint)

#########
## voi_dur_clo
#########       

## model CvsOff
voi_dur_clo.CvsOff.POA.order  = glmer(voi_dur_clo ~ syllable.scale + order.DBS + POA + (1|subject), acoustic.CvsOff, family = binomial)
voi_dur_clo.CvsOff.POA  = glmer(voi_dur_clo ~ syllable.scale + DBS + POA + (1|subject), acoustic.CvsOff, family = binomial)

## likelihood ratio tests
anova(voi_dur_clo.CvsOff.POA, voi_dur_clo.CvsOff.POA.order)      ## X2(1)=1.4621; p=0.2266    --> order does NOT improve the model significantly 
summary(voi_dur_clo.CvsOff.POA.order)


## model OffvsOn
voi_dur_clo.OffvsOn.POA.orderint = glmer(voi_dur_clo ~ syllable.scale + order.DBS * DBS + DBS + POA + (1|subject), acoustic.OffvsOn, family=binomial)
voi_dur_clo.OffvsOn.POA.order = glmer(voi_dur_clo ~  syllable.scale + order.DBS + DBS + POA + (1|subject), acoustic.OffvsOn, family=binomial)

## likelihood ratio tests
anova(voi_dur_clo.OffvsOn.POA.order, voi_dur_clo.OffvsOn.POA.orderint)      ## X2(1)=2.5755 ; p=0.1085   --> NO interaction with order
summary(voi_dur_clo.OffvsOn.POA.orderint)

## exclude order
voi_dur_clo.OffvsOn.POA = glmer(voi_dur_clo ~  syllable.scale + DBS + POA + (1|subject), acoustic.OffvsOn, family=binomial)

## likelihood ratio tests
anova(voi_dur_clo.OffvsOn.POA, voi_dur_clo.OffvsOn.POA.order)      ## X2(1)=1.2686 ; p=0.26   --> no main effect of order


######### Articulation ###########

#########
## acceleration
#########

## model CvsOff
acc.CvsOff.POA.order  = lmer(acc_2 ~ syllable.scale + order.DBS + POA + (1|subject), articulation.CvsOff, REML=FALSE)
acc.CvsOff.POA  = lmer(acc_2 ~ syllable.scale + DBS + POA + (1|subject), articulation.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(acc.CvsOff.POA, acc.CvsOff.POA.order)      ## X2(3)=2.6466; p=0.1038    --> order does NOT improve the model significantly 
summary(acc.CvsOff.POA.order)

## model OffvsOn
acc.OffvsOn.POA.order.int  = lmer(acc_2 ~ syllable.scale + order.DBS * DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)
acc.OffvsOn.POA.order = lmer(acc_2 ~  syllable.scale + order.DBS + DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(acc.OffvsOn.POA.order, acc.OffvsOn.POA.order.int)      ## X2(2)=17.065 ; p=3.612e-05 ***   --> interaction with order significantly improves model 
summary(acc.OffvsOn.POA.order.int)

#########
## decceleration
#########

## model CvsOff
dec.CvsOff.POA.order  = lmer(dec_2 ~ syllable.scale + order.DBS + POA + (1|subject), articulation.CvsOff, REML=FALSE)
dec.CvsOff.POA  = lmer(dec_2 ~ syllable.scale + DBS + POA + (1|subject), articulation.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(dec.CvsOff.POA, dec.CvsOff.POA.order)      ## X2(3)=1.3998; p=0.2367    --> adding order, doesn't improve fit
summary(dec.CvsOff.POA.order)

## model OffvsOn
dec.OffvsOn.POA.order  = lmer(dec_2 ~ syllable.scale + order.DBS * DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)
dec.OffvsOn.POA = lmer(dec_2 ~  syllable.scale + order.DBS + DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(dec.OffvsOn.POA, dec.OffvsOn.POA.order)      ## X2(1)=0.4029 ; p=0.5256   --> NO interaction with order

## exclude order
dec.OffvsOn = lmer(dec_2 ~  syllable.scale + DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(dec.OffvsOn, dec.OffvsOn.POA)      ## X2(1)=0.9269 ; p=0.3357   --> NO main effect

#########
## displacement
#########

## model CvsOff
displ.CvsOff.POA.order  = lmer(displ_2 ~ syllable.scale + order.DBS + POA + (1|subject), articulation.CvsOff, REML=FALSE)
displ.CvsOff.POA  = lmer(displ_2 ~ syllable.scale + DBS + POA + (1|subject), articulation.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(displ.CvsOff.POA, displ.CvsOff.POA.order)      ## X2(1)=0.0258; p=0.8724    --> adding order, doesn't improve fit
summary(displ.CvsOff.POA.order)

## model OffvsOn
displ.OffvsOn.POA.order.int  = lmer(displ_2 ~ syllable.scale + order.DBS * DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)
displ.OffvsOn.POA.order = lmer(displ_2 ~  syllable.scale + order.DBS + DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(displ.OffvsOn.POA.order, displ.OffvsOn.POA.order.int)      ## X2(1)=1.3839 ; p=0.2394   --> NO interaction with order

## exclude order
displ.OffvsOn = lmer(displ_2 ~  syllable.scale + DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(displ.OffvsOn, displ.OffvsOn.POA.order)      ## X2(1)=0.2034 ; p=0.652   --> NO main effect

summary(displ.OffvsOn.POA.order)

#########
## pvel
#########

## model CvsOff
pvel.CvsOff.POA.order  = lmer(pvel_yvel_2 ~ syllable.scale + order.DBS + POA + (1|subject), articulation.CvsOff, REML=FALSE)
pvel.CvsOff.POA  = lmer(pvel_yvel_2 ~ syllable.scale + DBS + POA + (1|subject), articulation.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(pvel.CvsOff.POA, pvel.CvsOff.POA.order)      ## X2(1)=1.5408; p=0.2145    --> adding order, doesn't improve fit
summary(pvel.CvsOff.POA.order)

## model OffvsOn
pvel.OffvsOn.POA.order.int  = lmer(pvel_yvel_2 ~ syllable.scale + order.DBS * DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)
pvel.OffvsOn.POA.order = lmer(pvel_yvel_2 ~  syllable.scale + order.DBS + DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(pvel.OffvsOn.POA.order, pvel.OffvsOn.POA.order.int)      ## X2(1)=6.6971 ; p= 0.009657 **   --> adding order interaction improves fit
summary(pvel.OffvsOn.POA.order.int)

#########
## stiffness
#########

## model CvsOff
stiff.CvsOff.POA.order  = lmer(stiff_2 ~ syllable.scale + order.DBS + POA + (1|subject), articulation.CvsOff, REML=FALSE)
stiff.CvsOff.POA  = lmer(stiff_2 ~ syllable.scale + DBS + POA + (1|subject), articulation.CvsOff, REML=FALSE)

## likelihood ratio tests
anova(stiff.CvsOff.POA, stiff.CvsOff.POA.order)      ## X2(3)=5.26; p=0.02182 *   --> adding order, doesn't improve fit (after correction)


## model OffvsOn
stiff.OffvsOn.POA.orderint  = lmer(stiff_2 ~ syllable.scale + order.DBS * DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)
stiff.OffvsOn.POA.order = lmer(stiff_2 ~  syllable.scale + order.DBS + DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(stiff.OffvsOn.POA.order, stiff.OffvsOn.POA.orderint)      ## X2(1)=4.1122 ; p=  0.04257 *   --> adding order interaction doesn't improve fit (after correction)

## exclude order
stiff.OffvsOn = lmer(stiff_2 ~  syllable.scale + DBS + POA + (1|subject), articulation.OffvsOn, REML=FALSE)

## likelihood ratio tests
anova(stiff.OffvsOn, stiff.OffvsOn.POA.order)      ## X2(1)=4.099 ; p=0.04291 *   --> NO main effect (after correction)





