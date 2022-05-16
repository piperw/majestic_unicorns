## Thermal niche analyses for snails
## Edited by Nyssa, Piper, Lauren, and Laura

rm(list=ls())

### load libraries
library(tidyverse)
library(here)
library(janitor)
library(performance)
library(cowplot)
library(ggeffects)

# read in the snail data
snails<-read_csv(here("data","FieldTemperatures","SnailTemps.csv")) %>%
  clean_names() # to clean the names because of spaces
  

# Tide height ocean times
# 1/28/22  = -0.155 m 
# 1/29 = -0.243 m
## transects were 40-45m each


# Add in TH data and convert to meters
snails <- snails %>%
  mutate(survey_th = ifelse(snails$ocean_th==6.833, -0.155, -0.243),
    ocean_th_m=ocean_th*0.3048, #convert to meters
         th_m=th*0.3048,
         actual_th=ocean_th_m + survey_th - th_m) #calculate TH


# Analysis
snails_calc <- snails %>%
  mutate(difftemp = body_temp - surface_temp, # temp difference 
         difftemp_mm = (body_temp - surface_temp)/length) # normalized temp difference)

# summary stats         
snail_summary <- snails_calc %>%
  group_by(species) %>%
  summarize(counts = n(),
            meandiff = mean(difftemp),
            se = meandiff/sqrt(n()))

# plot of size normalized temperature per species
snails_calc %>%
  ggplot(aes(x = species, y = difftemp_mm)) +
  geom_boxplot()+
  geom_jitter(width = 0.2)
  
# plot of snail temp by species
snails_calc %>%
  ggplot(aes(x = species, y = difftemp)) +
  geom_boxplot()+
  geom_jitter(width = 0.2)

## diff temperature between internal and external  
mod<-lm(difftemp~species, data = snails_calc)
check_model(mod)
anova(mod)



### 
# thermal niche
snails_calc %>%
ggplot(aes(x = species, y = surface_temp, color = air_temp)) +
  geom_boxplot()+
  geom_jitter(width = 0.2)


## plot species temps by TH
snails_calc %>%
  ggplot(aes(x=actual_th, y=difftemp_mm, color=species)) +
  geom_smooth(method='lm') +
  geom_point() +
  theme_bw()

snails_calc %>%
  ggplot(aes(x=surface_temp, y=difftemp_mm, color=species)) +
  geom_smooth(method='lm') +
  geom_point() +
  theme_bw()


## add TH bins
snails_calc <- snails_calc %>%
  mutate(th_bins.25= cut(actual_th, breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0)), 
         th_bins.5 = cut(actual_th, breaks=c(-0.5, 0, 0.5, 1.0, 1.5, 2.0)),
         th_bins.75 = cut(actual_th, breaks=c(-.25, 0.5, 1.25, 2)))



## see how surface temp varies w/ TH
temp.mod <- aov(surface_temp~th_bins.5, data=snails_calc, na.action = na.omit)
check_model(temp.mod)
summary(temp.mod)
TukeyHSD(temp.mod)

snails_calc %>%
  ggplot(aes(x=th_bins.5, y=surface_temp, color=air_temp)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  theme_bw()



snails_calc$substrate <- if(substrate == "rock shade") {
  substrate == "crevice"}


## substrate types by TH
substrate <- snails_calc %>%
  group_by(th_bins.75, substrate) %>%
  summarise(obs = n())

substrate %>%
  ggplot(aes(x=substrate, y = obs)) +
  geom_bar(stat="identity") + 
  facet_grid(~th_bins.75) +
  theme_bw()




## Diff temp ~ surface/th
snails_calc %>%
ggplot(aes(x=surface_temp, y=difftemp_mm, color=species)) +
  geom_smooth(method='lm') +
  geom_point() +
  theme_bw()

snails_calc %>%
  ggplot(aes(x=th_bins.5, y=difftemp_mm, color=species)) +
  geom_boxplot() +
  geom_point() +
  theme_bw()




## Linear regression
lm.mod <-lm(difftemp ~ log(surface_temp)*species, data=snails_calc)
anova(lr.mod)
check_model(lr.mod)

snails_calc %>%
  ggplot(aes(x=log(surface_temp), y=difftemp, color=species)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="lm", se=FALSE, method.args = list(family=binomial))+
  theme_bw()


th.mod <-lm(difftemp ~ th_bins.5*species, data=snails_calc)
anova(th.mod)
check_model(th.mod)


snails_calc %>%
  ggplot(aes(x=th_bins.5, y=difftemp, color=species)) + 
  geom_boxplot() +
  theme_bw()


snails_calc %>%
  ggplot(aes(x=log(surface_temp), y=difftemp, color=species)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="lm", se=FALSE, method.args = list(family=binomial))+
  theme_bw()


## Try logisic regression 
snails.log <- snails_calc %>%
  mutate(logistic = ifelse(difftemp>0, 1, 0))

log.mod <- glm(logistic ~ log(surface_temp)*species, data = snails.log, family = binomial(link = logit))
summary(log.mod)
check_model(log.mod)

snails.log %>%
  ggplot(aes(x=log(surface_temp), y=logistic, color=species)) + 
  geom_point() +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  theme_bw()



## Positive Negative Model
snails.log <- snails.log %>%
  mutate(pos.neg = ifelse(difftemp>0, 1, -1))

lm.mod.pn <-glm(pos.neg ~ log(surface_temp)*species, data=snails.log, family="gamma")
anova(lm.mod.pn)
check_model(lm.mod.pn)

snails.log %>%
  ggplot(aes(x=log(surface_temp), y=pos.neg, color=species)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="gam", se=FALSE) +
  theme_bw()


