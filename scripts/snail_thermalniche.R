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
         actual_th=ocean_th_m + survey_th - th_m) #calculate surveyed TH


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
  
# plot os snail temp by species
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


## Linear regression
lm.mod <-lm(difftemp ~ log(surface_temp)*species, data=snails_calc)
anova(lr.mod)
check_model(lr.mod)

snails_calc %>%
  ggplot(aes(x=log(surface_temp), y=difftemp, color=species)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="lm", se=FALSE, method.args = list(family=binomial))+
  theme_bw()

