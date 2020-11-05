#version 6-3-2019

#update if necessarry
#install.packages("installr")
#require(installr)
#updateR()
#install.packages("MatchIt")
#install.packages("optmatch")
#install.packages("sandwich")

require(foreign)
require("MatchIt")
require(optmatch)
require(survival)
require(sandwich)

#start with clean work
rm(list=ls())


ev <- read.dta("C:\\Users\\Administrator\\Documents\\Shared\\artikel azc\\data\\evazcRs12.dta")
#ev <- read.dta("G:\\SURFdrive_docs\\Shared\\artikel azc\\data\\evazcRs12.dta")

names(ev)
ev$threat_t1 <- (2*ev$threat_mean - ev$threat_afw)/2
ev$threat_t2 <- (2*ev$threat_mean + ev$threat_afw)/2

#define treatment variable: whether there has been an increase in total asylum seekers.  
ev$treatment <- ifelse(ev$tijd==1, as.numeric(ev$s123pc4_afw>0), as.numeric(ev$s123pc4_afw<0))
#check if treatment is not correlated with pretreatment outcome
table(ev$treatment[ev$tijd==0],ev$y_pvv[ev$tijd==0])
prop.table(table(ev$treatment[ev$tijd==0],ev$y_pvv[ev$tijd==0]), margin=1)
#no selective treatment!
#both pvv support just below 17%

#define treatment variable based on crisis ASC only: whether there has been an increase in total asylum seekers.  
ev$treatment2 <- ifelse(ev$tijd==1, as.numeric(ev$s1pc4_afw>0), as.numeric(ev$s1pc4_afw<0))
#check if treatment2 is not correlated with pretreatment outcome
table(ev$treatment2[ev$tijd==0],ev$y_pvv[ev$tijd==0])
prop.table(table(ev$treatment2[ev$tijd==0],ev$y_pvv[ev$tijd==0]), margin=1)
#more selectivity: in treated group 20.1% in untreated group 16.6%

#match data on time heterogeneity in crisis ASC
duur <- read.csv2("C:\\Users\\u838156\\surfdrive\\Shared\\artikel azc\\submission WEP\\resubmit\\Adressen alle AZCs_17022017.csv")
#duur <- read.csv2("G:\\SURFdrive_docs\\Shared\\artikel azc\\submission WEP\\resubmit\\Adressen alle AZCs_17022017.csv")
names(duur)
duur <- duur[,c("PC4", "Soort", "Cap_Gem", "Duur.tot.4.11")]
names(duur) <- c("pc4", "soort", "cap", "duur")
duur <- duur[duur$soort==3,]
sort(unique(duur$pc4[duur$cap>0]))

duur$s3_altop <- duur$duur * duur$cap
duur2 <- aggregate(duur[,c("cap", "duur", "s3_altop")], by=list(duur$pc4), FUN=sum)
duur2
names(duur2)[1] <- c("pc4")

ev <- merge(ev, duur2, all.x=T)
ev$duur[is.na(ev$duur)] <- 0
ev$s3_altop[is.na(ev$s3_altop)] <- 0
ev$cap[is.na(ev$cap)] <- 0

names(ev)
#don't forget to count number refugees per 1000 inhabitants
#ev$duur <- 1000 * ev$duur / ev$inw2014_pc4
ev$s3_altop2 <- 1000 * ev$s3_altop / (ev$inw2014_pc4 * ev$pauto2014_pc4)
ev$cap2 <- 1000 * ev$cap / (ev$inw2014_pc4 * ev$pauto2014_pc4)
ev$s3_altop <- 1000 * ev$s3_altop / ev$inw2014_pc4
ev$cap <- 1000 * ev$cap / ev$inw2014_pc4

cor((ev$s3_altop[ev$tijd==1 & ev$s3_altop>0]), ev$cap[ev$tijd==1 & ev$s3_altop>0])
#[1] 0.7461982
plot((ev$s3_altop[ev$tijd==1 & ev$s3_altop>0]), ev$cap[ev$tijd==1 & ev$s3_altop>0])
?cor

cor((ev$s3_altop[ev$tijd==1 & ev$s3_altop>0]), ev$cap[ev$tijd==1 & ev$s3_altop>0], method="spearman")
#[1] 0.5258987

plot((ev$cap[ev$tijd==1]) , 2*(ev$s3pc4_afw[ev$tijd==1])) 
abline(1:300, 1:300)
plot((ev$cap2[ev$tijd==1]) , 2*(ev$s3pc4_afw[ev$tijd==1])) 
cor((ev$duur[ev$tijd==1 & ev$s3pc4_afw>0]), ev$s3pc4_afw[ev$tijd==1 & ev$s3pc4_afw>0])
cor((ev$s3_altop[ev$tijd==1 & ev$s3pc4_afw>0]), ev$s3pc4_afw[ev$tijd==1 & ev$s3pc4_afw>0])
cor((ev$cap[ev$tijd==1 & ev$s3pc4_afw>0]), ev$s3pc4_afw[ev$tijd==1 & ev$s3pc4_afw>0])
plot((ev$s3_altop[ev$tijd==1 & ev$s3pc4_afw>0]), ev$s3pc4[ev$tijd==1 & ev$s3pc4_afw>0])
table((ev$duur[ev$tijd==1]>0), (ev$s3pc4_afw[ev$tijd==1]>0))
table((ev$cap[ev$tijd==1]>0), (ev$s3pc4_afw[ev$tijd==1]>0), useNA="always")

(ev$pc4[ev$tijd==1][(ev$cap[ev$tijd==1]>0) & !(ev$s3pc4_afw[ev$tijd==1]>0)])
 # 6222 6222 9341 # don/t know why. I do see them in our COA data, why not in original manuscript?
(ev$pc4[ev$tijd==1][!(ev$cap[ev$tijd==1]>0) & (ev$s3pc4_afw[ev$tijd==1]>0)])
table((ev$cap[ev$tijd==1]>0) , (ev$s3pc4_afw[ev$tijd==1])) 
 
#replicate results from original manuscript
#only select respondents who changed in voting behaviour
ev_sel <- ev[ev$sel_fe==1,]
(ev_sel$pc4[ev_sel$tijd==1][(ev_sel$cap[ev_sel$tijd==1]>0) & !(ev_sel$s3pc4_afw[ev_sel$tijd==1]>0)])

m1 <- clogit(y_pvv ~ s123pc4 + tijd + strata(PanelistIdQuestion), data=ev_sel)
summary(m1)

#replicate via different estimation of fixed effects model
#only select y_observation at timepoint 2
ev_sel2 <- ev_sel[ev_sel$tijd==1,]
#need to multiply _afw with 2 (thus becomes t2-t1, instead of t2 - mean(t2,t1)
ev_sel2$s123pc4_afw <- 2*ev_sel2$s123pc4_afw
ev_sel2$s1pc4_afw <- 2*ev_sel2$s1pc4_afw
ev_sel2$s2pc4_afw <- 2*ev_sel2$s2pc4_afw
ev_sel2$s3pc4_afw <- 2*ev_sel2$s3pc4_afw
ev_sel2$threat_afw <- 2*ev_sel2$threat_afw
ev_sel2$contactnw_afw <- 2*ev_sel2$contactnw_afw

m1_alt1 <- glm(y_pvv ~ s123pc4_afw , data=ev_sel2, family=binomial)
m1_alt2 <- glm(y_pvv ~ s1pc4_afw + s2pc4_afw + s3pc4_afw , data=ev_sel2, family=binomial)
m1_alt3 <- glm(y_pvv ~ s1pc4_afw + s2pc4_afw + s3pc4_afw + contactnw_afw + threat_afw , data=ev_sel2, family=binomial)
summary(m1_alt1)
summary(m1_alt2)
summary(m1_alt3)

#conclusion: R results exactly replicate stata results

#show that treatment is not related to changes in threat and contact. note 3 in revised manuscript. 
ev_sel2$treatment <- ev_sel2$s123pc4_afw>0
mean(ev_sel2$contactnw_afw)
by(ev_sel2$contactnw_afw, ev_sel2$treatment, function(x) sd(x))
with(ev_sel2, t.test(contactnw_afw~treatment))

#treated group less contact. untreated group more contact. difference is significant. 
mean(ev_sel2$threat_afw)
by(ev_sel2$threat_afw, ev_sel2$treatment, function(x) sd(x))
with(ev_sel2, t.test(threat_afw~treatment))
?t.test

#robustness check with newly matched crisis data. 
#m1_alt1e <- glm(y_pvv ~ s123pc4_afw , data=ev_sel2, family=binomial)
m1_alt2e <- glm(y_pvv ~ s1pc4_afw + s2pc4_afw + cap , data=ev_sel2, family=binomial)
m1_alt3e <- glm(y_pvv ~ s1pc4_afw + s2pc4_afw + cap + contactnw_afw + threat_afw , data=ev_sel2, family=binomial)
#summary(m1_alt1e)
summary(m1_alt2e)
summary(m1_alt3e)
#do we need to report this? 

#robustness check with binarization
ev_sel2$treatment <- ev_sel2$s123pc4_afw>0
ev_sel2$treatments1 <- ev_sel2$s1pc4_afw>0
ev_sel2$treatments2 <- ev_sel2$s2pc4_afw>0
ev_sel2$treatments3 <- ev_sel2$s3pc4_afw>0
m1_alt1b <- glm(y_pvv ~ treatment , data=ev_sel2, family=binomial)
m1_alt2b <- glm(y_pvv ~ treatments1 + treatments2 + treatments3 , data=ev_sel2, family=binomial)
m1_alt3b <- glm(y_pvv ~ treatments1 + treatments2 + treatments3 + contactnw_afw + threat_afw , data=ev_sel2, family=binomial)
summary(m1_alt1b)
summary(m1_alt2b)
summary(m1_alt3b)
#conclusion: not stable

#robustness check with time heterogeneity
#m1_alt1c <- glm(y_pvv ~ s123pc4_afw , data=ev_sel2, family=binomial)
m1_alt2c <- glm(y_pvv ~ s1pc4_afw  + s2pc4_afw  + s3_altop , data=ev_sel2, family=binomial)
m1_alt3c <- glm(y_pvv ~ s1pc4_afw  + s2pc4_afw  + s3_altop + contactnw_afw + threat_afw , data=ev_sel2, family=binomial)
#summary(m1_alt1c)
summary(m1_alt2c)
summary(m1_alt3c)

#robustness check with time only
#m1_alt1c <- glm(y_pvv ~ s123pc4_afw , data=ev_sel2, family=binomial)
m1_alt2c2 <- glm(y_pvv ~ s1pc4_afw  + s2pc4_afw  + duur , data=ev_sel2, family=binomial)
m1_alt3c2 <- glm(y_pvv ~ s1pc4_afw  + s2pc4_afw  + duur + contactnw_afw + threat_afw , data=ev_sel2, family=binomial)
m1_alt4c2 <- glm(y_pvv ~ s1pc4_afw  + s2pc4_afw + s3pc4_afw  + duur + contactnw_afw + threat_afw , data=ev_sel2, family=binomial)

#summary(m1_alt1c)
summary(m1_alt2c2)
summary(m1_alt3c2)
summary(m1_alt4c2)

#robustness check with exposure on municipality level 
ev_sel2$s123gc_afw <- 1000 * (ev_sel2$s1cap_gct2 + ev_sel2$s2cap_gct2 + ev_sel2$s3cap_gct2 - ev_sel2$s1cap_gct1 - ev_sel2$s2cap_gct1 - ev_sel2$s3cap_gct1)/ev_sel2$inw2014_gc
ev_sel2$s1gc_afw <- 1000 * (ev_sel2$s1cap_gct2 - ev_sel2$s1cap_gct1)/ev_sel2$inw2014_gc
ev_sel2$s2gc_afw <- 1000 * (ev_sel2$s2cap_gct2 - ev_sel2$s2cap_gct1)/ev_sel2$inw2014_gc
ev_sel2$s3gc_afw <- 1000 * (ev_sel2$s3cap_gct2 - ev_sel2$s3cap_gct1)/ev_sel2$inw2014_gc


m1_alt1d <- glm(y_pvv ~ s123gc_afw , data=ev_sel2, family=binomial)
m1_alt2d <- glm(y_pvv ~ s1gc_afw  + s2gc_afw  + s3gc_afw , data=ev_sel2, family=binomial)
m1_alt3d <- glm(y_pvv ~ s1gc_afw  + s2gc_afw  + s3gc_afw + contactnw_afw + threat_afw , data=ev_sel2, family=binomial)
summary(m1_alt1d)
summary(m1_alt2d)
summary(m1_alt3d)

#start preprocessing data (matching)
#because we also want to match on ethnic density of NB and SES of NB need to remove missing values.
#table(ev_sel2$pnwal2014_pc4, useNA="always")
ev_sel2 <- ev_sel2[!is.na(ev_sel2$pnwal2014_pc4),]
#lost 3 respondents, no treated
table(ev_sel2$woz2012_pc4, useNA="always")
ev_sel2 <- ev_sel2[!(ev_sel2$woz2012_pc4==0),]
#lost 1 respondents, no treated
#take log of NBses
ev_sel2$LNwoz <- log(ev_sel2$woz2012_pc4)

#define treatment variable: whether there has been an increase in total asylum seekers.  
ev_sel2$treatment <- ev_sel2$s123pc4_afw>0
table(ev_sel2$treatment, useNA="always")
#111 treated

ev_sel2$treatment2 <- ev_sel2$s3pc4_afw>0
table(ev_sel2$treatment2, useNA="always")
#75 treated

#let use smaller dataset (to remove missing values in non relevant vars)
ev_sel2 <- ev_sel2[,c("y_pvv", "treatment","treatment2", "s123pc4_afw", "s1pc4_afw", "s2pc4_afw" , "s3pc4_afw" , "gender", "age", "educ", "contactnw_afw", "threat_afw", "contactnw_t1" , "threat_t1" , "pnwal2014_pc4" , "woz2012_pc4", "LNwoz")]


mean(scale(ev_sel2$age))

m.out <- matchit(treatment ~ gender +  scale(age) + scale(educ) + scale(contactnw_t1) + scale(threat_t1) + scale(pnwal2014_pc4) + scale(LNwoz), data=ev_sel2)
summary(m.out)
data_matched <- match.data(m.out)

#quickly see effect of matching
prop.table(table(ev_sel2$treatment, ev_sel2$y_pvv), margin=1)
prop.table(table(data_matched$treatment, data_matched$y_pvv), margin=1)
#conclusion?: treatment took place in areas less likely to see increase in support for PVV

m1_alt_matched1 <- glm(y_pvv ~ s123pc4_afw , data=data_matched, family=binomial)
m1_alt_matched2 <- glm(y_pvv ~ s1pc4_afw + s2pc4_afw + s3pc4_afw , data=data_matched, family=binomial)
m1_alt_matched3 <- glm(y_pvv ~ s1pc4_afw + s2pc4_afw + s3pc4_afw + contactnw_afw + threat_afw, data=data_matched, family=binomial)

summary(m1_alt_matched1)
summary(m1_alt_matched2)
summary(m1_alt_matched3)

m.out2 <- matchit(treatment2 ~ scale(gender) +  scale(age) + scale(educ) + scale(contactnw_t1) + scale(threat_t1) + scale(pnwal2014_pc4) + scale(LNwoz), data=ev_sel2)
summary(m.out2)
data_matched2 <- match.data(m.out2)

#quickly see effect of matching
prop.table(table(ev_sel2$treatment2, ev_sel2$y_pvv), margin=1)
prop.table(table(data_matched2$treatment2, data_matched2$y_pvv), margin=1)
#conclusion?: treatment took place in areas less likely to see increase in support for PVV

#m1_alt_matched1b <- glm(y_pvv ~ s123pc4_afw , data=data_matched2, family=binomial)
m1_alt_matched2b <- glm(y_pvv ~ s3pc4_afw , data=data_matched2, family=binomial)
m1_alt_matched3b <- glm(y_pvv ~ s3pc4_afw + contactnw_afw + threat_afw, data=data_matched2, family=binomial)

#summary(m1_alt_matched1b)
summary(m1_alt_matched2b)
summary(m1_alt_matched3b)
#conclusion: matching makes effect stronger




##hybrid model 
#because we include ethnic density of NB and SES of NB need to remove missing values.
#table(ev_sel2$pnwal2014_pc4, useNA="always")
table(ev$pnwal2014_pc4, useNA="always")
ev_sel4 <- ev[!is.na(ev$pnwal2014_pc4),]
#lost 90 respondents
table(ev_sel4$woz2012_pc4, useNA="always")
ev_sel4 <- ev_sel4[!(ev_sel4$woz2012_pc4==0),]
#lost 22 respondents
ev_sel4 <- ev_sel4[!is.na(ev_sel4$woz2012_pc4),]
#lost 3 respondents

#center variables! 
ev_sel4$age_c <- ev_sel4$age - mean(ev_sel4$age)
ev_sel4$educ_c <- ev_sel4$educ - mean(ev_sel4$educ)
ev_sel4$pnwal2014_pc4_c <- ev_sel4$pnwal2014_pc4 - mean(ev_sel4$pnwal2014_pc4)
ev_sel4$woz2012_pc4_c <- ev_sel4$woz2012_pc4 - mean(ev_sel4$woz2012_pc4)

#replicate stata findings
m1h <- glm(y_pvv ~ tijd + s123pc4_afw + s123pc4_mean + gender + age_c + educ_c + pnwal2014_pc4_c + woz2012_pc4_c, data=ev_sel4, family=binomial)
m2h <- glm(y_pvv ~ tijd + s1pc4_afw + s2pc4_afw + s3pc4_afw + s1pc4_mean + s2pc4_mean + s3pc4_mean + gender + age + educ + pnwal2014_pc4 + woz2012_pc4, data=ev_sel4, family=binomial)
m3h <- glm(y_pvv ~ tijd + s1pc4_afw + s2pc4_afw + s3pc4_afw + s1pc4_mean + s2pc4_mean + s3pc4_mean + threat_mean + contactnw_mean + gender + age + educ + pnwal2014_pc4 + woz2012_pc4, data=ev_sel4, family=binomial)
summary(m1h)
summary(m2h)
summary(m3h)

coeftest(m1h, vcov = vcovCL(m1h, cluster = ~ PanelistIdQuestion))
coeftest(m2h, vcov = vcovCL(m2h, cluster = ~ PanelistIdQuestion))
coeftest(m3h, vcov = vcovCL(m3h, cluster = ~ PanelistIdQuestion))

#calculate some predicited probabilities
coef(test)
#women before and after crisis, everything at mean, no treatment
plogis(coef(m1h) %*% c(1,0,0,0,0,0,0,0,0))
plogis(coef(m1h) %*% c(1,1,0,0,0,0,0,0,0))
plogis(coef(m1h) %*% c(1,1,0,0,0,0,0,0,0)) - plogis(coef(m1h) %*% c(1,0,0,0,0,0,0,0,0))

# women after crisis, everything at mean yes/no treatment=100
plogis(coef(m1h) %*% c(1,1,0,0,0,0,0,0,0))
plogis(coef(m1h) %*% c(1,1,100,0,0,0,0,0,0))
plogis(coef(m1h) %*% c(1,1,100,0,0,0,0,0,0)) - plogis(coef(m1h) %*% c(1,1,0,0,0,0,0,0,0))

#men before and after crisis, everything at mean, no treatment
plogis(coef(m1h) %*% c(1,0,0,0,1,0,0,0,0))
plogis(coef(m1h) %*% c(1,1,0,0,1,0,0,0,0))
plogis(coef(m1h) %*% c(1,1,0,0,1,0,0,0,0)) - plogis(coef(m1h) %*% c(1,0,0,0,1,0,0,0,0))

#men after crisis, everything at mean yes/no treatment=100
plogis(coef(m1h) %*% c(1,1,0,0,1,0,0,0,0))
plogis(coef(m1h) %*% c(1,1,100,0,1,0,0,0,0))
plogis(coef(m1h) %*% c(1,1,100,0,1,0,0,0,0)) - plogis(coef(m1h) %*% c(1,1,0,0,1,0,0,0,0))

summary(ev_sel4$s123pc4_afw)
coef(m1h)

#robustness, effects of control per time period
m1hb <- glm(y_pvv ~ tijd + s123pc4_afw + s123pc4_mean + tijd*gender + tijd*age_c + tijd*educ_c + tijd*pnwal2014_pc4_c + tijd*woz2012_pc4_c, data=ev_sel4, family=binomial)
coeftest(m1hb, vcov = vcovCL(m1hb, cluster = ~ PanelistIdQuestion))
#only with ethnic density
m1hc <- glm(y_pvv ~ tijd + s123pc4_afw + s123pc4_mean + gender + age_c + educ_c + tijd*pnwal2014_pc4_c + woz2012_pc4_c, data=ev_sel4, family=binomial)
coeftest(m1hc, vcov = vcovCL(m1hc, cluster = ~ PanelistIdQuestion))
#conclusion: the increase in support for pvv was larger in areas with more minorities, but the impact of minorities decreased over time. This does not impact the estimate of exposure

#robustness, conditional effects of exposure with ethnic density
m1hd <- glm(y_pvv ~ tijd + s123pc4_afw + s123pc4_mean + gender + age_c + educ_c + s123pc4_afw*pnwal2014_pc4_c + woz2012_pc4_c, data=ev_sel4, family=binomial)
coeftest(m1hd, vcov = vcovCL(m1hd, cluster = ~ PanelistIdQuestion))
#conclusion: not significant


test <- glm(y_pvv ~ tijd , data=ev_sel4, family=binomial)
coef(test)
plogis(coef(test) %*% c(1,0))
plogis(coef(test) %*% c(1,1))

prop.table(table(ev_sel4$y_pvv, ev_sel4$tijd), margin=2)

require(lme4)
glmerm1 <- glmer(PVV ~ tijd + s123pc4_afw + s123pc4_mean + gender + age + educ + pnwal2014_pc4 + woz2012_pc4 + (1 | PanelistIdQuestion), data=ev_sel4, family=binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)


