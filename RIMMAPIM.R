---
title: "Random Intercept micro-macro APIM using lavaan (R)"
output: 
  html_document:
    toc:  true
    toc_float: true
    number_sections: true
    code_folding: show
    code_download: yes
bibliography: references.bib
---
  

<!---set global settings--> 
  
```{r, globalsettings, echo=FALSE, warning=FALSE}
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=100),tidy=TRUE, warning = FALSE, message = FALSE,comment = "#>", cache=TRUE)
options(width = 100)
rgl::setupKnitr()


colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, 
      x)
  } else x
}

```

<!---allow to copy paste to clipboard-->
  
  
```{r klippy, echo=FALSE, include=TRUE}
klippy::klippy(position = c('top', 'right'))
#klippy::klippy(color = 'darkred')
#klippy::klippy(tooltip_message = 'Click to copy', tooltip_success = 'Done')
```


## background reading.

For introduction to RI-CLPM see: [@hamaker2015critique]
<br>
```{r, echo=FALSE}
xfun::embed_file('data/Hamaker2015.pdf')
```
<br> 

For extensions of the RI-CLPM see: [@Mulder2020]
<br>
```{r, echo=FALSE}
xfun::embed_file('data/Mulder2020.pdf')
```
<br>  

 





## Setup
Below you can find the code for installing and loading the required package `lavaan` [@lavaan], as well as for reading in the data for the Random Intercept Cross-Lagged Panel Model (RI-CLPM) and its 3 extensions. You can specify the path to the data yourself, or through a menu by using the `file.choose()`-function. You can download the simulated example datasets [here](https://github.com/ellenhamaker/RI-CLPM/tree/master/data). 

Download the liss dataset here: 

<br>
```{r, echo=FALSE}
xfun::embed_file('data/lisscd_riclpm_testdata.Rdata')
```
<br>  



```{r setup, eval = T, message = F}
# If necessary, install the 'Lavaan' package. 
# install.packages('lavaan', dependencies = T)
# Load the required packages. 

rm(list=ls())

require(lavaan) 

# Load in the data. 
## Traditional RI-CLPM
dat <- read.table("data/RICLPM.dat", 
                  col.names = c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5")) 
## Extension 1
datZ <- read.table("data/RICLPM-Z.dat",
                   col.names = c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5", "z2", "z1"))
## Extension 2
datMG <- read.table("data/RICLPM-MG.dat",
                    col.names = c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5", "G")) 
## Extension 3
datMI <- read.table("data/RICLPM-MI.dat",
                   col.names = c("x11", "x12", "x13",
                                 "x21", "x22", "x23",
                                 "x31", "x32", "x33",
                                 "x41", "x42", "x43",
                                 "x51", "x52", "x53", 
                                
                                 "y11", "y12", "y13",
                                 "y21", "y22", "y23",
                                 "y31", "y32", "y33", 
                                 "y41", "y42", "y43", 
                                 "y51", "y52", "y53"))

load('data/lisscd_riclpm_testdata.Rdata')
# Een _ gevolgd door een cijfer geeft de survey wave aan. Een . gevolgd door een cijfer geeft de alter aan. Dus educalter_1.4 in de wide file betekent de opleiding van alter 4 in surveywave 1. Waarom deze volgorde en niet andersom? Nu kan je makkelijk van wide naar long omzetten en viceversa.  

#data liss wide
datalw <- lisscdn_mlsem_wide

#select only respondents with no missing on dependent variable in the last 5 waves. 
datalw <- datalw[complete.cases(cbind(datalw$eu_7,datalw$eu_8,datalw$eu_9,datalw$eu_10,datalw$eu_11)),]
  
```

## Goals

* compare standard two-wave APIM with CLPM. 
  - outcome: same model
* compare five-wave APIM/CLPM with two-wave APIM/CLPM  
  - outcome: cross-lagged effect of educalter ~ eu is now stronger than eu ~ educalter!
* compare five-wave CLPM with five-wave RI-CLPM  
  - outcome: cross-lagged effect of eu ~ educalter is no longer significant in RI-CLPM!
* compare five-wave CLPM with one indicator with five-wave CLPM with multiple indicators  (one model and two-step)  
  - when we use multiple indicators in one step we find the results we expect to see.  
  - In two-step approach cross-lagged paths are similar in strength.  
  - I am unsure about the preferred approach. My guess is that the two-step approach is closer to the bias corrected means idea of the micro-macro model. 
* compare five-wave CLPM with multiple indicator with RI-CLPM with multiple indicators.
  - comparison of two-step approach:  
  - comparison of one-step approach


## calculate the (bias corrected) means for use later. {.tabset .tabset-fade}

To estimate the bias corrected means over time. Do we need/want to add the covariances between the latent factors?? 
We also assume local independence; the educational level of the confidants are independent given the 'mean' educational level of the CDN. Following a SNA perspective this is actually quite unlikely. 

### mulder hamaker data

```{r}
myModel <- '  
  #####################
  # MEASUREMENT MODEL #
  #####################
  
  # Factor models for x at 5 waves (constrained). 
  FX1 =~ a*x11 + b*x12 + c*x13
  FX2 =~ a*x21 + b*x22 + c*x23
  FX3 =~ a*x31 + b*x32 + c*x33
  FX4 =~ a*x41 + b*x42 + c*x43
  FX5 =~ a*x51 + b*x52 + c*x53
  
  # Factor models for y at 5 waves (constrained).
  FY1 =~ d*y11 + e*y12 + f*y13
  FY2 =~ d*y21 + e*y22 + f*y23
  FY3 =~ d*y31 + e*y32 + f*y33
  FY4 =~ d*y41 + e*y42 + f*y43
  FY5 =~ d*y51 + e*y52 + f*y53
  
  # Constrained intercepts over time (this is necessary for strong factorial 
  # invariance; without these contraints we have week factorial invariance). 
  x11 + x21 + x31 + x41 + x51 ~ g*1
  x12 + x22 + x32 + x42 + x52 ~ h*1
  x13 + x23 + x33 + x43 + x53 ~ i*1
  y11 + y21 + y31 + y41 + y51 ~ j*1
  y12 + y22 + y32 + y42 + y52 ~ k*1
  y13 + y23 + y33 + y43 + y53 ~ l*1
  
  # Free latent means from t = 2 onward (only do this in combination with the 
  # constraints on the intercepts; without these, this would not be specified).
  # I DONT UNDERSTAND THIS. IF YOU HAVE CONSTRAINED INTERCEPTS AND CONSTRAINED FACTOR LOADINGS YOU WILL GET SIMILAR MEANS??!!
  # NO THIS IS NOT HOW IT WORKS IN SEM. IF YOU INCLUDE INTERCEPTS ONLY DEVIATIONS FROM THIS INTERCEPT PREDICT THE FACTOR LOADINGS. 
  # YOU ALSO SEE THIS IN THE RESULTS. INCLUDING INTERCEPTS LEADS TO SIMILAR MEANS ACROSS FX. CONSTRAINING ALLOWS FOR CHANGING MEANS. 
  FX2 + FX3 + FX4 + FX5 + FY2 + FY3 + FY4 + FY5 ~ 1
  
  
'



RICLPM5.ext3.fit <- cfa(myModel, data = datMI, missing = 'ML')

  # WHY WOULD WE WANT TO INCLUDE ALL VARIANCES AND COVARIANCES BETWEEN FACTORS?? 
summary(RICLPM5.ext3.fit, standardized = T)
```

```{r}
# 2. extract the predicted values of the cfa and add them to new dataframe data_wide2
datMIb <- data.frame(datMI, predict(RICLPM5.ext3.fit))

```


### liss data


**aggregated means**
```{r}
datalw$Meducalter_7 <- rowMeans(cbind(datalw$educalter_7.1,datalw$educalter_7.2,datalw$educalter_7.3,datalw$educalter_7.4,datalw$educalter_7.5), na.rm=T)

summary(datalw$Meducalter_7)

datalw$Meducalter_8 <- rowMeans(cbind(datalw$educalter_8.1,datalw$educalter_8.2,datalw$educalter_8.3,datalw$educalter_8.4,datalw$educalter_8.5), na.rm=T)

datalw$Meducalter_9 <- rowMeans(cbind(datalw$educalter_9.1,datalw$educalter_9.2,datalw$educalter_9.3,datalw$educalter_9.4,datalw$educalter_9.5), na.rm=T)

datalw$Meducalter_10 <- rowMeans(cbind(datalw$educalter_10.1,datalw$educalter_10.2,datalw$educalter_10.3,datalw$educalter_10.4,datalw$educalter_10.5), na.rm=T)

datalw$Meducalter_11 <- rowMeans(cbind(datalw$educalter_11.1,datalw$educalter_11.2,datalw$educalter_11.3,datalw$educalter_11.4,datalw$educalter_11.5), na.rm=T)


```


**bias corrected means**

* alters are identical thus equal loadings, variances and intercepts at each time point.  
* Free latent means from t = 2 onward, to allow time trends.  
* be aware that respondents with no confidants in a specific wave will get a score in the below approach. this means way less missings!! HOW DID THIJMEN DEAL WITH THIS??

```{r}
myModel <- '

  #alters are identical thus equal loadings, variances and intercepts at each time point. 
  
  #constrained loadings
  Feducalter_7 =~ 1*educalter_7.1 + 1*educalter_7.2 + 1*educalter_7.3 +  1*educalter_7.4 + 1*educalter_7.5
  Feducalter_8 =~ 1*educalter_8.1 + 1*educalter_8.2 + 1*educalter_8.3 +  1*educalter_8.4 + 1*educalter_8.5
  Feducalter_9 =~ 1*educalter_9.1 + 1*educalter_9.2 + 1*educalter_9.3 +  1*educalter_9.4 + 1*educalter_9.5
  Feducalter_10 =~ 1*educalter_10.1 + 1*educalter_10.2 + 1*educalter_10.3 +  1*educalter_10.4 + 1*educalter_10.5
  Feducalter_11 =~ 1*educalter_11.1 + 1*educalter_11.2 + 1*educalter_11.3 +  1*educalter_11.4 + 1*educalter_11.5
  
  Feducalter_7 ~~ Feducalter_7
  Feducalter_8 ~~ Feducalter_8
  Feducalter_9 ~~ Feducalter_9
  Feducalter_10 ~~ Feducalter_10
  Feducalter_11 ~~ Feducalter_11
  
  #constrained variances
  educalter_7.1 ~~ e*educalter_7.1
  educalter_7.2 ~~ e*educalter_7.2
  educalter_7.3 ~~ e*educalter_7.3
  educalter_7.4 ~~ e*educalter_7.4
  educalter_7.5 ~~ e*educalter_7.5
  
  educalter_8.1 ~~ f*educalter_8.1
  educalter_8.2 ~~ f*educalter_8.2
  educalter_8.3 ~~ f*educalter_8.3
  educalter_8.4 ~~ f*educalter_8.4
  educalter_8.5 ~~ f*educalter_8.5
  
  educalter_9.1 ~~ g*educalter_9.1
  educalter_9.2 ~~ g*educalter_9.2
  educalter_9.3 ~~ g*educalter_9.3
  educalter_9.4 ~~ g*educalter_9.4
  educalter_9.5 ~~ g*educalter_9.5
  
  educalter_10.1 ~~ h*educalter_10.1
  educalter_10.2 ~~ h*educalter_10.2
  educalter_10.3 ~~ h*educalter_10.3
  educalter_10.4 ~~ h*educalter_10.4
  educalter_10.5 ~~ h*educalter_10.5
  
  educalter_11.1 ~~ i*educalter_11.1
  educalter_11.2 ~~ i*educalter_11.2
  educalter_11.3 ~~ i*educalter_11.3
  educalter_11.4 ~~ i*educalter_11.4
  educalter_11.5 ~~ i*educalter_11.5
  
  #constrained intercepts / we want structural changes to be picked up by the factor
  educalter_7.1 ~ j*1
  educalter_7.2 ~ j*1
  educalter_7.3 ~ j*1
  educalter_7.4 ~ j*1
  educalter_7.5 ~ j*1
  
  educalter_8.1 ~ j*1
  educalter_8.2 ~ j*1
  educalter_8.3 ~ j*1
  educalter_8.4 ~ j*1
  educalter_8.5 ~ j*1
  
  educalter_9.1 ~ j*1
  educalter_9.2 ~ j*1
  educalter_9.3 ~ j*1
  educalter_9.4 ~ j*1
  educalter_9.5 ~ j*1
  
  educalter_10.1 ~ j*1
  educalter_10.2 ~ j*1
  educalter_10.3 ~ j*1
  educalter_10.4 ~ j*1
  educalter_10.5 ~ j*1
  
  educalter_11.1 ~ j*1
  educalter_11.2 ~ j*1
  educalter_11.3 ~ j*1
  educalter_11.4 ~ j*1
  educalter_11.5 ~ j*1
  
  # Free latent means from t = 2 onward. WHAT DID THIJMEN DO IN HIS STUDY??
  Feducalter_8 ~ 1
  Feducalter_9 ~ 1
  Feducalter_10 ~ 1
  Feducalter_11 ~ 1
  
  
  '
  
fit <- lavaan(myModel, data = datalw, missing = 'ML', fixed.x=FALSE, meanstructure = T)
summary(fit, standardized = T)

```

```{r}
# 2. extract the predicted values of the cfa and add them to new dataframe data_wide2
datalwb <- data.frame(datalw, predict(fit))

```

some descriptives
```{r}
summary(datalwb[,c(79:88)])
cor(datalwb[,c(79:88)], use="pairwise.complete.obs")
```

---


## Two wave APIM/CLPM {.tabset .tabset-fade}

Let us start with some descriptives.
```{r, results='hold'}

summary(datalw$eu_10)
summary(datalw$eu_11)
summary(datalw$educalter_10.1)
summary(datalw$educalter_11.1)

var(cbind(datalw$eu_10,datalw$eu_11,datalw$educalter_10.1,datalw$educalter_11.1 ), na.rm=F, use="pairwise.complete.obs")
```

### APIM

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '
  # Estimate the lagged effects
  eu_11 + educalter_11.1  ~ eu_10 + educalter_10.1
  
  # Estimate the covariance at the first wave. 
  eu_10 ~~ educalter_10.1 # Covariance

  # Estimate the covariances between the residuals
  eu_11 ~~ educalter_11.1
  
  # Estimate the variance 
  eu_10 ~~ eu_10 
  educalter_10.1 ~~ educalter_10.1
  
  # Estimate the residual variance
  eu_11 ~~ eu_11 
  educalter_11.1 ~~ educalter_11.1
  
  # intercepts
  eu_10 ~ 1
  eu_11 ~ 1
  educalter_10.1 ~ 1
  educalter_11.1 ~ 1
'



fit <- lavaan(myModel, data = datalw, missing = 'ML', meanstructure = T)
summary(fit, standardized = T)

```

--- 


### CLPM

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '
  # Create within-person centered variables
  weducalter_10.1 =~ 1*educalter_10.1
  weducalter_11.1 =~ 1*educalter_11.1
  weu_10 =~ 1*eu_10
  weu_11 =~ 1*eu_11 
  
  # Estimate the lagged effects
  weu_11 + weducalter_11.1  ~ weu_10 + weducalter_10.1
  
  # Estimate the covariance at the first wave. 
  weu_10 ~~ weducalter_10.1 # Covariance

  # Estimate the covariances between the residuals
  weu_11 ~~ weducalter_11.1
  
  # Estimate the variance 
  weu_10 ~~ weu_10 
  weducalter_10.1 ~~ weducalter_10.1
  
  # Estimate the residual variance
  weu_11 ~~ weu_11 
  weducalter_11.1 ~~ weducalter_11.1
 
'

fit <- lavaan(myModel, data = datalw, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```
--- 

## five wave APIM/CLPM {.tabset .tabset-fade}

### five wave APIM

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '
  # Estimate the lagged effects (constrained)
  eu_8   ~ a*eu_7 + b*educalter_7.1
  eu_9   ~ a*eu_8 + b*educalter_8.1
  eu_10  ~ a*eu_9 + b*educalter_9.1
  eu_11  ~ a*eu_10 + b*educalter_10.1
  
  educalter_8.1  ~ c*eu_7 + d*educalter_7.1
  educalter_9.1  ~ c*eu_8 + d*educalter_8.1
  educalter_10.1  ~ c*eu_9 + d*educalter_9.1
  educalter_11.1  ~ c*eu_10 + d*educalter_10.1
  
  # Estimate the covariance at the first wave. 
  eu_7 ~~ educalter_7.1 # Covariance

  # Estimate the covariances between the residuals
  eu_8 ~~ educalter_8.1
  eu_9 ~~ educalter_9.1
  eu_10 ~~ educalter_10.1
  eu_11 ~~ educalter_11.1
  
  # Estimate the variance 
  eu_7 ~~ eu_7 
  educalter_7.1 ~~ educalter_7.1
  
  # Estimate the residual variance
  eu_8 ~~ eu_8 
  educalter_8.1 ~~ educalter_8.1
  eu_9 ~~ eu_9 
  educalter_9.1 ~~ educalter_9.1
  eu_10 ~~ eu_10 
  educalter_10.1 ~~ educalter_10.1
  eu_11 ~~ eu_11 
  educalter_11.1 ~~ educalter_11.1
  
  # intercepts
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
  educalter_7.1 ~ 1
  educalter_8.1 ~ 1
  educalter_9.1 ~ 1
  educalter_10.1 ~ 1
  educalter_11.1 ~ 1
'



fit <- lavaan(myModel, data = datalw, missing = 'ML')
summary(fit, standardized = T)


```

### five wave CLPM

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '
  # Estimate the lagged effects (constrained)
  eu_8   ~ a*eu_7 + b*educalter_7.1
  eu_9   ~ a*eu_8 + b*educalter_8.1
  eu_10  ~ a*eu_9 + b*educalter_9.1
  eu_11  ~ a*eu_10 + b*educalter_10.1
  
  educalter_8.1  ~ c*eu_7 + d*educalter_7.1
  educalter_9.1  ~ c*eu_8 + d*educalter_8.1
  educalter_10.1  ~ c*eu_9 + d*educalter_9.1
  educalter_11.1  ~ c*eu_10 + d*educalter_10.1
  
  # Estimate the covariance at the first wave. 
  eu_7 ~~ educalter_7.1 # Covariance

  # Estimate the covariances between the residuals
  eu_8 ~~ educalter_8.1
  eu_9 ~~ educalter_9.1
  eu_10 ~~ educalter_10.1
  eu_11 ~~ educalter_11.1
  
  # Estimate the variance 
  eu_7 ~~ eu_7 
  educalter_7.1 ~~ educalter_7.1
  
  # Estimate the residual variance
  eu_8 ~~ eu_8 
  educalter_8.1 ~~ educalter_8.1
  eu_9 ~~ eu_9 
  educalter_9.1 ~~ educalter_9.1
  eu_10 ~~ eu_10 
  educalter_10.1 ~~ educalter_10.1
  eu_11 ~~ eu_11 
  educalter_11.1 ~~ educalter_11.1
  
  # intercepts
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
  
  educalter_7.1 ~ 1
   educalter_8.1 ~ 1
    educalter_9.1 ~ 1
     educalter_10.1 ~ 1
      educalter_11.1 ~ 1
  
'



fit <- lavaan(myModel, data = datalw, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```

--- 

## five wave CLPM vs RI-CLPM {.tabset .tabset-fade}

### five wave CLPM
```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '
  # Estimate the lagged effects (constrained)
  eu_8   ~ a*eu_7 + b*educalter_7.1
  eu_9   ~ a*eu_8 + b*educalter_8.1
  eu_10  ~ a*eu_9 + b*educalter_9.1
  eu_11  ~ a*eu_10 + b*educalter_10.1
  
  educalter_8.1  ~ c*eu_7 + d*educalter_7.1
  educalter_9.1  ~ c*eu_8 + d*educalter_8.1
  educalter_10.1  ~ c*eu_9 + d*educalter_9.1
  educalter_11.1  ~ c*eu_10 + d*educalter_10.1
  
  # Estimate the covariance at the first wave. 
  eu_7 ~~ educalter_7.1 # Covariance

  # Estimate the covariances between the residuals
  eu_8 ~~ educalter_8.1
  eu_9 ~~ educalter_9.1
  eu_10 ~~ educalter_10.1
  eu_11 ~~ educalter_11.1
  
  # Estimate the variance 
  eu_7 ~~ eu_7 
  educalter_7.1 ~~ educalter_7.1
  
  # Estimate the residual variance
  eu_8 ~~ eu_8 
  educalter_8.1 ~~ educalter_8.1
  eu_9 ~~ eu_9 
  educalter_9.1 ~~ educalter_9.1
  eu_10 ~~ eu_10 
  educalter_10.1 ~~ educalter_10.1
  eu_11 ~~ eu_11 
  educalter_11.1 ~~ educalter_11.1
  
  # intercepts
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
  
  educalter_7.1 ~ 1
   educalter_8.1 ~ 1
    educalter_9.1 ~ 1
     educalter_10.1 ~ 1
      educalter_11.1 ~ 1
  
'



fit <- lavaan(myModel, data = datalw, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```
--- 

### five wave RI-CLPM

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '
 # Create between components (random intercepts)
  RIx =~ 1*educalter_7.1 + 1*educalter_8.1 + 1*educalter_9.1 + 1*educalter_10.1 + 1*educalter_11.1
  RIy =~ 1*eu_7 + 1*eu_8 + 1*eu_9 + 1*eu_10 + 1*eu_11


   # Create within-person centered variables
  weducalter_7.1 =~ 1*educalter_7.1
  weducalter_8.1 =~ 1*educalter_8.1
  weducalter_9.1 =~ 1*educalter_9.1
  weducalter_10.1 =~ 1*educalter_10.1
  weducalter_11.1 =~ 1*educalter_11.1
  weu_7 =~ 1*eu_7
  weu_8 =~ 1*eu_8
  weu_9 =~ 1*eu_9
  weu_10 =~ 1*eu_10
  weu_11 =~ 1*eu_11 
  
  # Estimate the lagged effects (constrained)
  weu_8   ~ a*weu_7 + b*weducalter_7.1
  weu_9   ~ a*weu_8 + b*weducalter_8.1
  weu_10  ~ a*weu_9 + b*weducalter_9.1
  weu_11  ~ a*weu_10 + b*weducalter_10.1
  
  weducalter_8.1  ~ c*weu_7 + d*weducalter_7.1
  weducalter_9.1  ~ c*weu_8 + d*weducalter_8.1
  weducalter_10.1  ~ c*weu_9 + d*weducalter_9.1
  weducalter_11.1  ~ c*weu_10 + d*weducalter_10.1
  
  # Estimate the covariance at the first wave. WITHIN
  weu_7 ~~ weducalter_7.1 # Covariance

  # Estimate the covariances between the residuals. WITHIN
  weu_8 ~~ weducalter_8.1
  weu_9 ~~ weducalter_9.1
  weu_10 ~~ weducalter_10.1
  weu_11 ~~ weducalter_11.1
  
   # Estimate the variance and covariance of the random intercepts. BETWEEN
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  # Estimate the variance. WITHIN
  weu_7 ~~ weu_7 
  weducalter_7.1 ~~ weducalter_7.1
  
  # Estimate the residual variance. WITHIN
  weu_8 ~~ weu_8 
  weducalter_8.1 ~~ weducalter_8.1
  weu_9 ~~ weu_9 
  weducalter_9.1 ~~ weducalter_9.1
  weu_10 ~~ weu_10 
  weducalter_10.1 ~~ weducalter_10.1
  weu_11 ~~ weu_11 
  weducalter_11.1 ~~ weducalter_11.1
  
'



fit <- lavaan(myModel, data = datalw, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```

--- 

## five wave CLPM one indicator vs CLPM indicators five indactors (one step and two-step) vs aggregation method {.tabset .tabset-fade}

### five wave CLPM one indicator

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '
  # Estimate the lagged effects (constrained)
  eu_8   ~ a*eu_7 + b*educalter_7.1
  eu_9   ~ a*eu_8 + b*educalter_8.1
  eu_10  ~ a*eu_9 + b*educalter_9.1
  eu_11  ~ a*eu_10 + b*educalter_10.1
  
  educalter_8.1  ~ c*eu_7 + d*educalter_7.1
  educalter_9.1  ~ c*eu_8 + d*educalter_8.1
  educalter_10.1  ~ c*eu_9 + d*educalter_9.1
  educalter_11.1  ~ c*eu_10 + d*educalter_10.1
  
  # Estimate the covariance at the first wave. 
  eu_7 ~~ educalter_7.1 # Covariance

  # Estimate the covariances between the residuals
  eu_8 ~~ educalter_8.1
  eu_9 ~~ educalter_9.1
  eu_10 ~~ educalter_10.1
  eu_11 ~~ educalter_11.1
  
  # Estimate the variance 
  eu_7 ~~ eu_7 
  educalter_7.1 ~~ educalter_7.1
  
  # Estimate the residual variance
  eu_8 ~~ eu_8 
  educalter_8.1 ~~ educalter_8.1
  eu_9 ~~ eu_9 
  educalter_9.1 ~~ educalter_9.1
  eu_10 ~~ eu_10 
  educalter_10.1 ~~ educalter_10.1
  eu_11 ~~ eu_11 
  educalter_11.1 ~~ educalter_11.1
  
  # intercepts
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
  
  educalter_7.1 ~ 1
   educalter_8.1 ~ 1
    educalter_9.1 ~ 1
     educalter_10.1 ~ 1
      educalter_11.1 ~ 1
  
'



fit <- lavaan(myModel, data = datalw, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```
--- 

### five wave CLPM five indicators one-step

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '
   # Create LATENT variables
  Feducalter_7 =~ 1*educalter_7.1 + 1*educalter_7.2 + 1*educalter_7.3 +  1*educalter_7.4 + 1*educalter_7.5
  Feducalter_8 =~ 1*educalter_8.1 + 1*educalter_8.2 + 1*educalter_8.3 +  1*educalter_8.4 + 1*educalter_8.5
  Feducalter_9 =~ 1*educalter_9.1 + 1*educalter_9.2 + 1*educalter_9.3 +  1*educalter_9.4 + 1*educalter_9.5
  Feducalter_10 =~ 1*educalter_10.1 + 1*educalter_10.2 + 1*educalter_10.3 +  1*educalter_10.4 + 1*educalter_10.5
  Feducalter_11 =~ 1*educalter_11.1 + 1*educalter_11.2 + 1*educalter_11.3 +  1*educalter_11.4 + 1*educalter_11.5
  
  # Free latent means from t = 2 onward. WHAT DID THIJMEN DO IN HIS STUDY?? aND DOES THIS MATTER? 
  Feducalter_8 ~ 1
  Feducalter_9 ~ 1
  Feducalter_10 ~ 1
  Feducalter_11 ~ 1
  
  # Estimate the lagged effects (constrained)
  eu_8   ~ a*eu_7 + b*Feducalter_7
  eu_9   ~ a*eu_8 + b*Feducalter_8
  eu_10  ~ a*eu_9 + b*Feducalter_9
  eu_11  ~ a*eu_10 + b*Feducalter_10
  
  Feducalter_8  ~ c*eu_7 + d*Feducalter_7
  Feducalter_9  ~ c*eu_8 + d*Feducalter_8
  Feducalter_10  ~ c*eu_9 + d*Feducalter_9
  Feducalter_11  ~ c*eu_10 + d*Feducalter_10
  
  # Estimate the covariance at the first wave. 
  eu_7 ~~ Feducalter_7 # Covariance

  # Estimate the covariances between the residuals
  eu_8 ~~ Feducalter_8
  eu_9 ~~ Feducalter_9
  eu_10 ~~ Feducalter_10
  eu_11 ~~ Feducalter_11
  
  # Estimate the variance 
   
  educalter_7.1 ~~ e*educalter_7.1
  educalter_7.2 ~~ e*educalter_7.2
  educalter_7.3 ~~ e*educalter_7.3
  educalter_7.4 ~~ e*educalter_7.4
  educalter_7.5 ~~ e*educalter_7.5
  
  educalter_8.1 ~~ f*educalter_8.1
  educalter_8.2 ~~ f*educalter_8.2
  educalter_8.3 ~~ f*educalter_8.3
  educalter_8.4 ~~ f*educalter_8.4
  educalter_8.5 ~~ f*educalter_8.5
  
  educalter_9.1 ~~ g*educalter_9.1
  educalter_9.2 ~~ g*educalter_9.2
  educalter_9.3 ~~ g*educalter_9.3
  educalter_9.4 ~~ g*educalter_9.4
  educalter_9.5 ~~ g*educalter_9.5
  
  educalter_10.1 ~~ h*educalter_10.1
  educalter_10.2 ~~ h*educalter_10.2
  educalter_10.3 ~~ h*educalter_10.3
  educalter_10.4 ~~ h*educalter_10.4
  educalter_10.5 ~~ h*educalter_10.5
  
  educalter_11.1 ~~ i*educalter_11.1
  educalter_11.2 ~~ i*educalter_11.2
  educalter_11.3 ~~ i*educalter_11.3
  educalter_11.4 ~~ i*educalter_11.4
  educalter_11.5 ~~ i*educalter_11.5
  
  
  
  
  # Estimate the (residual) variance
  eu_7 ~~ eu_7
  eu_8 ~~ eu_8 
  eu_9 ~~ eu_9 
  eu_10 ~~ eu_10 
  eu_11 ~~ eu_11 
  
  Feducalter_7 ~~ Feducalter_7
  Feducalter_8 ~~ Feducalter_8
  Feducalter_9 ~~ Feducalter_9
  Feducalter_10 ~~ Feducalter_10
  Feducalter_11 ~~ Feducalter_11
  
  
  # intercepts
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
  
  #constrained intercepts
  educalter_7.1 ~ j*1
  educalter_7.2 ~ j*1
  educalter_7.3 ~ j*1
  educalter_7.4 ~ j*1
  educalter_7.5 ~ j*1
  
  educalter_8.1 ~ j*1
  educalter_8.2 ~ j*1
  educalter_8.3 ~ j*1
  educalter_8.4 ~ j*1
  educalter_8.5 ~ j*1
  
  educalter_9.1 ~ j*1
  educalter_9.2 ~ j*1
  educalter_9.3 ~ j*1
  educalter_9.4 ~ j*1
  educalter_9.5 ~ j*1
  
  educalter_10.1 ~ j*1
  educalter_10.2 ~ j*1
  educalter_10.3 ~ j*1
  educalter_10.4 ~ j*1
  educalter_10.5 ~ j*1
  
  educalter_11.1 ~ j*1
  educalter_11.2 ~ j*1
  educalter_11.3 ~ j*1
  educalter_11.4 ~ j*1
  educalter_11.5 ~ j*1
  
  
  
'



fit <- lavaan(myModel, data = datalw, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```

--- 

### five wave CLPM five indicators two step

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '
  # Estimate the lagged effects (constrained)
  eu_8   ~ a*eu_7 + b*Feducalter_7
  eu_9   ~ a*eu_8 + b*Feducalter_8
  eu_10  ~ a*eu_9 + b*Feducalter_9
  eu_11  ~ a*eu_10 + b*Feducalter_10
  
  Feducalter_8  ~ c*eu_7 + d*Feducalter_7
  Feducalter_9  ~ c*eu_8 + d*Feducalter_8
  Feducalter_10  ~ c*eu_9 + d*Feducalter_9
  Feducalter_11  ~ c*eu_10 + d*Feducalter_10
  
  # Estimate the covariance at the first wave. 
  eu_7 ~~ Feducalter_7 # Covariance

  # Estimate the covariances between the residuals
  eu_8 ~~ Feducalter_8
  eu_9 ~~ Feducalter_9
  eu_10 ~~ Feducalter_10
  eu_11 ~~ Feducalter_11
  
  # Estimate the variance 
  eu_7 ~~ eu_7 
  Feducalter_7 ~~ Feducalter_7
  
  # Estimate the residual variance
  eu_8 ~~ eu_8 
  Feducalter_8 ~~ Feducalter_8
  eu_9 ~~ eu_9 
  Feducalter_9 ~~ Feducalter_9
  eu_10 ~~ eu_10 
  Feducalter_10 ~~ Feducalter_10
  eu_11 ~~ eu_11 
  Feducalter_11 ~~ Feducalter_11
  
  #intercepts
  Feducalter_7 ~ 1
  Feducalter_8 ~ 1
  Feducalter_9 ~ 1
  Feducalter_10 ~ 1
  Feducalter_11 ~ 1
  
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
'



fit <- lavaan(myModel, data = datalwb, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```

--- 



---

### five wave CLPM aggregation method

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '
  # Estimate the lagged effects (constrained)
  eu_8   ~ a*eu_7 + b*Meducalter_7
  eu_9   ~ a*eu_8 + b*Meducalter_8
  eu_10  ~ a*eu_9 + b*Meducalter_9
  eu_11  ~ a*eu_10 + b*Meducalter_10
  
  Meducalter_8  ~ c*eu_7 + d*Meducalter_7
  Meducalter_9  ~ c*eu_8 + d*Meducalter_8
  Meducalter_10  ~ c*eu_9 + d*Meducalter_9
  Meducalter_11  ~ c*eu_10 + d*Meducalter_10
  
  # Estimate the covariance at the first wave. 
  eu_7 ~~ Meducalter_7 # Covariance

  # Estimate the covariances between the residuals
  eu_8 ~~ Meducalter_8
  eu_9 ~~ Meducalter_9
  eu_10 ~~ Meducalter_10
  eu_11 ~~ Meducalter_11
  
  # Estimate the variance 
  eu_7 ~~ eu_7 
  Meducalter_7 ~~ Meducalter_7
  
  # Estimate the residual variance
  eu_8 ~~ eu_8 
  Meducalter_8 ~~ Meducalter_8
  eu_9 ~~ eu_9 
  Meducalter_9 ~~ Meducalter_9
  eu_10 ~~ eu_10 
  Meducalter_10 ~~ Meducalter_10
  eu_11 ~~ eu_11 
  Meducalter_11 ~~ Meducalter_11
  
  #intercepts
  Meducalter_7 ~ 1
  Meducalter_8 ~ 1
  Meducalter_9 ~ 1
  Meducalter_10 ~ 1
  Meducalter_11 ~ 1
  
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
'



fit <- lavaan(myModel, data = datalwb, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```

--- 






## RI five indicators, one-step versus two-step MULDER {.tabset .tabset-fade}

### one-step

Be aware that Mulder is using `cfa` and thus allows for all covariances between the factors. 
Why are the residual variances set to zero? we are assuming that the indicators and the ri perfectly predict the factors. no measurement error.??

```{r}
RICLPM5.ext3 <- '
  
  #####################
  # MEASUREMENT MODEL #
  #####################
  
  # Factor models for x at 5 waves (constrained). 
  FX1 =~ a*x11 + b*x12 + c*x13
  FX2 =~ a*x21 + b*x22 + c*x23
  FX3 =~ a*x31 + b*x32 + c*x33
  FX4 =~ a*x41 + b*x42 + c*x43
  FX5 =~ a*x51 + b*x52 + c*x53
  
  # Factor models for y at 5 waves (constrained).
  FY1 =~ d*y11 + e*y12 + f*y13
  FY2 =~ d*y21 + e*y22 + f*y23
  FY3 =~ d*y31 + e*y32 + f*y33
  FY4 =~ d*y41 + e*y42 + f*y43
  FY5 =~ d*y51 + e*y52 + f*y53
  
  # Constrained intercepts over time (this is necessary for strong factorial 
  # invariance; without these contraints we have week factorial invariance). 
  x11 + x21 + x31 + x41 + x51 ~ g*1
  x12 + x22 + x32 + x42 + x52 ~ h*1
  x13 + x23 + x33 + x43 + x53 ~ i*1
  y11 + y21 + y31 + y41 + y51 ~ j*1
  y12 + y22 + y32 + y42 + y52 ~ k*1
  y13 + y23 + y33 + y43 + y53 ~ l*1
  
  # Free latent means from t = 2 onward (only do this in combination with the 
  # constraints on the intercepts; without these, this would not be specified).
  FX2 + FX3 + FX4 + FX5 + FY2 + FY3 + FY4 + FY5 ~ 1
  
  ################
  # BETWEEN PART #
  ################
  
  # Create between factors (random intercepts). 
  RIX =~ 1*FX1 + 1*FX2 + 1*FX3 + 1*FX4 + 1*FX5
  RIY =~ 1*FY1 + 1*FY2 + 1*FY3 + 1*FY4 + 1*FY5
  # Set the residual variances of all FX and FY variables to 0. 
  FX1 ~~ 0*FX1
  FX2 ~~ 0*FX2
  FX3 ~~ 0*FX3
  FX4 ~~ 0*FX4
  FX5 ~~ 0*FX5
  FY1 ~~ 0*FY1
  FY2 ~~ 0*FY2
  FY3 ~~ 0*FY3
  FY4 ~~ 0*FY4
  FY5 ~~ 0*FY5
  
  ###############
  # WITHIN PART #
  ###############
 
  # Create the within-part.
  WFX1 =~ 1*FX1
  WFX2 =~ 1*FX2
  WFX3 =~ 1*FX3
  WFX4 =~ 1*FX4
  WFX5 =~ 1*FX5
  
  WFY1 =~ 1*FY1
  WFY2 =~ 1*FY2
  WFY3 =~ 1*FY3
  WFY4 =~ 1*FY4
  WFY5 =~ 1*FY5
  
  # Specify the lagged effects between the within-person centered latent variables.
  WFX2 + WFY2 ~ WFX1 + WFY1
  WFX3 + WFY3 ~ WFX2 + WFY2
  WFX4 + WFY4 ~ WFX3 + WFY3
  WFX5 + WFY5 ~ WFX4 + WFY4
  
  # Estimate the correlations within the same wave.
  WFX2 ~~ WFY2
  WFX3 ~~ WFY3 
  WFX4 ~~ WFY4
  WFX5 ~~ WFY5
  
  ##########################
  # ADDITIONAL CONSTRAINTS #
  ##########################
  
  # Set correlations between the between-factors (random intercepts) and within-
  # factors at wave 1 at 0. 
  RIX + RIY ~~ 0*WFX1 + 0*WFY1
'
RICLPM5.ext3.fit <- cfa(RICLPM5.ext3, data = datMI, missing = 'ML') 
summary(RICLPM5.ext3.fit, standardized = T)
```

### two-step


```{r }
RICLPM5.ext3 <- '
  
  ################
  # BETWEEN PART #
  ################
  
  # Create between factors (random intercepts). 
  RIX =~ 1*FX1 + 1*FX2 + 1*FX3 + 1*FX4 + 1*FX5
  RIY =~ 1*FY1 + 1*FY2 + 1*FY3 + 1*FY4 + 1*FY5
  
  # Estimate the variance and covariance of the random intercepts. 
  RIX ~~ RIX
  RIY ~~ RIY
  RIX ~~ RIY
  
  
  ###############
  # WITHIN PART #
  ###############
 
  # Create the within-part.
  WFX1 =~ 1*FX1
  WFX2 =~ 1*FX2
  WFX3 =~ 1*FX3
  WFX4 =~ 1*FX4
  WFX5 =~ 1*FX5
  
  WFY1 =~ 1*FY1
  WFY2 =~ 1*FY2
  WFY3 =~ 1*FY3
  WFY4 =~ 1*FY4
  WFY5 =~ 1*FY5
  
  # Specify the lagged effects between the within-person centered latent variables.
  WFX2 + WFY2 ~ WFX1 + WFY1
  WFX3 + WFY3 ~ WFX2 + WFY2
  WFX4 + WFY4 ~ WFX3 + WFY3
  WFX5 + WFY5 ~ WFX4 + WFY4
  
  # Estimate the COVARIANCE within the same wave.
  WFX2 ~~ WFY2
  
  # Estimate the RESIDUAL COVARIANCE within the same wave.
  WFX2 ~~ WFY2
  WFX3 ~~ WFY3 
  WFX4 ~~ WFY4
  WFX5 ~~ WFY5
  
  # Estimate the (residual) variance of the within-person centered variables.
  WFX1 ~~ WFX1 # Variances
  WFY1 ~~ WFY1 # Variances
  WFX2 ~~ WFX2 # Residual variances
  WFX3 ~~ WFX3 # Residual variances
  WFX4 ~~ WFX4 # Residual variances
  WFX5 ~~ WFX5 # Residual variances
  WFY2 ~~ WFY2 # Residual variances
  WFY3 ~~ WFY3 # Residual variances
  WFY4 ~~ WFY4 # Residual variances
  WFY5 ~~ WFY5 # Residual variances
  
  
  # intercepts
  FX1 ~ 1
  FX2 ~ 1
  FX3 ~ 1
  FX4 ~ 1
  FX5 ~ 1
  
  FY1 ~ 1
  FY2 ~ 1
  FY3 ~ 1
  FY4 ~ 1
  FY5 ~ 1
  
  
  ##########################
  # ADDITIONAL CONSTRAINTS #
  ##########################
  
  # Set correlations between the between-factors (random intercepts) and within-
  # factors at wave 1 at 0. 
  RIX + RIY ~~ 0*WFX1 + 0*WFY1
'

RICLPM5.ext3.fit <- lavaan(RICLPM5.ext3, data = datMIb, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM5.ext3.fit, standardized = T)
```



## RI five indicators, one-step versus two-step LISS {.tabset .tabset-fade}

### RI-CLPM five indicators one-step
```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '
  ##################
  #measurement part#
  ##################
  
  #alters are identical thus equal loadings, variances and intercepts at each time point. 
  
  #constrained loadings
  Feducalter_7 =~ 1*educalter_7.1 + 1*educalter_7.2 + 1*educalter_7.3 +  1*educalter_7.4 + 1*educalter_7.5
  Feducalter_8 =~ 1*educalter_8.1 + 1*educalter_8.2 + 1*educalter_8.3 +  1*educalter_8.4 + 1*educalter_8.5
  Feducalter_9 =~ 1*educalter_9.1 + 1*educalter_9.2 + 1*educalter_9.3 +  1*educalter_9.4 + 1*educalter_9.5
  Feducalter_10 =~ 1*educalter_10.1 + 1*educalter_10.2 + 1*educalter_10.3 +  1*educalter_10.4 + 1*educalter_10.5
  Feducalter_11 =~ 1*educalter_11.1 + 1*educalter_11.2 + 1*educalter_11.3 +  1*educalter_11.4 + 1*educalter_11.5

  #constrained variances
  educalter_7.1 ~~ e*educalter_7.1
  educalter_7.2 ~~ e*educalter_7.2
  educalter_7.3 ~~ e*educalter_7.3
  educalter_7.4 ~~ e*educalter_7.4
  educalter_7.5 ~~ e*educalter_7.5
  
  educalter_8.1 ~~ f*educalter_8.1
  educalter_8.2 ~~ f*educalter_8.2
  educalter_8.3 ~~ f*educalter_8.3
  educalter_8.4 ~~ f*educalter_8.4
  educalter_8.5 ~~ f*educalter_8.5
  
  educalter_9.1 ~~ g*educalter_9.1
  educalter_9.2 ~~ g*educalter_9.2
  educalter_9.3 ~~ g*educalter_9.3
  educalter_9.4 ~~ g*educalter_9.4
  educalter_9.5 ~~ g*educalter_9.5
  
  educalter_10.1 ~~ h*educalter_10.1
  educalter_10.2 ~~ h*educalter_10.2
  educalter_10.3 ~~ h*educalter_10.3
  educalter_10.4 ~~ h*educalter_10.4
  educalter_10.5 ~~ h*educalter_10.5
  
  educalter_11.1 ~~ i*educalter_11.1
  educalter_11.2 ~~ i*educalter_11.2
  educalter_11.3 ~~ i*educalter_11.3
  educalter_11.4 ~~ i*educalter_11.4
  educalter_11.5 ~~ i*educalter_11.5
  
  #constrained intercepts
  educalter_7.1 ~ j*1
  educalter_7.2 ~ j*1
  educalter_7.3 ~ j*1
  educalter_7.4 ~ j*1
  educalter_7.5 ~ j*1
  
  educalter_8.1 ~ j*1
  educalter_8.2 ~ j*1
  educalter_8.3 ~ j*1
  educalter_8.4 ~ j*1
  educalter_8.5 ~ j*1
  
  educalter_9.1 ~ j*1
  educalter_9.2 ~ j*1
  educalter_9.3 ~ j*1
  educalter_9.4 ~ j*1
  educalter_9.5 ~ j*1
  
  educalter_10.1 ~ j*1
  educalter_10.2 ~ j*1
  educalter_10.3 ~ j*1
  educalter_10.4 ~ j*1
  educalter_10.5 ~ j*1
  
  educalter_11.1 ~ j*1
  educalter_11.2 ~ j*1
  educalter_11.3 ~ j*1
  educalter_11.4 ~ j*1
  educalter_11.5 ~ j*1
  
  # Free latent means from t = 2 onward. 
  Feducalter_8 ~ 1
  Feducalter_9 ~ 1
  Feducalter_10 ~ 1
  Feducalter_11 ~ 1

  #################
  #structural part#
  #################
  
  
  # Create between components (random intercepts)
  RIx =~ 1*Feducalter_7 + 1*Feducalter_8 + 1*Feducalter_9 + 1*Feducalter_10 + 1*Feducalter_11
  RIy =~ 1*eu_7 + 1*eu_8 + 1*eu_9 + 1*eu_10 + 1*eu_11

  # Set the residual variances of all FX variables to 0. I DONT UNDERSTAND THIS! no measurement error?? how do we deal with this in the two-step approach? if i do i get negative variance of within components. 
  #Feducalter_7 ~~ 0*Feducalter_7
  #Feducalter_8 ~~ 0*Feducalter_8
  #Feducalter_9 ~~ 0*Feducalter_9
  #Feducalter_10 ~~ 0*Feducalter_10
  #Feducalter_11 ~~ 0*Feducalter_11

  # Create within-person centered variables. 
  wFeducalter_7 =~ 1*Feducalter_7
  wFeducalter_8 =~ 1*Feducalter_8
  wFeducalter_9 =~ 1*Feducalter_9
  wFeducalter_10 =~ 1*Feducalter_10
  wFeducalter_11 =~ 1*Feducalter_11
  weu_7 =~ 1*eu_7
  weu_8 =~ 1*eu_8
  weu_9 =~ 1*eu_9
  weu_10 =~ 1*eu_10
  weu_11 =~ 1*eu_11 
  
  # Estimate the lagged effects (constrained)
  weu_8   ~ a*weu_7 + b*wFeducalter_7
  weu_9   ~ a*weu_8 + b*wFeducalter_8
  weu_10  ~ a*weu_9 + b*wFeducalter_9
  weu_11  ~ a*weu_10 + b*wFeducalter_10
  
  wFeducalter_8  ~ c*weu_7 + d*wFeducalter_7
  wFeducalter_9  ~ c*weu_8 + d*wFeducalter_8
  wFeducalter_10  ~ c*weu_9 + d*wFeducalter_9
  wFeducalter_11  ~ c*weu_10 + d*wFeducalter_10
  
  # Estimate the covariance at the first wave. 
  weu_7 ~~ wFeducalter_7 # Covariance

  # Estimate the covariances between the residuals
  weu_8 ~~ wFeducalter_8
  weu_9 ~~ wFeducalter_9
  weu_10 ~~ wFeducalter_10
  weu_11 ~~ wFeducalter_11
  
  # Estimate the variance 
  weu_7 ~~ weu_7 
  wFeducalter_7 ~~ wFeducalter_7
  
  # Estimate the residual variance
  weu_8 ~~ weu_8 
  wFeducalter_8 ~~ wFeducalter_8
  weu_9 ~~ weu_9 
  wFeducalter_9 ~~ wFeducalter_9
  weu_10 ~~ weu_10 
  wFeducalter_10 ~~ wFeducalter_10
  weu_11 ~~ weu_11 
  wFeducalter_11 ~~ wFeducalter_11
  
  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
  
  ##########################
  # ADDITIONAL CONSTRAINTS #
  ##########################
  
  # Set correlations between the between-factors (random intercepts) and within-
  # factors at wave 1 at 0. I DONT UNDERSTAND WHY, OR BETTER WHY THIS IS NOT INCLUDED IN THE MODEL WITH ONLY ONE INDICATOR
  
  RIx + RIy ~~ 0*wFeducalter_7 + 0*weu_7
  
  
'



fit <- lavaan(myModel, data = datalw, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```

---

### two step

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '

  ################
  # BETWEEN PART #
  ###############

  # Create between components (random intercepts)
  RIx =~ 1*Feducalter_7 + 1*Feducalter_8 + 1*Feducalter_9 + 1*Feducalter_10 + 1*Feducalter_11
  RIy =~ 1*eu_7 + 1*eu_8 + 1*eu_9 + 1*eu_10 + 1*eu_11

  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # Set the residual variances of all FX variables to 0. I DONT UNDERSTAND THIS! no measurement error?? 
  Feducalter_7 ~~ 0*Feducalter_7
  Feducalter_8 ~~ 0*Feducalter_8
  Feducalter_9 ~~ 0*Feducalter_9
  Feducalter_10 ~~ 0*Feducalter_10
  Feducalter_11 ~~ 0*Feducalter_11

  ###############
  # WITHIN PART #
  ###############

   # Create within-person centered variables. 
  wFeducalter_7 =~ 1*Feducalter_7
  wFeducalter_8 =~ 1*Feducalter_8
  wFeducalter_9 =~ 1*Feducalter_9
  wFeducalter_10 =~ 1*Feducalter_10
  wFeducalter_11 =~ 1*Feducalter_11
  weu_7 =~ 1*eu_7
  weu_8 =~ 1*eu_8
  weu_9 =~ 1*eu_9
  weu_10 =~ 1*eu_10
  weu_11 =~ 1*eu_11 
  
  # Estimate the lagged effects (constrained)
  weu_8   ~ a*weu_7 + b*wFeducalter_7
  weu_9   ~ a*weu_8 + b*wFeducalter_8
  weu_10  ~ a*weu_9 + b*wFeducalter_9
  weu_11  ~ a*weu_10 + b*wFeducalter_10
  
  wFeducalter_8  ~ c*weu_7 + d*wFeducalter_7
  wFeducalter_9  ~ c*weu_8 + d*wFeducalter_8
  wFeducalter_10  ~ c*weu_9 + d*wFeducalter_9
  wFeducalter_11  ~ c*weu_10 + d*wFeducalter_10
  
  # Estimate the covariance at the first wave. 
  weu_7 ~~ wFeducalter_7 # Covariance

  # Estimate the covariances between the residuals
  weu_8 ~~ wFeducalter_8
  weu_9 ~~ wFeducalter_9
  weu_10 ~~ wFeducalter_10
  weu_11 ~~ wFeducalter_11
  
  # Estimate the variance 
  weu_7 ~~ weu_7 
  wFeducalter_7 ~~ wFeducalter_7
  
  # Estimate the residual variance
  weu_8 ~~ weu_8 
  wFeducalter_8 ~~ wFeducalter_8
  weu_9 ~~ weu_9 
  wFeducalter_9 ~~ wFeducalter_9
  weu_10 ~~ weu_10 
  wFeducalter_10 ~~ wFeducalter_10
  weu_11 ~~ weu_11 
  wFeducalter_11 ~~ wFeducalter_11
  

  
  #intercepts
  Feducalter_7 ~ 1
  Feducalter_8 ~ 1
  Feducalter_9 ~ 1
  Feducalter_10 ~ 1
  Feducalter_11 ~ 1
  
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
  
  ##########################
  # ADDITIONAL CONSTRAINTS #
  ##########################
  
  # Set correlations between the between-factors (random intercepts) and within-
  # factors at wave 1 at 0. 
  RIx + RIy ~~ 0*wFeducalter_7 + 0*weu_7
  
  
'



fit <- lavaan(myModel, data = datalwb, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```

--- 

## include educational level ego {.tabset .tabset-fade}

I do not really understand why the cross-lagged effects become smaller after introduction of educego. 

### educego not included
```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '

  # Create between components (random intercepts)
  RIx =~ 1*Feducalter_7 + 1*Feducalter_8 + 1*Feducalter_9 + 1*Feducalter_10 + 1*Feducalter_11
  RIy =~ 1*eu_7 + 1*eu_8 + 1*eu_9 + 1*eu_10 + 1*eu_11

   # Create within-person centered variables. 
  wFeducalter_7 =~ 1*Feducalter_7
  wFeducalter_8 =~ 1*Feducalter_8
  wFeducalter_9 =~ 1*Feducalter_9
  wFeducalter_10 =~ 1*Feducalter_10
  wFeducalter_11 =~ 1*Feducalter_11
  weu_7 =~ 1*eu_7
  weu_8 =~ 1*eu_8
  weu_9 =~ 1*eu_9
  weu_10 =~ 1*eu_10
  weu_11 =~ 1*eu_11 
  
  # Estimate the lagged effects (constrained)
  weu_8   ~ a*weu_7 + b*wFeducalter_7
  weu_9   ~ a*weu_8 + b*wFeducalter_8
  weu_10  ~ a*weu_9 + b*wFeducalter_9
  weu_11  ~ a*weu_10 + b*wFeducalter_10
  
  wFeducalter_8  ~ c*weu_7 + d*wFeducalter_7
  wFeducalter_9  ~ c*weu_8 + d*wFeducalter_8
  wFeducalter_10  ~ c*weu_9 + d*wFeducalter_9
  wFeducalter_11  ~ c*weu_10 + d*wFeducalter_10
  
  # Estimate the covariance at the first wave. 
  weu_7 ~~ wFeducalter_7 # Covariance

  # Estimate the covariances between the residuals
  weu_8 ~~ wFeducalter_8
  weu_9 ~~ wFeducalter_9
  weu_10 ~~ wFeducalter_10
  weu_11 ~~ wFeducalter_11
  
  # Estimate the variance 
  weu_7 ~~ weu_7 
  wFeducalter_7 ~~ wFeducalter_7
  
  # Estimate the residual variance
  weu_8 ~~ weu_8 
  wFeducalter_8 ~~ wFeducalter_8
  weu_9 ~~ weu_9 
  wFeducalter_9 ~~ wFeducalter_9
  weu_10 ~~ weu_10 
  wFeducalter_10 ~~ wFeducalter_10
  weu_11 ~~ weu_11 
  wFeducalter_11 ~~ wFeducalter_11
  
  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  #intercepts
  Feducalter_7 ~ 1
  Feducalter_8 ~ 1
  Feducalter_9 ~ 1
  Feducalter_10 ~ 1
  Feducalter_11 ~ 1
  
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
  
  
'



fit <- lavaan(myModel, data = datalwb, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```

---

### educego included

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '

  # Create between components (random intercepts)
  RIx =~ 1*Feducalter_7 + 1*Feducalter_8 + 1*Feducalter_9 + 1*Feducalter_10 + 1*Feducalter_11
  RIy =~ 1*eu_7 + 1*eu_8 + 1*eu_9 + 1*eu_10 + 1*eu_11

  # Regression of random intercepts on z1. 
  RIx + RIy ~ educego_7   

   # Create within-person centered variables. 
  wFeducalter_7 =~ 1*Feducalter_7
  wFeducalter_8 =~ 1*Feducalter_8
  wFeducalter_9 =~ 1*Feducalter_9
  wFeducalter_10 =~ 1*Feducalter_10
  wFeducalter_11 =~ 1*Feducalter_11
  weu_7 =~ 1*eu_7
  weu_8 =~ 1*eu_8
  weu_9 =~ 1*eu_9
  weu_10 =~ 1*eu_10
  weu_11 =~ 1*eu_11 
  
  # Estimate the lagged effects (constrained)
  weu_8   ~ a*weu_7 + b*wFeducalter_7
  weu_9   ~ a*weu_8 + b*wFeducalter_8
  weu_10  ~ a*weu_9 + b*wFeducalter_9
  weu_11  ~ a*weu_10 + b*wFeducalter_10
  
  wFeducalter_8  ~ c*weu_7 + d*wFeducalter_7
  wFeducalter_9  ~ c*weu_8 + d*wFeducalter_8
  wFeducalter_10  ~ c*weu_9 + d*wFeducalter_9
  wFeducalter_11  ~ c*weu_10 + d*wFeducalter_10
  
  # Estimate the covariance at the first wave. 
  weu_7 ~~ wFeducalter_7 # Covariance

  # Estimate the covariances between the residuals
  weu_8 ~~ wFeducalter_8
  weu_9 ~~ wFeducalter_9
  weu_10 ~~ wFeducalter_10
  weu_11 ~~ wFeducalter_11
  
  # Estimate the variance 
  weu_7 ~~ weu_7 
  wFeducalter_7 ~~ wFeducalter_7
  
  # Estimate the residual variance
  weu_8 ~~ weu_8 
  wFeducalter_8 ~~ wFeducalter_8
  weu_9 ~~ weu_9 
  wFeducalter_9 ~~ wFeducalter_9
  weu_10 ~~ weu_10 
  wFeducalter_10 ~~ wFeducalter_10
  weu_11 ~~ weu_11 
  wFeducalter_11 ~~ wFeducalter_11
  
  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  #intercepts
  Feducalter_7 ~ 1
  Feducalter_8 ~ 1
  Feducalter_9 ~ 1
  Feducalter_10 ~ 1
  Feducalter_11 ~ 1
  
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
  
  
'



fit <- lavaan(myModel, data = datalwb, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```
---


## constrained vs unconstrained model {.tabset .tabset-fade}

### with constrained on cross-lagged effects

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '

  # Create between components (random intercepts)
  RIx =~ 1*Feducalter_7 + 1*Feducalter_8 + 1*Feducalter_9 + 1*Feducalter_10 + 1*Feducalter_11
  RIy =~ 1*eu_7 + 1*eu_8 + 1*eu_9 + 1*eu_10 + 1*eu_11

  # Regression of random intercepts on z1. 
  RIx + RIy ~ educego_7   

   # Create within-person centered variables. 
  wFeducalter_7 =~ 1*Feducalter_7
  wFeducalter_8 =~ 1*Feducalter_8
  wFeducalter_9 =~ 1*Feducalter_9
  wFeducalter_10 =~ 1*Feducalter_10
  wFeducalter_11 =~ 1*Feducalter_11
  weu_7 =~ 1*eu_7
  weu_8 =~ 1*eu_8
  weu_9 =~ 1*eu_9
  weu_10 =~ 1*eu_10
  weu_11 =~ 1*eu_11 
  
  # Estimate the lagged effects (constrained)
  weu_8   ~ a*weu_7 + b*wFeducalter_7
  weu_9   ~ a*weu_8 + b*wFeducalter_8
  weu_10  ~ a*weu_9 + b*wFeducalter_9
  weu_11  ~ a*weu_10 + b*wFeducalter_10
  
  wFeducalter_8  ~ c*weu_7 + d*wFeducalter_7
  wFeducalter_9  ~ c*weu_8 + d*wFeducalter_8
  wFeducalter_10  ~ c*weu_9 + d*wFeducalter_9
  wFeducalter_11  ~ c*weu_10 + d*wFeducalter_10
  
  # Estimate the covariance at the first wave. 
  weu_7 ~~ wFeducalter_7 # Covariance

  # Estimate the covariances between the residuals
  weu_8 ~~ wFeducalter_8
  weu_9 ~~ wFeducalter_9
  weu_10 ~~ wFeducalter_10
  weu_11 ~~ wFeducalter_11
  
  # Estimate the variance 
  weu_7 ~~ weu_7 
  wFeducalter_7 ~~ wFeducalter_7
  
  # Estimate the residual variance
  weu_8 ~~ weu_8 
  wFeducalter_8 ~~ wFeducalter_8
  weu_9 ~~ weu_9 
  wFeducalter_9 ~~ wFeducalter_9
  weu_10 ~~ weu_10 
  wFeducalter_10 ~~ wFeducalter_10
  weu_11 ~~ weu_11 
  wFeducalter_11 ~~ wFeducalter_11
  
  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  #intercepts
  Feducalter_7 ~ 1
  Feducalter_8 ~ 1
  Feducalter_9 ~ 1
  Feducalter_10 ~ 1
  Feducalter_11 ~ 1
  
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
  
  
'



fit <- lavaan(myModel, data = datalwb, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```
---

### without constrained on cross-lagged effects

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '

  # Create between components (random intercepts)
  RIx =~ 1*Feducalter_7 + 1*Feducalter_8 + 1*Feducalter_9 + 1*Feducalter_10 + 1*Feducalter_11
  RIy =~ 1*eu_7 + 1*eu_8 + 1*eu_9 + 1*eu_10 + 1*eu_11

  # Regression of random intercepts on z1. 
  RIx + RIy ~ educego_7   

   # Create within-person centered variables. 
  wFeducalter_7 =~ 1*Feducalter_7
  wFeducalter_8 =~ 1*Feducalter_8
  wFeducalter_9 =~ 1*Feducalter_9
  wFeducalter_10 =~ 1*Feducalter_10
  wFeducalter_11 =~ 1*Feducalter_11
  weu_7 =~ 1*eu_7
  weu_8 =~ 1*eu_8
  weu_9 =~ 1*eu_9
  weu_10 =~ 1*eu_10
  weu_11 =~ 1*eu_11 
  
  # Estimate the lagged effects (constrained)
  weu_8   ~ weu_7 + wFeducalter_7
  weu_9   ~ weu_8 + wFeducalter_8
  weu_10  ~ weu_9 + wFeducalter_9
  weu_11  ~ weu_10 + wFeducalter_10
  
  wFeducalter_8  ~ weu_7 + wFeducalter_7
  wFeducalter_9  ~ weu_8 + wFeducalter_8
  wFeducalter_10  ~ weu_9 + wFeducalter_9
  wFeducalter_11  ~ weu_10 + wFeducalter_10
  
  # Estimate the covariance at the first wave. 
  weu_7 ~~ wFeducalter_7 # Covariance

  # Estimate the covariances between the residuals
  weu_8 ~~ wFeducalter_8
  weu_9 ~~ wFeducalter_9
  weu_10 ~~ wFeducalter_10
  weu_11 ~~ wFeducalter_11
  
  # Estimate the variance 
  weu_7 ~~ weu_7 
  wFeducalter_7 ~~ wFeducalter_7
  
  # Estimate the residual variance
  weu_8 ~~ weu_8 
  wFeducalter_8 ~~ wFeducalter_8
  weu_9 ~~ weu_9 
  wFeducalter_9 ~~ wFeducalter_9
  weu_10 ~~ weu_10 
  wFeducalter_10 ~~ wFeducalter_10
  weu_11 ~~ weu_11 
  wFeducalter_11 ~~ wFeducalter_11
  
  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  #intercepts
  Feducalter_7 ~ 1
  Feducalter_8 ~ 1
  Feducalter_9 ~ 1
  Feducalter_10 ~ 1
  Feducalter_11 ~ 1
  
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
  
  
'



fit <- lavaan(myModel, data = datalwb, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```

## structural changes over time

this means that we no longer have a random intercept but rather a latent variable. We however know we have also structural changes. And our measures are one year apart. 

```{r,  attr.source = '.numberLines', results='hold'}
myModel <- '

  # Create between components (random intercepts)
  RIx =~ 1*Feducalter_7 + Feducalter_8 + Feducalter_9 + Feducalter_10 + Feducalter_11
  RIy =~ 1*eu_7 + eu_8 + eu_9 + eu_10 + eu_11

  # Regression of random intercepts / latent variable on z1. 
  RIx + RIy ~ educego_7   

   # Create within-person centered variables. 
  wFeducalter_7 =~ 1*Feducalter_7
  wFeducalter_8 =~ 1*Feducalter_8
  wFeducalter_9 =~ 1*Feducalter_9
  wFeducalter_10 =~ 1*Feducalter_10
  wFeducalter_11 =~ 1*Feducalter_11
  weu_7 =~ 1*eu_7
  weu_8 =~ 1*eu_8
  weu_9 =~ 1*eu_9
  weu_10 =~ 1*eu_10
  weu_11 =~ 1*eu_11 
  
  # Estimate the lagged effects (constrained)
  weu_8   ~ weu_7 + wFeducalter_7
  weu_9   ~ weu_8 + wFeducalter_8
  weu_10  ~ weu_9 + wFeducalter_9
  weu_11  ~ weu_10 + wFeducalter_10
  
  wFeducalter_8  ~ weu_7 + wFeducalter_7
  wFeducalter_9  ~ weu_8 + wFeducalter_8
  wFeducalter_10  ~ weu_9 + wFeducalter_9
  wFeducalter_11  ~ weu_10 + wFeducalter_10
  
  # Estimate the covariance at the first wave. 
  weu_7 ~~ wFeducalter_7 # Covariance

  # Estimate the covariances between the residuals
  weu_8 ~~ wFeducalter_8
  weu_9 ~~ wFeducalter_9
  weu_10 ~~ wFeducalter_10
  weu_11 ~~ wFeducalter_11
  
  # Estimate the variance 
  weu_7 ~~ weu_7 
  wFeducalter_7 ~~ wFeducalter_7
  
  # Estimate the residual variance
  weu_8 ~~ weu_8 
  wFeducalter_8 ~~ wFeducalter_8
  weu_9 ~~ weu_9 
  wFeducalter_9 ~~ wFeducalter_9
  weu_10 ~~ weu_10 
  wFeducalter_10 ~~ wFeducalter_10
  weu_11 ~~ weu_11 
  wFeducalter_11 ~~ wFeducalter_11
  
  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  #intercepts
  Feducalter_7 ~ 1
  Feducalter_8 ~ 1
  Feducalter_9 ~ 1
  Feducalter_10 ~ 1
  Feducalter_11 ~ 1
  
  eu_7 ~ 1
  eu_8 ~ 1
  eu_9 ~ 1
  eu_10 ~ 1
  eu_11 ~ 1
  
  
'



fit <- lavaan(myModel, data = datalwb, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(fit, standardized = T)


```



---

## References