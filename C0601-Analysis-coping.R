#' ---
#' title: Coping data analysis. t-test and marginal longitudinal models
#' author: Group H
#' date: April 15, 2018
#' ---
#+ echo=FALSE, warning=FALSE, include=FALSE

## 0. library
source("F0101-variableSet.R")
source("F0501-timeFunction.R")
source("F0502-analysis.R")
library(lme4); library(geepack)
library(dplyr); library(magrittr)
library(ggplot2); library(knitr)

m = 25

## 0. getting summary measures and long formats
# Getting summary measures
data.working.base <- readRDS("/Users/yiling/Desktop/UNC-Chapel Hill/Coursework/2018 Spring/841/Mindful/data.working.base.rds")
data.working.coping <- readRDS("/Users/yiling/Desktop/UNC-Chapel Hill/Coursework/2018 Spring/841/Mindful/data.working.coping.rds")
data.working.baf <- cbind.list(data.working.base, data.working.coping)
data.working.baf.long <- long.list (data.list = data.working.baf, 
                                    long.vars = c("IBSS","catastrophizing","rps"))

# transform time(1,2,3,4,5) to months
data.working.baf.long %<>% visit2month

# change (trt = 1,2) to (trt = 0,1)
data.working.baf.long %<>% trt01
data.working.baf %<>% trt01





#' All the analyses in this report are based on 25 multiple imputation, unless otherwise mentioned.
#' 
#' ## 1. t-test / ANCOVA  
#' ### 1.1 ANOVA of coping strategy
#' Effect of mindfulness intervention on the change in coping strategy from baseline to 3 months.
#' No significant results were obtained.
#+ warning=FALSE, echo=FALSE
lapply(1:m, function(i) 
  lm(catastrophizing.3 - catastrophizing.1 ~ trt , data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list() %>% kable(digits=3, caption = "ANOVA of Catastrophizing")

lapply(1:m, function(i) 
  lm(rps.3 - rps.1 ~ trt , data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list() %>% kable(digits=3, caption = "ANOVA of Reinterpreting Pain Sensation")


#' ## 2. ANCOVA of 3-month IBSS change and interaction with coping strategy (at baseline)
#' ### Model. (IBSS.3 - IBSS.1) ~ trt  * (catastrophizing.1 + rps.1)
#+ warning=FALSE, echo=FALSE

lapply(1:m, function(i) 
  lm(IBSS.3 - IBSS.1 ~ trt  * (catastrophizing.1 + rps.1), data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list() %>% kable(digits=3, caption = "ANCOVA of Catastrophizing and Reinterpreting Pain Sensation")


#' ### Model. (IBSS.3 - IBSS.1) ~ trt  * (each coping strategy category)
#+ warning=FALSE, echo=FALSE

lapply(1:m, function(i) 
  lm(IBSS.3 - IBSS.1 ~ trt  * catastrophizing.1, data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list() %>% kable(digits=3, caption = "ANCOVA of Catastrophizing")

lapply(1:m, function(i) 
  lm(IBSS.3 - IBSS.1 ~ trt  * rps.1, data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list() %>% kable(digits=3, caption = "ANCOVA of Reinterpreting Pain Sensation")


#' <br>
#' <br>
#' 
#' ## 3. marginal models (GEE)
#' ### Model. IBSS ~ trt * month * coping
#' The Marginal models were fitted with the full-interaction design.
#' 
#' <br>
#' All but the Catastrophizing were non-significant.
#' 
#+ echo=FALSE, warning=FALSE

lapply(1:m, function(i) 
  geeglm(IBSS ~ trt * month * (catastrophizing + rps), id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of IBSS adjusted for coping strategy summary scores")


#' ## Second model - Some interactions removed
#' ### Model. IBSS ~ trt * month + coping
#' 
#' 
#+ echo=FALSE, warning=FALSE

lapply(1:m, function(i) 
  geeglm(IBSS ~ trt * month + (catastrophizing + rps), id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of IBSS adjusted for coping strategy summary scores")

#' Controlling for coping strategy, treatment effects are negative on IBSS score, which is congruent to the previous result.
#' Controlling for time and treatment, people with higher Catastrophizing scores will have higher IBSS.
#' 
#' 

