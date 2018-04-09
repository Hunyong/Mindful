#' ---
#' title: Anger and IBSS data analysis. t-test and marginal longitudinal models
#' author: Group H
#' date: April 9, 2018
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
data.working.base <- readRDS("../dataImputed/data.working.base.rds")
data.working.anger <- readRDS("../dataImputed/data.working.anger.rds")
data.working.FILE <- readRDS("../dataImputed/data.working.FILE.rds")
data.working.baf <- cbind.list(data.working.base, data.working.anger)
data.working.baf <- cbind.list(data.working.baf, data.working.FILE)
data.working.baf.long <- long.list (data.list = data.working.baf, 
                                    long.vars = c("IBSS","unexp","extexp", "emoreg", "probsol"))

# transform time(1,2,3,4,5) to months
data.working.baf.long %<>% visit2month

# change (trt = 1,2) to (trt = 0,1)
data.working.baf.long %<>% trt01
data.working.baf %<>% trt01





#' All the analyses in this report are based on 25 multiple imputation, unless otherwise mentioned.
#' 
#' ## 1. ANCOVA of 3-month IBSS change and interaction with anger (at baseline)
#' ### Model. (IBSS.3 - IBSS.1) ~ trt  * (unexp.1 + extexp.1 + emoreg.1 + probsol.1)
#+ warning=FALSE, echo=FALSE

lapply(1:m, function(i) 
  lm(IBSS.3 - IBSS.1 ~ trt  * (unexp.1 + extexp.1 + emoreg.1 + probsol.1), data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list() %>% kable(digits=3, caption = "ANCOVA of Unexpressed Anger controlling for distressing events")


#' ### Model. (IBSS.3 - IBSS.1) ~ trt  * (each anger construct)
#+ warning=FALSE, echo=FALSE

lapply(1:m, function(i) 
  lm(IBSS.3 - IBSS.1 ~ trt  * unexp.1, data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list() %>% kable(digits=3, caption = "ANCOVA of Unexpressed 1) Unexpressed anger controlling for distressing events")

lapply(1:m, function(i) 
  lm(IBSS.3 - IBSS.1 ~ trt  * extexp.1, data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list() %>% kable(digits=3, caption = "ANCOVA of Unexpressed 2) External expression controlling for distressing events")

lapply(1:m, function(i) 
  lm(IBSS.3 - IBSS.1 ~ trt  * emoreg.1, data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list() %>% kable(digits=3, caption = "ANCOVA of Unexpressed 3) Emotion regulation controlling for distressing events")

lapply(1:m, function(i) 
  lm(IBSS.3 - IBSS.1 ~ trt  * probsol.1, data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list() %>% kable(digits=3, caption = "ANCOVA of Unexpressed 4) Problem solving controlling for distressing events")


#' <br>
#' <br>
#' 
#' ## 2. marginal models (GEE)
#' ### Model. IBSS ~ trt * month * anger
#' The Marginal models were fitted with the full-interaction design.
#' 
#' <br>
#' All but the Problem solving model coefficients were non-significant.
#' 
#+ echo=FALSE, warning=FALSE

lapply(1:m, function(i) 
  geeglm(IBSS ~ trt * month * (unexp + extexp + emoreg + probsol), id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of IBSS adjusted for anger scores")


#' ## Second model - Some interactions removed
#' ### Model. IBSS ~ trt * month + anger
#' 
#' 
#+ echo=FALSE, warning=FALSE

lapply(1:m, function(i) 
  geeglm(IBSS ~ trt * month + (unexp + extexp + emoreg + probsol), id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of IBSS adjusted for anger scores")

#' Controlling for anger scores, treatment effects are negative on IBSS score, which is congruent to the previous result.
#' Controlling for time and treatment, people with higher emotion regulation scores will have low IBSS.
#' 
#' 

#' ### Model. IBSS ~ trt * month + (emoreg + probsol) + FILE
#' 
#' 
#+ echo=FALSE, warning=FALSE

lapply(1:m, function(i) 
  geeglm(IBSS ~ trt * month + (emoreg + probsol) + FILE.recent, id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of IBSS adjusted for anger and FILE scores")


#' ### Model. IBSS ~ trt * month + (emoreg + probsol) * FILE
#' 
#' 
#+ echo=FALSE, warning=FALSE
lapply(1:m, function(i) 
  geeglm(IBSS ~ trt * month + (emoreg + probsol) * FILE.recent, id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
  Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of IBSS adjusted for anger scores with interaction")
