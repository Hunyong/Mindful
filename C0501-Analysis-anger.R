#' ---
#' title: Anger variables. t-test and mixed effect models based on a single dataset (will do multiple set combination later)
#' author: Group H
#' date: April 2, 2018
#' ---
#+ echo=FALSE, warning=FALSE

### C0501 Analysis - anger
## 0. library
source("F0101-variableSet.R")
source("F0501-timeFunction.R")
library(lme4)
library(dplyr)
library(magrittr)
library(geepack)
library(ggplot2)

## 0. getting summary measures and long formats
  # Getting summary measures
  data.working.base <- readRDS("../dataImputed/data.working.base.rds")
  data.working.anger <- readRDS("../dataImputed/data.working.anger.rds")
  data.working.base.anger <- cbind.list(data.working.base, data.working.anger)
  data.working.base.anger.long <- long.list (data.list = data.working.base.anger, 
                                             long.vars = c("IBSS","unexp","extexp", "emoreg", "probsol"))

  # transform time(1,2,3,4,5) to months
  data.working.base.anger.long %<>% visit2month
  
  # change (trt = 1,2) to (trt = 0,1)
  data.working.base.anger.long %<>% trt01
  data.working.base.anger %<>% trt01
  
  
#+ warning=FALSE
#' ## 1. t-test  
  lm(unexp.3 - unexp.1 ~ trt, data = data.working.base.anger[[1]]) %>% summary %>% "["("coefficients")
  lm(extexp.3 - extexp.1 ~ trt, data = data.working.base.anger[[1]]) %>% summary %>% "["("coefficients")
  lm(emoreg.3 - emoreg.1 ~ trt, data = data.working.base.anger[[1]]) %>% summary %>% "["("coefficients")
  lm(probsol.3 - probsol.1 ~ trt, data = data.working.base.anger[[1]]) %>% summary %>% "["("coefficients")

#' <br>
#' <br>
#' 
#+ warning=FALSE
#' ## 2. spaghetti plots
  data.working.base.anger.long[[1]] %>%
    ggplot(aes(month, unexp, col = factor(id), shape=id)) + geom_line()
  
  
#' <br>
#' <br>
#' 
#' ## 3. mixed effect models  
  lmer(unexp ~ trt * month  + (trt|id), data = data.working.base.anger.long[[1]]) %>% summary %>% "["("coefficients")
  lmer(extexp ~ trt * month + (trt|id), data = data.working.base.anger.long[[1]]) %>% summary %>% "["("coefficients")
  lmer(emoreg ~ trt * month + (trt|id), data = data.working.base.anger.long[[1]]) %>% summary %>% "["("coefficients")
  lmer(probsol ~ trt * month + (trt|id), data = data.working.base.anger.long[[1]]) %>% summary %>% "["("coefficients")
  

#' <br>
#' <br>
#' 
#' ## 4. marginal models (GEE)
  geeglm(unexp ~ trt * month, id = id, data = data.working.base.anger.long[[1]]) %>% summary %>% "["("coefficients")
  geeglm(extexp ~ trt * month, id = id, data = data.working.base.anger.long[[1]]) %>% summary %>% "["("coefficients")
  geeglm(emoreg ~ trt * month, id = id, data = data.working.base.anger.long[[1]]) %>% summary %>% "["("coefficients")
  geeglm(probsol ~ trt * month, id = id, data = data.working.base.anger.long[[1]]) %>% summary %>% "["("coefficients")
  
  geeglm(IBSS ~ trt * month + unexp*trt, id = id, data = data.working.base.anger.long[[1]]) %>% summary %>% "["("coefficients")
  geeglm(IBSS ~ trt * month + unexp + extexp + emoreg + probsol, id = id, data = data.working.base.anger.long[[1]]) %>% summary %>% "["("coefficients")
  
  