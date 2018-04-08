#' ---
#' title: Anger and FILE data analysis. t-test and marginal longitudinal models
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
#' ## 1. t-test / ANCOVA  
#' ### 1.1 ANOVA of anger constructs
#' Effect of mindfulness intervention on the change in outcomes from baseline to 3 months.
#' No significant results were obtained.
#+ warning=FALSE, echo=FALSE
  lapply(1:m, function(i) 
    lm(unexp.3 - unexp.1 ~ trt , data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list() %>% kable(digits=3, caption = "ANOVA of Unexpressed Anger")
  
  lapply(1:m, function(i) 
    lm(extexp.3 - extexp.1 ~ trt , data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list() %>% kable(digits=3, caption = "ANOVA of External Expression")
  
  lapply(1:m, function(i) 
    lm(emoreg.3 - emoreg.1 ~ trt , data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list() %>% kable(digits=3, caption = "ANOVA of Emotion Regulation")
  
  lapply(1:m, function(i) 
    lm(probsol.3 - probsol.1 ~ trt , data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list() %>% kable(digits=3, caption = "ANOVA of Problem Solving")
  
#' ### 1.2 ANCOVA of anger constructs and interaction with FILE
#+ warning=FALSE, echo=FALSE

  lapply(1:m, function(i) 
    lm(unexp.3 - unexp.1 ~ trt  * FILE.base, data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list() %>% kable(digits=3, caption = "ANCOVA of Unexpressed Anger controlling for distressing events")
  
  lapply(1:m, function(i) 
    lm(extexp.3 - extexp.1 ~ trt  * FILE.base, data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list() %>% kable(digits=3, caption = "ANCOVA of External Expression controlling for distressing events")
  
  lapply(1:m, function(i) 
    lm(emoreg.3 - emoreg.1 ~ trt  * FILE.base, data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list() %>% kable(digits=3, caption = "ANCOVA of Emotion Regulation controlling for distressing events")
  
  lapply(1:m, function(i) 
    lm(probsol.3 - probsol.1 ~ trt  * FILE.base, data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list() %>% kable(digits=3, caption = "ANCOVA of Problem Solving controlling for distressing events")
  
  
#' <br>
#' <br>
#' 
#+ warning=FALSE, echo=FALSE
#' ## 2. spaghetti plots (First imputation)
#+ warning=FALSE, echo=FALSE
  data.working.baf.long[[1]] %>%
    ggplot(aes(month, unexp, col = factor(id), shape=id)) + geom_line() + 
    guides(color = FALSE) + facet_grid(.~ifelse(trt, "Mindfulness Group", "Support Group")) +
    scale_x_continuous(breaks = c( .5, 3, 6, 12))+
    ylab("Unexpressed anger") +
    ggtitle("Unexpressed anger score trajectories for each subject")
  data.working.baf.long[[1]] %>%
    ggplot(aes(month, extexp, col = factor(id), shape=id)) + geom_line() + 
    guides(color = FALSE) + facet_grid(.~ifelse(trt, "Mindfulness Group", "Support Group")) +
    scale_x_continuous(breaks = c( .5, 3, 6, 12))+
    ylab("External expression") +
    ggtitle("External expression score trajectories for each subject")
  data.working.baf.long[[1]] %>%
    ggplot(aes(month, emoreg, col = factor(id), shape=id)) + geom_line() + 
    guides(color = FALSE) + facet_grid(.~ifelse(trt, "Mindfulness Group", "Support Group")) +
    scale_x_continuous(breaks = c( .5, 3, 6, 12))+
    ylab("Emotion regulation") +
    ggtitle("Emotion regulation score trajectories for each subject")
  data.working.baf.long[[1]] %>%
    ggplot(aes(month, probsol, col = factor(id), shape=id)) + geom_line() + 
    guides(color = FALSE) + facet_grid(.~ifelse(trt, "Mindfulness Group", "Support Group")) +
    scale_x_continuous(breaks = c( .5, 3, 6, 12))+
    ylab("Problem solving") +
    ggtitle("Problem solving score trajectories for each subject")
  
#' <br>
#' <br>
#' 
#' ## 3. mixed effect models  
#' ## 3.1 mixed effect models - No covariates
#' Note that 
#' 1) intercept = mean score for support group at baseline.
#' <br>
#' 2) trt = mean score (Intervention) - mean score (Support) at baseline. Expected to be zero.
#' 
#+ warning=FALSE, echo=FALSE
    lapply(1:m, function(i) 
      lmer(unexp ~ trt * month  + (trt|id), data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
      Rubin.list() %>% kable(digits=3, caption = "Mixed model of Unexpressed Anger")
    
    lapply(1:m, function(i) 
      lmer(extexp ~ trt * month + (trt|id), data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
      Rubin.list() %>% kable(digits=3, caption = "Mixed model of External Expression")
    
    lapply(1:m, function(i) 
      lmer(emoreg ~ trt * month + (trt|id), data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
      Rubin.list() %>% kable(digits=3, caption = "Mixed model of Emotion Regulation")
    
    lapply(1:m, function(i) 
      lmer(probsol ~ trt * month + (trt|id), data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
      Rubin.list() %>% kable(digits=3, caption = "ANMixed model of Problem Solving")

#' ## 3.2 mixed effect models with FILE-adjusted - NOT-CONVERGING

#+ echo=FALSE, warning=FALSE, include=FALSE, eval=FALSE
    lapply(1:m, function(i) 
      lmer(unexp ~ trt * month + trt *FILE.recent  + (trt + FILE.recent|id), data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
      Rubin.list() %>% kable(digits=3, caption = "Mixed model of Unexpressed Anger")
    
    lapply(1:m, function(i) 
      lmer(extexp ~ trt * month + trt *FILE.recent  + (trt + FILE.recent|id), data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
      Rubin.list() %>% kable(digits=3, caption = "Mixed model of External Expression")
    
    lapply(1:m, function(i) 
      lmer(emoreg ~ trt * month + trt *FILE.recent  + (trt + FILE.recent|id), data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
      Rubin.list() %>% kable(digits=3, caption = "Mixed model of Emotion Regulation")
    
    lapply(1:m, function(i) 
      lmer(probsol ~ trt * month + trt *FILE.recent  + (trt + FILE.recent|id), data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
      Rubin.list() %>% kable(digits=3, caption = "ANMixed model of Problem Solving")

#+ echo=FALSE, warning=FALSE
    
     
#' ## 4. marginal models (GEE)
#' ### Model. Constructs ~ trt * month * FILE
#' The Marginal models were fitted with the full-interaction design.
#' 
#' <br>
#' All but the Problem solving model coefficients were non-significant.
#' 
#+ echo=FALSE, warning=FALSE
    
  lapply(1:m, function(i) 
    geeglm(unexp ~ trt * month * FILE.recent, id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of Unexpressed Anger adjusted for FILE score")

  lapply(1:m, function(i) 
    geeglm(extexp ~ trt * month * FILE.recent, id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of External Expression adjusted for FILE score")

  lapply(1:m, function(i) 
    geeglm(emoreg ~ trt * month*FILE.recent, id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of Emotion Regulation adjusted for FILE score")
  
  lapply(1:m, function(i) 
    geeglm(probsol ~ trt * month*FILE.recent, id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of Problem Solving adjusted for FILE score")
  
#' The baseline problem solving score was lower for intervention group than for the support group. 
#' This is an unexpected result and might be caused by random chance since it is before the intervention was given.
#' Or this might be caused by the model misspecification in terms of time. (See the highly non-monotone trends)
#' 
#' <br>
#' 
#' The other coefficient to look at is the treatment-FILE interaction. 
#' Pay attention to the mean probelm solving scores at baseline.
#' The orders of low-high FILE are flipped between Mindfulness group and the support group.
#' Again, this should not be significant and does not have meaningful interpretation.
#' 
#' <br>
#' 
#' ## Second model - Some interactions removed
#' ### Model. Constructs ~ trt * month + FILE * trt
#' Interaction effects are mostly very small. 
#' Treatment * FILE interaction seems to be the only one that is potentially important.
#' 
#' 
#+ echo=FALSE, warning=FALSE
  
  lapply(1:m, function(i) 
    geeglm(unexp ~ trt * month + FILE.recent * trt, id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of Unexpressed Anger adjusted for FILE score")
  
  lapply(1:m, function(i) 
    geeglm(extexp ~ trt * month + FILE.recent * trt, id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of External Expression adjusted for FILE score")
  
  lapply(1:m, function(i) 
    geeglm(emoreg ~ trt * month + FILE.recent * trt, id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of Emotion Regulation adjusted for FILE score")
  
  lapply(1:m, function(i) 
    geeglm(probsol ~ trt * month + FILE.recent * trt, id = id, data = data.working.baf.long[[i]]) %>% summary %>% "[["("coefficients")) %>% 
    Rubin.list(std.name = "Std.err") %>% kable(digits=3, caption = "Marginal model of Problem Solving adjusted for FILE score")

#' Overall, people with many distressing events have higher scores
#' (small p-values for all but emotion regulation).
  