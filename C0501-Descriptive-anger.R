#' ---
#' title: Anger and FILE - Factor analysis and Descriptive statistics
#' author: Group H
#' date: April 9, 2018
#' ---
#+ echo=FALSE, warning=FALSE, include=FALSE

### C0502 Analysis - anger. Descriptive and factor analysis

library(foreign)
library(dplyr); library(magrittr)
library(psych) # Chronbach's alpha
library(ggplot2); library(knitr)
source("F0001-basic.R")
source("F0101-variableSet.R")
source("F0501-timeFunction.R")


data <- readRDS("data.raw.rds")
anger <- readRDS("anger.rds")
sample.include <- readRDS("sample.include.rds")
anger.vector <- do.call(c, do.call(c, anger))

data.working.base <- readRDS("../dataImputed/data.working.base.rds")
data.working.anger <- readRDS("../dataImputed/data.working.anger.rds")
data.working.FILE <- readRDS("../dataImputed/data.working.FILE.rds")
data.mice.FILE.r.list <- readRDS("../dataImputed/data.mice.FILE.r.list.rds")
data.working.baf <- cbind.list(data.working.base, data.working.anger)
data.working.baf <- cbind.list(data.working.baf, data.working.FILE)
data.working.baf.long <- long.list (data.list = data.working.baf, 
                                    long.vars = c("IBSS","unexp","extexp", "emoreg", "probsol"))

# transform time(1,2,3,4,5) to months
data.working.baf.long %<>% visit2month

# change (trt = 1,2) to (trt = 0,1)
data.working.baf.long %<>% trt01
data.working.baf %<>% trt01


timePoints = c("baseline", "2 weeks", "3 months", "6 months", "12 months")

#+ echo=FALSE, warning=FALSE
#' ## 1. Anger - Internal consistency of factors (confidence intervals of Cronbach's alpha)
#' Factor analysis results based on raw data. Four factors were calculated out of 26 items (9, 7, 5, 5 each). 
#' The second line is the point estimate and the first and the third lines are the 95% confidence bounds.
#' Cronbach's alpha estimates were high for all four constructs, with some exceptions such as external expression at 12th month.
#+ echo=FALSE, warning=FALSE
  sapply(anger$unexp, function(s) psych::alpha(data[,s]) %>% alpha.ci)[3:1,] %>% 
    kable(caption = "Unexpressed anger", col.names = timePoints)
  sapply(anger$extexp, function(s) psych::alpha(data[,s]) %>% alpha.ci)[3:1,] %>% 
    kable(caption = "External expression", col.names = timePoints)
  sapply(anger$emoreg, function(s) psych::alpha(data[,s]) %>% alpha.ci)[3:1,] %>% 
    kable(caption = "Emotional regulation", col.names = timePoints)
  sapply(anger$probsol, function(s) psych::alpha(data[,s]) %>% alpha.ci)[3:1,] %>% 
    kable(caption = "Problem Solving", col.names = timePoints)


#' ## 2. Anger - Descriptive statistics (The first imputed data)
#+ echo=FALSE, warning=FALSE
# 2X. getting summaries (mean, std)
  tmp <- data.working.baf.long[[1]]
  anger.factor = c("unexp", "extexp", "emoreg", "probsol")
  anger.factor.full = c("Unexpressed anger", "External expression", "Emotion regulation", "Problem solving")

  # mean outcome
  paste(anger.factor, "~ month + trt") %>% 
    lapply(as.formula) %>% 
    lapply(function(x) aggregate(x, data=tmp, FUN = mean)) ->
    tmp.2 # mean outcomes by month and trt
  lapply(1:4, function(i) {
    result <- tmp.2[[i]]
    result$factor = anger.factor.full[i]
    names(result)[3] = "outcome"
    return(result)
    }) %>% 
    do.call(what=rbind) -> tmp.3
  
  # std
  paste(anger.factor, "~ month + trt") %>% 
    lapply(as.formula) %>% 
    lapply(function(x) aggregate(x, data=tmp, FUN = sd)) ->
    tmp.2se # mean outcomes by month and trt
  lapply(1:4, function(i) {
    result <- tmp.2se[[i]]
    result$factor = anger.factor.full[i]
    names(result)[3] = "stdDev"
    return(result)
  }) %>% 
    do.call(what=rbind) -> tmp.3se
  tmp.3 <- left_join(tmp.3, tmp.3se, by=c("month", "trt", "factor"))

# 2A. Mean plot
  tmp.3 %>% 
    ggplot(aes(month, outcome, col=factor)) + geom_line() + geom_point() +
    #geom_errorbar(aes(ymin=outcome-stdDev, ymax=outcome+stdDev), width=.1) +
    facet_grid(.~ifelse(trt, "Mindfulness Group", "Support Group")) +
    theme(legend.position="bottom")+
    scale_x_continuous(breaks = c( .5, 3, 6, 12))+
    ggtitle("Mean values of anger constructs by Treatment group and Follow-up")

# 2B. Table - Support group
  reshape(tmp.3[tmp.3$trt==0,-2], direction = "wide", timevar="month", idvar=c("factor")) %>%
    reshape(direction = "long", idvar = "factor", varying = list(2:3, 4:5, 6:7, 8:9, 10:11), 
            v.names = c("0", "0.46", "3", "6", "12"), timevar = "measure", times = c("mean", "stdDev")) -> tmp.4
  
  tmp.4 <- tmp.4[rep(1:4,each=2) + c(0,4),] # rearranging
  names(tmp.4)[3:7] <- timePoints
  tmp.4$factor[2*(1:4)] <- ""
  kable(tmp.4, row.names = FALSE, digits = 2,
        caption = "Descriptive statistics of anger, Support Group")

# 2C. Table - Intervention group
  reshape(tmp.3[tmp.3$trt==1,-2], direction = "wide", timevar="month", idvar=c("factor")) %>%
    reshape(direction = "long", idvar = "factor", varying = list(2:3, 4:5, 6:7, 8:9, 10:11), 
            v.names = c("0", "0.46", "3", "6", "12"), timevar = "measure", times = c("mean", "stdDev")) -> tmp.4
  
  tmp.4 <- tmp.4[rep(1:4,each=2) + c(0,4),] # rearranging
  names(tmp.4)[3:7] <- timePoints
  tmp.4$factor[2*(1:4)] <- ""
  kable(tmp.4, row.names = FALSE, digits = 2,
        caption = "Descriptive statistics of anger, Mindfulness Group")
  rm(tmp.4, tmp.3)

#' ## 3. FILE
#' FILE (Family Inventory of Life Events and Changes) questionnaire consists of 71 items 
#' and were measured about 1) recent events (during last 12 months) and 2) historical events (before last 12 months).
#+ echo=FALSE, warning=FALSE
  
  alpha.ci(psych::alpha(data.mice.FILE.br.list[[1]][,FILE[[2]]])) %>%
    kable(caption = "Cronbach's alpha for FILE")
  
  # mean outcome
  paste(c("FILE.base", "FILE.recent"), "~ trt") %>% 
    lapply(as.formula) %>% 
    lapply(function(x) aggregate(x, data=tmp[tmp$month==0,], FUN = mean)) ->
    tmp.5 # mean outcomes by month and trt
  lapply(1:2, function(i) {
    result <- tmp.5[[i]]
    result$factor = c("historical", "recent")[i]
    names(result)[2] = "outcome"
    return(result)
  }) %>% 
    do.call(what=rbind) -> tmp.5
  
  # std
  paste(c("FILE.base", "FILE.recent"), "~ trt") %>% 
    lapply(as.formula) %>% 
    lapply(function(x) aggregate(x, data=tmp[tmp$month==0,], FUN = sd)) ->
    tmp.5se # mean outcomes by month and trt
  lapply(1:2, function(i) {
    result <- tmp.5se[[i]]
    result$factor = c("historical", "recent")[i]
    names(result)[2] = "stdDev"
    return(result)
  }) %>% 
    do.call(what=rbind) -> tmp.5se
  tmp.5 <- left_join(tmp.5, tmp.5se, by=c("trt", "factor"))
  
  
  # 3A. Mean plot
  tmp.5 %>% 
    ggplot(aes(factor, outcome, col=ifelse(trt, "Mindfulness Group", "Support Group"))) + geom_line() + geom_point() +
    theme(legend.position="bottom") + xlab(NULL) + scale_color_discrete(name=NULL) +
    ggtitle("Mean values of summed FILE by Treatment group and recency")
  
  # 3B. Table
  tmp.6 <- tmp.5[c(1,3,2,4),c(1,3,2,4)]
  names(tmp.6) <- c("Group", "Recency", "Mean", "Standard deviation")
  tmp.6$Group %<>% ifelse("Mindfulness Group", "Support Group")
  
  kable(tmp.6, row.names = FALSE, digits = 2,
        caption = "Descriptive statistics of FILE")
  rm(tmp.6, tmp.5)
  
  
#'
#' ## 4. Anger FILE interaction - Descriptive statistics (The first imputed data)
#+ echo=FALSE, warning=FALSE
# 4X. getting summaries (mean, std)
  tmp <- data.working.baf.long[[1]]
  tmp$FILE.recent %>% summary %>% as.matrix %>%t %>% data.frame %>% kable (caption="Summary of FILE") # median = 10
  #tmp$FILE.base %>% summary # median = 13
  
  tmp$FILE.recent.bin <- ifelse(tmp$FILE.recent <=10,'Low FILE', 'High FILE')
  #tmp$FILE.recent.bin <- ifelse(tmp$FILE.base <=13,'Low FILE', 'High FILE')
  
  # mean outcome
  paste(anger.factor, "~ month + trt + FILE.recent.bin") %>% 
    lapply(as.formula) %>% 
    lapply(function(x) aggregate(x, data=tmp, FUN = mean)) ->
    tmp.2 # mean outcomes by month and trt
  lapply(1:4, function(i) {
    result <- tmp.2[[i]]
    result$factor = anger.factor.full[i]
    names(result)[4] = "outcome"
    return(result)
  }) %>% 
    do.call(what=rbind) -> tmp.3
  
  # std
  paste(anger.factor, "~ month + trt + FILE.recent.bin") %>% 
    lapply(as.formula) %>% 
    lapply(function(x) aggregate(x, data=tmp, FUN = sd)) ->
    tmp.2se # mean outcomes by month and trt
  lapply(1:4, function(i) {
    result <- tmp.2se[[i]]
    result$factor = anger.factor.full[i]
    names(result)[4] = "stdDev"
    return(result)
  }) %>% 
    do.call(what=rbind) -> tmp.3se
  tmp.3 <- left_join(tmp.3, tmp.3se, by=c("month", "trt", "factor", "FILE.recent.bin"))
  
  # 2A. Mean plot
  tmp.3 %>% 
    ggplot(aes(month, outcome, col=factor, shape = FILE.recent.bin)) + geom_line(aes(linetype = FILE.recent.bin)) + geom_point() +
    #geom_errorbar(aes(ymin=outcome-stdDev, ymax=outcome+stdDev), width=.1) +
    facet_grid(.~ifelse(trt, "Mindfulness Group", "Support Group")) +
    theme(legend.position="bottom")+
    scale_x_continuous(breaks = c( .5, 3, 6, 12))+
    ggtitle("Mean values of anger constructs by Treatment group, FILE level, and Follow-up")

#'  Remarkable interactions are shown in Emotion regulation (red) and Unexpressed Anger (purple).
#'  <br>
#'  
#'  [Emotion regulation] Minfulness intervention increases Emotion regulation for the distressed group.
#'  <br>
#'  
#'  [Unexpressed Anger] For those less-distressed people, no intervention increases the Unexpressed Anger.