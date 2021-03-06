rm(list=ls())
library(foreign)
library(dplyr); library(magrittr)
library(psych) # Chronbach's alpha
source("F0001-basic.R")

#### 0. Data Step ##############################################################################
#### 0.0 data entry ############################################################################
data = read.spss("../data/ENTIRE_dataset_MIBS_2013_04_12.sav", to.data.frame=TRUE)
data$id %>% as.character %>% gsub(pattern=" ",replacement="") -> data$id
dim(data)  # 97 x 1608

#### 0.1 variables list-up ############################################################################
  # Getting data lables
    attr(data,"variable.labels") -> data.label
    data.label <- data.frame(name = names(data.label), label = data.label)
    data.label$name %<>% as.character
    data.label$label %<>% as.character
    rownames(data.label) <- NULL
    
    data.label$name[1:9] # main variables
    c('id', 'treatm_group', 'treatmgroup_nr', 'cohort', 'dropouts', 'treatm_sessions', 'treated', 'PRE_TREATMENT_QUEST', 'date')
  
  # what type of variables are there?  
    tmp <- gsub("[0-9]","",data.label$name[-(1:9)]) # numbers removed
    tmp <- gsub("\\_.*","",tmp)               # anything after underbar removed
    table(tmp)                                # results
    tmp %>% unique %>% length                 # 117 unique categories + main category

#### 0.2 data scoping ############################################################################
  ## 0.2.0 id, basic

    var.id = data.frame(variable = "id", category = "id", time = "common")
    var.base = data.frame(variable = c('treatmgroup_nr', 'cohort', 'treatm_sessions'), 
                          category = "base", 
                          time = "common") ## include basic variables
    var.demo = data.frame(variable = c( "age", "gender", "race_1", "marital", "income", "education", "profession"),
                          category = "demographic",
                          time = "common")
    
    #factors into ordered factors
    data$income %<>% ordered
    data$income %>% levels # make sure it's properly ordered
    
    data$education %<>% ordered
    data$education %>% levels # make sure it's properly ordered
    
    #numeric into factors
    data$treatm_sessions %<>% as.character %<>% as.factor

  ## 0.2.1 IBSS
    extractVar("IBS_severit")
    extractVar("IBS_severit.*_ITT")
    var.IBSS = data.frame(variable = c('IBS_severity_bl', 'IBS_severity_fu', 'IBS_severity_3mo_fu', 'IBS_severity_6mo', 'IBS_severity_12mo'),
                          category = "IBSS",
                          time = c("bl", "fu", "3mo", "6mo", "12mo"))
                      
  # "...ITT" in variable names is not actually ITT, but stands for LVCF (last value carry forward).

  #### 0.2.2 anger ############################################################################
    # reverse coding
      data$bax_1.re <- 5 - data$bax_1 %>% as.numeric  #will be reversed next time
      data$ax_1_1.re <- 5 - data$ax_1_1 %>% as.numeric
      data$ax1_3mo.re <- 5 - data$ax1_3mo %>% as.numeric
      data$ax_1_6mo.re <- 5 - data$ax_1_6mo %>% as.numeric
      data$ax1_12mo.re <- 5 - data$ax1_12mo %>% as.numeric
  
    # anger variable - construct mapping
      anger <- list(unexp = NA, extexp = NA, emoreg = NA, probsol = NA)
        anger$unexp <- data.frame(bl = paste0("bax_", c(39, 40, 25, 3, 15, 10, 18, 22, 6)),
                                  fu = paste0("ax_", c(391, 401, 251, "3_1", 151, 101, 181, 221, "6_1")),
                                  mo3 = paste0("ax", c(39, 40, 25, 3, 15, 10, 18, 22, 6), "_3mo"),
                                  mo6 = paste0("ax_", c(39, 40, 25, 3, 15, 10, 18, 22, 6), "_6mo"),
                                  mo12 = paste0("ax", c(39, 40, 25, 3, 15, 10, 18, 22, 6), "_12mo"),
                                  stringsAsFactors=FALSE)
        anger$extexp <- data.frame(bl = paste0("bax_", c(19, 2, 37, 17, 4, 9, "1.re")),
                                   fu = paste0("ax_", c(191, "2_1", 371, 171, "4_1", "9_1", "1_1.re")),
                                   mo3 = paste0("ax", c(19, 2, 37, 17, 4, 9, 1), c(rep("_3mo",6), "_3mo.re")),
                                   mo6 = paste0("ax_", c(19, 2, 37, 17, 4, 9, 1), c(rep("_6mo",6), "_6mo.re")),
                                   mo12 = paste0("ax", c(19, 2, 37, 17, 4, 9, 1), c(rep("_12mo",6), "_12mo.re")),
                                   stringsAsFactors=FALSE)
        anger$emoreg <- data.frame(bl = paste0("bax_", c(27, 34, 21, 26, 28)),
                                   fu = paste0("ax_", c(271, 341, 211, 261, 281)),
                                   mo3 = paste0("ax", c(27, 34, 21, 26, 28), "_3mo"),
                                   mo6 = paste0("ax_", c(27, 34, 21, 26, 28), "_6mo"),
                                   mo12 = paste0("ax", c(27, 34, 21, 26, 28), "_12mo"),
                                   stringsAsFactors=FALSE)
        anger$probsol <- data.frame(bl = paste0("bax_", c(31, 38, 20, 35, 36)),
                                    fu = paste0("ax_", c(311, 381, 201, 351, 361)),
                                    mo3 = paste0("ax", c(31, 38, 20, 35, 36), "_3mo"),
                                    mo6 = paste0("ax_", c(31, 38, 20, 35, 36), "_6mo"),
                                    mo12 = paste0("ax", c(31, 38, 20, 35, 36), "_12mo"),
                                    stringsAsFactors=FALSE)
  
      # anger variables in a vector
        anger.vector <- do.call(c, do.call(c, anger)) %>% as.vector
        var.anger <- data.frame(variable = anger.vector %>% as.character,
                                category = "anger",
                                time = do.call(c, sapply(c(9,7,5,5), function(s) rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=s))))
      # factor into numeric
        for (i in anger.vector) data[, i] %<>% as.numeric  
      
    # confidence intervals of cronbach's alpha
      sapply(anger$unexp, function(s) psych::alpha(data[,s]) %>% alpha.ci) # unexp
      sapply(anger$extexp, function(s) psych::alpha(data[,s]) %>% alpha.ci) # extexp
      sapply(anger$emoreg, function(s) psych::alpha(data[,s]) %>% alpha.ci) # emoreg
      sapply(anger$probsol, function(s) psych::alpha(data[,s]) %>% alpha.ci) # probsol
  
  
  ###############################################################################################

  ## 0.2.3 FILE (family inventory of life events and changes): something like trauma  
    FILE <- extractVar("^q[0-9]")  #^: begins with, [0-9]: numbers
    tmp <- which(grepl("_b", FILE[,2]))
    FILE <- list(bl = FILE[tmp, 2], recent = FILE[-tmp, 2])
      # bl: during last 12 months, hist: before last 12 months.
      order(gsub("q", "", gsub("_b", "", FILE$bl)) %>% as.numeric) == 1:71 # check if they are in the order
      order(gsub("q", "", FILE$recent) %>% as.numeric) == 1:71 # check if they are in the order
    FILE.vector = do.call(c, FILE)
    var.FILE = data.frame(variable = FILE.vector %>% as.character, 
                          category = "FILE", 
                          time = rep(c("common-hist", "common"), each=71))
    
    # factor into numeric
    for (i in FILE.vector) data[, i] %<>% as.numeric  # yes = 1, no = 2
    data[,FILE.vector] = 2 - data[,FILE.vector]       # recoding: yes = 1, no = 0
    
    # summed scores
    if (FALSE) { ##### !!!!This should be moved to after imputation!!!!! #####
      FILE.sum <- sapply(FILE, function(i) apply(data[ ,i], 1, sum)) %>% as.data.frame
      names(FILE.sum) <- c("FILE.bl", "FILE.hist")
    }
   
  ## 0.2.4 comorbidity
    names(data)[c(187,197)] # typo
    names(data)[c(187,197)] <- c("brpsq3", "brpsq13")
    
    comorbidity = data.frame(bl = c(paste0("brpsq", 1:26), "RPSQsom"),
                          fu = c(paste0("rpsq", 1:26), "RPSQSom_fu"),
                          mo3 = c(paste0("rpsq", 1:26, "_3mo"), "RPSQSom_3mo"),
                          mo6 = c(paste0("rpsq", 1:26, "_6mo"), "RPSQSom_6mo"),
                          mo12 = c(paste0("rpsq", 1:26, "_12mo"), "RPSQSom_12mo"),
                          stringsAsFactors=FALSE)
    
    var.comorbid = data.frame(variable =  comorbidity %>% as.matrix %>% as.vector %>% as.character,
                              category = "comorbidity",
                              time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=27))
    
  ## 0.2.5 work productivity
    work <- data.frame(bl = c("bwpai_q11","bwpai_q12","bhrs_m1","bhrs_m2","bhours1","bwpai_q5","bwpai_q6"),
                       fu = c("wpai_q11","wpai_q12","hrs_m1","hrs_m2","hours1","wpai_q5","wpai_q6"),
                       mo3 = paste(c("wpai_q11","wpai_q12","hrs_m1","hrs_m2","hours1","wpai_q5","wpai_q6"),"_3mo",sep=""),
                       mo6 = paste(c("wpai_q11","wpai_q12","hrs_m1","hrs_m2","hours1","wpai_q5","wpai_q6"),"_6mo",sep=""),
                       mo12 = paste(c("wpai_q11","wpai_q12","hrs_m1","hrs_m2","hours1","wpai_q5","wpai_q6"),"_12mo",sep=""),
                       stringsAsFactors=FALSE)
    
    var.work = data.frame(variable =  work %>% as.matrix %>% as.vector %>% as.character,
                          category = "work productivity",
                          time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=7))
    
    
  ## 0.2.6 coping strategy
    extractVar("csq")#check the name of coping variables
    
    coping = data.frame(bl = paste0("bcsq_", 1:48),
                        fu = paste0("csq_", 1:48),
                        mo3 = paste0("csq_", 1:48, "_3mo"),
                        mo6 = paste0("csq_", 1:48, "_6mo"),
                        mo12 = paste0("csq_", 1:48, "_12mo"),
                        stringsAsFactors=FALSE)
    
    var.coping = data.frame(variable =  as.vector(as.matrix(coping)),
                            category = "coping",
                            time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=48))
    
    # factor into numeric (adjust the answer levels to the same coding)
    for (i in var.coping$variable %>% as.character) data[, i] %<>% as.numeric  
    
    # numeric into factor
    for (i in var.coping$variable %>% as.character) data[, i] %<>% as.factor
    
  ## var.include, sample.include ################################################################
    var.include = rbind(var.id, var.base)       # id and base(trt,...)
    var.include = rbind(var.include, var.demo)  # adding demographic variables
    var.include = rbind(var.include, var.IBSS)  # adding IBSS variables
    var.include = rbind(var.include, var.anger) # adding anger variables
    var.include = rbind(var.include, var.FILE)  # adding FILE variables
    var.include = rbind(var.include, var.comorbid)  # adding comorbidity variables
    var.include = rbind(var.include, var.work)  # adding work variables
    var.include = rbind(var.include, var.coping)  # adding coping variables
    
    sample.include = which(!is.na(data$treatmgroup_nr))  # subject numbers of both arms
  ###############################################################################################

  data.working = data[sample.include, var.include$variable %>% as.character]
  #saveRDS(data.working, "data.working.rds")
  #saveRDS(data, "data.raw.rds")
  #saveRDS(anger, "anger.rds")
  #saveRDS(FILE, "FILE.rds")
  #saveRDS(sample.include, "sample.include.rds")
    