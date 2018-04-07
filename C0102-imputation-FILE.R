# 0.0 library 
source("C0001-dataEntry.R")
source("F0101-variableSet.R")
setwd("\\\\hematite.bios.unc.edu/users/hunycho/AppSense/Desktop/consulting-mindful/code2")
library(mice)
library(foreign)
library(dplyr); library(magrittr)
library(psych) # Chronbach's alpha

# 1.2 imputation - FILE
  tmp <- varSet(category="FILE", covariate = c("base", "demographic", "IBSS"))
  which(tmp$time =="common") #87 ~ 157
  tmp.b = tmp[1:86,]            # baseline FILE
  tmp.r = tmp[c(1:15, 87:157),] # recent FILE
  
  
  m <- 25 # number of imputations
  
  # baseline FILE (during last 12 months)
  set.seed(100)
  a <- Sys.time()
  # do imputation using whole sample (to borrow information from initial dropouts)
  data.mice.FILE.b.imp <- mice(data[,tmp.b$variable %>% as.character], m=m)
  Sys.time() - a #3.8 hours
  
  # See if there is non-imputed variables
  data.mice.FILE.b.imp$predictorMatrix %>% apply(2,sum) %>% "=="(0) %>% which
    # q57_b is still not imputed (multicolinear with q27_b)
    data[,c("q27_b", "q57_b")] # impute with 0 (mode)
    # 12 subjects out of 97 are missing.
  data.mice.FILE.b.list <- lapply(1:m, function(i) complete(data.mice.FILE.b.imp, action = i))
  
  for (i in 1:m) {
    tmp.b <- data.mice.FILE.b.list[[i]]
    tmp.b$q57_b[is.na(tmp.b$q57_b)] <- tmp.b$q27_b[is.na(tmp.b$q57_b)]
    data.mice.FILE.b.list[[i]] <- tmp.b
  }
  data.mice.FILE.b.list <- lapply(data.mice.FILE.b.list, function(x) x[sample.include,])
  saveRDS(data.mice.FILE.b.imp, "../dataImputed/data.mice.FILE.b.imp.rds")
  saveRDS(data.mice.FILE.b.list, "../dataImputed/data.mice.FILE.b.list.rds")

  
  # baseline FILE (before last 12 months)
  set.seed(100)
  a <- Sys.time()
  data.mice.FILE.r.imp <- mice(data[,tmp.r$variable %>% as.character], m=m)
  Sys.time() - a #3.8 hours
  
  # See if there is non-imputed variables
  data.mice.FILE.r.imp$predictorMatrix %>% apply(2,sum) %>% "=="(0) %>% which
  # q27 and q68 are still not imputed (multicolinear with q24(almost) and q67 respectively)
  # 3 and 5 subjects are missing.
  data[c("q12", "q23", "q24", "q27", "q57", "q67", "q68")] # impute with with q67 for q68, and 0(mode) for q27.
  # Not clear which one to impute for q27, but only 3 subjects are missing and mostly they are 0. so mode imputation is reasonable.
  
  data.mice.FILE.r.list <- lapply(1:m, function(i) complete(data.mice.FILE.r.imp, action = i))
  for (i in 1:m) {
    tmp.r <- data.mice.FILE.r.list[[i]]
    tmp.r$q27[is.na(tmp.r$q27)] <- 0
    tmp.r$q68[is.na(tmp.r$q68)] <- tmp.r$q67[is.na(tmp.r$q68)]
    data.mice.FILE.r.list[[i]] <- tmp.r
  }
  data.mice.FILE.r.list <- lapply(data.mice.FILE.r.list, function(x) x[sample.include,])
  saveRDS(data.mice.FILE.r.imp, "../dataImputed/data.mice.FILE.r.imp.rds")
  saveRDS(data.mice.FILE.r.list, "../dataImputed/data.mice.FILE.r.list.rds")
  
  if (FALSE) {
    data.mice.FILE.b.imp <- readRDS("../dataImputed/data.mice.FILE.b.imp.rds")
    data.mice.FILE.r.imp <- readRDS("../dataImputed/data.mice.FILE.r.imp.rds")
    data.mice.FILE.b.list <- readRDS("../dataImputed/data.mice.FILE.b.list.rds")
    data.mice.FILE.r.list <- readRDS("../dataImputed/data.mice.FILE.r.list.rds")
  }
  
  
  
# 2.0 summary of FILE
  data.mice.FILE.br.list <- cbind.list(data.mice.FILE.b.list, data.mice.FILE.r.list)
  data.mice.FILE.br.list <- lapply(data.mice.FILE.br.list, function(x) x[,-(87:101)])
  
  # Adding id for integrity
  for (i in 1:m) data.mice.FILE.br.list[[i]] = data.frame(id = (data$id %>% as.character)[sample.include], 
                                                          data.mice.FILE.br.list[[i]])
  for (i in 1:m) write.csv(data.mice.FILE.br.list[[i]],paste0("../dataImputedFILE/FILE_imputed_", i, ".csv"))
  
  View(data.label)  # description
  data.label[416,]  # q69
  
  tmp.FILE.bl <- lapply(data.mice.FILE.br.list, function(x)
    data.frame(FILE.base =  x[,FILE$bl %>% as.character] %>% apply(1, sum))
    )
  tmp.FILE.recent <- lapply(data.mice.FILE.br.list, function(x)
    data.frame(FILE.recent =  x[,FILE$recent %>% as.character] %>% apply(1, sum))
    )
  tmp.FILE.child <- lapply(data.mice.FILE.br.list, function(x) x[,"q69_b", drop=FALSE])
  
  # combine
  data.working.FILE <- cbind.list(tmp.FILE.bl, tmp.FILE.recent)
  data.working.FILE <- cbind.list(data.working.FILE, tmp.FILE.child)
  
  saveRDS(data.working.FILE, "../dataImputed/data.working.FILE.rds")
  # data.working.FILE <- readRDS("../dataImputed/data.working.FILE.rds")
  
  