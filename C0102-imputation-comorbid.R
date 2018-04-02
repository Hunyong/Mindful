# library
library(mice)
source("F0101-variableSet.R")

# 1.2 imputation - comorbidity
  tmp <- varSet(category="comorbidity", covariate = c("base", "demographic", "IBSS"))
  m <- 25 # number of imputations
  set.seed(100)
  a <- Sys.time()
  data.mice.comorbid.imp <- mice(data.working[,tmp$variable %>% as.character], m=m)
  data.mice.comorbid.list <- lapply(1:m, function(i) complete(data.FILE.imp, action = i))
  Sys.time() - a #18.13 hours
  saveRDS(data.mice.comorbid.imp, "../dataImputed/data.mice.comorbid.imp.rds")
  saveRDS(data.mice.comorbid.list, "../dataImputed/data.mice.comorbid.list.rds")
