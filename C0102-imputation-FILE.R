# library
library(mice)

# 1.2 imputation - FILE
  tmp <- varSet(category="FILE", covariate = c("base", "demographic", "IBSS"))
  m <- 25 # number of imputations
  set.seed(100)
  a <- Sys.time()
  data.FILE.imp <- mice(data.working[,tmp$variable %>% as.character], m=m)
  data.FILE.list <- lapply(1:m, function(i) complete(data.FILE.imp, action = i))
  Sys.time() - a #18.13 hours
  saveRDS(data.FILE.imp, "../dataImputed/data.FILE.imp.rds")
  saveRDS(data.FILE.list, "../dataImputed/data.FILE.list.rds")
