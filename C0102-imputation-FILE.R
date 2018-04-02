# library
library(mice)

# 1.2 imputation - FILE
  tmp <- varSet(category="FILE", covariate = c("base", "demographic", "IBSS"))
  m <- 25 # number of imputations
  set.seed(100)
  a <- Sys.time()
  data.mice.FILE.imp <- mice(data.working[,tmp$variable %>% as.character], m=m)
  data.mice.FILE.list <- lapply(1:m, function(i) complete(data.mice.FILE.imp, action = i))
  Sys.time() - a #18.13 hours
  saveRDS(data.mice.FILE.imp, "../dataImputed/data.mice.FILE.imp.rds")
  saveRDS(data.mice.FILE.list, "../dataImputed/data.mice.FILE.list.rds")

# 2.0 summary of FILE
  tmp.FILE <- summarizedVar(data = data.mice.FILE.list, var.name.matrix = FILE, measure = sum, var.name = "FILE")
  
  