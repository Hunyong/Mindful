# 0.0 library 
  source("C0001-dataEntry.R")
  source("F0101-variableSet.R")

# 0.1 list of time points and variable categories
  .timePoints
  .categories

# 0.2 variable subsetting examples: "F0101-variableSet.R" required
  if (FALSE) {
    varSet(category="comorbidity", time = "all", covariate = c("base", "demographic", "IBSS"))
    varSet(category="anger", time = c("bl", "fu"), covariate = c("base", "demographic", "IBSS"))
    varSet(category="anger", time = "all", covariate = c("base", "demographic", "IBSS"))
  }
  
# 1.0 imputation
  # 1.1 imputation - anger
  tmp <- varSet(category="anger", covariate = c("base", "demographic", "IBSS"))
  m <- 25 # number of imputations
  set.seed(100)
  a <- Sys.time()
  data.anger.imp <- mice(data.working[,tmp$variable], m=m)
  data.anger.list <- lapply(1:m, function(i) complete(data.anger.imp, action = i))
  Sys.time() - a #18.13 hours
  saveRDS(data.mice.anger.list, "../dataImputed/data.mice.anger.list.rds")
  saveRDS(data.mice.anger.imp, "../dataImputed/data.mice.anger.imp.rds")
  # data.mice.anger.list <- readRDS( "../dataImputed/data.mice.anger.list.rds")
  # data.mice.anger.imp <- readRDS("../dataImputed/data.mice.anger.imp.rds")
  
# 2.0 summary of anger
  tmp.unexp <- summarizedVar(data = data.mice.anger.list, var.name.matrix = anger$unexp, measure = mean, var.name = "unexp")
  tmp.extexp <- summarizedVar(data = data.mice.anger.list, var.name.matrix = anger$extexp, measure = mean, var.name = "extexp")
  tmp.emoreg <- summarizedVar(data = data.mice.anger.list, var.name.matrix = anger$emoreg, measure = mean, var.name = "emoreg")
  tmp.prob <- summarizedVar(data = data.mice.anger.list, var.name.matrix = anger$probsol, measure = mean, var.name = "probsol")
  
  #25 sets of imputed data (anger only)
  data.working.anger <- cbind.list(tmp.unexp, tmp.extexp)
  data.working.anger <- cbind.list(data.working.anger, tmp.emoreg)
  data.working.anger <- cbind.list(data.working.anger, tmp.prob)
  # or simply:  data.working.anger <- do.call(cbind.list, list(tmp.unexp, tmp.extexp, tmp.emoreg, tmp.prob))
  saveRDS(data.working.anger, "../dataImputed/data.working.anger.rds")
  # data.working.anger <- readRDS("../dataImputed/data.working.anger.rds")
  
# 3.0 25 sets of imputed data (basic data only)
  data.working.base <- 
    lapply(data.mice.anger.list, function(dataset) {
      base.var <- c(var.base$variable %>% as.character, 
                    var.demo$variable %>% as.character) 
      ibss <- dataset[,var.IBSS$variable %>% as.character]
      names(ibss) <- paste ("IBSS", 1:5, sep=".")
      result <- cbind(id = rownames(dataset), dataset[,base.var], ibss)
      names(result) <- gsub("race_1", "race", names(result))
      return(result)
    })
  saveRDS(data.working.base, "../dataImputed/data.working.base.rds")
  # data.working.base <- readRDS("../dataImputed/data.working.base.rds")
         
  # comorbidity dataset
  if (FALSE) {
    tmp <- varSet(category="comorbidity", covariate = c("base", "demographic", "IBSS"))
    tmp <- data.working[,tmp$variable]
    write.csv(tmp, "data.working.comorbidity.csv")
  }
  