# 0.0 library 
source("C0001-dataEntry.R")
source("F0101-variableSet.R")
source("C0002-coping_factor_analysis.R")
# 0.1 list of time points and variable categories
.timePoints
.categories

# 0.2 variable subsetting examples: "F0101-variableSet.R" required
if (FALSE) {
  varSet(category="coping", time = c("bl", "fu"), covariate = c("base", "demographic", "IBSS"))
  varSet(category="coping", time = "all", covariate = c("base", "demographic", "IBSS"))
}

# 1.0 imputation
# 1.2 imputation - coping
tmp <- varSet(category="coping", covariate = c("base", "demographic", "IBSS"))
m <- 25 # number of imputations
set.seed(100)
a <- Sys.time()
library(mice)
data.coping.imp <- mice(data.working[,tmp$variable %>% as.character], m=m)
data.coping.list <- lapply(1:m, function(i) complete(data.coping.imp, action = i))
Sys.time() - a #18.13 hours
saveRDS(data.coping.list, "../dataImputed/data.mice.coping.list.rds")
saveRDS(data.coping.imp, "../dataImputed/data.mice.coping.imp.rds")

data.mice.coping.list <- readRDS( "/Users/yiling/Desktop/UNC-Chapel Hill/Coursework/2018 Spring/841/Mindful/data.mice.coping.list.rds")
data.mice.coping.imp <- readRDS("/Users/yiling/Desktop/UNC-Chapel Hill/Coursework/2018 Spring/841/Mindful/data.mice.coping.imp.rds")


# 2.1 summary of coping

tmp.cat<- summarizedVar(data=data.mice.coping.list,var.name.matrix = CAT,measure = sum, var.name = "catastrophizing")
tmp.rs<- summarizedVar(data=data.mice.coping.list,var.name.matrix = RS,measure = sum, var.name = "rps")


#25 sets of imputed data (coping only)
data.working.coping <- do.call(cbind.list, list(tmp.cat,tmp.rs))
saveRDS(data.working.coping, "/Users/yiling/Desktop/UNC-Chapel Hill/Coursework/2018 Spring/841/Mindful/data.working.coping.rds")
# data.working.anger <- readRDS("../dataImputed/data.working.anger.rds")

# 3.0 25 sets of imputed data (basic data only)
data.working.base <- 
  lapply(data.mice.coping.list, function(dataset) {
    base.var <- c(var.base$variable %>% as.character, 
                  var.demo$variable %>% as.character) 
    ibss <- dataset[,var.IBSS$variable %>% as.character]
    names(ibss) <- paste ("IBSS", 1:5, sep=".")
    result <- cbind(id = rownames(dataset), dataset[,base.var], ibss)
    names(result) <- gsub("race_1", "race", names(result))
    return(result)
  })
saveRDS(data.working.base, "/Users/yiling/Desktop/UNC-Chapel Hill/Coursework/2018 Spring/841/Mindful/data.working.base.rds")
# data.working.base <- readRDS("../dataImputed/data.working.base.rds")



