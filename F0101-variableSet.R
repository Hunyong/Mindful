library(reshape2)

.timePoints = c('common', 'bl', 'fu', '3mo', '6mo', '12mo', 'common-hist')
.categories =  c('id', 'base', 'demographic', 'IBSS', 'anger', 'FILE', 'comorbidity', 'work productivity','coping')
# var.include$time %>% unique %>% paste(collapse ="', '")
# var.include$category %>% unique %>% paste(collapse ="', '")

# 1. variable subsetting
  # returns a set of variables of a specific category at a specific time point
  varSet <- function (category = "anger", time="all", covariate = NULL, var.data = var.include) {
    # screening
    if (time[1] == "all") {time = .timePoints}
    if (! all (time %in% .timePoints)) 
      stop (paste ("Each time variable should be one of ", paste(.timePoints, collapse = ", ")))
    if (! all (category %in% .categories)) 
      stop (paste ("Each category should be one of ", paste(.categories, collapse = ", ")))
    if (!is.null(covariate) & ! all (covariate %in% .categories)) 
      stop (paste ("Each covariate should be one of ", paste(.categories, collapse = ", ")))
    
    var.data = var.data[var.data$category %in% c(category, covariate),]
    var.data = var.data[var.data$time %in% c(time, "common", "common-history"),]
    
    return(var.data)
  }
  
    if (FALSE) {
      # example
      varSet(time = c("bl"), category="anger", covariate = c("id", "demographic"))
    }


# 2. summary function
  # summarize the complete data
  .summarizedVar <- function(data = data.working, var.name.matrix, measure = mean, var.name) {
    # var.name.matrix: each column is the time points
    # var.name: name of the construct
    result <- sapply(var.name.matrix, function(j) apply(data[ ,j], 1, measure))
    result <- as.data.frame(result)
    names(result) <- paste(var.name, 1:5, sep = ".")
    return(result)
  }
  # summarize the imputed datasets (list)
  summarizedVar <- function(data.list, var.name.matrix, measure = mean, var.name) {
    # data.list: m datasets
    lapply(data.list, function(x) {
      .summarizedVar(data = x, var.name.matrix = var.name.matrix, measure = mean, var.name = var.name)
    })
  }

    if (FALSE) {
      .summarizedVar(data = data.working, var.name.matrix = anger$probsol, measure = mean, var.name = "probsol")
      summarizedVar(data = data.anger.list, var.name.matrix = anger$probsol, measure = mean, var.name = "probsol")
    }
  
# 3. list-wise cbinding
  cbind.list <- function(list.x, list.y) {
    if (length(list.x) != length(list.y)) 
      stop ("The lengths of list.x and list.y differ.")
    
    lapply(1:(length(list.x)), function(i) {
      if (!identical(row.names(list.x[[i]]), row.names(list.y[[i]]))) 
        stop ("The row names of list.x and list.y differ.")
      cbind(list.x[[i]], list.y[[i]])
    })
  }
  
# 4. list-wise reshaping
  # reshaping a dataset (wide to long)
  .wide2long <- function(data, long.vars, time = 1:5, id = "id") {
    varying = do.call(c, lapply(long.vars, function(s) grep(s, names(data))))
    reshape(data, direction = "long", idvar = id, v.names = long.vars, varying = varying) 
  }
  # wide to long for list
  long.list <- function(data.list, long.vars, time = 1:5, id = "id") {
    lapply(data.list, function(x) .wide2long(data=x, long.vars=long.vars, time=time, id=id))
  }
  # example
  if (FALSE) {
    tmp <- long.list(data.working.anger2, long.vars = c("IBSS","unexp","extexp", "emoreg", "probsol"))
  }
  
  
  
  