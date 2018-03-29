.timePoints = c('common', 'bl', 'fu', '3mo', '6mo', '12mo', 'common-hist')
.categories =  c('id', 'base', 'demographic', 'IBSS', 'anger', 'FILE', 'comorbidity', 'work productivity')
# var.include$time %>% unique %>% paste(collapse ="', '")
# var.include$category %>% unique %>% paste(collapse ="', '")


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
