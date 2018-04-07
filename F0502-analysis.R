Rubin <- function (mean.vec, var.vec, std.out = FALSE){
  if (length(mean.vec) != length(var.vec)) stop("Lengths do not match between mean.vec and var.vec")
  len = length(mean.vec)
  mean.overall = mean(mean.vec)
  var.between = var(mean.vec)
  var.within = mean(var.vec)
  var.overall = var.within + var.between * (1+1/len)
  z.val = mean.overall / sqrt(var.overall)
  p.val = 2* (1 - pnorm(abs(z.val)))
  result = c(Estimate = mean.overall, var = var.overall, z.val = z.val, p.val= p.val) 
  if (std.out) {result[2] = sqrt(var.overall); names(result)[2] = "Std.err" }
  return(result)
}
Rubin.list <- function(table.list, mean.name = "Estimate", std.name = "Std. Error") {
  parm.names = table.list[[1]] %>% row.names
  mean.mat = sapply(table.list, function(tab) tab[,mean.name]) # rows=param, col=imputation replicates
  var.mat = sapply(table.list, function(tab) tab[,std.name]^2)
  
  sapply(1:length(parm.names), function(i) {
    mean.vec = (mean.mat %>% t %>% data.frame)[,i]
    var.vec = (var.mat %>% t %>% data.frame)[,i]
    Rubin(mean.vec = mean.vec, var.vec = var.vec, std.out = TRUE)}
    ) %>% t -> result
  row.names(result) <- parm.names
  return(result)
}
if (FALSE) { # example
  Rubin(mean.vec = c(1,2,3,4,5), var.vec = c(1,3,2,2,3))
  Rubin(mean.vec = c(1,2,3,4,5), var.vec = c(1,3,2,2,3), std.out= T)
  lapply(1:3, function(i) (lm(unexp.3 - unexp.1 ~ trt , data = data.working.baf[[i]]) %>% summary %>% "[["("coefficients"))) %>%
    Rubin.list()
}