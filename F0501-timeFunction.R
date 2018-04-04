
# transfrom timepoints (1,2,3,4,5) to months
.visit2month <- function(x) {
  if (!(is.numeric(x) | is.integer(x))) stop ("x is not numeric.")
  if (any(as.double(x) != x)) stop ("x is not integers.")
  c(0, 0.46, 3, 6, 12) [x]
}


visit2month <- function(data.list, time = "time", month = "month") {
  lapply(data.list, function(x) {
    x[,time] <- .visit2month(x[,time])
    names(x)[which(names(x) == time)] = month
    return(x)
  })
}

# change (treatmgroup_nr = 1,2) to (trt = 0,1)
trt01 <- function(data.list, trt.var = "treatmgroup_nr", renamed.var = "trt") {
  lapply(data.list, function(x) {
    x[,trt.var] <- x[,trt.var] - 1
    names(x)[which(names(x) == trt.var)] = renamed.var
    return(x)
  })
}
