work.summary <- function(x){
  trtno <- x[,1]
  blwi <- x[,18]/10
  fuwi <- x[,24]/10
  mo3wi <- x[,30]/10
  mo6wi <- x[,36]/10
  mo12wi <- x[,42]/10
  blai <- x[,19]/10
  fuai <- x[,25]/10
  mo3ai <- x[,31]/10
  mo6ai <- x[,37]/10
  mo12ai <- x[,43]/10
  summary <- cbind(trtno,blwi,fuwi,mo3wi,mo6wi,mo12wi,blai,fuai,mo3ai,mo6ai,mo12ai)
  return(summary)
}

##Function for Computing sample mean difference and estimate of its variance
StatforTest <- function(X){
  changewi <- X[,12]
  changeai <- X[,13]
  trtno <- X[,1] ##1 is mindfulness group, 2 is support group
  changewi1 <- changewi[which(trtno == 1)]
  changewi2 <- changewi[which(trtno == 2)]
  widiff <- mean(changewi1) - mean(changewi2)
  wivar <- var(changewi1)/length(changewi1) + var(changewi2)/length(changewi2)##assuming unequal variances
  changeai1 <- changeai[which(trtno == 1)]
  changeai2 <- changeai[which(trtno == 2)]
  aidiff <- mean(changeai1) - mean(changeai2)
  aivar <- var(changeai1)/length(changeai1) + var(changeai2)/length(changeai2)##assuming unequal variances
  return(
    c(widiff,wivar,aidiff,aivar) 
  )
}

##Convert a matrix from wide to long format
widetolong <- function(X) {
  long <- NULL
  subject <- matrix(rep(0,25),ncol=5) ##initialize a matrix
  for (i in 1:nrow(X)){
    subject[,1] <- i ##"id" for each subject
    subject[,2] <- c(0,1.5,3,6,12) ##time in months
    subject[,3] <- 2 - X[i,1] ##Change the coding for treatment no.
    subject[,4] <- t(X[i,2:6])
    subject[,5] <- t(X[i,7:11])
    long <- rbind(long,subject)
  }
  colnames(long) <- c("ID","month","trtno","wi","ai")
  long <- as.data.frame(long)
  return(long)
}

##Store the estimates and standard errors from geeglm
gee <- function(X){
  fit1 <- geeglm(wi ~ trtno+month+trtno*month, data=X, id=ID)##assuming normality
  fit2 <- geeglm(ai ~ trtno+month+trtno*month, data=X, id=ID)##assuming normality
  wi <- summary(fit1)$coefficients[2:4,1:2]
  wi <- as.vector(as.matrix(t(wi)))
  ai <- summary(fit2)$coefficients[2:4,1:2]
  ai <- as.vector(as.matrix(t(ai)))
  output <- cbind(t(wi),t(ai))
  colnames(output) <- c("WI trt","WI trt se","WI mth","WI mth se","WI int","WI int se",
                        "AI trt","AI trt se","AI mth","AI mth se","AI int","AI int se")
  return(output)
}

##Store the estimates and standard errors from lmer
mixed <- function(X){
  fit1 <- lmer(wi~trtno*month+(month|ID), data=X)
  fit2 <- lmer(ai~trtno*month+(month|ID), data=X)
  wi <- summary(fit1)$coefficients[2:4,1:2]
  wi <- as.vector(as.matrix(t(wi)))
  ai <- summary(fit2)$coefficients[2:4,1:2]
  ai <- as.vector(as.matrix(t(ai)))
  output <- cbind(t(wi),t(ai))
  colnames(output) <- c("WI trt","WI trt se","WI mth","WI mth se","WI int","WI int se",
                        "AI trt","AI trt se","AI mth","AI mth se","AI int","AI int se")
  return(output)
}