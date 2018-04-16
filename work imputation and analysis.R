library(foreign)
library(mice)
library(geepack)
library(CorrMixed)
library(lme4)
library(grid)
library(gridExtra)
library(gtable)
source("/Users/tangxin/Desktop/Coursework/2018 Spring/841/proj 1/extractVar.R")
source("/Users/tangxin/Desktop/Coursework/2018 Spring/841/proj 1/work.summary.R")
source("/Users/tangxin/Desktop/Coursework/2018 Spring/841/proj 1/StatsForTTest.R")
source("/Users/tangxin/Desktop/Coursework/2018 Spring/841/proj 1/wide2long.R")
source("/Users/tangxin/Desktop/Coursework/2018 Spring/841/proj 1/gee.R")
source("/Users/tangxin/Desktop/Coursework/2018 Spring/841/proj 1/mixed.R")

data = read.spss("/Users/tangxin/Desktop/Coursework/2018 Spring/841/proj 1/ENTIRE_dataset_MIBS_2013_04_12.sav", to.data.frame=TRUE)

work <- data.frame(bl = c("bwpai_q11","bwpai_q12","bhrs_m1","bhrs_m2","bhours1","bwpai_q5","bwpai_q6"),
                   fu = c("wpai_q11","wpai_q12","hrs_m1","hrs_m2","hours1","wpai_q5","wpai_q6"),
                   mo3 = paste(c("wpai_q11","wpai_q12","hrs_m1","hrs_m2","hours1","wpai_q5","wpai_q6"),"_3mo",sep=""),
                   mo6 = paste(c("wpai_q11","wpai_q12","hrs_m1","hrs_m2","hours1","wpai_q5","wpai_q6"),"_6mo",sep=""),
                   mo12 = paste(c("wpai_q11","wpai_q12","hrs_m1","hrs_m2","hours1","wpai_q5","wpai_q6"),"_12mo",sep=""),
                   stringsAsFactors=FALSE)

var.work = data.frame(variable =  as.vector(as.matrix(work)),
                          category = "work productivity",
                          time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=7))

##Columns for wpai at each time point:178-184,593-599,846-852,1217-1223,1489-1495##

norandom <- which(is.na(data[,3])) ##3rd column is treatment no., exclude subjects without treatment assigment
bldata <- data[-norandom,178:184] ##wpai at baseline
##Correct Typos
blh1 <- as.numeric(as.character(bldata[,3]))
blh2 <- as.numeric(as.character(bldata[,4]))
blh3 <- as.numeric(as.character(bldata[,5]))
blh3[7] <- 40
blh3[49] <- 40
blh3[54] <- 16
blh3[60] <- 40
bldata <- data.frame(bldata[,2],blh1,blh2,blh3,
                     as.numeric(as.character(bldata[,6])),as.numeric(as.character(bldata[,7])))
colnames(bldata) <- c("bl.q1","bl.q2","bl.q3","bl.q4","bl.q5","bl.q6")##Baseline data ready for use
bldrop <- c("3") ##subjects who "drop out"
bldropdata <- bldata[bldrop,]
blobsdata <- bldata[! rownames(bldata) %in% bldrop,] ##partially missing or complete questionaires
##blimpdata will be the imputed blobsdata using MICE
##For summary measure, visit http://www.reillyassociates.net/WPAI_Scoring.html and see WPAI:SHP, 4 possible scores
##please use 4th summary for all subjects and 3rd summary for those who always work

fudata <- data[-norandom,593:599]
##Correct Typos
fuh1 <- as.numeric(as.character(fudata[,3]))
fuh2 <- as.numeric(as.character(fudata[,4]))
fuh3 <- as.numeric(as.character(fudata[,5]))
fuh3[12] <- 45
fuh3[49] <- 25
fudata <- data.frame(fudata[,2],fuh1,fuh2,fuh3,
                     as.numeric(as.character(fudata[,6])),as.numeric(as.character(fudata[,7])))
colnames(fudata) <- c("fu.q1","fu.q2","fu.q3","fu.q4","fu.q5","fu.q6")##follow-up data ready for use
fudrop <- c("13","22","33","36","50","56","59","60","69","89","90")
fudropdata <- fudata[fudrop,]
fuobsdata <- fudata[! rownames(fudata) %in% fudrop,]


mo3data <- data[-norandom,846:852]
##Correct Typos
mo3h1 <- as.numeric(as.character(mo3data[,3]))
mo3h2 <- as.numeric(as.character(mo3data[,4]))
mo3h3 <- as.numeric(as.character(mo3data[,5]))
mo3h3[58] <- 65
mo3data <- data.frame(mo3data[,2],mo3h1,mo3h2,mo3h3,
                      as.numeric(as.character(mo3data[,6])),as.numeric(as.character(mo3data[,7])))
colnames(mo3data)<-c("mo3.q1","mo3.q2","mo3.q3","mo3.q4","mo3.q5","mo3.q6")##3mo data ready for use
mo3drop <- c("22","25","35","36","50","56","59","60","61","69","77","83","89","90")
mo3dropdata <- mo3data[mo3drop,]
mo3obsdata <- mo3data[! rownames(mo3data) %in% mo3drop,]


mo6data <- data[-norandom,1217:1223]
##Correct Typos
mo6h1 <- as.numeric(as.character(mo6data[,3]))
mo6h2 <- as.numeric(as.character(mo6data[,4]))
mo6h3 <- as.numeric(as.character(mo6data[,5]))
mo6h3[30] <- 40
mo6h3[49] <- 40
mo6data <- data.frame(mo6data[,2],mo6h1,mo6h2,mo6h3,
                      as.numeric(as.character(mo6data[,6])),as.numeric(as.character(mo6data[,7]))) 
colnames(mo6data) <- c("mo6.q1","mo6.q2","mo6.q3","mo6.q4","mo6.q5","mo6.q6")##6mo data ready for use
mo6drop <- c("5","7","12","13","19","22","25","33","35","36","40","50","56","60","69","70","75","77","83","90","97")
mo6dropdata <- mo6data[mo6drop,]
mo6obsdata <- mo6data[! rownames(mo6data) %in% mo6drop,]


mo12data <- data[-norandom,1489:1495]
##Correct Typos
mo12h1 <- as.numeric(as.character(mo12data[,3]))
mo12h2 <- as.numeric(as.character(mo12data[,4]))
mo12h3 <- as.numeric(as.character(mo12data[,5]))
mo12h3[61] <- 7.5
mo12work <- as.numeric(as.character(mo12data[,2]))
mo12work[13] <- 1
mo12work[12] <- 0
mo12work[18] <- 0
mo12work[19:20] <- 1
mo12work[22] <- 1
mo12work[29] <- 1
mo12work[33] <- 1
mo12work[40] <- 0
mo12work[55] <- 0
mo12work <- as.factor(mo12work)
mo12data <- data.frame(mo12work,mo12h1,mo12h2,mo12h3,
                       as.numeric(as.character(mo12data[,6])),as.numeric(as.character(mo12data[,7])))
colnames(mo12data) <- c("mo12.q1","mo12.q2","mo12.q3","mo12.q4","mo12.q5","mo12.q6")##12mo data ready for use
mo12drop <- c("2","21","22","32","33","34","35","36","40","41","50","54","59","60","62","66","69","70","77","83","84",
              "89","92")
mo12dropdata <- mo12data[mo12drop,]
mo12obsdata <- mo12data[! rownames(mo12data) %in% mo12drop,]

wpai <- cbind(bldata,fudata,mo3data,mo6data,mo12data) 

##Supplementary variables for MI
supp <- cbind(data$treatmgroup_nr, data$age, data$gender, data$race_1, data$marital, data$income,
              data$education, data$profession,
              data$IBS_severity_bl, data$IBS_severity_fu, data$IBS_severity_3mo_fu, data$IBS_severity_6mo, data$IBS_severity_12mo)
supp <- supp[-norandom,] ##delete subject without treatment No., all columns are numeric for supp
colnames(supp) <- c("trtno","age","gender","race","marital","income","education","profession",
                    "bl.ibss","fu.ibss","mo3.ibss","mo6.ibss","mo12.ibss")

workdata <- cbind(supp,wpai) ##ready for imputation


data.work.imp <- mice(workdata, m=25)
data.work.list <- lapply(1:25, function(i) complete(data.work.imp, action = i))
saveRDS(data.work.list, "/Users/tangxin/Desktop/Coursework/2018 Spring/841/proj 1/data.work.list.rds")
saveRDS(data.work.imp, "/Users/tangxin/Desktop/Coursework/2018 Spring/841/proj 1/data.work.imp.rds")
#data.work.list <- readRDS("/Users/tangxin/Desktop/Coursework/2018 Spring/841/proj 1/data.work.list.rds")



data.work.sum <- lapply(data.work.list,work.summary) ##the dataset is still wide here



##T-test for change from baseline to 3 months between 2 groups##
for (i in 1:25){
  changewi <- data.work.sum[[i]][,4] - data.work.sum[[i]][,2]
  data.work.sum[[i]] <- cbind(data.work.sum[[i]],changewi)
  changeai <- data.work.sum[[i]][,9] - data.work.sum[[i]][,7]
  data.work.sum[[i]] <- cbind(data.work.sum[[i]],changeai)
} ##compute the changes for each imputed dataset

estimate <- lapply(data.work.sum,StatforTest)##obtain mean difference and variance estimate
estimate <- do.call(rbind,estimate) ##convert the list to a 25*4 matrix

widiff <- mean(estimate[,1])
wivar <- mean(estimate[,2]) + (1+1/25)*var(estimate[,1]) ##Combined estimates for mean diff and variances
aidiff <- mean(estimate[,3])
aivar <- mean(estimate[,4]) + (1+1/25)*var(estimate[,3]) ##Combined estimates for mean diff and variances
wiT <- widiff/sqrt(wivar) ## T test for Work Impairment
aiT <- aidiff/sqrt(aivar) ## T test for Activity Impairment
wiP <- 1-(pt(abs(wiT),1) - pt(-abs(wiT),1))##Two sided test
aiP <- 1-(pt(abs(aiT),1) - pt(-abs(aiT),1))##Two sided test

wiT <- cbind(widiff,sqrt(wivar),wiT,wiP)
wiT <- round(wiT, digits = 5)
colnames(wiT) <- c("Estimate","Std.err","Test","PValue")
rownames(wiT) <- "Difference in Mean Change"
wiT <- as.data.frame(wiT)
aiT <- cbind(aidiff,sqrt(aivar),aiT,aiP)
aiT <- round(aiT, digits = 5)
colnames(aiT) <- c("Estimate","Std.err","Test","PValue")
rownames(aiT) <- "Difference in Mean Change"
aiT <- as.data.frame(aiT)

tt <- ttheme_minimal()

g <- tableGrob(wiT, theme=tt)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
title <- textGrob("Table 3.1: T Test for Change in Work Impairment", gp = gpar(fontsize = 10))
padding <- unit(0.5,"line")
g <- gtable_add_rows(
  g, heights = grobHeight(title) + padding, pos = 0
)
g <- gtable_add_grob(
  g, list(title),
  t = 1, l = 1, r = ncol(g))
grid.draw(g)


g <- tableGrob(aiT, theme=tt)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
title <- textGrob("Table 3.2: T Test for Change in Activity Impairment", gp = gpar(fontsize = 10))
padding <- unit(0.5,"line")
g <- gtable_add_rows(
  g, heights = grobHeight(title) + padding, pos = 0
)
g <- gtable_add_grob(
  g, list(title),
  t = 1, l = 1, r = ncol(g))
grid.draw(g)


##Fitting marginal model for longitudinal data (trt, time, trt-time interaction)
data.work.list <- readRDS("/Users/tangxin/Desktop/Coursework/2018 Spring/841/proj 1/data.work.list.rds")
data.work.sum <- lapply(data.work.list,work.summary) ##the dataset is still wide here
data.work.long <- lapply(data.work.sum,widetolong) ##convert to long format
output <- lapply(data.work.long,gee)
output <- do.call(rbind,output)
witrt <- mean(output[,1])
witrtse <- sqrt(mean((output[,2])^2) + (1+1/25)*var(output[,1]))
wimth <- mean(output[,3])
wimthse <- sqrt(mean((output[,4])^2) + (1+1/25)*var(output[,3]))
wiint <- mean(output[,5])
wiintse <- sqrt(mean((output[,6])^2) + (1+1/25)*var(output[,5]))
wi <- cbind(c(witrt,wimth,wiint),c(witrtse,wimthse,wiintse))
rownames(wi) <- c("trtno","month","trtno:month")
colnames(wi) <- c("Estimate","Std.err")
Wald <- (wi[,1]/wi[,2])^2
PValue <- 1 - pchisq(Wald,1)
wigee <- cbind(wi,Wald,PValue)
wigee <- round(wigee, digits = 5)##Summary table for combined result of geeglm for Work Impairment

g <- tableGrob(wigee, theme=tt)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
title <- textGrob("Table 3.3: Marginal Model with Work Impairment Response", gp = gpar(fontsize = 10))
padding <- unit(0.5,"line")
g <- gtable_add_rows(
  g, heights = grobHeight(title) + padding, pos = 0
)
g <- gtable_add_grob(
  g, list(title),
  t = 1, l = 1, r = ncol(g))
grid.draw(g)

aitrt <- mean(output[,7])
aitrtse <- sqrt(mean((output[,8])^2) + (1+1/25)*var(output[,7]))
aimth <- mean(output[,9])
aimthse <- sqrt(mean((output[,10])^2) + (1+1/25)*var(output[,9]))
aiint <- mean(output[,11])
aiintse <- sqrt(mean((output[,12])^2) + (1+1/25)*var(output[,11]))
ai <- cbind(c(aitrt,aimth,aiint),c(aitrtse,aimthse,aiintse))
rownames(ai) <- c("trtno","month","trtno:month")
colnames(ai) <- c("Estimate","Std.err")
Wald <- (ai[,1]/ai[,2])^2
PValue <- 1 - pchisq(Wald,1)
aigee <- cbind(ai,Wald,PValue)
aigee <- round(aigee, digits = 5)##Summary table for combined result of geeglm for Activity Impairment

g <- tableGrob(aigee, theme=tt)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
title <- textGrob("Table 3.4: Marginal Model with Activity Impairment Response", gp = gpar(fontsize = 10))
padding <- unit(0.5,"line")
g <- gtable_add_rows(
  g, heights = grobHeight(title) + padding, pos = 0
)
g <- gtable_add_grob(
  g, list(title),
  t = 1, l = 1, r = ncol(g))
grid.draw(g)

##Descriptive Plots for individual trajectories (First Imputation)
Spaghetti.Plot(data.work.long[[1]], Outcome = wi, Time = month, Id = ID, xlab = "Months")
mtext("Plot 3.1: Work Impairment Trajectories for Total Sample")
Spaghetti.Plot(data.work.long[[1]][which(data.work.long[[1]][,3] == 0),], Outcome = wi, Time = month, Id = ID,
               xlab = "Months")
mtext("Plot 3.2: Work Impairment Trajectories for Support Group")
Spaghetti.Plot(data.work.long[[1]][which(data.work.long[[1]][,3] == 1),], Outcome = wi, Time = month, Id = ID,
               xlab = "Months")
mtext("Plot 3.3: Work Impairment Trajectories for Mindfulness Training Group")

Spaghetti.Plot(data.work.long[[1]], Outcome = ai, Time = month, Id = ID,xlab = "Months")
mtext("Plot 3.4: Activity Impairment Trajectories for Total Sample")
Spaghetti.Plot(data.work.long[[1]][which(data.work.long[[1]][,3] == 0),], Outcome = ai, Time = month, Id = ID,
               xlab = "Months")
mtext("Plot 3.5: Activity Impairment Trajectories for Support Group")
Spaghetti.Plot(data.work.long[[1]][which(data.work.long[[1]][,3] == 1),], Outcome = ai, Time = month, Id = ID,
               xlab = "Months")
mtext("Plot 3.6: Activity Impairment Trajectories for Mindfulness Training Group")


##Fitting mixed model for longitudinal data (trtno, time, interaction are fixed, random intercept to account
##for correlation between observations within same subject)
##fit <- lmer(wi~trtno*month+(1|ID), data=data.work.long[[1]])
##summary(fit)$coefficient, need to combine all 25 datasets, same as geeglm results
output <- lapply(data.work.long,mixed)
output <- do.call(rbind,output)
witrt <- mean(output[,1])
witrtse <- sqrt(mean((output[,2])^2) + (1+1/25)*var(output[,1]))
wimth <- mean(output[,3])
wimthse <- sqrt(mean((output[,4])^2) + (1+1/25)*var(output[,3]))
wiint <- mean(output[,5])
wiintse <- sqrt(mean((output[,6])^2) + (1+1/25)*var(output[,5]))
wi <- cbind(c(witrt,wimth,wiint),c(witrtse,wimthse,wiintse))
rownames(wi) <- c("trtno","month","trtno:month")
colnames(wi) <- c("Estimate","Std.err")
Wald <- (wi[,1]/wi[,2])^2
PValue <- 1 - pchisq(Wald,1)
wimixed <- cbind(wi,Wald,PValue)##Summary table for combined result of geeglm for Work Impairment

aitrt <- mean(output[,7])
aitrtse <- sqrt(mean((output[,8])^2) + (1+1/25)*var(output[,7]))
aimth <- mean(output[,9])
aimthse <- sqrt(mean((output[,10])^2) + (1+1/25)*var(output[,9]))
aiint <- mean(output[,11])
aiintse <- sqrt(mean((output[,12])^2) + (1+1/25)*var(output[,11]))
ai <- cbind(c(aitrt,aimth,aiint),c(aitrtse,aimthse,aiintse))
rownames(ai) <- c("trtno","month","trtno:month")
colnames(ai) <- c("Estimate","Std.err")
Wald <- (ai[,1]/ai[,2])^2
PValue <- 1 - pchisq(Wald,1)
aimixed <- cbind(ai,Wald,PValue)##Summary table for combined result of geeglm for Activity Impairment
