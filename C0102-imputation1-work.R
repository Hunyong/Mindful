source("C0001-dataEntry.R")
source("F0001-basic.R")

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
bldata <- data.frame(bldata[,1:2],blh1,blh2,blh3,bldata[,6:7]) ##Baseline data ready for use
bldrop <- c("3") ##subjects who "drop out"
bldropdata <- bldata[bldrop,]
blobsdata <- bldata[! rownames(bldata) %in% bldrop,] ##partially missing or complete questionaires
##blimpdata will be the imputed blobsdata using MICE
##For summary measure, visit http://www.reillyassociates.net/WPAI_Scoring.html and see WPAI:SHP, 4 possible scores

fudata <- data[-norandom,593:599]
##Correct Typos
fuh1 <- as.numeric(as.character(fudata[,3]))
fuh2 <- as.numeric(as.character(fudata[,4]))
fuh3 <- as.numeric(as.character(fudata[,5]))
fuh3[12] <- 45
fuh3[49] <- 25
fudata <- data.frame(fudata[,1:2],fuh1,fuh2,fuh3,fudata[,6:7]) ##follow-up data ready for use
fudrop <- c("13","22","33","36","50","56","59","60","69","89","90")
fudropdata <- fudata[fudrop,]
fuobsdata <- fudata[! rownames(fudata) %in% fudrop,]


mo3data <- data[-norandom,846:852]
##Correct Typos
mo3h1 <- as.numeric(as.character(mo3data[,3]))
mo3h2 <- as.numeric(as.character(mo3data[,4]))
mo3h3 <- as.numeric(as.character(mo3data[,5]))
mo3h3[58] <- 65
mo3data <- data.frame(mo3data[,1:2],mo3h1,mo3h2,mo3h3,mo3data[,6:7]) ##3mo data ready for use
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
mo6data <- data.frame(mo6data[,1:2],mo6h1,mo6h2,mo6h3,mo6data[,6:7]) ##6mo data ready for use
mo6drop <- c("5","7","12","13","19","22","25","33","35","36","40","50","56","60","69","70","75","77","83","90","97")
mo6dropdata <- mo6data[mo6drop,]
mo6obsdata <- mo6data[! rownames(mo6data) %in% mo6drop,]


mo12data <- data[-norandom,1489:1495]
##Correct Typos
mo12h1 <- as.numeric(as.character(mo12data[,3]))
mo12h2 <- as.numeric(as.character(mo12data[,4]))
mo12h3 <- as.numeric(as.character(mo12data[,5]))
mo12h3[61] <- 7.5
mo12data <- data.frame(mo12data[,1:2],mo12h1,mo12h2,mo12h3,mo12data[,6:7]) ##12mo data ready for use
mo12drop <- c("2","21","22","32","33","34","35","36","40","41","50","54","59","60","62","66","69","70","77","83","84",
              "89","92")
mo12dropdata <- mo12data[mo12drop,]
mo12obsdata <- mo12data[! rownames(mo12data) %in% mo12drop,]
