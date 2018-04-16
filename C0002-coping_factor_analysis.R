#Coping strategy factor analysis

## 0.2.6 coping strategy
extractVar("csq")

coping = data.frame(bl = paste0("bcsq_", 1:48),
                    fu = paste0("csq_", 1:48),
                    mo3 = paste0("csq_", 1:48, "_3mo"),
                    mo6 = paste0("csq_", 1:48, "_6mo"),
                    mo12 = paste0("csq_", 1:48, "_12mo"),
                    stringsAsFactors=FALSE)

var.coping = data.frame(variable =  as.vector(as.matrix(coping)),
                        category = "coping",
                        time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=48))

# we further divide the coping strategy into 9 factors

# 0.2.6.1 Distraction(DA)
DA = data.frame(bl = paste0("bcsq_", c(3,30,31,43,45)),
                fu = paste0("csq_", c(3,30,31,43,45)),
                mo3 = paste0("csq_", c(3,30,31,43,45), "_3mo"),
                mo6 = paste0("csq_", c(3,30,31,43,45), "_6mo"),
                mo12 = paste0("csq_", c(3,30,31,43,45), "_12mo"),
                stringsAsFactors=FALSE)

var.da = data.frame(variable =  as.vector(as.matrix(DA)),
                    category = "distraction",
                    time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=5))
# 0.2.6.2 Catastrophizing(CAT)
CAT = data.frame(bl = paste0("bcsq_", c(5,12,14,28,38,42)),
                 fu = paste0("csq_", c(5,12,14,28,38,42)),
                 mo3 = paste0("csq_", c(5,12,14,28,38,42), "_3mo"),
                 mo6 = paste0("csq_", c(5,12,14,28,38,42), "_6mo"),
                 mo12 = paste0("csq_", c(5,12,14,28,38,42), "_12mo"),
                 stringsAsFactors=FALSE)

var.cat = data.frame(variable =  as.vector(as.matrix(CAT)),
                     category = "catastrophyzing",
                     time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=6))
# 0.2.6.3 Ignoring Pain Sensations(IS)
IS = data.frame(bl = paste0("bcsq_", c(20,24,27,35,40)),
                fu = paste0("csq_", c(20,24,27,35,40)),
                mo3 = paste0("csq_", c(20,24,27,35,40), "_3mo"),
                mo6 = paste0("csq_", c(20,24,27,35,40), "_6mo"),
                mo12 = paste0("csq_", c(20,24,27,35,40), "_12mo"),
                stringsAsFactors=FALSE)

var.is = data.frame(variable =  as.vector(as.matrix(IS)),
                    category = "Ignoring Pain Sensations",
                    time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=5))
# 0.2.6.4 Distancing from Pain(DP)
DP = data.frame(bl = paste0("bcsq_", c(1,18,34,46)),
                fu = paste0("csq_", c(1,18,34,46)),
                mo3 = paste0("csq_", c(1,18,34,46), "_3mo"),
                mo6 = paste0("csq_", c(1,18,34,46), "_6mo"),
                mo12 = paste0("csq_", c(1,18,34,46), "_12mo"),
                stringsAsFactors=FALSE)

var.dp = data.frame(variable =  as.vector(as.matrix(DP)),
                    category = "distancing from pain",
                    time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=4))
# 0.2.6.5 Coping Self-Statements(CSS)
CSS = data.frame(bl = paste0("bcsq_", c(6,8,23,37)),
                 fu = paste0("csq_", c(6,8,23,37)),
                 mo3 = paste0("csq_", c(6,8,23,37), "_3mo"),
                 mo6 = paste0("csq_", c(6,8,23,37), "_6mo"),
                 mo12 = paste0("csq_", c(6,8,23,37), "_12mo"),
                 stringsAsFactors=FALSE)

var.css = data.frame(variable =  as.vector(as.matrix(CSS)),
                     category = "Coping Self-Statements",
                     time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=4))
# 0.2.6.6 Praying(PR)
PR = data.frame(bl = paste0("bcsq_", c(17,32,41)),
                fu = paste0("csq_", c(17,32,41)),
                mo3 = paste0("csq_", c(17,32,41), "_3mo"),
                mo6 = paste0("csq_", c(17,32,41), "_6mo"),
                mo12 = paste0("csq_", c(17,32,41), "_12mo"),
                stringsAsFactors=FALSE)

var.pr = data.frame(variable =  as.vector(as.matrix(PR)),
                    category = "praying",
                    time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=3))
# 0.2.6.7 Increasing Activity(IBA)
IBA = data.frame(bl = paste0("bcsq_", c(2,16)),
                 fu = paste0("csq_", c(2,16)),
                 mo3 = paste0("csq_", c(2,16), "_3mo"),
                 mo6 = paste0("csq_", c(2,16), "_6mo"),
                 mo12 = paste0("csq_", c(2,16), "_12mo"),
                 stringsAsFactors=FALSE)

var.iba = data.frame(variable =  as.vector(as.matrix(IBA)),
                     category = "increasing activity",
                     time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=2))
# 0.2.6.8 Hoping(HP)
HP = data.frame(bl = paste0("bcsq_", c(15,21,25)),
                fu = paste0("csq_", c(15,21,25)),
                mo3 = paste0("csq_", c(15,21,25), "_3mo"),
                mo6 = paste0("csq_", c(15,21,25), "_6mo"),
                mo12 = paste0("csq_", c(15,21,25), "_12mo"),
                stringsAsFactors=FALSE)

var.hp = data.frame(variable =  as.vector(as.matrix(HP)),
                    category = "hoping",
                    time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=3))
# 0.2.6.9 Reinterpreting Pain Sensations(RS)
RS = data.frame(bl = paste0("bcsq_", c(4,10,11)),
                fu = paste0("csq_", c(4,10,11)),
                mo3 = paste0("csq_", c(4,10,11), "_3mo"),
                mo6 = paste0("csq_", c(4,10,11), "_6mo"),
                mo12 = paste0("csq_", c(4,10,11), "_12mo"),
                stringsAsFactors=FALSE)

var.rs = data.frame(variable =  as.vector(as.matrix(RS)),
                    category = "reinterpreting pain sensations",
                    time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=3))

# 0.2.6.10 Summary of 9 factors (CP)
CP = data.frame(bl = paste0("bcsq_", c(1:6,8,10:12,14:18,20,21,23:25,27,28,30:32,34,35,37,38,40:43,45,46)),
                fu = paste0("csq_", c(1:6,8,10:12,14:18,20,21,23:25,27,28,30:32,34,35,37,38,40:43,45,46)),
                mo3 = paste0("csq_", c(1:6,8,10:12,14:18,20,21,23:25,27,28,30:32,34,35,37,38,40:43,45,46), "_3mo"),
                mo6 = paste0("csq_", c(1:6,8,10:12,14:18,20,21,23:25,27,28,30:32,34,35,37,38,40:43,45,46), "_6mo"),
                mo12 = paste0("csq_", c(1:6,8,10:12,14:18,20,21,23:25,27,28,30:32,34,35,37,38,40:43,45,46), "_12mo"),
                stringsAsFactors=FALSE)

var.cp = data.frame(variable =  as.vector(as.matrix(CP)),
                    category = "coping9",
                    time = rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=35))

#variable of interests.
var.ins<-