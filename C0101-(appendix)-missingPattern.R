#' ---
#' title: Number of missing items within a questionnaire for each subject
#' author: Group H
#' date: March 21, 2018
#' ---

#+ echo=FALSE, warning=FALSE, include=FALSE
#varNA(data.working)
source("C0001-dataEntry.R")
source("F0001-basic.R")
library(dplyr);library(knitr)

kable.numMisItems <- function(object, title, questionnaire)
  kable(object[[2]], row.names = TRUE, caption = paste0(title, "\n - ", questionnaire,  ": Total number of items = ", object[[1]]))

#' ## anger variables     
#' There exist a few moderately missing subjects, and many lightly missing subjects
#+ echo=FALSE, warning=FALSE
title.tmp = "Number of subject for each missing item count"
numMisItems(var.anger, "bl", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Anger (baseline)")
numMisItems(var.anger, "fu", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Anger (2 weeks)")
numMisItems(var.anger, "3mo", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Anger (3 months)")
numMisItems(var.anger, "6mo", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Anger (6 months)")
numMisItems(var.anger, "12mo", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Anger (12 months)")

#' <br>
#' 
#' ## work productivity variables
#' Skip structure, but given a small size of questionnaire, might need multiple imputation.
#+ echo=FALSE, warning=FALSE
numMisItems(var.work, "bl", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Work productivity (baseline)")
numMisItems(var.work, "fu", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Work productivity (2 weeks)")
numMisItems(var.work, "3mo", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Work productivity (3 months)")
numMisItems(var.work, "6mo", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Work productivity (6 months)")
numMisItems(var.work, "12mo", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Work productivity (12 months)")

#' <br>
#' <br>
#' <br>
#' 
#' ## comorbidity variables      
#' There exist a few lightly missing subjects
#+ echo=FALSE, warning=FALSE
numMisItems(var.comorbid, "bl", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Comorbidity (baseline)")
numMisItems(var.comorbid, "fu", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Comorbidity (2 weeks)")
numMisItems(var.comorbid, "3mo", data, sample = sample.include)  %>% kable.numMisItems(title.tmp, "Comorbidity (3 months)")
numMisItems(var.comorbid, "6mo", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Comorbidity (6 months)")
numMisItems(var.comorbid, "12mo", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Comorbidity (12 months)")

#' <br>
#' <br>
#' <br>
#' 
#' ## FILE variables              
#' There exist many moderately missing subjects
#+ echo=FALSE, warning=FALSE
numMisItems(var.FILE, "common", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "FILE (last 12 months)")
numMisItems(var.FILE, "common-hist", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "FILE (before last 12 months)")

#' <br>
#' <br>
#' <br>
#' 
#' ## coping strategy variables
#' There exist a few moderately missing subjects
#+ echo=FALSE, warning=FALSE
numMisItems(var.coping, "bl", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Coping strategy (baseline)")
numMisItems(var.coping, "fu", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Coping strategy (2 weeks)")
numMisItems(var.coping, "3mo", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Coping strategy (3 months)")
numMisItems(var.coping, "6mo", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Coping strategy (6 months)")
numMisItems(var.coping, "12mo", data, sample = sample.include) %>% kable.numMisItems(title.tmp, "Coping strategy (12 months)")
