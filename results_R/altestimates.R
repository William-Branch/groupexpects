library(tidyverse)
library(zoo)
library(gridExtra)
library(fredr) 
library(ggplot2)
library(bartik.weight)
library(fixest)
library(modelsummary)
library(kableExtra)


setwd("/Users/billbranch/Dropbox/Cloud/Research/panel expectations/make-panelexpects")

# Michigan survey data;

#michigan <- read_csv("/Users/billbranch/Dropbox/Cloud/Research/panel expectations/data/CSV/Michigan_220524.csv")
michigan <- read_csv("data/Michigan_220524.csv")

# Remove n/a's and fix date format;

michigan <- michigan %>%
  mutate(survey_date = as.Date(paste0(michigan[["YYYYMM"]],01), format = "%Y%m%d")) %>%
  filter(PX1<96) %>%
  filter(PX1!=-97)

# Include only first time respondents;

michiganFirst <- michigan %>%
  filter(is.na(IDPREV))

# Exclude |P^e|>25;

michiganSmall <- michigan %>%
  filter(PX1 %in% (-25:25))

region_data <- read_csv("data/dataBuild/regiondata.csv") %>%
  mutate(cenREGION = REGION, group = industry) %>%
  select(-c(n, industry, REGION))

local_dataBig <- read_csv("data/dataBuild/localdataBig.csv")

paneldataSmall <-  read_csv("data/dataBuild/paneldataSmall.csv") %>%
  mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
  panel(~REGION + survey_date)
paneldataFirst <-  read_csv("data/dataBuild/paneldataFirst.csv") %>%
  mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
  panel(~REGION + survey_date)
paneldataState <-  read_csv("data/dataBuild/paneldataState.csv") %>%
  mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
  panel(~REGION + survey_date)
paneldataLag <-  read_csv("data/dataBuild/paneldataLag.csv") %>%
  mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
  panel(~REGION + survey_date)


est1 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik  , paneldataSmall)
est2 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik  , paneldataFirst)
est3 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik  , paneldataState)
est4 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_lag  , paneldataLag)



table_1 <- etable(est1, est2, est3, est4, vcov = "DK", dict = c("Bartik_cps" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
kbl(table_1, "latex", booktabs = T, caption = "Alternative estimates", col.names = c("small", "first-only", "state-CPI", "lag michigan shares"), linesep = "\\addlinespace", label = "table:2sls:robust") %>%
  kable_styling(latex_options = "striped") %>%
  footnote(general = "``small'' removes large survey responses.  ``first'' includes only first-time survey respondents.  ``state-cpi'' measures regional inflation by aggregating state-level CPI's.  ``lag michigan shares'' instruments with 12 month lagged survey shares." , threeparttable = T) %>%
  save_kable("tables/robust.tex")


#Group Categorizations;

local_dataBig_First <- michiganFirst %>%
  mutate(
    industry = case_when(
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 1,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 2,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 3,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 4,
      SEX == 1 & AGE %in% (18:24) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 5,
      SEX == 1 & AGE %in% (18:24) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 6,
      SEX == 1 & AGE %in% (18:24) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 7,
      SEX == 1 & AGE %in% (18:24) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 8,
      SEX == 1 & AGE %in% (18:24) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 9,
      SEX == 1 & AGE %in% (18:24) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 10,
      SEX == 1 & AGE %in% (18:24) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 11,
      SEX == 1 & AGE %in% (18:24) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 12,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 13,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 14,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 15,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 16,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 17,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 18,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 19,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 20,
      SEX == 1 & AGE %in% (25:34) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 21,
      SEX == 1 & AGE %in% (25:34) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 22,
      SEX == 1 & AGE %in% (25:34) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 23,
      SEX == 1 & AGE %in% (25:34) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 24,
      SEX == 1 & AGE %in% (25:34) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 25,
      SEX == 1 & AGE %in% (25:34) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 26,
      SEX == 1 & AGE %in% (25:34) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 27,
      SEX == 1 & AGE %in% (25:34) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 28,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 29,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 30,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 31,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 32,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 33,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 34,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 35,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 36,
      SEX == 1 & AGE %in% (35:49) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 37,
      SEX == 1 & AGE %in% (35:49) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 38,
      SEX == 1 & AGE %in% (35:49) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 39,
      SEX == 1 & AGE %in% (35:49) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 40,
      SEX == 1 & AGE %in% (35:49) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 41,
      SEX == 1 & AGE %in% (35:49) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 42,
      SEX == 1 & AGE %in% (35:49) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 43,
      SEX == 1 & AGE %in% (35:49) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 44,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 45,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 46,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 47,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 48,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 49,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 50,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 51,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 52,
      SEX == 1 & AGE %in% (50:64) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 53,
      SEX == 1 & AGE %in% (50:64) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 54,
      SEX == 1 & AGE %in% (50:64) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 55,
      SEX == 1 & AGE %in% (50:64) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 56,
      SEX == 1 & AGE %in% (50:64) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 57,
      SEX == 1 & AGE %in% (50:64) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 58,
      SEX == 1 & AGE %in% (50:64) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 59,
      SEX == 1 & AGE %in% (50:64) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 60,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 61,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 62,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 63,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 64,
      SEX == 1 & AGE > 64 & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 65,
      SEX == 1 & AGE > 64 & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 66,
      SEX == 1 & AGE > 64 & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 67,
      SEX == 1 & AGE > 64 & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 68,
      SEX == 1 & AGE > 64 & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 69,
      SEX == 1 & AGE > 64 & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 70,
      SEX == 1 & AGE > 64 & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 71,
      SEX == 1 & AGE > 64 & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 72,
      SEX == 1 & AGE > 64 & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 73,
      SEX == 1 & AGE > 64 & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 74,
      SEX == 1 & AGE > 64 & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 75,
      SEX == 1 & AGE > 64 & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 76,
      SEX == 1 & AGE > 64 & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 77,
      SEX == 1 & AGE > 64 & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 78,
      SEX == 1 & AGE > 64 & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 79,
      SEX == 1 & AGE > 64 & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 80,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 81,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 82,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 83,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 84,
      SEX == 2 & AGE %in% (18:24) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 85,
      SEX == 2 & AGE %in% (18:24) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 86,
      SEX == 2 & AGE %in% (18:24) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 87,
      SEX == 2 & AGE %in% (18:24) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 88,
      SEX == 2 & AGE %in% (18:24) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 89,
      SEX == 2 & AGE %in% (18:24) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 90,
      SEX == 2 & AGE %in% (18:24) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 91,
      SEX == 2 & AGE %in% (18:24) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 92,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 93,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 94,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 95,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 96,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 97,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 98,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 99,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 100,
      SEX == 2 & AGE %in% (25:34) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 101,
      SEX == 2 & AGE %in% (25:34) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 102,
      SEX == 2 & AGE %in% (25:34) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 103,
      SEX == 2 & AGE %in% (25:34) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 104,
      SEX == 2 & AGE %in% (25:34) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 105,
      SEX == 2 & AGE %in% (25:34) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 106,
      SEX == 2 & AGE %in% (25:34) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 107,
      SEX == 2 & AGE %in% (25:34) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 108,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 109,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 110,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 111,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 112,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 113,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 114,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 115,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 116,
      SEX == 2 & AGE %in% (35:49) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 117,
      SEX == 2 & AGE %in% (35:49) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 118,
      SEX == 2 & AGE %in% (35:49) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 119,
      SEX == 2 & AGE %in% (35:49) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 120,
      SEX == 2 & AGE %in% (35:49) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 121,
      SEX == 2 & AGE %in% (35:49) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 122,
      SEX == 2 & AGE %in% (35:49) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 123,
      SEX == 2 & AGE %in% (35:49) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 124,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 125,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 126,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 127,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 128,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 129,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 130,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 131,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 132,
      SEX == 2 & AGE %in% (50:64) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 133,
      SEX == 2 & AGE %in% (50:64) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 134,
      SEX == 2 & AGE %in% (50:64) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 135,
      SEX == 2 & AGE %in% (50:64) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 136,
      SEX == 2 & AGE %in% (50:64) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 137,
      SEX == 2 & AGE %in% (50:64) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 138,
      SEX == 2 & AGE %in% (50:64) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 139,
      SEX == 2 & AGE %in% (50:64) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 140,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 141,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 142,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 143,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 144,
      SEX == 2 & AGE > 64 & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 145,
      SEX == 2 & AGE > 64 & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 146,
      SEX == 2 & AGE > 64 & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 147,
      SEX == 2 & AGE > 64 & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 148,
      SEX == 2 & AGE > 64 & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 149,
      SEX == 2 & AGE > 64 & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 150,
      SEX == 2 & AGE > 64 & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 151,
      SEX == 2 & AGE > 64 & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 152,
      SEX == 2 & AGE > 64 & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 153,
      SEX == 2 & AGE > 64 & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 154,
      SEX == 2 & AGE > 64 & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 155,
      SEX == 2 & AGE > 64 & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 156,
      SEX == 2 & AGE > 64 & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 157,
      SEX == 2 & AGE > 64 & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 158,
      SEX == 2 & AGE > 64 & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 159,
      SEX == 2 & AGE > 64 & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 160,
      
      
      
    )
  ) 

# third, removed outliers;

local_dataBig_Small <- michiganSmall %>%
  mutate(
    industry = case_when(
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 1,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 2,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 3,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 4,
      SEX == 1 & AGE %in% (18:24) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 5,
      SEX == 1 & AGE %in% (18:24) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 6,
      SEX == 1 & AGE %in% (18:24) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 7,
      SEX == 1 & AGE %in% (18:24) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 8,
      SEX == 1 & AGE %in% (18:24) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 9,
      SEX == 1 & AGE %in% (18:24) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 10,
      SEX == 1 & AGE %in% (18:24) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 11,
      SEX == 1 & AGE %in% (18:24) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 12,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 13,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 14,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 15,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 16,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 17,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 18,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 19,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 20,
      SEX == 1 & AGE %in% (25:34) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 21,
      SEX == 1 & AGE %in% (25:34) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 22,
      SEX == 1 & AGE %in% (25:34) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 23,
      SEX == 1 & AGE %in% (25:34) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 24,
      SEX == 1 & AGE %in% (25:34) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 25,
      SEX == 1 & AGE %in% (25:34) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 26,
      SEX == 1 & AGE %in% (25:34) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 27,
      SEX == 1 & AGE %in% (25:34) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 28,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 29,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 30,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 31,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 32,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 33,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 34,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 35,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 36,
      SEX == 1 & AGE %in% (35:49) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 37,
      SEX == 1 & AGE %in% (35:49) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 38,
      SEX == 1 & AGE %in% (35:49) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 39,
      SEX == 1 & AGE %in% (35:49) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 40,
      SEX == 1 & AGE %in% (35:49) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 41,
      SEX == 1 & AGE %in% (35:49) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 42,
      SEX == 1 & AGE %in% (35:49) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 43,
      SEX == 1 & AGE %in% (35:49) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 44,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 45,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 46,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 47,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 48,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 49,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 50,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 51,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 52,
      SEX == 1 & AGE %in% (50:64) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 53,
      SEX == 1 & AGE %in% (50:64) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 54,
      SEX == 1 & AGE %in% (50:64) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 55,
      SEX == 1 & AGE %in% (50:64) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 56,
      SEX == 1 & AGE %in% (50:64) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 57,
      SEX == 1 & AGE %in% (50:64) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 58,
      SEX == 1 & AGE %in% (50:64) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 59,
      SEX == 1 & AGE %in% (50:64) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 60,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 61,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 62,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 63,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 64,
      SEX == 1 & AGE > 64 & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 65,
      SEX == 1 & AGE > 64 & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 66,
      SEX == 1 & AGE > 64 & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 67,
      SEX == 1 & AGE > 64 & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 68,
      SEX == 1 & AGE > 64 & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 69,
      SEX == 1 & AGE > 64 & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 70,
      SEX == 1 & AGE > 64 & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 71,
      SEX == 1 & AGE > 64 & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 72,
      SEX == 1 & AGE > 64 & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 73,
      SEX == 1 & AGE > 64 & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 74,
      SEX == 1 & AGE > 64 & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 75,
      SEX == 1 & AGE > 64 & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 76,
      SEX == 1 & AGE > 64 & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 77,
      SEX == 1 & AGE > 64 & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 78,
      SEX == 1 & AGE > 64 & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 79,
      SEX == 1 & AGE > 64 & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 80,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 81,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 82,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 83,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 84,
      SEX == 2 & AGE %in% (18:24) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 85,
      SEX == 2 & AGE %in% (18:24) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 86,
      SEX == 2 & AGE %in% (18:24) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 87,
      SEX == 2 & AGE %in% (18:24) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 88,
      SEX == 2 & AGE %in% (18:24) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 89,
      SEX == 2 & AGE %in% (18:24) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 90,
      SEX == 2 & AGE %in% (18:24) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 91,
      SEX == 2 & AGE %in% (18:24) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 92,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 93,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 94,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 95,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 96,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 97,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 98,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 99,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 100,
      SEX == 2 & AGE %in% (25:34) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 101,
      SEX == 2 & AGE %in% (25:34) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 102,
      SEX == 2 & AGE %in% (25:34) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 103,
      SEX == 2 & AGE %in% (25:34) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 104,
      SEX == 2 & AGE %in% (25:34) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 105,
      SEX == 2 & AGE %in% (25:34) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 106,
      SEX == 2 & AGE %in% (25:34) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 107,
      SEX == 2 & AGE %in% (25:34) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 108,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 109,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 110,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 111,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 112,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 113,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 114,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 115,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 116,
      SEX == 2 & AGE %in% (35:49) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 117,
      SEX == 2 & AGE %in% (35:49) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 118,
      SEX == 2 & AGE %in% (35:49) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 119,
      SEX == 2 & AGE %in% (35:49) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 120,
      SEX == 2 & AGE %in% (35:49) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 121,
      SEX == 2 & AGE %in% (35:49) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 122,
      SEX == 2 & AGE %in% (35:49) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 123,
      SEX == 2 & AGE %in% (35:49) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 124,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 125,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 126,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 127,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 128,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 129,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 130,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 131,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 132,
      SEX == 2 & AGE %in% (50:64) & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 133,
      SEX == 2 & AGE %in% (50:64) & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 134,
      SEX == 2 & AGE %in% (50:64) & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 135,
      SEX == 2 & AGE %in% (50:64) & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 136,
      SEX == 2 & AGE %in% (50:64) & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 137,
      SEX == 2 & AGE %in% (50:64) & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 138,
      SEX == 2 & AGE %in% (50:64) & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 139,
      SEX == 2 & AGE %in% (50:64) & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 140,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 141,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 142,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 143,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 144,
      SEX == 2 & AGE > 64 & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID ==0 ~ 145,
      SEX == 2 & AGE > 64 & EDUC %in% (1:2) & MARRY %in% (1:4) & NUMKID >0 ~ 146,
      SEX == 2 & AGE > 64 & EDUC %in% (1:2) & MARRY == 5 & NUMKID == 0 ~ 147,
      SEX == 2 & AGE > 64 & EDUC %in% (1:2) & MARRY == 5 & NUMKID >0 ~ 148,
      SEX == 2 & AGE > 64 & EDUC == 3 & MARRY %in% (1:4) & NUMKID ==0 ~ 149,
      SEX == 2 & AGE > 64 & EDUC == 3 & MARRY %in% (1:4) & NUMKID > 0 ~ 150,
      SEX == 2 & AGE > 64 & EDUC == 3 & MARRY == 5 & NUMKID == 0 ~ 151,
      SEX == 2 & AGE > 64 & EDUC == 3 & MARRY == 5 & NUMKID > 0 ~ 152,
      SEX == 2 & AGE > 64 & EDUC == 4 & MARRY %in% (1:4) & NUMKID == 0 ~ 153,
      SEX == 2 & AGE > 64 & EDUC == 4 & MARRY %in% (1:4) & NUMKID > 0 ~ 154,
      SEX == 2 & AGE > 64 & EDUC == 4 & MARRY == 5 & NUMKID == 0 ~ 155,
      SEX == 2 & AGE > 64 & EDUC == 4 & MARRY == 5 & NUMKID >0 ~ 156,
      SEX == 2 & AGE > 64 & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID == 0 ~ 157,
      SEX == 2 & AGE > 64 & EDUC %in% (5:6) & MARRY %in% (1:4) & NUMKID > 0 ~ 158,
      SEX == 2 & AGE > 64 & EDUC %in% (5:6) & MARRY == 5 & NUMKID ==0 ~ 159,
      SEX == 2 & AGE > 64 & EDUC %in% (5:6) & MARRY == 5 & NUMKID > 0 ~ 160,
      
      
      
    )
  ) 





# Leave one out, Bartik. (Small,First,State,Lag)

#Small
dataSmall_out_west <- local_dataBig_Small %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==1) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION = 1)

dataSmall_out_mw <- local_dataBig_Small %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==2) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION =2)

dataSmall_out_ne <- local_dataBig_Small %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==3) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION =3)

dataSmall_out_s <- local_dataBig_Small %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==4) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION=4)


dataSmall_out <- rbind(dataSmall_out_west,dataSmall_out_mw, dataSmall_out_ne,dataSmall_out_s) %>% mutate(group = industry) %>% select(-industry)

region_data_Small <- local_dataBig_Small %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  drop_na() %>%
  group_by(REGION, survey_date, industry) %>%
  summarise(n=n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(survey_date > as.Date("1977-12-01"))

Bartik_bench_out_Small <- merge(region_data_Small, dataSmall_out) %>%
  drop_na() %>%
  group_by(cenREGION, survey_date) %>%
  arrange(survey_date) %>%
  summarise(Bartik_bench_out_Small = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = cenREGION)



panel_bench_out_Small <- merge(paneldataSmall, Bartik_bench_out_Small) %>% panel(~REGION + survey_date)


# First;

dataFirst_out_west <- local_dataBig_First %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==1) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION = 1)

dataFirst_out_mw <- local_dataBig_First %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==2) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION =2)

dataFirst_out_ne <- local_dataBig_First %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==3) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION =3)

dataFirst_out_s <- local_dataBig_First %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==4) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION=4)


dataFirst_out <- rbind(dataFirst_out_west,dataFirst_out_mw, dataFirst_out_ne,dataFirst_out_s) %>% mutate(group = industry) %>% select(-industry)

region_data_First <- local_dataBig_First %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  drop_na() %>%
  group_by(REGION, survey_date, industry) %>%
  summarise(n=n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(survey_date > as.Date("1977-12-01"))

Bartik_bench_out_First <- merge(region_data_First, dataFirst_out) %>%
  drop_na() %>%
  group_by(cenREGION, survey_date) %>%
  arrange(survey_date) %>%
  summarise(Bartik_bench_out_First = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = cenREGION)


panel_bench_out_First <- merge(paneldataFirst, Bartik_bench_out_First) %>% panel(~REGION + survey_date)


# State;

data_out_west <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==1) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION = 1)

data_out_mw <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==2) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION =2)

data_out_ne <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==3) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION =3)

data_out_s <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==4) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION=4)

data_out <- rbind(data_out_west,data_out_mw, data_out_ne,data_out_s) %>% mutate(group = industry) %>% select(-industry)


Bartik_state_out <- merge(region_data, data_out) %>%
  #drop_na() %>%
  group_by(cenREGION, survey_date) %>%
  arrange(survey_date) %>%
  summarise(Bartik_bench_out_State = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = cenREGION)






panel_out_State <- merge(paneldataState, Bartik_state_out) %>% panel(~REGION + survey_date)

# Lag

Bartik_Lag_out <- merge(region_data, data_out) %>%
  mutate(lagprop = lag(prop,12)) %>%
  drop_na() %>%
  group_by(cenREGION, survey_date) %>%
  arrange(survey_date) %>%
  summarise(Bartik_bench_out_Lag = sum(lagprop*Agg_pe_ind)) %>%
  mutate(REGION = cenREGION)






panel_out_Lag <- merge(paneldataLag, Bartik_Lag_out) %>% panel(~REGION + survey_date)



est1 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out_Small  , panel_bench_out_Small)
est2 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out_First  , panel_bench_out_First)
est3 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out_State  , panel_out_State)
est4 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out_Lag  , panel_out_Lag)



table_1 <- etable(est1, est2, est3, est4, vcov = "DK", dict = c("Bartik_cps" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
kbl(table_1, "latex", booktabs = T, caption = "Alternative estimates", col.names = c("small", "first-only", "state-CPI", "lag michigan shares"), linesep = "\\addlinespace", label = "table:2sls:robust") %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  footnote(general = "``small'' removes large survey responses.  ``first'' includes only first-time survey respondents.  ``state-cpi'' measures regional inflation by aggregating state-level CPI's.  ``lag michigan shares'' instruments with 12 month lagged survey shares." , threeparttable = T) %>%
  save_kable("tables/robustOut.tex")


# est_out <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panel_out)
# est_bench_out<- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panel_bench_out)
# table_out_1 <- etable(est_bench_out, est_out, vcov = "DK", dict = c("Bartik_bench_out" = "Bartik",   "Bartik_out" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
# table_out_2 <- etable(est_bench_out, est_out, vcov = "DK", dict = c("yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
# 
# kbl(table_out_1, "latex", booktabs = T, caption = "2SLS: first stage", col.names = c("survey", "CPS78"), linesep = "\\addlinespace", label = "base:out:2sls:stage1") %>%
#   kable_styling(latex_options = "striped") %>%
#   footnote(general = "Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.", number = c("Signif. codes: * = .05; ** = .01; *** = .001.") , threeparttable = T) %>%
#   save_kable("tables/base1yearStage1.tex")
# 
# kbl(table_out_2, "latex", booktabs = T, caption = "2SLS: coefficient estimates", col.names = c("survey", "CPS78"), linesep = "\\addlinespace", label = "base:out:2sls:stage2") %>%
#   kable_styling(latex_options = "striped") %>%
#   footnote(general = "Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS." , number = c("Signif. codes: * = .05; ** = .01; *** = .001."), threeparttable = T) %>%
#   save_kable("tables/base1year.tex")
# 
# # First-stage and reduced-form plots.
# 
# rf_reg <- feols(RegInf ~  Bartik , data =paneldata)
# Fstage_reg <- feols(pe ~ Bartik, data = paneldata)
# 
# pRF <- ggplot(data = paneldata, aes(x=Bartik, y=RegInf)) + geom_point() + geom_abline(slope = coefficients(rf_reg)[[2]], intercept = coefficients(rf_reg)[[1]], col = rgb(.541,.2,.141), size = 1.2) +
#   labs(y = ("reg. inflation"), x = "Bartik (mich.)")+
#   theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 34,colour = rgb(.26, .17, .16)), legend.position = "none")  
# 
# pF1 <- ggplot(data = paneldata, aes(x=Bartik, y=pe)) + geom_point() + geom_abline(slope = coefficients(Fstage_reg)[[2]], intercept = coefficients(Fstage_reg)[[1]], col = rgb(.541,.2,.141), size = 1.2) +
#   labs(y = ("pe (survey)"), x = "Bartik (mich.)")+
#   theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 34,colour = rgb(.26, .17, .16)), legend.position = "none")  


