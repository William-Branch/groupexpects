# generate the 5 year tables with survey and CPS 78 shares + applying split-sample Jackknife bias correction.
# outputs:  base1and5yearStage1.tex, base1and5year.tex,  base5yearStage1.tex, base5year.tex, redform5only.pdf, firstStage5only.pdf.

library(tidyverse)
library(zoo)
library(gridExtra)
library(fredr)
library(ggplot2)
library(bartik.weight)
library(ipumsr)
library(fixest)
library(kableExtra)


setwd("~/Dropbox/Cloud/Research/panel expectations/make-panelexpects")

cps78 <- read_csv("data/cps_00002.csv")
data_globalCPSw5 <- read_csv("data/dataBuild/globaldatawith5.csv")
paneldataw5 <-  read_csv("data/dataBuild/paneldatawith5.csv") %>%
  mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
  panel(~REGION + survey_date)
region_dataw5 <- read_csv("data/dataBuild/regiondatawith5.csv") %>%
  mutate(cenREGION = REGION, group = industry) %>%
  select(-c(n, industry, REGION))
local_dataBig <- read_csv("data/dataBuild/localdataBig.csv")


local_dataCPS78 <- cps78 %>%
  mutate(
    group = case_when(
      SEX == 1 & AGE %in% (18:24) & EDUC <72 & !MARST ==6 & NCHILD ==0 ~ 1,
      SEX == 1 & AGE %in% (18:24) & EDUC <72 & !MARST ==6 & NCHILD >0 ~ 2,
      SEX == 1 & AGE %in% (18:24) & EDUC <72 & MARST == 6 & NCHILD == 0 ~ 3,
      SEX == 1 & AGE %in% (18:24) & EDUC <72 & MARST == 6 & NCHILD >0 ~ 4,
      SEX == 1 & AGE %in% (18:24) & EDUC == 73 & !MARST ==6 & NCHILD ==0 ~ 5,
      SEX == 1 & AGE %in% (18:24) & EDUC == 73 & !MARST ==6 & NCHILD > 0 ~ 6,
      SEX == 1 & AGE %in% (18:24) & EDUC == 73 & MARST == 6 & NCHILD == 0 ~ 7,
      SEX == 1 & AGE %in% (18:24) & EDUC == 73 & MARST == 6 & NCHILD > 0 ~ 8,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (80-110) & !MARST ==6 & NCHILD == 0 ~ 9,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (80-110) & !MARST ==6 & NCHILD > 0 ~ 10,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (80-110) & MARST == 6 & NCHILD == 0 ~ 11,
      SEX == 1 & AGE %in% (18:24) & EDUC %in% (80-110) & MARST == 6 & NCHILD >0 ~ 12,
      SEX == 1 & AGE %in% (18:24) & EDUC > 110 & !MARST ==6 & NCHILD == 0 ~ 13,
      SEX == 1 & AGE %in% (18:24) & EDUC > 110 & !MARST ==6 & NCHILD > 0 ~ 14,
      SEX == 1 & AGE %in% (18:24) & EDUC > 110 & MARST == 6 & NCHILD ==0 ~ 15,
      SEX == 1 & AGE %in% (18:24) & EDUC > 110 & MARST == 6 & NCHILD > 0 ~ 16,
      SEX == 1 & AGE %in% (25:34) & EDUC <72 & !MARST ==6 & NCHILD ==0 ~ 17,
      SEX == 1 & AGE %in% (25:34) & EDUC <72 & !MARST ==6 & NCHILD >0 ~ 18,
      SEX == 1 & AGE %in% (25:34) & EDUC <72 & MARST == 6 & NCHILD == 0 ~ 19,
      SEX == 1 & AGE %in% (25:34) & EDUC <72 & MARST == 6 & NCHILD >0 ~ 20,
      SEX == 1 & AGE %in% (25:34) & EDUC == 73 & !MARST ==6 & NCHILD ==0 ~ 21,
      SEX == 1 & AGE %in% (25:34) & EDUC == 73 & !MARST ==6 & NCHILD > 0 ~ 22,
      SEX == 1 & AGE %in% (25:34) & EDUC == 73 & MARST == 6 & NCHILD == 0 ~ 23,
      SEX == 1 & AGE %in% (25:34) & EDUC == 73 & MARST == 6 & NCHILD > 0 ~ 24,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (80-110) & !MARST ==6 & NCHILD == 0 ~ 25,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (80-110) & !MARST ==6 & NCHILD > 0 ~ 26,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (80-110) & MARST == 6 & NCHILD == 0 ~ 27,
      SEX == 1 & AGE %in% (25:34) & EDUC %in% (80-110) & MARST == 6 & NCHILD >0 ~ 28,
      SEX == 1 & AGE %in% (25:34) & EDUC > 110 & !MARST ==6 & NCHILD == 0 ~ 29,
      SEX == 1 & AGE %in% (25:34) & EDUC > 110 & !MARST ==6 & NCHILD > 0 ~ 30,
      SEX == 1 & AGE %in% (25:34) & EDUC > 110 & MARST == 6 & NCHILD ==0 ~ 31,
      SEX == 1 & AGE %in% (25:34) & EDUC > 110 & MARST == 6 & NCHILD > 0 ~ 32,
      SEX == 1 & AGE %in% (35:49) & EDUC <72 & !MARST ==6 & NCHILD ==0 ~ 33,
      SEX == 1 & AGE %in% (35:49) & EDUC <72 & !MARST ==6 & NCHILD >0 ~ 34,
      SEX == 1 & AGE %in% (35:49) & EDUC <72 & MARST == 6 & NCHILD == 0 ~ 35,
      SEX == 1 & AGE %in% (35:49) & EDUC <72 & MARST == 6 & NCHILD >0 ~ 36,
      SEX == 1 & AGE %in% (35:49) & EDUC == 73 & !MARST ==6 & NCHILD ==0 ~ 37,
      SEX == 1 & AGE %in% (35:49) & EDUC == 73 & !MARST ==6 & NCHILD > 0 ~ 38,
      SEX == 1 & AGE %in% (35:49) & EDUC == 73 & MARST == 6 & NCHILD == 0 ~ 39,
      SEX == 1 & AGE %in% (35:49) & EDUC == 73 & MARST == 6 & NCHILD > 0 ~ 40,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (80-110) & !MARST ==6 & NCHILD == 0 ~ 41,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (80-110) & !MARST ==6 & NCHILD > 0 ~ 42,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (80-110) & MARST == 6 & NCHILD == 0 ~ 43,
      SEX == 1 & AGE %in% (35:49) & EDUC %in% (80-110) & MARST == 6 & NCHILD >0 ~ 44,
      SEX == 1 & AGE %in% (35:49) & EDUC > 110 & !MARST ==6 & NCHILD == 0 ~ 45,
      SEX == 1 & AGE %in% (35:49) & EDUC > 110 & !MARST ==6 & NCHILD > 0 ~ 46,
      SEX == 1 & AGE %in% (35:49) & EDUC > 110 & MARST == 6 & NCHILD ==0 ~ 47,
      SEX == 1 & AGE %in% (35:49) & EDUC > 110 & MARST == 6 & NCHILD > 0 ~ 48,
      SEX == 1 & AGE %in% (50:64) & EDUC <72 & !MARST ==6 & NCHILD ==0 ~ 49,
      SEX == 1 & AGE %in% (50:64) & EDUC <72 & !MARST ==6 & NCHILD >0 ~ 50,
      SEX == 1 & AGE %in% (50:64) & EDUC <72 & MARST == 6 & NCHILD == 0 ~ 51,
      SEX == 1 & AGE %in% (50:64) & EDUC <72 & MARST == 6 & NCHILD >0 ~ 52,
      SEX == 1 & AGE %in% (50:64) & EDUC == 73 & !MARST ==6 & NCHILD ==0 ~ 53,
      SEX == 1 & AGE %in% (50:64) & EDUC == 73 & !MARST ==6 & NCHILD > 0 ~ 54,
      SEX == 1 & AGE %in% (50:64) & EDUC == 73 & MARST == 6 & NCHILD == 0 ~ 55,
      SEX == 1 & AGE %in% (50:64) & EDUC == 73 & MARST == 6 & NCHILD > 0 ~ 56,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (80-110) & !MARST ==6 & NCHILD == 0 ~ 57,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (80-110) & !MARST ==6 & NCHILD > 0 ~ 58,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (80-110) & MARST == 6 & NCHILD == 0 ~ 59,
      SEX == 1 & AGE %in% (50:64) & EDUC %in% (80-110) & MARST == 6 & NCHILD >0 ~ 60,
      SEX == 1 & AGE %in% (50:64) & EDUC > 110 & !MARST ==6 & NCHILD == 0 ~ 61,
      SEX == 1 & AGE %in% (50:64) & EDUC > 110 & !MARST ==6 & NCHILD > 0 ~ 62,
      SEX == 1 & AGE %in% (50:64) & EDUC > 110 & MARST == 6 & NCHILD ==0 ~ 63,
      SEX == 1 & AGE %in% (50:64) & EDUC > 110 & MARST == 6 & NCHILD > 0 ~ 64,
      SEX == 1 & AGE > 64 & EDUC <72 & !MARST ==6 & NCHILD ==0 ~ 65,
      SEX == 1 & AGE > 64 & EDUC <72 & !MARST ==6 & NCHILD >0 ~ 66,
      SEX == 1 & AGE > 64 & EDUC <72 & MARST == 6 & NCHILD == 0 ~ 67,
      SEX == 1 & AGE > 64 & EDUC <72 & MARST == 6 & NCHILD >0 ~ 68,
      SEX == 1 & AGE > 64 & EDUC == 73 & !MARST ==6 & NCHILD ==0 ~ 69,
      SEX == 1 & AGE > 64 & EDUC == 73 & !MARST ==6 & NCHILD > 0 ~ 70,
      SEX == 1 & AGE > 64 & EDUC == 73 & MARST == 6 & NCHILD == 0 ~ 71,
      SEX == 1 & AGE > 64 & EDUC == 73 & MARST == 6 & NCHILD > 0 ~ 72,
      SEX == 1 & AGE > 64 & EDUC %in% (80-110) & !MARST ==6 & NCHILD == 0 ~ 73,
      SEX == 1 & AGE > 64 & EDUC %in% (80-110) & !MARST ==6 & NCHILD > 0 ~ 74,
      SEX == 1 & AGE > 64 & EDUC %in% (80-110) & MARST == 6 & NCHILD == 0 ~ 75,
      SEX == 1 & AGE > 64 & EDUC %in% (80-110) & MARST == 6 & NCHILD >0 ~ 76,
      SEX == 1 & AGE > 64 & EDUC > 110 & !MARST ==6 & NCHILD == 0 ~ 77,
      SEX == 1 & AGE > 64 & EDUC > 110 & !MARST ==6 & NCHILD > 0 ~ 78,
      SEX == 1 & AGE > 64 & EDUC > 110 & MARST == 6 & NCHILD ==0 ~ 79,
      SEX == 1 & AGE > 64 & EDUC > 110 & MARST == 6 & NCHILD > 0 ~ 80,
      SEX == 2 & AGE %in% (18:24) & EDUC <72 & !MARST ==6 & NCHILD ==0 ~ 81,
      SEX == 2 & AGE %in% (18:24) & EDUC <72 & !MARST ==6 & NCHILD >0 ~ 82,
      SEX == 2 & AGE %in% (18:24) & EDUC <72 & MARST == 6 & NCHILD == 0 ~ 83,
      SEX == 2 & AGE %in% (18:24) & EDUC <72 & MARST == 6 & NCHILD >0 ~ 84,
      SEX == 2 & AGE %in% (18:24) & EDUC == 73 & !MARST ==6 & NCHILD ==0 ~ 85,
      SEX == 2 & AGE %in% (18:24) & EDUC == 73 & !MARST ==6 & NCHILD > 0 ~ 86,
      SEX == 2 & AGE %in% (18:24) & EDUC == 73 & MARST == 6 & NCHILD == 0 ~ 87,
      SEX == 2 & AGE %in% (18:24) & EDUC == 73 & MARST == 6 & NCHILD > 0 ~ 88,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (80-110) & !MARST ==6 & NCHILD == 0 ~ 89,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (80-110) & !MARST ==6 & NCHILD > 0 ~ 90,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (80-110) & MARST == 6 & NCHILD == 0 ~ 91,
      SEX == 2 & AGE %in% (18:24) & EDUC %in% (80-110) & MARST == 6 & NCHILD >0 ~ 92,
      SEX == 2 & AGE %in% (18:24) & EDUC > 110 & !MARST ==6 & NCHILD == 0 ~ 93,
      SEX == 2 & AGE %in% (18:24) & EDUC > 110 & !MARST ==6 & NCHILD > 0 ~ 94,
      SEX == 2 & AGE %in% (18:24) & EDUC > 110 & MARST == 6 & NCHILD ==0 ~ 95,
      SEX == 2 & AGE %in% (18:24) & EDUC > 110 & MARST == 6 & NCHILD > 0 ~ 96,
      SEX == 2 & AGE %in% (25:34) & EDUC <72 & !MARST ==6 & NCHILD ==0 ~ 97,
      SEX == 2 & AGE %in% (25:34) & EDUC <72 & !MARST ==6 & NCHILD >0 ~ 98,
      SEX == 2 & AGE %in% (25:34) & EDUC <72 & MARST == 6 & NCHILD == 0 ~ 99,
      SEX == 2 & AGE %in% (25:34) & EDUC <72 & MARST == 6 & NCHILD >0 ~ 100,
      SEX == 2 & AGE %in% (25:34) & EDUC == 73 & !MARST ==6 & NCHILD ==0 ~ 101,
      SEX == 2 & AGE %in% (25:34) & EDUC == 73 & !MARST ==6 & NCHILD > 0 ~ 102,
      SEX == 2 & AGE %in% (25:34) & EDUC == 73 & MARST == 6 & NCHILD == 0 ~ 103,
      SEX == 2 & AGE %in% (25:34) & EDUC == 73 & MARST == 6 & NCHILD > 0 ~ 104,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (80-110) & !MARST ==6 & NCHILD == 0 ~ 105,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (80-110) & !MARST ==6 & NCHILD > 0 ~ 106,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (80-110) & MARST == 6 & NCHILD == 0 ~ 107,
      SEX == 2 & AGE %in% (25:34) & EDUC %in% (80-110) & MARST == 6 & NCHILD >0 ~ 108,
      SEX == 2 & AGE %in% (25:34) & EDUC > 110 & !MARST ==6 & NCHILD == 0 ~ 109,
      SEX == 2 & AGE %in% (25:34) & EDUC > 110 & !MARST ==6 & NCHILD > 0 ~ 110,
      SEX == 2 & AGE %in% (25:34) & EDUC > 110 & MARST == 6 & NCHILD ==0 ~ 111,
      SEX == 2 & AGE %in% (25:34) & EDUC > 110 & MARST == 6 & NCHILD > 0 ~ 112,
      SEX == 2 & AGE %in% (35:49) & EDUC <72 & !MARST ==6 & NCHILD ==0 ~ 113,
      SEX == 2 & AGE %in% (35:49) & EDUC <72 & !MARST ==6 & NCHILD >0 ~ 114,
      SEX == 2 & AGE %in% (35:49) & EDUC <72 & MARST == 6 & NCHILD == 0 ~ 115,
      SEX == 2 & AGE %in% (35:49) & EDUC <72 & MARST == 6 & NCHILD >0 ~ 116,
      SEX == 2 & AGE %in% (35:49) & EDUC == 73 & !MARST ==6 & NCHILD ==0 ~ 117,
      SEX == 2 & AGE %in% (35:49) & EDUC == 73 & !MARST ==6 & NCHILD > 0 ~ 118,
      SEX == 2 & AGE %in% (35:49) & EDUC == 73 & MARST == 6 & NCHILD == 0 ~ 119,
      SEX == 2 & AGE %in% (35:49) & EDUC == 73 & MARST == 6 & NCHILD > 0 ~ 120,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (80-110) & !MARST ==6 & NCHILD == 0 ~ 121,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (80-110) & !MARST ==6 & NCHILD > 0 ~ 122,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (80-110) & MARST == 6 & NCHILD == 0 ~ 123,
      SEX == 2 & AGE %in% (35:49) & EDUC %in% (80-110) & MARST == 6 & NCHILD >0 ~ 124,
      SEX == 2 & AGE %in% (35:49) & EDUC > 110 & !MARST ==6 & NCHILD == 0 ~ 125,
      SEX == 2 & AGE %in% (35:49) & EDUC > 110 & !MARST ==6 & NCHILD > 0 ~ 126,
      SEX == 2 & AGE %in% (35:49) & EDUC > 110 & MARST == 6 & NCHILD ==0 ~ 127,
      SEX == 2 & AGE %in% (35:49) & EDUC > 110 & MARST == 6 & NCHILD > 0 ~ 128,
      SEX == 2 & AGE %in% (50:64) & EDUC <72 & !MARST ==6 & NCHILD ==0 ~ 129,
      SEX == 2 & AGE %in% (50:64) & EDUC <72 & !MARST ==6 & NCHILD >0 ~ 130,
      SEX == 2 & AGE %in% (50:64) & EDUC <72 & MARST == 6 & NCHILD == 0 ~ 131,
      SEX == 2 & AGE %in% (50:64) & EDUC <72 & MARST == 6 & NCHILD >0 ~ 132,
      SEX == 2 & AGE %in% (50:64) & EDUC == 73 & !MARST ==6 & NCHILD ==0 ~ 133,
      SEX == 2 & AGE %in% (50:64) & EDUC == 73 & !MARST ==6 & NCHILD > 0 ~ 134,
      SEX == 2 & AGE %in% (50:64) & EDUC == 73 & MARST == 6 & NCHILD == 0 ~ 135,
      SEX == 2 & AGE %in% (50:64) & EDUC == 73 & MARST == 6 & NCHILD > 0 ~ 136,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (80-110) & !MARST ==6 & NCHILD == 0 ~ 137,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (80-110) & !MARST ==6 & NCHILD > 0 ~ 138,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (80-110) & MARST == 6 & NCHILD == 0 ~ 139,
      SEX == 2 & AGE %in% (50:64) & EDUC %in% (80-110) & MARST == 6 & NCHILD >0 ~ 140,
      SEX == 2 & AGE %in% (50:64) & EDUC > 110 & !MARST ==6 & NCHILD == 0 ~ 141,
      SEX == 2 & AGE %in% (50:64) & EDUC > 110 & !MARST ==6 & NCHILD > 0 ~ 142,
      SEX == 2 & AGE %in% (50:64) & EDUC > 110 & MARST == 6 & NCHILD ==0 ~ 143,
      SEX == 2 & AGE %in% (50:64) & EDUC > 110 & MARST == 6 & NCHILD > 0 ~ 144,
      SEX == 2 & AGE > 64 & EDUC <72 & !MARST ==6 & NCHILD ==0 ~ 145,
      SEX == 2 & AGE > 64 & EDUC <72 & !MARST ==6 & NCHILD >0 ~ 146,
      SEX == 2 & AGE > 64 & EDUC <72 & MARST == 6 & NCHILD == 0 ~ 147,
      SEX == 2 & AGE > 64 & EDUC <72 & MARST == 6 & NCHILD >0 ~ 148,
      SEX == 2 & AGE > 64 & EDUC == 73 & !MARST ==6 & NCHILD ==0 ~ 149,
      SEX == 2 & AGE > 64 & EDUC == 73 & !MARST ==6 & NCHILD > 0 ~ 150,
      SEX == 2 & AGE > 64 & EDUC == 73 & MARST == 6 & NCHILD == 0 ~ 151,
      SEX == 2 & AGE > 64 & EDUC == 73 & MARST == 6 & NCHILD > 0 ~ 152,
      SEX == 2 & AGE > 64 & EDUC %in% (80-110) & !MARST ==6 & NCHILD == 0 ~ 153,
      SEX == 2 & AGE > 64 & EDUC %in% (80-110) & !MARST ==6 & NCHILD > 0 ~ 154,
      SEX == 2 & AGE > 64 & EDUC %in% (80-110) & MARST == 6 & NCHILD == 0 ~ 155,
      SEX == 2 & AGE > 64 & EDUC %in% (80-110) & MARST == 6 & NCHILD >0 ~ 156,
      SEX == 2 & AGE > 64 & EDUC > 110 & !MARST ==6 & NCHILD == 0 ~ 157,
      SEX == 2 & AGE > 64 & EDUC > 110 & !MARST ==6 & NCHILD > 0 ~ 158,
      SEX == 2 & AGE > 64 & EDUC > 110 & MARST == 6 & NCHILD ==0 ~ 159,
      SEX == 2 & AGE > 64 & EDUC > 110 & MARST == 6 & NCHILD > 0 ~ 160,
      
      
      
    )
  ) 

local_dataCPS78 <- local_dataCPS78 %>%
  mutate(cenREGION = case_when(REGION == 41 | REGION ==42 ~ 1, 
                               REGION == 21 | REGION ==22 ~ 2,
                               REGION == 11 | REGION ==12 ~ 3,
                               REGION == 31 | REGION ==32 ~ 4))


region_CPS78w5 <- local_dataCPS78 %>%
  select(c( "cenREGION", "group")) %>%
  #drop_na() %>%
  group_by(cenREGION,  group) %>%
  summarise(n=n( )) %>%
  mutate(prop = n/sum(n, na.rm = TRUE)) %>%
  select(-n)
#write_csv(region_CPS78,"data/dataBuild/regiondataout.csv")


data_globalCPSw5 <- data_globalCPSw5 %>%
  mutate(group = industry) %>%
  select(-industry)


Bartik_CPS78w5 <- merge(region_CPS78w5, data_globalCPSw5) %>%
  drop_na() %>%
  group_by(cenREGION, survey_date)%>%
  arrange(survey_date)

Bartik_CPS78w5 <- Bartik_CPS78w5 %>%
  group_by(cenREGION, survey_date) %>%
  summarise(Bartik_cps = sum(prop*Agg_pe_ind), Bartik5_cps = sum(prop*Agg_pe5_ind)) %>%
  mutate(REGION = cenREGION)



panel_cpsw5 <- merge(paneldataw5, Bartik_CPS78w5) %>% panel(~REGION + survey_date)

est5_cps <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe + pe5  ~ Bartik_cps + Bartik5_cps  , panel_cpsw5)
est5_bench <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe + pe5 ~ Bartik + Bartik5  , paneldataw5)
table5_1 <- etable(est5_bench, est5_cps, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe",  "Bartik5_cps" = "Bartik5",   "Bartik_cps" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table5_2 <- etable(est5_bench, est5_cps, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe","yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))


kbl(table5_1, "latex", booktabs = T, caption = "2SLS with short and long-expectations: first stage", col.names = c("survey short pe","survey long pe", "CPS78 short pe", "CPS78 long pe"), linesep = "\\addlinespace") %>%
  kable_styling(latex_options = c("striped","scale_down")) %>%
  footnote(general = "Survey is the benchmark shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS." , threeparttable = T) %>%
  save_kable("tables/base1and5yearStage1noleaveoutout.tex")

kbl(table5_2, "latex", booktabs = T, caption = "2SLS with short and long-expectations: coefficient estimates", col.names = c("survey", "CPS78"), linesep = "\\addlinespace") %>%
  kable_styling(latex_options = "striped") %>%
  footnote(general = "Survey is the benchmark shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS." , threeparttable = T) %>%
  save_kable("tables/base1and5yearnoleaveoneout.tex")


# 5 year only;

est5only_cps <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe5  ~  Bartik5_cps  , panel_cpsw5)
est5only_bench <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe5 ~  Bartik5  , paneldataw5)
table5only_1 <- etable(est5only_bench, est5only_cps, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe",  "Bartik5_cps" = "Bartik5",   "Bartik_cps" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table5only_2 <- etable(est5only_bench, est5only_cps, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe","yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))


kbl(table5only_1, "latex", booktabs = T, caption = "2SLS with long-expectations: first stage", col.names = c("survey long pe", "CPS78 long pe"), linesep = "\\addlinespace", label = "2sls:long:stage1") %>%
  kable_styling(latex_options = c("striped")) %>%
  footnote(general = "Survey is the benchmark shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS." , threeparttable = T) %>%
  save_kable("tables/base5yearStage1.tex")

kbl(table5only_2, "latex", booktabs = T, caption = "2SLS with long-expectations: coefficient estimates", col.names = c("survey", "CPS78"), linesep = "\\addlinespace", label = "2sls:long:stage2") %>%
  kable_styling(latex_options = "striped") %>%
  footnote(general = "Survey is the benchmark shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS." , threeparttable = T) %>%
  save_kable("tables/base5year.tex")

rf_reg5 <- feols(RegInf ~  Bartik5 , data =paneldataw5)
Fstage_reg5 <- feols(pe5 ~ Bartik5, data = paneldataw5)

pRF5 <- ggplot(data = filter(paneldataw5,!Bartik5==0), aes(x=Bartik5, y=RegInf)) + geom_point() + geom_abline(slope = coefficients(rf_reg5)[[2]], intercept = coefficients(rf_reg5)[[1]], col = rgb(.541,.2,.141), size = 1.2) +
  labs(y = ("reg. inflation"), x = "Bartik5 (mich.)")+
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 34,colour = rgb(.26, .17, .16)), legend.position = "none")  

pF15 <- ggplot(data = filter(paneldataw5,!Bartik5==0), aes(x=Bartik5, y=pe5)) + geom_point() + geom_abline(slope = coefficients(Fstage_reg5)[[2]], intercept = coefficients(Fstage_reg5)[[1]], col = rgb(.541,.2,.141), size = 1.2) +
  labs(y = ("pe5 (survey)"), x = "Bartik5 (mich.)")+
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 34,colour = rgb(.26, .17, .16)), legend.position = "none")  

ggsave("figs/firstStage5only.pdf", pF15)
ggsave("figs/redform5only.pdf", pRF5)


# Leave one out, Bartik.

data_out_westw5 <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1", "PX5")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==1) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE), Agg_pe5_ind = mean(PX5, na.rm=TRUE)) %>%
  mutate(cenREGION = 1)

data_out_mww5 <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1", "PX5")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==2) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE), Agg_pe5_ind = mean(PX5, na.rm=TRUE)) %>%
  mutate(cenREGION =2)

data_out_new5 <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1", "PX5")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==3) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE), Agg_pe5_ind = mean(PX5, na.rm=TRUE)) %>%
  mutate(cenREGION =3)

data_out_sw5 <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1", "PX5")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==4) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE), Agg_pe5_ind = mean(PX5, na.rm=TRUE)) %>%
  mutate(cenREGION=4)

data_outw5 <- rbind(data_out_westw5,data_out_mww5, data_out_new5,data_out_sw5) %>% mutate(group = industry) %>% select(-industry)


Bartik_bench_outw5 <- merge(region_dataw5, data_outw5) %>%
  drop_na() %>%
  group_by(cenREGION, survey_date) %>%
  arrange(survey_date) %>%
  summarise(Bartik_bench_out = sum(prop*Agg_pe_ind),Bartik5_bench_out = sum(prop*Agg_pe5_ind)) %>%
  mutate(REGION = cenREGION)


Bartik_outw5 <- merge(region_CPS78w5, data_outw5) %>%
  drop_na() %>%
  group_by(cenREGION, survey_date)%>%
  arrange(survey_date)

Bartik_outw5 <- Bartik_outw5 %>%
  group_by(cenREGION, survey_date) %>%
  summarise(Bartik_out = sum(prop*Agg_pe_ind), Bartik5_out = sum(prop*Agg_pe5_ind)) %>%
  mutate(REGION = cenREGION)

panel_outw5 <- merge(paneldataw5, Bartik_outw5) %>% panel(~REGION + survey_date)
panel_bench_outw5 <- merge(paneldataw5, Bartik_bench_outw5) %>% panel(~REGION + survey_date)

est_outw5 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe + pe5  ~ Bartik_out + Bartik5_out  , panel_outw5)
est_bench_outw5<- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe + pe5  ~ Bartik_bench_out + Bartik5_bench_out , panel_bench_outw5)
table5_1w5 <- etable(est_bench_outw5, est_outw5, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe","Bartik_bench_out" = "Bartik", "Bartik5_bench_out" = "Bartik5",  "Bartik5_out" = "Bartik5",   "Bartik_out" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table5_2w5 <- etable(est_bench_outw5, est_outw5, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe","yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))


kbl(table5_1w5, "latex", booktabs = T, caption = "2SLS with short and long-expectations: first stage", col.names = c("survey short pe","survey long pe", "CPS78 short pe", "CPS78 long pe"), linesep = "\\addlinespace", label = "2sls:shortandlong:stage1") %>%
  kable_styling(latex_options = c("striped","scale_down")) %>%
  footnote(general = "Survey is the benchmark shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS." , threeparttable = T) %>%
  save_kable("tables/base1and5yearStage1.tex")

kbl(table5_2w5, "latex", booktabs = T, caption = "2SLS with short and long-expectations: coefficient estimates", col.names = c("survey", "CPS78"), linesep = "\\addlinespace", label = "2sls:shortandlong:stage2") %>%
  kable_styling(latex_options = "striped") %>%
  footnote(general = "Survey is the benchmark shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS." , threeparttable = T) %>%
  save_kable("tables/base1and5year.tex")



