# generate the bench table with survey and CPS 78 shares + applying split-sample Jackknife bias correction.
# outputs: regiondataout.csv, base1yearStage1, base1year, biascorrections, redform.pdf, firststage.pdf, redform_cps.pdf, firststage_cps.pdf.

library(tidyverse)
library(zoo)
library(gridExtra)
library(fredr)
library(ggplot2)
library(bartik.weight)
library(ipumsr)
library(kableExtra)
library(fixest)


setwd("~/Dropbox/Cloud/Research/panel expectations/make-panelexpects")
cps78 <- read_csv("data/cps_00002.csv")
data_globalCPS <- read_csv("data/dataBuild/globaldata.csv")
paneldata <-  read_csv("data/dataBuild/paneldata.csv") %>%
  mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
  panel(~REGION + survey_date)
region_data <- read_csv("data/dataBuild/regiondata.csv") %>%
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


region_CPS78 <- local_dataCPS78 %>%
  select(c( "cenREGION", "group")) %>%
  #drop_na() %>%
  group_by(cenREGION,  group) %>%
  summarise(n=n( )) %>%
  mutate(prop = n/sum(n, na.rm = TRUE)) %>%
  select(-n)
write_csv(region_CPS78,"data/dataBuild/regiondataout.csv")



data_globalCPS <- data_globalCPS %>%
  mutate(group = industry) %>%
  select(-industry)


Bartik_CPS78 <- merge(region_CPS78, data_globalCPS) %>%
  drop_na() %>%
  group_by(cenREGION, survey_date)%>%
  arrange(survey_date)

Bartik_CPS78 <- Bartik_CPS78 %>%
  group_by(cenREGION, survey_date) %>%
  summarise(Bartik_cps = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = cenREGION)



panel_cps <- merge(paneldata, Bartik_CPS78) %>% panel(~REGION + survey_date)
write_csv(panel_cps,"data/dataBuild/panelcps.csv")

est_cps <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_cps  , panel_cps)
est_bench <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik  , paneldata)
table_1 <- etable(est_bench, est_cps, vcov = "DK", dict = c("Bartik_cps" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_2 <- etable(est_bench, est_cps, vcov = "DK", dict = c("yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))


kbl(table_1, "latex", booktabs = T, caption = "2SLS: first stage", col.names = c("survey", "CPS78"), linesep = "\\addlinespace", label = "table:2sls:stage1:noloo") %>%
  kable_styling(latex_options = "striped") %>%
  footnote(general = "Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS." , threeparttable = T) %>%
  save_kable("tables/base1yearStage1noleaveoneout.tex")

kbl(table_2, "latex", booktabs = T, caption = "2SLS: coefficient estimates", col.names = c("survey", "CPS78"), linesep = "\\addlinespace") %>%
  kable_styling(latex_options = "striped") %>%
  footnote(general = "Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS." , threeparttable = T) %>%
  save_kable("tables/base1yearnooneleaveout.tex")

# Leave one out, Bartik.

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


Bartik_bench_out <- merge(region_data, data_out) %>%
  drop_na() %>%
  group_by(cenREGION, survey_date) %>%
  arrange(survey_date) %>%
  summarise(Bartik_bench_out = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = cenREGION)


Bartik_out <- merge(region_CPS78, data_out) %>%
  drop_na() %>%
  group_by(cenREGION, survey_date)%>%
  arrange(survey_date)

Bartik_out <- Bartik_out %>%
  group_by(cenREGION, survey_date) %>%
  summarise(Bartik_out = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = cenREGION)

panel_out <- merge(paneldata, Bartik_out) %>% panel(~REGION + survey_date)
write_csv(panel_out,"data/dataBuild/panelcpsout.csv")
panel_bench_out <- merge(paneldata, Bartik_bench_out) %>% panel(~REGION + survey_date)
write_csv(panel_bench_out,"data/dataBuild/panelbenchout.csv")

est_out <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panel_out)
est_bench_out<- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panel_bench_out)
table_out_1 <- etable(est_bench_out, est_out, vcov = "DK", dict = c("Bartik_bench_out" = "Bartik",   "Bartik_out" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_out_2 <- etable(est_bench_out, est_out, vcov = "DK", dict = c("yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))

kbl(table_out_1, "latex", booktabs = T, caption = "2SLS: first stage", col.names = c("survey", "CPS78"), linesep = "\\addlinespace", label = "base:out:2sls:stage1") %>%
  kable_styling(latex_options = "striped") %>%
  footnote(general = "Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.", number = c("Signif. codes: * = .05; ** = .01; *** = .001.") , threeparttable = T) %>%
  save_kable("tables/base1yearStage1.tex")

kbl(table_out_2, "latex", booktabs = T, caption = "2SLS: coefficient estimates", col.names = c("survey", "CPS78"), linesep = "\\addlinespace", label = "base:out:2sls:stage2") %>%
  kable_styling(latex_options = "striped") %>%
  footnote(general = "Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS." , number = c("Signif. codes: * = .05; ** = .01; *** = .001."), threeparttable = T) %>%
  save_kable("tables/base1year.tex")

# First-stage and reduced-form plots.

rf_reg <- feols(RegInf ~  Bartik , data =paneldata)
Fstage_reg <- feols(pe ~ Bartik, data = paneldata)

pRF <- ggplot(data = paneldata, aes(x=Bartik, y=RegInf)) + geom_point() + geom_abline(slope = coefficients(rf_reg)[[2]], intercept = coefficients(rf_reg)[[1]], col = rgb(.541,.2,.141), size = 1.2) +
  labs(y = ("reg. inflation"), x = "Bartik (mich.)")+
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 34,colour = rgb(.26, .17, .16)), legend.position = "none")  

pF1 <- ggplot(data = paneldata, aes(x=Bartik, y=pe)) + geom_point() + geom_abline(slope = coefficients(Fstage_reg)[[2]], intercept = coefficients(Fstage_reg)[[1]], col = rgb(.541,.2,.141), size = 1.2) +
  labs(y = ("pe (survey)"), x = "Bartik (mich.)")+
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 34,colour = rgb(.26, .17, .16)), legend.position = "none")  


rf_cps <- feols(RegInf ~  Bartik_cps , data =panel_cps)
Fstage_cps <- feols(pe ~ Bartik_cps, data = panel_cps)

pRF_cps <- ggplot(data = panel_cps, aes(x=Bartik_cps, y=RegInf)) + geom_point() + geom_abline(slope = coefficients(rf_cps)[[2]], intercept = coefficients(rf_cps)[[1]], col = rgb(.541,.2,.141), size = 1.2) +
  labs(y = ("reg. inflation"), x = "Bartik (CPS)")+
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 34,colour = rgb(.26, .17, .16)), legend.position = "none")  

pF1_cps <- ggplot(data = panel_cps, aes(x=Bartik_cps, y=pe)) + geom_point() + geom_abline(slope = coefficients(Fstage_cps)[[2]], intercept = coefficients(Fstage_cps)[[1]], col = rgb(.541,.2,.141), size = 1.2) +
  labs(y = ("pe (survey)"), x = "Bartik (CPS)")+
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 34,colour = rgb(.26, .17, .16)), legend.position = "none")  

ggsave("figs/redform.pdf", pRF)
ggsave("figs/firststage.pdf", pF1)
ggsave("figs/redform_cps.pdf", pRF_cps)
ggsave("figs/firststage_cps.pdf", pF1_cps)







# Jacknife correction.

panel_out2 <- panel_out %>%
  mutate(RegSample = case_when(REGION ==1 | REGION ==2 ~ 1, REGION ==3 | REGION ==4 ~ 0),
         TimeSample = case_when(survey_date < as.Date("2000-02-01")~1,survey_date >= as.Date("2000-02-01")~2))
out_bias1 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panel_out2, fsplit = ~TimeSample, vcov = "DK")
out_bias2 <-  feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panel_out2, fsplit = ~RegSample, vcov = "DK")
c_out1 = coefficients(out_bias1)
c_out2 = coefficients(out_bias2)
s61= se( feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panel_out, vcov = "DK"))
adj_out = 2*c_out1[1]-(c_out2[2]+c_out2[3])/2

panel_bench_out2 <- panel_bench_out %>%
  mutate(RegSample = case_when(REGION ==1 | REGION ==2 ~ 1, REGION ==3 | REGION ==4 ~ 0),
         TimeSample = case_when(survey_date < as.Date("2000-02-01")~1,survey_date >= as.Date("2000-02-01")~2))
out_bench_bias1 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panel_bench_out2, fsplit = ~TimeSample, vcov = "DK")
out_bench_bias2 <-  feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panel_bench_out2, fsplit = ~RegSample, vcov = "DK")
c_b_out1 = coefficients(out_bench_bias1)
c_b_out2 = coefficients(out_bench_bias2)
s62= se( feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panel_bench_out, vcov = "DK"))
adj_b_out = 2*c_b_out1[1]-(c_b_out2[2]+c_b_out2[3])/2


adjT <- as.tibble(c(  round(adj_b_out,4),  round(s62[1],4))) %>%
  cbind(c( round(adj_out,4), round(s61[1],4))) 

colnames(adjT) <- c("survey shares", "CPS78 shares")
rownames(adjT) <- c("coeff.", "se ")
kbl(adjT, "latex", booktabs = T, caption = "Bias Correction", label = "bias:out:2sls:stage2") %>%
  kable_styling(latex_options = "striped") %>%
  footnote(general = "Applies the split-sample jacknife bias correction. The column ``survey shares'' computes shares from Michigan survey,``CPS78 shares'' uses the CPS 1978.1 shares." , threeparttable = T) %>%
  save_kable("tables/biascorrections.tex")




