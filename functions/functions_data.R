# Defines demographic shares, Michigan;
classify_michigan <- function(data) {
  data %>%
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
}

classify_cps <- function(data) {
  data %>%
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
}

process_region_data <- function(data, date_filter = "1977-12-01") {
  data %>%
    select("survey_date", "REGION", "industry", "PX1") %>%
    group_by(REGION, survey_date, industry) %>%
    summarise(n = n()) %>%
    mutate(prop = n / sum(n, na.rm = TRUE)) %>%
    filter(survey_date > as.Date(date_filter))
}

# process_region_data_cps <- function(data) {
#   data %>%
#   select(c("survey_date", "cenREGION", "group", "PX1")) %>%
#   #drop_na() %>%
#   group_by(cenREGION, group) %>%
#   summarise(n=n( )) %>%
#   mutate(prop = n/sum(n, na.rm = TRUE)) #%>%
#     #rename(industry = group) %>%
#   #select(-n)
#   
# }

process_region_data_cps <- function(data) {
  data %>%
    select( "cenREGION", "group") %>%
    group_by(cenREGION,  group) %>%
    summarise(n = n()) %>%
    mutate(prop = n / sum(n, na.rm = TRUE)) 
}

transform_region_data <- function(region_data) {
  region_data %>%
    select(-n) %>%
    mutate(industry = stringr::str_glue("t{survey_date}_sh_ind_{industry}")) %>%
    tidyr::spread(industry, prop, fill = 0)
}

transform_region_data_cps <- function(reg_data) {
  reg_data %>%
    select(-n) %>%
    mutate(group = stringr::str_glue("t{survey_date}_sh_ind_{group}")) %>%
    tidyr::spread(group, prop, fill = 0)
}

process_global_data <- function(data, date_filter = "1977-12-01", NoLong = FALSE) {
  if (NoLong){
    data %>%
      select("survey_date", "REGION", "industry", "PX1") %>%
      group_by(survey_date, industry) %>%
      summarise(Agg_pe_ind = mean(PX1, na.rm = TRUE)) %>%
      filter(survey_date > as.Date(date_filter))
  }
  else {
  data %>%
    select("survey_date", "REGION", "industry", "PX1", "PX5") %>%
    group_by(survey_date, industry) %>%
    summarise(Agg_pe_ind = mean(PX1, na.rm = TRUE),Agg_pe5_ind = mean(PX5, na.rm = TRUE)) %>%
    filter(survey_date > as.Date(date_filter)) }
}

process_global_data_CPS <- function(data, date_filter = "1977-12-01", NoLong = FALSE) {
  if (NoLong){
    data %>%
      select("survey_date", "cenREGION", "group", "PX1") %>%
      group_by(survey_date, group) %>%
      summarise(Agg_pe_ind = mean(PX1, na.rm = TRUE)) %>%
      filter(survey_date > as.Date(date_filter))
  }
  else {
    data %>%
      select("survey_date", "cenREGION", "group", "PX1", "PX5") %>%
      group_by(survey_date, group) %>%
      summarise(Agg_pe_ind = mean(PX1, na.rm = TRUE),Agg_pe5_ind = mean(PX5, na.rm = TRUE)) %>%
      filter(survey_date > as.Date(date_filter)) }
}

calculate_bartik <- function(region_data, data_global, lag = FALSE, NoLong = FALSE) {
  regions <- unique(region_data$REGION)
  bartik_data <- data.frame()

  for (region in regions) {
    region_data_filtered <- filter(region_data, REGION == region)
    merged_data <- merge(region_data_filtered, data_global)

    if (lag) {
      merged_data <- merged_data %>%
        mutate(lagprop = lag(prop, 12))
    }
    
    if (NoLong){
      bartik_region <- merged_data %>%
        group_by(survey_date) %>%
        summarise(Bartik = sum(prop * Agg_pe_ind, na.rm = FALSE)) %>%
        mutate(REGION = region)
    }
    else {bartik_region <- merged_data %>%
      group_by(survey_date) %>%
      summarise(Bartik = sum(prop * Agg_pe_ind, na.rm = FALSE),Bartik5 = sum(prop * Agg_pe5_ind, na.rm = FALSE)) %>%
      mutate(REGION = region) }

    bartik_data <- rbind(bartik_data, bartik_region)
  }

  bartik_data
}

calculate_bartik_original <- function(region_data, data_global, NoLong = FALSE) {
  if (NoLong) {
    Bartik_dataW <- merge(filter(region_data, REGION == 1), data_global) %>%
      group_by(survey_date) %>%
      summarise(Bartik = sum(prop * Agg_pe_ind)) %>%
      mutate(REGION = 1)
    
    Bartik_dataMW <- merge(filter(region_data, REGION == 2), data_global) %>%
      group_by(survey_date) %>%
      summarise(Bartik = sum(prop * Agg_pe_ind)) %>%
      mutate(REGION = 2)
    
    Bartik_dataNE <- merge(filter(region_data, REGION == 3), data_global) %>%
      group_by(survey_date) %>%
      summarise(Bartik = sum(prop * Agg_pe_ind)) %>%
      mutate(REGION = 3)
    
    Bartik_dataS <- merge(filter(region_data, REGION == 4), data_global) %>%
      group_by(survey_date) %>%
      summarise(Bartik = sum(prop * Agg_pe_ind)) %>%
      mutate(REGION = 4)
  }
  else {
  Bartik_dataW <- merge(filter(region_data, REGION == 1), data_global) %>%
    group_by(survey_date) %>%
    summarise(Bartik = sum(prop * Agg_pe_ind), Bartik5 = sum(prop * Agg_pe5_ind)) %>%
    mutate(REGION = 1)
  
  Bartik_dataMW <- merge(filter(region_data, REGION == 2), data_global) %>%
    group_by(survey_date) %>%
    summarise(Bartik = sum(prop * Agg_pe_ind), Bartik5 = sum(prop * Agg_pe5_ind)) %>%
    mutate(REGION = 2)
  
  Bartik_dataNE <- merge(filter(region_data, REGION == 3), data_global) %>%
    group_by(survey_date) %>%
    summarise(Bartik = sum(prop * Agg_pe_ind), Bartik5 = sum(prop * Agg_pe5_ind)) %>%
    mutate(REGION = 3)
  
  Bartik_dataS <- merge(filter(region_data, REGION == 4), data_global) %>%
    group_by(survey_date) %>%
    summarise(Bartik = sum(prop * Agg_pe_ind), Bartik5 = sum(prop * Agg_pe5_ind)) %>%
    mutate(REGION = 4) }
  
  Bartik_data <- rbind(Bartik_dataW, Bartik_dataMW, Bartik_dataNE, Bartik_dataS)
  
  Bartik_data
}




process_continuous_controls <- function(michigan, gasPrices) {
  ContControls <- michigan %>%
    group_by(REGION, survey_date) %>%
    summarise(ye = mean(INEX, na.rm = TRUE), gas = mean(GASPX2, na.rm = TRUE)) %>%
    drop_na() %>%
    merge(gasPrices) %>%
    mutate(gasExp = gas / GasP) %>%
    select(-c("gas", "GasP"))
  ContControls
}

process_discrete_controls <- function(michigan) {
  # Create a list to hold the discrete controls
  DiscControls_list <- list()
  
  # Define the variables to group by
  variables <- c("PAGO", "RINC", "BAGO", "BEXP",   "PEXP",  "UNEMP")
  new_names <- c("FinBetterLast1", "RincUp", "BizGood", "BizExGood",   "FinBetterNext1",  "UNEMPGood")
  
  # Iterate through the variables and compute the discrete controls
  for (i in 1:length(variables)) {
    var <- variables[i]
    new_name <- new_names[i]
    if (var == "UNEMP") {
      DiscControls_list[[i]] <- michigan %>%
        group_by(REGION, survey_date, .data[[var]]) %>%
        summarise(n = n(), .groups = 'drop') %>%
        group_by(REGION, survey_date) %>%
        mutate(!!new_name := n / sum(n)) %>%
        filter(.data[[var]] == 3) %>%
        select(-c(var, "n"))
    } else {
      DiscControls_list[[i]] <- michigan %>%
        group_by(REGION, survey_date, .data[[var]]) %>%
        summarise(n = n(), .groups = 'drop') %>%
        group_by(REGION, survey_date) %>%
        mutate(!!new_name := n / sum(n)) %>%
        filter(.data[[var]] == 1) %>%
        select(-c(var, "n"))
    }
  }
  
  # Merge all discrete controls
  DiscControls <- Reduce(left_join, DiscControls_list)
  DiscControls
}

merge_panel_data <- function(master_data, bartik_data, unEm, controls) {
  panel_data <- merge(master_data, bartik_data) %>%
    merge(unEm) %>%
    full_join(controls) %>%
    filter(!is.na(pe))
  panel_data
}

create_panel_cps <- function(region_data, data_global, paneldata, survey_date_column, NoLong = FALSE) {
 
  if (NoLong) {
    
    Bartik_CPS <- merge(region_data, data_global) %>%
      drop_na() %>%
      group_by(cenREGION, !!sym(survey_date_column)) %>%
      arrange(!!sym(survey_date_column)) %>%
      group_by(cenREGION, !!sym(survey_date_column)) %>%
      summarise(Bartik_cps = sum(prop*Agg_pe_ind)) %>%
      mutate(REGION = cenREGION)
  }
  else {Bartik_CPS <- merge(region_data, data_global) %>%
    drop_na() %>%
    group_by(cenREGION, !!sym(survey_date_column)) %>%
    arrange(!!sym(survey_date_column)) %>%
    group_by(cenREGION, !!sym(survey_date_column)) %>%
    summarise(Bartik_cps = sum(prop*Agg_pe_ind), Bartik5_cps = sum(prop*Agg_pe5_ind)) %>%
    mutate(REGION = cenREGION)}
  
  panel_cps <- merge(paneldata, Bartik_CPS)
  
  return(panel_cps)
}



read_and_prepare_cps_data <- function(cps_file, global_data_file, panel_data_file, region_data_file, local_data_big_file) {
  cps78 <- read_csv(cps_file)
  data_globalCPS <- read_csv(global_data_file)
  paneldata <- read_csv(panel_data_file) %>%
    mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
    panel(~REGION + survey_date)
  region_data <- read_csv(region_data_file) %>%
    mutate(cenREGION = REGION, group = industry) %>%
    select(-c(n, industry, REGION))
  local_dataBig <- read_csv(local_data_big_file)
  
  # Return as a list
  list(cps78 = cps78, data_globalCPS = data_globalCPS, paneldata = paneldata, region_data = region_data, local_dataBig = local_dataBig)
}


calculate_leave_one_out <- function(local_data, region_data, region_cpsdata, paneldata, survey_date_column, NoLong = FALSE) {
  data_out <- NULL
  for (cenREGION in 1:4) {
    if (NoLong) {
      data_out_region <- local_data %>%
        select(survey_date_column, "REGION", "industry", "PX1") %>%
        group_by(!!sym(survey_date_column), industry) %>%
        filter(!REGION == cenREGION) %>%
        summarise(Agg_pe_ind = mean(PX1, na.rm = TRUE)) %>%
        mutate(cenREGION = cenREGION)
    } else {
      data_out_region <- local_data %>%
        select(survey_date_column, "REGION", "industry", "PX1", "PX5") %>%
        group_by(!!sym(survey_date_column), industry) %>%
        filter(!REGION == cenREGION) %>%
        summarise(Agg_pe_ind = mean(PX1, na.rm = TRUE), Agg_pe5_ind = mean(PX5, na.rm = TRUE)) %>%
        mutate(cenREGION = cenREGION)
    }
    data_out <- rbind(data_out, data_out_region)
  }



  data_out <- data_out %>%
    mutate(group = industry) %>%
    select(-industry)

  if (NoLong) {

    Bartik_bench_out <- merge(region_data, data_out) %>%
      #drop_na() %>%
      group_by(cenREGION, !!sym(survey_date_column)) %>%
      arrange(!!sym(survey_date_column)) %>%
      summarise(Bartik_bench_out = sum(prop*Agg_pe_ind)) %>%
      mutate(REGION = cenREGION)



    Bartik_out <- merge(region_cpsdata, data_out) %>%
      drop_na() %>%
      group_by(cenREGION, !!sym(survey_date_column)) %>%
      arrange(!!sym(survey_date_column)) %>%
      summarise(Bartik_out = sum(prop*Agg_pe_ind)) %>%
      mutate(REGION = cenREGION)

  }
  else {Bartik_bench_out <- merge(region_data, data_out) %>%
    #drop_na() %>%
    group_by(cenREGION, !!sym(survey_date_column)) %>%
    arrange(!!sym(survey_date_column)) %>%
    summarise(Bartik_bench_out = sum(prop*Agg_pe_ind), Bartik5_bench_out = sum(prop*Agg_pe5_ind)) %>%
    mutate(REGION = cenREGION)



  Bartik_out <- merge(region_cpsdata, data_out) %>%
    drop_na() %>%
    group_by(cenREGION, !!sym(survey_date_column)) %>%
    arrange(!!sym(survey_date_column)) %>%
    summarise(Bartik_out = sum(prop*Agg_pe_ind), Bartik5_out = sum(prop*Agg_pe5_ind)) %>%
    mutate(REGION = cenREGION)}

  panel_out <- merge(paneldata, Bartik_out,)
  panel_bench_out <- merge(paneldata, Bartik_bench_out)



  return(list(panel_out = panel_out, panel_bench_out = panel_bench_out))
}




