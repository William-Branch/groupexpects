library(tidyverse)
library(zoo)
library(gridExtra)
library(fredr)
library(ggplot2)
library(tempdisagg)
library(tsibble)


fredr_set_key("1eb4018cc8ead6ac6445763becf7bf42")

# Michigan survey data;

#michigan <- read_csv("/Users/billbranch/Dropbox/Cloud/Research/panel expectations/data/CSV/Michigan_220524.csv")
michigan <- read_csv("data/Michigan_220524.csv")
# state-cpi from Hazell, Herreno, Nakamura, Steinsson;

#state_cpi <- read_csv("/Users/billbranch/Dropbox/Cloud/Research/panel expectations/data/csv/statecpi_beta.csv")
state_cpi <- read_csv("data/statecpi_beta.csv")

inflation <- state_cpi %>%
  mutate(date = as.yearqtr(paste0(state_cpi$year, "-", state_cpi$quarter)))

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


# REGION CPI inflation: monthly begins 1987



infNE <- fredr(series_id = "CUUR0100SA0", units = "pc1") %>%
  filter(!is.na(value))
infS <- fredr(series_id = "CUUR0300SA0", units = "pc1") %>%
  filter(!is.na(value))
infMW <- fredr(series_id = "CUUR0200SA0", units = "pc1") %>%
  filter(!is.na(value))
infW <- fredr(series_id = "CUUR0400SA0", units = "pc1") %>%
  filter(!is.na(value))

RegionInfW <- mutate(infW, REGION = 1) 
RegionInfW <- RegionInfW[c("date", "value", "REGION")]
RegionInfMW <- mutate(infMW, REGION = 2) 
RegionInfMW <- RegionInfMW[c("date", "value", "REGION")]
RegionInfNE <- mutate(infNE, REGION = 3) 
RegionInfNE <- RegionInfNE[c("date", "value", "REGION")]
RegionInfS <- mutate(infS, REGION = 4) 
RegionInfS <- RegionInfS[c("date", "value", "REGION")]
RegionInf <- merge(merge(merge(RegionInfW, RegionInfMW, all.x = TRUE, all.y = TRUE),RegionInfNE, all.x = TRUE, all.y = TRUE), RegionInfS, all.x = TRUE, all.y = TRUE)
RegionInf <- rename(RegionInf, survey_date = date)
RegionInf <- rename(RegionInf, RegInf = value)


# get region level inflation data from state-CPI;

NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
             "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
            "KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
            "HI","OR","WA")
W.ref <- c(W.name,W.abrv)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref)

NE_inflation <- filter(inflation, state %in% NE.name)
aggregate(pi ~ date, NE_inflation, mean)

region_inflation <- aggregate( pi ~ date, filter(inflation, state %in% NE.name), mean)

weighted.geomean <- function(x, w, ...)
{
  return(prod(x^w, ...)^(1/sum(w)))
}

# Expenditure weights



West_params <- list(
  series_id = c("AKPCE", "CAPCE", "HIPCE", "ORPCE", "WAPCE", "COPCE", "UTPCE"),
  observation_date = as.Date("1998-01-01")
) 
MidWest_params <- list(
  series_id = c("ILPCE", "MIPCE", "MOPCE", "MNPCE", "OHPCE", "WIPCE", "INPCE", "KSPCE"),
  observation_date = as.Date("1998-01-01")
) 
South_params <- list(
  series_id = c("DCPCE", "FLPCE", "GAPCE", "MDPCE", "TNPCE", "TXPCE", "ALPCE", "ARPCE", "LAPCE", "MSPCE", "NCPCE", "OKPCE", "SCPCE", "VAPCE"),
  observation_date = as.Date("1998-01-01")
) 
NE_params <- list(
  series_id = c("MAPCE", "NJPCE", "NYPCE", "PAPCE", "CTPCE"),
  observation_date = as.Date("1998-01-01")
) 

West_exp <- pmap_dfr(
  .l = West_params,
  .f = ~ fredr(series_id = .x, observation_start = .y, observation_end = .y)
)
MidWest_exp <- pmap_dfr(
  .l = MidWest_params,
  .f = ~ fredr(series_id = .x, observation_start = .y, observation_end = .y)
)
South_exp <- pmap_dfr(
  .l = South_params,
  .f = ~ fredr(series_id = .x, observation_start = .y, observation_end = .y)
)
NE_exp <- pmap_dfr(
  .l = NE_params,
  .f = ~ fredr(series_id = .x, observation_start = .y, observation_end = .y)
)

west_pre_states = c("AKPCE", "CAPCE", "HIPCE", "ORPCE", "WAPCE")
west_post_weights <- transmute(West_exp,
                               weights = value/sum(value) )
west_pre_weights <- transmute(filter(West_exp, series_id %in% west_pre_states),
                              weights = value/sum(value))

Midwest_pre_states = c("ILPCE", "MIPCE", "MOPCE", "MNPCE", "OHPCE", "WIPCE")
Midwest_post_weights <- transmute(MidWest_exp,
                                  weights = value/sum(value) )
Midwest_pre_weights <- transmute(filter(MidWest_exp, series_id %in% Midwest_pre_states),
                                 weights = value/sum(value))

South_pre_states = c("DCPCE", "FLPCE", "GAPCE", "GAPCE", "MDPCE", "TNPCE", "TXPCE")
South_1989_states = c("DCPCE", "FLPCE", "GAPCE", "MDPCE", "TNPCE", "TXPCE", "ALPCE", "ARPCE", "LAPCE", "MSPCE", "NCPCE", "SCPCE", "VAPCE")
South_post_weights <- transmute(South_exp,
                                weights = value/sum(value) )
South_pre_weights <- transmute(filter(South_exp, series_id %in% South_pre_states),
                               weights = value/sum(value))
South_1989_weights <- transmute(filter(South_exp, series_id %in% South_1989_states),
                                weights = value/sum(value))

NE_pre_states = c("MAPCE", "NJPCE", "NYPCE", "PAPCE")
NE_post_weights <- transmute(NE_exp,
                             weights = value/sum(value) )
NE_pre_weights <- transmute(filter(NE_exp, series_id %in% NE_pre_states),
                            weights = value/sum(value))


west_inflation <- filter(inflation, state %in% W.name)
west_pre <- filter(west_inflation, date>= "1978 Q1" & date< "1987 Q4")
west_post <- filter(west_inflation, date>= "1988 Q4")
midwest_inflation <- filter(inflation, state %in% MW.name)
midwest_pre <- filter(midwest_inflation, date>= "1978 Q1" & date<= "1987 Q4")
south_inflation <- filter(inflation, state %in% S.name)
south_pre <- filter(south_inflation, date>= "1978 Q1" & date<= "1987 Q4")
ne_inflation <- filter(inflation, state %in% NE.name)
ne_pre <- filter(ne_inflation, date>= "1978 Q1" & date<= "1987 Q4")

pre_dates <-  transmute(filter(west_pre, state == "Alaska"), dates = date)
Tp = 36



select(west_pre, pi)

west_pre_inf <-  0
midwest_pre_inf <-  0
south_pre_inf <- 0
ne_pre_inf <- 0

for(i in 1:Tp){
  west_pre_inf[i] = weighted.mean(select(filter(west_pre, date %in% pre_dates[[i,1]]), pi), west_pre_weights)
  midwest_pre_inf[i] = weighted.mean(select(filter(midwest_pre, date %in% pre_dates[[i,1]]), pi), Midwest_pre_weights)
  south_pre_inf[i] = weighted.mean(select(filter(south_pre, date %in% pre_dates[[i,1]]), pi), South_pre_weights)
  ne_pre_inf[i] = weighted.mean(select(filter(ne_pre, date %in% pre_dates[[i,1]]), pi), NE_pre_weights)
  
}




predates = west_pre[["date"]][1:36]



west_cpi_inf <- tibble(time = as.yearqtr(predates), west_inf = west_pre_inf) 
west_cpi_inf$time <- yearquarter(west_cpi_inf$time)
midwest_cpi_inf <- tibble(time = as.yearqtr(predates), midwest_inf = midwest_pre_inf) 
midwest_cpi_inf$time <- yearquarter(midwest_cpi_inf$time)
south_cpi_inf <- tibble(time = as.yearqtr(predates), south_inf = south_pre_inf) 
south_cpi_inf$time <- yearquarter(south_cpi_inf$time)
ne_cpi_inf <- tibble(time = as.yearqtr(predates), ne_inf = ne_pre_inf) 
ne_cpi_inf$time <- yearquarter(ne_cpi_inf$time)



west_ts <- west_cpi_inf %>%
  as_tsibble( index = 'time')
midwest_ts <- midwest_cpi_inf %>%
  as_tsibble( index = 'time')
south_ts <- south_cpi_inf %>%
  as_tsibble( index = 'time')
ne_ts <- ne_cpi_inf %>%
  as_tsibble( index = 'time')

mW <- td(west_ts ~ 1, to = "monthly")
mMW <- td(midwest_ts ~ 1, to = "monthly")
mS <- td(south_ts ~ 1, to = "monthly")
mNE <- td(ne_ts ~ 1, to = "monthly")

WestMonth <- predict(mW) %>% mutate(REGION = 1) 
MidwestMonth <- predict(mMW) %>% mutate(REGION = 2)
SouthMonth <- predict(mS) %>% mutate(REGION = 4)
NEMonth <- predict(mNE) %>% mutate(REGION = 3)

WestMonth <- as_tibble(WestMonth)
MidwestMonth <- as_tibble(MidwestMonth)
SouthMonth <- as_tibble(SouthMonth)
NEMonth <- as_tibble(NEMonth)

infMonth <- merge(WestMonth,MidwestMonth, all.x = TRUE, all.y = TRUE) %>%
  merge(SouthMonth, all.x = TRUE, all.y = TRUE) %>%
  merge(NEMonth, all.x = TRUE, all.y = TRUE) %>%
  mutate(RegInf = 4*value) %>%
  mutate(survey_date = time) %>%
  select(c("survey_date", "RegInf", "REGION"))


RegionInfState <- merge(infMonth, filter(RegionInf, survey_date > "1986-12-01"), all.x = TRUE, all.y = TRUE)

# REGION unemployment rates

unAgg <- fredr(series_id = "UNRATE") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, AggUNRATE = value)%>%
  filter(survey_date > as.Date("1977-12-01"))

unW <- fredr(series_id = "CWSTUR") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, UNRATE = value) %>%
  mutate(REGION = 1) %>%
  merge(unAgg)%>%
  filter(survey_date > as.Date("1977-12-01"))

unMW <- fredr(series_id = "CMWRUR") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, UNRATE = value) %>%
  mutate(REGION = 2) %>%
  merge(unAgg) %>%
  filter(survey_date > as.Date("1977-12-01"))

unNE <- fredr(series_id = "CNERUR") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, UNRATE = value) %>%
  mutate(REGION = 3) %>%
  merge(unAgg) %>%
  filter(survey_date > as.Date("1977-12-01"))

unS <- fredr(series_id = "CSOUUR") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, UNRATE = value) %>%
  mutate(REGION = 4) %>%
  merge(unAgg) %>%
  filter(survey_date > as.Date("1977-12-01"))

unEm <- rbind(unW, unMW, unNE, unS)

#Region unleaded gas prices;

gasW <- fredr(series_id = "APU040074714") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, GasP = 100*value) %>%
  mutate(REGION = 1) 
#filter(survey_date > as.Date("1986-12-01"))

gasMW <- fredr(series_id = "APU020074714") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, GasP = 100*value) %>%
  mutate(REGION = 2) 

gasNE <- fredr(series_id = "APU010074714") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, GasP = 100*value) %>%
  mutate(REGION = 3) 

gasS <- fredr(series_id = "APU030074714") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, GasP = 100*value) %>%
  mutate(REGION = 4) 

gasPrices <- rbind(gasW, gasMW, gasNE, gasS)


# master-data set for Rotemberg weights, for each of the 3 Michigan sets;
# note PX5 shortens data set to 1990;

master_dataBig <- michigan %>%
  group_by(REGION,survey_date) %>%
  summarise(pe = mean(PX1, na.rm = TRUE))
master_data <- master_dataBig[c("survey_date", "pe", "REGION")]

master_dataBig_First <- michiganFirst %>%
  group_by(REGION,survey_date) %>%
  summarise(pe = mean(PX1, na.rm = TRUE))
master_data_First <- master_dataBig_First[c("survey_date", "pe", "REGION")]

master_dataBig_Small <- michiganSmall %>%
  group_by(REGION,survey_date) %>%
  summarise(pe = mean(PX1, na.rm = TRUE))
master_data_Small <- master_dataBig_Small[c("survey_date", "pe", "REGION")]




# Regional Inflation and mean inflation expectations, i.e. the DGP states;

master_data <- full_join(master_data,RegionInfState)
master_data_First <- full_join(master_data_First,RegionInfState)
master_data_Small <- full_join(master_data_Small, RegionInfState)

# construct "industries";

# first, main data set;

local_dataBig <- michigan %>%
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

# second, first-time respondents only;

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

# Construct local data set of industry shares by region and date;

region_data <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  drop_na() %>%
  group_by(REGION, survey_date, industry) %>%
  summarise(n=n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(survey_date > as.Date("1977-12-01"))

region_data_First <- local_dataBig_First %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  drop_na() %>%
  group_by(REGION, survey_date, industry) %>%
  summarise(n=n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(survey_date > as.Date("1977-12-01"))

region_data_Small <- local_dataBig_Small %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  drop_na() %>%
  group_by(REGION, survey_date, industry) %>%
  summarise(n=n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(survey_date > as.Date("1977-12-01"))

data_local <-  region_data %>%
  select(-n) %>%
  mutate(industry = str_glue("t{survey_date}_sh_ind_{industry}")) %>%
  spread(industry, prop, fill = 0) 

data_local_First <-  region_data_First %>%
  select(-n) %>%
  mutate(industry = str_glue("t{survey_date}_sh_ind_{industry}")) %>%
  spread(industry, prop, fill = 0) 

data_local_Small <-  region_data_Small %>%
  select(-n) %>%
  mutate(industry = str_glue("t{survey_date}_sh_ind_{industry}")) %>%
  spread(industry, prop, fill = 0) 

# Construct global data set of aggregate industry inflation expectations at 1 and 5 year horizons;

data_global <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  drop_na() %>%
  group_by(survey_date, industry) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  filter(survey_date > as.Date("1977-12-01"))

data_global_First <- local_dataBig_First %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  drop_na() %>%
  group_by(survey_date, industry) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  filter(survey_date > as.Date("1977-12-01"))

data_global_Small <- local_dataBig_Small %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  drop_na() %>%
  group_by(survey_date, industry) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  filter(survey_date > as.Date("1977-12-01"))



# Construct instruments;

Bartik_dataW <- merge(filter(region_data,REGION==1), data_global) %>%
  group_by(survey_date) %>%
  summarise(Bartik = sum(prop*Agg_pe_ind))%>%
  mutate(REGION =1)


Bartik_dataMW <- merge(filter(region_data,REGION==2), data_global) %>%
  group_by(survey_date) %>%
  summarise(Bartik = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION =2)

Bartik_dataNE <- merge(filter(region_data,REGION==3), data_global) %>%
  group_by(survey_date) %>%
  summarise(Bartik = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = 3)

Bartik_dataS <- merge(filter(region_data,REGION==4), data_global) %>%
  group_by(survey_date) %>%
  summarise(Bartik = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = 4)

Bartik_data <- rbind(Bartik_dataW, Bartik_dataMW, Bartik_dataNE, Bartik_dataS)

Bartik_dataW_First <- merge(filter(region_data_First,REGION==1), data_global_First) %>%
  group_by(survey_date) %>%
  summarise(Bartik = sum(prop*Agg_pe_ind))%>%
  mutate(REGION =1)


Bartik_dataMW_First <- merge(filter(region_data_First,REGION==2), data_global_First) %>%
  group_by(survey_date) %>%
  summarise(Bartik = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION =2)

Bartik_dataNE_First <- merge(filter(region_data_First,REGION==3), data_global_First) %>%
  group_by(survey_date) %>%
  summarise(Bartik = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = 3)

Bartik_dataS_First <- merge(filter(region_data_First,REGION==4), data_global_First) %>%
  group_by(survey_date) %>%
  summarise(Bartik = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = 4)

Bartik_data_First <- rbind(Bartik_dataW_First, Bartik_dataMW_First, Bartik_dataNE_First, Bartik_dataS_First)

Bartik_dataW_Small <- merge(filter(region_data_Small,REGION==1), data_global_Small) %>%
  group_by(survey_date) %>%
  summarise(Bartik = sum(prop*Agg_pe_ind))%>%
  mutate(REGION =1)


Bartik_dataMW_Small <- merge(filter(region_data_Small,REGION==2), data_global_Small) %>%
  group_by(survey_date) %>%
  summarise(Bartik = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION =2)

Bartik_dataNE_Small <- merge(filter(region_data_Small,REGION==3), data_global_Small) %>%
  group_by(survey_date) %>%
  summarise(Bartik = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = 3)

Bartik_dataS_Small <- merge(filter(region_data_Small,REGION==4), data_global_Small) %>%
  group_by(survey_date) %>%
  summarise(Bartik = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = 4)

Bartik_data_Small <- rbind(Bartik_dataW_Small, Bartik_dataMW_Small, Bartik_dataNE_Small, Bartik_dataS_Small)

panel_data <- merge(master_data, Bartik_data) %>%
  merge(unEm) 

#panel_data <- panel_data[c("REGION", "survey_date", "pe", "pe5", "RegInf", "Bartik", "UNRATE")]

panel_data_First <- merge(master_data_First, Bartik_data_First) %>%
  merge(unEm) 

#panel_data_First <- panel_data_First[c("REGION", "survey_date", "pe", "RegInf", "Bartik", "UNRATE")]

panel_data_Small <- merge(master_data_Small, Bartik_data_Small) %>%
  merge(unEm) 

#panel_data_Small <- panel_data_Small[c("REGION", "survey_date", "pe", "RegInf", "Bartik", "UNRATE")]


# INEX and GASPX@ shortens sample as they appear sporadically;

ContControls <- michigan %>%
  group_by(REGION,survey_date) %>%
  summarise(ye = mean(INEX, na.rm = TRUE), gas = mean(GASPX2, na.rm=TRUE)) %>%
  drop_na()

ContControls <- merge(ContControls, gasPrices) %>%
  mutate(gasExp = gas/GasP) %>%
  select(-c("gas", "GasP"))



DiscControls1 <- michigan %>%
  group_by(REGION,survey_date, PAGO) %>%
  summarise(n=n()) %>%
  group_by(REGION, survey_date) %>%
  mutate(FinBetterLast1 = n/sum(n)) %>%
  filter(PAGO ==1) %>%
  select(-c("PAGO", "n"))


DiscControls2 <- michigan %>%
  group_by(REGION, survey_date, RINC) %>%
  summarise(nr = n()) %>%
  group_by(REGION, survey_date) %>% 
  mutate(RincUp = nr/sum(nr)) %>%
  filter(RINC ==1) %>%
  select(-c("RINC", "nr"))

DiscControls3 <- michigan %>%
  group_by(REGION, survey_date, BAGO) %>%
  summarise(nb = n()) %>%
  group_by(REGION, survey_date) %>% 
  mutate(BizGood = nb/sum(nb)) %>%
  filter(BAGO == 1) %>%
  select(-c("BAGO", "nb"))

DiscControls4 <- michigan %>%
  group_by(REGION, survey_date, BEXP) %>%
  summarise(nbe = n()) %>%
  group_by(REGION, survey_date) %>% 
  mutate(BizExGood = nbe/sum(nbe)) %>%
  filter(BEXP ==1) %>%
  select(-c("BEXP", "nbe"))

DiscControls5 <- michigan %>%
  group_by(REGION, survey_date, PAGOR1) %>%
  summarise(n5 = n()) %>%
  group_by(REGION, survey_date) %>% 
  mutate(FinBetterFamily = n5/sum(n5)) %>%
  filter(PAGOR1 == 21) %>%
  select(-c("PAGOR1", "n5"))

DiscControls6 <- michigan %>%
  group_by(REGION,survey_date, PAGO5) %>%
  summarise(n6=n()) %>%
  group_by(REGION, survey_date) %>%
  mutate(FinBetterLast5 = n6/sum(n6)) %>%
  filter(PAGO5 ==1) %>%
  select(-c("PAGO5", "n6"))

DiscControls7 <- michigan %>%
  group_by(REGION,survey_date, PEXP) %>%
  summarise(n7=n()) %>%
  group_by(REGION, survey_date) %>%
  mutate(FinBetterNext1 = n7/sum(n7)) %>%
  filter(PEXP ==1) %>%
  select(-c("PEXP", "n7"))

DiscControls8 <- michigan %>%
  group_by(REGION,survey_date, PEXP5) %>%
  summarise(n8=n()) %>%
  group_by(REGION, survey_date) %>%
  mutate(FinBetterNext5 = n8/sum(n8)) %>%
  filter(PEXP5 ==1) %>%
  select(-c("PEXP5", "n8"))

DiscControls9 <- michigan %>%
  group_by(REGION,survey_date, UNEMP) %>%
  summarise(n9=n()) %>%
  group_by(REGION, survey_date) %>%
  mutate(UNEMPGood = n9/sum(n9)) %>%
  filter(UNEMP ==3) %>%
  select(-c("UNEMP", "n9"))

DiscControls <- merge(DiscControls1, DiscControls2) %>%
  merge(DiscControls3) %>%
  merge(DiscControls4) %>%
  #merge(DiscControls5) %>%
  #merge(DiscControls6) %>%
  merge(DiscControls7) %>%
  #merge(DiscControls8) %>%
  merge(DiscControls9)

# full_join includes NA for missing observations;
# eliminating the continuous controls also fixes;

#controls <- merge(ContControls, DiscControls)
controls <- full_join(ContControls, DiscControls)
#panel_data <- merge(panel_data, controls)
panel_data <- full_join(panel_data, controls) %>%
  filter(!is.na(pe))

panel_data_First <- full_join(panel_data_First, controls) %>%
  filter(!is.na(pe))

panel_data_Small <- full_join(panel_data_Small, controls) %>%
  filter(!is.na(pe))

write_csv(panel_data,"data/dataBuild/paneldataState.csv")
write_csv(panel_data_First,"data/dataBuild/paneldataFirstState.csv")
write_csv(panel_data_Small,"data/dataBuild/paneldataSmallState.csv")
write_csv(master_data,"data/dataBuild/masterdataState.csv")
write_csv(master_data_First,"data/dataBuild/masterdataFirstState.csv")
write_csv(master_data_Small,"data/dataBuild/masterdataSmallState.csv")
write_csv(data_local,"data/dataBuild/localdataState.csv")
write_csv(data_local_First,"data/dataBuild/localdataFirstState.csv")
write_csv(data_local_Small,"data/dataBuild/localdataSmallState.csv")
write_csv(data_global,"data/dataBuild/globaldataState.csv")
write_csv(data_global_First,"data/dataBuild/globaldataFirstState.csv")
write_csv(data_global_Small,"data/dataBuild/globaldataSmallState.csv")

