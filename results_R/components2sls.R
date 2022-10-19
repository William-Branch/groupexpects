library(tidyverse)
library(zoo)
library(gridExtra)
library(fredr)
library(ggplot2)
library(bartik.weight)
library(ipumsr)
library(kableExtra)
library(fixest)








fredr_set_key("1eb4018cc8ead6ac6445763becf7bf42")

setwd("~/Dropbox/Cloud/Research/panel expectations/groupexpects")
paneldata <-  read_csv("data/dataBuild/paneldata.csv") %>%
  mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
  panel(~REGION + survey_date)

panel_out <- read_csv("data/dataBuild/panelcpsout.csv") %>% panel(~REGION + survey_date)
panel_bench_out <- read_csv("data/dataBuild/panelbenchout.csv") %>% panel(~REGION + survey_date)
panel_out_w5 <- read_csv("data/dataBuild/paneloutw5.csv") %>% panel(~REGION + survey_date)
panel_bench_out_w5 <- read_csv("data/dataBuild/panelbenchoutw5.csv") %>% panel(~REGION + survey_date)

#Non durables:
#west
infW_ND <- fredr(series_id = "CUUR0400SAN", units = "pc1") %>%
  filter(!is.na(value)) 
infW_ND <-   approx(x = infW_ND$date, y = infW_ND$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infW_non <- as_tibble(infW_ND) %>% mutate(date = x, infND = y, REGION = 4) %>% select(c(date, infND, REGION))
#midwest:
infMW_ND <- fredr(series_id = "CUUR0200SAN", units = "pc1") %>%
  filter(!is.na(value)) 
infMW_ND <-   approx(x = infMW_ND$date, y = infMW_ND$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infMW_non <- as_tibble(infMW_ND) %>% mutate(date = x, infND = y, REGION = 2) %>% select(c(date, infND, REGION))
#northeast:
infNE_ND <- fredr(series_id = "CUUR0100SAN", units = "pc1") %>%
  filter(!is.na(value)) 
infNE_ND <-   approx(x = infNE_ND$date, y = infNE_ND$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infNE_non <- as_tibble(infNE_ND) %>% mutate(date = x, infND = y, REGION = 1) %>% select(c(date, infND, REGION))
#south:
infS_ND<- fredr(series_id = "CUUR0300SAN", units = "pc1") %>%
  filter(!is.na(value)) 
infS_ND <-   approx(x = infS_ND$date, y = infS_ND$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infS_non <- as_tibble(infS_ND) %>% mutate(date = x, infND = y, REGION = 3) %>% select(c(date, infND, REGION))

inf_ND <- rbind(infW_non,infMW_non,infNE_non, infS_non) %>% mutate(survey_date = date) %>% select(-date)

#Durables:
#west
infW_D <- fredr(series_id = "CUUR0400SAD", units = "pc1") %>%
  filter(!is.na(value)) 
infW_D <-   approx(x = infW_D$date, y = infW_D$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infW_dur <- as_tibble(infW_D) %>% mutate(date = x, infD = y, REGION = 4) %>% select(c(date, infD, REGION))
#midwest:
infMW_D <- fredr(series_id = "CUUR0200SAD", units = "pc1") %>%
  filter(!is.na(value)) 
infMW_D <-   approx(x = infMW_D$date, y = infMW_D$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infMW_dur <- as_tibble(infMW_D) %>% mutate(date = x, infD = y, REGION = 2) %>% select(c(date, infD, REGION))
#northeast:
infNE_D <- fredr(series_id = "CUUR0100SAD", units = "pc1") %>%
  filter(!is.na(value)) 
infNE_D <-   approx(x = infNE_D$date, y = infNE_D$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infNE_dur <- as_tibble(infNE_D) %>% mutate(date = x, infD = y, REGION = 1) %>% select(c(date, infD, REGION))
#south:
infS_D<- fredr(series_id = "CUUR0300SAD", units = "pc1") %>%
  filter(!is.na(value)) 
infS_D <-   approx(x = infS_D$date, y = infS_D$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infS_dur <- as_tibble(infS_D) %>% mutate(date = x, infD = y, REGION = 3) %>% select(c(date, infD, REGION))
inf_D <- rbind(infW_dur,infMW_dur,infNE_dur, infS_dur) %>% mutate(survey_date = date) %>% select(-date)

#Services:CUUR0400SAS
infW_S <- fredr(series_id = "CUUR0400SAS", units = "pc1") %>%
  filter(!is.na(value)) 
infW_S <-   approx(x = infW_S$date, y = infW_S$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infW_ser <- as_tibble(infW_S) %>% mutate(date = x, infS = y, REGION = 4) %>% select(c(date, infS, REGION))
#midwest:
infMW_S <- fredr(series_id = "CUUR0200SAS", units = "pc1") %>%
  filter(!is.na(value)) 
infMW_S <-   approx(x = infMW_S$date, y = infMW_S$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infMW_ser <- as_tibble(infMW_S) %>% mutate(date = x, infS = y, REGION = 2) %>% select(c(date, infS, REGION))
#northeast:
infNE_S <- fredr(series_id = "CUUR0100SAS", units = "pc1") %>%
  filter(!is.na(value)) 
infNE_S <-   approx(x = infNE_S$date, y = infNE_S$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infNE_ser <- as_tibble(infNE_S) %>% mutate(date = x, infS = y, REGION = 1) %>% select(c(date, infS, REGION))
#south:
infS_S<- fredr(series_id = "CUUR0300SAS", units = "pc1") %>%
  filter(!is.na(value)) 
infS_S <-   approx(x = infS_S$date, y = infS_S$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infS_ser <- as_tibble(infS_S) %>% mutate(date = x, infS = y, REGION = 3) %>% select(c(date, infS, REGION))
inf_S <- rbind(infW_ser,infMW_ser,infNE_ser, infS_ser) %>% mutate(survey_date = date) %>% select(-date)

#Commodities:
infW_C <- fredr(series_id = "CUUR0400SAC", units = "pc1") %>%
  filter(!is.na(value)) 
infW_C <-   approx(x = infW_C$date, y = infW_C$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infW_com <- as_tibble(infW_C) %>% mutate(date = x, infC = y, REGION = 4) %>% select(c(date, infC, REGION))
#midwest:
infMW_C <- fredr(series_id = "CUUR0200SAC", units = "pc1") %>%
  filter(!is.na(value)) 
infMW_C <-   approx(x = infMW_C$date, y = infMW_C$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infMW_com <- as_tibble(infMW_C) %>% mutate(date = x, infC = y, REGION = 2) %>% select(c(date, infC, REGION))
#northeast:
infNE_C <- fredr(series_id = "CUUR0100SAC", units = "pc1") %>%
  filter(!is.na(value)) 
infNE_C <-   approx(x = infNE_C$date, y = infNE_C$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infNE_com <- as_tibble(infNE_C) %>% mutate(date = x, infC = y, REGION = 1) %>% select(c(date, infC, REGION))
#south:
infS_C<- fredr(series_id = "CUUR0300SAC", units = "pc1") %>%
  filter(!is.na(value)) 
infS_C <-   approx(x = infS_C$date, y = infS_C$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infS_com <- as_tibble(infS_C) %>% mutate(date = x, infC = y, REGION = 3) %>% select(c(date, infC, REGION))
inf_C <- rbind(infW_com,infMW_com,infNE_com, infS_com) %>% mutate(survey_date = date) %>% select(-date)

#Commodities less food and energy: CUUS0400SACL1E
infW_Clf <- fredr(series_id = "CUUS0400SACL1E", units = "pc1") %>%
  filter(!is.na(value)) 
infW_Clf <-   approx(x = infW_Clf$date, y = infW_Clf$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infW_comlf <- as_tibble(infW_Clf) %>% mutate(date = x, infClf = y, REGION = 4) %>% select(c(date, infClf, REGION))
#midwest:
infMW_Clf <- fredr(series_id = "CUUS0200SACL1E", units = "pc1") %>%
  filter(!is.na(value)) 
infMW_Clf <-   approx(x = infMW_Clf$date, y = infMW_Clf$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infMW_comlf <- as_tibble(infMW_Clf) %>% mutate(date = x, infClf = y, REGION = 2) %>% select(c(date, infClf, REGION))
#northeast:
infNE_Clf <- fredr(series_id = "CUUS0100SACL1E", units = "pc1") %>%
  filter(!is.na(value)) 
infNE_Clf <-   approx(x = infNE_Clf$date, y = infNE_Clf$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infNE_comlf <- as_tibble(infNE_Clf) %>% mutate(date = x, infClf = y, REGION = 1) %>% select(c(date, infClf, REGION))
#south:
infS_Clf<- fredr(series_id = "CUUS0300SACL1E", units = "pc1") %>%
  filter(!is.na(value)) 
infS_Clf <-   approx(x = infS_Clf$date, y = infS_Clf$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infS_comlf <- as_tibble(infS_Clf) %>% mutate(date = x, infClf = y, REGION = 3) %>% select(c(date, infClf, REGION))
inf_Clf <- rbind(infW_comlf,infMW_comlf,infNE_comlf, infS_comlf) %>% mutate(survey_date = date) %>% select(-date)

#non durables less food: CUUR0400SANL113
infW_NDlf <- fredr(series_id = "CUUR0400SANL113", units = "pc1") %>%
  filter(!is.na(value)) 
infW_NDlf <-   approx(x = infW_NDlf$date, y = infW_NDlf$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infW_nonlf <- as_tibble(infW_NDlf) %>% mutate(date = x, infNDlf = y, REGION = 4) %>% select(c(date, infNDlf, REGION))
#midwest:
infMW_NDlf <- fredr(series_id = "CUUR0200SANL113", units = "pc1") %>%
  filter(!is.na(value)) 
infMW_NDlf <-   approx(x = infMW_NDlf$date, y = infMW_NDlf$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infMW_nonlf <- as_tibble(infMW_NDlf) %>% mutate(date = x, infNDlf = y, REGION = 2) %>% select(c(date, infNDlf, REGION))
#northeast:
infNE_NDlf <- fredr(series_id = "CUUR0100SANL113", units = "pc1") %>%
  filter(!is.na(value)) 
infNE_NDlf <-   approx(x = infNE_NDlf$date, y = infNE_NDlf$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infNE_nonlf <- as_tibble(infNE_NDlf) %>% mutate(date = x, infNDlf = y, REGION = 1) %>% select(c(date, infNDlf, REGION))
#south:
infS_NDlf<- fredr(series_id = "CUUR0300SANL113", units = "pc1") %>%
  filter(!is.na(value)) 
infS_NDlf <-   approx(x = infS_NDlf$date, y = infS_NDlf$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infS_nonlf <- as_tibble(infS_NDlf) %>% mutate(date = x, infNDlf = y, REGION = 3) %>% select(c(date, infNDlf, REGION))

inf_NDlf <- rbind(infW_nonlf,infMW_nonlf,infNE_nonlf, infS_nonlf) %>% mutate(survey_date = date) %>% select(-date)

#services less rent: CUUR0400SASL2RS
infW_Slr <- fredr(series_id = "CUUR0400SASL2RS", units = "pc1") %>%
  filter(!is.na(value)) 
infW_Slr <-   approx(x = infW_Slr$date, y = infW_Slr$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infW_serlr <- as_tibble(infW_Slr) %>% mutate(date = x, infSlr = y, REGION = 4) %>% select(c(date, infSlr, REGION))
#midwest:
infMW_Slr <- fredr(series_id = "CUUR0200SASL2RS", units = "pc1") %>%
  filter(!is.na(value)) 
infMW_Slr <-   approx(x = infMW_Slr$date, y = infMW_Slr$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infMW_serlr <- as_tibble(infMW_Slr) %>% mutate(date = x, infSlr = y, REGION = 2) %>% select(c(date, infSlr, REGION))
#northeast:
infNE_Slr <- fredr(series_id = "CUUR0100SASL2RS", units = "pc1") %>%
  filter(!is.na(value)) 
infNE_Slr <-   approx(x = infNE_Slr$date, y = infNE_Slr$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infNE_serlr <- as_tibble(infNE_Slr) %>% mutate(date = x, infSlr = y, REGION = 1) %>% select(c(date, infSlr, REGION))
#south:
infS_Slr<- fredr(series_id = "CUUR0300SASL2RS", units = "pc1") %>%
  filter(!is.na(value)) 
infS_Slr <-   approx(x = infS_Slr$date, y = infS_Slr$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infS_serlr <- as_tibble(infS_Slr) %>% mutate(date = x, infSlr = y, REGION = 3) %>% select(c(date, infSlr, REGION))
inf_Slr <- rbind(infW_serlr,infMW_serlr,infNE_serlr, infS_serlr) %>% mutate(survey_date = date) %>% select(-date)

#services less medical: CUUR0400SASL5
infW_Slm <- fredr(series_id = "CUUR0400SASL5", units = "pc1") %>%
  filter(!is.na(value)) 
infW_Slm <-   approx(x = infW_Slm$date, y = infW_Slm$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infW_serlm <- as_tibble(infW_Slm) %>% mutate(date = x, infSlm = y, REGION = 4) %>% select(c(date, infSlm, REGION))
#midwest:
infMW_Slm <- fredr(series_id = "CUUR0200SASL5", units = "pc1") %>%
  filter(!is.na(value)) 
infMW_Slm <-   approx(x = infMW_Slm$date, y = infMW_Slm$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infMW_serlm <- as_tibble(infMW_Slm) %>% mutate(date = x, infSlm = y, REGION = 2) %>% select(c(date, infSlm, REGION))
#northeast:
infNE_Slm <- fredr(series_id = "CUUR0100SASL5", units = "pc1") %>%
  filter(!is.na(value)) 
infNE_Slm <-   approx(x = infNE_Slm$date, y = infNE_Slm$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infNE_serlm <- as_tibble(infNE_Slm) %>% mutate(date = x, infSlm = y, REGION = 1) %>% select(c(date, infSlm, REGION))
#south:
infS_Slm<- fredr(series_id = "CUUR0300SASL5", units = "pc1") %>%
  filter(!is.na(value)) 
infS_Slm <-   approx(x = infS_Slm$date, y = infS_Slm$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infS_serlm <- as_tibble(infS_Slm) %>% mutate(date = x, infSlm = y, REGION = 3) %>% select(c(date, infSlm, REGION))
inf_Slm <- rbind(infW_serlm,infMW_serlm,infNE_serlm, infS_serlm) %>% mutate(survey_date = date) %>% select(-date)

#all less shelter: CUUR0400SA0L2
#west
infW_als <- fredr(series_id = "CUUR0400SA0L2", units = "pc1") %>%
  filter(!is.na(value)) 
infW_als <-   approx(x = infW_als$date, y = infW_als$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infW_allls <- as_tibble(infW_als) %>% mutate(date = x, infals = y, REGION = 4) %>% select(c(date, infals, REGION))
#midwest:
infMW_als <- fredr(series_id = "CUUR0200SA0L2", units = "pc1") %>%
  filter(!is.na(value)) 
infMW_als <-   approx(x = infMW_als$date, y = infMW_als$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infMW_allls <- as_tibble(infMW_als) %>% mutate(date = x, infals = y, REGION = 2) %>% select(c(date, infals, REGION))
#northeast:
infNE_als <- fredr(series_id = "CUUR0100SA0L2", units = "pc1") %>%
  filter(!is.na(value)) 
infNE_als <-   approx(x = infNE_als$date, y = infNE_als$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infNE_allls <- as_tibble(infNE_als) %>% mutate(date = x, infals = y, REGION = 1) %>% select(c(date, infals, REGION))
#south:
infS_als<- fredr(series_id = "CUUR0300SA0L2", units = "pc1") %>%
  filter(!is.na(value)) 
infS_als <-   approx(x = infS_als$date, y = infS_als$value, xout = seq(as.Date("1978-12-01"), as.Date("2022/8/1"), "months"))
infS_allls <- as_tibble(infS_als) %>% mutate(date = x, infals = y, REGION = 3) %>% select(c(date, infals, REGION))
inf_allls <- rbind(infW_allls,infMW_allls,infNE_allls, infS_allls) %>% mutate(survey_date = date) %>% select(-date)



panelout_sub <- merge(panel_out,inf_ND) %>% merge(inf_D) %>% merge(inf_S) %>% 
  merge(inf_C) %>%
  merge(inf_Clf) %>%
  merge(inf_NDlf) %>%
  merge(inf_Slr) %>%
  merge(inf_Slm) %>%
  merge(inf_allls) %>%
  panel(~REGION + survey_date)

panelbench_sub <- merge(panel_bench_out, inf_ND) %>% merge(inf_D) %>% merge(inf_S) %>% 
  merge(inf_C) %>%
  merge(inf_Clf) %>%
  merge(inf_NDlf) %>%
  merge(inf_Slr) %>%
  merge(inf_Slm) %>%
  merge(inf_allls) %>%
  panel(~REGION + survey_date)

panelout_sub_w5 <- merge(panel_out_w5,inf_ND) %>% merge(inf_D) %>% merge(inf_S) %>% 
  merge(inf_C) %>%
  merge(inf_Clf) %>%
  merge(inf_NDlf) %>%
  merge(inf_Slr) %>%
  merge(inf_Slm) %>%
  merge(inf_allls) %>%
  panel(~REGION + survey_date)

panelbench_sub_w5 <- merge(panel_bench_out_w5, inf_ND) %>% merge(inf_D) %>% merge(inf_S) %>% 
  merge(inf_C) %>%
  merge(inf_Clf) %>%
  merge(inf_NDlf) %>%
  merge(inf_Slr) %>%
  merge(inf_Slm) %>%
  merge(inf_allls) %>%
  panel(~REGION + survey_date)

#non-durables
est_out_nd <- feols(infND ~  l(infND,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panelout_sub)
est_bench_out_nd<- feols(infND ~  l(infND,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panelbench_sub)
#durables
est_out_d <- feols(infD ~  l(infD,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panelout_sub)
est_bench_out_d<- feols(infD ~  l(infD,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panelbench_sub)
#durables w/5
est_out_d_w5 <- feols(infD ~  l(infD,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe5  ~ Bartik5  , panelout_sub_w5)
est_bench_out_d_w5<- feols(infD ~  l(infD,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe5  ~ Bartik5_bench_out  , panelbench_sub_w5)
#services
est_out_s <- feols(infS ~  l(infS,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panelout_sub)
est_bench_out_s<- feols(infS ~  l(infS,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panelbench_sub)
#commodities
est_out_c <- feols(infC ~  l(infC,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panelout_sub)
est_bench_out_c<- feols(infC ~  l(infC,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panelbench_sub)
#commodities less food and energy
est_out_clf <- feols(infClf ~  l(infClf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panelout_sub)
est_bench_out_clf <- feols(infClf ~  l(infClf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panelbench_sub)
#nondurables less food and energy
est_out_ndlf <- feols(infNDlf ~  l(infNDlf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panelout_sub)
est_bench_out_ndlf <- feols(infNDlf ~  l(infNDlf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panelbench_sub)
#services less rent
est_out_slr <- feols(infSlr ~  l(infSlr,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panelout_sub)
est_bench_out_slr <- feols(infSlr ~  l(infSlr,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panelbench_sub)
#services less medical
est_out_slm <- feols(infSlm ~  l(infSlm,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panelout_sub)
est_bench_out_slm <- feols(infSlm ~  l(infSlm,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panelbench_sub)
#all less services
est_out_als <- feols(infals ~  l(infals,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_out  , panelout_sub)
est_bench_out_als <- feols(infals ~  l(infals,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out  , panelbench_sub)


table_out_1 <- etable(est_bench_out_nd, est_out_nd, vcov = "DK", dict = c("yearquarter" = "TIME","l(infND,1)" = "lag 1", "l(infND,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_out_2 <- etable(est_bench_out_d, est_out_d, vcov = "DK", dict = c("yearquarter" = "TIME","l(infD,1)" = "lag 1", "l(infD,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_out_3 <- etable(est_bench_out_s, est_out_s, vcov = "DK", dict = c("yearquarter" = "TIME","l(infS,1)" = "lag 1", "l(infS,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_out_4 <- etable(est_bench_out_d_w5, est_out_d_w5, vcov = "DK", dict = c("yearquarter" = "TIME","l(infD,1)" = "lag 1", "l(infD,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_out_5 <- etable(est_bench_out_c, est_out_c, vcov = "DK", dict = c("yearquarter" = "TIME","l(infC,1)" = "lag 1", "l(infC,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_out_6 <- etable(est_bench_out_clf, est_out_clf, vcov = "DK", dict = c("yearquarter" = "TIME","l(infClf,1)" = "lag 1", "l(infClf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_out_7 <- etable(est_bench_out_ndlf, est_out_ndlf, vcov = "DK", dict = c("yearquarter" = "TIME","l(infNDlf,1)" = "lag 1", "l(infNDlf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_out_8 <- etable(est_bench_out_slr, est_out_slr, vcov = "DK", dict = c("yearquarter" = "TIME","l(infSlr,1)" = "lag 1", "l(infSlr,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_out_9 <- etable(est_bench_out_slm, est_out_slm, vcov = "DK", dict = c("yearquarter" = "TIME","l(infSlm,1)" = "lag 1", "l(infSlm,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_out_10 <- etable(est_bench_out_als, est_out_als, vcov = "DK", dict = c("yearquarter" = "TIME","l(infals,1)" = "lag 1", "l(infals,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))

table_out_cats <- etable(est_out_c, est_out_nd, est_out_d, est_out_s, est_out_slr, est_out_slm, vcov = "DK", stage = 2, title = "2SLS estimates: component inflation",dict = c("yearquarter" = "TIME","l(infC,1)" = "lag 1C", "l(infC,2)" = "lag 2C", "l(infND,1)" = "lag 1ND", "l(infND,2)" = "lag 2ND", "l(infD,1)" = "lag 1D", "l(infD,2)" = "lag 2D", "l(infS,1)" = "lag 1S", "l(infS,2)" = "lag 2S", "l(infSlr,1)" = "lag 1Slr", "l(infSlr,2)" = "lag 2Slr", "l(infSlm,1)" = "lag 1Slm", "l(infSlm,2)" = "lag 2Slm", "infC" = "commod. infl.", "infND" = "non-dur. infl.", "infD" = "dur. infl.", "infS" = "serv. infl.", "infSlr" = "serv.-rent infl.", "inflSlm" = "serv.-med. infl."), group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1C", "lag 2C", "lag 1ND", "lag 2ND", "lag 1D", "lag 2D", "lag 1S", "lag 2S", "lag 1Slr", "lag 2Slr", "lag 1Slm", "lag 2Slm", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp"))) 


kbl(table_out_cats, "latex", booktabs = T, caption = "2SLS by component inflation", col.names = c("commodities","non-durables", "durables", "services", "services-house", "services-med."), linesep = "\\addlinespace", label = "components:2sls:stage2") %>%
  kable_styling(latex_options = c("striped","scale_down")) %>%
  footnote(general = "Commodities include all non-durable and durable goods.  Services-housing and services-med. remove housing and medical services, respectively." , threeparttable = T) %>%
  add_header_above(c(" " = 1, "Commodities" = 3, "Services" = 3), color = "BillRed")%>%
  save_kable("tables/2slscomponents.tex")
  
  
  
  



