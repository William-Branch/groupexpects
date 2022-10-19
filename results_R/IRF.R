# Plots IRF's for CPS and michigan weights.
# outputs: figs/tex/IRFcps.tex, figs/tex/IRFout.tex.

library(tidyverse)
library(zoo)
library(gridExtra)
library(fredr)
library(ggplot2)
library(bartik.weight)
library(fixest)
library(modelsummary)
library(tikzDevice)

setwd("~/Dropbox/Cloud/Research/panel expectations/groupexpects")
paneldata <-  read_csv("data/dataBuild/paneldata.csv") %>%
  mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
  panel(~REGION + survey_date)

panel_bench_out <- read_csv("data/dataBuild/panelbenchout.csv")%>% panel(~REGION + survey_date)
panel_out <- read_csv("data/dataBuild/panelcpsout.csv") %>% panel(~REGION + survey_date)




cpser <- feols(csw0(RegInf, l(RegInf,-1), l(RegInf, -2), l(RegInf,-3), l(RegInf,-4), l(RegInf, -5), l(RegInf, -6), l(RegInf, -7), l(RegInf,-8), l(RegInf,-9)) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK")
cb <- coef(cpser)

cps1 <- confint(feols(RegInf ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
cps2 <- confint(feols(l(RegInf,-1) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
cps3 <- confint(feols(l(RegInf,-2) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
cps4 <- confint(feols(l(RegInf,-3) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
cps5 <- confint(feols(l(RegInf,-4) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
cps6 <- confint(feols(l(RegInf,-5) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
cps7 <- confint(feols(l(RegInf,-6) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
cps8 <- confint(feols(l(RegInf,-7) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
cps9 <- confint(feols(l(RegInf,-8) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
cps10 <- confint(feols(l(RegInf,-9) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))



piPredcps <- as_tibble(cb[1:10,1]) %>%
  #rbind(-0.001) %>%
  mutate(inflation = value, qtrs = c(0,1,2,3,4,5,6,7,8,9),lb = c(cps1[1,1], cps2[1,1], cps3[1,1], cps4[1,1], cps5[1,1], cps6[1,1], cps7[1,1], cps8[1,1], cps9[1,1], cps10[1,1]), ub = c(cps1[1,2], cps2[1,2], cps3[1,2], cps4[1,2], cps5[1,2], cps6[1,2], cps7[1,2], cps8[1,2], cps9[1,2], cps10[1,2])) 

tikz(file = "figs/tex/IRFcps.tex", height=3, width =3)
pIRFc <- ggplot(data=piPredcps, aes(x=qtrs, y = inflation)) + geom_line(size = 2.0, linetype = "dashed", colour="red")+geom_ribbon(aes(x=qtrs, ymax=ub, ymin=lb), fill="grey", alpha=.2)+theme(panel.border = element_blank(),
                                                                                                                                                                                               panel.background = element_blank(),
                                                                                                                                                                                               panel.grid.minor = element_blank(),
                                                                                                                                                                                               axis.line = element_line(colour = "grey"))
print(pIRFc)
dev.off()



outer <- feols(csw0(RegInf, l(RegInf,-1), l(RegInf, -2), l(RegInf,-3), l(RegInf,-4), l(RegInf, -5), l(RegInf, -6), l(RegInf, -7), l(RegInf,-8), l(RegInf,-9)) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK")
co <- coef(outer)

cps1 <- confint(feols(RegInf ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
cps2 <- confint(feols(l(RegInf,-1) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
cps3 <- confint(feols(l(RegInf,-2) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
cps4 <- confint(feols(l(RegInf,-3) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
cps5 <- confint(feols(l(RegInf,-4) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
cps6 <- confint(feols(l(RegInf,-5) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
cps7 <- confint(feols(l(RegInf,-6) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
cps8 <- confint(feols(l(RegInf,-7) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
cps9 <- confint(feols(l(RegInf,-8) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
cps10 <- confint(feols(l(RegInf,-9) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))



piPredout <- as_tibble(co[1:10,1]) %>%
  #rbind(-0.001) %>%
  mutate(inflation = value, qtrs = c(0,1,2,3,4,5,6,7,8,9),lb = c(cps1[1,1], cps2[1,1], cps3[1,1], cps4[1,1], cps5[1,1], cps6[1,1], cps7[1,1], cps8[1,1], cps9[1,1], cps10[1,1]), ub = c(cps1[1,2], cps2[1,2], cps3[1,2], cps4[1,2], cps5[1,2], cps6[1,2], cps7[1,2], cps8[1,2], cps9[1,2], cps10[1,2])) 

tikz(file = "figs/tex/IRFout.tex", height=3, width =3)
pIRFo <- ggplot(data=piPredout, aes(x=qtrs, y = inflation)) + geom_line(size = 2.0, linetype = "dashed", colour="red")+geom_ribbon(aes(x=qtrs, ymax=ub, ymin=lb), fill="grey", alpha=.2)+theme(panel.border = element_blank(),
                                                                                                                                                                                               panel.background = element_blank(),
                                                                                                                                                                                               panel.grid.minor = element_blank(),
                                                                                                                                                                                               axis.line = element_line(colour = "grey"))
print(pIRFo)
dev.off()