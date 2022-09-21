# Computes simple introductory correlations.
# Outputs: figs/tex/Regscatter.tex, figs/tex/Aggscatter.tex
library(fredr) 
library(ggplot2)
library(bartik.weight)
library(fixest)
library(modelsummary)
library(tidyverse)
library(corrr)
library(latex2exp)
library(viridis)
library(ggrepel)
library(zoo)
library(kableExtra)
library(tikzDevice)

setwd("~/Dropbox/Cloud/Research/panel expectations/make-panelexpects")
fredr_set_key("1eb4018cc8ead6ac6445763becf7bf42")

michigan <- read_csv("data/Michigan_220524.csv")

# Remove n/a's and fix date format;

michigan <- michigan %>%
  mutate(survey_date = as.Date(paste0(michigan[["YYYYMM"]],01), format = "%Y%m%d")) %>%
  filter(PX1<96) %>%
  filter(PX1!=-97)

infl <- fredr(series_id = "CPIAUCSL", units = "pc1") %>%
  filter(!is.na(value))%>%
  mutate(survey_date = date, infl = value) %>%
  select(c(survey_date, infl)) 

messaround <- michigan %>%
  select(c("survey_date", "PX1","PX5")) %>%
  group_by(survey_date) %>%
  summarise(peavg = mean(PX1), longavg = mean(PX5))

unAgg <- fredr(series_id = "UNRATE") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, AggUNRATE = value)%>%
  filter(survey_date > as.Date("1977-12-01"))

Aggdata <- merge(infl, messaround) %>% merge(unAgg)

estAvg <- lm(infl ~ peavg + AggUNRATE + lag(infl,1) + lag(infl,2) + lag(infl, 3) + lag(infl,4), data = Aggdata)


yresidA <- resid(lm(infl ~  AggUNRATE + lag(infl,1) + lag(infl,2) + lag(infl, 3) + lag(infl,4), data = Aggdata))
xresidA <- resid(lm(peavg ~  AggUNRATE + lag(infl,1) + lag(infl,2) + lag(infl, 3) + lag(infl,4), data = Aggdata))
Acoeffs <- lm(yresidA ~xresidA, data=NULL)

tikz(file = "figs/tex/Aggscatter.tex", height = 3, width = 3)
pAggUS <- ggplot(data=NULL, aes(x=xresidA, y=yresidA))+geom_point(size = 1.0, alpha = 0.5)+ geom_abline(slope = coefficients(Acoeffs)[[2]], intercept = coefficients(Acoeffs)[[1]], col = rgb(.541,.2,.141), size = 1.2) +
   annotate("text", label = "slope = 0.18", x = 2.0, y=-1.0, size =03,col = rgb(.541,.2,.141)) +
  annotate("segment", x = 2.0, xend = 3.0, y = -0.90, yend = .5,
           colour = rgb(.541,.2,.141), size = 0.5, alpha =0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  labs(y = ("U.S. inflation"), x = "inflation expect.'s")+
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 12), legend.position = "none")  
print(pAggUS)
dev.off()

yresid <- resid(feols(RegInf ~   l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter ,   paneldata))
xresid <- resid(feols(pe ~   l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter ,   paneldata))

rcoeffs <- lm(yresid ~xresid, data=NULL)
tikz(file = "figs/tex/Regscatter.tex", height = 3, width = 3)
pREGUS <- ggplot(data=NULL, aes(x=xresid, y=yresid))+geom_point(size=1.0, alpha = 0.5)+ geom_abline(slope = coefficients(rcoeffs)[[2]], intercept = coefficients(rcoeffs)[[1]], col = rgb(.541,.2,.141), size = 1.2)+
  annotate("text", label = "slope = 0.069", x = 2.05, y=-1.0, size =03,col = rgb(.541,.2,.141)) +
  annotate("segment", x = 2.00, xend = 3.2, y = -0.90, yend = .15,
           colour = rgb(.541,.2,.141), size = 0.5, alpha =0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  labs(y = ("regional inflation"), x = "inflation expect.'s")+
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 12), legend.position = "none")  
print(pREGUS)
dev.off()


messaroundS <- michigan %>%
  select(c("survey_date", "PX1","PX5", "IDPREV")) %>%
  filter(PX1>-5 & PX1<25 & is.na(IDPREV)) %>%
  group_by(survey_date) %>%
  summarise(peavg = mean(PX1), longavg = mean(PX5))

unAgg <- fredr(series_id = "UNRATE") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, AggUNRATE = value)%>%
  filter(survey_date > as.Date("1977-12-01"))

AggdataS <- merge(infl, messaroundS) %>% merge(unAgg)

yresidS <- resid(lm(infl ~  AggUNRATE + lag(infl,1) + lag(infl,2) + lag(infl, 3) + lag(infl,4), data = AggdataS))
xresidS <- resid(lm(peavg ~  AggUNRATE + lag(infl,1) + lag(infl,2) + lag(infl, 3) + lag(infl,4), data = AggdataS))
Scoeffs <- lm(yresidS ~xresidS, data=NULL)
