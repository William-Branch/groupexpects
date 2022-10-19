#computes descriptive plots.
# outputs: figs/menwomen.pdf, figs/regions.pdf, figs/aggIndComp.pdf, weightsTop10comp.pdf, figs/weightsDiverse.pdf, figs/weightsTime.pdf,figs/weightsCPS78.pdf



library(tidyverse)
library(zoo)
library(gridExtra)
library(fredr) 
library(ggplot2)
library(bartik.weight)
library(fixest)
library(modelsummary)
library(ggstream)
library(viridis)
library(latex2exp)
library(tikzDevice)



setwd("~/Dropbox/Cloud/Research/panel expectations/groupexpects")

michigan <- read_csv("data/Michigan_220524.csv")
region_data <- read_csv("data/dataBuild/regiondata.csv")
local_dataBig_Small <- read_csv("data/dataBuild/localdataBigSmall.csv")

# Remove n/a's and fix date format;

michigan <- michigan %>%
  mutate(survey_date = as.Date(paste0(michigan[["YYYYMM"]],01), format = "%Y%m%d")) %>%
  filter(PX1<96) %>%
  filter(PX1!=-97) %>%
  filter(PX1 %in% (-25:25))

paneldata <-  read_csv("data/dataBuild/paneldata.csv") %>%
  mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
  panel(~REGION + survey_date)

master_dataGender <- michigan %>%
  group_by(SEX,survey_date) %>%
  summarise(pe = mean(PX1, na.rm = TRUE))
master_dataGender <- master_dataGender[c("survey_date", "pe", "SEX")] %>%
  filter(!is.na(pe), !is.na(SEX))

master_dataRegion <- michigan %>%
  group_by(REGION, survey_date) %>%
  summarise(pe = mean(PX1, na.rm = TRUE)) %>%
  filter(!is.na(pe), !is.na(REGION))
  
Gender_alt <- master_dataGender %>% mutate(SEX = as.character(SEX))

Region_alt <- master_dataRegion %>%
  group_by(survey_date, REGION) %>%
  summarise(n = sum(pe), pe = pe) %>%
  mutate( percentage = n/sum(n), REGION = as.character(REGION)) %>%
  filter(percentage>0)

Industry <- local_dataBig_Small %>%
  select(c("survey_date", "PX1", "industry")) %>%
  filter(industry %in% c(5,41,21,37,90,134,81,141,85,62)) %>%
  group_by(industry, survey_date) %>%
  summarise( pe = mean(PX1, na.rm = TRUE)) %>%
  mutate(industry = as.character(industry))

AllIndustry  <- local_dataBig_Small %>%
  select(c("survey_date", "PX1", "industry")) %>%
  filter(!(industry %in% c(148,149))) %>%
  group_by(industry, survey_date) %>%
  summarise( pe = mean(PX1, na.rm = TRUE)) %>%
  mutate(industry = as.character(industry))
         

# men/women comparison.
tikz(file = "figs/tex/menwomen.tex", engine = "xetex", height = 3, width = 6.5)
pMW <- ggplot(data = master_dataGender, aes(x=survey_date, y = pe, group = SEX, color = factor(SEX)))+geom_line() + scale_color_manual(labels = c("men", "women"), values = c(rgb(.541,.2,.141), rgb(0,0.2,0.5)))+
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 12)) +
  labs(x =  "survey date", y = "$pe$")
#ggsave("figs/menwomen.pdf", pMW)
print(pMW)
dev.off()

# cross-region comparison.
tikz(file = "figs/tex/regions.tex", width = 6.5, height = 3)
pCR <- ggplot(data = Region_alt, aes(x=survey_date, y = percentage, fill = factor(REGION)))+geom_area(alpha = 0.6, size=1, colour="black")+ylim(-.1,1.02) + labs(y = "$pe$ ", x = "date", fill = "REGION")+
  scale_color_manual(labels = c("W", "MW", "NE", "S"))+ scale_fill_viridis(discrete=TRUE,  option="E",labels = c("W", "MW", "NE", "S")) +
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 12))
print(pCR)
dev.off()
#ggsave("figs/regions.pdf", pCR)

# comparison by top 10 weighted and across all industries.


pWt10 <- ggplot(data = Industry, aes(x=survey_date, y = pe, fill = industry, color = industry)) + geom_stream(type="proportional") +
  labs(y = "pe (% of total, smooth)", x = "survey date") +
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 30, colour = rgb(.26, .17, .16)), axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position = "none")  



pAgg <- ggplot(data = AllIndustry, aes(x=survey_date, y = pe, fill = industry, color = industry)) + geom_stream(type="ridge") +
  labs(y = "pe (sum by group)", x = "survey date") + 
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 30,colour = rgb(.26, .17, .16)), axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position = "none")  


Diverse <- region_data %>% drop_na() %>% group_by(REGION, industry) %>% summarise(MnProp = mean(prop)) 
tikz(file = "figs/tex/weightsDiverse.tex", width = 6.5, height = 3.5)
pD1 <- ggplot(data = Diverse, aes(x=industry, y= 100*MnProp, group = industry, fill = industry)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  labs(y = "$\\omega_{R,g,t}$", x = "group")+ 
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 12, colour = rgb(.26, .17, .16)), legend.position = "none")  
print(pD1)
dev.off()

pD2 <- ggplot(data = drop_na(region_data), aes(x=survey_date, y = 100*prop, group = survey_date, fill = industry)) + geom_boxplot( outlier.shape = 1) +
  stat_boxplot(geom ='errorbar') + facet_wrap(. ~ REGION, nrow = 2, labeller = labeller(REGION = 
                                                                                          c("1" = "west",
                                                                                            "2" = "midwest",
                                                                                            "3" = "northeast", "4" = "south")
  )) +
  labs(y = TeX("$\\omega_{R,g,t}$ "), x = "date")+ 
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 32), legend.position = "none")  



ggsave("figs/aggIndComp.pdf", pAgg)
ggsave("figs/weightsTop10comp.pdf", pWt10)
#ggsave("figs/weightsDiverse.pdf", pD1)
ggsave("figs/weightsTime.pdf", pD2)


# Comparison to January 1978 CPS.

region_CPS78 <- read_csv("data/dataBuild/regiondataout.csv")
panel_cps <- read_csv("data/dataBuild/panelcps.csv")

tikz(file = "figs/tex/weightsCPS78.tex", width =6.5, height =3.5)
pD3 <- ggplot(data = region_CPS78, aes(x=group, y= 100*prop, group = group, fill = group)) + geom_boxplot(outlier.colour = "red", outlier.shape = )+
  labs(y = "$\\omega_{R,g,t}$ ", x = "group")+ 
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 12,colour = rgb(.26, .17, .16)), legend.position = "none")  
print(pD3)
dev.off()
#ggsave("figs/weightsCPS78.pdf", pD3)




# Check predictability of covariates given CPS shares;
CheckCov <- merge(region_CPS78, panel_cps)
EstCov <- feols( sw(gasExp, ye, UNRATE, UNEMPGood, FinBetterLast1, RincUp, FinBetterNext1, BizGood, BizExGood) ~ prop, data = CheckCov)

