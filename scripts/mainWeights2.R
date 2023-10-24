# Rotemberg weights with leave-one-out: cps and michigan shares
# outputs: tables/weightsout.tex, figs/weightsout.pdf, tables/weightsCPS.tex, figs/weightsCPSout.pdf, tables/weightsGroupcps.tex 


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
master_data <- read_csv("data/dataBuild/masterdataSmall.csv") %>% filter(!is.na(REGION)) %>% filter(!is.na(pe)) %>% filter(!is.na(RegInf))
data_local <- read_csv("data/dataBuild/localdataSmall.csv") %>% filter(!is.na(REGION))
data_global <- read_csv("data/dataBuild/globaldataSmall.csv")
panel_bench_out <- read_csv("data/dataBuild/panelbenchout.csv")
panel_out <- read_csv("data/dataBuild/panelcpsout.csv")
region_data2 <- read_csv("data/dataBuild/regiondata2.csv")
region_CPS78 <- read_csv("data/dataBuild/regiondataout.csv")




#conform data sets;

fixd <- merge(master_data,data_local)
fixl <- fixd %>%
  select(-c("pe","RegInf")) %>%
  as_tibble()
fixm <- fixd %>%
  select(c("survey_date", "pe", "REGION", "RegInf"))
fixg <- data_global

panel_fixT <- as_tibble(panel_bench_out) %>%
  select(-c(Bartik)) %>%
  mutate(Bartik = Bartik_bench_out)

panel_fixTcps <- as_tibble(panel_out) %>%
  select(-c(Bartik)) %>%
  mutate(Bartik = Bartik_out)



fixmc <- panel_fixT %>%
  filter(!is.na(RegInf) & !is.na(REGION)) %>%
  select(-c(Bartik, Bartik_bench_out, cenREGION, year, AggUNRATE, yearquarter, ye, UNRATE,   gasExp, RincUp,  BizGood, BizExGood, FinBetterNext1, FinBetterLast1, UNEMPGood))

fixmc_cps <- panel_fixTcps %>%
  filter(!is.na(RegInf) & !is.na(REGION)) %>%
  select(-c(Bartik, Bartik_out, cenREGION, year, AggUNRATE, yearquarter, ye, UNRATE,   gasExp, RincUp,  BizGood, BizExGood, FinBetterNext1, FinBetterLast1, UNEMPGood))

# Rotemberg weights using Goldsmith-Pinkham, Sorkin, and Swift (2019) R-package.

index = c("REGION", "survey_date")
y = "RegInf"
x = "pe"
Z = setdiff(names(fixl), c(index))
G = "Agg_pe_ind"
controls = NULL
weight = NULL
controls = setdiff(names(fixmc), c(index, y, x, weight))


Bw_out = bw(fixmc, y, x, controls, weight, fixl, Z, fixg, G)

index = c("REGION", "survey_date")
y = "RegInf"
x = "pe"
Z = setdiff(names(fixl), c(index))
G = "Agg_pe_ind"
controls = NULL
weight = NULL
controls = setdiff(names(fixmc_cps), c(index, y, x, weight))

Bw_cps = bw(fixmc_cps, y, x, controls, weight, fixl, Z, fixg, G)

Bwagg_out <- Bw_out %>%
  filter(!is.na(beta))%>%
  group_by(industry) %>%
  summarise(AggAlpha = sum(alpha), AggBeta = sum(alpha*beta)/AggAlpha) 


Bwagg2_out <- Bwagg_out %>%
  top_n(10, AggAlpha) %>%
  arrange(desc(AggAlpha))

BBwaggTime <- Bw_out %>%
  filter(!is.na(beta)) %>%
  group_by(survey_date) %>%
  summarise(AggAlpha = sum(alpha), AggBeta = sum(alpha*beta)/AggAlpha) %>%
  mutate(betat = AggAlpha*AggBeta) 

tikz(file = "figs/tex/weightsTime.tex", height =4, width =6)  
pT <- ggplot(data = BwaggTime, aes(x=survey_date, y= AggAlpha)) + geom_line(colour = rgb(0.0,0.2,0.5)) +
  xlab("date")+ylab("$\\alpha_{k}$") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey"))
print(pT)
dev.off()

#f-stat's

Fk <- matrix(0,160,1)
ck <- matrix(0,160,1)
for (i in 1:160){
  data  <-  filter(region_data2, industry == i);
  
  if (dim(data)[1]>10){
    data2 <- filter(fixg, industry == i);
    data3 <- panel_fixT %>% select(-c("pe", "RegInf", "Bartik", "Bartik_bench_out", "cenREGION"));
    #%>% select(c("survey_date", "yearquarter"))
    #data4 <- fixmc %>% select(-c("pe", "RegInf", "Bartik"))
    #data <- merge(data, data2) %>% merge(data3) %>% merge(data4) %>% mutate(inst = prop*Agg_pe_ind)
    data <- merge(data,data2) %>% merge(data3) %>% mutate(inst = prop*Agg_pe_ind);
    #data <- data %>% panel(~REGION + survey_date);
    est <- feols(pe ~ inst + UNRATE + FinBetterLast1 + AggUNRATE+ RincUp + BizGood + BizExGood + FinBetterNext1 + UNEMPGood   , data)
    
    Fk[i] <- fitstat(est, ~f.stat)[[1]]
  } else {-1000}
  
}

estF_out <- feols(RegInf ~  UNRATE + FinBetterLast1 + RincUp + BizGood + BizExGood + FinBetterNext1 + UNEMPGood | REGION + yearquarter |  pe ~ Bartik_bench_out + yearquarter  , panel_fixT)

bnot_out <- coefficients(estF_out)[[1]]



Bwagg_out <- Bwagg_out %>% mutate(Fstats = Fk) 
Bwagg2_out <- Bwagg_out %>%
  top_n(10, AggAlpha) %>%
  arrange(desc(AggAlpha))
BwaggSmall_out <- Bwagg_out %>% filter(AggBeta>-5, AggBeta<5, abs(Fstats)>5, abs(Fstats) < 200)


Bwagg2_out <- Bwagg2_out %>% mutate(group = c("Ml,18-24,hs,M,NK", "Ml,25-34,hs,M,NK", "Ml,18-24,c,M,NK","Fl,18-24,<hs,M,NK", "Fl,18-24,hs,M,NK", "Ml,18-24,sc,M,NK","Ml,18-24,hs,M,K", "Fl,18-24,sc,M,NK","Ml,35-49,sc,M,NK", "Ml,18-24,c,NM,NK"))

BwaggSmall_out <- Bwagg_out %>% filter(AggBeta>-5, AggBeta<5, abs(Fstats)>5, abs(Fstats) < 200)

printAgg_out <- Bwagg2_out %>% select(c(group,AggAlpha, AggBeta)) %>% mutate(AggAlpha = round(AggAlpha,4), AggBeta = round(AggBeta,4))





kbl(printAgg_out, "latex", booktabs = T, caption = "Top-10 weighted groups: michigan shares", col.names = c("group", "$\\alpha_g$", "$\\beta_g$"),label = "rotweights:michigan",  linesep = "\\addlinespace", escape = FALSE) %>%
  kable_styling(latex_options = c("striped")) %>%
  footnote(general = "Group labels ordered: sex, age, educ., marital, children. Top-10 weighted groups according to the ``Rotemberg'' weights as in Bartik paper." , threeparttable = T) %>%
  save_kable("tables/weightsout.tex")

pWout <- ggplot(data = BwaggSmall_out, aes(x=Fstats, y=AggBeta, size = AggAlpha, fill = industry, shape = AggAlpha<0)) + geom_point(show.legend = FALSE) +
  geom_point(data=Bwagg2_out, aes(x=Fstats, y=AggBeta,  fill = industry), shape =  21, size = 36, alpha = 0.5, colour = "black" ) + 
  scale_fill_viridis(discrete=FALSE, guide="none", option="A") +
  geom_hline(yintercept=bnot_out, linetype = "dashed", colour = "red") + 
  scale_y_continuous(breaks = c(-2, 0, bnot_out, 2, 4), labels = c("-2", "0", TeX("$\\hat{\\beta}$"), "2", "4" )) +
  xlab(TeX("$F_g$"))+ylab(TeX("$\\beta_{g}$")) +
  #ggrepel::geom_label_repel(aes(label = group), data = Bwagg2, box.padding = 4, fill = "white", size = 10) + 
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 24,colour = rgb(.26, .17, .16)), legend.position = "none")  
ggsave("figs/weightsout.pdf", pWout)

# CPS weights.
#conform data sets;


