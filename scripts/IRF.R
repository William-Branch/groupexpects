# Plots IRF's for CPS and michigan weights.
# outputs: figs/tex/IRFcps.tex, figs/tex/IRFout.tex.

source('scripts/common_libraries.R')
source('functions/functions_results.R')


paneldata <-  read_csv("data/dataBuild/paneldata.csv") %>%
  mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
  panel(~REGION + survey_date)

panel_bench_out <- read_csv("data/dataBuild/panelbenchout.csv")#%>% panel(~REGION + survey_date)
panel_out <- read_csv("data/dataBuild/panelcpsout.csv") #%>% panel(~REGION + survey_date)

panel_bench_out <- preprocess_panel_data(panel_bench_out)
panel_out <- preprocess_panel_data(panel_out)


# cpser <- feols(csw0(RegInf, l(RegInf,-1), l(RegInf, -2), l(RegInf,-3), l(RegInf,-4), l(RegInf, -5), l(RegInf, -6), l(RegInf, -7), l(RegInf,-8), l(RegInf,-9)) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK")
# cb <- coef(cpser)
# 
# cps1 <- confint(feols(RegInf ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
# cps2 <- confint(feols(l(RegInf,-1) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
# cps3 <- confint(feols(l(RegInf,-2) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
# cps4 <- confint(feols(l(RegInf,-3) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
# cps5 <- confint(feols(l(RegInf,-4) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
# cps6 <- confint(feols(l(RegInf,-5) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
# cps7 <- confint(feols(l(RegInf,-6) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
# cps8 <- confint(feols(l(RegInf,-7) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
# cps9 <- confint(feols(l(RegInf,-8) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))
# cps10 <- confint(feols(l(RegInf,-9) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_out  , panel_out, vcov = "DK"))

# 
# 
# piPredcps <- as_tibble(cb[1:10,1]) %>%
#   #rbind(-0.001) %>%
#   mutate(inflation = value, qtrs = c(0,1,2,3,4,5,6,7,8,9),lb = c(cps1[1,1], cps2[1,1], cps3[1,1], cps4[1,1], cps5[1,1], cps6[1,1], cps7[1,1], cps8[1,1], cps9[1,1], cps10[1,1]), ub = c(cps1[1,2], cps2[1,2], cps3[1,2], cps4[1,2], cps5[1,2], cps6[1,2], cps7[1,2], cps8[1,2], cps9[1,2], cps10[1,2])) 

# Function to run regression and get coefficients and confidence intervals
# run_regression_irf <- function(lag, panel_data, bartik_var) {
#   model <- feols(paste0("csw0(RegInf, l(RegInf,", lag, ")) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~ ", bartik_var), panel_data, vcov = "DK")
#   coef_val <- coef(model)
#   conf_val <- confint(model)
#   return(list(coef_val, conf_val))
# }

# run_regression_irf <- function(lag, panel_data, bartik_var) {
#   formula_text <- paste0("csw0(RegInf, l(RegInf,", lag, ")) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~ ", bartik_var)
#   formula_obj <- as.formula(formula_text)
#   model <- feols(formula_obj, panel_data, vcov = "DK")
#   coef_val <- coef(model)
#   conf_val <- confint(model)
#   return(list(coef_val, conf_val))
# }
# 
# run_regression_irf <- function(lag, panel_data, bartik_var) {
#   formula_text <- paste0("csw0(RegInf, l(RegInf,", lag, ")) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~ ", bartik_var)
#   formula_obj <- as.formula(formula_text)
#   model <- feols(formula_obj, panel_data, vcov = "DK")
#   
#   # Extract the sub-model corresponding to the lag
#   sub_model <- model[[lag + 1]]
#   
#   coef_val <- coef(sub_model)
#   conf_val <- confint(sub_model)
#   
#   return(list(coef_val, conf_val))
# }

run_regression_irf <- function(lag, panel_data, bartik_var) {
  formula_text <- paste0("l(RegInf,", lag, ") ~  l(RegInf,1:2)  + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~ ", bartik_var)
  formula_obj <- as.formula(formula_text)
  model <- feols(formula_obj, panel_data, vcov = "DK")
  
  coef_val <- coef(model)
  conf_val <- confint(model)
  
  return(list(coef_val, conf_val))
}


# Initialize lists to store results
# coef_list_cps <- list()
# conf_list_cps <- list()
# coef_list_out <- list()
# conf_list_out <- list()

# Loop through lags
lags <- -9:0
coef_list_cps <- vector("list", length(lags))
conf_list_cps <- vector("list", length(lags))
coef_list_out <- vector("list", length(lags))
conf_list_out <- vector("list", length(lags))

#for(lag in lags) {
for(i in seq_along(lags)) {
  lag <- lags[i]
  results_cps <- run_regression_irf(lag, panel_out, "Bartik_out")
  coef_list_cps[[11-i]] <- results_cps[[1]]
  conf_list_cps[[11-i]] <- results_cps[[2]]
  
  results_out <- run_regression_irf(lag, panel_bench_out, "Bartik_bench_out")
  coef_list_out[[11-i]] <- results_out[[1]]
  conf_list_out[[11-i]] <- results_out[[2]]
}

# Convert lists to tibbles

piPredcps <- as_tibble(do.call(rbind, coef_list_cps)) %>% 
  rename(inflation = fit_pe) %>%
  mutate(qtrs = lags,
         lb = sapply(conf_list_cps, function(x) x[1,1]),
         ub = sapply(conf_list_cps, function(x) x[1,2]))

piPredout <- as_tibble(do.call(rbind, coef_list_out)) %>% 
  rename(inflation = fit_pe) %>%
  mutate(qtrs = lags,
         lb = sapply(conf_list_out, function(x) x[1,1]),
         ub = sapply(conf_list_out, function(x) x[1,2]))


tikz(file = "figs/tex/IRFcps.tex", height=3, width =3)
pIRFc <- ggplot(data=piPredcps, aes(x=qtrs, y = inflation)) + geom_line(size = 2.0, linetype = "dashed", colour="red")+geom_ribbon(aes(x=qtrs, ymax=ub, ymin=lb), fill="grey", alpha=.4)+theme(panel.border = element_blank(),
                                                                                                                                                                                               panel.background = element_blank(),
                                                                                                                                                                                               panel.grid.minor = element_blank(),
                                                                                                                                                                                               axis.line = element_line(colour = "grey"))
print(pIRFc)
dev.off()

# piPredout <- as_tibble(co[1:10,1]) %>%
#   mutate(inflation = value, qtrs = c(0,1,2,3,4,5,6,7,8,9),lb = c(cps1[1,1], cps2[1,1], cps3[1,1], cps4[1,1], cps5[1,1], cps6[1,1], cps7[1,1], cps8[1,1], cps9[1,1], cps10[1,1]), ub = c(cps1[1,2], cps2[1,2], cps3[1,2], cps4[1,2], cps5[1,2], cps6[1,2], cps7[1,2], cps8[1,2], cps9[1,2], cps10[1,2])) 

tikz(file = "figs/tex/IRFout.tex", height=3, width =3)
pIRFo <- ggplot(data=piPredout, aes(x=qtrs, y = inflation)) + geom_line(size = 2.0, linetype = "dashed", colour="red")+geom_ribbon(aes(x=qtrs, ymax=ub, ymin=lb), fill="grey", alpha=.4)+theme(panel.border = element_blank(),
                                                                                                                                                                                               panel.background = element_blank(),
                                                                                                                                                                                               panel.grid.minor = element_blank(),
                                                                                                                                                                                               axis.line = element_line(colour = "grey"))
print(pIRFo)
dev.off()

# outer <- feols(csw0(RegInf, l(RegInf,-1), l(RegInf, -2), l(RegInf,-3), l(RegInf,-4), l(RegInf, -5), l(RegInf, -6), l(RegInf, -7), l(RegInf,-8), l(RegInf,-9)) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK")
# co <- coef(outer)
# 
# cps1 <- confint(feols(RegInf ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
# cps2 <- confint(feols(l(RegInf,-1) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
# cps3 <- confint(feols(l(RegInf,-2) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
# cps4 <- confint(feols(l(RegInf,-3) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
# cps5 <- confint(feols(l(RegInf,-4) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
# cps6 <- confint(feols(l(RegInf,-5) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
# cps7 <- confint(feols(l(RegInf,-6) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
# cps8 <- confint(feols(l(RegInf,-7) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
# cps9 <- confint(feols(l(RegInf,-8) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
# cps10 <- confint(feols(l(RegInf,-9) ~  l(RegInf,1:2)  +UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION +yearquarter | pe  ~  Bartik_bench_out  , panel_bench_out, vcov = "DK"))
# 
# 
# 
# piPredout <- as_tibble(co[1:10,1]) %>%
#   #rbind(-0.001) %>%
#   mutate(inflation = value, qtrs = c(0,1,2,3,4,5,6,7,8,9),lb = c(cps1[1,1], cps2[1,1], cps3[1,1], cps4[1,1], cps5[1,1], cps6[1,1], cps7[1,1], cps8[1,1], cps9[1,1], cps10[1,1]), ub = c(cps1[1,2], cps2[1,2], cps3[1,2], cps4[1,2], cps5[1,2], cps6[1,2], cps7[1,2], cps8[1,2], cps9[1,2], cps10[1,2])) 
# 
# tikz(file = "figs/tex/IRFout.tex", height=3, width =3)
# pIRFo <- ggplot(data=piPredout, aes(x=qtrs, y = inflation)) + geom_line(size = 2.0, linetype = "dashed", colour="red")+geom_ribbon(aes(x=qtrs, ymax=ub, ymin=lb), fill="grey", alpha=.2)+theme(panel.border = element_blank(),
#                                                                                                                                                                                                panel.background = element_blank(),
#                                                                                                                                                                                                panel.grid.minor = element_blank(),
#                                                                                                                                                                                                axis.line = element_line(colour = "grey"))
# print(pIRFo)
# dev.off()