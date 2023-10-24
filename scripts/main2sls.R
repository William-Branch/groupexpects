# main2sls.R

# Load common libraries
library(dotenv)
print("dotenv library loaded")
source('scripts/common_libraries.R')
load_dot_env(".env")
fred_api_key <- Sys.getenv("FRED_API_KEY")
fredr_set_key(fred_api_key)
# Load functions required for data processing
#source('functions/functions_data.R')
print("functions/results")
source('functions/functions_results.R')
print("after functions/results")
# Data files created by data_build
main_panel_path <- 'data/dataBuild/paneldata.csv'
cps_panel_path <- 'data/dataBuild/panelcps.csv'
main_out_panel_path <- 'data/dataBuild/panelbenchout.csv'
cps_out_panel_path <- 'data/dataBuild/panelcpsout.csv'

# Load data
paneldata <- load_panel_data(main_panel_path)
panel_cps <- load_panel_data(cps_panel_path)
panel_bench_out <- load_panel_data(main_out_panel_path)
panel_out <- load_panel_data(cps_out_panel_path)

#estimation

est_cps <- run_regression(panel_cps, "pe", "Bartik_cps")
est_bench <- run_regression(paneldata, "pe" , "Bartik")
est_out <- run_regression(panel_out, "pe" , "Bartik_out")
est_bench_out <- run_regression(panel_bench_out, "pe" , "Bartik_bench_out")



est5_cps <- run_regression(panel_cps, c("pe","pe5"), c("Bartik_cps", "Bartik5_cps"))
est5_bench <- run_regression(paneldata,c("pe","pe5"), c( "Bartik",  "Bartik5"))
est5_out <- run_regression(panel_out, c("pe","pe5"), c("Bartik_out", "Bartik5_out"))
est5_bench_out <- run_regression(panel_bench_out, c("pe","pe5"), c("Bartik_bench_out", "Bartik5_bench_out"))
est5only_cps <- run_regression(panel_cps, "pe5" , "Bartik5_cps")
est5only_bench <- run_regression(panel_cps, "pe5" , "Bartik5")

# Create tables
table_1 <- etable(est_bench, est_cps, vcov = "DK", dict = c("Bartik_cps" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_2 <- etable(est_bench, est_cps, vcov = "DK", dict = c("yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_1_out <- etable(est_bench_out, est_out, vcov = "DK", dict = c("Bartik_bench_out" = "Bartik",   "Bartik_out" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table_2_out <- etable(est_bench_out, est_out, vcov = "DK", dict = c("yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table5_1 <- etable(est5_bench, est5_cps, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe",  "Bartik5_cps" = "Bartik5",   "Bartik_cps" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table5_2 <- etable(est5_bench, est5_cps, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe","yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table5out_1 <- etable(est5_bench_out, est5_out, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe",  "Bartik5_out" = "Bartik5",   "Bartik_out" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table5out_2 <- etable(est5_bench_out, est5_out, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe","yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))

table5only_1 <- etable(est5only_bench, est5only_cps, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe",  "Bartik5_cps" = "Bartik5",   "Bartik_cps" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
table5only_2 <- etable(est5only_bench, est5only_cps, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe","yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))


# Table 1 benchmark
create_latex_table(
  data = table_1,
  label = "table:2sls:stage1:noloo",
  caption = "2SLS: first stage",
  file_path = "tables/base1yearStage1noleaveoneout.tex",
  col_names = c("survey", "CPS78"),
  general_footnote = "Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
  number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
)
# Table 2 benchmark
create_latex_table(
  data = table_2,
  label = "table:2sls:stage2:noloo",
  caption = "2SLS: coefficient estimates",
  file_path = "tables/base1yearnooneleaveout.tex",
  col_names = c("survey", "CPS78"),
  general_footnote = "Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
  number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
)
# Table 1 out
create_latex_table(
  data = table_1_out,
  label = "base:out:2sls:stage1",
  caption = "2SLS: first stage",
  file_path = "tables/base1yearStage1.tex",
  col_names = c("survey", "CPS78"),
  general_footnote = "First stage from 2sls panel regression of regional inflation on expected inflation. In the first stage, the Bartik instrument is a good predictor of inflation expectations. Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
  number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
)

# Table 2 out
create_latex_table(
  data = table_2_out,
  label = "base:out:2sls:stage2",
  caption = "2SLS: coefficient estimates",
  file_path = "tables/base1year.tex",
  col_names = c("survey", "CPS78"),
  general_footnote = "Coefficient estimates from 2sls panel regression of regional inflation on expected inflation. Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
  number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
)

# Table 1 benchmark, 1 and 5 year;
create_latex_table(
  data = table5_1,
  label = "table:2sls:shortlong:stage1:noloo",
  caption = "2SLS with short and long expectations: first stage",
  file_path = "tables/bench1and5yearStage1.tex",
  col_names = c("survey short pe","survey long pe", "CPS78 short pe", "CPS78 long pe"),
  general_footnote = "Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
  number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001."), 
  multi_instruments = TRUE
)
# Table 2 benchmark, 1 and 5 year;
create_latex_table(
  data = table5_2,
  label = "table:2sls:shortlong:stage2:noloo",
  caption = "2SLS with short and long expectations: coefficient estimates",
  file_path = "tables/bench1and5year.tex",
  col_names = c("survey", "CPS78"),
  general_footnote = "Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
  number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001."),
  multi_instruments = TRUE
)

# Table 1 leave one out, 1 and 5 year;
create_latex_table(
  data = table5out_1,
  label = "table:2sls:shortlong:stage1",
  caption = "2SLS with short and long expectations: first stage",
  file_path = "tables/base1and5yearStage1.tex",
  col_names = c("survey short pe","survey long pe", "CPS78 short pe", "CPS78 long pe"),
  general_footnote = "Reports first stage from a panel regression with instruments for short and long expectations. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
  number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001."),
  multi_instruments = TRUE
)
# Table 2 leave one out, 1 and 5 year;
create_latex_table(
  data = table5out_2,
  label = "table:2sls:shortlong:stage2",
  caption = "2SLS with short and long expectations: coefficient estimates",
  file_path = "tables/base1and5year.tex",
  col_names = c("survey", "CPS78"),
  general_footnote = "Coefficient estimates with instruments for short and long expectations. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
  number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001."),
  multi_instruments = TRUE
)

# Table 1 5 only
create_latex_table(
  data = table5only_1,
  label = "base:5only:2sls:stage1",
  caption = "2SLS with long expectations: first stage",
  file_path = "tables/base5yearStage1.tex",
  col_names = c("survey", "CPS78"),
  general_footnote = "First stage with long expectations only. Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
  number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
)

# Table 2 5 only
create_latex_table(
  data = table5only_2,
  label = "base:5only:2sls:stage2",
  caption = "2SLS with long expectations: coefficient estimates",
  file_path = "tables/base5year.tex",
  col_names = c("survey", "CPS78"),
  general_footnote = "Coefficient estimates with long expectations only. Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
  number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
)

# First-stage and reduced-form plots.


configurations <- list(
  list(data = paneldata, formula = 'RegInf ~ Bartik', labels = list(y = "reg. inflation", x = "Bartik (mich.)"), filename = "figs/tex/redform.tex"),
  list(data = paneldata, formula = 'pe ~ Bartik', labels = list(y = "$p^e$ (survey)", x = "Bartik (mich.)"), filename = "figs/tex/firststage.tex"),
  list(data = panel_cps, formula = 'RegInf ~ Bartik_cps', labels = list(y = "reg. inflation", x = "Bartik (CPS)"), filename = "figs/tex/redform_cps.tex"),
  list(data = panel_cps, formula = 'pe ~ Bartik_cps', labels = list(y = "$p^e$ (survey)", x = "Bartik (CPS)"), filename = "figs/tex/firststage_cps.tex"),
  list(data = filter(paneldata,!Bartik5==0), formula = 'pe5 ~ Bartik5', labels = list(y = ("pe5 (survey)"), x = "Bartik5 (mich.)"), filename = "figs/tex/firstStage5only.tex"),
  list(data = filter(paneldata,!Bartik5==0), formula = 'RegInf ~ Bartik5', labels = list(y = ("reg. inflation"), x = "Bartik5 (mich.)"), filename = "figs/tex/redform5only.tex")
)

# Main loop to fit models and create plots
for (config in configurations) {
  model_info <- fit_model(config$data, as.formula(config$formula))
  create_and_save_plot(config$data, all.vars(as.formula(config$formula))[2], all.vars(as.formula(config$formula))[1], config$labels, config$filename, model_info$coef)
}




# Jacknife correction.



panel_bench_out2 <- as.data.frame(panel_bench_out)
panel_out2 <- as.data.frame(panel_out)
datasets <- list(panel_bench_out2 = panel_bench_out2, panel_out2 = panel_out2)
bartik_vars <- c("Bartik_bench_out", "Bartik_out")



results <- lapply(1:2, function(i) {
  calculate_bias_correction2(datasets[[i]], bartik_vars[i])
})
view(results)

adjT <- as.tibble(c(round(results[[1]]$adj_out, 4), round(results[[1]]$se, 4))) %>%
  cbind(c(round(results[[2]]$adj_out, 4), round(results[[2]]$se, 4)))

        #)

colnames(adjT) <- c("survey shares", "CPS78 shares")
rownames(adjT) <- c("coeff.", "se ")
kbl(adjT, "latex", booktabs = T, caption = "Bias Correction", label = "bias:out:2sls:stage2") %>%
  kable_styling(latex_options = "striped") %>%
  footnote(general = "Applies the split-sample jacknife bias correction to the 2sls coefficient estimates. The column ``survey shares'' computes shares from Michigan survey,``CPS78 shares'' uses the CPS 1978.1 shares." , threeparttable = T) %>%
  save_kable("tables/biascorrections.tex")

