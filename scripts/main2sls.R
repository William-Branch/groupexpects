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

endo_vars <-  "pe"
instr_vars_out <- "Bartik_out"
instr_vars_bench_out <- "Bartik_bench_out"

base_models <- list(
  "(1)" = run_regression2(panel_out, endo_vars, instr_vars_out, "REGION + yearquarter"),
  "(2)" = run_regression2(panel_out, endo_vars, instr_vars_out, "" ),
  "(3)" = run_regression2(panel_out, endo_vars, instr_vars_out, "REGION" ),
  "(4)" = run_regression2(panel_out, endo_vars, instr_vars_out, "yearquarter" )
)

base_models_bench <- list(
  "(1)" = run_regression2(panel_bench_out, endo_vars, instr_vars_bench_out, "REGION + yearquarter"),
  "(2)" = run_regression2(panel_bench_out, endo_vars, instr_vars_bench_out, "" ),
  "(3)" = run_regression2(panel_bench_out, endo_vars, instr_vars_bench_out, "REGION" ),
  "(4)" = run_regression2(panel_bench_out, endo_vars, instr_vars_bench_out, "yearquarter" )
)

first_base <- lapply(base_models, function(model) model$iv_first_stage)
first_base_bench <- lapply(base_models_bench, function(model) model$iv_first_stage)

endo_vars <- c("pe", "pe5")
instr_vars_out <- c("Bartik_out", "Bartik5_out")
instr_vars_bench_out <- c("Bartik_bench_out", "Bartik5_bench_out")

five_models <- list(
  "(1)" = run_regression2(panel_out, endo_vars, instr_vars_out, "REGION + yearquarter"),
  "(2)" = run_regression2(panel_out, endo_vars, instr_vars_out, "" ),
  "(3)" = run_regression2(panel_out, endo_vars, instr_vars_out, "REGION" ),
  "(4)" = run_regression2(panel_out, endo_vars, instr_vars_out, "yearquarter" )
)

five_models_bench <- list(
  "(1)" = run_regression2(panel_bench_out, endo_vars, instr_vars_bench_out, "REGION + yearquarter"),
  "(2)" = run_regression2(panel_bench_out, endo_vars, instr_vars_bench_out, "" ),
  "(3)" = run_regression2(panel_bench_out, endo_vars, instr_vars_bench_out, "REGION" ),
  "(4)" = run_regression2(panel_bench_out, endo_vars, instr_vars_bench_out, "yearquarter" )
)

first_five <- lapply(five_models, function(model) model$iv_first_stage)
first_five_bench <- lapply(five_models_bench, function(model) model$iv_first_stage)

endo_vars <- "pe5"
instr_vars_out <- "Bartik5_out"
instr_vars_out_bench <- "Bartik5_bench_out"

five_only_models <- list(
  "(1)" = run_regression2(panel_out, endo_vars, instr_vars_out, "REGION + yearquarter"),
  "(2)" = run_regression2(panel_out, endo_vars, instr_vars_out, "" ),
  "(3)" = run_regression2(panel_out, endo_vars, instr_vars_out, "REGION" ),
  "(4)" = run_regression2(panel_out, endo_vars, instr_vars_out, "yearquarter" )
)

five_only_models_bench <- list(
  "(1)" = run_regression2(panel_bench_out, endo_vars, instr_vars_bench_out, "REGION + yearquarter"),
  "(2)" = run_regression2(panel_bench_out, endo_vars, instr_vars_bench_out, "" ),
  "(3)" = run_regression2(panel_bench_out, endo_vars, instr_vars_bench_out, "REGION" ),
  "(4)" = run_regression2(panel_bench_out, endo_vars, instr_vars_bench_out, "yearquarter" )
)

first_five_only <- lapply(five_only_models, function(model) model$iv_first_stage)
first_five_only_bench <- lapply(five_only_models_bench, function(model) model$iv_first_stage)

#tables

first_stage_table(first_base, first_base_bench, "tables/base1yearStage1.tex","base:out:2sls:stage1","2SLS: first stage")
second_stage_table(
  base_models,
  base_models_bench,
  "tables/base1year.tex",
  "base:out:2sls:stage2",
  "2SLS: coefficient estimates",
  "Coefficient estimates from 2sls panel regression of regional inflation on expected inflation. Bartik instruments computed using a leave-one-out procedure. Panel A presents estimates using shift-share constructed from the 1978.1 CPS. Panel B is the shift-share instrument using Michigan survey shares. Regional unemployment is instrumented with 12-month lagged unemployment rates.",
  c("Signif. codes: * = .1; ** = .05; *** = .01.")
  )

first_stage_table(first_five_only, first_five_only_bench, "tables/base5yearStage1.tex","base:5only:2sls:stage1", "2SLS with long expectations: first stage")
second_stage_table(five_only_models,
                   five_only_models_bench,
                   "tables/base5year.tex",
                   "base:5only:2sls:stage2",
                   "2SLS with long expectations: coefficient estimates",
                   "Coefficient estimates with long expectations only. Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
                   c("Signif. codes: * = .1; ** = .05; *** = .01.")
                   )

first_stage_table_five(first_five, first_five_bench, "tables/base1and5yearStage1.tex","table:2sls:shortlong:stage1", "2SLS with short and long expectations: first stage")
second_stage_table_five(five_models,
                   five_models_bench,
                   "tables/base1and5year.tex",
                   "table:2sls:shortlong:stage2",
                   "2SLS with short and long expectations: coefficient estimates",
                   "Coefficient estimates with short and long expectations. Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
                   c("Signif. codes: * = .1; ** = .05; *** = .01.")
)

# est_cps <- run_regression(panel_cps, "pe", "Bartik_cps")
# est_bench <- run_regression(paneldata, "pe" , "Bartik")
# est_out <- run_regression(panel_out, "pe" , "Bartik_out")
# est_bench_out <- run_regression(panel_bench_out, "pe" , "Bartik_bench_out")
# 
# 
# 
# est5_cps <- run_regression(panel_cps, c("pe","pe5"), c("Bartik_cps", "Bartik5_cps"))
# est5_bench <- run_regression(paneldata,c("pe","pe5"), c( "Bartik",  "Bartik5"))
# est5_out <- run_regression(panel_out, c("pe","pe5"), c("Bartik_out", "Bartik5_out"))
# est5_bench_out <- run_regression(panel_bench_out, c("pe","pe5"), c("Bartik_bench_out", "Bartik5_bench_out"))
# est5only_cps <- run_regression(panel_cps, "pe5" , "Bartik5_cps")
# est5only_bench <- run_regression(panel_cps, "pe5" , "Bartik5")
# 
# # Create tables
# table_1 <- etable(est_bench, est_cps, vcov = "DK", dict = c("Bartik_cps" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
# table_2 <- etable(est_bench, est_cps, vcov = "DK", dict = c("yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
# table_1_out <- etable(est_bench_out, est_out, vcov = "DK", dict = c("Bartik_bench_out" = "Bartik",   "Bartik_out" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
# table_2_out <- etable(est_bench_out, est_out, vcov = "DK", dict = c("yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
# table5_1 <- etable(est5_bench, est5_cps, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe",  "Bartik5_cps" = "Bartik5",   "Bartik_cps" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
# table5_2 <- etable(est5_bench, est5_cps, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe","yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
# table5out_1 <- etable(est5_bench_out, est5_out, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe",  "Bartik5_out" = "Bartik5",   "Bartik_out" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
# table5out_2 <- etable(est5_bench_out, est5_out, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe","yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
# 
# table5only_1 <- etable(est5only_bench, est5only_cps, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe",  "Bartik5_cps" = "Bartik5",   "Bartik_cps" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 1, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
# table5only_2 <- etable(est5only_bench, est5only_cps, vcov = "DK", dict = c("pe" = "short-run pe", "pe5" = "long-run pe","yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))
# 
# 
# 
# # create_latex_table_stage1 <- function(data, label, caption, file_path, 
# #                                       linesep = "\\addlinespace",
# #                                       general_footnote = NULL, number_footnote = NULL) {
#   
# #   # Manually setting column names to the new headings
# #   col_names <- c("Instrument", "Bartik inst.", "Unemp. inst.", "Bartik CPS inst.", "Unemp inst.")
#   
# #   # Assuming the first row in `data` contains the original "Bartik" heading that needs to be changed
# #   # And assuming `data` is a data frame or matrix
# #   if("Bartik" %in% rownames(data)) {
# #     rownames(data)[rownames(data) == "Bartik"] <- "Instrument"
# #   }
#   
# #   # Generate the LaTeX table with kable
# #   kable_output <- kbl(data, format = "latex", booktabs = TRUE, 
# #                         caption = caption, align = "l", col.names = col_names, linesep = linesep, label = label) %>%
# #                     kable_styling(latex_options = c("striped", "scale_down"), full_width = FALSE) %>%
# #                     footnote(general = general_footnote, number = number_footnote, threeparttable = TRUE)
#   
# #   # Save the LaTeX table to file
# #   save_kable(kable_output, file = file_path)
# # }
# 
# create_latex_table_stage1 <- function(data, label, caption, file_path, 
#                                       linesep = "\\addlinespace",
#                                       general_footnote = NULL, number_footnote = NULL) {
#     # Step 1: Adjust the column names directly
#     col_names <- c(" ", "Bartik inst.", "Unemp. inst.", "Bartik CPS inst.", "Unemp inst.")
#     
#     # Ensure data is the correct object (a dataframe with expected structure)
#     if (!is.data.frame(data)) stop("Data must be a dataframe")
# 
#     # Step 2: Replace "Bartik" with "Instrument" in the first unnamed column
#     data[[1]][data[[1]] == "Bartik"] <- "Instrument"
# 
#     # Use kable to generate the LaTeX table with custom column names
#     kable_output <- kbl(data, format = "latex", booktabs = TRUE, 
#                         caption = caption, col.names = col_names, linesep = linesep, label = label) %>%
#                     kable_styling(latex_options = c("striped", "scale_down"), full_width = FALSE) %>%
#                     footnote(general = general_footnote, number = number_footnote, threeparttable = TRUE)
#     
#     # Save the generated LaTeX table to file
#     save_kable(kable_output, file = file_path)
# }
# 
# 
# 
# create_latex_table_stage1(
#   data = table_1,  # Assuming this is the preprocessed dataframe
#   label = "table:2sls:stage1:noloo",
#   caption = "2SLS: first stage",
#   file_path = "tables/base1yearStage1noleaveoneout.tex",
#   general_footnote = "Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
#   number_footnote = "Signif. codes: * = .05; ** = .01; *** = .001."
# )
# 
# # Table 1 benchmark
# # create_latex_table(
# #   data = table_1,
# #   label = "table:2sls:stage1:noloo",
# #   caption = "2SLS: first stage",
# #   file_path = "tables/base1yearStage1noleaveoneout.tex",
# #   col_names = c("survey", "CPS78"),
# #   general_footnote = "Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
# #   number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
# # )
# # Table 2 benchmark
# 
# str(table_2)
# 
# create_latex_table(
#   data = table_2,
#   label = "table:2sls:stage2:noloo",
#   caption = "2SLS: coefficient estimates",
#   file_path = "tables/base1yearnooneleaveout.tex",
#   # Provide a name for the unnamed column or leave it as an empty string if you prefer
#   col_names = c("", "Bartik inst.", "Bartik CPS inst."),
#   general_footnote = "Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
#   number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
# )
# 
# 
# # create_latex_table(
# #   data = table_2,
# #   label = "table:2sls:stage2:noloo",
# #   caption = "2SLS: coefficient estimates",
# #   file_path = "tables/base1yearnooneleaveout.tex",
# #   col_names = c(" ", "survey", "CPS78"),
# #   general_footnote = "Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
# #   number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
# # )
# 
# 
# # Table 1 out
# create_latex_table_stage1(
#   data = table_1_out,
#   label = "base:out:2sls:stage1",
#   caption = "2SLS: first stage",
#   file_path = "tables/base1yearStage1.tex",
#   #col_names = c("survey", "CPS78"),
#   general_footnote = "First stage from 2sls panel regression of regional inflation on expected inflation. In the first stage, the Bartik instrument is a good predictor of inflation expectations. Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
#   number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
# )
# 
# # Table 2 out
# create_latex_table(
#   data = table_2_out,
#   label = "base:out:2sls:stage2",
#   caption = "2SLS: coefficient estimates",
#   file_path = "tables/base1year.tex",
#   col_names = c(" ", "survey", "CPS78"),
#   general_footnote = "Coefficient estimates from 2sls panel regression of regional inflation on expected inflation. Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
#   number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
# )
# 
# # Table 1 benchmark, 1 and 5 year;
# create_latex_table_stage1(
#   data = table5_1,
#   label = "table:2sls:shortlong:stage1:noloo",
#   caption = "2SLS with short and long expectations: first stage",
#   file_path = "tables/bench1and5yearStage1.tex",
#   #col_names = c("survey short pe","survey long pe", "CPS78 short pe", "CPS78 long pe"),
#   general_footnote = "Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
#   number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001."), 
#  # multi_instruments = TRUE
# )
# # Table 2 benchmark, 1 and 5 year;
# create_latex_table(
#   data = table5_2,
#   label = "table:2sls:shortlong:stage2:noloo",
#   caption = "2SLS with short and long expectations: coefficient estimates",
#   file_path = "tables/bench1and5year.tex",
#   col_names = c(" ", "survey", "CPS78"),
#   general_footnote = "Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
#   number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001."),
#   multi_instruments = TRUE
# )
# 
# # Table 1 leave one out, 1 and 5 year;
# create_latex_table(
#   data = table5out_1,
#   label = "table:2sls:shortlong:stage1",
#   caption = "2SLS with short and long expectations: first stage",
#   file_path = "tables/base1and5yearStage1.tex",
#   col_names = c("survey short pe","survey long pe", "CPS78 short pe", "CPS78 long pe"),
#   general_footnote = "Reports first stage from a panel regression with instruments for short and long expectations. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
#   number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001."),
#   multi_instruments = TRUE
# )
# # Table 2 leave one out, 1 and 5 year;
# create_latex_table(
#   data = table5out_2,
#   label = "table:2sls:shortlong:stage2",
#   caption = "2SLS with short and long expectations: coefficient estimates",
#   file_path = "tables/base1and5year.tex",
#   col_names = c("survey", "CPS78"),
#   general_footnote = "Coefficient estimates with instruments for short and long expectations. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
#   number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001."),
#   multi_instruments = TRUE
# )
# 
# # Table 1 5 only
# create_latex_table(
#   data = table5only_1,
#   label = "base:5only:2sls:stage1",
#   caption = "2SLS with long expectations: first stage",
#   file_path = "tables/base5yearStage1.tex",
#   #col_names = c("survey", "CPS78"),
#   general_footnote = "First stage with long expectations only. Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
#   number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
# )
# 
# # Table 2 5 only
# create_latex_table(
#   data = table5only_2,
#   label = "base:5only:2sls:stage2",
#   caption = "2SLS with long expectations: coefficient estimates",
#   file_path = "tables/base5year.tex",
#   col_names = c(" ", "survey", "CPS78"),
#   general_footnote = "Coefficient estimates with long expectations only. Instruments computed using a leave-one-out procedure. Survey is the shift-share instrument using Michigan survey shares. CPS78 is constructed from the 1978.1 CPS.",
#   number_footnote = c("Signif. codes: * = .05; ** = .01; *** = .001.")
# )

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
  print(model_info$coef)
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

