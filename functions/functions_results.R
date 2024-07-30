#functions_results.R
# Place this code snippet at the beginning of functions_results.R



# Function to preprocess Michigan data
preprocess_michigan_data <- function(df) {
  df %>%
    mutate(survey_date = as.Date(paste0(df[["YYYYMM"]], 01), format = "%Y%m%d")) %>%
    filter(PX1 < 96, PX1 != -97, PX1 %in% (-25:25))
}

# Function to preprocess panel data
preprocess_panel_data <- function(df) {
  df %>%
    mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
    panel(~REGION + survey_date)
}


#Function to preprocess region data
preprocess_region_data <- function(df) {
  df %>% 
    drop_na() %>%
    group_by(REGION, group) %>%
    summarise(MnProp = mean(prop)) 
}

# Function to calculate mean 'pe' by grouping variable and date
calculate_mean_pe <- function(df, group_var) {
  df %>%
    group_by(!!sym(group_var), survey_date) %>%
    summarise(pe = mean(PX1, na.rm = TRUE)) %>%
    filter(!is.na(pe), !is.na(!!sym(group_var)))
}

# Function to calculate 'percentage' for Region data
calculate_region_percentage <- function(df) {
  df %>%
    group_by(survey_date, REGION) %>%
    summarise(n = sum(pe), pe = pe) %>%
    mutate(percentage = n / sum(n), REGION = as.character(REGION)) %>%
    filter(percentage > 0)
}

# Function to calculate 'pe' for Industry data
calculate_industry_pe <- function(df, industry_codes) {
  df %>%
    select(c("survey_date", "PX1", "industry")) %>%
    filter(industry %in% industry_codes) %>%
    group_by(industry, survey_date) %>%
    summarise(pe = mean(PX1, na.rm = TRUE)) %>%
    mutate(industry = as.character(industry))
}

# Function to calculate 'pe' for AllIndustry data
calculate_all_industry_pe <- function(df, exclude_industry_codes) {
  df %>%
    select(c("survey_date", "PX1", "industry")) %>%
    filter(!(industry %in% exclude_industry_codes)) %>%
    group_by(industry, survey_date) %>%
    summarise(pe = mean(PX1, na.rm = TRUE)) %>%
    mutate(industry = as.character(industry))
}

#Function to generate Aggregate pie/pi plot
generate_aggregate_scatter <- function(df) {

  yresidA <- resid(lm(infl ~  AggUNRATE + lag(infl,1) + lag(infl,2) + lag(infl, 3) + lag(infl,4), data = df))
  xresidA <- resid(lm(peavg ~  AggUNRATE + lag(infl,1) + lag(infl,2) + lag(infl, 3) + lag(infl,4), data = df))
  Acoeffs <- lm(yresidA ~xresidA, data=NULL)
  
  tikz(file = "figs/tex/Aggscatter.tex", height = 3, width = 3)
  pAggUS <- ggplot(data=NULL, aes(x=xresidA, y=yresidA))+geom_point(size = 1.0, alpha = 0.5)+ geom_abline(slope = coefficients(Acoeffs)[[2]], intercept = coefficients(Acoeffs)[[1]], col = rgb(.541,.2,.141), size = 1.2) +
    annotate("text", label = "slope = 0.18", x = 2.0, y=-1.0, size =03,col = rgb(.541,.2,.141)) +
    annotate("segment", x = 2.0, xend = 3.0, y = -0.90, yend = .5,
             colour = rgb(.541,.2,.141), size = 0.5, alpha =0.5, arrow = arrow(length = unit(0.03, "npc"))) +
    labs(y = ("U.S. inflation"), x = "inflation expect.'s")+
    theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 10), legend.position = "none")  
  print(pAggUS)
  dev.off()
}

generate_regional_scatter <- function(df) {
  
  yresid <- resid(feols(RegInf ~   l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter ,   df))
  xresid <- resid(feols(pe ~   l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter ,   df))
  
  rcoeffs <- lm(yresid ~xresid, data=NULL)
  tikz(file = "figs/tex/Regscatter.tex", height = 3, width = 3)
  pREGUS <- ggplot(data=NULL, aes(x=xresid, y=yresid))+geom_point(size=1.0, alpha = 0.5)+ geom_abline(slope = coefficients(rcoeffs)[[2]], intercept = coefficients(rcoeffs)[[1]], col = rgb(.541,.2,.141), size = 1.2)+
    annotate("text", label = "slope = 0.069", x = 2.05, y=-1.0, size =03,col = rgb(.541,.2,.141)) +
    annotate("segment", x = 2.00, xend = 3.2, y = -0.90, yend = .15,
             colour = rgb(.541,.2,.141), size = 0.5, alpha =0.5, arrow = arrow(length = unit(0.03, "npc"))) +
    labs(y = ("regional inflation"), x = "inflation expect.'s")+
    theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 10), legend.position = "none")  
  print(pREGUS)
  dev.off()
  
  
}

# Function to generate the men/women comparison plot
generate_men_women_plot <- function(df) {
  tikz(file = "figs/tex/menwomen.tex", width = 6.5, height=3)
  pMW <- ggplot(data = df, aes(x = survey_date, y = pe, group = SEX, color = factor(SEX))) +
    geom_line() +
    scale_color_manual(labels = c("men", "women"), values = c(rgb(.541, .2, .141), rgb(0, 0.2, 0.5))) +
    labs(x = "survey date", y = "$p^e$", color = " ")+
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "grey"),
      text = element_text(size = 10)
    ) 
  print(pMW)
  dev.off()
}

# Function to generate the cross-region comparison plot
generate_cross_region_plot <- function(df) {
  tikz(file = "figs/tex/regions.tex", width = 6.5, height = 3)
  pCR <- ggplot(data = df, aes(x=survey_date, y = percentage, fill = factor(REGION)))+geom_area(alpha = 0.6, size=1, colour="black")+ylim(-.1,1.02) + labs(y = "$p^e$ ", x = "survey date", fill = "Region")+
    scale_color_manual(labels = c("W", "MW", "NE", "S"))+ scale_fill_viridis(discrete=TRUE,  option="E",labels = c("W", "MW", "NE", "S")) +
    theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 10))
  print(pCR)
  dev.off()
}

# Function to generate the top 10 weighted industries plot
generate_top10_industry_plot <- function(df) {
  
  pWt10 <- ggplot(data = df, aes(x=survey_date, y = pe, fill = industry, color = industry)) + geom_stream(type="proportional") +
    labs(y = TeX("pe"), x = "survey date") + #scale_x_date(breaks = "10 years") +
    theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 12, colour = rgb(.26, .17, .16)), axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position = "none")  
  return(pWt10) 
}

# Function to generate the all industries plot
generate_all_industry_plot <- function(df) {
  
  pAgg <- ggplot(data = df, aes(x=survey_date, y = pe, fill = industry, color = industry)) + geom_stream(type="ridge") +
    labs(y = TeX("pe"), x = "survey date") + 
    theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 12,colour = rgb(.26, .17, .16)), axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position = "none")  
  return(pAgg)
}

# Function to generate the diverse weights plot
generate_diverse_weights_plot <- function(df1, df2) {
  
  df1 <- df1 %>%
    mutate(industry = group)
  df2 <- df2 %>%
    mutate(industry = group)
  df2$year <- floor_date(ymd(df2$survey_date), "year")
  df2_annual <- df2 %>%
    group_by(year, REGION, industry) %>%
    summarise(mean_prop = mean(prop, na.rm = TRUE))
  
  pD1 <- ggplot(data = df1, aes(x=industry, y= 100*MnProp, group = industry, fill = industry)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
    labs(y = "$\\omega_{R,g,t}$", x = "group")+ 
    theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 12, colour = rgb(.26, .17, .16)), legend.position = "none")  
  # pD2 <- ggplot(data = drop_na(df2), aes(x=survey_date, y = 100*prop, group = survey_date, fill = industry)) + geom_boxplot( outlier.shape = 1) +
  #   stat_boxplot(geom ='errorbar') + facet_wrap(. ~ REGION, nrow = 2, labeller = labeller(REGION = 
  #                                                                                           c("1" = "west",
  #                                                                                             "2" = "midwest",
  #                                                                                             "3" = "northeast", "4" = "south")
  #   )) +
  #   labs(y = TeX("$\\omega_{R,g,t}$ "), x = "date")+ 
  #   theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 14), legend.position = "none")  
  # 
  pD2 <- ggplot(data = drop_na(df2_annual), aes(x=year, y = 100*mean_prop, group = year)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 1) +
    stat_boxplot(geom ='errorbar') +
    facet_wrap(. ~ REGION, nrow = 2, labeller = labeller(REGION = c("1" = "west", "2" = "midwest", "3" = "northeast", "4" = "south"))) +
    labs(y = TeX("$\\omega_{R,g,t}$ "), x = "Year") +
    theme(strip.background = element_rect(fill="#C2A061"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "grey"),
          text = element_text(size = 14),
          legend.position = "none")
  
  return(list(pD1 = pD1, pD2 = pD2))  # assuming pD1 and pD2 are the ggplot objects for your two diverse weights plots
}

generate_cps_weights_plot <- function(df) {
  
  
  tikz(file = "figs/tex/weightsCPS78.tex", width =6.5, height =3.5)
  pD3 <- ggplot(data = df, aes(x=group, y= 100*prop, group = group, fill = group)) + geom_boxplot(outlier.colour = "red", outlier.shape = )+
    labs(y = "$\\omega_{R,g,t}$ ", x = "group")+ 
    theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 12,colour = rgb(.26, .17, .16)), legend.position = "none")  
  print(pD3)
  dev.off()
  
}

# Function that plots distribution of shares.

plot_share_distributions <- function(df1, df2){
  #df1 should be region_CPS78
  #df2 should be regiondata
  
  # Create a data frame for group definitions
  group_indices_list <- list(
    Men = c(1:80),
    Women = c(81:160),
    `Ages 18-24` = c(1:16, 81:96),
    `Ages 25-34` = c(17:32, 97:112),
    `Ages 35-49` = c(33:48, 113:128),
    `Ages 50-64` = c(49:64,129:144),
    `Ages 65+` = c(65:80, 145:160),
    `w/h.s.` = c(5:8,21:24,37:40,53:56,69:72,85:88,101:104,117:120,133:136,149:152),
    `college +` = c(13:16,29:32,45:48,61:64,77:80,93:96,109:112,125:128,141:144,157:160)
  )
  
  # Convert list to data frame
  group_definitions <- stack(group_indices_list) %>%
    rename(group = values, category = ind)
  
  Groupdata <- df1 %>%
    filter(!is.na(group) & !is.na(cenREGION) & !is.na(prop))
  
  Groupdata <- Groupdata %>%
    filter(prop<0.10)%>% 
    left_join(group_definitions, by = "group")
  
  # Calculate broader categories
  Groupdata <- Groupdata %>%
    mutate(
      sex = case_when(
        group %in% group_indices_list$Men ~ "Men",
        group %in% group_indices_list$Women ~ "Women",
        TRUE ~ "sex unknown"
      ),
      age = case_when(
        group %in% group_indices_list$`Ages 18-24` ~ "Ages 18-24",
        group %in% group_indices_list$`Ages 25-34` ~ "Ages 25-34",
        group %in% group_indices_list$`Ages 35-49` ~ "Ages 35-49",
        group %in% group_indices_list$`Ages 50-64` ~ "Ages 50-64",
        group %in% group_indices_list$`Ages 65+` ~ "Ages 65+",
        TRUE ~ "age unknown"
      ),
      education = case_when(
        group %in% group_indices_list$`w/h.s.` ~ "w/h.s.",
        group %in% group_indices_list$`college +` ~ "college +",
        TRUE ~ "< h.s."
      ),
      broad_category = paste(sex, age, education, sep = "-")
    )
  
  region_totals <- Groupdata %>%
    group_by(cenREGION) %>%
    summarize(total_n = sum(n))
  
  # Step 2: Merge the total `n` for each region back into the main data
  Groupdata <- Groupdata %>%
    left_join(region_totals, by = "cenREGION")
  
  catN <- Groupdata %>% group_by(category) %>% summarize(catN = sum(n, na.r = TRUE)) 
  nCPS <- catN$catN[1]+catN$catN[2]
  
  Groupdata <- Groupdata %>%
    mutate(sex = ifelse(category %in% c("Men", "Women"), category, NA_character_))
  
  # Filter to include only rows where sex is defined
  Groupdata_filtered <- Groupdata %>%
    filter(!is.na(sex))
  
  # Aggregating
  region_sex_totals <- Groupdata_filtered %>%
    group_by(cenREGION, sex) %>%
    summarize(total_n = sum(n))
  
  region_sex_totals <- region_sex_totals %>%
    group_by(cenREGION) %>% summarise(nSreg = sum(total_n, na.r = TRUE))
  
  
  Groupdata <- Groupdata %>%
    left_join(region_sex_totals, by = c("cenREGION"))
  
  
  # CPS summaries...
  summary_table_3 <- Groupdata %>%
    group_by(cenREGION, category) %>%
    summarize(new_prop = sum(n) / first(nSreg)) %>%
    spread(cenREGION, new_prop)
  
  
  print(summary_table_3) 
  
  
  Groupdata <- Groupdata %>%
    group_by(cenREGION, broad_category) %>%
    mutate(new_prop = sum(n) / first(nSreg)) %>%
    ungroup()
  
  Groupdata <- Groupdata %>%
    mutate(cenREGION = factor(cenREGION, levels = c(1, 2, 3, 4), labels = c("W", "MW", "NE", "S")))
  
  
  
  p1 <- ggplot(Groupdata, aes(x = reorder(broad_category, new_prop, sum), y = new_prop, fill = factor(cenREGION))) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ cenREGION, scales = "fixed") +
    labs(x = "Group", y = "Proportion of Population Share", fill = "Census Region",
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4), legend.position = "none")
  
  # Export the plot as a TikZ/PGFplots file
  tikz("figs/tex/CPS78_distribution.tex", width = 6, height = 4)
  print(p1)
  dev.off()
  
  # Now Michigan:
  
  GroupdataMich <- df2 %>%
    filter(!is.na(group) & !is.na(cenREGION) & !is.na(prop))
  # Merge group definitions with the main data
  GroupdataMich <- GroupdataMich %>%
    filter(prop<0.10)%>%
    left_join(group_definitions, by = "group")
  
  # Calculate broader categories
  GroupdataMich <- GroupdataMich %>%
    mutate(
      sex = case_when(
        group %in% group_indices_list$Men ~ "Men",
        group %in% group_indices_list$Women ~ "Women",
        TRUE ~ "sex unknown"
      ),
      age = case_when(
        group %in% group_indices_list$`Ages 18-24` ~ "Ages 18-24",
        group %in% group_indices_list$`Ages 25-34` ~ "Ages 25-34",
        group %in% group_indices_list$`Ages 35-49` ~ "Ages 35-49",
        group %in% group_indices_list$`Ages 50-64` ~ "Ages 50-64",
        group %in% group_indices_list$`Ages 65+` ~ "Ages 65+",
        TRUE ~ "age unknown"
      ),
      education = case_when(
        group %in% group_indices_list$`w/h.s.` ~ "w/h.s.",
        group %in% group_indices_list$`college +` ~ "college +",
        TRUE ~ "< h.s."
      ),
      broad_category = paste(sex, age, education, sep = "-")
    )
  
  # Step 1: Calculate the mean of `prop` across `survey_date`
  region_summary <- GroupdataMich %>%
    group_by(survey_date,cenREGION, broad_category) %>%
    summarize(total_prop = sum(prop, na.rm = TRUE), .groups = 'drop')
  
  region_avg <- region_summary %>%
    group_by(cenREGION, broad_category) %>%
    summarize(mean_prop = mean(total_prop, na.rm = TRUE), .groups = 'drop')
  
  region_avg <- region_avg %>%
    mutate(cenREGION = factor(cenREGION, levels = c(1, 2, 3, 4), labels = c("W", "MW", "NE", "S")))
  
  
  
  # Step 2: Create the faceted bar graph
  p2 <- ggplot(region_avg, aes(x = reorder(broad_category, mean_prop, sum), y = mean_prop, fill = factor(cenREGION))) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ cenREGION, scales = "fixed") +
    labs(x = "Group", y = "Mean Population Share", fill = "Census Region") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4), legend.position = "none")
  
  
  # Export the plot as a TikZ/PGFplots file
  tikz("figs/tex/michigan_distribution.tex", width = 6, height = 4)
  print(p2)
  dev.off()
  
  
}


# Function to plot Group Price Mean Expectations

plot_group_mean_expectations <- function(df){
  # df should be localdataBig
  
  mean_expectation_by_industry <- df %>%
    group_by( industry) %>%
    summarise(mean_inflation = mean(PX1, na.rm = TRUE))
  
  
  
  
  # Define lists for different demographic groups
  group_indices_list <- list(
    Men = c(1:80),
    Women = c(81:160),
    `Ages 18-24` = c(1:16, 81:96),
    `Ages 25-34` = c(17:32, 97:112),
    `Ages 35-49` = c(33:48, 113:128),
    `Ages 50-64` = c(49:64,129:144),
    `Ages 65+` = c(65:80, 145:160),
    `-h.s.` = c(1:4, 17:20,33:36,49:52,65:68,81:84,97:100,113:116,129:132,145:148),
    `w/h.s.` = c(5:8,21:24,37:40,53:56,69:72,85:88,101:104,117:120,133:136,149:152),
    `-c` = c(9:12,25:28,41:44,57:60,73:76,89:92,105:108,121:124,137:140, 153:156),
    `+c` = c(13:16,29:32,45:48,61:64,77:80,93:96,109:112,125:128,141:144,157:160),
    `marry` = c(1:2,5:6,9:10,13:14,17:18,21:22,25:26,29:30,33:34,37:38,41:42,45:46,49:50,53:54,57:58,61:62,65:66,69:70,73:74,77:78,81:82,85:86,89:90,93:94,97:98,101:102,105:106,109:110,113:114,117:118,121:122,125:126,129:130,133:134,137:138,141:142,145:146,149:150,153:154,157:158)
    
  )
  
  # Create a new column for age brackets
  mean_expectation_by_industry$age_bracket <- case_when(
    mean_expectation_by_industry$industry %in% c(1:16, 81:96) ~ "Ages 18-24",
    mean_expectation_by_industry$industry %in% c(17:32, 97:112) ~ "Ages 25-34",
    mean_expectation_by_industry$industry %in% c(33:48, 113:128) ~ "Ages 35-49",
    mean_expectation_by_industry$industry %in% c(49:64, 129:144) ~ "Ages 50-64",
    mean_expectation_by_industry$industry %in% c(65:80, 145:160) ~ "Ages 65+"
  )
  
  # Create a new column for age brackets
  mean_expectation_by_industry$educ_bracket <- case_when(
    mean_expectation_by_industry$industry %in% c(1:4, 17:20,33:36,49:52,65:68,81:84,97:100,113:116,129:132,145:148) ~ "- h.s.",
    mean_expectation_by_industry$industry %in% c(5:8,21:24,37:40,53:56,69:72,85:88,101:104,117:120,133:136,149:152) ~ "h.s.",
    mean_expectation_by_industry$industry %in% c(9:12,25:28,41:44,57:60,73:76,89:92,105:108,121:124,137:140, 153:156) ~ "- c",
    mean_expectation_by_industry$industry %in% c(13:16,29:32,45:48,61:64,77:80,93:96,109:112,125:128,141:144,157:160) ~ "+ c"
  )
  
  
  # Create a new column for 'kids' based on industry numbers being even
  mean_expectation_by_industry$kids <- ifelse(mean_expectation_by_industry$industry %% 2 == 0, "Kids", "No Kids")
  
  mean_expectation_by_industry_filtered <- mean_expectation_by_industry %>% 
    filter(!is.na(age_bracket) & !is.na(educ_bracket))
  
  # Create the plot
  p <- ggplot(mean_expectation_by_industry_filtered, aes(x = industry, y = mean_inflation, color = age_bracket, shape = educ_bracket)) +
    geom_point(aes(size = kids), stroke = 1.5) +
    scale_shape_manual(values = c(1, 2, 3, 4)) +
    scale_size_manual(values = c("Kids" = 3, "No Kids" = 2)) +
    labs(
      x = "group",
      y = "$p^e$",
      color = "Age",
      shape = "Educ",
      size = "Kids"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black")
    )
  
  p
  
  # Save as a TikZ diagram
  tikz("figs/tex/group_means.tex", standAlone = FALSE, width = 6, height = 6)
  print(p)
  dev.off()
  
  
}

# Function to load and transform panel data
load_panel_data <- function(file_path) {
  read_csv(file_path) %>%
    mutate(year = format(survey_date, "%Y"), yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d")) %>%
    panel(~REGION + survey_date)
}

# Runs 2sls and returns tables by stages





run_regression <- function(data, endo_vars, instr_vars) {
  # Define the terms to be included in the formula (excluding UNRATE from controls)
  TERMS <- c("l(RegInf,1:2)",  "FinBetterLast1",  "FinBetterNext1", "BizGood", "BizExGood", "gasExp")
  
  # Create the formula as a string for the main part
  MODEL_FORMULA_STR <- paste("RegInf ~", paste(TERMS, collapse = " + "), "|", "REGION + yearquarter")
  
  # Adjust here to include l(UNRATE, 12) as an instrument for UNRATE
  # Check if we have more than one endogenous variable and adjust accordingly
  if (length(endo_vars) > 1 || !is.null(endo_vars)) {
    endo_str <- paste(endo_vars, collapse = " + ")
    instr_str <- paste(instr_vars, collapse = " + ")
    # Adding UNRATE and its instrument
    endo_instr_str <- paste(endo_str, "+ UNRATE ~", instr_str, "+ l(UNRATE, 12)")
  } else {
    # If no endogenous variables specified, just instrument UNRATE
    endo_instr_str <- "UNRATE ~ l(UNRATE, 12)"
  }
  
  # Finalize the model formula with instrumented UNRATE
  MODEL_FORMULA_STR <- paste(MODEL_FORMULA_STR, "|", endo_instr_str)
  
  # Convert the string to a formula object
  MODEL_FORMULA <- as.formula(MODEL_FORMULA_STR)
  
  # Run the regression using feols
  est <- feols(MODEL_FORMULA, data = data)
  
  return(est)
}

run_regression2 <- function(data, endo_vars, instr_vars, fixed_effects) {
  library(fixest)
  library(broom)
  
  print(paste("Running regression with fixed effects:", fixed_effects))
  
  TERMS <- c("l(RegInf,1:2)", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "gasExp")
  
  if (fixed_effects == "") {
    MODEL_FORMULA_STR <- paste("RegInf ~", paste(TERMS, collapse = " + "))
  } else {
    MODEL_FORMULA_STR <- paste("RegInf ~", paste(TERMS, collapse = " + "), "|", fixed_effects)
  }
  
  if (length(endo_vars) > 0 && length(instr_vars) > 0) {
    endo_str <- paste(endo_vars, collapse = " + ")
    instr_str <- paste(instr_vars, collapse = " + ")
    endo_instr_str <- paste(endo_str, "+ UNRATE ~", instr_str, "+ l(UNRATE, 12)")
  } else {
    endo_instr_str <- "UNRATE ~ l(UNRATE, 12)"
  }
  
  MODEL_FORMULA_STR <- paste(MODEL_FORMULA_STR, "|", endo_instr_str)
  MODEL_FORMULA <- as.formula(MODEL_FORMULA_STR)
  
  est <- feols(MODEL_FORMULA, data = data)
  
  results <- tidy(est)
  
  return(est)
  
  #return(list(model = est, results = results))
}



# Table creation helper functions;

# Function to extract tabular content
extract_tabular <- function(latex_code) {
  start <- str_locate(latex_code, "\\\\begin\\{tabular\\}")[1, 1]
  end <- str_locate(latex_code, "\\\\end\\{tabular\\}")[1, 2]
  substr(latex_code, start, end)
}

extract_tabular_and_notes <- function(latex_code) {
  # Look for the start of the threeparttable or ThreePartTable environment
  # as it will include both the tabular content and the footnotes.
  # Adjust depending on what your LaTeX code uses (threeparttable is used here as an example).
  start <- str_locate(latex_code, "\\\\begin\\{threeparttable\\}")[1, 1]
  end <- str_locate(latex_code, "\\\\end\\{threeparttable\\}")[1, 2]

  # If the threeparttable environment is not used, fall back to the tabular environment.
  if (is.na(start) || is.na(end)) {
    start <- str_locate(latex_code, "\\\\begin\\{tabular\\}")[1, 1]
    end <- str_locate(latex_code, "\\\\end\\{tabular\\}")[1, 2]
  }

  # Extract the content.
  substr(latex_code, start, end)
}

# Extract tabular content from LaTeX output
# tabular_panel_a <- extract_tabular(panel_a_latex)
# tabular_panel_b <- extract_tabular_and_notes(panel_b_latex)




# After tabular...
combined_latex_func <- function(panel_a, panel_b, label, caption) {
  paste0(
    "\\begin{table}[!htbp] \\centering\n",
    "\\caption{", caption, "}\n",
    "\\label{tab:", label, "}\n",
    "\\textbf{Panel A: CPS shares} \\\\ \n",
    panel_a,
    " \\vspace{5mm} \n",
    "\\\\ \\textbf{Panel B: Michigan shares} \\\\ \n",
    panel_b,
    "\\end{table}\n"
  )
}




first_stage_table_five <- function(first_cps, first_bench, table_name, table_label, table_caption){

  stats_matrix <- matrix(NA, nrow = 3, ncol = 8,
                         dimnames = list(c("Cf.", "$R^2$", "F"),
                                         c("(1)-pe", "(1)-pe5", "(2)-pe", "(2)-pe5",
                                           "(3)-pe", "(3)-pe5", "(4)-pe", "(4)-pe5")))
  stats_matrix_bench <- matrix(NA, nrow = 3, ncol = 8,
                               dimnames = list(c("Cf.", "$R^2$", "F"),
                                               c("(1)-pe", "(1)-pe5", "(2)-pe", "(2)-pe5",
                                                 "(3)-pe", "(3)-pe5", "(4)-pe", "(4)-pe5")))

  # Function to format coefficients and standard errors
  format_coef_se <- function(coef, se) {
    paste0(round(coef, 2), " (", round(se, 2), ")")
  }
  # Format numeric values with three decimal places
  format_numeric_value <- function(value) {
    sprintf("%.2f", value)
  }

  for (i in 1:4) {


    # CPS
    model_pe <- first_cps[[i]]$pe
    model_pe5 <- first_cps[[i]]$pe5



    stats_matrix["Cf.", paste0("(", i, ")-pe")] <- format_coef_se(coef(summary(model_pe))["Bartik_out"], se(summary(model_pe))["Bartik_out"])
    stats_matrix["$R^2$", paste0("(", i, ")-pe")] <- format_numeric_value(r2(model_pe, type = "r2"))
    stats_matrix["F", paste0("(", i, ")-pe")] <- format_numeric_value(fitstat(model_pe, type = "ivf")$ivf$stat)

    stats_matrix["Cf.", paste0("(", i, ")-pe5")] <- format_coef_se(coef(summary(model_pe5))["Bartik5_out"], se(summary(model_pe5))["Bartik5_out"])
    stats_matrix["$R^2$", paste0("(", i, ")-pe5")] <- format_numeric_value(r2(model_pe5, type = "r2"))
    stats_matrix["F", paste0("(", i, ")-pe5")] <- format_numeric_value(fitstat(model_pe5, type = "ivf")$ivf$stat)

    # Michigan

    model_pe_bench <- first_bench[[i]]$pe
    model_pe5_bench <- first_bench[[i]]$pe5

    stats_matrix_bench["Cf.", paste0("(", i, ")-pe")] <- format_coef_se(coef(summary(model_pe_bench))["Bartik_bench_out"], se(summary(model_pe_bench))["Bartik_bench_out"])
    #stats_matrix_bench["Coef.", paste0("(", i, ")-pe")] <- coef(summary(model_pe_bench))["Bartik_bench_out"]
    stats_matrix_bench["$R^2$", paste0("(", i, ")-pe")] <- format_numeric_value(r2(model_pe_bench, type = "r2"))
    stats_matrix_bench["F", paste0("(", i, ")-pe")] <- format_numeric_value(fitstat(model_pe_bench, type = "ivf")$ivf$stat)

    stats_matrix_bench["Cf.", paste0("(", i, ")-pe5")] <- format_coef_se(coef(summary(model_pe5_bench))["Bartik5_bench_out"], se(summary(model_pe5_bench))["Bartik5_bench_out"])
    #stats_matrix_bench["Coef.", paste0("(", i, ")-pe5")] <- coef(summary(model_pe5_bench))["Bartik5_bench_out"]
    stats_matrix_bench["$R^2$", paste0("(", i, ")-pe5")] <- format_numeric_value(r2(model_pe5_bench, type = "r2"))
    stats_matrix_bench["F", paste0("(", i, ")-pe5")] <- format_numeric_value(fitstat(model_pe5_bench, type = "ivf")$ivf$stat)
  }
  # Convert the matrix to a data.frame for easier handling with kableExtra
  stats_df <- as.data.frame(stats_matrix, stringsAsFactors = FALSE)
  stats_df_bench <- as.data.frame(stats_matrix_bench, stringsAsFactors = FALSE)


  latex_table <- kable(stats_df, format = "latex", booktabs = TRUE, escape = FALSE, digits = 2,  align = 'lccccccc',
                       caption = "2sls: first stage") %>%
    kable_styling(latex_options = c("striped", "hold_position","repeat_header", "longtable = TRUE"), full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    as.character()



  latex_table_bench <- kable(stats_df_bench, format = "latex", booktabs = TRUE, escape = FALSE, digits = 2,  align = 'lccccccc',
                             caption = "2sls: first stage") %>%
    kable_styling(latex_options = c("striped", "hold_position", "repeat_header", "longtable = TRUE"), full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    as.character()

  panel_a_latex <- extract_tabular(latex_table)
  panel_b_latex <- extract_tabular(latex_table_bench)
  print(table_label)
  # After tabular...
  combined_latex_func_stage1 <- function(panel_a, panel_b, label, caption) {
    paste0(
      "\\begin{landscape}\n",
      "\\begin{table}[!htbp] \\centering\n",
      "\\caption{", caption, "}\n",
      "\\label{tab:", label, "}\n",
      "\\textbf{Panel A: CPS shares} \\\\ \n",
      panel_a,
      " \\vspace{5mm} \n",
      "\\\\ \\textbf{Panel B: Michigan shares} \\\\ \n",
      panel_b,
      "\\end{table}\n",
      "\\end{landscape}\n"
    )
  }

  # Write combined LaTeX to file
  writeLines(combined_latex_func_stage1(panel_a_latex, panel_b_latex, table_label, table_caption), table_name)

}

# 1st stage function with pe and pe5

first_stage_table <- function(first_cps, first_bench, table_name, table_label, table_caption){

  stats_matrix <- matrix(NA, nrow = 3, ncol = 8,
                         dimnames = list(c("Coef.", "$R^2$", "F (1st stage)"),
                                         c("(1)-pe", "(1)-U", "(2)-pe", "(2)-U",
                                           "(3)-pe", "(3)-U", "(4)-pe", "(4)-U")))
  stats_matrix_bench <- matrix(NA, nrow = 3, ncol = 8,
                               dimnames = list(c("Coef.", "$R^2$", "F (1st stage)"),
                                               c("(1)-pe", "(1)-U", "(2)-pe", "(2)-U",
                                                 "(3)-pe", "(3)-U", "(4)-pe", "(4)-U")))



  for (i in 1:4) {


    # CPS
    model_pe <- first_cps[[i]]$pe
    model_UNRATE <- first_bench[[i]]$UNRATE

    stats_matrix["Coef.", paste0("(", i, ")-pe")] <- coef(summary(model_pe))["Bartik_out"]
    stats_matrix["$R^2$", paste0("(", i, ")-pe")] <- r2(model_pe, type = "r2")
    stats_matrix["F (1st stage)", paste0("(", i, ")-pe")] <- fitstat(model_pe, type = "ivf")$ivf$stat

    stats_matrix["Coef.", paste0("(", i, ")-U")] <- coef(summary(model_UNRATE))["l(UNRATE, 12)"]
    stats_matrix["$R^2$", paste0("(", i, ")-U")] <- r2(model_UNRATE, type = "r2")
    stats_matrix["F (1st stage)", paste0("(", i, ")-U")] <- fitstat(model_UNRATE, type = "ivf")$ivf$stat

    # Michigan

    model_pe_bench <- first_bench[[i]]$pe
    model_UNRATE_bench <- first_bench[[i]]$UNRATE

    stats_matrix_bench["Coef.", paste0("(", i, ")-pe")] <- coef(summary(model_pe_bench))["Bartik_bench_out"]
    stats_matrix_bench["$R^2$", paste0("(", i, ")-pe")] <- r2(model_pe_bench, type = "r2")
    stats_matrix_bench["F (1st stage)", paste0("(", i, ")-pe")] <- fitstat(model_pe_bench, type = "ivf")$ivf$stat

    stats_matrix_bench["Coef.", paste0("(", i, ")-U")] <- coef(summary(model_UNRATE_bench))["l(UNRATE, 12)"]
    stats_matrix_bench["$R^2$", paste0("(", i, ")-U")] <- r2(model_UNRATE_bench, type = "r2")
    stats_matrix_bench["F (1st stage)", paste0("(", i, ")-U")] <- fitstat(model_UNRATE_bench, type = "ivf")$ivf$stat
  }
  # Convert the matrix to a data.frame for easier handling with kableExtra
  stats_df <- as.data.frame(stats_matrix, stringsAsFactors = FALSE)
  stats_df_bench <- as.data.frame(stats_matrix_bench, stringsAsFactors = FALSE)


  latex_table <- kable(stats_df, format = "latex", booktabs = TRUE, escape = FALSE, digits = 3,  align = 'lccccccc',
                       caption = "2sls: first stage") %>%
    kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    as.character()

  latex_table_bench <- kable(stats_df_bench, format = "latex", booktabs = TRUE, escape = FALSE, digits = 3,  align = 'lccccccc',
                             caption = "2sls: first stage") %>%
    kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    as.character()

  panel_a_latex <- extract_tabular(latex_table)
  panel_b_latex <- extract_tabular(latex_table_bench)
  print(table_label)
  
  combined_latex_func_stage1 <- function(panel_a, panel_b, label, caption) {
    paste0(
      "\\begin{table}[!htbp] \\centering\n",
      "\\caption{", caption, "}\n",
      "\\label{tab:", label, "}\n",
      "\\textbf{Panel A: CPS shares} \\\\ \n",
      panel_a,
      " \\vspace{5mm} \n",
      "\\\\ \\textbf{Panel B: Michigan shares} \\\\ \n",
      panel_b,
      "\\end{table}\n"
    )
  }
}




# 2nd stage function

second_stage_table <- function(models, models_bench, table_name, table_label, table_caption, general_footnote, number_footnote){

  f <- function(x) format(round(x, 3), big.mark=",")
  gm <- list(
    list("raw" = "nobs", "clean" = "N", "fmt" = f),
    list("raw" = "r.squared", "clean" = "R2", "fmt" = f),
    list("raw" = "r2.within", "clean" = "within R2", "fmt" = f),
    list("raw" = "FE: REGION", "clean" = "Region FE", "fmt" = f),
    list("raw" = "FE: yearquarter", "clean" = "Time FE", "fmt" = f),
    list("raw" = "vcov.type", "clean" = "SE", "fmt" = f))

  gm2 <- list(
    list("raw" = "nobs", "clean" = "N", "fmt" = f),
    list("raw" = "r.squared", "clean" = "R2", "fmt" = f),
    list("raw" = "r2.within", "clean" = "within R2", "fmt" = f))


  # Define a helper function to dynamically set coefficient names
  getCoefNames <- function(model_list) {
    # Assuming 'model_list' is a list of models
    # Start by assuming the default names
    rename_list <- c("fit_pe" = "$p^e$", "fit_UNRATE" = "$u^r$")

    # Check the first model for presence of specific coefficient names
    
    if ("fit_pe5" %in% names(coef(model_list[[1]]))) {
      rename_list["fit_pe5"] <- "$p5^e$"
      # Remove the default fit_pe if fit_pe5 exists
      rename_list <- rename_list[!names(rename_list) %in% c("fit_pe")]
    }

    return(rename_list)
  }

  # For Panel A
  coef_rename_list_a <- getCoefNames(models)
  panel_a_df <- modelsummary(
    models,
    estimate  = c("{estimate} ({std.error}){stars}",
                  "{estimate} ({std.error}){stars}","{estimate} ({std.error}){stars}","{estimate} ({std.error}){stars}"),
    statistic = NULL,
    coef_rename = coef_rename_list_a,
    coef_omit =  c(-1, -2),
    vcov = ~REGION,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = gm,  # Ensure 'gm' is defined elsewhere in your function
    output = "data.frame"
  )


  panel_a_df <- panel_a_df %>%
    select(-part, -statistic)
  names(panel_a_df)[names(panel_a_df) == "term"] <- ""


  # For Panel B
  coef_rename_list_b <- getCoefNames(models_bench)
  panel_b_df <- modelsummary(
    models_bench,
    estimate  = c("{estimate} ({std.error}){stars}",
                  "{estimate} ({std.error}){stars}","{estimate} ({std.error}){stars}","{estimate} ({std.error}){stars}"),
    statistic = NULL,
    coef_rename = coef_rename_list_b,
    coef_omit =  c(-1, -2),
    vcov = ~REGION,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = gm,  # Ensure 'gm' is defined elsewhere in your function
    output = "data.frame"
  )
  panel_b_df <- panel_b_df %>%
    select(-part, -statistic)
  names(panel_b_df)[names(panel_b_df) == "term"] <- ""

  # Function to add \hline after the row for $u^r$
  add_hline_after_ur <- function(latex_str) {
    # Split the input LaTeX string into lines
    lines <- str_split(latex_str, "\n")[[1]]
    # Find the line number containing the last occurrence of $u^r$
    ur_line <- max(grep("\\$u\\^r\\$", lines))
    # Add a line containing \hline after the $u^r$ line
    lines <- append(lines, "\\hline", ur_line)
    # Rejoin the lines into a single string
    paste(lines, collapse = "\n")
  }

  # Create LaTeX tables using kableExtra
  panel_a_latex <- panel_a_df %>%
    kable(format = "latex", booktabs = TRUE, escape = FALSE) %>%
    kable_styling(latex_options = c("striped", "hold_position"), full_width = F) %>%
    as.character()

  panel_b_latex <- panel_b_df %>%
    kable(format = "latex", booktabs = TRUE, escape = FALSE) %>%
    kable_styling(latex_options = c("striped", "hold_position"), full_width = F) %>%
    footnote(general = general_footnote,
             number = number_footnote, threeparttable = T) %>%
    as.character()

  # Add \hline after $u^r$ in both Panel A and Panel B LaTeX code
  panel_a_latex <- add_hline_after_ur(panel_a_latex)
  panel_b_latex <- add_hline_after_ur(panel_b_latex)

  # Write combined LaTeX to file
  writeLines(combined_latex_func_stage1(panel_a_latex, panel_b_latex, table_label, table_caption), table_name)

}

# Second stage function that handles both 1 and 5 year ahead expectations;

second_stage_table_five <- function(models, models_bench, table_name, table_label, table_caption, general_footnote, number_footnote){

  f <- function(x) format(round(x, 3), big.mark=",")
  gm <- list(
    list("raw" = "nobs", "clean" = "N", "fmt" = f),
    list("raw" = "r.squared", "clean" = "R2", "fmt" = f),
    list("raw" = "r2.within", "clean" = "within R2", "fmt" = f),
    list("raw" = "FE: REGION", "clean" = "Region FE", "fmt" = f),
    list("raw" = "FE: yearquarter", "clean" = "Time FE", "fmt" = f),
    list("raw" = "vcov.type", "clean" = "SE", "fmt" = f))


  getCoefNames <- function(model_list) {
    # Assuming 'model_list' is a list of models
    # Adjusted to handle an additional instrument
    rename_list <- c("fit_pe" = "$p^e$", "fit_pe5" = "$p5^e$", "fit_UNRATE" = "$u^r$")  # Include initial renaming for fit_pe5

    # Example for dynamic adjustments, similar logic could be applied if needed
    if (!"fit_pe5" %in% names(coef(model_list[[1]]))) {
      # Adjust the rename list if 'fit_pe5' isn't found
      rename_list["fit_pe5"] <- NULL  # Remove fit_pe5 if not present
    }

    return(rename_list)
  }

  coef_rename_list_a <- getCoefNames(models)
  panel_a_df <- modelsummary(
    models,
    estimate  = c("{estimate} ({std.error}){stars}",
                  "{estimate} ({std.error}){stars}","{estimate} ({std.error}){stars}","{estimate} ({std.error}){stars}"),
    statistic = NULL,
    coef_rename = coef_rename_list_a,
    coef_omit =  c(-1, -2, -3),
    vcov = ~REGION,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = gm,
    output = "data.frame"
  )

  panel_a_df <- panel_a_df %>%
    select(-part, -statistic)
  names(panel_a_df)[names(panel_a_df) == "term"] <- ""

  coef_rename_list_b <- getCoefNames(models_bench)
  panel_b_df <- modelsummary(
    models_bench,
    estimate  = c("{estimate} ({std.error}){stars}",
                  "{estimate} ({std.error}){stars}","{estimate} ({std.error}){stars}","{estimate} ({std.error}){stars}"),
    statistic = NULL,
    coef_rename = coef_rename_list_b,
    coef_omit =  c(-1, -2, -3),
    vcov = ~REGION,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = gm,
    output = "data.frame"
  )
  panel_b_df <- panel_b_df %>%
    select(-part, -statistic)
  names(panel_b_df)[names(panel_b_df) == "term"] <- ""

  createLatexTable <- function(df, footnote_general, footnote_number){
    latex_table <- df %>%
      kable(format = "latex", booktabs = TRUE, escape = FALSE) %>%
      kable_styling(latex_options = c("striped", "hold_position"), full_width = F) %>%
      footnote(general = footnote_general, number = footnote_number, threeparttable = T) %>%
      as.character()

    return(extract_tabular(latex_table))
  }

  panel_a_latex <- createLatexTable(panel_a_df, general_footnote, number_footnote)
  panel_b_latex <- createLatexTable(panel_b_df, general_footnote, number_footnote)

  combined_latex_func <- function(panel_a, panel_b, label, caption) {
    paste0(
      "\\begin{table}[!htbp] \\centering\n",
      "\\caption{", caption, "}\n",
      "\\label{tab:", label, "}\n",
      "\\textbf{Panel A: CPS shares} \\\\ \n",
      panel_a,
      " \\vspace{5mm} \n",
      "\\\\ \\textbf{Panel B: Michigan shares} \\\\ \n",
      panel_b,
      "\\end{table}\n"
    )
  }

  # Write combined LaTeX to file
  writeLines(combined_latex_func(panel_a_latex, panel_b_latex, table_label, table_caption), table_name)

}

create_latex_table <- function(data, label, caption, file_path,
                               col_names = NULL, linesep = "\\addlinespace",
                               general_footnote = NULL, number_footnote = NULL,
                               multi_instruments = FALSE) {

  if (multi_instruments) {
    # Additional formatting for multiple instruments
    kbl(data, "latex", booktabs = T, caption = caption, col.names = col_names, linesep = linesep, label = label) %>%
      kable_styling(latex_options = c("striped", "scale_down")) %>%
      footnote(general = general_footnote, number = number_footnote, threeparttable = T) %>%
      save_kable(file_path)
  } else {
    # Existing formatting
    kbl(data, "latex", booktabs = T, caption = caption, col.names = col_names, linesep = linesep, label = label) %>%
      kable_styling(latex_options = "striped") %>%
      footnote(general = general_footnote, number = number_footnote, threeparttable = T) %>%
      save_kable(file_path)
  }
}


# # Function to create and save plots


# Function to fit the model and get coefficients
fit_model <- function(data, formula) {
  model <- feols(formula, data = data)
  coef <- coefficients(model)
  return(list(model = model, coef = coef))
}

# Function to create and save the plot
create_and_save_plot <- function(data, x_var, y_var, labels, filename, coef) {
  if (length(coef) < 2) {
    stop("Coefficient vector 'coef' does not contain at least two elements.")
  }
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_abline(slope = coef[2], intercept = coef[1], col = rgb(.541, .2, .141), size = 1.2) +
    labs(y = labels$y, x = labels$x) +
    theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 14), legend.position = "none")

  #ggsave(filename, p)
  tikz(file = filename, height = 3, width = 3)
  print(p)
  dev.off()
}



calculate_bias_correction <- function(panel_data, bartik_variable) {
  panel_data_2 <- panel_data %>%
    mutate(RegSample = case_when(REGION == 1 | REGION == 2 ~ 1, REGION == 3 | REGION == 4 ~ 0),
           TimeSample = case_when(survey_date < as.Date("2000-02-01") ~ 1, survey_date >= as.Date("2000-02-01") ~ 2))

  bias_formula <- as.formula(paste("RegInf ~", paste(VARIABLES, collapse = " + "), "| REGION + yearquarter | pe ~", bartik_variable))
  out_bias1 <- feols(bias_formula, panel_data_2, fsplit = ~TimeSample, vcov = "DK")
  out_bias2 <- feols(bias_formula, panel_data_2, fsplit = ~RegSample, vcov = "DK")
  c_out1 <- coefficients(out_bias1)
  c_out2 <- coefficients(out_bias2)
  s <- se(feols(bias_formula, panel_data, vcov = "DK"))

  adj_out <- 2 * c_out1[1] - (c_out2[2] + c_out2[3]) / 2
  return(list(adj_out = adj_out, se = s[1]))
}



calculate_bias_correction2 <- function(panel, bartik_var) {

  if (!bartik_var %in% colnames(panel)) {
    stop("The specified bartik_var does not exist in the panel data frame.")
  }

  panel2 <- panel %>%
    mutate(RegSample = case_when(REGION == 1 | REGION == 2 ~ 1, REGION == 3 | REGION == 4 ~ 0),
           TimeSample = case_when(survey_date < as.Date("2000-02-01") ~ 1, survey_date >= as.Date("2000-02-01") ~ 2))

  panel2 <- panel2 %>% panel(~REGION + survey_date)

  # Update the regression and IV formula
  formula <- as.formula(paste("RegInf ~ l(RegInf, 1:2) + FinBetterLast1 + RincUp + FinBetterNext1 + BizGood + BizExGood + gasExp | REGION + yearquarter | pe + UNRATE ~", bartik_var, "+ l(UNRATE, 12)"))

  # Ensure bartik_var column exists in panel2
  if (!bartik_var %in% colnames(panel2)) {
    stop("The specified bartik_var does not exist in the panel2 data frame.")
  }

  bias1 <- feols(formula, panel2, fsplit = ~TimeSample, vcov = "DK")
  bias2 <- feols(formula, panel2, fsplit = ~RegSample, vcov = "DK")

  c1 <- coefficients(bias1)
  c2 <- coefficients(bias2)

  se_out <- se(feols(formula, panel, vcov = "DK"))

  c1_fit_pe <- c1["fit_pe"]
  c2_fit_pe <- c2["fit_pe"]

  adj_out <- 2 * c1_fit_pe[1,] - (c2_fit_pe[2,] + c2_fit_pe[3,]) / 2

  return(list(adj_out = adj_out, se = se_out[1]))
}


# calculate_bias_correction2 <- function(panel, bartik_var) {
#   panel2 <- panel %>%
#     mutate(RegSample = case_when(REGION == 1 | REGION == 2 ~ 1, REGION == 3 | REGION == 4 ~ 0),
#            TimeSample = case_when(survey_date < as.Date("2000-02-01") ~ 1, survey_date >= as.Date("2000-02-01") ~ 2))
#
#   print(colnames(panel2))
#
#   panel2 <- panel2 %>% panel(~REGION + survey_date)
#
#   bias1 <- feols(RegInf ~ l(RegInf, 1:2) + FinBetterLast1 + RincUp + FinBetterNext1 + BizGood + BizExGood + gasExp | REGION + yearquarter | pe + UNRATE ~ bartik_var + l(UNRATE, 12), panel2, fsplit = ~TimeSample, vcov = "DK")
#   bias2 <- feols(RegInf ~ l(RegInf, 1:2)  + FinBetterLast1 + RincUp + FinBetterNext1 + BizGood + BizExGood + gasExp | REGION + yearquarter | pe + UNRATE ~ bartik_var + l(UNRATE,12), panel2, fsplit = ~RegSample, vcov = "DK")
#   c1 <- coefficients(bias1)
#   c2 <- coefficients(bias2)
#   print(c1)
#   print(c2)
#   se_out <- se(feols(RegInf ~ l(RegInf, 1:2) + UNRATE + UNEMPGood + FinBetterLast1 + RincUp + FinBetterNext1 + BizGood + BizExGood + gasExp | REGION + yearquarter | pe +UNRATE ~ bartik_var + l(UNRATE,12), panel, vcov = "DK"))
#   adj_out <- 2 * c1[1] - (c2[2] + c2[3]) / 2
#
#   return(list(adj_out = adj_out, se = se_out[1]))
# }

load_master_data <- function(filepath) {
  master_data <- read_csv(filepath) %>%
    filter(!is.na(REGION)) %>%
    filter(!is.na(pe)) %>%
    filter(!is.na(RegInf))
  return(master_data)
}

merge_fix_data <- function(master_data, local_data, global_data) {
  fixd <- merge(master_data, local_data) %>% drop_na()
  fixl <- fixd %>% select(-c("pe","RegInf","survey_date", "REGION")) %>% drop_na() %>% as_tibble()
  fixm <- fixd %>% select(c("survey_date", "pe", "REGION", "RegInf"))
  fixg <- global_data

  return(list(fixd = fixd, fixl = fixl, fixm = fixm, fixg = fixg))
}

calculate_weights <- function(fix, y, x, controls, weight, fixl, Z, fixg, G) {
  print(c(class(fix), class(y), class(x), class(controls), class(weight), class(fixl), class(Z), class(fixg), class(G)))
  print(head(fixl))
  Bw_out = bw(fix, y, x, controls, weight, fixl, Z, fixg, G)
  return(Bw_out)
}

aggregate_weights <- function(Bw_out) {
  Bwagg_out <- Bw_out %>%
    filter(!is.na(beta))%>%
    group_by(industry) %>%
    summarise(AggAlpha = sum(alpha), AggBeta = sum(alpha*beta)/AggAlpha)

  return(Bwagg_out)
}

aggregate_weights_cps <- function(Bw_out) {
  Bwagg_out <- Bw_out %>%
    filter(!is.na(beta))%>%
    group_by(group) %>%
    summarise(AggAlpha = sum(alpha), AggBeta = sum(alpha*beta)/AggAlpha)

  return(Bwagg_out)
}

# Function to transform panel data
transform_panel_data <- function(panel_data, bartik_col_name) {
  transformed_panel <- as_tibble(panel_data) %>%
    select(-c(Bartik)) %>%
    mutate(Bartik = bartik_col_name)
  return(transformed_panel)
}

# Function to create fixmc and fixmc_cps
create_fixmc <- function(panel_data_transformed) {


  fixmc <- panel_data_transformed %>%
    filter(!is.na(RegInf) & !is.na(REGION)) %>%
    select(-c(Bartik, Bartik_bench_out, cenREGION,  AggUNRATE, pe5, ye, UNRATE, gasExp, RincUp, BizGood, BizExGood, FinBetterNext1, FinBetterLast1, UNEMPGood))
  return(fixmc)
}
create_fixmc_cps <- function(panel_data_transformed) {
  panel_data_transformed <- panel_data_transformed %>%
    mutate(year = lubridate::year(survey_date),yearquarter = as.yearqtr(survey_date, format = "%Y-%m-%d"))

  fixmc <- panel_data_transformed %>%
    filter(!is.na(RegInf) & !is.na(REGION)) %>%
    select(-c(Bartik, Bartik_out,  cenREGION, year, AggUNRATE, yearquarter, pe5, ye, UNRATE, gasExp, RincUp, BizGood, BizExGood, FinBetterNext1, FinBetterLast1, UNEMPGood))
  return(fixmc)
}


# Function to generate LaTeX table
generate_weights_table <- function(df, filename,label, caption) {
  kbl(df, "latex", booktabs = T, caption = caption, label = label, linesep = "\\addlinespace", escape = FALSE) %>%
    kable_styling(latex_options = c("striped")) %>%
    footnote(general = "Group labels ordered: sex, age, educ., marital, children. Top-10 weighted groups according to the ``Rotemberg'' weights as in Goldsmith-Pinkham, et al (2020).",
             threeparttable = T) %>%
    save_kable(filename)
}

# Function to generate figure
generate_figure <- function(df, filename, bnot_out) {
  pWout <- ggplot(data = df, aes(x=Fstats, y=AggBeta, size = AggAlpha, fill = industry, shape = AggAlpha<0)) +
    geom_point(show.legend = FALSE) +
    geom_point(data=Bwagg2_out, aes(x=Fstats, y=AggBeta,  fill = industry), shape = 21, size = 36, alpha = 0.5, colour = "black" ) +
    scale_fill_viridis(discrete=FALSE, guide="none", option="A") +
    geom_hline(yintercept=bnot_out, linetype = "dashed", colour = "red") +
    scale_y_continuous(breaks = c(-2, 0, bnot_out, 2, 4), labels = c("-2", "0", TeX("$\\hat{\\beta}$"), "2", "4" )) +
    xlab(TeX("$F_g$"))+ylab(TeX("$\\beta_{g}$")) +
    theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 24, colour = rgb(.26, .17, .16)), legend.position = "none")

  ggsave(filename, pWout)
}

calculate_f_statistics <- function(region, fixg, panel) {
  Fk <- matrix(0, 160, 1)  # Initialize matrix to store F-stats
  for (i in 1:160) {
    data <- filter(region, industry == i)

    if (dim(data)[1] > 10) {
      data2 <- filter(fixg, industry == i)
      data3 <- panel %>% select(-c("pe", "RegInf", "Bartik", "Bartik_bench_out", "cenREGION"))
      data <- merge(data, data2) %>% merge(data3) %>% mutate(inst = prop * Agg_pe_ind)
      est <- feols(pe ~ inst + UNRATE + FinBetterLast1 + AggUNRATE + RincUp + BizGood + BizExGood + FinBetterNext1 + UNEMPGood, data)
      Fk[i] <- fitstat(est, ~f.stat)[[1]]
    } else {
      Fk[i] <- -1000  # Assign a placeholder value for insufficient data
    }
  }
  return(Fk)
}

# Group numbers to demographic characteristics
reverse_lookup <- function(industry_number, reference_data) {
  filtered_row <- subset(reference_data, Industry == industry_number)
  return(filtered_row)
}
# Function to add group descriptions based on industry numbers
add_group_descriptions <- function(df, reference_data, is_cps = FALSE) {
  # Initialize an empty vector to store group descriptions
  group_descriptions <- vector("character", nrow(df))
  print(head(df))
  # Loop over each row to populate the group description
  for (i in 1:nrow(df)) {
    industry_number <- if (is_cps) {df$group[i]} else { df$industry[i]}
    group_info <- reverse_lookup(industry_number, reference_data)

    # Convert the row to a description string
    group_desc <- paste(group_info$SEX, group_info$AGE, group_info$EDUC, group_info$MARRY, group_info$NUMKID, sep = ",")

    # Add the description to the group_descriptions vector
    group_descriptions[i] <- group_desc
  }

  # Add the group_descriptions as a new column in df
  df$group <- group_descriptions

  return(df)
}

# Create a function to transform a single group
transform_group <- function(group) {
  # Split the group string into individual codes
  codes <- strsplit(group, ",")[[1]]

  # Map each code to its new label
  new_codes <- c(
    mapping_list$sex[codes[1]],
    mapping_list$age[codes[2]],
    mapping_list$educ[codes[3]],
    mapping_list$marry[codes[4]],
    mapping_list$numkid[codes[5]]
  )

  # Join the new labels back into a single string
  new_group <- paste(new_codes, collapse = ",")

  return(new_group)
}

# Probing identification

markup_share_relationship <- function(groups_of_interest, df_Bartik) {

  # Define Census State IDs, state abbreviations, and FIPS codes
  census_state_ids <- tibble(
    state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida",
              "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
              "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
              "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
              "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas",
              "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
    STATEFIP = as.double(c("01", "02", "04", "05", "06", "08", "09", "10", "12",
                           "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
                           "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
                           "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
                           "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")),
    abbrev = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL",
               "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
               "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
               "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
               "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  )

  # Nakamura-Steinsson state-level data for 1/labor_share.
  NS <- read_dta("data/fiscal_stimulus_coded.dta")
  NSuse <- NS %>%
    select(c(year, state,earnings_work_SA04, earnings_residence_SA04, agg,Total_GSP, state_output_SA04))%>%
    mutate(markup = state_output_SA04/earnings_work_SA04)

  # 1978
  NS78 <- NSuse %>%
    filter(year == 1978)

  NS78 <- left_join(NS78, census_state_ids, by = c("state" = "abbrev"))

  NS78 <- NS78 %>%
    drop_na()%>%
    select(c(markup, STATEFIP))


  short_Bartik <- filter(df_Bartik, survey_date == "1978-01-01")


  Bart_use <- short_Bartik %>%
    group_by(STATEFIP) %>%
    select(c(prop, STATEFIP, group))



  combi_mark <- merge(Bart_use, NS78)
  combi_filtered <- combi_mark[combi_mark$group %in% groups_of_interest, ]
  combi_filtered <- combi_filtered %>%
    drop_na()


  # Estimate panel regression of markup on shares
  fixed_effects_model <- plm(markup ~ prop , data = combi_filtered, index = c("group"), model = "within")
  summary(fixed_effects_model)
  tidy_model <- tidy(fixed_effects_model)
  r2_fe <- summary(fixed_effects_model)$r.squared
  print(r2_fe)
  kbl(tidy_model, "latex", booktabs = T, caption = "Markups and groups", label = "table_markups",  linesep = "\\addlinespace") %>%
    kable_styling(latex_options = "striped") %>%
    footnote(general = "Dep. variable = 1978 markups by state. Indep. variable = top 10 group shares by state.", symbol = sprintf("The $R^2$ value is %.6f.", r2_fe[1]) , number = c("Signif. codes: * = .05; ** = .01; *** = .001."), threeparttable = T, escape = FALSE) %>%
    save_kable("tables/table_markups.tex")



  # Get top-10 groups, and filter to those with observations in many states.
  top_group_state_counts <- combi_filtered %>%
    group_by(group) %>%
    summarise(n_states = n_distinct(STATEFIP))

  top_groups_many_states <- top_group_state_counts %>% filter(n_states>=30) %>% pull(group)

  combi_top_many_states <- combi_filtered %>%
    filter(group %in% top_groups_many_states) %>% drop_na()

  # Print Markup Across States and Demographic Shares
  pmu <- ggplot(combi_top_many_states, aes(x=prop, y=markup)) +
    geom_point(aes(color=STATEFIP)) +
    geom_smooth(method="lm", se=TRUE, level = .95, color="black", aes(group=1), fill = "grey80") +
    facet_wrap(~ group) +
    #labs(title="Markup Across States and Demographic Groups") +
    theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 12, colour = rgb(.26, .17, .16)), legend.position = "none")
  ggsave('figs/markups.pdf', pmu)

}

share_covariates_relationship <- function (top_groups, fred, Bartik_df){



  fetch_fred_data <- function(series_id) {
    response <- GET(url = paste0("https://api.stlouisfed.org/fred/series/observations?series_id=", series_id,
                                 "&api_key=", fred_api_key, "&file_type=json"))
    content <- content(response, "text", encoding = "UTF-8")
    json <- fromJSON(content, flatten = TRUE)
    tibble(date = as.Date(json$observations$date), value = as.numeric(json$observations$value), series = series_id)
  }

  # Define Census State IDs, state abbreviations, and FIPS codes
  census_state_ids <- tibble(
    state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida",
              "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
              "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
              "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
              "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas",
              "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
    STATEFIP = as.double(c("01", "02", "04", "05", "06", "08", "09", "10", "12",
                           "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
                           "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
                           "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
                           "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")),
    abbrev = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL",
               "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
               "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
               "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
               "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  )

  # Fetch data for each state and each variable
  data <- tibble()
  for (i in 1:nrow(census_state_ids)) {
    for (var in c("PCPI", "UR", "STHPI", "NQGSP", "WTOT")) {
      series_id <- paste0(census_state_ids$abbrev[i], var)
      data_state_var <- fetch_fred_data(series_id) %>% select(-c("series"))
      data_state_var$state <- census_state_ids$state[i]
      data_state_var$STATEFIP <- census_state_ids$STATEFIP[i]
      data_state_var$var <- var
      data <- rbind(data, data_state_var)
    }
  }



  # Pivot the data to have one column per variable
  data <- data %>%
    pivot_wider(names_from = var, values_from = value)



  short_data <- filter(data, date == "1978-01-01") %>% mutate(survey_date = date) %>%
    select(-c("date","state"))

  short_Bartik <- filter(Bartik_df, survey_date == "1978-01-01")

  combi <- merge(short_data,short_Bartik)

  top_groups <- c(30,22,126,111,98, 102, 21, 47, 31,95)

  reg1 <- lm(prop ~ PCPI + UR + STHPI, data = combi)
  reg2 <- lm(prop ~ PCPI + UR + STHPI, data = filter(combi, group %in% top_groups))
  reg3 <- lm(Bartik_cps ~ PCPI + UR + STHPI, data = combi)
  reg4 <- lm(Bartik_cps ~ PCPI + UR + STHPI, data = filter(combi, group %in% top_groups))


  tidy1 <- tidy(reg1)
  tidy2 <- tidy(reg2)
  tidy3 <- tidy(reg3)
  tidy4 <- tidy(reg4)

  # Combine coefficients
  coefs <- bind_rows(tidy1, tidy2, tidy3, tidy4, .id = "model")

  # Use broom to glance models
  glance1 <- glance(reg1)
  glance2 <- glance(reg2)
  glance3 <- glance(reg3)
  glance4 <- glance(reg4)


  # Combine R^2
  r2s <- bind_rows(glance1, glance2, glance3, glance4, .id = "model") %>%
    select(model, r.squared)

  # List of linear models
  lms = list(reg1 = reg1, reg2 = reg2, reg3 = reg3, reg4 = reg4)

  # Combine tidy data and glance data
  coefs <- lapply(names(lms), function(name) {
    df <- broom::tidy(lms[[name]])
    df$model <- name
    return(df)
  })

  # Combine them into a single data frame
  coefs_combined <- do.call(dplyr::bind_rows, coefs)
  coefs_combined$term[coefs_combined$term == "(Intercept)"] <- NA
  coefs_combined <- coefs_combined %>%
    dplyr::filter(!is.na(term)) %>%
    dplyr::mutate(info = paste0(sprintf("%.6f", estimate), " (", sprintf("%.4f", std.error), ")"))

  # Pivot to wide format
  coefs_wide <- coefs_combined %>%
    dplyr::select(term, model, info) %>%
    tidyr::pivot_wider(names_from = model, values_from = info)

  # Add R^2
  glances <- lapply(names(lms), function(name) {
    df <- broom::glance(lms[[name]])
    df$model <- name
    df$term <- "$R^2$"  # Using LaTeX math mode
    df$info <- sprintf("%.6f", df$r.squared)
    return(df[, c("term", "model", "info")])
  })
  glance_data <- do.call(dplyr::bind_rows, glances)
  glance_wide <- glance_data %>%
    tidyr::pivot_wider(names_from = model, values_from = info)

  final_table <- dplyr::bind_rows(coefs_wide, glance_wide)

  #Create LaTeX table
  final_table %>%
    kable("latex", booktabs = TRUE, col.names = c(" ", "All shares", "Top-10 shares", "All Bartik", "Top-10 Bartik"), caption = "State level shares and characteristics", label = "table:identCheck", escape = FALSE) %>%
    kable_styling(latex_options = c("striped", "hold_position")) %>%
    footnote(general = "Each column represents OLS regression output from regressing subsets of 1978 shares, or Bartik instrument, on producer price index (PCPI), the unemployment rate (UR), personal income (STHPI).  Standard errors are in parentheses.", threeparttable = TRUE) %>%
    save_kable("tables/testIdent.tex")

  return(final_table)
}

