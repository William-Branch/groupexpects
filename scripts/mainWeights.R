#mainWeights.R
library(dotenv)
load_dot_env(".env")
fred_api_key <- Sys.getenv("FRED_API_KEY")
fredr_set_key(fred_api_key)
source('scripts/common_libraries.R')
source('functions/functions_results.R')
source('functions/functions_data.R')

master_data <- load_master_data("data/dataBuild/masterdataSmallNo5.csv")
local_data <- read_csv("data/dataBuild/localdataSmall.csv") %>% filter(!is.na(REGION))
local_data_cps <- read_csv("data/dataBuild/localdataCPS.csv")
local_data_cps <- local_data_cps %>% mutate(REGION = cenREGION) %>% filter(!is.na(REGION))
global_data <- read_csv("data/dataBuild/globaldataSmallNo5.csv")
global_data_cps <- read_csv("data/dataBuild/globaldataCPS.csv")
panel_bench_out <- read_csv("data/dataBuild/panelbenchoutNo5.csv")
panel_out <- read_csv("data/dataBuild/panelcpsoutNo5.csv")
region_data2 <- read_csv("data/dataBuild/regiondata.csv")
region_CPS78 <- read_csv("data/dataBuild/regiondataout.csv")
cps78 <- read_csv("data/cps_00003.csv")
data_globalCPS <- read_csv("data/dataBuild/globaldata.csv")

#outputs fixd, fixl, fixm,fixg
merged_data <- merge_fix_data(master_data, local_data, global_data)
merged_data_cps <- merge_fix_data(master_data, local_data_cps, global_data_cps)

# Transform panel data
panel_fixT <- transform_panel_data(panel_bench_out, "Bartik_bench_out")
panel_fixTcps <- transform_panel_data(panel_out, "Bartik_out")

# Create fixmc and fixmc_cps

fixmc <- create_fixmc(panel_fixT)
fixmc_cps <- create_fixmc_cps(panel_fixTcps)


index = c("REGION", "survey_date")
index_cps = c("cenREGION", "survey_date")
y = "RegInf"
x = "pe"
Z = setdiff(names(merged_data$fixl), c(index))
Z_cps = setdiff(names(merged_data_cps$fixl), c(index_cps))
G = "Agg_pe_ind"
controls = NULL
weight = NULL
controls = setdiff(names(fixmc), c(index, y, x, weight))

Bw_out <- calculate_weights(fixmc, y, x, controls, weight, merged_data$fixl, Z, merged_data$fixg, G)
Bw_cps <- calculate_weights(fixmc_cps, y, x, controls, weight, merged_data_cps$fixl, Z_cps, merged_data_cps$fixg, G)

Bwagg_out <- aggregate_weights(Bw_out)
Bwagg_cps <- aggregate_weights_cps(Bw_cps)

Bwagg2_out <- Bwagg_out %>%
  top_n(11, AggAlpha) %>%
  arrange(desc(AggAlpha))%>%
  slice(-1)
  
Bwagg2_cps <- Bwagg_cps %>%
  top_n(11, AggAlpha) %>%
  arrange(desc(AggAlpha)) %>%
  slice(-1)

top10_groups <- Bwagg2_cps$group

# Define the sets for each demographic variable
sex_set <- c(1, 2)
age_set <- c('18:24', '25:34', '35:49', '50:64', '65+')
educ_set <- c('1:2', '3','4','5:6')
marry_set <- c('1:4','5')
numkid_set <- c(0,1)  # Assuming no kids for now, can be expanded

# Create all combinations (Cartesian product) of the sets
expanded_reference_data <- expand.grid(
  SEX = sex_set,
  AGE = age_set,
  EDUC = educ_set,
  MARRY = marry_set,
  NUMKID = numkid_set
)
# Create a list with mappings
mapping_list <- list(
  sex = list('1' = 'Ml', '2' = 'Fl'),
  age = list('18:24' = '18-24', '25:34' = '25-34', '35:49' = '35-49', '50:64' = '50-64', '65+' = '65+'),
  educ = list('1:2' = 's.h.s.', '3' = 'h.s.', '4' = 's.c.', '5:6' = 'c+'),
  marry = list('1:4' = 'M', '5' = 'NM'),
  numkid = list('0' = 'NK', '1' = 'K')
)

# Sort the DataFrame for readability
expanded_reference_data <- expanded_reference_data[order(expanded_reference_data$SEX, expanded_reference_data$AGE, expanded_reference_data$EDUC, expanded_reference_data$MARRY, expanded_reference_data$NUMKID), ]


expanded_reference_data$Industry <- seq_len(nrow(expanded_reference_data))
Bwagg2_out <- add_group_descriptions(Bwagg2_out, expanded_reference_data, is_cps = FALSE)
Bwagg2_cps <- add_group_descriptions(Bwagg2_cps, expanded_reference_data, is_cps = TRUE)

# Prepare the tables
printAgg_out <- Bwagg2_out %>% select(c(group, AggAlpha, AggBeta)) %>% mutate(AggAlpha = round(AggAlpha, 4), AggBeta = round(AggBeta, 4))
printAgg_cps <- Bwagg2_cps %>% select(c(group, AggAlpha, AggBeta)) %>% mutate(AggAlpha = round(AggAlpha, 4), AggBeta = round(AggBeta, 4))
printAgg_out <- printAgg_out %>%
  rowwise() %>% 
  mutate(new_group = list(transform_group(group))) %>% 
  ungroup() %>%
  rename(`group id` = new_group) %>%
  select(`group id`, everything(), -group)

printAgg_cps <- printAgg_cps %>%
  rowwise() %>% 
  mutate(new_group = list(transform_group(group))) %>% 
  ungroup() %>%
  rename(`group id` = new_group) %>%
  select(`group id`, everything(), -group)


#Generate f-stats
Fk <- matrix(0,160,1)
fixg <- merged_data$fixg

for (i in 1:160){
  data  <-  filter(region_data2, group == i);
  
  if (dim(data)[1]>10){
    data2 <- filter(fixg, industry == i);
    data3 <- panel_fixT %>% select(-c( "RegInf", "Bartik", "Bartik_bench_out", "cenREGION"));
    data <- merge(data,data2) %>% merge(data3) %>% mutate(inst = prop*Agg_pe_ind);
    est <- feols(pe ~ inst + UNRATE + FinBetterLast1 + AggUNRATE+ RincUp + BizGood + BizExGood + FinBetterNext1 + UNEMPGood   , data)
    
    Fk[i] <- fitstat(est, ~f.stat)[[1]]
  } else {-1000}
  
}

panel_yq <- preprocess_panel_data(panel_bench_out)

estF_out <- feols(RegInf ~  UNRATE + FinBetterLast1 + RincUp + BizGood + BizExGood + FinBetterNext1 + UNEMPGood | REGION + yearquarter |  pe ~ Bartik_bench_out   , panel_yq)

bnot_out <- coefficients(estF_out)[[1]]

Fk <- c(Fk, NA)
BwaggSmall_out <- Bwagg_out %>% mutate(Fstats = Fk) 
BwaggSmall_out <- BwaggSmall_out %>% filter(AggBeta>-5, AggBeta<5, abs(Fstats)>5, abs(Fstats) < 100)

BwaggSmall_2 <- BwaggSmall_out %>%
  top_n(10, AggAlpha) %>%
  arrange(desc(AggAlpha)) 

# Generate tables
generate_weights_table(printAgg_out, "tables/weightsout.tex","rotweights:michigan" , "Top 10 weighted groups: Michigan shares")
generate_weights_table(printAgg_cps, "tables/weightsCPS.tex", "rotweights:cps", "Top 10 weighted groups: CPS shares")

# Generate figures

BwaggTime <- Bw_out %>%
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
        axis.line = element_line(colour = "grey"),axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
print(pT)
dev.off()


pWout <- ggplot(data = BwaggSmall_out, aes(x=Fstats, y=AggBeta, size = AggAlpha, fill = industry, shape = AggAlpha<0)) + geom_point(show.legend = FALSE) +
  geom_point(data=BwaggSmall_2, aes(x=Fstats, y=AggBeta,  fill = industry), shape =  21, size = 36, alpha = 0.5, colour = "black" ) + 
  scale_fill_viridis(discrete=FALSE, guide="none", option="A") +
  geom_hline(yintercept=bnot_out, linetype = "dashed", colour = "red") + 
  scale_y_continuous(breaks = c(-2, 0, bnot_out, 2, 4), labels = c("-2", "0", TeX("$\\hat{\\beta}$"), "2", "4" )) +
  xlab(TeX("$F_g$"))+ylab(TeX("$\\beta_{g}$")) +
  #ggrepel::geom_label_repel(aes(label = group), data = Bwagg2, box.padding = 4, fill = "white", size = 10) + 
  theme(panel.border = element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text = element_text(size = 14,colour = rgb(.26, .17, .16)), legend.position = "none")  
ggsave("figs/weightsout.pdf", pWout)

# Aggregated groups;

# Main function to generate LaTeX table
create_custom_latex_table <- function(Bwagg_out, Bwagg_cps) {
  
  # Define lists for different demographic groups
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
  
  # Sum weights for each group in one pass
  total_weight_out <- sum(Bwagg_out$AggAlpha)
  total_weight_cps <- sum(Bwagg_cps$AggAlpha)
  
  table_data <- lapply(names(group_indices_list), function(group_name) {
    cps_weight <- sum(Bwagg_cps[Bwagg_cps$group %in% group_indices_list[[group_name]], "AggAlpha"]) / total_weight_cps
    out_weight <- sum(Bwagg_out[Bwagg_out$industry %in% group_indices_list[[group_name]], "AggAlpha"]) / total_weight_out
    
    data.frame(
      shares = group_name,
      CPS = round(cps_weight, 3),
      Michigan = round(out_weight, 3)
    )
  }) %>% bind_rows()
  
  # Transpose the data for rotated table
  table_data_transposed <- as.data.frame(t(table_data[-1,]))
  colnames(table_data_transposed) <- table_data$Category
  
  # Generate LaTeX table
  # kbl(table_data_transposed, "latex", caption = "Rotemberg weights for broad demographic groups.", label = "rotweights:groups", booktabs = T, col.names = names(table_data_transposed)) %>%
  #   #kable_styling(latex_options = c("striped", "scale_down", "HOLD_position", "full_width = TRUE")) %>%
  #   kable_styling(latex_options = c("striped", "scale_down", "HOLD_position")) %>%
  #   #add_header_above(c("Shares" = 1, "CPS" = 1, "Michigan" = 1), background = "white") %>%
  #   add_footnote("Note: Table reports fraction of overall Rotemberg weights attributable to a broader categorization of demographic groups. This table gives indication of which broad groups are key to identification.", threeparttable = T) %>%
  #   save_kable("tables/weightsGroupCPS.tex")
  kbl(table_data_transposed, "latex", 
     caption = "Rotemberg weights for broad demographic groups.", 
     label = "rotweights:groups", 
     booktabs = TRUE, 
     col.names = names(table_data_transposed)) %>%
    #kable_styling(latex_options = c("striped", "scale_down", "HOLD_position")) %>%
    kable_styling(latex_options = c("striped","scale_down", "longtable = TRUE")) %>%
    #add_header_above(c("Shares" = 1, "CPS" = 1, "Michigan" = 1), background = "white") %>%
    footnote(general = "Table reports fraction of overall Rotemberg weights attributable to a broader categorization of demographic groups. This table gives indication of which broad groups are key to identification.", 
                 threeparttable = TRUE) %>%
    save_kable("tables/weightsGroupCPS.tex")
}

# Apply the function on your data frames Bwagg_out and Bwagg_cps
create_custom_latex_table(Bwagg_out, Bwagg_cps)

# Probing the identification

data_globalCPS <- data_globalCPS %>%
  mutate(group = industry) %>%
  select(-industry)

local_dataCPS78 <- classify_cps(cps78)
local_dataCPS78 <- local_dataCPS78 %>%
  mutate(cenREGION = case_when(REGION == 41 | REGION ==42 ~ 1, 
                               REGION == 21 | REGION ==22 ~ 2,
                               REGION == 11 | REGION ==12 ~ 3,
                               REGION == 31 | REGION ==32 ~ 4))
state_CPS78 <- local_dataCPS78 %>%
  select(c( "STATEFIP", "group")) %>%
  #drop_na() %>%
  group_by(STATEFIP,  group) %>%
  summarise(n=n( )) %>%
  mutate(prop = 10*n/sum(n, na.rm = TRUE)) %>%
  select(-n)

Bartik_CPS78 <- merge(state_CPS78, data_globalCPS) %>%
  group_by(STATEFIP, survey_date)%>%
  arrange(survey_date)

# Bartik_CPS78 <- Bartik_CPS78 %>%
#   group_by(cenREGION, survey_date) %>%
#   summarise(Bartik_cps = sum(prop*Agg_pe_ind))# %>%
#   #mutate(REGION = cenREGION)

Bartik_CPS78 <- Bartik_CPS78 %>%
  group_by(STATEFIP, survey_date, group,prop) %>%
  summarise(Bartik_cps = sum(prop*Agg_pe_ind)) 

library(plm)
tidy_fe <- markup_share_relationship(top10_groups, Bartik_CPS78)
tCovarShares <- share_covariates_relationship(top10_groups,fred_api_key, Bartik_CPS78)
# Package PLM interferes with a variety of tidyverse functions.
detach("package:plm", unload=TRUE)
