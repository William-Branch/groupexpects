# descriptive_plots.R

# Load common libraries and functions
library(dotenv)
print("dotenv library loaded")
source('scripts/common_libraries.R')
load_dot_env(".env")
fred_api_key <- Sys.getenv("FRED_API_KEY")
fredr_set_key(fred_api_key)
source('functions/functions_results.R')

# Load Data

michigan_path <- 'data/Michigan_220524.csv'
region_path <- 'data/dataBuild/regiondata.csv'
local_path <- 'data/dataBuild/localdataBigSmall.csv'
panel_path <- 'data/dataBuild/paneldata.csv'
regionCPS78_path <- 'data/dataBuild/regiondataout.csv'
panelcps_path <- 'data/dataBuild/panelcps.csv'
localdatabig_path <- 'data/dataBuild/localdataBig.csv'

michigan <- read_csv(michigan_path)
region_data <- read_csv(region_path)
local_dataBig_Small <- read_csv(local_path)
paneldata <- read_csv(panel_path)
region_CPS78 <- read_csv(regionCPS78_path)
panel_cps <- read_csv(panelcps_path)
localdataBig <- read_csv(localdatabig_path)

unAgg <- fredr(series_id = "UNRATE") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, AggUNRATE = value)%>%
  filter(survey_date > as.Date("1977-12-01"))
infl <- fredr(series_id = "CPIAUCSL", units = "pc1") %>%
  filter(!is.na(value))%>%
  mutate(survey_date = date, infl = value) %>%
  select(c(survey_date, infl)) 

# Preprocess Data
michigan <- preprocess_michigan_data(michigan)
paneldata <- preprocess_panel_data(paneldata)
Diverse <- preprocess_region_data(region_data)

# Data Transformation
master_dataGender <- calculate_mean_pe(michigan, "SEX")
master_dataRegion <- calculate_mean_pe(michigan, "REGION")
Gender_alt <- mutate(master_dataGender, SEX = as.character(SEX))
Region_alt <- calculate_region_percentage(master_dataRegion)
Industry <- calculate_industry_pe(local_dataBig_Small, c(5,41,21,37,90,134,81,141,85,62))
AllIndustry <- calculate_all_industry_pe(local_dataBig_Small, c(148,149))
Agger <- michigan %>%
  select(c("survey_date", "PX1","PX5")) %>%
  group_by(survey_date) %>%
  summarise(peavg = mean(PX1), longavg = mean(PX5))

Aggdata <- merge(infl, Agger) %>% merge(unAgg)

# Generate Introductory Scatter Plots

pScattA <- generate_aggregate_scatter(Aggdata)
pScattR <- generate_regional_scatter(paneldata)

# Generate and Save Men/Women Comparison Plot
pMW <- generate_men_women_plot(master_dataGender)


# Generate and Save Cross-Region Comparison Plot

pCR <- generate_cross_region_plot(Region_alt)


pWt10 <- generate_top10_industry_plot(Industry)


pAgg <- generate_all_industry_plot(AllIndustry)

diverse_plots <- generate_diverse_weights_plot(Diverse, region_data)

# Save the first plot using tikz
tikz(file = "figs/tex/weightsDiverse.tex", width = 6.5, height = 3.5)
print(diverse_plots$pD1)
dev.off()

pCPS <- generate_cps_weights_plot(region_CPS78)

pgroups <- plot_group_mean_expectations(localdataBig)

pShareDists <- plot_share_distributions(region_CPS78, region_data)

# Save the PDF plots
#ggsave("figs/regions.pdf", pCR)
#ggsave("figs/menwomen.pdf", pMW)
ggsave("figs/aggIndComp.pdf", pAgg)
ggsave("figs/weightsTop10comp.pdf", pWt10)
ggsave("figs/weightsTime.pdf", diverse_plots$pD2)
