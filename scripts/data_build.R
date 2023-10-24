# data_build.R

# Load common libraries
library(dotenv)
print("dotenv library loaded")
source('scripts/common_libraries.R')
load_dot_env(".env")
fred_api_key <- Sys.getenv("FRED_API_KEY")
fredr_set_key(fred_api_key)
# Load functions required for data processing
source('functions/functions_data.R')

# Set paths or parameters (if any)
mich_data_path <- 'data/Michigan_220524.csv'
cps_data_path <- 'data/cps_00002.csv'
output_data_path <- 'data/data_build/'

# Main data processing



michigan <- read_csv(mich_data_path)
michigan <- michigan %>%
  mutate(survey_date = as.Date(paste0(michigan[["YYYYMM"]],01), format = "%Y%m%d")) %>%
  filter(PX1<96) %>%
  filter(PX1!=-97)

cps78 <- read_csv(cps_data_path)

# Include only first time respondents;

michiganFirst <- michigan %>%
  filter(is.na(IDPREV))

# Exclude |P^e|>25;

michiganSmall <- michigan %>%
  filter(PX1 %in% (-25:25))

# regional data:
# REGION CPI inflation: monthly begins 1987



infNE <- fredr(series_id = "CUUR0100SA0", units = "pc1") %>%
  filter(!is.na(value))
infS <- fredr(series_id = "CUUR0300SA0", units = "pc1") %>%
  filter(!is.na(value))
infMW <- fredr(series_id = "CUUR0200SA0", units = "pc1") %>%
  filter(!is.na(value))
infW <- fredr(series_id = "CUUR0400SA0", units = "pc1") %>%
  filter(!is.na(value))

RegionInfW <- mutate(infW, REGION = 1) 
RegionInfW <- RegionInfW[c("date", "value", "REGION")]
RegionInfMW <- mutate(infMW, REGION = 2) 
RegionInfMW <- RegionInfMW[c("date", "value", "REGION")]
RegionInfNE <- mutate(infNE, REGION = 3) 
RegionInfNE <- RegionInfNE[c("date", "value", "REGION")]
RegionInfS <- mutate(infS, REGION = 4) 
RegionInfS <- RegionInfS[c("date", "value", "REGION")]
RegionInf <- merge(merge(merge(RegionInfW, RegionInfMW, all.x = TRUE, all.y = TRUE),RegionInfNE, all.x = TRUE, all.y = TRUE), RegionInfS, all.x = TRUE, all.y = TRUE)
RegionInf <- rename(RegionInf, survey_date = date)
RegionInf <- rename(RegionInf, RegInf = value)

# REGION unemployment rates

unAgg <- fredr(series_id = "UNRATE") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, AggUNRATE = value)%>%
  filter(survey_date > as.Date("1977-12-01"))

unW <- fredr(series_id = "CWSTUR") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, UNRATE = value) %>%
  mutate(REGION = 1) %>%
  merge(unAgg)%>%
  filter(survey_date > as.Date("1977-12-01"))

unMW <- fredr(series_id = "CMWRUR") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, UNRATE = value) %>%
  mutate(REGION = 2) %>%
  merge(unAgg) %>%
  filter(survey_date > as.Date("1977-12-01"))

unNE <- fredr(series_id = "CNERUR") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, UNRATE = value) %>%
  mutate(REGION = 3) %>%
  merge(unAgg) %>%
  filter(survey_date > as.Date("1977-12-01"))

unS <- fredr(series_id = "CSOUUR") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, UNRATE = value) %>%
  mutate(REGION = 4) %>%
  merge(unAgg) %>%
  filter(survey_date > as.Date("1977-12-01"))

unEm <- rbind(unW, unMW, unNE, unS)

#Region unleaded gas prices;

gasW <- fredr(series_id = "APU040074714") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, GasP = 100*value) %>%
  mutate(REGION = 1) 


gasMW <- fredr(series_id = "APU020074714") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, GasP = 100*value) %>%
  mutate(REGION = 2) 

gasNE <- fredr(series_id = "APU010074714") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, GasP = 100*value) %>%
  mutate(REGION = 3) 

gasS <- fredr(series_id = "APU030074714") %>%
  filter(!is.na(value)) %>%
  summarise(survey_date = date, GasP = 100*value) %>%
  mutate(REGION = 4) 

gasPrices <- rbind(gasW, gasMW, gasNE, gasS)


# master-data set for Rotemberg weights, for each of the 3 Michigan sets;
# note PX5 shortens data set to 1990;

master_dataBig <- michigan %>%
  group_by(REGION,survey_date) %>%
  summarise(pe = mean(PX1, na.rm = TRUE), pe5 = mean(PX5, na.rm = TRUE))
master_data <- master_dataBig[c("survey_date", "pe","pe5", "REGION")] %>%
  filter(!is.na(pe))

master_dataBig_no5 <- michigan %>%
  group_by(REGION, survey_date) %>%
  summarise(pe = mean(PX1, na.rm = TRUE))
master_data_no5 <- master_dataBig_no5[c("survey_date","pe","REGION")] %>%
  filter(!is.na(pe))

master_dataBig_First <- michiganFirst %>%
  group_by(REGION,survey_date) %>%
  summarise(pe = mean(PX1, na.rm = TRUE), pe5 = mean(PX5, na.rm = TRUE))
master_data_First <- master_dataBig_First[c("survey_date", "pe", "pe5", "REGION")] %>%
  filter(!is.na(pe))

master_dataBig_First_no5 <- michiganFirst %>%
  group_by(REGION, survey_date) %>%
  summarise(pe = mean(PX1, na.rm = TRUE))
master_data_First_no5 <- master_dataBig_First_no5[c("survey_date","pe","REGION")] %>%
  filter(!is.na(pe))

master_dataBig_Small <- michiganSmall %>%
  group_by(REGION,survey_date) %>%
  summarise(pe = mean(PX1, na.rm = TRUE), pe5 = mean(PX5, na.rm = TRUE))
master_data_Small <- master_dataBig_Small[c("survey_date", "pe", "pe5", "REGION")] %>%
  filter(!is.na(pe))

master_dataBig_Small_no5 <- michiganSmall %>%
  group_by(REGION,survey_date) %>%
  summarise(pe = mean(PX1, na.rm = TRUE))
master_data_Small_no5 <- master_dataBig_Small_no5[c("survey_date", "pe", "REGION")] %>%
  filter(!is.na(pe))



# Regional Inflation and mean inflation expectations, i.e. the DGP states;

master_data <- full_join(master_data,RegionInf) %>%
  filter(!is.na(pe), !is.na(REGION))

master_data_no5 <- full_join(master_data_no5,RegionInf) %>%
  filter(!is.na(pe), !is.na(REGION))

master_data_First <- full_join(master_data_First,RegionInf) %>%
  filter(!is.na(pe), !is.na(REGION))

master_data_First_no5 <- full_join(master_data_First_no5,RegionInf) %>%
  filter(!is.na(pe), !is.na(REGION))

master_data_Small <- full_join(master_data_Small, RegionInf) %>%
  filter(!is.na(pe), !is.na(REGION))

master_data_Small_no5 <- full_join(master_data_Small_no5, RegionInf) %>%
  filter(!is.na(pe), !is.na(REGION))

# Shares classification:
local_dataBig <- classify_michigan(michigan)
local_dataBig_First <- classify_michigan(michiganFirst)
local_dataBig_Small <- classify_michigan(michiganSmall)
local_dataCPS78 <- classify_cps(cps78)
local_dataCPS78 <- local_dataCPS78 %>%
  mutate(cenREGION = case_when(REGION == 41 | REGION ==42 ~ 1, 
                               REGION == 21 | REGION ==22 ~ 2,
                               REGION == 11 | REGION ==12 ~ 3,
                               REGION == 31 | REGION ==32 ~ 4))


types <- c("", "_First", "_Small")
suffixes <- c("", "_lag")


# Initialize types and suffixes
types <- c("", "_First", "_Small")
suffixes <- c("", "_lag")

# Loop through each type
for (type in types) {
  for (NoLong in c(TRUE, FALSE)) {
    # Handle the '_no5' suffix for NoLong == TRUE
    no5_suffix <- if (NoLong) "_no5" else ""
    
    # Apply regional data transformation
    data_var <- get(paste0("local_dataBig", type))
    region_data_var <- process_region_data(data_var)
    assign(paste0("region_data", type, no5_suffix), region_data_var)
    
    # Transform regional data to data_local
    assign(paste0("data_local", type, no5_suffix), transform_region_data(region_data_var))
    
    # Apply global data transformation
    global_data_var <- process_global_data(data_var, NoLong = NoLong)
    assign(paste0("data_global", type, no5_suffix), global_data_var)
    
    # Loop through each suffix
    for (suffix in suffixes) {
      # Calculate Bartik
      bartik_var <- calculate_bartik_original(region_data_var, global_data_var, NoLong = NoLong)
      assign(paste0("Bartik_data", type, suffix, no5_suffix), bartik_var)
    }
  }
}






region_CPS78 <- process_region_data_cps(local_dataCPS78)
# Copy survey_dates for CPS;
unique_dates <- unique(region_data$survey_date)
expanded_data <- expand.grid(survey_date = unique_dates, 
                             cenREGION = unique(region_CPS78$cenREGION), 
                             group = unique(region_CPS78$group))

final_data <- merge(expanded_data, region_CPS78, by = c("cenREGION", "group"), all.x = TRUE)
final_data <- as_tibble(final_data)


duplicates <- final_data %>%
  group_by(group, survey_date) %>%
  summarise(n = n()) %>%
  filter(n > 1)


final_data <- final_data %>% drop_na()


#data_local_CPS78 <- transform_region_data_cps(final_data)



local_sub <- local_dataBig_Small %>% select(c(survey_date, REGION, industry, PX1)) %>% group_by(survey_date, REGION,industry) %>%
  rename(cenREGION = REGION, group = industry)
  
final_data <- final_data %>%
  merge(local_sub)

write_csv(final_data,"data/dataBuild/final_2delete.csv")

data_local_CPS78 <- final_data %>%
  select(-n, -PX1) %>%
  mutate(group = stringr::str_glue("t{survey_date}_sh_ind_{group}"))
data_local_CPS78 <- data_local_CPS78 %>%
  group_by(cenREGION, group, survey_date) %>%
  summarise(prop = mean(prop, na.rm = TRUE)) %>%
  ungroup() %>%
  tidyr::spread(group, prop, fill = 0)

data_global_CPS78 <- process_global_data_CPS(final_data, NoLong = TRUE)

#data_global_CPS78 <- process_global_data(data_local_CPS78, NoLong = NoLong)
print("HERE IS THE GLOBAL DATA")
print(head(data_global_CPS78))
print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
data_globalCPS <- data_global %>%
  mutate(group = industry) %>%
  select(-industry)

data_globalCPS_no5 <- data_global_no5 %>%
  mutate(group = industry) %>%
  select(-industry)

# Process continuous controls
ContControls <- process_continuous_controls(michigan, gasPrices)
view(ContControls)

# Process discrete controls
DiscControls <- process_discrete_controls(michigan)

# Merge continuous and discrete controls
controls <- full_join(ContControls, DiscControls)

master_data_list <- list(
  "main" = master_data,
  "First" = master_data_First,
  "Small" = master_data_Small,
  "lag" = master_data,
  "First_lag" = master_data_First,
  "Small_lag" = master_data_Small
)

bartik_data_list <- list(
  "main" = Bartik_data,
  "First" = Bartik_data_First,
  "Small" = Bartik_data_Small,
  "lag" = Bartik_data_lag,
  "First_lag" = Bartik_data_First_lag,
  "Small_lag" = Bartik_data_Small_lag
)

# Extend the master_data_list and bartik_data_list with versions having _no5 suffix
master_data_list <- modifyList(master_data_list, list(
  "main_no5" = master_data_no5, # Assuming you've created these versions
  "First_no5" = master_data_First_no5,
  "Small_no5" = master_data_Small_no5
))

bartik_data_list <- modifyList(bartik_data_list, list(
  "main_no5" = Bartik_data_no5,
  "First_no5" = Bartik_data_First_no5,
  "Small_no5" = Bartik_data_Small_no5
))


# panel_data_list <- list()
# 
# for (key in names(master_data_list)) {
#   bartik_key <- key
#   if (endsWith(key, "_lag")) {
#     master_key <- sub("_lag", "", key)
#   } else {
#     master_key <- key
#   }
#   panel_data_list[[key]] <- merge_panel_data(
#     master_data_list[[master_key]],
#     bartik_data_list[[bartik_key]],
#     unEm,
#     controls
#   )
# }

panel_data_list <- list()

# Loop through each key in master_data_list to merge
for (key in names(master_data_list)) {
  
  # Check if the key ends with "_lag" or "_no5"
  is_lag = endsWith(key, "_lag")
  is_no5 = endsWith(key, "_no5")
  
  # Remove suffixes to find the original key
  original_key = key
  if (is_lag) {
    original_key = sub("_lag", "", original_key)
  }
  if (is_no5) {
    original_key = sub("_no5", "", original_key)
  }
  
  # Get the appropriate bartik_key
  bartik_key = if (is_no5) { paste0(original_key, "_no5") } else { original_key }
  if (is_lag) {
    bartik_key = paste0(bartik_key, "_lag")
  }
  
  # Merge the panel data
  panel_data_list[[key]] = merge_panel_data(
    master_data_list[[original_key]],
    bartik_data_list[[bartik_key]],
    unEm,
    controls
  )
}

panel_data <- panel_data_list[["main"]]
panel_data_no5 <- panel_data_list[["main_no5"]]
panel_data_First <- panel_data_list[["First"]]
panel_data_Small <- panel_data_list[["Small"]]
panel_data_lag <- panel_data_list[["lag"]]
panel_data_First_lag <- panel_data_list[["First_lag"]]
panel_data_Small_lag <- panel_data_list[["Small_lag"]]


panel_cps <- create_panel_cps(region_CPS78, data_globalCPS, panel_data, "survey_date")
panel_cps_no5 <- create_panel_cps(region_CPS78, data_globalCPS, panel_data, "survey_date", NoLong = TRUE)



# Leave out out Bartik
region_data <- region_data%>%
  mutate(cenREGION = REGION, group = industry) %>%
  select(-c(n, industry, REGION))

out_results <- calculate_leave_one_out(local_dataBig, region_data, region_CPS78, panel_data, "survey_date")


out_results_no5 <- calculate_leave_one_out(local_dataBig, region_data, region_CPS78, panel_data_no5, "survey_date",NoLong = TRUE)

# Write output data

data_to_save <- list(
  paneldata = panel_data,
  paneldataLag = panel_data_lag,
  paneldataFirstLag = panel_data_First_lag,
  paneldataSmallLag = panel_data_Small_lag,
  paneldataFirst = panel_data_First,
  paneldataSmall = panel_data_Small,
  regiondata = region_data,
  regiondataout = region_CPS78,
  masterdata = master_data,
  masterdataFirst = master_data_First,
  masterdataSmall = master_data_Small,
  masterdataSmallNo5 = master_data_Small_no5,
  localdata = data_local,
  localdataFirst = data_local_First,
  localdataSmall = data_local_Small,
  localdataCPS = data_local_CPS78,
  globaldata = data_global,
  globaldataFirst = data_global_First,
  globaldataSmall = data_global_Small,
  globaldataSmallNo5 = data_global_Small_no5,
  globaldataCPSNo5 = data_globalCPS_no5,
  globaldataCPS = data_global_CPS78,
  localdataBig = local_dataBig,
  localdataBigSmall = local_dataBig_Small,
  panelcps = panel_cps,
  panelcpsout = out_results$panel_out,
  panelbenchout = out_results$panel_bench_out,
  panelcpsoutNo5 = out_results_no5$panel_out,
  panelbenchoutNo5 = out_results_no5$panel_bench_out
)



path <- "data/dataBuild/"

for (name in names(data_to_save)) {
  filename <- paste0(path, name, ".csv")
  write_csv(data_to_save[[name]], filename)
}


# Print success message
print('Data processing completed successfully!')

