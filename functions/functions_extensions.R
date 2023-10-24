# functions_extensions.R

read_and_panel <- function(file_path, panel_formula) {
  read_csv(file_path) %>%
    panel(panel_formula)
}


#Function to calculate regional inflation from state-level CPI;

region_cpi_inflation <- function(df){

  
  
  inflation <- df %>%
    mutate(date = as.yearqtr(paste0(df$year, "-", df$quarter)))
  
  
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
  
NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
             "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
            "KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
            "HI","OR","WA")
W.ref <- c(W.name,W.abrv)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref)

NE_inflation <- filter(inflation, state %in% NE.name)
aggregate(pi ~ date, NE_inflation, mean)

region_inflation <- aggregate( pi ~ date, filter(inflation, state %in% NE.name), mean)

weighted.geomean <- function(x, w, ...)
{
  return(prod(x^w, ...)^(1/sum(w)))
}

# Expenditure weights



West_params <- list(
  series_id = c("AKPCE", "CAPCE", "HIPCE", "ORPCE", "WAPCE", "COPCE", "UTPCE"),
  observation_date = as.Date("1998-01-01")
) 
MidWest_params <- list(
  series_id = c("ILPCE", "MIPCE", "MOPCE", "MNPCE", "OHPCE", "WIPCE", "INPCE", "KSPCE"),
  observation_date = as.Date("1998-01-01")
) 
South_params <- list(
  series_id = c("DCPCE", "FLPCE", "GAPCE", "MDPCE", "TNPCE", "TXPCE", "ALPCE", "ARPCE", "LAPCE", "MSPCE", "NCPCE", "OKPCE", "SCPCE", "VAPCE"),
  observation_date = as.Date("1998-01-01")
) 
NE_params <- list(
  series_id = c("MAPCE", "NJPCE", "NYPCE", "PAPCE", "CTPCE"),
  observation_date = as.Date("1998-01-01")
) 

West_exp <- pmap_dfr(
  .l = West_params,
  .f = ~ fredr(series_id = .x, observation_start = .y, observation_end = .y)
)
MidWest_exp <- pmap_dfr(
  .l = MidWest_params,
  .f = ~ fredr(series_id = .x, observation_start = .y, observation_end = .y)
)
South_exp <- pmap_dfr(
  .l = South_params,
  .f = ~ fredr(series_id = .x, observation_start = .y, observation_end = .y)
)
NE_exp <- pmap_dfr(
  .l = NE_params,
  .f = ~ fredr(series_id = .x, observation_start = .y, observation_end = .y)
)

west_pre_states = c("AKPCE", "CAPCE", "HIPCE", "ORPCE", "WAPCE")
west_post_weights <- transmute(West_exp,
                               weights = value/sum(value) )
west_pre_weights <- transmute(filter(West_exp, series_id %in% west_pre_states),
                              weights = value/sum(value))

Midwest_pre_states = c("ILPCE", "MIPCE", "MOPCE", "MNPCE", "OHPCE", "WIPCE")
Midwest_post_weights <- transmute(MidWest_exp,
                                  weights = value/sum(value) )
Midwest_pre_weights <- transmute(filter(MidWest_exp, series_id %in% Midwest_pre_states),
                                 weights = value/sum(value))

South_pre_states = c("DCPCE", "FLPCE", "GAPCE", "GAPCE", "MDPCE", "TNPCE", "TXPCE")
South_1989_states = c("DCPCE", "FLPCE", "GAPCE", "MDPCE", "TNPCE", "TXPCE", "ALPCE", "ARPCE", "LAPCE", "MSPCE", "NCPCE", "SCPCE", "VAPCE")
South_post_weights <- transmute(South_exp,
                                weights = value/sum(value) )
South_pre_weights <- transmute(filter(South_exp, series_id %in% South_pre_states),
                               weights = value/sum(value))
South_1989_weights <- transmute(filter(South_exp, series_id %in% South_1989_states),
                                weights = value/sum(value))

NE_pre_states = c("MAPCE", "NJPCE", "NYPCE", "PAPCE")
NE_post_weights <- transmute(NE_exp,
                             weights = value/sum(value) )
NE_pre_weights <- transmute(filter(NE_exp, series_id %in% NE_pre_states),
                            weights = value/sum(value))


west_inflation <- filter(inflation, state %in% W.name)
west_pre <- filter(west_inflation, date>= "1978 Q1" & date< "1987 Q4")
west_post <- filter(west_inflation, date>= "1988 Q4")
midwest_inflation <- filter(inflation, state %in% MW.name)
midwest_pre <- filter(midwest_inflation, date>= "1978 Q1" & date<= "1987 Q4")
south_inflation <- filter(inflation, state %in% S.name)
south_pre <- filter(south_inflation, date>= "1978 Q1" & date<= "1987 Q4")
ne_inflation <- filter(inflation, state %in% NE.name)
ne_pre <- filter(ne_inflation, date>= "1978 Q1" & date<= "1987 Q4")

pre_dates <-  transmute(filter(west_pre, state == "Alaska"), dates = date)
Tp = 36



select(west_pre, pi)

west_pre_inf <-  0
midwest_pre_inf <-  0
south_pre_inf <- 0
ne_pre_inf <- 0

for(i in 1:Tp){
  west_pre_inf[i] = weighted.mean(select(filter(west_pre, date %in% pre_dates[[i,1]]), pi), west_pre_weights)
  midwest_pre_inf[i] = weighted.mean(select(filter(midwest_pre, date %in% pre_dates[[i,1]]), pi), Midwest_pre_weights)
  south_pre_inf[i] = weighted.mean(select(filter(south_pre, date %in% pre_dates[[i,1]]), pi), South_pre_weights)
  ne_pre_inf[i] = weighted.mean(select(filter(ne_pre, date %in% pre_dates[[i,1]]), pi), NE_pre_weights)
  
}




predates = west_pre[["date"]][1:36]



west_cpi_inf <- tibble(time = as.yearqtr(predates), west_inf = west_pre_inf) 
west_cpi_inf$time <- yearquarter(west_cpi_inf$time)
midwest_cpi_inf <- tibble(time = as.yearqtr(predates), midwest_inf = midwest_pre_inf) 
midwest_cpi_inf$time <- yearquarter(midwest_cpi_inf$time)
south_cpi_inf <- tibble(time = as.yearqtr(predates), south_inf = south_pre_inf) 
south_cpi_inf$time <- yearquarter(south_cpi_inf$time)
ne_cpi_inf <- tibble(time = as.yearqtr(predates), ne_inf = ne_pre_inf) 
ne_cpi_inf$time <- yearquarter(ne_cpi_inf$time)



west_ts <- west_cpi_inf %>%
  as_tsibble( index = 'time')
midwest_ts <- midwest_cpi_inf %>%
  as_tsibble( index = 'time')
south_ts <- south_cpi_inf %>%
  as_tsibble( index = 'time')
ne_ts <- ne_cpi_inf %>%
  as_tsibble( index = 'time')

mW <- td(west_ts ~ 1, to = "monthly")
mMW <- td(midwest_ts ~ 1, to = "monthly")
mS <- td(south_ts ~ 1, to = "monthly")
mNE <- td(ne_ts ~ 1, to = "monthly")

WestMonth <- predict(mW) %>% mutate(REGION = 1) 
MidwestMonth <- predict(mMW) %>% mutate(REGION = 2)
SouthMonth <- predict(mS) %>% mutate(REGION = 4)
NEMonth <- predict(mNE) %>% mutate(REGION = 3)

WestMonth <- as_tibble(WestMonth)
MidwestMonth <- as_tibble(MidwestMonth)
SouthMonth <- as_tibble(SouthMonth)
NEMonth <- as_tibble(NEMonth)

infMonth <- merge(WestMonth,MidwestMonth, all.x = TRUE, all.y = TRUE) %>%
  merge(SouthMonth, all.x = TRUE, all.y = TRUE) %>%
  merge(NEMonth, all.x = TRUE, all.y = TRUE) %>%
  mutate(RegInf = 4*value) %>%
  mutate(survey_date = time) %>%
  select(c("survey_date", "RegInf", "REGION"))


RegionInfState <- merge(infMonth, filter(RegionInf, survey_date > "1986-12-01"), all.x = TRUE, all.y = TRUE)
return(RegionInfState)

}




build_robust_table <- function(michigan, michiganFirst, michiganSmall, paneldataState, paneldataFirst, paneldataSmall, paneldataLag){



#Group Categorizations;

local_dataBig <- classify_michigan(michigan)
local_dataBig_First <- classify_michigan(michiganFirst)
local_dataBig_Small <- classify_michigan(michiganSmall)
region_data <- process_region_data(local_dataBig)


# Leave one out, Bartik. (Small,First,State,Lag)

#Small
dataSmall_out_west <- local_dataBig_Small %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==1) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION = 1)

dataSmall_out_mw <- local_dataBig_Small %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==2) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION =2)

dataSmall_out_ne <- local_dataBig_Small %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==3) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION =3)

dataSmall_out_s <- local_dataBig_Small %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==4) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION=4)


dataSmall_out <- rbind(dataSmall_out_west,dataSmall_out_mw, dataSmall_out_ne,dataSmall_out_s) %>% mutate(group = industry) %>% select(-industry)

region_data_Small <- local_dataBig_Small %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  drop_na() %>%
  group_by(REGION, survey_date, industry) %>%
  summarise(n=n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(survey_date > as.Date("1977-12-01"))

Bartik_bench_out_Small <- merge(region_data_Small, dataSmall_out) %>%
  drop_na() %>%
  group_by(cenREGION, survey_date) %>%
  arrange(survey_date) %>%
  summarise(Bartik_bench_out_Small = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = cenREGION)



panel_bench_out_Small <- merge(paneldataSmall, Bartik_bench_out_Small) %>% panel(~REGION + survey_date)


# First;

dataFirst_out_west <- local_dataBig_First %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==1) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION = 1)

dataFirst_out_mw <- local_dataBig_First %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==2) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION =2)

dataFirst_out_ne <- local_dataBig_First %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==3) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION =3)

dataFirst_out_s <- local_dataBig_First %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==4) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION=4)


dataFirst_out <- rbind(dataFirst_out_west,dataFirst_out_mw, dataFirst_out_ne,dataFirst_out_s) %>% mutate(group = industry) %>% select(-industry)

region_data_First <- local_dataBig_First %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  drop_na() %>%
  group_by(REGION, survey_date, industry) %>%
  summarise(n=n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(survey_date > as.Date("1977-12-01"))

Bartik_bench_out_First <- merge(region_data_First, dataFirst_out) %>%
  drop_na() %>%
  group_by(cenREGION, survey_date) %>%
  arrange(survey_date) %>%
  summarise(Bartik_bench_out_First = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = cenREGION)


panel_bench_out_First <- merge(paneldataFirst, Bartik_bench_out_First) %>% panel(~REGION + survey_date)


# State;

data_out_west <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==1) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION = 1)

data_out_mw <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==2) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION =2)

data_out_ne <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==3) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION =3)

data_out_s <- local_dataBig %>%
  select(c("survey_date", "REGION", "industry", "PX1")) %>%
  #drop_na() %>%
  group_by(survey_date, industry) %>%
  filter(!REGION==4) %>%
  summarise(Agg_pe_ind = mean(PX1, na.rm=TRUE)) %>%
  mutate(cenREGION=4)

data_out <- rbind(data_out_west,data_out_mw, data_out_ne,data_out_s) %>% mutate(group = industry) %>% select(-industry)


Bartik_state_out <- merge(region_data, data_out) %>%
  #drop_na() %>%
  group_by(cenREGION, survey_date) %>%
  arrange(survey_date) %>%
  summarise(Bartik_bench_out_State = sum(prop*Agg_pe_ind)) %>%
  mutate(REGION = cenREGION)






panel_out_State <- merge(paneldataState, Bartik_state_out) %>% panel(~REGION + survey_date, duplicate.method = 'first')

# Lag

Bartik_Lag_out <- merge(region_data, data_out) %>%
  mutate(lagprop = lag(prop,12)) %>%
  drop_na() %>%
  group_by(cenREGION, survey_date) %>%
  arrange(survey_date) %>%
  summarise(Bartik_bench_out_Lag = sum(lagprop*Agg_pe_ind)) %>%
  mutate(REGION = cenREGION)






panel_out_Lag <- merge(paneldataLag, Bartik_Lag_out) %>% panel(~REGION + survey_date)



est1 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out_Small  , panel_bench_out_Small)
est2 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out_First  , panel_bench_out_First)
est3 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out_State  , panel_out_State)
est4 <- feols(RegInf ~  l(RegInf,1:2) + UNRATE + UNEMPGood +  FinBetterLast1+ RincUp+ FinBetterNext1+ BizGood+ BizExGood+ gasExp | REGION + yearquarter | pe  ~ Bartik_bench_out_Lag  , panel_out_Lag)




table_1 <- etable(est1, est2, est3, est4, vcov = "DK", dict = c("Bartik_cps" = "Bartik", "yearquarter" = "TIME","l(RegInf,1)" = "lag 1", "l(RegInf,2)" = "lag 2"), stage = 2, title = "CPS shift-share regressions",  group = list(Controls = c("UNRATE", "UNEMPGood", "lag 1", "lag 2", "UNRATE", "UNEMPGood", "FinBetterLast1", "FinBetterNext1", "BizGood", "BizExGood", "RincUp","gasExp")))


kbl(table_1, "latex", booktabs = T, caption = "Alternative estimates", col.names = c( "small", "first-only", "state-CPI", "lag michigan shares"), linesep = "\\addlinespace", label = "table:2sls:robust") %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  footnote(general = "Reports panel regression results for a variety of alternative specifications. ``small'' removes large survey responses.  ``first'' includes only first-time survey respondents.  ``state-cpi'' measures regional inflation by aggregating state-level CPI's.  ``lag michigan shares'' instruments with 12 month lagged survey shares." , threeparttable = T) %>%
  save_kable("tables/robustOut.tex")

}