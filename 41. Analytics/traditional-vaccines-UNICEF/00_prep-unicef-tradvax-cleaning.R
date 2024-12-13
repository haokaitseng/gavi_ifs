#### FILE DESCRIPTION ####
# Filename: 00_prep-unicef-trad-vax-cleaning.R
# Author: Anthony Nguyen, Hao-Kai Tseng
# Date: 2024-05-31, modified on 2024-09-06
# Description: read and clean tradiional vaccine data from UNICEF, generate master data (wihtout subsetting),
# check missingness and mismatch of traditional vaccines, imputing vaccine forecast estimate by actuals allocations



#### SETUP ####
setwd("I:/")
#setwd("C:/Users/hktseng/Gavi/Immunisation Financing and Sustainability - Documents") # set this to the correct path on your system

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(readxlsb)) install.packages("readxlsb", repos = "http://cran.us.r-project.org")#for xlsb form
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")#for arrange 4in1 plot
if(!require(countrycode)) install.packages("countrycode", repos = "http://cran.us.r-project.org")
if(!require(cleanepi)) install.packages("cleanepi", repos = "http://cran.us.r-project.org")

options(scipen=999)

rm(list=ls())

#parameters
params_year_analysis <- 2024 


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### 1. READ DATA: traditional actuals & forecast and related data at IFS ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# (1) read tradvax 
source_path <- "40. Data/UNICEF_traditional-vaccines/Procurement of traditional vaccines report - Q3 2024 - final.xlsb"#Procurement of traditional vaccines report - Q2 2024.xlsb

# actuals = Q3 = Q1+Q2+Q3
dat_trad_gavi_q <- read_xlsb(paste0(getwd(), "/", source_path), sheet = "Actuals Q3 Gavi", col_names = TRUE, skip = 1) |> 
  {\(dat) {colnames(dat)[c(1, 6:15)] <- c("gavi_non_gavi", 
                                          paste0(dat[2, 6:15], rep(c("_unicef_cofi","_unicef_gavi","_programme", "_PS", "_all"), each = 2))) 
  dat}}() |> #single line 
  clean_names() |>
  filter(!gavi_non_gavi %in% c("", "Grand Total")) |>
  mutate_if(is.character, ~ na_if(., "")) |> 
  mutate_at(vars(6:15), as.numeric)


dat_trad_former_q <- read_xlsb(paste0(getwd(), "/", source_path), sheet = "Actuals Q3 former Gavi", col_names = TRUE, skip = 1) |> 
  {\(dat) {colnames(dat)[c(1, 6:15)] <- c("gavi_non_gavi", 
                                          paste0(dat[2, 6:15], rep(c("_unicef_cofi","_unicef_gavi","_programme", "_PS", "_all"), each = 2))) 
  dat}}() |> 
  clean_names() |>
  filter(!gavi_non_gavi %in% c("", "Grand Total")) |>
  mutate_if(is.character, ~ na_if(., "")) |> 
  mutate_at(vars(6:15), as.numeric)

dat_trad_gavi_forecast <- read_xlsb(paste0(getwd(), "/", source_path), sheet = "Forecast Gavi", col_names = TRUE, skip = 2) |> 
  {\(dat) {colnames(dat)[c(1:8)] <- c("gavi_non_gavi","country","product_group","procurement_unicef_cofi_doses","procurement_unicef_gavi_doses","procurement_programme_doses","procurement_PS_doses","total_doses" )
  dat}}() |> 
  clean_names() |>
  filter(gavi_non_gavi !="Grand Total") |>
  mutate_if(is.character, ~ na_if(., "")) |> 
  mutate_at(vars(4:8), as.numeric)
  
dat_trad_former_forecast <- read_xlsb(paste0(getwd(), "/", source_path), sheet = "Forecast former Gavi", col_names = TRUE, skip = 2) |> 
  {\(dat) {colnames(dat)[c(1:8)] <- c("gavi_non_gavi","country","product_group","procurement_unicef_cofi_doses","procurement_unicef_gavi_doses","procurement_programme_doses","procurement_PS_doses","total_doses"  )
  dat}}() |> 
  clean_names() |>
  filter(gavi_non_gavi !="Grand Total") |>
  mutate_if(is.character, ~ na_if(., "")) |> 
  mutate_at(vars(4:8), as.numeric)

# combine Gavi and former-Gavi for Q3 and forecast, respectively
# caution that the total doses column from Q3 include cofi and gavi, so I need to calculate my own defined total traditional cost and doses
dat_trad_q <- rbind(dat_trad_gavi_q, dat_trad_former_q)|>
  rename(global_total_doses_all = total_doses_all, #the "total" said as all
         global_total_shipped_value_all = total_shipped_value_all)|>
  mutate(total_doses_all= rowSums(cbind(no_of_doses_programme,no_of_doses_ps),na.rm=T),
         total_shipped_value_all= rowSums(cbind(shipped_value_programme,shipped_value_ps),na.rm=T))

dat_trad_forecast <- rbind(dat_trad_gavi_forecast, dat_trad_former_forecast)|>
  rename(global_total_doses = total_doses)|>
  mutate(total_doses= rowSums(cbind(procurement_programme_doses,procurement_ps_doses),na.rm=T))

rm(dat_trad_gavi_q, dat_trad_former_q, dat_trad_gavi_forecast, dat_trad_former_forecast)

# (2) read Gavi data 
# REFERENCE USING SAP & GROUPING 
dat_SAP <- read_excel(paste0(getwd(),"/40. Data/SAP_me2j-po-per-project/SAP_Purchase-Orders_FLAT_2024-12-05.xlsx")) |> #FLAT_2024-10-11
  clean_names() |>
  mutate(budget_period=if_else(iso3%in%c("PAK","TAZ","ETH","KEN"), as.integer(budget_period)+1, as.integer(budget_period)))|>
  filter(budget_period == params_year_analysis)

dat_grouping <- read_excel(paste0(getwd(),"/40. Data/IFS_mapping-files/country-mapping.xlsx"),sheet = "country_mapping") |>  # not 40. Data/MEL_country-groupings/country_groupings_2024-03.xlsx
  clean_names() |>
  select(iso3, country_name_ifs, cofi_group, gavi_segment) |> # last_update
  mutate(cofi_group_acro = case_when( 
  cofi_group == "Accelerated transition phase" ~ "AT", 
  cofi_group == "Fully self-financing" ~ "FS", 
  cofi_group == "Initial self-financing" ~ "ISF", 
  cofi_group == "Preparatory transition phase" ~ "PT"))

dat_vaccine_group <- read_excel(paste0(getwd(),"/40. Data/IFS_mapping-files/vaccine-groups.xlsx"),sheet = "long_library") |>  
  clean_names() 

dat_sf <- read_excel(paste0(getwd(),"/40. Data/Salesforce_cofi-tracking-more-vars/export-raw/Co-financing Tracking more variables-2024-10-23-15-57-40.xlsx")) |> 
  clean_names()|>
  filter(year %in%c("2024","2024-2025"))


dat_birth <- read_excel("41. Analytics/population/live_birth_surviving_age_one_until_year_2023_2024-11-03.xlsx")|>clean_names()

dat_shipment <- read_excel("40. Data/UNICEF_shipment-report/Gavi-WEB-(all-regions) - September 2024.xlsx",sheet ="2024",skip = 2)|> clean_names()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. CLEAN DATA: merge traditional with grouping and SAP #### 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dat_trad_q <- dat_trad_q |> 
  mutate(iso3 = countrycode(country, 
                            origin = 'country.name', 
                            destination = 'iso3c'),
         iso3 = if_else(country=="Central Afr.Rep","CAF",iso3),
         iso3 = if_else(country=="S.Tome&Principe","STP",iso3))|>
  left_join(dat_grouping |> select(iso3, country_name_ifs))
  
dat_trad_forecast <- dat_trad_forecast |> 
  mutate(iso3 = countrycode(country, 
                            origin = 'country.name', 
                            destination = 'iso3c'),
         iso3 = if_else(country=="Pacific Islands","PCE",iso3))|> #PCE is NOT an official iso3
  left_join(dat_grouping |> select(iso3, country_name_ifs))|>
  mutate(country_name_ifs = if_else(country=="Pacific Islands","Pacific Islands",country_name_ifs))


dat_SAP <- left_join(dat_SAP, dat_grouping, by="iso3")|> #attach country name
   mutate(country_name_ifs = if_else(iso3=="TAR","Syria-TARAA",country_name_ifs ))


dat_sf <- dat_sf |> 
  rename(iso3=country_code)|>
  group_by(iso3)|>
  summarise(cost = sum(country_amount_paid_including_devices,na.rm=T),.groups="drop")|>
  mutate(category = "Co-financing paid",
         doses=NA,
         year=params_year_analysis,
         vaccine_group_wbs=NA)|>
  left_join(dat_grouping, by="iso3")|>
  rename(country = country_name_ifs)
  
dat_shipment <-  clean_using_dictionary(
  data = dat_shipment|> 
      mutate(vaccine_group_wbs = sub("^[^-]+-([^-]+).*", "\\1", gavi_business_key),
             whether_vac = grepl("VAC", gavi_business_key))|>
      filter(whether_vac == TRUE),
  dictionary = dat_vaccine_group|> # standardize vax names, only Lebanon and Sri Lanka changed for DT
      select(- src_options, - src_values))|>
  mutate(iso3 = countrycode(country, 
                            origin = 'country.name', 
                            destination = 'iso3c'),
         iso3 = if_else(country=="Kosovo","XKX", iso3),
         planned_delivery_year = substr(planned_delivery_month,1,4),
         flag_shipment = case_when(
           !is.na(actual_delivery_date_to_country)~"Delivered",
           planned_delivery_year=="2024"~"Planned 2024",
           planned_delivery_year=="2025"~"Planned 2025",
           TRUE ~ NA) 
         )|>
  select(country, iso3,vaccine_group_wbs,gavi_business_key, total_quantity_in_doses, flag_shipment, gavi_non_gavi )|>
  rename(doses = total_quantity_in_doses, gavi_co_financing = gavi_non_gavi)

 # aggregate by period
dat_shipment_aggr <- dat_shipment|>
  filter(#flag_shipment != "Planned 2025",
         vaccine_group_wbs!="C19")|>
  group_by(country, iso3, vaccine_group_wbs, flag_shipment)|>
  summarise( doses = sum(doses,na.rm=T), .groups = "drop")|>
  pivot_wider(id_cols = c("country","iso3","vaccine_group_wbs"), 
              values_from = doses, 
              names_from = flag_shipment,
              names_prefix="")|>
  rename(doses_ship_delivered = Delivered, doses_ship_planned =`Planned 2024`,doses_ship_planned_2025 =`Planned 2025`)|>
  mutate(doses_ship_delivered = if_else(is.na(doses_ship_delivered),0,doses_ship_delivered),
         doses_ship_planned = if_else(is.na(doses_ship_planned),0,doses_ship_planned),
         doses_ship_planned_2025= if_else(is.na(doses_ship_planned_2025),0,doses_ship_planned_2025),
         doses_ship_forecast = doses_ship_delivered + doses_ship_planned,
         doses_ship_forecast_nextyr = doses_ship_delivered + doses_ship_planned+doses_ship_planned_2025
         )

#+++++++++++++++++++++++++++++++
# 3. FIND MISSING COUNTRIES ####
#+++++++++++++++++++++++++++++++
#based on forecast data rather than actuals data
table_trad_missing <- merge(dat_trad_forecast, 
                          dat_SAP|> select(-country_name_ifs), 
                          by="iso3", all=TRUE)|>
  select(iso3, gavi_non_gavi, country, country_name_ifs, cofi_group, gavi_segment)|>
  unique()|>
  filter(is.na(gavi_non_gavi))|>
  mutate(country_name_ifs = if_else(iso3=="TAR","Syria-TARAA",country_name_ifs ))|> #TAR is not a iso code in real life, just for Gavi internal key
  select(-c(gavi_non_gavi,country))|>
  arrange(cofi_group)

print(table_trad_missing)

#++++++++++++++++++++++++++++++++++++++++++++++++
# 4. COST ESTIMATION: for vaccine unit price #### 
#++++++++++++++++++++++++++++++++++++++++++++++++
# using Q3 actuals to impute forecast of the whole year with two approaches: (1) global, (2) gavi or non-gavi country 
# the global estimate approach is easy bu not recommended by UNICEF, so not used anymore 
# (1) global estimate, regardless of gavi or non gavi - this is not recommended 
# temp_vaccine_unit_price <- dat_trad_q |> group_by(product_group)|> # also by country is doable, but not efficient, and doesn't have significant difference
#   summarise(vaccine_unit_price = mean(total_shipped_value_all / total_doses_all))


# (2) deliberate estimate, considering gavi or non gavi - the way that SD do their estimation
temp_vaccine_unit_price_bygavinongavi <- dat_trad_q |>
  mutate(gavi_non_gavi=if_else(country=="Congo","GAVI",gavi_non_gavi))|> #wrong label for Congo from UNICEF
  group_by(gavi_non_gavi,product_group)|> 
  summarise(vaccine_unit_price = mean(global_total_shipped_value_all/ #using Global costs/doses, assuming no difference between channels
                                      global_total_doses_all), .groups = "drop") 

temp_vaccine_unit_price_bygavinongavi_flat <- temp_vaccine_unit_price_bygavinongavi |>
  filter(gavi_non_gavi=="Former GAVI")|>
  full_join(temp_vaccine_unit_price_bygavinongavi |> 
              filter(gavi_non_gavi=="GAVI")|>
              rename(gavi_non_gavi2=gavi_non_gavi,vaccine_unit_price2=vaccine_unit_price),
            by="product_group") # left is former, right is gavi
  # rabies is expensive but true according to unicef website

#fill price for those haven't appear in actuals Q2 using (i) information borrowing between gavi-nongavi (using temp_vaccine_unit_price_bygavinongavi_flat)(ii) public price on websites (for Mening, Malaria, Hib)  
temp_vaccine_unit_price_bygavinongavi <- rbind(
  temp_vaccine_unit_price_bygavinongavi
  ,
  data.frame(gavi_non_gavi = "Former GAVI", #fill missing for former inspecting temp_vaccine_unit_price_bygavinongavi_flat
             product_group = c("Ebola",  #using temp_vaccine_unit_price_bygavinongavi_flat
                               "Malaria", 
                               "Mening"),
             vaccine_unit_price = c(98.6,
                                    7.6101007,
                                    0.7001562))
  ,
  data.frame(gavi_non_gavi = "GAVI", 
             product_group = c("Hep A"),#using temp_vaccine_unit_price_bygavinongavi_flat
             vaccine_unit_price = c(6.9900000))
  ,
  data.frame(gavi_non_gavi = "Former GAVI",
             product_group = c(#"Mening", #according to UNICEF (https://www.unicef.org/supply/documents/meningococcal-vaccine-price-data)
                               #"Malaria", # according to UNICEF (https://www.unicef.org/supply/documents/malaria-vaccine-price-data)
                               "Hib"),#weird why Kenya need Hib?  according to expensive US CDC $11.178 too high (https://www.cdc.gov/vaccines-for-children/php/awardees/current-cdc-vaccine-price-list.html)  
             vaccine_unit_price = c(#2.067,#(0.7010+2.5+3)/3=2.067
                                    #3.145,#(€9.3+$3.9)/2=($10.24+$3.9)/2=7.07
                                    0.8525))#I use PENTA ($0.8525000)from actuals instead  
  ,
  data.frame(gavi_non_gavi = "GAVI",
             product_group = c(#"Mening", 
                               #"Malaria", 
                               "Hib"),
             vaccine_unit_price = c(#2.067,
                                    #3.145,
                                    0.8525))  
)

temp_vaccine_unit_price_bygavinongavi_flat <- temp_vaccine_unit_price_bygavinongavi |> # cover the flat data with missing
  filter(gavi_non_gavi=="Former GAVI")|>
  full_join(temp_vaccine_unit_price_bygavinongavi |> 
              filter(gavi_non_gavi=="GAVI")|>
              rename(gavi_non_gavi2=gavi_non_gavi,vaccine_unit_price2=vaccine_unit_price),
            by="product_group")|>
  filter(product_group!="(blank)")#sucks that missing in vaccine group 

#aligning traditional vax names to SAP names
dat_trad_forecast <- dat_trad_forecast|>
  mutate(gavi_non_gavi=if_else(country=="Congo","GAVI",gavi_non_gavi))|>
  left_join(temp_vaccine_unit_price_bygavinongavi,
            by = c("gavi_non_gavi","product_group"))|>    # using deliberate estimate
  mutate(total_cost = round(total_doses * vaccine_unit_price)) #! IMPORTANT calculation

dat_trad_forecast <- clean_using_dictionary(
 data = dat_trad_forecast|> 
   mutate(vaccine_group_wbs = product_group),
  dictionary = dat_vaccine_group|>
               select(-src_options,-src_values))|>
  select(- product_group)
  
dat_trad_q <- clean_using_dictionary(
  data = dat_trad_q|> 
    mutate(vaccine_group_wbs = product_group),
  dictionary = dat_vaccine_group|>
    select(-src_options,-src_values))|>
  select(- product_group)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. MASTER DATASET: a complete datasets, can be used for all purpose and subsetting ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# (1) long raw ####
dat_master_long <- rbind(
  dat_trad_forecast |>
    select(iso3,country, vaccine_group_wbs,total_doses,total_cost)|>
    rename(doses=total_doses, cost = total_cost)|>
    mutate(funding_source_name = NA,
           category = "Traditional: forecast") 
  , 
  dat_trad_q|>
    select(iso3,country, vaccine_group_wbs,total_doses_all,total_shipped_value_all,funding_source_name)|>
    rename(doses=total_doses_all, cost=total_shipped_value_all)|>
    mutate(category = "Traditional: actuals")
  ,
  dat_SAP|>
    select(iso3,country_name_ifs,vaccine_group_wbs,co_financing_doses,co_financing_usd)|>
    rename(country=country_name_ifs, doses=co_financing_doses, cost= co_financing_usd)|>
    mutate(funding_source_name = NA,
           category = "Co-financing obligation")
  ,
  dat_SAP|>
    select(iso3,country_name_ifs,vaccine_group_wbs,gavi_support_doses,gavi_support_usd)|>
    rename(country=country_name_ifs, doses=gavi_support_doses, cost=gavi_support_usd)|>
    mutate(funding_source_name = NA,
           category = "Gavi-support")
)|>
  left_join(dat_grouping, by="iso3")|>
  mutate(country = country_name_ifs, #cover the country name that may exist mismatch
         year = 2024 )|>
  select(- country_name_ifs)|>
  #funding sources  
  mutate(funding_source_cat_original = if_else(!is.na(funding_source_name),
                                               case_when( # category
                                                 grepl("(?i)mini|national|gouvernement|public|center", funding_source_name) ~ "MoH",     #case-insensitive natural expression
                                                 grepl("(?i)UNICEF|children", funding_source_name) ~ "UNICEF", 
                                                 grepl("(?i)bank", funding_source_name) ~ "WB",       
                                                 category=="Co-financing obligation" ~ "Co-financing obligation",
                                                 category=="Gavi-support" ~ "Gavi-support",
                                                 TRUE ~ "Other" ),
                                               NA))|>
  mutate(funding_source_cat_original = if_else(
    funding_source_name%in%c("D.G. CENTRAL MEDICAL STORES DEPT.",
                             "Government of Kenya (VII)",
                             "Government of Kenya (VII), KENYA-MoPH",
                             "CENTRAL MEDICAL STORES",
                             "DIRECTION GEN. DE LA SANTE ET DES",
                             "Directorate General Health Services",
                             "PERMANENT SECRETARY (TECHNICAL",
                             "PERMANENT SECRETARY",
                             "BORDER HEALTH SERVICES - PAKISTAN"),
    "MoH",
    funding_source_cat_original
  ))|>
  mutate(funding_source_cat_original= if_else(category == "Traditional: actuals" & # address AFG rabies has NA in funding source procured
                                                cost > 0 &
                                                !is.na(cost) &
                                                is.na(funding_source_cat_original),
                                              "Other", 
                                              funding_source_cat_original))|>
  mutate(funding_source_cat = case_when(
    funding_source_cat_original=="UNICEF" ~ "Traditional actuals: UNICEF",
    funding_source_cat_original=="WB" ~ "Traditional actuals: WB",
    funding_source_cat_original=="MoH" ~ "Traditional actuals: MoH",
    funding_source_cat_original=="Other" ~ "Traditional actuals: Other",
    TRUE ~ funding_source_cat_original))|>
  mutate(funding_source_cat=factor(funding_source_cat,levels=c("Gavi-support","Co-financing obligation","Traditional actuals: MoH","Traditional actuals: UNICEF","Traditional actuals: WB","Traditional actuals: Other")))|>     
  #flag Gavi-funded programmes, regardless of cofi obligations 
  left_join( 
    dat_SAP|> 
      select(iso3,vaccine_group_wbs)|>
      unique()|>
      mutate(flag_gavi_funded = "Gavi-funded"),
    by=c("iso3","vaccine_group_wbs")
  )|>
  mutate(flag_gavi_funded = if_else(is.na(flag_gavi_funded),"Non-Gavi-funded",flag_gavi_funded))|>
  # attach cofi paid, note that it is aggregated and no vaccine group 
  rbind(
    dat_sf |>
      mutate(funding_source_name=NA,funding_source_cat_original=NA,funding_source_cat=NA,flag_gavi_funded="Gavi-funded")|>
      select(iso3, country, vaccine_group_wbs, doses, cost,funding_source_name,category, cofi_group, gavi_segment, cofi_group_acro, year, funding_source_cat_original,funding_source_cat,flag_gavi_funded)
  )



# (2) long aggregated ####
dat_master_long_agg <- dat_master_long |>
  group_by(iso3, country,vaccine_group_wbs,category,cofi_group,cofi_group_acro,gavi_segment,flag_gavi_funded)|>
  summarise(doses = sum(doses,na.rm=T), #some have duplicated product
            cost = sum(cost,na.rm=T),.groups = "drop")|>
  #calculating forecast remaining
  left_join(dat_trad_q|> 
              select(iso3,vaccine_group_wbs,total_doses_all,total_shipped_value_all)|>
              rename( trad_q_doses = total_doses_all, trad_q_cost = total_shipped_value_all)|>
              group_by(iso3, vaccine_group_wbs)|>
              summarise(trad_q_doses=sum(trad_q_doses,na.rm=T),
                        trad_q_cost=sum(trad_q_cost,na.rm=T), .groups = "drop")  
            ,
            by=c("iso3","vaccine_group_wbs"))|>
  mutate(trad_q_doses=if_else(is.na(trad_q_doses),0,trad_q_doses),
         trad_q_cost=if_else(is.na(trad_q_cost),0,trad_q_cost),
         trad_forecast_q_abs_diff_cost = if_else(category=="Traditional: forecast", 
                                                 pmax(cost - trad_q_cost,0), NA), #absolute positive
         pie_trad_total_cost = if_else(category=="Traditional: forecast", # this means, if it is remained forecasted, use the abs_diff_cost; if it is actuals, use total_cost
                                       trad_forecast_q_abs_diff_cost, cost),
         category_remained = if_else(category=="Traditional: forecast","Traditional: forecast remained" ,category),
         category_remained = factor(category_remained, levels=c("Traditional: forecast remained","Traditional: actuals","Co-financing obligation"))
  )|>
  group_by(iso3) |> 
  mutate(pie_trad_total_cost_normalized = # summed up to be 100%
           pie_trad_total_cost / sum(pie_trad_total_cost,na.rm=T) * 100)|>
  # funding sources percentage - country level (not antigen specific)  
  left_join(
    dat_master_long |>
      filter(category =="Traditional: actuals")|>
      group_by(funding_source_cat_original, iso3)|> 
      summarise(cost = sum(cost,na.rm=T),.groups="drop")|>
      group_by(iso3) |>
      summarise(
        q_cost_src_all = sum(cost),
        q_cost_src_MoH = sum(cost[grepl("MoH", funding_source_cat_original)]),
        share_src_MoH = round(q_cost_src_MoH / q_cost_src_all,2)*100,
        q_cost_src_UNICEF = sum(cost[grepl("UNICEF", funding_source_cat_original)]),
        share_src_UNICEF = round(q_cost_src_UNICEF / q_cost_src_all,2)*100,
        q_cost_src_Other = sum(cost[grepl("Other", funding_source_cat_original)]),
        share_src_Other = round(q_cost_src_Other / q_cost_src_all,2)*100,
        q_cost_src_WB = sum(cost[grepl("WB", funding_source_cat_original)]),
        share_src_WB = round(q_cost_src_WB / q_cost_src_all,2)*100,
        .groups = "drop"),
    by="iso3"
  )|>
  mutate(
    funding_src_text = paste(
      ifelse(share_src_MoH > 0, paste0(share_src_MoH, "% MoH"), NA),
      ifelse(share_src_UNICEF > 0, paste0(share_src_UNICEF, "% UNICEF"), NA),
      ifelse(share_src_WB > 0, paste0(share_src_WB, "% WB"), NA),
      ifelse(share_src_Other > 0, paste0(share_src_Other, "% Other"), NA),
      sep = ", ") %>%
      gsub(", NA", "", .) %>%    # remove na values and preceding commas
      gsub("NA, ", "", .) %>%    # Remove na values and following commas
      gsub(", NA,", ",", .),    # Handle any potential double commas
    funding_src_text = if_else(funding_src_text=="NA",NA,funding_src_text)
  )|>
  # funding sources percentage - country-antigen specific 
  left_join(
    dat_master_long|>
      filter(category =="Traditional: actuals",
             !is.na(vaccine_group_wbs),
             cost!=0)|>
      group_by(iso3, vaccine_group_wbs, funding_source_cat_original)|>
      summarise(cost=sum(cost,na.rm=T), .groups = "drop")|>
      pivot_wider(id_cols=c("iso3","vaccine_group_wbs"), 
                  values_from=cost, 
                  names_from=funding_source_cat_original,
                  names_prefix="q_cost_src_vaccine_"), #funding source country programme specific   
    by = c("iso3","vaccine_group_wbs")  
  )|>
  mutate( #keep only trad actual
    q_cost_src_vaccine_MoH = if_else(category!="Traditional: actuals",NA,q_cost_src_vaccine_MoH), 
    q_cost_src_vaccine_UNICEF = if_else(category!="Traditional: actuals",NA,q_cost_src_vaccine_UNICEF), 
    q_cost_src_vaccine_Other = if_else(category!="Traditional: actuals",NA,q_cost_src_vaccine_Other), 
    q_cost_src_vaccine_WB = if_else(category!="Traditional: actuals",NA,q_cost_src_vaccine_WB)
  )|>
  mutate(
    category_remained = if_else(category%in%c("Gavi-support","Co-financing paid"), category, category_remained)
  )

# (3) flat aggregated by country & antigen ####
#not joining cofi paid

 
dat_master_flat <- dat_trad_forecast |>
  select(iso3,country, vaccine_group_wbs,total_doses,total_cost)|>
  rename(forecast_doses=total_doses,
         forecast_cost_estimated=total_cost)|>
  group_by(iso3,country,vaccine_group_wbs)|>
  summarise(forecast_doses=sum(forecast_doses, na.rm=T),
            forecast_cost_estimated=sum(forecast_cost_estimated, na.rm=T),.groups="drop"
  )|>
  #actuals
  full_join(dat_trad_q|>   #not using leftjoin because many has actuals but lacks forecast
              select(country,iso3,vaccine_group_wbs,total_doses_all,total_shipped_value_all)|>
              rename( q_doses=total_doses_all, q_cost=total_shipped_value_all)|>
              group_by(iso3,vaccine_group_wbs)|> #need to aggregate
              summarise(q_doses = sum(q_doses,na.rm=T),
                        q_cost  = sum(q_cost,na.rm=T),.groups="drop") ,
            by=c("iso3","vaccine_group_wbs")
  )|>
  #SAP
  full_join(dat_SAP|>       #full join so that we know the whole picture
              select(country_name_ifs,iso3,vaccine_group_wbs,co_financing_doses,co_financing_usd,gavi_support_doses,gavi_support_usd)|>
              group_by(iso3,vaccine_group_wbs)|> #need to aggregate
              summarise(co_financing_doses = sum(co_financing_doses,na.rm=T),
                        co_financing_usd   = sum(co_financing_usd,na.rm=T),
                        gavi_support_doses = sum(gavi_support_doses,na.rm=T),
                        gavi_support_usd   = sum(gavi_support_usd,na.rm=T), .groups="drop"),
            by=c("iso3","vaccine_group_wbs")
  )|>
  ungroup()|>
  mutate(share_trad_q_over_all_doses = round(q_doses / 
                                               rowSums(cbind(q_doses,co_financing_doses,gavi_support_doses),na.rm =T),4),#not including forecast_doses
         share_trad_q_over_all_doses= if_else(share_trad_q_over_all_doses==1, NA,share_trad_q_over_all_doses), # if there's no gavi , then make it NA
         share_trad_forecast_over_all_doses = round(forecast_doses / 
                                                      rowSums(cbind(forecast_doses,co_financing_doses,gavi_support_doses),na.rm =T),4),
         share_trad_forecast_over_all_doses= if_else(share_trad_forecast_over_all_doses==1, NA,share_trad_forecast_over_all_doses), # if there's no gavi , then make it NA
         
         perc_q_over_forecast_doses = round(q_doses / forecast_doses,2),
         perc_q_over_forecast_doses= if_else(is.na(perc_q_over_forecast_doses), 0,perc_q_over_forecast_doses),
         difference_q_forecast_doses = forecast_doses - q_doses,
         difference_q_forecast_doses= if_else(is.na(difference_q_forecast_doses), 0,difference_q_forecast_doses))|>
  left_join(dat_grouping, by="iso3")|>
  mutate(country=country_name_ifs,
         country=if_else(iso3=="PCE","Pacific Islands",country))|> #cover the country name using grouping file, that may exist mismatch
  select(-country_name_ifs)|>
  arrange(country, vaccine_group_wbs,share_trad_forecast_over_all_doses)|>
  #shipment 
  left_join(dat_shipment_aggr,
            by = c("country","iso3","vaccine_group_wbs"))






#### SAVE ####

openxlsx::write.xlsx(
  list(
    "Missing Gavi countries" = table_trad_missing,
    
    "Vaccine unit price (seperate)" =temp_vaccine_unit_price_bygavinongavi_flat|>
      mutate(product_group = case_when( 
        product_group == "Measles" ~ "M", #unicef name ~ SAP name
        product_group == "Td" ~ "TD", 
        product_group == "DTP-HepB/Hib" ~ "PENTA", 
        product_group == "Rota" ~ "RV", # need check
        product_group == "Mening"~ "MEN",
        product_group == "Malaria" ~ "MAL",
        TRUE ~ product_group),
        vaccine_unit_price=round(vaccine_unit_price,2),
        vaccine_unit_price2=round(vaccine_unit_price2,2))
  ),
  file = "41. Analytics/traditional-vaccines-UNICEF/output tables/traditional_vaccine_unit_price_and_missing_country.xlsx",
  asTable = TRUE,
  showGridLines=FALSE,
  tableStyle = "TableStyleLight1"
)

rm(list=ls(pattern="temp"))

save.image("41. Analytics/traditional-vaccines-UNICEF/Master_datasets.RData")
