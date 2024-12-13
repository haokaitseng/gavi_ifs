#### FILE DESCRIPTION ####
# Filename: 05_unicef-trad-vacc-master-data.R
# Author: Hao-Kai Tseng
# Date: 2024-10-21
# Description: a long and flat file accomodating all purporse for forecast, actuals, cofi, gavi, funding source, label whether supported by Gavi

#++++++++++++++++++++++++++++++++++++++++++++++
#### DATA: long master data for everything ####
#++++++++++++++++++++++++++++++++++++++++++++++
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
  

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### DATA: flat master data for everything aggregated by country, antigen ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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


# save ####
#save.image("/41. Analytics/traditional-vaccines-UNICEF/output tables/data_master_longflat.RData")


#check number of programmes for nga ####
# cofi requiered 
test <- dat_master_long_agg|>
  filter(category=="Co-financing obligation",
         cost>0,
         !is.na(cost))|> 
  select(iso3,country,vaccine_group_wbs,cofi_group_acro,flag_gavi_funded)|>
  filter(!is.na(vaccine_group_wbs))|># , category !="Gavi-support"
  unique()

test2 <- test|>
  group_by(iso3) %>%
  summarise(vaccine_count = n(), .groups = "drop")

test %>%
  filter(cofi_group_acro == "AT",
         iso3!="NGA"
         #,flag_gavi_funded=="Gavi-funded"
         ) %>%          # Adjust to your column name for identifying AT countries
  group_by(country) %>%                   # Group by country to get program count per country
  summarise(program_count = n()) %>%      # Count the number of programs per country
  summarise(mean_programs = mean(program_count)) 

# number of trad forecasted
test_trad <- dat_master_long_agg|>
  filter(category=="Traditional: forecast",#Traditional: actuals
         cost>0,
         !is.na(cost),
         flag_gavi_funded=="Non-Gavi-funded")|> 
  select(iso3,country,vaccine_group_wbs,cofi_group_acro,flag_gavi_funded)|>
  filter(!is.na(vaccine_group_wbs))|># , category !="Gavi-support"
  unique()

test_trad %>%
  filter(cofi_group_acro == "PT",#PT , AT
         iso3!="NGA"
         ,flag_gavi_funded=="Non-Gavi-funded"
  ) %>%          # Adjust to your column name for identifying AT countries
  group_by(country) %>%                   # Group by country to get program count per country
  summarise(program_count = n()) %>%      # Count the number of programs per country
  summarise(mean_programs = mean(program_count)) 




# number of trad + COfi 2024
test_combine <-rbind(test,test_trad)

test_combine|>
   filter(cofi_group_acro == "AT",#PT , AT
       iso3!="NGA"
       #,flag_gavi_funded=="Gavi-funded"
) %>%          # Adjust to your column name for identifying AT countries
  group_by(country) %>%                   # Group by country to get program count per country
  summarise(program_count = n()) %>%      # Count the number of programs per country
  summarise(mean_programs = mean(program_count)) 

# for 2025

dat_SAP_2025 <- read_excel(paste0(getwd(),"/40. Data/SAP_me2j-po-per-project/SAP_Purchase-Orders_FLAT_2024-10-30.xlsx")) |> #FLAT_2024-10-11
  clean_names() |>
  mutate(budget_period=if_else(iso3%in%c("PAK","TAZ","ETH","KEN"), as.integer(budget_period)+1, as.integer(budget_period)))|>
  filter(budget_period==2025)

test_2025 <- dat_SAP_2025 |> 
  filter(programme_type=="Routine")|>
  group_by(iso3,vaccine_group_wbs)|> # to remove the effect of different programmes for different type
  summarise(co_financing_usd= sum(co_financing_usd,na.rm=T),
            gavi_support_usd=sum(gavi_support_usd,na.rm=T),.groups="drop")|>
  left_join(dat_grouping|> select(iso3,country_name_ifs, cofi_group_acro), by ="iso3")|>
  
  
  #filter(vaccine_group_wbs=="IPV", iso3!="NGA", cofi_group_acro != "FS", cofi_group_acro=="PT")|>  #for IPV only  # filter(cofi_group_acro == "PT", # PT , AT
  filter(co_financing_usd!=0,
         iso3!="NGA",
         !is.na(cofi_group_acro),
         cofi_group_acro!="FS",
         vaccine_group_wbs!="IPV",
         cofi_group_acro=="AT"  # at or pt here
         ) %>%          
  group_by(country_name_ifs) %>%                   # Group by country to get program count per country
  summarise(program_count = n()) %>%      # Count the number of programs per country
  summarise(mean_programs = mean(program_count)) 




# assessing shipment ####
temp <- dat_master_flat |>
  select(country, vaccine_group_wbs, forecast_doses, q_doses, co_financing_doses,gavi_support_doses,doses_ship_delivered,doses_ship_forecast,doses_ship_forecast_nextyr)

temp|> 
  filter(forecast_doses ==doses_ship_forecast)|>
  view()
