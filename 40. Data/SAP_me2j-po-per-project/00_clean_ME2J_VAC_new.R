#### FILE DESCRIPTION ####
# Filename: clean_ME2J_VAC.R
# Author: Anthony Nguyen
# Date: Tue Nov 28 22:06:05 2023
# Description: This file cleans the ME2J export from SAP, adding ISO3c, Unit prices, 
# and total costs. This file keeps all vaccine programs that are tied to country-based fund centers, 
#including C19, OCV and EOS vaccines, plus MICs.


#### SETUP ####

setwd("I://40. Data/SAP_me2j-po-per-project") # set this to the correct path on your system

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(skimr)) install.packages("skimr", repos = "http://cran.us.r-project.org")


options(scipen=999)

rm(list=ls())

#### DEFINE USER FUNCTIONS ####

source("I://41. Analytics/.src/id_latest.R")

source("I://41. Analytics/.src/get_date.R")

#### READ DATA ####

#SAP Fund names. This mapping file is pulled from distinct pairs found in the SAC PFM 3.0 export file
fund_names <- readRDS("I://40. Data/RData/sap_fund-names.Rds")

#SAP Fund center names
funds_center_names <- readRDS("I://40. Data/RData/sap_funds-center-names.Rds") %>% select(-iso3) #better to take the iso3 from the WBS key

#Find latest SAP export
raw_path <- id_latest("export-raw", extension="xlsx")

#read SAP PO data
raw <-  read_excel(raw_path) |> 
  clean_names() %>% 
  filter(!is.na(purchasing_document)) %>% 
  mutate(document_date = ymd(document_date))
  
#### SET PARAMS ####

last_update <- get_date(raw_path)

#### CLEAN ####

skim(raw)

dat <- raw |> select(-c(
  purch_doc_category,
  purchasing_group,
  acct_assignment_cat,
  plant,
  order_unit,
  release_group,
  purch_organization,
  grant,
  wbs_element_2,
  req_tracking_number,
  network,
  activity,
  po_history_release_documentation,
  item_category,
  storage_location,
  validity_per_start,
  validity_period_end,
  created_on,
  targ_val_hdr,
  total_open_value,
  open_value,
  released_value,
  target_quantity,
  open_target_quantity,
  qty_released_to_date,
  no_of_positions
  ))

#gen cols
dat <- dat %>% 
  left_join(funds_center_names) %>%
  filter(!is.na(funds_center_name)) %>% #drop global vaccines with no country fund centers
  mutate(
    iso3=substr(wbs_element, 1, 3),
    vaccine_group_wbs = gsub(".*?-(.*?)-.*", "\\1", wbs_element),
    unit_price = net_price / price_unit,
    order_cost = order_quantity * unit_price,
    material_group_label = case_when(
      material_group=="9003" ~ "Vaccines",
      material_group=="9004" ~ "Devices",
      material_group=="9005" ~ "Freight",
      material_group=="9085" ~ "Cash support - partners",
      .default = "OTHER"
      ),
    deletion_indicator_label = case_when(
      deletion_indicator=="L" ~ "Deleted",
      deletion_indicator=="S" ~ "Blocked",
      .default = deletion_indicator
    ),
    programme_type = case_when(
      str_detect(wbs_element, "-HPV-M-VAC-FED") ~ "Campaign MAC FED",
      str_detect(wbs_element, "-HPV-M-") ~ "Campaign MAC",
      str_detect(wbs_element, "-BCU-") ~ "Campaign Big Catch-up",
      str_detect(wbs_element, "-C-FU-FER-") ~ "Campaign Follow-up FER",
      str_detect(wbs_element, "-C-FU") ~ "Campaign Follow-up",
      str_detect(wbs_element, "-C-CU") ~ "Campaign Catch-up",
      str_detect(wbs_element, "-C-") ~ "Campaign",
      str_detect(wbs_element, "-R-1&2-FER") ~ "Routine FER",
      str_detect(wbs_element, "-R-VAC-FED") ~ "Routine FED",
      str_detect(wbs_element, "-R-FER") ~ "Routine FER",
      str_detect(wbs_element, "-R-") ~ "Routine",
      str_detect(wbs_element, "-EOS-") ~ "Emergency Outbreak Support",
      .default = "OTHER"
    )
  )|>
  mutate( #to fix the duplicated Malaria that WBS_element cannot solely solve
    wbs_element_material = paste0(wbs_element,"-",material)
  )

#drop cash support
dat <- dat %>% filter(!(vaccine_group_wbs %in% c("PTG", "TCA", "HSS")))
dat <- dat %>% filter(material_group!="9085") #drop two POs to IDN for JSI vacc support in 2016

#add fund names
dat <- dat %>% left_join(fund_names)

#reorder
dat <- dat |> 
  select(
    wbs_element_material,
    purchasing_document,
    wbs_element,
    document_date,
    budget_period,
    funds_center,
    funds_center_name,
    iso3,
    vaccine_group_wbs,
    programme_type,
    deletion_indicator,
    deletion_indicator_label,
    fund,
    fund_name,
    material_group,
    material_group_label,
    material,
    short_text,
    order_quantity,
    net_price,
    price_unit,
    order_cost,
    unit_price,
    currency, 
    functional_area
    # everything()
  )


#### EXTRACT VACCINE PRICE TABLES ####

#unique material numbers and vaccines as per SAP
sapvax <- dat |> 
  filter(material_group=="9003") |> 
  filter(!(vaccine_group_wbs=="M" & material=="90030022")) |>
  filter(!(vaccine_group_wbs=="HPV" & material=="90030060")) |>
  distinct(material, vaccine_group_wbs, short_text) 

# #WAP according to SAP
# sapvaxwap <- dat |> 
#   filter(material_group=="9003") |> 
#   filter(!(vaccine_group_wbs=="M" & material=="90030022")) |> 
#   filter(!(vaccine_group_wbs=="HPV" & material=="90030060")) |> 
#   filter(unit_price < 100) |> 
#   group_by(budget_period, material, vaccine_group_wbs, short_text) |> 
#   summarise(
#     price_max = round(max(unit_price), 3),
#     price_min = round(min(unit_price),3),
#     price_avg = round(mean(unit_price),3),
#     # Calculate the sum of total values and sum of quantities
#     total_cost_sum=sum(order_cost),
#     total_quantity_sum=sum(order_quantity),
#     # Calculate the weighted average price
#     wap_sap = round(total_cost_sum / total_quantity_sum,3)
#   ) |> 
#   ungroup() |> 
#   select(-total_cost_sum, -total_quantity_sum) |> 
#   arrange(vaccine_group_wbs, short_text, budget_period)


#### SAVE ####

dat <- dat %>% arrange(iso3, budget_period, programme_type, vaccine_group_wbs, material_group)

saveRDS(mutate(dat, last_update=last_update), "I://40. Data/RData/sappos-vac.Rds")
saveRDS(mutate(sapvax, last_update=last_update), "I://40. Data/RData/sap_material.Rds")

# write_csv(mutate(dat, last_update=last_update), "csv/sappos.csv")
# write_csv(mutate(sapvax, last_update=last_update), "csv/sapvax.csv")
# write_csv(mutate(sapvaxwap, last_update=last_update), "csv/sapvaxwap.csv")

names(dat) <- str_replace_all(names(dat), "_", " ")

openxlsx::write.xlsx(
  list(
    "All SAP VAC POs" = dat,
    "SAP Material No" = sapvax
    # "SAP Vx WAP" = sapvaxwap
  ),
  file = paste0("SAP_Purchase-Orders_", last_update, ".xlsx"),
  asTable = TRUE,
  showGridLines=FALSE,
  tableStyle = "TableStyleLight1"
)
