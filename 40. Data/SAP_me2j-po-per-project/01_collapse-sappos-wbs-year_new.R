#### FILE DESCRIPTION ####
# Filename: collapse-sappos-wbs.R
# Author: Anthony Nguyen
# Date: 2024-02-10
# # Description: this script takes the SAP POs all VAC file, and collapses it by WBS Key by Year,
# to show Country Procurement Cost, Gavi Procurement Cost and breakdowns. 
# Does not includes Deleted or Blocked POs.  Drops POs from before 2021.
# UNICEF presentation codes currently only complete for co-finacned programmes
# DROP COVAX AMC/COVAX HB/C19 programme doses
# DROP MICS doses
# DROP India doses


#### SETUP ####

setwd("I://40. Data/SAP_me2j-po-per-project") # set this to the correct path on your system

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")

options(scipen=999)

rm(list=ls())

#### DEFINE USER FUNCTIONS ####

#### READ DATA ####
sappos <- readRDS("I://40. Data/RData/sappos-vac.Rds") %>% clean_names()

#this file is used to bring in UNICEF presentation codes that are needed for the CoFi monitoring table
presentations <- read_excel("I://40. Data/IFS_mapping-files/vaccine-product-presentation.xlsx", sheet="vaccines")

# excel_sheets("Updating Salesforce/Bulk data upload to Salesforce/Load File Co-financing 2023/Load File Cofinancing 2023 preparation.xlsx")

# vaccines <- read_excel("Load File Co-financing 2023/Load File Cofinancing 2023 preparation.xlsx", 
#                        sheet = "vaccines")
# 
# country_names <- read_excel("Load File Co-financing 2023/Load File Cofinancing 2023 preparation.xlsx", 
#                        sheet = "sf_country_names")
# 
# sf_allvax <- read_excel("Load File Co-financing 2023/Load File Cofinancing 2023 preparation.xlsx", 
#                                     sheet = "All Vaccine List_aN") %>% clean_names()
# 
# load2023 <- read_excel("Load File Co-financing 2023/Load File Cofinancing 2023 preparation.xlsx", 
#                        sheet = "2023 Cofinancing")


#### SET PARAMS ####

last_update <- unique(sappos$last_update)

drop_funds <- c(
  # "1000001", #GAVI Portfolio Management projects     
  # "1000003", # Advance Market Commitment for PCV      
  # "1000007", #IPV projects                           
  # "1000002", # Co-financing by countries              
  "1000019", #COVAX AMC countries                    
  "1000040", # C-19 Programme AMC                     
  # "1000010", #Malaria                                
  # "1000016", # FOC/EOS/Vaccine pre-payments           
  "1000036", #MICs Country specific tools & Fragility
  "1000012", # India MR, PCV, Rota, HPV projects      
  "1000013", #India IPV project                      
  "1000031" # COVAX Humanitarian buffer 
)

#### GET TOTALS CoFi & Gavi ####

sappos2 <- sappos %>% 
  filter(is.na(deletion_indicator) | deletion_indicator=="S") %>% #keep active
  filter(budget_period >= "2021") %>%  #drop entries before SAP integration in 2021
  filter(!(fund %in% drop_funds)) %>% #COVAX/C19, MICs, India
  select(
    wbs_element_material,
    purchasing_document,
    wbs_element,
    document_date,
    # deletion_indicator,
    # deletion_indicator_label,
    iso3,
    funds_center,
    funds_center_name,
    budget_period,
    vaccine_group_wbs,
    programme_type,
    fund, fund_name,
    material_group, material_group_label,
    short_text,
    order_quantity,
    order_cost,
    unit_price,
    last_update
  ) 

#remove the PAHO extension from HTI PCV wbs element
sappos2 <- sappos2 %>%
  mutate(
    wbs_element = str_remove(wbs_element, "-PAHO"),
    wbs_element = str_remove(wbs_element, "-MOH")
    )|>
  mutate(
    wbs_element_material = str_remove(wbs_element_material, "-PAHO"),
    wbs_element_material = str_remove(wbs_element_material, "-MOH")
  )

#add presentation codes 
sappos2 <- left_join(sappos2, select(presentations, short_text=sap_short_text, 
                                     presentation=unicef_approved_presentation)) %>% 
  mutate(product_presentation = if_else(material_group=="9003", short_text, NA))

#fill presentation codes, create dose column to collapse on:
    #Material group 9003 is vaccines, the other are freight and devices.  
    #Zero out the order quantities in the non-vaccine categories, so that when we collapse 
    #on order quantity, we are only left with the number of vaccine doses.
sappos2 <- sappos2 %>% 
    group_by(wbs_element_material, wbs_element, budget_period, fund_name) %>% 
    fill(c(presentation, product_presentation), .direction = "updown") %>% 
    ungroup() %>% 
    mutate(doses = if_else(material_group=="9003", order_quantity, 0))

#### Country contribution table ####

#get cofi usd and dose totals
country <- sappos2 %>% 
  filter(fund=="1000002") %>% 
  group_by( wbs_element, iso3, budget_period, vaccine_group_wbs, product_presentation,  #uniquely identified by product_presentation
    programme_type
  ) |> 
  summarise(
    co_financing_usd = sum(order_cost), #this includes freight/devices
    co_financing_doses = sum(doses) #this is just the doses
  ) |> 
  ungroup() |> 
  arrange(iso3, vaccine_group_wbs)

#add vaccine product_ext information
country_product <- sappos2 %>% 
  filter(fund=="1000002" & material_group=="9003") %>% 
  distinct(
    wbs_element_material, wbs_element, iso3, budget_period, vaccine_group_wbs, short_text, presentation,  #can't include doc date because they can be different per wbs_element (i.e. vaccine PO and freight PO document date different)
    programme_type
  ) %>% 
  rename(product_presentation=short_text)

#get country cost breakdown
country_breakdown <- sappos2 %>% 
  filter(fund=="1000002") %>% 
  mutate(short_text = if_else(material_group=="9003", "Country Vaccine cost", short_text)) %>% 
  group_by(
    wbs_element_material, wbs_element, iso3, budget_period, vaccine_group_wbs, product_presentation, presentation, 
    programme_type, short_text
  ) |> 
  summarise(
    cost = sum(order_cost) 
  ) |> 
  ungroup() |> 
  arrange(iso3, vaccine_group_wbs) %>% 
  pivot_wider(
    names_from = short_text,
    values_from = cost
  ) %>% 
  relocate(`Country Vaccine cost`, .after=programme_type)

names(country_breakdown)[which(names(country_breakdown)=="Freight  costs - doses"):which(names(country_breakdown)=="Syringe, Re-constitution")] <- 
  paste0("Country ", names(country_breakdown)[which(names(country_breakdown)=="Freight  costs - doses"):which(names(country_breakdown)=="Syringe, Re-constitution")])#equals to [10:14]

#### Gavi contribution table ####

#gavi support usd and doses
gavi <- sappos2 %>% 
  filter(fund!="1000002") %>% 
  group_by(
    wbs_element_material, wbs_element, iso3, budget_period, vaccine_group_wbs, product_presentation,  #uniquely identified by product_presentation
    programme_type
  ) |> 
  summarise(
    gavi_support_usd = sum(order_cost), 
    gavi_support_doses = sum(doses)
  ) |> 
  ungroup() |> 
  arrange(iso3, vaccine_group_wbs)

#add vaccine product_ext information
gavi_product <- sappos2 %>% 
  filter(fund!="1000002" & material_group=="9003") %>% 
  distinct(
    wbs_element_material, wbs_element, iso3, budget_period, vaccine_group_wbs, short_text, presentation,  
    programme_type
  ) %>% 
  rename(product_presentation=short_text)

#get gavi cost breakdown
gavi_breakdown <- sappos2 %>%
  filter(fund!="1000002") %>%
  mutate(short_text = if_else(material_group=="9003", "Gavi Vaccine cost", short_text)) %>%
  group_by(
    wbs_element_material, wbs_element, iso3, budget_period, vaccine_group_wbs, product_presentation, presentation, 
    programme_type, short_text
  ) |>
  summarise(
    cost = sum(order_cost)
  ) |>
  ungroup() |>
  arrange(iso3, vaccine_group_wbs) %>%
  pivot_wider(
    names_from = short_text,
    values_from = cost
  ) %>% 
  relocate(`Gavi Vaccine cost`, .after=programme_type)

names(gavi_breakdown)[which(names(gavi_breakdown)=="Syringe, A-D 0.5ml"):which(names(gavi_breakdown)=="Freight costs - doses Donation In-kind")] <- 
  paste0("Gavi ", names(gavi_breakdown)[which(names(gavi_breakdown)=="Syringe, A-D 0.5ml"):which(names(gavi_breakdown)=="Freight costs - doses Donation In-kind")])#equals to 10:16

#### JOIN TABLES ####

# prog_list_names <- bulk2024country %>% 
#   filter(material_group=="9003") %>% 
#   distinct(wbs_element, short_text)

country_fullbreakdown <- left_join(country, country_product) %>% relocate(presentation, .after="product_presentation")# the console shows it is joining by wbs_element_material et al - correct
country_fullbreakdown <- left_join(country_fullbreakdown, country_breakdown)

gavi_fullbreakdown <- left_join(gavi, gavi_product) %>% relocate(presentation, .after="product_presentation")
gavi_fullbreakdown <- left_join(gavi_fullbreakdown, gavi_breakdown)

sapflat <- full_join(country_fullbreakdown, gavi_fullbreakdown)


#gen total cost, total doses and cofsh
sapflat <- sapflat %>% 
  rowwise() %>% 
  mutate(
    total_doses = sum(co_financing_doses, gavi_support_doses, na.rm=T), 
    total_cost = sum(co_financing_usd, gavi_support_usd, na.rm=T),
    co_financing_share = co_financing_usd/total_cost
    ) %>% 
  ungroup()


#### SAVE ####

sapflat <- sapflat %>% arrange(iso3, budget_period, programme_type, vaccine_group_wbs)

saveRDS(mutate(sapflat, last_update=last_update), "I://40. Data/RData/sapflat.Rds")

names(sapflat) <- str_replace_all(names(sapflat), "_", " ")

openxlsx::write.xlsx(
  list(
    "SAP VAC POs 2021+" = sapflat
    
    # "SAP Vx WAP" = sapvaxwap
  ),
  file = paste0("SAP_Purchase-Orders_FLAT_", last_update, ".xlsx"),
  asTable = TRUE,
  showGridLines=FALSE,
  tableStyle = "TableStyleLight1"
)


