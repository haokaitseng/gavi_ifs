#### FILE DESCRIPTION ####
# Filename: sap_fund_ids.R
# Author: Anthony Nguyen
# Date: Tue Nov 28 22:06:05 2023
# Description: Having a look at the different SAP Fund IDs


#### SETUP ####

setwd("Q:///00-data/SAP_ME2J_PO-per-project") # set this to the correct path on your system

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(skimr)) install.packages("skimr", repos = "http://cran.us.r-project.org")


options(scipen=999)

rm(list=ls())

#### DEFINE USER FUNCTIONS ####

id_latest <- function(directory_name, extension){
  #enter file extension suffix after "."
  list.files(path = directory_name,
             pattern = paste0(extension,"$"),
             full.names = TRUE) %>%
    pluck(which.max(file.mtime(.)))
}


#### READ DATA ####

raw_path <- id_latest("export-raw", extension="xlsx")

raw <-  read_excel(raw_path) |> 
  clean_names()

raw %>% 
  filter(budget_period > 2022) %>% distinct(fund)

raw %>% 
  filter(budget_period > 2022) %>% 
  filter(fund=="1000016") %>% View()


# `Fund` code >2022

#1000002 = CoFi RI
#1000007 = IPV
#1000001 = Gavi support RI
#1000036 = MICS
#1000010 = Malaria
#1000040 = C19
#1000019 = COVID19
#1000016 = EOS OCV/YF
