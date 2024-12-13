#### FILE DESCRIPTION ####
# Filename: 03-01_unicef-tradvax-fragile-subsetB.R
# Author: Hao-Kai Tseng
# Date: 2024-10-28
# Description: for 8 waiver/fragile countries, check % paid over forecast by country and also by antigen
# based on subset B 
# If look subset A, CAR Men will be the only difference to Subset B, which is questionable in whether Men means MenA 

rm(list=ls())
setwd("I:/")
load("41. Analytics/traditional-vaccines-UNICEF/Master_datasets.RData")

library(scales)
library(ggrepel)
#+++++++++++++++++++++
# 1. Prepare data ####
#+++++++++++++++++++++
temp_gavi_vaccines <- c("HPV","PCV","PENTA","MAL","RV","MENA","YF","MEN","EBL","MPOX","JEV","IPV") # reference : dat_SAP|> select(vaccine_group_wbs)|>unique()
  
  
temp_1 <- dat_master_long_agg |> 
           filter(iso3 %in% c("AFG","CAF","HTI","SOM","SSD","SYR","YEM","SDN") ,
                  category_remained %in% c("Traditional: forecast remained","Traditional: actuals"),
                  flag_gavi_funded=="Non-Gavi-funded", #important to exclude gavi country-programme
                  !is.na(country),        #excl. pacific island  
                  cofi_group_acro!="FS",
                  cost!=0,# to not include Ebola. Malaria if not procured  
                  ! vaccine_group_wbs %in% temp_gavi_vaccines)|> #!!! strictly no Gavi vaccines 
           anti_join(table_trad_missing, by= "iso3")|>
           group_by(iso3, vaccine_group_wbs) |> # also need to group by vaccine group,  
           mutate(pie_trad_total_cost_normalized = # summed up to be 100%
                    pie_trad_total_cost / sum(pie_trad_total_cost,na.rm=T) * 100)|>
           left_join(dat_birth|> 
              filter(year ==2023)|> # 2023 most update year 
              select(iso3,live_births),
            by = "iso3" )|>
           mutate(prop_doses_births = doses/live_births)
  

temp_text_1 <- dat_master_long_agg |>
  anti_join(table_trad_missing, by= "iso3")|>
  filter(iso3 %in% c("AFG","CAF","HTI","SOM","SSD","SYR","YEM","SDN"),
         category_remained %in% c("Traditional: forecast remained","Traditional: actuals"),
         flag_gavi_funded=="Non-Gavi-funded",
         !is.na(country),
         cofi_group_acro!="FS",
         cost!=0,
         ! vaccine_group_wbs %in% temp_gavi_vaccines)|>
  group_by(iso3, vaccine_group_wbs)|> #, country, funding_src_text, category_remained
  mutate(pie_trad_total_cost_normalized = #need to calculate it again here, different from the temp
           pie_trad_total_cost / sum(pie_trad_total_cost,na.rm=T) * 100)|>
  filter(category_remained == "Traditional: actuals")|>
  group_by(iso3, country, vaccine_group_wbs, category_remained, funding_src_text)|>
  summarise(pie_trad_total_cost_normalized=sum(pie_trad_total_cost_normalized, na.rm=T), 
            pie_trad_total_cost=sum(pie_trad_total_cost, na.rm=T),
            #recalculate finding source composition because this is a new subset            
            q_cost_src_vaccine_MoH = sum(q_cost_src_vaccine_MoH, na.rm=T),
            q_cost_src_vaccine_UNICEF = sum(q_cost_src_vaccine_UNICEF, na.rm=T),
            q_cost_src_vaccine_WB = sum(q_cost_src_vaccine_WB, na.rm=T),
            q_cost_src_vaccine_Other = sum(q_cost_src_vaccine_Other, na.rm=T),
            
            share_src_vaccine_MoH =  round(100*q_cost_src_vaccine_MoH / pie_trad_total_cost), 
            share_src_vaccine_UNICEF =  round(100*q_cost_src_vaccine_UNICEF / pie_trad_total_cost), 
            share_src_vaccine_WB =  round(100*q_cost_src_vaccine_WB / pie_trad_total_cost), 
            share_src_vaccine_Other =  round(100*q_cost_src_vaccine_Other/ pie_trad_total_cost), 
            
            text_vaccine =  str_c(vaccine_group_wbs, collapse = ", "),
            .groups="drop")|>
  mutate(
    funding_src_vaccine_text = paste(
      ifelse(share_src_vaccine_MoH > 0, paste0(share_src_vaccine_MoH, "% MoH"), NA),
      ifelse(share_src_vaccine_UNICEF > 0, paste0(share_src_vaccine_UNICEF, "% UNICEF"), NA),
      ifelse(share_src_vaccine_WB > 0, paste0(share_src_vaccine_WB, "% WB"), NA),
      ifelse(share_src_vaccine_Other > 0, paste0(share_src_vaccine_Other, "% Other"), NA),
      sep = ", ") %>%
      gsub(", NA", "", .) %>%    # remove na values and preceding commas
      gsub("NA, ", "", .) %>%    # Remove na values and following commas
      gsub(", NA,", ",", .),    # Handle any potential double commas
    funding_src_vaccine_text = if_else(funding_src_vaccine_text=="NA",NA,funding_src_vaccine_text)
  ) 

#++++++++++++++++++++++++++++++++++++++++++++++++
# 2. pie chart for % paid by country-antigen ####
#++++++++++++++++++++++++++++++++++++++++++++++++
plot_pie_fragile_paid <- ggplot(temp_1 , # use pie Normalize to 100% 
                               aes(x = "", 
                                   y = pie_trad_total_cost_normalized , #100% based
                                   fill = category_remained)) +
  geom_col() +
  coord_polar(theta = "y")+
  facet_grid( vaccine_group_wbs ~ country,
              switch = "y")+
  geom_text(data=temp_text_1,
                          aes(label = ifelse(category_remained == "Traditional: actuals", 
                               ifelse(pie_trad_total_cost < 1000, 
                                      paste0(round(pie_trad_total_cost_normalized), 
                                             "% paid\n($< 1K)",
                                             "\n",
                                             str_wrap(funding_src_vaccine_text, width = 11)),  
                                      paste0(round(pie_trad_total_cost_normalized),
                                             "% paid\n($",
                                             scales::comma(round(pie_trad_total_cost / 1000)), 
                                             "K)",
                                             "\n",
                                             str_wrap(funding_src_vaccine_text, width = 11)
                                             )), 
                               "")),  # Hide remaining figures of forecast
            position = position_stack(vjust = 0.5),  
            color = "black") +
  theme_void()+
  scale_fill_manual(values = c("Traditional: forecast remained" = "#3E9B6E",  
                               "Traditional: actuals" = "#FF7F24"))+
  labs(title="Percentage paid as of actual cost over forecast cost (US$) for traditional vaccines, fragile countries",fill="")+
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 18),
    legend.text = element_text(size = 22),
    strip.text.y = element_text(size = 14),
    strip.text.x = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    panel.spacing = unit(0, "lines"),  # spacing facets, the large the more the space are sacrificed 
    strip.background = element_blank(),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0))

print(plot_pie_fragile_paid)

#++++++++++++++++++++++++++++++
# 3. country-level summary ####
#++++++++++++++++++++++++++++++
# for manually making bar chart in power point
temp_3 <-  temp_1 |>
  filter(iso3 %in% c("AFG","CAF","HTI","SOM","SSD","SYR","YEM","SDN"),
         category%in%c("Traditional: forecast","Traditional: actuals"))|>
  as.data.frame()|>
  group_by(country, category)|>#
  summarise(cost=round(sum(cost)), .groups="drop")

#+++++++++++++++++++++++
# 4. funding source ####
#+++++++++++++++++++++++
# need recalculation due to the temp_text_1 funding_source_text represent whole data, not subset data
temp_4 <- temp_text_1 |>
  group_by(iso3, country)|>
  summarise(trad_q_cost = sum(pie_trad_total_cost,na.rm=T),
            q_cost_src_MoH = sum(q_cost_src_vaccine_MoH,na.rm=T), # name remove "vaccine"
            q_cost_src_UNICEF = sum(q_cost_src_vaccine_UNICEF,na.rm=T),
            q_cost_src_WB = sum(q_cost_src_vaccine_WB,na.rm=T),
            q_cost_src_Other = sum(q_cost_src_vaccine_Other,na.rm=T), .groups = "drop"
            )|>
  mutate(share_src_MoH = round(100*(q_cost_src_MoH / trad_q_cost)),
         share_src_UNICEF = round(100*(q_cost_src_UNICEF / trad_q_cost)),
         share_src_WB = round(100*(q_cost_src_WB / trad_q_cost)),
         share_src_Other = round(100*(q_cost_src_Other / trad_q_cost)),
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
         
         
         )

#+++++++++++++++++++++++++++++++++++++++++
# 5. stack bar chart for each country ####
#+++++++++++++++++++++++++++++++++++++++++

plot_FC_q_cost_by_antigen <- ggplot(temp_1 |> 
         filter(category == "Traditional: actuals")) + 
  geom_bar(aes(fill = vaccine_group_wbs, 
               y = cost, 
               x = country),
           position = "stack", stat = "identity") +
  geom_text(data = temp_3 |>
              filter(category == "Traditional: actuals")  , 
            aes(x = country, # labeling annual totals
                y = cost, 
                label = comma(cost)),
            position = position_stack(vjust = 1.05), 
            size = 5,
            hjust =0.5)+
  scale_fill_brewer(palette = "Set2") +
  labs(title="Traditional vaccine procured by antigen, F&C countries",fill="Vaccine",x="",y="Expenditure (US$)")+
  scale_y_continuous(labels = function(x) paste0(x / 1e6, "M"))+
  theme_minimal()+
  theme(
    legend.text = element_text(size = 22),  
    axis.text = element_text(size = 19),    
    axis.title = element_text(size = 19))  

print(plot_FC_q_cost_by_antigen)

#summary
(temp_1 |>
    ungroup() |>
    filter(category == "Traditional: actuals") |>
    summarise(
      cost_top_three_antigens = sum(if_else(vaccine_group_wbs %in% c("BCG","TD"), cost, 0, missing = 0)),
      total_cost = sum(cost, na.rm = TRUE),
      percentage = (cost_top_three_antigens / total_cost) * 100) )

#++++++++++++++++++++++++++++++
# 6. live birth assessment ####
#++++++++++++++++++++++++++++++
# line and dots

plot_FC_birth <- ggplot(temp_1|>
         filter(category =="Traditional: forecast")  , 
       aes(y = category, 
                   x = doses)) +
  geom_point() +
#  geom_text(aes(label = vaccine_group_wbs), vjust = -0.5, hjust = 0.5, size =6) +
  geom_label_repel(aes(label = vaccine_group_wbs), box.padding = 0.3, point.padding = 0.4, size =5)+
  geom_vline(aes(xintercept = live_births), linetype = "dashed", color = "#005CB9",size=0.9) +
  geom_text(aes(x = live_births, 
                label = paste0("Births: ",live_births)), vjust = -2.6, hjust = -0.1,color = "#005CB9", size = 4.5)+
  facet_grid(country ~.)+
  scale_x_continuous(breaks = seq(0,10000000, by = 1000000),labels = comma) +
  labs(title = "Number of forecasted traditional vaccine doses vs live births",
       subtitle = "Year of live birth surviving age one: 2023.",
       y = "Vaccines",
       x = "Number of doses or live births") +
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.title.x = element_text(size = 18),
        strip.text.y = element_text(size = 19, angle = 0))

print(plot_FC_birth)


#summaries on forecasts
temp_1|>
  filter(category =="Traditional: forecast")|>
  group_by(vaccine_group_wbs, category)|>
  summarise( median = round(median(prop_doses_births,na.rm=T),1),
             maximum = round(max(prop_doses_births,na.rm=T),1),
             count = sum(prop_doses_births > 1, na.rm = TRUE)
  )


#++++++++++
# SAVE ####
#++++++++++
ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/13_subsetB_pie_fragile_percentage_paid.png",
       plot=plot_pie_fragile_paid, width=17,dpi = 1000, height=11,bg = "white")

ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/13_subsetB_procured_cost_fragile_bycountryantigen.png",
       plot=plot_FC_q_cost_by_antigen, width=12,dpi = 500, height=8,bg = "white")

ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/13_subsetB_live_birth_doses_fragile.png",
       plot=plot_FC_birth, width=12,dpi = 500, height=8,bg = "white")


