#### FILE DESCRIPTION ####
# Filename: 01_unicef-tradvax-subsetA.R
# Author: Hao-Kai Tseng
# Date: 2024-10-23
# Description: using master data to see pure traditional vaccines (not funded by gavi)

rm(list=ls())
load("41. Analytics/traditional-vaccines-UNICEF/Master_datasets.RData")


#+++++++++++++++++++++
# 0. prepare data ####
#+++++++++++++++++++++
temp_main_1 <- dat_master_long_agg |> 
  filter(category_remained %in% c("Traditional: forecast remained","Traditional: actuals"),
         flag_gavi_funded=="Non-Gavi-funded", #important to exclude gavi country-programme
         !is.na(country),        #excl. pacific island  
         cofi_group_acro!="FS",
         cost!=0)|> # to not include Ebola. Malaria if not procured  
  anti_join(table_trad_missing, by= "iso3")|>
  group_by(iso3) |> 
  mutate(pie_trad_total_cost_normalized = # summed up to be 100%
           pie_trad_total_cost / sum(pie_trad_total_cost,na.rm=T) * 100)

temp_text_1 <- dat_master_long_agg |>
  anti_join(table_trad_missing, by= "iso3")|>
  filter(category_remained %in% c("Traditional: forecast remained","Traditional: actuals"),
         flag_gavi_funded=="Non-Gavi-funded",
         !is.na(country),
         cofi_group_acro!="FS",
         cost!=0)|>
  group_by(iso3)|> #, country, funding_src_text, category_remained
  mutate(pie_trad_total_cost_normalized = # need to calculate it again here , different from the temp
           pie_trad_total_cost / sum(pie_trad_total_cost,na.rm=T) * 100)|>
  filter(category_remained == "Traditional: actuals")|>
  group_by(iso3, country, category_remained)|> #funding_src_text
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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. pie for pure traditional % paid + funding sources at individual country level####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# (1) plotting ####
list_pie_each_country_share_paid <- list()

for (i in unique(temp_main_1$country)){
  plot <- ggplot(data = temp_main_1 |>
                  filter(country == i ) ,
                aes(x = "", 
                    y = pie_trad_total_cost_normalized, #100% based accounting each antigen, paid + unpaid = 100
                    fill = category_remained)) +
    geom_col() +
    coord_polar(theta = "y")+
    geom_text(
      data = temp_text_1 |>
        filter(country == i),
              aes(label = ifelse(category_remained == "Traditional: actuals",
                                 ifelse(pie_trad_total_cost < 1000,
                                        paste0(round(pie_trad_total_cost_normalized),
                                               "% paid\n($< 1K)\n for ", # "% paid\n($< 1M)\n",
                                               str_wrap(text_vaccine, width = 18),
                                               "\n",
                                               str_wrap(funding_src_vaccine_text, width = 20)),#exceeding 20 lengths go to next line; not using funding_src_text which is national level
                                        paste0(round(pie_trad_total_cost_normalized),
                                               "% paid\n($",
                                               scales::comma(round(pie_trad_total_cost / 1000)),
                                               "K)\n for ",
                                               str_wrap(text_vaccine, width = 18),
                                               "\n",
                                               str_wrap(funding_src_vaccine_text, width = 20))
                                 ),
                                 "")),  # Hide remaining forecast
              position = position_stack(vjust = 0), #0.5=center of the "actuals"
              color = "black",
              size=3.9) +
    theme_void()+
    scale_fill_manual(values = c("Traditional: forecast remained" = "#3E9B6E",  
                                 "Traditional: actuals" = "#FF7F24"))+
    labs(title="",fill="")+
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
  
  list_pie_each_country_share_paid[[i]] <- plot
}  

list_pie_each_country_share_paid$Afghanistan
list_pie_each_country_share_paid$`Congo DRC`


# all in one 
plot_pie_each_country_all_in_one <- ggarrange(
  plotlist =list_pie_each_country_share_paid,
  labels =  names(list_pie_each_country_share_paid) ,
  ncol = 10, 
  nrow = 5,
  label.x = 0,
  common.legend =TRUE,  #share the same one !
  legend = "bottom")

plot_pie_each_country_all_in_one <- annotate_figure(
  plot_pie_each_country_all_in_one,
  top = text_grob("% paid in pure traditional channels (without Gavi-supported programmes), excluding FS countries", face = "bold", size = 20 )) 

print(plot_pie_each_country_all_in_one)

# (2) summary as total view, different to "average" view ####
temp_summary_text_1 <- list(
  #mean shares 
  temp_text_1 |>  
    summarise(pie_trad_total_cost_normalized=mean(pie_trad_total_cost_normalized))|> round()
,
  temp_text_1 |> 
    filter(pie_trad_total_cost_normalized >= 99.5)|> select(country)
,
  temp_text_1 |> 
    filter(pie_trad_total_cost_normalized < 50)|> select(country)
,
  temp_text_1|>
    summarise(perc_MoH = sum(q_cost_src_vaccine_MoH,na.rm=T)/sum(pie_trad_total_cost,na.rm = T),
              perc_UNICEF = sum(q_cost_src_vaccine_UNICEF,na.rm=T)/sum(pie_trad_total_cost,na.rm = T),
              perc_WB = sum(q_cost_src_vaccine_WB,na.rm=T)/sum(pie_trad_total_cost,na.rm = T),
              perc_Other = sum(q_cost_src_vaccine_Other,na.rm=T)/sum(pie_trad_total_cost,na.rm = T),.groups = "drop"
              )
)#|>  as.data.frame()


#+++++++++++++++++++++++++++++++
# 2. grand total comparison cofi and temp ####
#+++++++++++++++++++++++++++++++
temp_main_2 <- dat_master_long_agg |>
  anti_join(table_trad_missing, by= "iso3")|>
  filter(!is.na(country), #excl. pacific island  
         cofi_group_acro!="FS",
         flag_gavi_funded=="Non-Gavi-funded",
         category %in% c("Traditional: forecast",
                         "Traditional: actuals",
                         "Co-financing obligation"))

temp_main_2 |>
  group_by(category)|> #vaccine_group_wbs
  summarise( cost=sum(cost,na.rm=T))



#+++++++++++++++++++++++++++++++
# 3. antigen groups ####
#+++++++++++++++++++++++
plot_trad_vaccine_cost_category_global <- ggplot(  
  temp_main_1 |>
    group_by(vaccine_group_wbs, category)|>
    summarise(cost=sum(cost,na.rm=T),.groups="drop")|>
    filter(cost!=0)|>
    arrange(vaccine_group_wbs)|>
    mutate(vaccine_group_wbs = fct_reorder(vaccine_group_wbs,- cost, .fun = sum))
) + 
  geom_bar(aes(fill=category, 
               y=cost,
               x=vaccine_group_wbs),
           position = position_dodge2(width = 0.9, preserve = "single"), 
           stat="identity")+
  geom_text(aes(x=vaccine_group_wbs,
                y=cost,
                label = if_else(cost / 1e6 < 1 & cost / 1e6 > 0, "<1",
                                scales::comma(cost / 1e6, accuracy = 1))), 
            position = position_dodge2(width = 0.9, preserve = "single"),  
            vjust=-0.5, hjust=0.5, size = 5.5) +
  scale_fill_manual(values = c("Traditional: actuals" = "#FF7F24",  # Customize color for Traditional forecast
                               "Traditional: forecast" = "#3E9B6E"))+
  labs(title = "Total costs by antigen",subtitle = "Only Non-Gavi-funded programmes, excluding FS countries",x="",y="Costs in Million (US$)",fill="")+
  theme_minimal()+
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=18),
        legend.text = element_text(size = 25),
        legend.position = "bottom",
        axis.title = element_text(size = 19)
        )+
  scale_y_continuous(labels = function(x) scales::comma(x / 1e6),
                     breaks = c(5e6, 10e6, 15e6, 20e6, 25e6)) 
  
print(plot_trad_vaccine_cost_category_global)

# summary top three
(temp_summary_text_3 <- temp_main_1 |>
  ungroup() |>
  filter(category == "Traditional: actuals") |>
  summarise(
    cost_top_three_antigens = sum(if_else(vaccine_group_wbs %in% c("BCG", "TD", "MR"), cost, 0, missing = 0)),
    total_cost = sum(cost, na.rm = TRUE),
    percentage = (cost_top_three_antigens / total_cost) * 100) )

#++++++++++++++++++++++++++++++++++++
# 5. Top five procured countries ####
#++++++++++++++++++++++++++++++++++++
temp_5 <- temp_text_1 |> #all actuals
  arrange(-pie_trad_total_cost)|>
  mutate(rank = row_number(),
         country_new = if_else(rank>5,"Others",country))|>
  group_by(country_new)|>
  summarise(cost=sum(pie_trad_total_cost,na.rm=T))|>
  mutate(percentage = cost / sum(cost) * 100,
         label = paste0(country_new, ": ", round(percentage, 1), "%"))|>
  arrange(desc(percentage))

ggplot(temp_5, aes(x = "", y = percentage, fill = country_new)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Cost Distribution by Country", 
       fill = "Country") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(aes(label = label, 
                y = cumsum(percentage) - percentage / 2),  # Position in the middle of each segment
            nudge_x = 0,  # Increase this value to move labels further outward
            size = 5, 
            color = "black")




#+++++++++++++++++++++++
# 6. Funding source ####
#+++++++++++++++++++++++





#++++++++++
# save ####
#++++++++++
ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/10_individual_country_funding_sources_excludinggaviprogrammes.png",
       plot=plot_pie_each_country_all_in_one, width=17,dpi = 1000, height=11,bg = "white")

ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/11_dodge_bar_pure_trad_antigen_cost_forecast_actual.png",
       plot=plot_trad_vaccine_cost_category_global, width=14,dpi = 500, height=7,bg = "white")




openxlsx::write.xlsx(
  list(
    "traditional, non-Gavi-funded" = temp_main_1|>
      select(iso3,country,vaccine_group_wbs,category,doses,cost,cofi_group,gavi_segment,flag_gavi_funded,funding_src_text)|>
      rename(`Funding sources` = funding_src_text),
    "Summary individual country"=temp_summary_text_1,
    "Summary antigens"=temp_summary_text_3,
    "top five" = temp_text_1
    
    ),
  file = "41. Analytics/traditional-vaccines-UNICEF/output tables/table_traditional_vaccine_non_gavi_funded_programmes.xlsx",
  asTable = TRUE,
  showGridLines=FALSE,
  tableStyle = "TableStyleLight1"
)
