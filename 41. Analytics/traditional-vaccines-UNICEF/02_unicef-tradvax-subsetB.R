#### FILE DESCRIPTION ####
# Filename: 06_unicef-trad-vacc-pure-trad.R
# Author: Hao-Kai Tseng
# Date: 2024-10-25
# Description: using master data to see more pure traditional vaccines (not including gavi products)
# using strict subset B

rm(list=ls())
load("41. Analytics/traditional-vaccines-UNICEF/Master_datasets.RData")

#+++++++++++++++++++++
# 0. prepare data ####
#+++++++++++++++++++++
temp_gavi_vaccines <- c("HPV","PCV","PENTA","MAL","RV","MENA","YF","MEN","EBL","MPOX","JEV","IPV") # reference : dat_SAP|> select(vaccine_group_wbs)|>unique()

view_main_1 <- dat_master_long_agg |> 
  filter(category_remained %in% c("Traditional: forecast remained","Traditional: actuals"),
         flag_gavi_funded=="Non-Gavi-funded", #important to exclude gavi country-programme
         !is.na(country),        #excl. pacific island  
         cofi_group_acro!="FS",
         cost!=0,# to not include Ebola. Malaria if not procured  
         ! vaccine_group_wbs %in% temp_gavi_vaccines)|> #!!! strictly no Gavi vaccines 
  anti_join(table_trad_missing, by= "iso3")|>
  group_by(iso3) |> 
  mutate(pie_trad_total_cost_normalized = # summed up to be 100%
           pie_trad_total_cost / sum(pie_trad_total_cost, na.rm=T) * 100)

view_text_1 <- dat_master_long_agg |>
  anti_join(table_trad_missing, by= "iso3")|>
  filter(category_remained %in% c("Traditional: forecast remained","Traditional: actuals"),
         flag_gavi_funded=="Non-Gavi-funded",
         !is.na(country),
         cofi_group_acro!="FS",
         cost!=0,
         ! vaccine_group_wbs %in% temp_gavi_vaccines)|>
  group_by(iso3)|> #, country, funding_src_text, category_remained
  mutate(pie_trad_total_cost_normalized = #need to calculate it again here , different from the temp
           pie_trad_total_cost / sum(pie_trad_total_cost,na.rm=T) * 100)|>
  filter(category_remained == "Traditional: actuals")|>
  group_by(iso3, country, category_remained, funding_src_text)|>
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

# n of countries
setdiff(dat_grouping|>
          filter(cofi_group_acro!="FS")|>
          pull(country_name_ifs),
        unique(view_main_1$country))
#[1] "Solomon Isl." "Mozambique"   "Haiti"        "Nepal" 

# flat data for share
view_main_1_flat <- view_main_1 |>
  pivot_wider(id_cols = c("iso3","country","vaccine_group_wbs","cofi_group_acro","gavi_segment","flag_gavi_funded"), 
              values_from = c(doses, cost), 
              names_from = category,
              names_prefix ="")|>
  left_join(
    view_main_1|>
      filter(category=="Traditional: actuals")|>
      select(iso3,vaccine_group_wbs,q_cost_src_vaccine_MoH,q_cost_src_vaccine_UNICEF,q_cost_src_vaccine_WB,q_cost_src_vaccine_Other),
  by = c("iso3","vaccine_group_wbs")
  )|> 
  clean_names()|>
# funding source original ####
  left_join(  
    dat_master_long |>
    filter(category=="Traditional: actuals")|>
    select(iso3, vaccine_group_wbs, funding_source_name, funding_source_cat_original)|>
    unique()|>
    group_by(iso3, vaccine_group_wbs, funding_source_cat_original)|>
    summarise(
    funding_source_name =  str_c(funding_source_name, collapse = ", ")
    ,.groups="drop")|>
    pivot_wider(id_cols = c("iso3","vaccine_group_wbs"), 
                values_from = funding_source_name, 
                names_from = funding_source_cat_original,
                names_prefix ="Funding source name "),
    by=c("iso3","vaccine_group_wbs")
)|>
  left_join(dat_birth|>
              filter(year ==2023)|> # 2023 most update year 
              select(iso3,live_births),
    by = "iso3" 
  )

#+++++++++++++++++++++++++++++++++++++
# 1. individual country pie chart ####
#+++++++++++++++++++++++++++++++++++++
# (1) plotting ####
list_pie_each_country_share_paid_subsetB <- list()

for (i in unique(view_main_1$country)){   # using view_text_1 can filter out 0% paid Guinea
  plot <- ggplot(data = view_main_1 |>
                   filter(country == i ) ,
                 aes(x = "", 
                     y = pie_trad_total_cost_normalized, #100% based accounting each antigen, paid + unpaid = 100
                     fill = category_remained)) +
    geom_col() +
    coord_polar(theta = "y")+
    geom_text(
      data = view_text_1 |>
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
  
  list_pie_each_country_share_paid_subsetB[[i]] <- plot
}  

list_pie_each_country_share_paid_subsetB$Afghanistan
list_pie_each_country_share_paid_subsetB$`Congo DRC`

# all in one 
plot_pie_each_country_all_in_one_subsetB <- ggarrange(
  plotlist =list_pie_each_country_share_paid_subsetB,
  labels =  names(list_pie_each_country_share_paid_subsetB) ,
  ncol = 10, 
  nrow = 5,
  label.x = 0,
  common.legend =TRUE,  #share the same one !
  legend = "bottom")

plot_pie_each_country_all_in_one_subsetB <- annotate_figure(
  plot_pie_each_country_all_in_one_subsetB,
  top = text_grob("% paid in pure traditional channels (without Gavi-supported programmes and common Gavi-supported vaccines)", face = "bold", size = 20 )) 

print(plot_pie_each_country_all_in_one_subsetB)



# (2) summary as total view, different to "average" view ####
view_summary_text_1 <- list(
  #mean shares 
  view_text_1 |>  
    summarise(pie_trad_total_cost_normalized=mean(pie_trad_total_cost_normalized))|> round()
  ,
  view_text_1 |> 
    filter(pie_trad_total_cost_normalized >= 99.5)|> select(country)
  ,
  view_text_1 |> 
    filter(pie_trad_total_cost_normalized < 50)|> select(country)
  ,
  view_text_1|>
    summarise(perc_MoH = sum(q_cost_src_vaccine_MoH,na.rm=T)/sum(pie_trad_total_cost,na.rm = T),
              perc_UNICEF = sum(q_cost_src_vaccine_UNICEF,na.rm=T)/sum(pie_trad_total_cost,na.rm = T),
              perc_WB = sum(q_cost_src_vaccine_WB,na.rm=T)/sum(pie_trad_total_cost,na.rm = T),
              perc_Other = sum(q_cost_src_vaccine_Other,na.rm=T)/sum(pie_trad_total_cost,na.rm = T),.groups = "drop"
    )
)#|>  as.data.frame()


#+++++++++++++++++++++++++++++++++++++++++++++
# 2. Gross comparison cofi and temp ####
#+++++++++++++++++++++++++++++++++++++++++++++
view_main_2 <- dat_master_long_agg |> 
  anti_join(table_trad_missing, by= "iso3")|>
  filter(!is.na(country), #excl. pacific island  
         cofi_group_acro!="FS",
         flag_gavi_funded=="Non-Gavi-funded",
         category %in% c("Traditional: forecast",
                         "Traditional: actuals",
                         "Co-financing obligation"),
         ! vaccine_group_wbs %in% temp_gavi_vaccines)

view_main_2 |>
  group_by(category)|> #vaccine_group_wbs
  summarise( cost=sum(cost,na.rm=T))

#+++++++++++++++++++++++
# 3. antigen groups ####
#+++++++++++++++++++++++
plot_trad_vaccine_cost_category_global <- ggplot(  
  view_main_1 |>
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

(view_summary_text_3 <- view_main_1 |>
    ungroup() |>
    filter(category == "Traditional: actuals") |>
    summarise(
      cost_top_three_antigens = sum(if_else(vaccine_group_wbs %in% c("BCG", "TD", "MR"), cost, 0, missing = 0)),
      total_cost = sum(cost, na.rm = TRUE),
      percentage = (cost_top_three_antigens / total_cost) * 100) )

#++++++++++++++++++++++++++++++++++++
# 5. Top five procured countries ####
#++++++++++++++++++++++++++++++++++++
view_5 <- view_text_1 |> #all actuals
  arrange(-pie_trad_total_cost)|>
  mutate(rank = row_number(),
         country_new = if_else(rank > 5,"Others",country))|>
  group_by(country_new)|>
  summarise(cost=sum(pie_trad_total_cost,na.rm=T))|>
  mutate(percentage = cost / sum(cost) * 100,
         label = paste0(country_new, ": ", round(percentage, 1), "%"))|>
  arrange(desc(percentage))

ggplot(view_5, aes(x = "", y = percentage, fill = country_new)) +
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

#++++++++++++++++++++++++++++++++++++++++++++++++++
#6. share of trad over total vaccine financing #### 
#++++++++++++++++++++++++++++++++++++++++++++++++++
#Because I am doing subsetB, I should not using master_flat, which may include non-subset B
view_main_6 <- view_main_1 |>
  select(iso3, country, vaccine_group_wbs, category, cofi_group_acro, gavi_segment, flag_gavi_funded, doses, cost, q_cost_src_vaccine_MoH,q_cost_src_vaccine_UNICEF,q_cost_src_vaccine_WB,q_cost_src_vaccine_Other)|>
  rbind(
    dat_master_long|>
      filter(category %in% c("Co-financing obligation","Co-financing paid"),
             !is.na(country),
             cofi_group_acro!="FS"# This won't exclude mozambique, nepal, solomon that is not the 50 country that we focus 
             )|> #cost!=0
      select(iso3, country, vaccine_group_wbs, category, cofi_group_acro, gavi_segment, flag_gavi_funded, doses, cost)|>
      mutate(q_cost_src_vaccine_MoH=NA, q_cost_src_vaccine_UNICEF=NA, q_cost_src_vaccine_WB=NA, q_cost_src_vaccine_Other=NA)
  )|>
  anti_join(table_trad_missing, by= "iso3")|>
  group_by(iso3, country, category, cofi_group_acro, gavi_segment, flag_gavi_funded)|>
  summarise(cost = sum(cost, na.rm=T),
            q_cost_src_vaccine_MoH = sum(q_cost_src_vaccine_MoH, na.rm=T),
            q_cost_src_vaccine_UNICEF = sum(q_cost_src_vaccine_UNICEF, na.rm=T),
            q_cost_src_vaccine_WB = sum(q_cost_src_vaccine_WB, na.rm=T),
            q_cost_src_vaccine_Other = sum(q_cost_src_vaccine_Other, na.rm=T), .groups="drop")|>
#waiver zero out
    mutate(cost=if_else(iso3%in%c("SDN","AFG")&category=="Co-financing obligation",0,cost))|> # mannual zero out waivers
  group_by(iso3)|>
  mutate(share_forecast_cofiobli = if_else(category %in% c("Traditional: forecast", "Co-financing obligation"),
                                   round(100*cost / sum(cost[category %in% c("Traditional: forecast", "Co-financing obligation")]),1),
                                   NA_real_),
         share_tradpaid_cofipaid = if_else(category %in% c("Traditional: actuals", "Co-financing paid"),
                                   round(100*cost / sum(cost[category %in% c("Traditional: actuals", "Co-financing paid")]),1),
                                   NA_real_)  
         )|>
  mutate(share_forecast_cofiobli=if_else(share_forecast_cofiobli=="NaN",NA,share_forecast_cofiobli),
         share_tradpaid_cofipaid=if_else(share_tradpaid_cofipaid=="NaN",NA,share_tradpaid_cofipaid),
         share_trad_cofi = rowSums(across(c(share_forecast_cofiobli, share_tradpaid_cofipaid)), na.rm = TRUE),
         category_share =  if_else(category %in% c("Traditional: forecast", "Co-financing obligation"),"Forecast","Paid") ,
         gavi_segment = factor(gavi_segment, levels=c("High Impact","Fragile & Conflict","Core")),
         category_share = factor(category_share, levels=c("Forecast","Paid"))
         )

# global mean & median
view_main_6 |>
  filter(!country %in%c("Afghanistan","Sudan","Somalia","Syria","Yemen","Papua NG","Congo Rep.","Korea DPR"))|>
  #filter(share_forecast_cofiobli!=100)|>
  ungroup()|>
  filter(category%in%c("Traditional: forecast","Traditional: actuals"))|>
  group_by(category_share)|>
  summarise(
    share_forecast_cofiobli_mean = mean(share_trad_cofi, na.rm = TRUE),
    share_forecast_cofiobli_median = median(share_trad_cofi, na.rm = TRUE)
  )# guinea is na in paid because no subset B data

# high shares countries
view_6_list <- view_main_6 |>
  #filter(share_forecast_cofiobli!=100)|>
  ungroup()|>
  filter(category%in%c("Traditional: forecast","Traditional: actuals"))|>
  select(iso3, country, share_trad_cofi, category_share)|>
  pivot_wider(id_cols = c("iso3","country"), 
            values_from = share_trad_cofi, 
            names_from = category_share,
            names_prefix ="share_trad_cofi_")

view_main_1_flat <- view_main_1_flat |>
  left_join(view_6_list, by =c("iso3","country"))

# those paid traditional early, but paid gavi late
view_6_list_outstanding <- view_6_list |>
  filter(share_trad_cofi_Paid>70,
         share_trad_cofi_Forecast!=100)



# (1) forecast box plot ####
plot_box_share_forecast_cofiobli <- ggplot(view_main_6 |> 
           filter(category == "Traditional: forecast"), 
         aes(x =category_share,
             y = share_trad_cofi)) + #share_forecast_cofiobli also work
    geom_boxplot(width = 0.5) +
    geom_jitter(aes(color = gavi_segment), 
                width = 0.2, 
                alpha = 0.8,
                size =4.9) +
    scale_color_manual(values = c(
      "Fragile & Conflict" = "#FF7F24",
      "Core" = "grey",
      "High Impact" = "#005CB9"))+
    labs(x = "", color="",y = "Traditional forecast as % of total vaccine financing needs(%)",title="Box plot for the share of traditional vaccine forecast cost as % of total vaccine financing needs",subtitle = "Expeced total vaccine financing = Traditional forecast + Co-financing obligaiton") +
    theme_minimal()+
    theme(
      legend.position="bottom",
      legend.text = element_text(size = 27), 
      axis.text = element_text(size = 21),
      axis.text.x = element_text(size = 24),
      axis.title = element_text(size = 18), 
      axis.title.y  = element_text(size = 17)
     )

print(plot_box_share_forecast_cofiobli)

# (2) paid box plot ####
plot_box_share_tradpaid_cofipaid <- ggplot(
  view_main_6 |> 
     filter(category == "Traditional: actuals"), 
     aes(x =category_share,
         y = share_trad_cofi)) + #share_forecast_cofiobli also work
  geom_boxplot(width = 0.5) +
  geom_jitter(aes(color = gavi_segment), 
              width = 0.2, 
              alpha = 0.8,
              size =4.9) +
  scale_color_manual(values = c(
    "Fragile & Conflict" = "#FF7F24",
    "Core" = "grey",
    "High Impact" = "#005CB9"))+
  labs(x = "", color="",y = "Traditional procured as % of total vaccine financing paid(%)",title="Box plot for the share of traditional vaccine procured as % of total vaccine financing paid",subtitle = "Total vaccine financing paid = Traditional actuals + Co-financing paid") +
  theme_minimal()+
  theme(
    legend.position="bottom",
    legend.text = element_text(size = 27), 
    axis.text = element_text(size = 21),
    axis.text.x = element_text(size = 24),
    axis.title = element_text(size = 18), 
    axis.title.y  = element_text(size = 17)
  )

print(plot_box_share_tradpaid_cofipaid)

# (3)2in1 box plot ####
#!!excluding waiver countries
plot_box_share_2in1 <- ggplot(
  view_main_6 |> 
    filter(category %in%c("Traditional: actuals","Traditional: forecast"),
           !country %in%c("Afghanistan","Sudan","Somalia","Syria","Yemen","Papua NG","Congo Rep.","Korea DPR")), #excluding waiver countries
  aes(x =category_share,
      y = share_trad_cofi)) + #share_forecast_cofiobli also work
  geom_boxplot(width = 0.4) +
  geom_jitter(aes(color = gavi_segment), 
              width = 0.2, 
              alpha = 0.8,
              size =4.9) +
  # geom_line(aes(group = iso3,   #hide lines connecting two boxes
  #               color =  ifelse(iso3 %in% view_6_list_outstanding$iso3,
  #                               "Outstanding",
  #                               "Rest")
  #               ),
  #           size = 0.5, 
  #           alpha = 0.6,
  #           linetype="dashed")+
  scale_color_manual(values = c(
    "Fragile & Conflict" = "#FF7F24",
    "Core" = "grey",
    "High Impact" = "#005CB9",
    "Outstanding" = "red",
    "Rest" = "lightgrey"
    ),
    breaks = c("Fragile & Conflict", "High Impact","Core","Outstanding"))+ # hide line legend
  labs(x = "", color="",y = "Traditional vaccine as % of total vaccine financing (%)",title="Traditional vaccines in the total vaccine financing",subtitle = "Total vaccine financing = Traditional cost + Co-financing cost, subject to forecast(obligation) or paid\n Excluding: Afghanistan, Sudan, Somalia, Syria, Yemen, PNG, Congo, Korea DPR") +
  theme_minimal()+
  theme(
    legend.position="bottom",
    legend.text = element_text(size = 27), 
    axis.text = element_text(size = 21),
    axis.text.x = element_text(size = 24),
    axis.title = element_text(size = 18), 
    axis.title.y  = element_text(size = 17)
  )

print(plot_box_share_2in1)

#++++++++++++++++++++++++++++++++++++++
# 7. validate live birth and doses ####
#++++++++++++++++++++++++++++++++++++++
view_main_1_flat <- view_main_1_flat |>
  mutate(prop_actuals_births = doses_traditional_actuals/live_births, 
         prop_forecast_births = doses_traditional_forecast/live_births)

view_main_1_flat|>
 # filter(prop_actuals_births>1)|>
  group_by(vaccine_group_wbs)|>
  summarise( median = round(median(prop_actuals_births,na.rm=T),1),
             maximum = round(max(prop_actuals_births,na.rm=T),1),
             count = sum(prop_actuals_births > 1, na.rm = TRUE)
             )|>
  view()

#++++++++++++++++++++++++++++++++++
# 8.Funding source sankey plot ####
#++++++++++++++++++++++++++++++++++





#++++++++++
# save ####
#++++++++++
ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/12_subsetB_dodge_bar_strict_pure_trad_antigen_cost_forecast_actual.png",
       plot=plot_trad_vaccine_cost_category_global, width=14,dpi = 500, height=7,bg = "white")

ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/12_subsetB_individual_country_funding_sources_excludinggaviprogrammes_andcommongaviantigen.png",
       plot=plot_pie_each_country_all_in_one_subsetB, width=17,dpi = 1000, height=11,bg = "white")

ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/12_subsetB_boxplot_share_forecast_cofiobli.png",
       plot=plot_box_share_forecast_cofiobli, width=12,dpi = 500, height=10,bg = "white")

ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/12_subsetB_boxplot_share_tradpaid_cofipaid.png",
       plot=plot_box_share_tradpaid_cofipaid, width=12,dpi = 500, height=10,bg = "white")

ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/12_subsetB_boxplot_share_2in1_lines.png",
       plot=plot_box_share_2in1, width=12,dpi = 500, height=10,bg = "white")


# data to be shared
openxlsx::write.xlsx(
  list(
    "traditional subset B flat" = view_main_1_flat|>
      select(-c(flag_gavi_funded,prop_actuals_births,prop_forecast_births))|>
    rename(
      `vaccine group`=vaccine_group_wbs,
      `Co-financing phase`=cofi_group_acro,
      `Gavi segment`=gavi_segment,
      `doses traditional actuals`=doses_traditional_actuals,
      `doses traditional forecast`=doses_traditional_forecast,
      `cost traditional actuals`=cost_traditional_actuals,
      `cost traditional forecast`=cost_traditional_forecast,
      `cost traditional actuals MoH`=q_cost_src_vaccine_mo_h,
      `cost traditional actuals UNICEF`=q_cost_src_vaccine_unicef,
      `cost traditional actuals WB`=q_cost_src_vaccine_wb,
      `cost traditional actuals Other`=q_cost_src_vaccine_other,
      `Share of traditional vaccine procured as % of total vaccine financing paid`=share_trad_cofi_Paid,
      `Share of traditional vaccine forecast as % of total vaccine financing needs`=share_trad_cofi_Forecast
    ),
    #raw long unaggregated file
    
    "traditional subset B long" = dat_master_long|>
      filter(category%in%c("Traditional: forecast","Traditional: actuals"),
             flag_gavi_funded=="Non-Gavi-funded", #important to exclude gavi country-programme
             !is.na(country),        #excl. pacific island  
             cofi_group_acro!="FS",
             cost!=0 ,# to not include Ebola. Malaria if not procured  
             ! vaccine_group_wbs %in% temp_gavi_vaccines)|> #!!! strictly no Gavi vaccines 
      anti_join(table_trad_missing, by= "iso3")|>
      select(iso3,country,category,vaccine_group_wbs,doses,cost,funding_source_name,funding_source_cat_original,cofi_group_acro,gavi_segment)|>
      rename(
        `vaccine group`=vaccine_group_wbs,
        `Co-financing phase`=cofi_group_acro,
        `Gavi segment`=gavi_segment,
        `Funding source name`=funding_source_name,
        `Funding source category`=funding_source_cat_original
      )
    
    
    # long aggregated
    # "traditional subset B" = view_main_1|>
    #   select(iso3,country,vaccine_group_wbs,category,doses,cost,cofi_group,gavi_segment,flag_gavi_funded)|> #funding_src_text should not be used difrect because it is from master difrectly, should be calcualted again
    #   mutate(flag_common_gavi_antigen = if_else(vaccine_group_wbs %in% temp_gavi_vaccines, "Common Gavi-supported antigen","Not common Gavi-supported antigen" ))
    # rename(`Funding sources` = funding_src_text)#,
    # "Summary individual country"=temp_summary_text_1,
    # "Summary antigens"=temp_summary_text_3,
    # "top five" = temp_text_1

  ),
  file = paste0("41. Analytics/traditional-vaccines-UNICEF/output tables/table_traditional_vaccine_subsetB-",Sys.Date(),".xlsx"),
  asTable = TRUE,
  showGridLines=FALSE,
  tableStyle = "TableStyleLight1"
)
