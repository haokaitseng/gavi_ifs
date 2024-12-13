#### FILE DESCRIPTION ####
# Filename: 03-02_unicef-tradvax-fragile.R
# Author: Hao-Kai Tseng
# Date: 2024-12-08
# Description: for three groups of fragile countries
# gp 1 (chronically fragile countries): Fragile & conflict segment: Afghanistan, Central African Republic, Chad, Haiti, Mali, Niger, Papua New Guinea, Somalia, South Sudan, Sudan, Syrian Arab Republic and Yemen ; Core segment: Myanmar, Cameroon, Burkina Faso
# gp 2 (unicef selected ):("AFG","CAF","HTI","SOM","SSD","SYR","YEM","SDN")  and Haiti
# gp 3 waiver countreis: ("AFG","SOM","SSD","SYR","YEM","SDN")
# This is based on subset B 
# If it 's subsetA, there would be HPV for PNG; MEN for BFA, CAR,MALi;PCV for PNG,Penta forPNG; Td for BFA and Cameroun

rm(list=ls())
load("41. Analytics/traditional-vaccines-UNICEF/Master_datasets.RData")
library(scales)
library(ggrepel)
#+++++++++++++++++++++
# 1. Prepare data ####
#+++++++++++++++++++++
#gp3 is a subset of gp2, gp2 is a subset of gp1
iso3_gp1 <- c("AFG","CAF","TCD","HTI","MLI","NER","PNG","SOM","SSD","SDN","SYR","YEM","MMR","CMR","BFA") # and Haiti missing due to PAHO
iso3_gp2 <- c("AFG","CAF","HTI","SOM","SSD","SYR","YEM","SDN") # and Haiti missing due to PAHO
iso3_gp3 <- c("AFG","SOM","SSD","SYR","YEM","SDN")

temp_gavi_vaccines <- c("HPV","PCV","PENTA","MAL","RV","MENA","YF","MEN","EBL","MPOX","JEV","IPV") # reference : dat_SAP|> select(vaccine_group_wbs)|>unique()


temp_1 <- dat_master_long_agg |> 
           filter(iso3 %in% iso3_gp1, # gp1 set as the mother data frame 
                  category_remained %in% c("Traditional: forecast remained","Traditional: actuals"),
                  flag_gavi_funded=="Non-Gavi-funded", #important to exclude gavi country-programme
                  !is.na(country),        #excl. pacific island  
                  cofi_group_acro!="FS",
                  cost!=0,# to not include Ebola. Malaria if not procured  
                  ! vaccine_group_wbs %in% temp_gavi_vaccines)|> 
           anti_join(table_trad_missing, by= "iso3")|>
           group_by(iso3, vaccine_group_wbs) |> # also need to group by vaccine group,  
           mutate(pie_trad_total_cost_normalized = # summed up to be 100%
                    pie_trad_total_cost / sum(pie_trad_total_cost,na.rm=T) * 100)|>
           mutate(flag_group = ifelse(iso3%in%iso3_gp3,"Group 3", ifelse(iso3%in%iso3_gp2, "Group 2 & 3", "Group 1, 2 & 3")   ),
                  country = factor(country, levels=c("Afghanistan","Somalia","South Sudan","Syria","Yemen","Sudan", #gp3
                                                     "Haiti","CAR",
                                                     "Burkina Faso","Cameroun","Mali","Myanmar","Niger","Papua NG","Chad"))
                  )|>
           left_join(dat_birth|> 
              filter(year ==2023)|> # 2023 most update year 
              select(iso3,live_births),
              by = "iso3" )|>
           mutate(prop_doses_births = doses/live_births)
  

temp_text_1 <- dat_master_long_agg |>
  anti_join(table_trad_missing, by= "iso3")|>
  filter(iso3 %in% iso3_gp1, 
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
  )|>
  mutate(flag_group = ifelse(iso3%in%iso3_gp3,"Group 3", ifelse(iso3%in%iso3_gp2, "Group 2 & 3", "Group 1, 2 & 3")   ))





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

#summary
temp_1 |>
  ungroup()|>
  filter(category=="Traditional: actuals")|>
  summarise(mean(pie_trad_total_cost_normalized))


# 3. country-level total costs ####
temp_3 <-  temp_1 |>
  filter(category%in%c("Traditional: forecast","Traditional: actuals"))|>
  as.data.frame()|>
  group_by(country, category, flag_group)|>#
  summarise(cost=round(sum(cost)), .groups="drop")


# 4. funding source ####
# need recalculation due to the temp_text_1 funding_source_text represent whole data, not subset data
temp_4 <- temp_text_1 |>
  group_by(iso3, country, flag_group)|>
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


temp_4 |>
  filter(iso3 %in% iso3_gp3)|>
  summarise(share_src_MoH = mean(share_src_MoH),
            share_src_UNICEF = mean(share_src_UNICEF),
            share_src_WB = mean(share_src_WB),
            share_src_Other = mean(share_src_Other))

  # mannualy do pie chart in ppt


# 5. stack bar chart for each country ####
list_plot_stackbar <- list()

for (i in unique(temp_1$flag_group)){

plot <- ggplot(temp_1 |> 
        filter(category == "Traditional: actuals",
               flag_group == i
               )) + 
  geom_bar(aes(fill = vaccine_group_wbs, 
               y = cost, 
               x = country),
           position = "stack", stat = "identity") +
  geom_text(data = temp_3 |>
              filter(category == "Traditional: actuals",
                     flag_group == i)  , 
            aes(x = country, # labeling annual totals
                y = cost, 
                label = comma(cost)),
            position = position_stack(vjust = 1.05), 
            size = 4,
            hjust =0.5)+
  scale_fill_brewer(palette = "Set3") +
  labs(title="",fill="Vaccine",x="",y="Paid (US$)")+
  scale_y_continuous(labels = function(x) paste0(x / 1e6, "M"))+
  #facet_grid(~flag_group)+
  theme_minimal()+
  theme(
    legend.text = element_text(size = 20),  
    axis.text = element_text(size = 15),    
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1))  

list_plot_stackbar[[i]] <-plot
}

plot_stackbar_all_in_one <- ggarrange(
  plotlist =list(list_plot_stackbar$`Group 3`,list_plot_stackbar$`Group 2 & 3`,list_plot_stackbar$`Group 1, 2 & 3`),
  #list_plots[1],list_plots[2],list_plots[3],list_plots[4],list_plots[5],list_plots[6],
  labels =  c("Group 3 (Waivers)","Group 2 (add.)","Group 1 (add.)") ,
  ncol = 3, 
  nrow = 1,
  label.x = 0.08,
  common.legend =TRUE,  #share the same one !
  legend = "bottom")

print(plot_stackbar_all_in_one)

#summary
(temp_1 |>
    ungroup() |>
    filter(category == "Traditional: actuals") |>
    group_by(flag_group)|>
    summarise(  total_cost = sum(cost, na.rm = TRUE) ))

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
  geom_vline(aes(xintercept = live_births), linetype = "solid", color = "#005CB9",size=0.9) +
  geom_text(aes(x = live_births, 
                label = paste0("Births: ",comma(live_births))), vjust = -0.6, hjust = -0.1,color = "#005CB9", size = 3)+
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
             mean = round(mean(prop_doses_births,na.rm=T),1),
             count = sum(prop_doses_births > 1, na.rm = TRUE)
  )

temp_1|>
  filter(category =="Traditional: actuals")|>
  group_by(vaccine_group_wbs, category)|>
  summarise( median = round(median(prop_doses_births,na.rm=T),1),
             maximum = round(max(prop_doses_births,na.rm=T),1),
             mean = round(mean(prop_doses_births,na.rm=T),1),
             count = sum(prop_doses_births > 1, na.rm = TRUE)
  )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7. Estimating three groups in five years, imaging freight and devices and OPV ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# (1) Guess OPV, assuming no wastgage, 100% coverage, no buffer
temp_OPV <- temp_1 |>
  ungroup()|>
  select(iso3, live_births)|>
  unique()|>
  mutate(unit_price_OPV = 0.141 , #UNICEF public price range 0.11-0.20, 0.141 derived from AFG's forecast 
         doses_OPV = live_births * 4, # assumed 4 doses full vaccinated
         cost_OPV = doses_OPV * unit_price_OPV )

temp_OPV_aggre <- temp_OPV |>
  summarise(cost_OPV = sum(cost_OPV, na.rm=T))|>
  mutate(Group = "Group 1")|>
  rbind(
    temp_OPV |>
      filter(iso3 %in% iso3_gp2)|>
      summarise(cost_OPV = sum(cost_OPV, na.rm=T))|>
      mutate(Group = "Group 2")
  )|>
  rbind(
    temp_OPV |>
      filter(iso3 %in% iso3_gp3)|>
      summarise(cost_OPV = sum(cost_OPV, na.rm=T))|>
      mutate(Group = "Group 3")
  )
  
# (2)UNICEF data
temp_7 <- temp_1 |>
     group_by(category)|>
     summarise(cost = sum(cost, na.rm=T))|>
     mutate(Group = "Group 1")|>
  rbind(
    temp_1 |>
      filter(iso3 %in% iso3_gp2)|>
      group_by(category)|>
      summarise(cost = sum(cost, na.rm=T))|>
      mutate(Group = "Group 2")
   )|>
  rbind(
    temp_1 |>
      filter(iso3 %in% iso3_gp3)|>
      group_by(category)|>
      summarise(cost = sum(cost, na.rm=T))|>
      mutate(Group = "Group 3")
  )|>
  left_join(temp_OPV_aggre, by="Group")|>
  mutate(`Incl. OPV` = "OPV included")


# (3)brave projection
temp_7 <- temp_7 |>
  rbind(temp_7|> 
          mutate(cost_OPV = 0, # zero out the scenario without OPV
                 `Incl. OPV` = "OPV not included") )|>
  mutate(
    vac_cost = cost + cost_OPV, 
    cost_freight_vac_20perc = vac_cost * 0.2,
    guess_share_device_vac = 0.26,
    cost_device = vac_cost * guess_share_device_vac, 
    cost_freight_device_7perc = cost_device * 0.07,
    total_cost_2026 = vac_cost + cost_freight_vac_20perc + cost_device + cost_freight_device_7perc,
    total_cost_2027 = total_cost_2026 *1.02 , #growth rates for population growth
    total_cost_2028 = total_cost_2027 *1.02 , #freight and device costs asumming not constant
    total_cost_2029 = total_cost_2028 *1.02 , 
    total_cost_2030 = total_cost_2029 *1.02 ,
    total_cost_2026_2030 = total_cost_2026+total_cost_2027+total_cost_2028+total_cost_2029+total_cost_2030, 
    cost_raw_2026_2030 = cost + cost*1.02 + cost*1.02*1.02 + cost*1.02*1.02*1.02 +cost*1.02*1.02*1.02*1.02
  )|>
  rename(cost_raw_2026 = cost)

#transpose: 
temp_7_trans <- temp_7|>
  select( c(category,Group,`Incl. OPV`,cost_raw_2026,cost_raw_2026_2030, total_cost_2026,total_cost_2026_2030 ) )

temp_7_trans <- temp_7_trans |>
  pivot_longer(cols=c(starts_with("total_cost_"),starts_with("cost_raw")),
               values_to="cost", 
               names_to="year_group")|>
  pivot_wider(id_cols= c(Group, 'Incl. OPV',year_group), 
              values_from = cost, 
              names_from = category,
              names_prefix = "")|>
  mutate(flag_freight_devices = ifelse(startsWith(year_group, "total_cost"), 
                                  "Incl. freight & devices", 
                                  "Not incl. freight & devices"),
         duration = ifelse(year_group%in%c("cost_raw_2026","total_cost_2026"),"2026","2026-2030") 
         )

  
  
# (4) plotting
list_scenario_projections <- list()

for (i in c("OPV included","OPV not included")){
plot  <- ggplot(data = temp_7_trans
         |>  filter(`Incl. OPV`== i )
         )+
  geom_linerange(aes(x = duration, 
                     ymin = `Traditional: actuals`,
                     ymax = `Traditional: forecast`,
                     color= flag_freight_devices ),
                 linetype=1,
                 size = 1.5
                  )+
  geom_text(aes(x = duration, y = `Traditional: actuals`, label = scales::comma(`Traditional: actuals`)), 
              vjust = 0, hjust=1.2, color = "black", size = 3.9) +
  geom_text(aes(x = duration, y = `Traditional: forecast`, label = scales::comma(`Traditional: forecast`)), 
              vjust = 0, hjust=-0.18, color = "black", size = 3.9) +
  geom_point(aes(x = duration,y=`Traditional: actuals`),size=3,color="#FF7F24")+
  geom_point(aes(x = duration,y=`Traditional: forecast`),size=3,color="#3E9B6E")+
  scale_color_manual(name = "Freight & Devices",
                     values = c("Incl. freight & devices" = "#005CB9", 
                                  "Not incl. freight & devices" = "grey")) + 
  scale_y_continuous(labels = function(x) {paste0(scales::comma(x / 1e6), "M")}, 
                     breaks = seq(0, max(temp_7_trans$`Traditional: forecast`, na.rm = TRUE),  by = 10000000)) +
  theme_minimal()+
  facet_grid(`Incl. OPV` ~ Group)+
    theme(
      legend.text = element_text(size = 19),  # legend text
      axis.text = element_text(size = 18),    # axis text
      axis.title = element_text(size = 18),   #axis titles
      strip.text = element_text(size = 11),
      legend.position = "bottom"
    )+
  labs(title = "Traditional vaccine projection for Gavi 6.0", y="Cost in US$",x="",subtitle=paste0(i,". Haiti is missing."))

list_scenario_projections[[i]] <- plot
}

print(list_scenario_projections)




# save ####
ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/13_subsetB_pie_chronic_fragile_percentage_paid.png",
       plot=plot_pie_fragile_paid, width=17,dpi = 1000, height=11,bg = "white")

ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/13_subsetB_procured_cost_chonic_fragile_bycountryantigen.png",
       plot=plot_stackbar_all_in_one, width=15,dpi = 500, height=7,bg = "white")


ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/13_subsetB_live_birth_doses_chronic_fragile.png",
       plot=plot_FC_birth, width=12,dpi = 500, height=8,bg = "white")

ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/13_subsetB_forecast_scenarios_chronic_fragile_inclOPV.png",
       plot=list_scenario_projections[[1]], width=16,dpi = 500, height=8,bg = "white")
ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/13_subsetB_forecast_scenarios_chronic_fragile_exclOPV.png",
       plot=list_scenario_projections[[2]], width=16,dpi = 500, height=8,bg = "white")
