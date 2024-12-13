#### FILE DESCRIPTION ####
# Filename: 09_unicef-trad-vacc-subsets-comparison.R
# Author: Hao-Kai Tseng
# Date: 2024-10-25
# Description: compare 4 cells in comparison,
# Whether Gavi supports that programme in that country? 
# Whether the program involves one of the common Gavi-supported antigens, such as HPV, PCV, PENTA, Malaria, RV, MenA, YF, MEN, Ebola, MPOX, or JEV?

library(treemapify)  

#+++++++++++++++++++++++++++++++++++++++++++++++
# 1.labeling four cells of confusion matrix ####
#+++++++++++++++++++++++++++++++++++++++++++++++
temp_gavi_vaccines <- c("HPV","PCV","PENTA","MAL","RV","MENA","YF","MEN","EBL","MPOX","JEV","IPV") # reference : dat_SAP|> select(vaccine_group_wbs)|>unique()


temp_1 <- dat_master_long_agg |> 
  anti_join(table_trad_missing, by= "iso3")|>
  filter(category %in% c("Traditional: forecast","Traditional: actuals"),
         !is.na(country), #excl. pacific island 
         cofi_group_acro!="FS",
         cost!=0  )|> #  to not include Ebola. Malaria if not procured
  mutate(flag_cell =case_when(
    flag_gavi_funded=="Gavi-funded" & vaccine_group_wbs %in% temp_gavi_vaccines ~ 1,
    flag_gavi_funded=="Non-Gavi-funded" & vaccine_group_wbs %in% temp_gavi_vaccines ~ 2,
    flag_gavi_funded=="Gavi-funded" & ! vaccine_group_wbs %in% temp_gavi_vaccines ~ 3,
    flag_gavi_funded=="Non-Gavi-funded" & ! vaccine_group_wbs %in% temp_gavi_vaccines ~ 4,
    TRUE ~ NA) 
  )

# 2. flag trad paid doses = cofi doses ####
temp_2 <- temp_1 |> 
  filter(category=="Traditional: actuals")|>
  left_join( dat_SAP|>
               group_by(iso3,vaccine_group_wbs)|>#need aggregation other may have BCU
               summarise(co_financing_doses =sum(co_financing_doses,na.rm=T),.groups = "drop"),
     by = c("iso3","vaccine_group_wbs")
  )|>
  mutate(flag_problematic = if_else(doses==co_financing_doses & !is.na(co_financing_doses),"error" , "no error identified"),
         flag_cell_error = if_else(flag_problematic=="error",paste0(as.character(flag_cell)," error"),as.character(flag_cell))
         )


# summary: assess vax type and cost
temp_2 |> 
  #filter(category=="Traditional: actuals")|>
  group_by(flag_cell,flag_problematic)|> #vaccine_group_wbs
  summarise(n_programmes_procured =  n(),
            cost=sum(cost,na.rm=T))


# 3. antigen landscape bar ####
ggplot( 
  temp_1 |> 
    filter(category=="Traditional: actuals")) + 
  geom_bar(aes(fill = vaccine_group_wbs, 
               y = as.factor(flag_cell), 
               x = cost),
           position = "fill", stat = "identity") +
  facet_grid( ~ vaccine_group_wbs)




# 4. treemap for four cells ####
plot_tree <- ggplot(temp_1|>
         filter(category=="Traditional: actuals")|>
         group_by(flag_cell,vaccine_group_wbs)|>
         summarise(cost = sum(cost,na.rm=T),.groups="drop")|>
         mutate(cost_label = ifelse(cost >= 1000000,
                                    paste0(round(cost / 1000000), "M"),
                                    "<1M")), 
       aes(area = cost, 
           fill = vaccine_group_wbs,
           label= vaccine_group_wbs,
           subgroup = factor(flag_cell))) +
  geom_treemap()+
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15,
                    grow = TRUE)+
  geom_treemap_text(aes(label = cost_label), 
                    colour = "white", 
                    place = "bottomright", 
                    size = 18,
                    alpha = 0.8) +
  geom_treemap_subgroup_text(place = "centre", 
                             grow = TRUE,
                             alpha = 0.25, 
                             colour = "black",
                             fontface = "italic")+
  labs(fill="Vaccine",title="Vaccine procured through traditional vaccine channel (US$)",subtitle = "Full dataset. Excluding FS countries")+
  theme(
    legend.text = element_text(size = 15)
  )

print(plot_tree)  


# 5. treemap labelling error ####
plot_tree_error <- ggplot(temp_2|>
                      group_by(flag_cell_error,vaccine_group_wbs)|> #not flag_cell
                      summarise(cost = sum(cost,na.rm=T),.groups="drop")|>
                      mutate(cost_label = ifelse(cost >= 1000000,
                                                 paste0(round(cost / 1000000), "M"),
                                                 "<1M")), 
                    aes(area = cost, 
                        fill = vaccine_group_wbs,
                        label= vaccine_group_wbs,
                        subgroup = flag_cell_error)) +
  geom_treemap()+
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15,
                    grow = TRUE)+
  geom_treemap_text(aes(label = cost_label), 
                    colour = "white", 
                    place = "bottomright", 
                    size = 18,
                    alpha = 0.8) +
  geom_treemap_subgroup_text(place = "centre", 
                             grow = TRUE,
                             alpha = 0.4, 
                             colour = "black",
                             fontface = "italic")+
  labs(fill="Vaccine",title="Vaccine procured through traditional vaccine channel (US$)",subtitle = "Full dataset. Excluding FS countries")+
  theme(
    legend.text = element_text(size = 15)
  )

print(plot_tree_error)  



# SAVE ####
ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/14_full_data_treemap_tradprocured.png",
       plot=plot_tree, width=13,dpi = 500, height=10,bg = "white")

ggsave("41. Analytics/traditional-vaccines-UNICEF/output graphs/14_full_data_treemap_tradprocured_errorlabelling.png",
       plot=plot_tree_error, width=13,dpi = 500, height=10,bg = "white")

