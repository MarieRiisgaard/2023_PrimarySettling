##################################
## Complex Upset
##################################


upset_plot<- function(wilcox_data = data_genus_all_samples){

upset_df <- wilcox_data %>% 
  filter(str_detect(Tax, "unclassi", negate = T)) %>% 
  mutate(
    Sign = if_else(mean_log2 > 0, "Increase", "Decrease"),
    Sign = if_else(p_adjust > 0.05, "Insignificant", Sign), 
    Sign = factor(Sign), 
    Sign = fct_rev(Sign)) %>% 
  complete(Plant, Tax) %>%
  arrange(Plant) %>%
  mutate(
    #Plant = str_replace(Plant, " ", "_"),
    #Sign = if_else(mean_log2 > 0, "Increase", "Decrease"), 
    Genera = str_c(Tax, Sign, sep = "_")
  )  %>% 
  select(-X, -p_adjust, -mean_log2) %>% 
  pivot_wider(names_from = Plant, values_from = Genera) %>%
  filter(!is.na(Sign)) %>%
  mutate_at(c( "Randers","Ejby Mølle", "Esbjerg West", "Aalborg West"), 
            function(x){if_else(is.na(x), F, T)}) %>% 
  mutate(Sign = factor(Sign, levels = c("Insignificant", "Increase", "Decrease")))

plants = rev(c("Aalborg West", "Ejby Mølle", "Esbjerg West", "Randers"))

upset_df[plants] = upset_df[plants] == 1


#upset_df %>% filter(`Ejby M?lle` == FALSE & `Aalborg West` == FALSE & `Randers` == TRUE & `Esbjerg West` == FALSE) %>% group_by(Sign) %>% summarise(n())

all_samples = ComplexUpset::upset(upset_df, plants, 
                    name='Number of genera shared across WWTP', 
                    set_sizes = ComplexUpset::upset_set_size(geom=geom_bar(
                      aes(fill=Sign), show.legend = T, size = 14)) +
                      guides(fill = guide_legend(title = NULL, override.aes = list(size = 3))) +
                      scale_x_continuous(expand = c(0,0), trans = "reverse", breaks = c(0, 100, 200, 300, 400, 500)) +
                      scale_y_continuous(expand = c(0,0), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
                      ylab('Number of genera') + 
                      theme(#legend.text = element_text(size = 15)
                        axis.text = element_text(color = "black")
                      ) +
                      scale_fill_manual(values = c("Decrease" = "#CE7E7E", 
                                                   "Increase"= "#6F8FAF",
                                                   "Insignificant"="gray80")),
                    base_annotations=list(
                      'Intersection size'= ComplexUpset::intersection_size(
                        counts=F, text = list(size = 14, color = "black"),
                        text_colors = c("black", "black", "black"),
                        mapping=aes(fill=Sign), 
                      ) +
                        scale_fill_manual(values = c("Decrease" = "#CE7E7E",
                                                     "Increase"="#6F8FAF", 
                                                     "Insignificant"="gray80")
                        ) + 
                        scale_y_continuous(expand = c(0,0), breaks = seq(20, 220, 20)) +
                        guides(fill = "none") +
                        theme(axis.ticks.y.left = element_blank(), 
                              axis.text.y = element_text(color = "black"),
                              panel.grid.major.y = element_line(color = "grey50", linewidth = 0.2), 
                              panel.grid.major.x = element_blank(), 
                              axis.line.y.left = element_line(color = "black", linewidth = 0.2)
                        )
                    ),
                    width_ratio=0.3, # 0.3 
                    height_ratio = 0.3, # 0.5
                    sort_sets=F,  guides='over',
                    themes=ComplexUpset::upset_default_themes(text=element_text(size = 24, color = "black"), 
                                                title = element_text(color = "black"),
                                                axis.text = element_text(color = "black")
                                                #strip.text = element_text(color= "black")
                    )
)
all_samples

}
