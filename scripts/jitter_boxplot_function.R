##############################################
# JITTER-BOX-PLOT - can be scaled by COD 
##############################################

# Making function to make gitter plot in the different functional guilds --> GENUS LEVEL
      #Can be scaled to the measured COD 
#Possible to plot: 
  #Groups + can exclude different values in the groups eg. PAOs 

## REQUIRENMENTS ##
# Needs a genus wilcox results 
# Needs special dataframe for COD 

### Vector for easy pltting of all
x <- 
  data[[3]] %>% 
  mutate(samples = map(samples, ~ 
                         distinct(., Genus, Guild, growth_group_assignment)  %>%
                         filter(str_detect(Genus, "nclassified", negate = T)))) %>% 
  unnest(cols = samples) %>% distinct(Genus, Guild, growth_group_assignment)


jitter_boxplot_function <- function(
    tidy_df = data[[3]], 
    d_meta_by_date = data[[2]], 
    filter_level = Genus, ## can be used to filter eg. by guild
    level_vec = unlist(x$Genus), ## must be x$filter_level, or if filter_level = Guild it can be c("__PAO__)
    max_no_genus = 10000000000, 
    wilcox_df = data_genus_all_samples, 
    rel_abun_tax = Genus, ## Always Genus (depends on wilcox df)
    tax_level = Genus, ## Always Genus (depends on wilcox df)
    include_tax, ## vector with genus name (eg. x$Genus)
    relative_to_COD = F,
    plant_colors = data[[4]], 
    number_of_columns = 6
    ){


plant_colors <- c("#2166AC", "#92C5DE", "#8B4513", "#ce661c")
  

## Select genus function


### Function to select GENUS only !!! 
select_taxa_and_plot_wilcox_result_genus_level <- function(filter_level = filter_level, 
                                                           level_vec = level_vec, 
                                                           max_no_genus = max_no_genus
                                                           ){
  
  sig_genus <- wilcox_df %>% 
    distinct() %>% ungroup() %>%
    filter((p_adjust) <= 0.05) %>% 
    mutate(Genus_plant = paste0(Tax, "_", Plant)) %>% group_by(Tax) %>% 
    #filter(min(p_adjust) < 0.01) %>% #distinct(Tax, Plant) %>% 
    ungroup()
  
  
  tidy_sig_genus <- tidy_df %>% 
    mutate(samples = map(samples, ~ 
                           select(., -OTU, -rel_abun_ASV, -Species, -rel_abun_species, -count) %>%
                           filter(Genus %in% unlist(sig_genus$Tax)) %>%
                           filter(!str_detect(Genus, "unclass")) %>% 
                           distinct(.keep_all = T) #%>% 
                           #filter({{filter_level}} %in% {{level_vec}}) # <- include to select specific genus 
    )) %>% 
    unnest(cols = samples) %>% 
    select(Genus, rel_abun_genus, Plant) %>% 
    left_join(., sig_genus %>% rename("Genus" = "Tax") %>% 
                select(Genus, mean_log2, Plant)) %>% 
    group_by(Plant, Genus, mean_log2) %>% 
    summarise(median_rel = median(rel_abun_genus),
              #mean_log2 = mean(mean_log2), 
              .groups = "drop") %>% 
    mutate(Genus_plant = paste0(Genus, "_", Plant), 
           sig_in_plant = ifelse(Genus_plant %in% unlist(sig_genus$Genus_plant), T, F),
           sign_log2 = "NS",
           sign_log2 = if_else(mean_log2 > 0 & sig_in_plant == T, "+", sign_log2), 
           sign_log2 = if_else(mean_log2 < 0 & sig_in_plant == T, "-", sign_log2), 
           Plant = factor(Plant, levels = c("Aalborg West", "Ejby Mølle", "Esbjerg West", "Randers")),
    ) %>%
    distinct() %>% 
    arrange(Plant) %>% 
    group_by(Genus) %>% 
    mutate(signs_cross_plants = paste(sign_log2, collapse = "|"), .groups = "drop") %>% 
    group_by(Plant) %>% 
    arrange(desc(median_rel)) %>%
    slice_head(n = {{max_no_genus}}) %>% 
    ungroup() %>% 
    select(-sign_log2) %>% 
    distinct()
}


### Make dataframe that include COD values
COD_calclated <- 
  d_meta_by_date %>% 
  select(Plant, Date_rawdata, 
         Pre_COD_AfterPS_ST_norm, 
         Pre_COD_BeforePS_ST_norm,
         COD_beforePS_weekly_Mean,
         COD_afterPS_weekly_Mean,
         COD_beforePS, 
         COD_afterPS, 
         #COD_BeforePS_cal, COD_AfterPS_cal
  ) %>% 
  filter(!is.na(Pre_COD_BeforePS_ST_norm)) %>% filter(!is.na(Pre_COD_AfterPS_ST_norm)) %>% 
  group_by(Plant) %>% 
  #filter(Date_rawdata %in% unlist(tidy_meta$Date_rawdata)) %>% 
  mutate(
    COD_BeforePS_cal = Pre_COD_BeforePS_ST_norm*sd(COD_beforePS, na.rm = T) + 
      mean(COD_beforePS, na.rm = T), 
    COD_AfterPS_cal = Pre_COD_AfterPS_ST_norm*sd(COD_afterPS, na.rm = T) + 
      mean(COD_afterPS, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(COD_red_cal = ((1-(COD_AfterPS_cal/COD_BeforePS_cal))*100), 
         COD_red_RAW = (1-(COD_afterPS/COD_beforePS))*100) 

tidy_COD <- 
  tidy_df %>% 
  ungroup() %>% 
  left_join(., select(COD_calclated, 
                      Plant, Date_rawdata,
                      COD_BeforePS_cal, COD_AfterPS_cal,
                      COD_BeforePS_cal, COD_AfterPS_cal), by = c("Date_rawdata", "Plant"))

## Plotting function
plot_Genus_groups_jitter_no_pool <- function(#rel_abun_tax = rel_abun_genus, 
                                             #tax_level = Genus, 
                                             include_tax,
                                             relative_to_COD,
                                             number_of_columns
                                             ){
  
  tidy_all_functional_guild <- tidy_COD %>% 
    mutate(samples = map(samples, ~ 
                           select(all_of(.), -OTU, -rel_abun_ASV, -Species, -rel_abun_species, -count) %>%
                           filter(Genus %in% {{include_tax}})  %>%
                           distinct(.keep_all = T))) %>% 
    unnest(cols = samples) %>% 
    distinct(across(c(SampleID, PrimarySettler, Plant, Genus, rel_abun_genus,
                      #{{tax_level}}, {{rel_abun_tax}}, 
                      #DMI_rain_mm, 
                      Date_rawdata, #Season, 
                      #days_since_rain_event, time_before_rain_event, 
                      #Flow_BeforePS_ST_norm,
                      COD_BeforePS_cal, COD_AfterPS_cal
    ))) %>% 
    mutate(Genus_plant = paste0({{tax_level}}, "_", Plant), 
           COD_AfterPS_cal = COD_AfterPS_cal,
           #log(COD_AfterPS_cal),
           #(COD_AfterPS_cal-median(COD_AfterPS_cal)) /
           #                  (quantile(COD_AfterPS_cal, probs = .75)-
           #                    quantile(COD_AfterPS_cal, probs = 0.25)), 
           COD_BeforePS_cal =COD_BeforePS_cal 
           #log(COD_BeforePS_cal)
           #(COD_BeforePS_cal-median(COD_BeforePS_cal)) /
           #                  (quantile(COD_BeforePS_cal, probs = .75)-
           #                    quantile(COD_BeforePS_cal, probs = 0.25)) 
    ) %>%  
    #filter(Genus_plant %in% {{genus_plant_vec}} | str_detect(PrimarySettler, "Before")) %>% 
    #group_by(Plant) %>% 
    mutate(median = rel_abun_genus, 
           median = if_else(PrimarySettler == "Before" & {{relative_to_COD}}, 
                            rel_abun_genus*COD_BeforePS_cal*1/100, 
                            median),
           median = if_else(PrimarySettler == "After" & {{relative_to_COD}}, 
                            rel_abun_genus*COD_AfterPS_cal*1/100, median),
           tax_lab = Genus
    ) %>% 
    distinct(median, Date_rawdata, Plant, PrimarySettler, .keep_all = T) #%>% 
  #filter(median > 0) 
  
  x_ny = tidy_all_functional_guild %>%
    mutate(
      lab = tax_lab,
      #lab = fct_reorder(lab, median, .desc=T),
    ) %>% 
    left_join(., add_to_facet_title %>% 
                rename("add" = 2))
  
  # Plotting
  y <- x_ny %>%  
    distinct(lab, Plant, PrimarySettler, COD_AfterPS_cal, COD_BeforePS_cal, median, Date_rawdata, add) %>% 
    #filter(Plant == plant) %>% 
    mutate(lab = str_sub(lab, start = 4),
           lab = if_else(str_detect(lab, "midas", negate = T) & str_detect(lab, "Ca_", negate = T), 
                         paste0("*", lab, "*"), lab), 
           lab = str_replace(lab, "Ca_", "*Ca.* "), 
           lab = paste0(lab, "<br>", add),
           PrimarySettler = as.character(PrimarySettler),
           color = PrimarySettler, 
           PrimarySettler = if_else(PrimarySettler == "Before", 
                                    true = paste0(Plant, " ", PrimarySettler),
                                    false = PrimarySettler), 
           PrimarySettler = if_else(PrimarySettler == "After", 
                                    true = paste0(Plant, " ", PrimarySettler),
                                    false = PrimarySettler),
           PrimarySettler = factor(PrimarySettler, 
                                   levels = c("Aalborg West Before", "Aalborg West After", 
                                              "Ejby Mølle Before", "Ejby Mølle After",
                                              "Esbjerg West Before", "Esbjerg West After", 
                                              "Randers Before", "Randers After"), 
                                   labels = c("B_A", "A_A", "B_E", "A_E","B_M", "A_M","B_R","A_R")), 
           Plant = factor(Plant, levels = c("Aalborg West", "Ejby Mølle", "Esbjerg West", "Randers")),
           PS_num = as.numeric(PrimarySettler)
    ) 
  y <- y %>% 
    ggplot(aes(
      y = (median), 
      #fill=Plant, 
      x=PrimarySettler)) + 
    annotate("rect", xmin=0.5,xmax=2.5,ymin=-Inf,ymax=Inf, alpha=0.4, fill = plant_colors[1]) +
    annotate("rect", xmin=2.5,xmax=4.5,ymin=-Inf,ymax=Inf, alpha=0.4 , fill=plant_colors[2]) +
    annotate("rect", xmin=4.5,xmax=6.5,ymin=-Inf,ymax=Inf, alpha=0.4, fill=plant_colors[3]) +
    annotate("rect", xmin=6.5,xmax=8.5,ymin=-Inf,ymax=Inf,alpha=0.4,fill=plant_colors[4]) +
    geom_jitter(size = 2, alpha = 0.6,
                aes(color = color, shape = Plant,
                ), 
                position = position_jitterdodge(dodge.width = 0.95, jitter.width = 0.3)) +
    scale_shape_manual(values = c(rep(16,4))) +
    geom_boxplot(aes(fill = color, color = color), outlier.shape = NA, alpha = 0.3) +
    facet_wrap(~lab, scales = "free_y", ncol =  {{number_of_columns}} #space = "free"
    ) +
    scale_x_discrete(labels = str_remove(c("B_A", "A_A", "B_E", "A_E","B_M", "A_M","B_R", "A_R"), "_."), 
                     expand=c(0, 0)) + 
    labs(x = NULL) +
    ylab(label = if_else({{relative_to_COD}},"COD [mg/L] * Relative abundance [%]" ,"Relative abundance [%]")) +
    scale_color_manual(values = c("gray0", "gray60")) +#rep(plant_colors, each = 2)) +
    scale_fill_manual(values = c("gray0", "gray60")) +#rep(plant_colors, each = 2)) +
    theme_bw() +
    theme(axis.text.x = element_blank(), #element_markdown(angle = 0, vjust = 0.5, size = 13), 
          axis.text.y = element_markdown(size = 20), 
          strip.text.x = element_markdown(size = 24, lineheight = 0.1), 
          strip.background = element_rect(color="grey90", fill="grey90", size=1.0,
                                          linetype="solid"), 
          legend.margin = margin(b = unit(3, "mm")),
          legend.text = element_markdown(size = 24), 
          #legend.title = element_markdown(size = 15), 
          #plot.title = element_markdown(),
          legend.key.size = unit(8, "pt"),
          axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), 
          #title = element_text(size = 13), 
          axis.title.y = element_markdown(size = 20),
          panel.grid.minor = element_blank(), 
          panel.grid.major.x = element_blank(),
          legend.position = "bottom", legend.box = "vertical",
    ) + 
    guides(fill = guide_legend(legend.position = "bottom", title = " ", reverse = T, 
                               override.aes = list(size = 5)), 
           color = "none", 
           shape = guide_legend(title = " ", override.aes = list(fill = plant_colors, size = 5, shape = 21, 
                                                                 color = "gray40"
           ))
           #guide_legend(legend.position = "bottom", 
           #                     title = "Plant", 
           #                     override.aes = list(size = 3, breaks = unique(y$Plant), color = plant_colors)))
    ) 
  
  y
}


## Use "select_taxa_and_plot_wilcox_result_genus_level" function
tidy_sig_genus <- 
  select_taxa_and_plot_wilcox_result_genus_level(
    filter_level = filter_level,
    level_vec = level_vec,
    max_no_genus = max_no_genus)

## Make vectors to input in plotting fung 
genus_vec <- tidy_sig_genus %>% distinct(Genus) %>% unlist()
genus_plant_vec <- tidy_sig_genus %>%  filter(sig_in_plant == T) %>%  distinct(Genus_plant) %>% unlist()
add_to_facet_title <- tidy_sig_genus %>% select(Genus, signs_cross_plants) %>% rename("lab" = "Genus")


#filter(SampleID %in% unlist(paired_samples)) %>% 
  plot_Genus_groups_jitter_no_pool(relative_to_COD = relative_to_COD, 
    include_tax = include_tax, number_of_columns = number_of_columns)
  
}
