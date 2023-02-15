##########################################
############Sankey plot
#########################################

san_key_plot <- function(plant){
  
  unique_genus <- data[[3]] %>% sample_n(50) %>% 
    unnest(samples) %>% distinct(Genus, Species) 
  
  # Get result on genus level
  genus_results <- data_genus_all_samples %>% 
    mutate(
      Sign_genus = if_else(mean_log2 > 0, "Increase", "Decrease"),
      Sign_genus = if_else(p_adjust > 0.05, "Insignificant", Sign_genus)) %>% 
    rename("Genus" = "Tax") %>% 
    select(Plant, Genus, Sign_genus) 
  
  # Get result at the species level and merge with genus result
  species_results <- data_species_all_samples  %>% 
    mutate(
      Sign_species = if_else(mean_log2 > 0, "Increase", "Decrease"),
      Sign_species = if_else(p_adjust >= 0.05, "Insignificant", Sign_species)) %>% 
    rename("Species" = "Tax") %>% 
    select(Plant, Species, Sign_species) %>% 
    left_join(., unique_genus, by = "Species")
  
  # Make sankey df using the make_long function
  san_key <-  
    full_join(genus_results, species_results) %>% 
    filter(Plant == plant) %>% 
    mutate(Sign_species = ifelse(is.na(Sign_species), "Not tested", Sign_species),
           Sign_genus = ifelse(is.na(Sign_genus), "Not tested", Sign_genus))  %>% 
    select(Plant, Sign_genus, Sign_species) %>% 
    ggsankey::make_long(Plant, Sign_genus, Sign_species) 
  
  # Make df for lables  
  n_genus <- genus_results %>% 
    filter(Plant == plant) %>% 
    group_by(Sign_genus) %>% 
    summarise(genus = n(), next_x = "Sign_species", .groups = "drop") %>% 
    rename("node" = "Sign_genus")
  n_genus <- n_genus %>% 
    add_row(node = plant, genus = sum(n_genus$genus), next_x = "Sign_genus")
  
  # Add labels to sankey df
  dagg <- san_key %>%
    #filter(next_x == "Sign_genus") %>% 
    group_by(node, next_x)%>% 
    tally() %>%
    left_join(n_genus) %>%
    group_by(node, next_x,n) %>%
    summarise(genus = ifelse(is.na(next_x), paste0(n) , paste0(n, "/", genus)),
              .groups = "drop")
  
  #Make levels manualy inorder  
  df2 <- full_join(san_key, dagg) %>%
    mutate(
      next_x = "1", 
      next_x = if_else(x == "Plant", "2", next_x), 
      next_x = if_else(x == "Sign_genus", "3", next_x),
      next_x = if_else(x == "Sign_species", "NA", next_x)
    )
  
  # Factor in order to determine the order inwhich the results appear 
  pl <- 
    df2 %>% 
    mutate(node = factor(node, levels = c(plant, "Not tested", 
                                          "Insignificant", 
                                          "Decrease", 
                                          "Increase")), 
    next_node = factor(next_node, levels = c("Not tested", 
                                             "Insignificant", 
                                             "Decrease", 
                                             "Increase", NA))) %>% 
    ggplot(aes(x = x
               , next_x = next_x
               , node = node
               , next_node = next_node
               , fill = node
               , label = paste0(node," (", genus, ")"))) +
    ggsankey::geom_sankey(flow.alpha = 0.5
                , node.color = "black"
                  ,show.legend = FALSE) + 
    ggsankey::geom_sankey_label(size = 8, color = "black", fill= "white", hjust = 0.2) + 
    scale_x_discrete(expand = c(0.001,0.5), 
                     labels = c(
                       "Plant" = "No. of tested taxa<br>(No. of species/No. of genera)", 
                       "Sign_genus" = "Trends when testing<br>at the genus level<br>(No. of species/No. of genera)", "Sign_species" = "Trends when testing<br>at the species level<br>(No. of species)")) +
    theme_xaringan() + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(), axis.title.y = element_blank()
          , axis.text.y = element_blank()
          , axis.ticks.x = element_blank(), axis.ticks.y = element_blank()  
          , panel.grid = element_blank(), axis.text.x = element_markdown(color = "black")
    ) +
    scale_fill_manual(values = c(plant = "white", 
                                 "Not tested"="gray30", 
                                 
                                 "Decrease"="#CE7E7E", 
                                 "Increase"="#6F8FAF", "Insignificant"="gray80"
    ))
  
  pl
}

