#################################################
#### Season plot function #####################
############################################

# function from https://newbedev.com/determine-season-from-date-using-lubridate-in-r
# the season cut dates (in the form MMDD) are based on the astronomical seasons. 
# correlates better with the process tank temperature profile than cutting by months.
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  cuts <- base::cut(numeric.date, breaks = c(0,320,0621,0923,1222,1231))
  levels(cuts) <- c("Winter","Spring","Summer","Autumn","Winter")
  return(cuts)
}

## Function to make PCA seperated by plant

PCA_paired_season_color <- function(plant, amp_object){
  
  #plant = "Randers"
  samples <- amp_object 
  
  samples$metadata <- samples$metadata %>% 
    mutate(Season = getSeason(Date_rawdata)) %>% 
    select(SampleID, Plant, PrimarySettler, Date_rawdata, Season
           ) %>% 
    mutate(#month = month(Date_rawdata),
           #date_num = as.numeric(month),
           #month_fct = as.factor(month),
           #month_names = month(Date_rawdata, label = T, abbr = T),
           s_ = factor(Season,
                       #levels = c("DJF", "MAM", "JJA", "SON"),
                       levels = c("Winter", "Spring", "Summer", "Autumn")
           )) %>%
    rename("PS_" = "PrimarySettler")
  
  paired_samples <-  
    samples$metadata %>% 
    distinct(Plant, PS_, Date_rawdata) %>% 
    group_by(Plant, Date_rawdata) %>% 
    summarise(count = n(), .groups = "drop") %>% 
    filter(#if_else(Plant == "Randers", count == 4, 
      count == 2#)
    ) %>%
    left_join(.,  samples$metadata, by = c("Plant", "Date_rawdata")) %>% 
    distinct(SampleID) %>% unlist()
  
  samples$metadata %>% filter(SampleID %in% paired_samples) %>% 
    distinct(Plant, PS_, Date_rawdata) %>% 
    group_by(Plant, PS_) %>%  
    summarise(count = n(), .groups = "drop") 
  
  
  month_names = sort(unique(samples$meta$month_names))
  shape_vec = rep(22, 150)
  season_colors <- c("#6F8FAF", "#A9DFBF", "#F9E79F", "#D98880")
  month_colors <- c("#702963", "#CBC3E3",  #Jan + feb
                    "#87CEEB" ,"#4682B4", "#5F8575",  #Marts-may
                    "#A9DFBF", "#F9E79F", "#DAA520", # jun-aug
                    "#CC7722", "#F8C8DC","#A95C68", #sep-nov
                    "#800020") # December
  month_colors <- c("#7B69BC", "#697BBC",  #Jan + feb
                    "#699BBC" ,"#58A63D", "#ABE896",  #Marts-may
                    "#DEE0B9", "#F5F340","#F5AD40", # jun-aug
                    "#92570B", "#92340B","#BC6989", #sep-nov
                    "#C8A0D2") # December
  month_colors <- c("#0B3392", "#0B5292",  #Jan + feb
                    "#0B7492" ,"#0B926A", "#0B923B",  #Marts-may
                    "#8ED398", "#CDE120","#F40909", # jun-aug
                    "#BC2C38", "#BA5961","#6462DB", #sep-nov
                    "#1B3FC9") # December
  plot_df <- samples %>% 
    amp_subset_samples(Plant == plant) %>% 
    amp_subset_samples(SampleID %in% paired_samples)
  
  plot <- plot_df %>%   
    amp_ordinate(sample_color_by = "PS_", 
                 transform = "none", 
                 distmeasure = "bray",
                 filter_species = 0.01, 
                 type = "pcoa", 
                 sample_shape_by = "PS_", 
                 sample_point_size = 2.5, 
                 #envfit_factor = "PS_", #"month_fct", #  #"s_", 
                 #envfit_numeric = "date_num",
                 #envfit_textcolor = "black", 
                 #envfit_textsize = 20
                 #species_plot = T, 
                 #species_nlabels = 7, 
    )
  
  #plot[["layers"]][[2]][["data"]][["Name"]] = c("Before", "After")
  
  plot <- plot + 
    geom_point(aes(fill = PS_, #s_, 
                   shape = PS_
    ), size = 2.5, stroke = 1) +
    scale_color_manual(values = c("gray20", "gray80")) +
    # scale_fill_gradientn(colors = month_colors, 
    #                      breaks= 1:12, 
    #                      labels = month_names
    #                      ) +
    #scale_color_manual(values = season_colors) +
    #scale_shape_manual(values = c(15,16,17,18), 
    scale_fill_manual(values = c("gray20", "gray80"), #values = season_colors, 
                      labels = c("Before", "After")) +  #c("Winter", "Spring", "Summer", "Autumn")) +
    scale_shape_manual(values = c(21, 22), 
                       labels = c("Before", "After")
    ) +
    #labs(title = paste0('__', plant, "__")) +
    theme(legend.text = element_markdown(size = 20, lineheight = 0.001), 
          legend.title = element_blank(), #element_markdown(size = 20, lineheight = 0.001),
          legend.spacing.y = unit(0.001, 'cm'),
          legend.position = "bottom",
          legend.box = "horizontal",
          plot.subtitle = element_markdown(size = 20, lineheight = 0.0001),
          axis.title = element_text(size = 20, color = "black"),
          axis.text = element_text(size = 16, color = "black"),
          plot.title = element_markdown(size = 24),
          panel.grid.minor = element_blank()) +
    guides(shape = "none",  #guide_legend("Location<br>Before Settler"), 
           fill = "none", 
           # guide_legend(title = "__Sampling<br>season__", 
           #                     nrow = 2, vjust = 1, order = 0, 
           #                     override.aes=list(shape=c(22), size = 3.5)
           # ),
           color = guide_legend(override.aes=list(shape=c(21,22), 
                                                  fill = c("gray20", "gray80"),  
                                                  size = 5,
                                                  color = c("gray20", "gray80"), 
                                                  stroke = 1.1),
                                title = "__Primary Settler__", 
                                #direction = "horizontal", 
                                title.position = "left",
                                vjust = 1, nrow = 1, order = 1)
    ) 
  
  
  # x = plot[["layers"]][[2]]
  # y = plot[["layers"]][[3]]
  # 
  # plot[["layers"]][[2]] = y
  # plot[["layers"]][[3]] = x
  
  
  bc.dist.matrix <- vegan::vegdist(t(plot_df$abund), method = "bray")
  metadata <- plot_df$metadata
  PS <- vegan::adonis2(bc.dist.matrix ~ PS_, data = metadata)
  x <- vegan::adonis2(bc.dist.matrix ~ s_, data = metadata)
  a = paste0("Primary Settler: R<sup>2</sup>=", round(PS$R2[1],2), ", p<", PS$`Pr(>F)`[1])
  b = paste0("Season: R<sup>2</sup>=", round(x$R2[1],2), ", p<", x$`Pr(>F)`[1])
  print(a)
  print(b)
  
  plot + labs(subtitle = paste0(a#, "<br>", b 
  ))
  
}

PCA_paired_season_color


plot_all_influent_streams <- function(amp_object, primary_settler = c("Before")){ 
  
  
  samples <- amp_object 
  
  month_names = sort(unique(samples$meta$month_names))
  shape_vec = rep(22, 150)
  
  samples$metadata <- samples$metadata %>% 
    mutate(Season = getSeason(Date_rawdata)) %>% 
    select(SampleID, Plant, PrimarySettler, Date_rawdata, Season) %>% 
    mutate(
           s_ = factor(Season, 
                       levels = c("DJF", "MAM", "JJA", "SON"),
                       labels = c("Winter", "Spring", "Summer", "Autumn")
           )) %>% 
    rename("PS_" = "PrimarySettler")
  
  paired_samples <-  
    samples$metadata %>% 
    distinct(Plant, PS_, Date_rawdata) %>% 
    group_by(Plant, Date_rawdata) %>% 
    summarise(count = n(), .groups = "drop") %>% 
    filter(#if_else(Plant == "Randers", count == 4, 
      count == 2#)
    ) %>%
    left_join(.,  samples$metadata, by = c("Plant", "Date_rawdata")) %>% 
    distinct(SampleID) %>% unlist()
  
  samples$metadata %>% filter(SampleID %in% paired_samples) %>% 
    distinct(Plant, PS_, Date_rawdata) %>% 
    group_by(Plant, PS_) %>%  
    summarise(count = n(), .groups = "drop") 
  
  
  plot = samples %>% 
    amp_subset_samples(PS_ %in% primary_settler) %>% 
    amp_subset_samples(SampleID %in% paired_samples) %>% 
    amp_ordinate(sample_color_by = "Plant", 
                 transform = "none", 
                 distmeasure = "bray",
                 filter_species = 0.01, 
                 type = "pcoa", 
                 #output = "complete",
                 sample_shape_by = "Plant", 
                 sample_point_size = 2.5, 
                 #envfit_factor = "Plant", 
                 #envfit_numeric = "date_num",
                 #envfit_textcolor = "black", 
                 #envfit_textsize = 6, 
                 sample_colorframe = T,
                 #species_plot = T, 
                 #species_nlabels = 7, species_label_taxonomy = "OTU"
    ) +
    scale_color_manual(values = c("gray20", "#804600", "#6F8FAF", "gray80")
                       #data[[4]]
    ) +
    scale_fill_manual(values = c("gray20", "#804600", "#6F8FAF", "gray80")
                      #data[[4]]
                      
    ) +
  scale_shape_manual(values = c(16,16,16,16)) +
  labs(title = paste0('__', primary_settler, " primary settling","__")) +
    theme(legend.text = element_markdown(size = 20), 
          legend.title = element_blank(), 
          #legend.position = "none", 
          plot.subtitle = element_markdown(size = 20, lineheight = 0.0001),
          axis.title = element_text(size = 20, color = "black"),
          axis.text = element_text(size = 16, color = "black"),
          legend.box = "horizontal",
          plot.title = element_markdown(size = 24),
          panel.grid.minor = element_blank()) +
    guides( 
      line = "none",
      shape = "none",  #guide_legend("Location<br>Before Settler"), 
      fill = "none", 
      color = guide_legend(override.aes=list(shape=c(16,16,16,16), 
                                             size = 5,
                                             fill = NA,
                                             stroke = 0, 
                                             linetype=c(0,0,0,0)),
                           title = "__Primary Settler__", 
                           #direction = "horizontal", 
                           title.position = "left",
                           vjust = 1, nrow = 1, order = 1))
      
  # plot[["layers"]][[3]][["data"]][["Name"]] = c("Ejby Mølle",
  #                                               "Esbjerg West",
  #                                               "Randers",     
  #                                               "Aalborg West")
  # 
  plot
  
}






# plant = "Aalborg West"
# env_factor = "s_"
# primarysettler = "Before"

plot_each_influent_stream <- function(plant, env_factor, amp_object, primarysettler){ 
  
  samples <- amp_object 
  
  
  month_names = sort(unique(samples$meta$month_names))
  shape_vec = rep(22, 150)
  season_colors <- c("#6F8FAF", "#A9DFBF", "#F9E79F", "#D98880")
  
  #  fill = c("#6F8FAF", "#A9DFBF", "#F9E79F", "#D98880")
  #season_colors <- c("gray30", "#CE7E7E", "#6F8FAF", "gray80")
  
  
  samples$metadata <- samples$metadata %>% 
    #mutate(Season = season(Date_rawdata)) %>% 
    mutate(Season = getSeason(Date_rawdata)) %>% 
    select(SampleID, Plant, PrimarySettler, Date_rawdata, Season) %>% 
    mutate(month = month(Date_rawdata),
           date_num = as.numeric(month),
           month_fct = as.factor(month),
           month_names = month(Date_rawdata, label = T, abbr = T), 
           s_ = factor(Season, 
                       levels = c("Winter","Spring","Summer","Autumn")
                       #levels = c("DJF", "MAM", "JJA", "SON"),
                       #labels = c("Winter", "Spring", "Summer", "Autumn"
                      )
           ) %>% 
    rename("PS_" = "PrimarySettler")
  
  paired_samples <-  
    samples$metadata %>% 
    distinct(Plant, PS_, Date_rawdata) %>% 
    group_by(Plant, Date_rawdata) %>% 
    summarise(count = n(), .groups = "drop") %>% 
    filter(#if_else(Plant == "Randers", count == 4, 
      count == 2#)
    ) %>%
    left_join(.,  samples$metadata, by = c("Plant", "Date_rawdata")) %>% 
    distinct(SampleID) %>% unlist()
  
  plot_df = samples %>% 
    amp_subset_samples(Plant == plant) %>% 
    amp_subset_samples(PS_ %in% primarysettler) %>% 
    amp_subset_samples(SampleID %in% paired_samples)
  
  
  #rows_with_values_greater_than_001 <- apply(plot_df$abund > 0.01, 1, any)
  #tax <- rownames(plot_df$abund[rows_with_values_greater_than_001, ])
  
  
  plot <- plot_df %>% 
    #amp_subset_taxa(tax_vector = tax) %>% 
    amp_ordinate(sample_color_by = env_factor, 
                 transform = "none", 
                 distmeasure = "bray", 
                 filter_species = 0.01, 
                 #envfit_signif_level = 0.1,
                 type = "pcoa", 
                 #sample_shape_by = "Plant", 
                 sample_point_size = 1, 
                 #envfit_factor = env_factor, 
                 #envfit_numeric = "date_num",
                 #envfit_textcolor = "black", 
                 #envfit_textsize = 7, 
                 #sample_label_by = "SampleID",
                 sample_colorframe = T#,
                 #species_plot = T, 
                 #species_nlabels = 7 
    ) +
    scale_color_manual(values = season_colors) + 
    scale_fill_manual(values = season_colors) +
    #geom_point(aes(fill = Plant, #shape = Plant
    #), size = 3, stroke = 1, alpha = 0.8) +
    #scale_xaringan_color_discrete(aes(alpha = Location)) +
    #scale_xaringan_fill_discrete() + 
    # scale_fill_gradientn(colors = month_colors, 
    #                      breaks= 1:12, 
    #                      labels = month_names
    #                      ) +
    #scale_shape_manual(values = c(22, 22, 22, 22), 
    #                   #labels = c("After", "Before")
    #) +
  #labs(title = paste0('__', plant, "__")) +
  #theme_xaringan(css_file = "xaringan-themer.css") + 
    theme(legend.text = element_markdown(size = 20, color = "black"), 
          legend.key.size = unit(8, "pt"),
          legend.title = element_markdown(size = 20, color = "black"), 
          axis.title = element_markdown(color = "black", size = 16),
          legend.position = "none", 
          plot.subtitle = element_markdown(size = 20, lineheight = 0.0001, 
                                           margin=margin(b = unit(1, "mm")), color = "black"),
          axis.text = element_markdown(size = 20, color = "black"), 
          #legend.box = "horizontal", 
          axis.ticks = element_line(size = 0.01), 
          plot.title = element_markdown(size = 24, color = "black", 
                                        margin=margin(),lineheight = 0.0001),
          panel.grid.major = element_line(linewidth = 0.05), 
          panel.grid.minor = element_blank()
    ) +
    guides(#shape = "none", 
      color = "none",
      fill = guide_legend(title = "__Season__", 
                          nrow = 1, vjust = 1, order = 0, 
                          override.aes=list(shape=c(21), fill = season_colors, alpha = 1, stroke = 1.1)),
      # fill = guide_legend(title = "__Plant__", 
      #                     nrow = 2, vjust = 1, order = 0, 
      #                     override.aes=list(shape=c(1)) 
      #                     #guide_legend(override.aes=list(shape=c(22,22), 
      #                     #                                        fill = "white", 
      #                     #                                        color = c("black", "gray80"), 
      #                     #                                        stroke = 2),
      #                     #                      title = "__Primary<br>Settler__", 
      #                     #                      direction = "vertical", 
      #                     #                      title.position = "left",
      #                     #                      vjust = 1, nrow = 2, order = 1
      # )
    )
  
  # plot[["layers"]][[3]][["data"]][["Name"]] = c("Ejby M?lle",
  #                                               "Esbjerg West",
  #                                               "Randers",     
  #                                               "Aalborg West")
  # 
  
  bc.dist.matrix <- vegan::vegdist(t(plot_df$abund), method = "bray")
  metadata <- plot_df$metadata
  x <- vegan::adonis2(bc.dist.matrix ~ s_, data = metadata)
  a = paste0("Season: R<sup>2</sup>=", round(x$R2[1],2), ", p<", x$`Pr(>F)`[1])
  print(a)
  
  
  # if (plant != "xRanders"){
  #   plot[["layers"]][[3]][["data"]][["Name"]] = c("Winter", "Spring", "Summer", "Autumn")
  #   plot
  # }  else {
  #     plot
  # }
  
  plot + labs(subtitle = paste0(a))
  
  
  
  
}


