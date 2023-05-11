#######################################################################
# Supplementary results plots
#####################################################################

# Load packages 

pacman::p_load(tidyverse, 
               ggplot2, 
               ggtext 
               )



#############################################################
######################   S1     ############################# 
#############################################################

# Timeline over samples
samples <- data[[3]] %>% 
  mutate(Plant = factor(Plant, 
                        levels = c("Aalborg West", "Ejby Mølle", "Esbjerg West", "Randers")),
         Plant = fct_rev(Plant)) %>% 
  distinct(SampleID, Date_rawdata, Plant) %>% 
  #filter(Plant != "Ejby Mølle") %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata)) %>% 
  #group_by(Plant, PrimarySettler, Location, Date_rawdata) %>% 
  #summarise(n_samples = n(), .groups = "drop") %>%  
  ggplot(aes(x = Date_rawdata, y = Plant, fill = Plant)) + 
  geom_point(size = 2, shape = 21) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  #theme_xaringan(css_file = "xaringan-themer.css") +
  #scale_xaringan_color_discrete() +  
  #scale_xaringan_fill_discrete() +
  scale_fill_manual(values = rev(c("gray27", "#804600", "#6F8FAF", "gray70"))) +
  xlab("Date") +
  theme(
    axis.text.y = element_markdown(size = 22, color = "black"),
    axis.title.x = element_markdown(size = 22, color = "black"), 
    axis.text.x = element_markdown(size = 18, color = "black"), 
    #strip.background = element_rect(fill = "#C2CBD0"), 
    #strip.text = element_text(size = 15, face = "bold", color = "black"),
    legend.text = element_text(size = 15), 
    legend.title = element_markdown(size = 15),
    axis.title = element_blank(), 
    axis.ticks.x = element_line(color = "black", linewidth = 0.2),
    axis.ticks.y = element_line(color = "black", linewidth = 0.2),
    axis.line = element_line(color = "black", linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey80"),
    panel.grid.minor.x =element_line(linewidth = 0.2, color = "grey80"),
    panel.background = element_rect(fill = "white")
  ) +
  guides(fill = "none") 

# Number of samples
samples2 <- 
  data[[3]] %>% 
  distinct(Date_rawdata, Plant) %>% 
  mutate(Plant = factor(Plant, 
                        levels = c("Aalborg West", "Ejby Mølle", "Esbjerg West", "Randers")), 
         Plant = fct_rev(Plant)) %>% 
  group_by(Plant) %>% 
  summarise(n_samples = n(), .groups = "drop") %>% 
  ggplot(aes(x = n_samples, y = Plant, fill = Plant)) + 
  geom_col() + 
  geom_label(aes(label = n_samples, x = 10), size = 8,color = "black", fill = "grey80" ) +
  #scale_xaringan_color_discrete() +  
  #scale_xaringan_fill_discrete() +
  #theme_xaringan(css_file = "xaringan-themer.css") +
  scale_fill_manual(values = rev(c("gray27", "#804600", "#6F8FAF", "gray70"))) +
  xlab("Number of sample pairs") +
  scale_x_continuous(expand = c(0,0)) +
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(size = 22, color = "black"),
    axis.title.x = element_markdown(size = 22, color = "black"), 
    axis.text.x = element_markdown(size = 18, color = "black"), 
    #strip.background = element_rect(fill = "#C2CBD0"), 
    #strip.text = element_text(size = 15, face = "bold", color = "black"),
    legend.text = element_text(size = 15), 
    legend.title = element_markdown(size = 15),
    axis.title = element_blank(), 
    axis.ticks.x = element_line(color = "black", linewidth = 0.2),
    axis.ticks.y = element_line(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey80"),
    panel.grid.minor.x =element_line(linewidth = 0.2, color = "grey80"),
    panel.background = element_rect(fill = "white")
  ) 


#Make map over WWTP
plant_coordinates <- tibble(
  Plant = c("Aalborg West", "Randers", "Ejby Mølle", "Esbjerg West"), 
  lon = c(57.04801344945664, 56.45395355374147, 55.3975822511686, 55.488027086861095),
  lat = c(9.865426704108463, 10.07086020271391, 10.417051385497906, 8.430713970155672)) %>% 
  mutate(Plant = factor(Plant, 
                        levels = c("Aalborg West", "Ejby Mølle", "Esbjerg West", "Randers")))

map_limits <- c(8, 54.74, 11.25, 57.8)
map <- ggmap::ggmap(ggmap::get_stamenmap(map_limits, zoom = 9,  maptype = "toner-lite"))

map <-  
  map + 
  geom_point(data = plant_coordinates, 
             aes(x = lat, y = lon, fill = Plant), shape = 21, size = 3, color = "black"
  ) + 
  #theme_xaringan(css_file = "xaringan-themer.css") +
  #scale_xaringan_fill_discrete() +
  #scale_xaringan_color_discrete() +
  scale_fill_manual(values = rev(c("gray27", "#804600", "#6F8FAF", "gray70"))) +
  geom_label_repel(data = plant_coordinates, 
                   aes(label = Plant, lat, lon, fill = Plant, color = Plant), seed = 123,
                   size = 10, max.overlaps = 4, 
                   label.size = 0,
                   box.padding = 0.5,
                   label.padding = 0,
                   point.padding = 5,
                   #min.segment.length = 10, 
                   alpha = 0.3,
  ) + 
  geom_text_repel(data = plant_coordinates, 
                  aes(label = Plant, lat, lon), seed = 123, 
                  size = 10, 
                  max.overlaps = 4, 
                  box.padding = 0.5, 
                  point.padding = 5,
                  #min.segment.length = 10, 
                  #color = "black"
  ) + 
  guides(fill = "none", color = "none") + 
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) 

design <- c(
  area(1, 1),
  area(1, 2),
  area(1, 3),
  area(1, 4, 2),
  area(2,1),
  area(2,2)
)

tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.53, y = 0.93, label = "**A**"), size=20, fill = NA, label.color = NA) + 
  theme_void() + 
  samples +
  tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.53, y = 0.93, label = "**C**"), size=20, fill = NA, label.color = NA) + 
  theme_void() +
  map +
  tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.53, y = 0.93, label = "**B**"), size=20, fill = NA, label.color = NA) + 
  theme_void() +
  samples2 + 
  plot_layout(ncol = 4, widths = c(0.3, 6, 0.3, 3), design = design) #& theme(strip.placement = NULL)


ggsave(paste0(OutputPath, "plots/supplementary/FigureS1_map_", Sys.Date(),".png"), width = 8.5, height = 4)
#ggsave(paste0(primarysettling_folder, "/output/plots/PrimarySettling_article/supplementary/map.png"), width = 8.5, height = 4)

rm(map, plant_coordinates, map_limits, samples, samples2)


#############################################################
######################   S2   ############################### 
#############################################################


# library(GGally)
# library(ggcorrplot)
# library(corrplot)
# library(glue)
# pacman::p_load(rstatix)


dry <- data[[2]] %>% 
  filter(DMI_rain_mm == 0) %>% 
  group_by(Plant) %>% 
  summarise(n = n(), 
            dry_flow = mean(Flow_beforePS_m3, na.rm = T), 
            sd = sd(Flow_beforePS_m3, na.rm = T))


correlation_data <- data[[2]] %>% 
  left_join(., dry, by = "Plant") %>% 
  mutate(COD_removal =
           ((COD_beforePS-COD_afterPS)/COD_beforePS)*100, 
         p_dry_weather = Flow_beforePS_m3/dry_flow*100) %>% 
  filter(COD_removal > 0) %>% 
  select(Plant, COD_removal, COD_afterPS, COD_beforePS, Date_rawdata, Flow_beforePS_m3, p_dry_weather) %>% 
  filter(!is.na(COD_afterPS)) %>% 
  filter(!is.na(COD_beforePS)) %>% 
  mutate(Plant = factor(Plant, 
                        levels = c("Aalborg West", "Ejby Mølle", "Esbjerg West", "Randers")),
  ) %>% 
  ungroup() %>%  
  select(Plant, Date_rawdata, 
         Flow_beforePS_m3,
         COD_removal, p_dry_weather) %>% 
  select(-Date_rawdata)

# Flow 
flow <- 
  data[[2]] %>% 
  left_join(., dry, by = "Plant") %>% 
  mutate(COD_removal =
           ((COD_beforePS-COD_afterPS)/COD_beforePS)*100, 
         p_dry_weather = Flow_beforePS_m3/dry_flow*100, 
  ) %>% 
  mutate(Plant = factor(Plant, 
                        levels = c("Aalborg West", "Ejby Mølle", "Esbjerg West", "Randers"))) %>% 
  ggplot(aes(y = Flow_beforePS_m3, x = Plant, fill = Plant)) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.6) + 
  geom_jitter(shape = 21, size =0.8, alpha = 0.7, color = "black",
              position = position_jitterdodge(jitter.width = 0.9), aes(fill = Plant)) +
  geom_point(aes(Plant, dry_flow), shape=95, size=40, color = "darkred") +
  facet_grid(~Plant, scales = "free_x") +
  #theme_xaringan(css_file = "xaringan-themer.css") +
  scale_fill_manual(values = rev(c("gray20", "#804600", "#6F8FAF", "gray60"))) +
  scale_color_manual(values = rev(c("gray20", "#804600", "#6F8FAF", "gray60"))) +
  labs(y = "Flow before primary settling<br>[m<sup>3</sup>/day]") +
  guides(fill = guide_legend(fill = "none", color = "none")) +
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(size = 24, color = "black"),
    axis.title.y = element_markdown(size = 28, color = "black", linewidth = 0.00000001, lineheight = 0.1), 
    axis.title.x = element_blank(),
    axis.text.x = element_blank(), #element_markdown(size = 18, color = "black"), 
    axis.ticks.y = element_line(color = "black", linewidth = 0.2),
    axis.ticks.x = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.2), 
    panel.grid.minor = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text.x = element_text(size = 28),
  )

# COD removal 
COD <- 
  correlation_data %>% 
  ggplot(aes(y = COD_removal, x = Plant, fill = Plant)) + 
  geom_jitter(position = position_jitterdodge(jitter.width = 0.20), shape = 21, color = "black") + 
  geom_boxplot(alpha = 0.8,
               position = position_dodge(), outlier.shape = NA) + 
  #scale_xaringan_color_discrete() +  
  #scale_xaringan_fill_discrete()+
  #theme_xaringan(css_file = "xaringan-themer.css") +
  scale_color_manual(values = rev(c("gray20", "#804600", "#6F8FAF", "gray60"))) +
  scale_fill_manual(values = rev(c("gray20", "#804600", "#6F8FAF", "gray60"))) +
  scale_y_continuous("COD-removal [%]") + 
  guides(fill = "none") +
  facet_grid(~Plant, scales = "free_x") +
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(size = 24, color = "black"),
    axis.title.y = element_markdown(size = 28, color = "black", linewidth = 0.00000001, lineheight = 0.1),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(), #element_markdown(size = 18, color = "black"), 
    axis.ticks.y = element_line(color = "black", linewidth = 0.2),
    axis.ticks.x = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.2), 
    panel.grid.minor = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text.x = element_text(size = 28)
  )


# Check normality proir to correlation analysis ###
### Conclusion --> not normal --> use of spearman correlation
check_normality <- function(varible){
  correlation_data %>% 
    group_by(Plant)%>% 
    select(Flow_beforePS_m3, p_dry_weather, Plant, COD_removal) %>%
    rstatix::shapiro_test({{varible}}) %>% print()
  
  correlation_data %>% 
    group_by(Plant)%>% 
    select(Flow_beforePS_m3, p_dry_weather, Plant, COD_removal) %>%
    ggplot(aes(sample = {{varible}}, color = Plant)) +
    stat_qq() + 
    stat_qq_line()
}
check_normality(Flow_beforePS_m3)
check_normality(p_dry_weather)
check_normality(COD_removal)


x_lab = 75
y_lab = 87 
# Calculate correlation with p-values
result_correlation <- correlation_data %>% 
  group_by(Plant)%>% 
  select(Flow_beforePS_m3, p_dry_weather, Plant, COD_removal) %>%
  filter(!is.na(Flow_beforePS_m3), !is.na(COD_removal)) %>% 
  summarise(COR = stats::cor.test(Flow_beforePS_m3, COD_removal, method = "spearman")$estimate,
            pval = stats::cor.test(Flow_beforePS_m3, COD_removal, method = "spearman")$p.value
            , .groups = "drop") %>% 
  mutate(corr = paste0("Spearman correlation: ", round(COR, 2), "\n",  "p-value: ", round(pval, 3)),
         x  = c(x_lab, x_lab, x_lab, x_lab),
         y  = c(y_lab, y_lab, y_lab, y_lab))


coor_plot <- 
  correlation_data %>% 
  group_by(Plant)%>% 
  select(Flow_beforePS_m3, p_dry_weather, Plant, COD_removal) %>%
  filter(!is.na(Flow_beforePS_m3), !is.na(COD_removal)) %>% 
  ggplot(aes(p_dry_weather, COD_removal, color = Plant)) + 
  geom_point() + 
  facet_grid(~Plant, scales = "free_y") +
  geom_smooth(method = "lm", se = F) + 
  geom_text(data = result_correlation, 
            mapping = aes(x = x, y = y, 
                          label = corr),
            hjust   = 0, lineheight = 0.25,
            #vjust   = -1, 
            size = 12
  ) + 
  guides(label = "none", color = "none") +
  #theme_xaringan(css_file = "xaringan-themer.css") +
  scale_color_manual(values = rev(c("gray20", "#804600", "#6F8FAF", "gray60"))) +
  labs(x = "% of dry weather flow", 
       y = "COD removal [%]") + 
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(size = 24, color = "black"),
    axis.title.y = element_markdown(size = 28, color = "black", linewidth = 0.00000001, lineheight = 0.1),
    axis.title.x = element_text(size = 28, color = "black"),
    axis.text.x = element_text(size = 24, color = "black"), 
    axis.ticks = element_line(color = "black", linewidth = 0.2),
    axis.line = element_line(color = "black", linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.2), 
    panel.grid.minor = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text.x = element_text(size = 28)
  )


design <- c(
  area(1, 1),
  area(1, 2),
  area(1, 3),
  area(1, 4),
  area(2,1),
  area(2,2,2,4)
)


ggplot() + scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.53, y = 0.98, label = "**A**"), size=24, fill = NA, label.color = NA) + 
  theme_void() + 
  flow + 
  ggplot() + scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.53, y = 0.98, label = "**B**"), size=24, fill = NA, label.color = NA) + 
  theme_void() +
  COD + 
  ggplot() + scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.53, y = 0.98, label = "**C**"), size=24, fill = NA, label.color = NA) + 
  theme_void() +
  coor_plot + 
  plot_layout(ncol = 4, design = design, widths = c(0.2,3,0.2,2), heights = c(1,1,3))




ggsave(paste0(OutputPath, "plots/supplementary/FigureS2_flow-correlation_and_COD_removal_", Sys.Date(),".png"), width = 10, height = 8)

rm(design, flow, COD, coor_plot, check_normality, x_lab, y_lab, correlation_data)


#############################################################
######################   S3   ############################### 
#############################################################

# Dimentions of primary settlers 
V = tibble(
  Plant = c("Randers",      "Ejby Mølle", "Esbjerg West",   "Aalborg West"), 
  v =     c(3*1425,          7*1200,       2*1046,           2*1900), 
  s =     c(3*3.14*13.25^2 - (3*3.14*2.55^2),  #Randers
            7*58*8,                          #Ejby Mølle"
            2*3.14*12.5^2 - (2*3.14*4^2),      # Esbjerg
            2*7.7*52.5))                       # Aalborg

plant_parameters <- data[[2]] %>% 
  distinct(Date_rawdata, Flow_beforePS_m3, Plant) %>% filter(!is.na(Flow_beforePS_m3)) %>% 
  left_join(., V, by = c("Plant")) %>% 
  group_by(Plant, v, Date_rawdata) %>% 
  mutate(Plant = factor(Plant, 
                        levels = c("Aalborg West", "Ejby Mølle", "Esbjerg West", "Randers"))) %>% 
  summarise(res = (v)/(Flow_beforePS_m3)*24, 
            SOR = Flow_beforePS_m3/s/24)


# Summary
plant_parameters %>% group_by(Plant) %>% summarise(mean(SOR), sd(SOR), mean(res), sd(res))

# Boxplot
plant_parameters %>% 
  ggplot(aes(x = Plant, y = res, color = Plant)) + 
  geom_jitter(position = position_jitterdodge(jitter.width = 0.4), alpha = 0.8, size = 0.6) +
  geom_boxplot(outlier.shape = NA, aes(fill = Plant), alpha = 0.2) +
  #theme_xaringan() +
  ylab("Residence time [h]") +
  scale_fill_manual(values = c("gray27", "#804600", "#6F8FAF", "gray70")) +
  scale_color_manual(values = c("gray27", "#804600", "#6F8FAF", "gray70")) +
  scale_y_continuous(limits = c(0,9)) +
  plant_parameters %>% 
  ggplot(aes(x = Plant, y = SOR, color = Plant)) + 
  geom_jitter(position = position_jitterdodge(jitter.width = 0.4), alpha = 0.8, size = 1, stroke = 0) +
  geom_boxplot(outlier.shape = NA, aes(fill = Plant), alpha = 0.2) +
  #theme_xaringan() +
  ylab("Surface overflow rate [m<sup>3</sup>/m<sup>2</sup>/h]") +
  scale_fill_manual(values = c("gray27", "#804600", "#6F8FAF", "gray70")) +
  scale_color_manual(values = c("gray27", "#804600", "#6F8FAF", "gray70")) +
  scale_y_continuous(limits = c(0,13)) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "A", 
  ) &
  theme(axis.title.x = element_blank(),
        plot.tag = element_text(color = "black", size = 80, face = "bold"),
        axis.text = element_text(size = 32, color = "black"),
        panel.grid = element_line(linewidth = 0.2),
        axis.ticks = element_line(color = "black", linewidth = 0.2),
        axis.line = element_line(color = "black", linewidth = 0.2),
        axis.title.y = element_markdown(size = 36, color = "black"), 
        legend.position = "none")

ggsave(paste0(OutputPath, "plots/supplementary/FigureS3_SOR_ressidence_", Sys.Date(), ".png"), width = 9, height = 6, units = "in")



rm(plant_parameters, V)


#############################################################
######################   S4   ############################### 
#############################################################


stacked_bar_classified <- function(tax_level, tax_rel_abun){
  
  tax_level_name <- ifelse(str_detect(data[[3]] %>% sample_n(60) %>% unnest(samples) %>% 
                                        filter(str_detect({{tax_level}},"nclassified", T) |
                                                 str_detect({{tax_level}}, "midas", T)) %>% 
                                        select({{tax_level}}) %>% unlist(), "g__"),
                           "genera", 
                           "species")
  tax_level_name <- tax_level_name[1]
  print(tax_level_name)
  
  see2 <- data[[3]] %>% 
    mutate(samples = 
             map(.x = samples, ~
                   ungroup(.) %>% 
                   mutate(uncla = ifelse(str_detect({{tax_level}}, "unclassified"),
                                             "Unclassified ASVs", 
                                             paste0(ifelse(tax_level_name == "species", "Species", "Genus"), 
                                                    " level classification"))) %>% 
                   distinct(across(c({{tax_level}}, {{tax_rel_abun}}, uncla))) %>% 
                   filter({{tax_rel_abun}} != 0) %>% 
                   group_by(uncla) %>% 
                   summarise(sum_abun = sum({{tax_rel_abun}}), n_species = n(), .groups = "drop")
             )) %>% 
    unnest(samples) %>% 
    select(Plant,  PrimarySettler, uncla, sum_abun, n_species) %>% 
    group_by(Plant, PrimarySettler, uncla) %>% 
    summarise(mean = mean(sum_abun), mean_n = mean(n_species), 
              #sd_n = sd(n_species), sd = sd(sum_abun),
              .groups = "drop") %>% 
    pivot_longer(cols = -c(Plant, PrimarySettler, uncla), names_to = "names", values_to = "values")
  
  see3 <- see2 %>%  
    mutate(p = ifelse(uncla != "Unclassified ASVs", 0.8, 0.1)) %>% 
    #filter(Plant == "Randers") %>% 
    ggplot(aes(fill = uncla, x = names, y = values)) + 
    geom_bar(position="fill", stat="identity") + 
    facet_wrap(Plant~PrimarySettler, nrow = 1) +
    geom_text(aes(label = ifelse(names == "mean", 
                                 paste0(round(values, 0), "%"), 
                                 paste0(round(values, 0))       
    ), y = p), size = 8) + 
    scale_y_continuous(expand = c(0,0)) +
    scale_x_discrete(labels = c("mean" = "Relative<br>abundance", 
                                "mean_n" = paste0("Distinct<br>", tax_level_name))
    ) +
    scale_fill_manual(values =c("#6F8FAF", "gray70", "red")) +
    guides(fill = guide_legend()) +
    ylab("Fraction") +
    #theme_xaringan(css_file = "xaringan-themer.css") +
    theme(
      strip.text.x = element_text(size = 24, color = "black"),
      axis.text.y = element_text(size = 19, color = "black"),
      axis.title.y = element_markdown(size = 28, linewidth = 0.00000001, lineheight = 0.1, color = "black"),
      #axis.title.x = element_markdown(size = 24), 
      axis.text.x = element_markdown(size = 19, color = "black", lineheight = 0.1),
      axis.title.x = element_blank(), #element_markdown(size = 28, linewidth = 0.00000001, lineheight = 0.1, color = "black"),, 
      plot.title = element_markdown(face = "bold", size = 40, color = "black"),
      legend.position = "bottom", 
      legend.text = element_markdown(margin = margin(t = 1, b = 2), size = 20, color = "black"),
      legend.title = element_blank(), 
      axis.line = element_line(linewidth = 0.2, color = "black"),
      axis.ticks = element_line(linewidth = 0.2, color = "black"),
      panel.grid.major = element_line(linewidth = 0.2, color = "gray90"), 
    ) + 
    guides(fill = guide_legend(override.aes = list(size = 1)))
  
  tax_level_name
  see3
  
}

specoies <- stacked_bar_classified(Species, rel_abun_species)
genus <- stacked_bar_classified(Genus, rel_abun_genus)

ggplot() + scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.53, y = 0.5, label = "**A**"), size=20, fill = NA, label.color = NA) + 
  theme_void() + 
  plot_spacer() + plot_spacer() + 
  specoies + 
  ggplot() + scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.53, y = 0.5, label = "**B**"), size=20, fill = NA, label.color = NA) + 
  theme_void() + 
  plot_spacer() + plot_spacer() + 
  genus + 
  plot_layout(ncol = 2, widths = c(0.1,3), heights = c(0.18,1, 0.18,1)) 


ggsave(paste0(OutputPath, "plots/supplementary/FigureS4_classified_genera_", Sys.Date(), ".png"), width = 22.5, height = 14, units = "in")

#ggsave(paste0(primarysettling_folder, "/output/plots/PrimarySettling_article/supplementary/classified_genus.png"), width =9, height = 7, units = "in")


rm(stacked_bar_classified, specoies, genus)



#############################################################
######################   S5   ############################### 
#############################################################

rank_abundance <- function(plant){
  amp_merged_speces <- data[[4]]
  plot_ran <-amp_merged_species %>% 
    ampvis2::amp_subset_samples(Plant == plant, normalise = F) %>% 
    ampvis2::amp_rank_abundance(group_by = c("PrimarySettler"))
  plot_ran + 
    #theme_xaringan(css_file = "xaringan-themer.css") +
    scale_fill_manual(values = c("#6F8FAF", "gray50", "gray70", "red")) +
    scale_color_manual(values = c("#6F8FAF", "gray50", "gray70", "red")) +
    theme(
      strip.text.x = element_text(size = 28, color = "black"),
      axis.text.y = element_text(size = 24, color = "black"),
      axis.title.y = element_markdown(size = 28, linewidth = 0.00000001, lineheight = 0.1, color = "black"),
      axis.text.x = element_text(size = 24, color = "black"),
      axis.title.x = element_markdown(size = 28, linewidth = 0.00000001, lineheight = 0.1, color = "black"),
      plot.title = element_markdown(face = "bold", size = 40, color = "black"),
      legend.position = "bottom", 
      legend.text = element_markdown(margin = margin(t = 10, b = 5), size = 28, color = "black"),
      legend.title = element_blank(), 
      axis.line = element_line(linewidth = 0.2, color = "black"),
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_line(linewidth = 0.2, color = "grey80"),
      #panel.grid.minor.x =element_line(linewidth = 0.2, color = "grey80"),
      panel.background = element_rect(fill = "white")
    ) +
    scale_x_continuous(limits = c(1, 25000), trans = "log10", expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + 
    labs(title = paste0(plant))
}

p1 <-rank_abundance("Aalborg West")
p2 <-rank_abundance("Esbjerg West")
p3 <-rank_abundance("Ejby Mølle")
p4 <-rank_abundance("Randers")

p1 + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  p3 + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
             axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  p2 +  
  p4 + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")

ggsave(paste0(OutputPath, "plots/supplementary/FigureS5_rank_abundance_20230510.png"), width =7, height = 6, units = "in")




#############################################################
######################   S6   ############################### 
#############################################################


plot_functional_guild_tax <- function(data, tax_level = Genus, rel_abun_tax = rel_abun_genus, n_tax, primary_settler = "Before"){
  
  
  tax_vec <- data %>% 
    filter(str_detect(PrimarySettler, primary_settler)) %>% 
    mutate(samples = map(samples, ~ 
                           distinct(., {{tax_level}},{{rel_abun_tax}}))) %>% 
    unnest(samples) %>% 
    group_by(across(c({{tax_level}}, Plant))) %>% 
    summarise(median = median({{rel_abun_tax}}, na.rm = T), .groups = "drop") %>% 
    rename("tax_column" = {{tax_level}}) %>% 
    group_by(Plant) %>% 
    arrange(desc(median)) %>% slice_max(n=n_tax, order_by = median) %>% 
    ungroup() %>% distinct(tax_column) %>% unlist()
  
  tidy_all_functional_guild <- data %>% 
    filter(PrimarySettler == primary_settler) %>% 
    mutate(samples = map(samples, ~ 
                           distinct(., {{tax_level}}, {{rel_abun_tax}}, Genus))) %>%   ## Unnester
    unnest(samples) %>% 
    mutate(Plant = factor(Plant, levels = c("Aalborg West", "Ejby Mølle","Esbjerg West", "Randers"))) %>% 
    group_by(across(c({{tax_level}}, Plant))) %>% 
    summarise(median = median({{rel_abun_tax}}), .groups = "drop", Plant, Genus) %>%  ##Mean cross all samples
    distinct() %>% 
    filter(median > 0) %>%
    mutate(pool_taxa = if_else(!{{tax_level}} %in% tax_vec | str_detect({{tax_level}}, "nclassified"), 
                               TRUE, FALSE)) %>% # Pooling all unclassified 
    mutate(
      Species = as.character({{tax_level}}),
      tax_lab = if_else(pool_taxa, "pool", {{tax_level}}),
      tax_lab = if_else(str_detect(tax_lab, "pool") & str_detect({{tax_level}}, "unclas") & 
                          !as.character({{tax_level}}) %in% tax_vec, 
                        "Unclassified", tax_lab), 
      tax_lab = if_else(str_detect(tax_lab, "pool") & !str_detect({{tax_level}}, "nclass") & 
                          !as.character({{tax_level}}) %in% tax_vec,  
                        "Remaining", tax_lab), 
      tax_lab = if_else(tax_lab == "pool", Species, tax_lab)
    ) %>% 
    filter(median > 0 |  pool_taxa == F) %>%               # Removing median= 0 for all pooled taxa
    group_by(Plant, tax_lab, pool_taxa) %>% 
    summarise(median_rel_abun = sum(median), count = n(), Genus) %>% 
    ungroup()
  
  x = tidy_all_functional_guild %>% 
    mutate(
      lab = tax_lab,
      lab = if_else(str_detect(lab, "midas_s_"),
                    false = if_else(str_detect(lab, "midas_g_") | str_detect(Genus, "ASV"),
                                    false = paste0("*", str_sub(lab,  start = 4), "*"),
                                    true = paste0(str_sub(Genus,  start = 4)),
                    ),
                    true = if_else(str_detect(Genus, "midas_g_") | str_detect(Genus, "ASV"), 
                                   paste0(str_sub(Genus,  start = 4), ";" , str_sub(lab,  start = 4)),
                                   paste0("*", str_sub(Genus,  start = 4), "*;" , str_sub(lab,  start = 4)))), 
      lab = if_else(str_detect(lab, pattern = "sified"), 
                    true = paste0("Unclassified *", str_sub(lab,  start = 15)), 
                    false = lab),
      lab = if_else(str_detect(lab, "aining"),  
                    true = paste0("__Remaining__"), 
                    false = lab), 
      lab = if_else(str_detect(lab, "Unclassified"),  
                    true = paste0("__Unclassified ASVs__"), 
                    false = lab), 
      lab = if_else(str_detect(lab, "Ca_"), str_remove_all(lab, "\\*"), lab),
      lab = if_else(str_detect(lab, "Ca_"), str_replace(lab, "Ca_", "*Ca.* "), lab),
      lab = fct_reorder(lab, median_rel_abun, .desc=F),
      lab = fct_relevel(lab, "__Unclassified ASVs__", after = 0),
      lab = fct_relevel(lab, "__Remaining__", after = 0)
    )
  
  
  # Plotting
  y = x %>%  
    ggplot(aes(
      y = lab, 
      fill=median_rel_abun, x=Plant)) + 
    geom_tile() +
    geom_text(aes(label = ifelse((count > 1), 
                                 paste0(format(round(median_rel_abun, 2)), " (", count,")"), 
                                 paste0(if_else(median_rel_abun < 0.01, " ", 
                                                format(round(median_rel_abun, 2)))))), size = 5) +
    facet_grid(. ~ Plant, scales = "free", space = "free"
    ) +  # <--- 
    labs(x = NULL, y = NULL) + 
    scale_fill_gradientn(colours = c("#75A5C6","#BCD2E8","#e2e2e2", "#f4cccc","#ea9999","#e06666"), trans = "pseudo_log",
                         name = "Relative<br>Abundance (%)",
    ) +
    guides(fill = "none") +
    scale_y_discrete(expand = c(0,0)) + 
    scale_x_discrete(expand = c(0,0)) + 
    theme(axis.text.x = element_blank(), #element_markdown(angle = 0, vjust = 0.5, size = 13), 
          axis.text.y = element_markdown(size = 13), 
          strip.text.x = element_markdown(size = 18), 
          strip.background = element_rect(color="grey90", fill="grey90", linewidth = 1.0, linetype="solid"), 
          legend.text = element_text(size = 15), 
          legend.title = element_markdown(size = 15), 
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          title = element_markdown(size = 20),
    ) 
  
  y
}


HM_before <-plot_functional_guild_tax(data = data[[3]] #%>% sample_n(8) 
                                      , n_tax = 15, primary_settler = "Before")
HM_after <-plot_functional_guild_tax(data = data[[3]] #%>% sample_n(8) 
                                     , n_tax = 15, primary_settler = "After")


design <- c(
  area(1, 1),
  area(1, 2),
  area(2, 1),
  area(2, 2)
)


tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.5, y = 0.98, label = "**A**"), size=13, fill = NA, label.color = NA) + 
  theme_void() +
  HM_before +
  tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.5, y = 0.98, label = "**B**"), size=13, fill = NA, label.color = NA) + 
  theme_void() +
  HM_after + 
  plot_layout(ncol = 2, 
              widths = c(0.7, 9),
              heights = c(8, 9),
              design = design, guides = "collect") & 
  theme(strip.placement = NULL, legend.position = "bottom", 
        legend.box = "vertical")


ggsave(paste0(OutputPath, "plots/supplementary/FigureS6_heatmap_before_and_after_", Sys.Date(), ".png"), 
       width = 4, height = 5, units = "in")

ggsave(paste0(OutputPath, "plots/supplementary/FigureS3_SOR_ressidence_", Sys.Date(), ".png"), width = 9, height = 6, units = "in")

#############################################################
######################   S7   ############################### 
#############################################################

#Ordinations: seanonallity investigation 

source(paste0(SourcePath, "/PCA_plots.R"))

amp_merged_species = data[[4]]
before <- ggplot() + 
  scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.5, y = 0.35, 
                    label = paste0("__Before__")), size=12, fill = NA, label.color = NA, lineheight = 0.1) + 
  theme_void() 
after <- ggplot() + 
  scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.5, y = 0.35, 
                    label = paste0("__After__")), size=12, fill = NA, label.color = NA) + 
  theme_void() 
both <- ggplot() + 
  scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.5, y = 0.35, 
                    label = paste0("__Before and after__")), size=12, fill = NA, label.color = NA) + 
  theme_void() 

AAW <- tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.5, y = 0.35, 
                    label = paste0("__Aalborg West__<br>(n=36)")), size=12, fill = NA, label.color = NA, lineheight = 0.35) + 
  theme_void() 
A <- plot_each_influent_stream("Aalborg West", "s_", amp_merged_species, c("Before"))
B <- plot_each_influent_stream("Aalborg West", "s_", amp_merged_species, c("After"))
C <- plot_each_influent_stream("Aalborg West", "s_", amp_merged_species, c("Before", "After"))

EB <- tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.5, y = 0.35, 
                    label = paste0("__Ejby Mølle__<br>(n=29)")), size=12, fill = NA, label.color = NA, lineheight = 0.35) + 
  theme_void() 
D <- plot_each_influent_stream("Ejby Mølle", "s_", amp_merged_species, c("Before"))
E <- plot_each_influent_stream("Ejby Mølle", "s_", amp_merged_species, c("After"))
Fi <- plot_each_influent_stream("Ejby Mølle", "s_", amp_merged_species, c("Before", "After"))

ESW <- tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.5, y = 0.35, 
                    label = paste0("__Esbjerg West__<br>(n=24)")), size=12, fill = NA, label.color = NA, lineheight = 0.35) + 
  theme_void() 
G <- plot_each_influent_stream("Esbjerg West", "s_", amp_merged_species, c("Before"))
H <- plot_each_influent_stream("Esbjerg West", "s_", amp_merged_species, c("After"))
I <- plot_each_influent_stream("Esbjerg West", "s_", amp_merged_species, c("Before", "After"))

Ran <- tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.5, y = 0.35, 
                    label = paste0("__Randers__<br>(n=42)")), size=12, fill = NA, label.color = NA, lineheight = 0.35) + 
  theme_void() 
J <- plot_each_influent_stream("Randers", "s_", amp_merged_species, c("Before"))
K <- plot_each_influent_stream("Randers", "s_", amp_merged_species, c("After"))
L <- plot_each_influent_stream("Randers", "s_", amp_merged_species, c("Before", "After"))

ggplot() + theme_void() + 
  before + after + both +
  AAW + 
  A + B + C+ 
  EB +
  D + E + Fi + 
  ESW + 
  G + H + I +
  Ran +
  J + K + L + 
  plot_layout(guides = "collect", 
              ncol = 4, 
              widths = c(2.5, 3,3,3), 
              heights = c(0.6, 3,3,3,3)) & 
  theme(legend.position = "bottom", 
        #legend.box = "vertical", 
        #legend.direction = "vertical"
  )  

ggsave(paste0(OutputPath, "plots/supplementary/FigureS7_seasonal_PCoA_", Sys.Date(), ".png"), 
       width = 6.5, height = 8)


rm(before, after, both, AAW, A, B, C,EB,D,E,Fi,ESW, G, H, I, Ran,J,K,L) 



#############################################################
######################   S8   ############################### 
#############################################################

abun_tested_species <- 
  data[[3]] %>% 
  mutate(samples = map(samples, ~
                         ungroup(.) %>% 
                         distinct(Species, rel_abun_species))) %>% 
  unnest(samples) %>%
  left_join(., data_species_all_samples %>% 
              group_by(Plant, Sign) %>% 
              mutate(n = n()) %>% ungroup() %>% 
              rename("Species" = "Tax"), by = c("Species", "Plant")) %>% 
  mutate(tested = ifelse(is.na(Sign), 
                         "Not tested", 
                         Sign)) %>%
  group_by(SampleID, tested, Plant, PrimarySettler, n) %>% 
  summarise(sum_abun = sum(rel_abun_species)) %>% 
  group_by(Plant, PrimarySettler, tested, n) %>%
  summarise(mean_abun = mean(sum_abun)) %>% 
  mutate(Tax = "Species")

abun_tested_genus_table <- 
  data[[3]] %>% 
  mutate(samples = map(samples, ~
                         ungroup(.) %>% 
                         distinct(Genus, rel_abun_genus))) %>% 
  unnest(samples) %>%
  left_join(., data_genus_all_samples %>% 
              group_by(Plant, Sign) %>% 
              mutate(n = n()) %>% ungroup() %>% 
              rename("Genus" = "Tax"), by = c("Genus", "Plant")) %>% 
  mutate(tested = ifelse(is.na(Sign), 
                         "Not tested", 
                         Sign)) %>% 
  group_by(SampleID, Date_rawdata, tested, Plant, PrimarySettler, n) %>% 
  summarise(sum_abun = sum(rel_abun_genus), .groups = "drop")

abun_tested_genus <- 
  abun_tested_genus_table %>% 
  group_by(Plant, PrimarySettler, tested, n) %>%
  summarise(mean_abun = mean(sum_abun), 
            sd = sd(sum_abun)) %>% 
  mutate(Tax = "Genus")

merged <- rbind(abun_tested_genus, abun_tested_species)

merged %>%
  group_by(Plant, PrimarySettler, Tax) %>% 
  mutate(tested = factor(tested, levels = rev(c("Not tested", 
                                                "Insignificant", "Increase", "Decrease"))),
         Plant = factor(Plant, levels = c("Aalborg West", "Ejby Mølle",
                                          "Esbjerg West", "Randers" 
         )),
         p = cumsum(mean_abun) - (0.5 * mean_abun)) %>%
  ggplot(aes(fill = tested, x = PrimarySettler, y = mean_abun, 
             label = ifelse(tested == "Not tested", sprintf("%s", paste0(round(mean_abun, 0), "%")),
                            sprintf("%s", paste0(round(mean_abun, 0), "%","\n", "(", n, ")" ))))) + 
  geom_bar(#position="fill", 
    stat="identity") + 
  facet_wrap(Plant~Tax, nrow = 1) +
  geom_text(size = 7, position = position_stack(vjust = 0.6), lineheight = 0.22) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c(#"#797597",  "#6F8FAF", "#708090", "gray80"),
    "Not tested"="gray45","Decrease"="#CE7E7E", 
    "Increase"="#6F8FAF", "Insignificant"="gray85")
  ) +
  ylab("Relative abundance [%]") +
  #theme_xaringan(css_file = "xaringan-themer.css") +
  theme(#axis.ticks.y.right = element_blank(),
    strip.text.x = element_text(size = 24, lineheight = 0.0005, margin = margin(b = 1, t = 1), color = "gray10"),
    axis.text.y = element_text(size = 20, color = "gray10"),
    axis.ticks.x = element_blank(), 
    axis.title.y = element_markdown(size = 24, color = "gray10",
                                    linewidth = 0.00000001, lineheight = 0.0001),
    #axis.title.x = element_markdown(size = 24), 
    axis.text.x = element_markdown(size =20,linewidth = 0.00000001, lineheight = 0.0001, color = "gray10"),
    axis.title.x = element_blank(), 
    legend.position = "bottom", 
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_markdown(margin = margin(t = 0.1, b = 0.1), color = "gray10", size = 24), 
    legend.title = element_blank(), 
    axis.line.y = element_line(color = "gray10", linewidth = 0.2),
    axis.ticks.y = element_line(color = "gray10", linewidth = 0.2),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(linewidth = 0.2, color = "grey80"),
    panel.background = element_rect(fill = "white")
  )

ggsave(paste0(OutputPath, "plots/supplementary/FigureS8_cummulative_abundance_tested_species", Sys.Date(), ".png"), width =6.8, height = 3, units = "in")


#############################################################
######################  Table S1   ############################### 
#############################################################

DF <- abun_tested_genus_table %>% 
  select(-n, -SampleID) %>% 
  group_by(Plant, tested) %>% 
  rstatix::t_test(sum_abun ~ PrimarySettler, p.adjust.method = "bonferroni", paired = T) %>% 
  filter(p < 0.05)

write_tsv(DF, file = paste0(OutputPath, "files/trend_cummulative_abundance_t_test.tsv"))

rm(DF, merged, abun_tested_genus_table, abun_tested_genus, abun_tested_species)

#############################################################
######################   S9   ############################### 
#############################################################

# In results.R

#############################################################
######################   S10   ############################### 
#############################################################


# In results.R

#############################################################
######################   S11  ############################### 
#############################################################

source(paste0(SourcePath, "/upset_plot.R"))

tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.53, y = 0.93, label = "**A**"), size=30, fill = NA, label.color = NA) + 
  theme_void() + 
  upset_plot(data_genus_all_samples) +
  tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.53, y = 0.93, label = "**B**"), size=30, fill = NA, label.color = NA) + 
  theme_void() +
  upset_plot(data_genus_random_subsamling)+
  plot_layout(ncol = 2, widths = c(0.15, 5))

ggsave(paste0(OutputPath, "plots/supplementary/FigureS11_upset_plot.png"), width = 10, height = 7)

rm(upset_plot)

#############################################################
######################   S12   ############################### 
#############################################################

#Making vector of plant and species that are tested for in all plants!!!!!
upset_df_filtered <- 
  data_genus_all_samples %>% 
  filter(!str_detect(Tax, "nclassified")) %>% 
  select(-X) %>% 
  mutate(
    Sign = if_else(mean_log2 > 0, "Increase", "Decrease"),
    Sign = if_else(p_adjust > 0.05, "Insignificant", Sign))

plantpair_ambigous_species <- function(plant_pair){
  #plant_pair = c("Randers", "Aalborg West")
  upset_df_filtered %>% 
    select(-mean_log2, -p_adjust) %>% 
    filter(Plant %in% plant_pair) %>% 
    group_by(Tax) %>% 
    mutate(Plant = paste0(Plant, collapse = "-"), 
           Sign = paste0(Sign, collapse = "-"), 
           Plant_pair = paste(plant_pair, collapse = "-<br>")) %>% 
    distinct() %>% 
    group_by(Plant, Sign, Plant_pair) %>% 
    summarise(n = n())
}

ambigous_species_count <- rbind(
  plantpair_ambigous_species(c("Aalborg West", "Ejby Mølle")),
  plantpair_ambigous_species(c("Aalborg West", "Esbjerg West")),
  plantpair_ambigous_species(c("Esbjerg West", "Ejby Mølle")),
  plantpair_ambigous_species(c("Randers", "Aalborg West")), 
  plantpair_ambigous_species(c("Randers", "Ejby Mølle")), 
  plantpair_ambigous_species(c("Randers", "Esbjerg West"))
)


ambigous_species_count %>% 
  mutate(common = case_when(
    Sign == "Decrease-Decrease" | Sign == "Increase-Increase" ~"Same trend",
    str_detect(Sign, "Insigni") ~ "Insignificant in one or both", 
    Sign %in% c("Increase-Decrease", "Decrease-Increase") ~ "Inconsistent trend"
  ), 
  Plant_pair = factor(Plant_pair, levels = c("Aalborg West-<br>Ejby Mølle", "Aalborg West-<br>Esbjerg West", 
                                             "Esbjerg West-<br>Ejby Mølle", "Randers-<br>Aalborg West", 
                                             "Randers-<br>Ejby Mølle", "Randers-<br>Esbjerg West"
  )),
  Sign = factor(Sign, levels = c("Increase-Increase", "Decrease-Decrease", "Increase-Decrease", "Decrease-Increase",
                                 "Decrease-Insignificant", "Increase-Insignificant", "Insignificant-Decrease", 
                                 "Insignificant-Increase", "Insignificant-Insignificant")), 
  common = factor(common, levels = c("Same trend","Inconsistent trend", "Insignificant in one or both")),
  newy=cumsum(n)
  ) %>%
  ggplot(aes(y=n, x = Plant_pair)) +
  ggpattern::geom_bar_pattern(position="stack",stat="identity",
                              mapping=aes(pattern=common, 
                                          fill = Sign,
                              ), pattern_spacing = 0.013) + 
  ggpattern::scale_pattern_manual(
    values= c("stripe", "crosshatch", "none"))+   #manually assign pattern
  geom_label(aes(y = n, label = n, fill  = Sign), size = 6.5, label.size = NA,
             color = "black", show.legend = F, alpha = 0.8, label.padding = unit(0.05, "lines"),
             position = position_stack(vjust = 0.5), family = "sans" 
  ) + 
  scale_fill_manual(values = c("#CE7E7E", "#6F8FAF", "#DAA520","#F5DEB3", "grey50","grey60", "grey70", "grey80", "grey90")) +
  scale_y_continuous(expand = c(0,0)) +
  ylab("Number of genera") + 
  #theme_xaringan(css_file = "xaringan-themer.css") + 
  theme(axis.text.x = element_markdown(angle = 0, hjust = 0.5, vjust = 1, size = 16, lineheight = 0.1, color = "gray10"), 
        axis.text.y = element_text(size = 20, color = "gray10"),
        axis.title.x = element_blank(), 
        axis.title.y = element_markdown(size = 20, color = "gray10",
                                        #linewidth = 1, lineheight = 0.1
                                        ),
        panel.grid.minor = element_blank(), 
        axis.line.y = element_line(color = "gray10", linewidth = 0.2),
        axis.ticks.y = element_line(color = "gray10", linewidth = 0.2),
        axis.ticks.x = element_blank(), 
        legend.position = "right", legend.direction = "vertical", 
        legend.text = element_markdown(size = 16, color = "black"), 
        legend.key.size = unit(8, "pt"),
        legend.title = element_markdown(size = 20, color = "black", lineheight = 0.1), 
        panel.grid.major = element_line(linewidth = 0.2, color = "grey80"),
        panel.grid.minor.x =element_line(linewidth = 0.2, color = "grey80"),
        panel.background = element_rect(fill = "white")
  ) + 
  guides(fill = guide_legend(title = "Trend<br>(Plant 1-Plant 2)", title.position = "top", title.vjust = 1, 
                             nrow = 10, order = 2,
                             override.aes = list(pattern = c("none","none","none", "none", 
                                                             "none","none","none", "none", "none")) 
  ),
  pattern = guide_legend(title = "Consistency<br>across WWTPs", 
                         title.position = "top", title.vjust = 1, nrow = 3, order = 1, 
                         override.aes = list(fill ="white", color = "black", pattern_spacing = 0.01))
  )


ggsave(paste0(OutputPath, "plots/supplementary/FigureS12_ambigous_trends.png"), 
       width = 5, height = 3.3, units = "in")


rm(ambigous_species_count, plantpair_ambigous_species)


#############################################################
######################   S13   ############################### 
#############################################################

source(file = paste0(SourcePath, "jitter_boxplot_function.R"))


p60000 <- jitter_boxplot_function(
  tidy_df = data[[3]] %>% mutate(samples = map(samples, 
                                               ~mutate(., rel_abun_genus = 
                                                         ifelse(rel_abun_genus == 0,
                                                                0.0001,
                                                                rel_abun_genus)))),
  include_tax = x %>% filter(Guild == "__Nitrifiers__") %>% filter(!Genus %in% c("g__Obscuribacter", "g__midas_g_94")) %>% distinct(Genus) %>% unlist(),
  relative_to_COD = F) + #theme(legend.position = "none") +
  coord_trans(y = "log10")


data_genus_100000_randers <- read.csv(paste0(OutputPath, "files/genus_wilcox_test_2022-09_30_rare_100000_min_12_all_samples_Randers.txt"))

load(file = paste0(OutputPath, "R_environments/Randers_rare_100000_20220930.Rdata"))    ### Called df_for_function

p100000 <- jitter_boxplot_function(tidy_df = df_for_function %>% 
                                     mutate(samples = map(samples, 
                                                          ~mutate(., rel_abun_genus = 
                                                                    ifelse(rel_abun_genus == 0,
                                                                           0.0001,
                                                                           rel_abun_genus)))), 
                                   wilcox_df = data_genus_100000_randers,
                                   include_tax = x %>% filter(Guild == "__Nitrifiers__") %>% distinct(Genus) %>% unlist(),
                                   relative_to_COD = F) + 
  coord_trans(y = "log10")


design <- c(
  area(1, 1),
  area(2, 2),
  area(3, 1),
  area(4, 2)
)


tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.5, y = 0.5, label = "**A**"), size=20, fill = NA, label.color = NA) + 
  theme_void() +
  p60000 +
  tibble(x = c(0,1), y=c(0,1)) %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
  geom_richtext(aes(x = 0.5, y = 0.5, label = "**B**"), size=20, fill = NA, label.color = NA) + 
  theme_void() +
  p100000 + 
  plot_layout(ncol = 2, 
              widths = c(1, 9),
              heights = c(1.2, 8),
              design = design, guides = "collect") & 
  theme(strip.placement = NULL, legend.position = "bottom", 
        legend.box = "vertical")


ggsave(paste0(OutputPath, "plots/supplementary_results/nitrifiers.png"), 
       width = 6, height = 7)


rm(jitter_boxplot_function, p60000, data_genus_100000_randers, design, p100000, df_for_function, x )











