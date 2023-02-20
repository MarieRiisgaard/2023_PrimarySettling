#######################################################################
# Results plots
#####################################################################

# Set WD
setwd("C:/Users/marie/Desktop/visual_studio_code/2023_PrimarySettling")

## Load data into ampvis format
DataPath <- "C:/Users/marie/Desktop/visual_studio_code/2023_PrimarySettling/data/"
OutputPath <- "C:/Users/marie/Desktop/visual_studio_code/2023_PrimarySettling/output/"
SourcePath <- "C:/Users/marie/Desktop/visual_studio_code/2023_PrimarySettling/scripts/"


#################################
# Load packages 
################################
pacman::p_load(
  patchwork,
  ggtext, 
  ComplexUpset,
  lubridate,
  ggrepel,
  scales,
  xaringanthemer
)

style_duo_accent(
  primary_color = "#211a52", secondary_color = "#594fbf", inverse_background_color = "#54616e", 
  text_font_google = google_font("Barlow"))


#################################################
# Load data
#################################################

#Load workspace back to RStudio
load(paste0(OutputPath, "R_environments/","Environment_20221025.RData"))

# Generate dataframe from scratch 
  # source(paste0(SourcePath, "load_data.R"))
  # data <- master_function(reads_randers = 60000,
  #                         reads_other = 60000,
  #                         rarefy = T,
  #                         save_non_merged_ampvis = T)
  # save.image(file=paste0(OutputPath, "/R_environments/", "Environment_", format(Sys.Date(), format = "%Y%m%d"), ".RData"))


# Load results from wilcox_test.R
data_genus_random_subsamling <- 
  read.csv(paste0(OutputPath, "files/Genus_wilcox_test_2022-09-06_rare_60000_n_min12_obs_BEFORE_cross_all_24_sample_pairs.txt")) %>% mutate(
  Sign = if_else(mean_log2 > 0, "Increase", "Decrease"),
  Sign = if_else(p_adjust > 0.05, "Insignificant", Sign)) 

data_genus_all_samples <- 
  read.csv(paste0(OutputPath, "files/genus_wilcox_test_2022-10-06_rare_60000_min_12_all_samples.txt")) %>% 
  mutate(
    Sign = if_else(mean_log2 > 0, "Increase", "Decrease"),
    Sign = if_else(p_adjust > 0.05, "Insignificant", Sign)) 

data_species_all_samples <- 
  read.csv(paste0(OutputPath, "files/species_wilcox_test_2022-10-06_rare_60000_min_12_all_samples.txt"))  %>% 
  mutate(
    Sign = if_else(mean_log2 > 0, "Increase", "Decrease"),
    Sign = if_else(p_adjust > 0.05, "Insignificant", Sign))



############################################
# Figure 1: COD removal
#############################################

data[[2]] %>% 
  mutate(COD_removal =
           ((COD_beforePS-COD_afterPS)/COD_beforePS)*100) %>% 
  filter(COD_removal > 0) %>% 
  select(Plant, COD_removal, COD_afterPS, COD_beforePS, Date_rawdata) %>% 
  filter(!is.na(COD_afterPS)) %>% 
  filter(!is.na(COD_beforePS)) %>% 
  pivot_longer(cols = c(COD_beforePS, COD_afterPS, COD_removal), 
               names_to = "name",
               values_to = "values") %>% 
  group_by(name, Plant) %>% 
  mutate(quant25 = quantile(values, probs = 0.25), 
         quant75 = quantile(values, probs = 0.75),
         iqr = quant75-quant75, 
         Plant = factor(Plant, 
                        levels = c("Aalborg West", "Ejby Mølle", "Esbjerg West", "Randers")),
  ) %>% 
  ungroup() %>% 
  group_by(Plant, name) %>% 
  mutate(values = if_else(name == "COD_removal", values*15, values), 
         name = factor(name,
                       levels = c("COD_beforePS", "COD_afterPS", "COD_removal"),
                       labels = 
                         c("COD before<br>primary settling<br>[mg/L]",
                           "COD after<br>primary settling<br>[mg/L]", 
                           "COD-removal [%]"))) %>% 
  ggplot() + 
  geom_jitter(aes(x = Plant, y = values, color = name), 
              position = position_jitterdodge(jitter.width = 0.20), size = 1) + 
  geom_boxplot(aes(x = Plant, y = values, 
                   color = name, fill = name),
               alpha = 0.8,
               position = position_dodge(), outlier.shape = NA) + 
  scale_fill_manual(values = c("gray30", "#708090", "gray70")) +
  scale_color_manual(values = c("gray30", "#708090", "gray70")) +
  theme_xaringan(css_file = "xaringan-themer.css") +
  #scale_fill_manual(values = c("white", "white","#211a52")) +
  #facet_wrap(~Plant, ncol = 4, scales = "free_x") +
  scale_y_continuous(expand = c(0,100),
                     "COD [mg/L]", 
                     sec.axis = sec_axis(~ . /15, name = "COD-removal [%]", 
                                         labels = c(seq(0,100,by=20)),
                                         breaks = c(seq(0,100,by=20)))
  ) + 
  guides(fill = guide_legend()) +
  theme(#axis.ticks.y.right = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "right", 
        axis.text.y = element_text(size = 30, color = "black"), 
        #axis.text.x = element_blank(),
        axis.text.x = element_text(size = 36, color = "black"), 
        legend.text = element_markdown(size = 32, color = "black",margin = margin(t = 10, b = 5), lineheight = 0.001), 
        legend.title = element_blank(), 
        axis.line = element_line(color = "black", linewidth = 0.4),
        axis.ticks.y.left = element_line(color = "black", linewidth = 0.4),
        axis.ticks.y.right = element_line(color = "black", linewidth = 0.4),
        axis.ticks.x = element_line(color = "black", linewidth = 0.4),
        axis.title.y.left = element_markdown(size = 32, color = "black"),
        axis.title.y.right = element_markdown(size = 32, color = "black"), 
        panel.grid.major = element_line(linewidth = 0.2),
        panel.grid.minor.x =element_line(linewidth = 0.2),
        panel.spacing = unit(0,'lines'))


ggsave(paste0(OutputPath, "plots/results/COD_removal.png"), width = 8, height = 4, units = "in")
ggsave(paste0(OutputPath, "plots/results/COD_removal.pdf"), width = 8, height = 4, units = "in", scale = 2)



############################################
# Figure 2: Overall PCA
#######################################

# Plot function: 
PCA_plantwise <- source(paste0(SourcePath, "PCA_plots.R"), local = knitr::knit_global())

amp_merged_species <- data[[4]] 
all <- 
  PCA_paired_season_color("Aalborg West", amp_object = amp_merged_species) + labs(title = paste0("__Aalborg West__ (n=36)")) +
  PCA_paired_season_color("Ejby Mølle", amp_object = amp_merged_species) + labs(title = paste0("__Ejby Mølle__ (n=29)")) +
  PCA_paired_season_color("Esbjerg West", amp_object = amp_merged_species) + labs(title = paste0("__Esbjerg West__ (n=24)")) +
  PCA_paired_season_color("Randers", amp_object = amp_merged_species) + labs(title = paste0("__Randers__ (n=42)")) 

all + plot_layout(guides = "collect") & theme(legend.position = "bottom",
                                              legend.box = "horizontal")

ggsave(paste0(OutputPath, "plots/results/overall_PCA.png"), width = 7.5, height = 7, units = "in")


############################################
# Figure 3: Influent streams
#######################################

plot_all_influent_streams(amp_object = amp_merged_species) + 
  plot_all_influent_streams(amp_object = amp_merged_species, primary_settler = "After") + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom",legend.box = "horizontal")

ggsave(paste0(OutputPath, "plots/results/all_influentstreams.png"), width = 10, height = 5.5, units = "in")


####################################
# Figure 5: Upset plot
###################################

source(paste0(SourcePath, "upset_plot.R"))

upset_plot(data_genus_all_samples)

ggsave(paste0(primarysettling_folder, "output/plots/PrimarySettling_article/upset.png"), width = 7.5, height = 4)


####################################
# Figure 4 + S10: Sankey
###################################

source(paste0(SourcePath, "sankey_plot.R"))

## All 
san_key_plot("Randers") + theme(axis.text.x = element_blank()) +
  san_key_plot("Ejby Mølle")  + theme(axis.text.x = element_blank())+ 
  san_key_plot("Esbjerg West")  + theme(axis.text.x = element_blank()) +
  san_key_plot("Aalborg West") + theme(axis.text.x = element_markdown(size = 26, lineheight = 0.3)) +
  plot_layout(ncol = 1)

ggsave(paste0(OutputPath, "plots/supplementary_results/sankey.png"), width = 6.5, height = 6.5,  units = "in")

## Only Esbjerg West
san_key_plot("Esbjerg West") + theme(axis.text.x = element_markdown(size = 26, lineheight = 0.3)) +
  plot_layout(ncol = 1)

ggsave(paste0(OutputPath, "plots/results/sankey_Esbjerg.png"), width = 6.5, height = 2,  units = "in")

rm(san_key_plot)


###################################################
# Figure 6: Stacked bar with cummulative abundance
###################################################

abun_tested_genus <- 
  data[[3]] %>% 
  mutate(samples = map(samples, ~
                         distinct(., Genus, rel_abun_genus))) %>% 
  unnest(samples) %>%
  left_join(., data_genus_all_samples %>% 
              group_by(Plant, Sign) %>% 
              mutate(n = n()) %>% ungroup() %>% 
              rename("Genus" = "Tax"), by = c("Genus", "Plant")) %>% 
  mutate(tested = ifelse(is.na(Sign), 
                         "Not tested", 
                         Sign)) %>% 
  group_by(SampleID, Date_rawdata, tested, Plant, PrimarySettler, n) %>% 
  summarise(sum_abun = sum(rel_abun_genus), .groups = "drop") %>% 
  group_by(Plant, PrimarySettler, tested, n) %>%
  summarise(mean_abun = mean(sum_abun), 
            sd = sd(sum_abun)) %>% 
  mutate(Tax = "Genus")

abun_tested_genus %>%
  group_by(Plant, PrimarySettler, Tax) %>% 
  filter(Tax == "Genus") %>% 
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
  facet_wrap(~Plant, nrow = 1) +
  geom_text(size = 8, position = position_stack(vjust = 0.6), lineheight = 0.22) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c(#"#797597",  "#6F8FAF", "#708090", "gray80"),
    "Not tested"="gray45","Decrease"="#CE7E7E", 
    "Increase"="#6F8FAF", "Insignificant"="gray85")
  ) +
  ylab("Relative abundance [%]") +
  theme_xaringan(css_file = "xaringan-themer.css") +
  theme(#axis.ticks.y.right = element_blank(),
    strip.text.x = element_text(size = 28, lineheight = 0.0005, margin = margin(b = 1, t = 1), color = "gray10"),
    axis.text.y = element_text(size = 20, color = "gray10"),
    axis.ticks.x = element_blank(), 
    axis.title.y = element_markdown(size = 24, color = "gray10",
                                    linewidth = 0.00000001, lineheight = 0.0001),
    #axis.title.x = element_markdown(size = 24), 
    axis.text.x = element_markdown(size =20,linewidth = 0.00000001, lineheight = 0.0001, color = "gray10"),
    axis.title.x = element_blank(), 
    legend.position = "bottom", 
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_markdown(margin = margin(t = 0.1, b = 0.1), color = "gray10"), 
    legend.title = element_blank(), 
    axis.line.y = element_line(color = "gray10", linewidth = 0.2),
    axis.ticks.y = element_line(color = "gray10", linewidth = 0.2),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(linewidth = 0.2, color = "gray90")
  )

ggsave(paste0(OutputPath, "plots/results/stackedbar_cum_abun_genus.png"), width =5, height = 3, units = "in")


####################################
# Figure 7 + S9: Fold change
###################################

source(paste0(SourcePath, "fold_change_plot.R"))

plot_article
ggsave(paste0(OutputPath, "plots/results/foldchange.png"), width = 8.5, height = 8.8,  units = "in")

plot_supplementary
ggsave(paste0(OutputPath, "plots/supplementary_results/foldchange_common.png"), width = 8.8, height = 10,  units = "in")

rm(plot_supplementary, plot_article)






