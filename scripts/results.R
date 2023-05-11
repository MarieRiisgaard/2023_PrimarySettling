#######################################################################
# Results plots
#####################################################################

# Set WD
#setwd("C:/Users/marie/Desktop/visual_studio_code/2023_PrimarySettling")

## Load data into ampvis format
DataPath <- "C:/Users/HD95LP/OneDrive - Aalborg Universitet/visual_studio_code/2023_PrimarySettling/data/"
OutputPath <- "C:/Users/HD95LP/OneDrive - Aalborg Universitet/visual_studio_code/2023_PrimarySettling/output/"
SourcePath <- "C:/Users/HD95LP/OneDrive - Aalborg Universitet/visual_studio_code/2023_PrimarySettling/scripts"

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
  xaringanthemer,
  tidyverse, 
  showtext, 
  ampvis2, 
  ragg, 
  rasterpdf
)

#################################################
# Load data
#################################################

#Load workspace back to RStudio
load(paste0(OutputPath, "R_environments/","Environment_20230315.RData"))

# Generate dataframe from scratch 
  # source(paste0(SourcePath, "/load_data.R"))
  # data <- master_function(reads_randers = 60000,
  #                         reads_other = 60000,
  #                         rarefy = T,
  #                         save_non_merged_ampvis = T)
  # rm(d_COD_3, d_meta_by_date, tidy_meta, metadata, getSeason, randers_prop_flow, master_function)
  # save.image(file=paste0(OutputPath, "/R_environments/", "Environment_", format(Sys.Date(), format = "%Y%m%d"), ".RData"))



# Load results from wilcox_test.R
data_genus_random_subsamling <- 
  read.csv(paste0(OutputPath, "files/Genus_wilcox_test_2022-09-06_rare_60000_n_min12_obs_BEFORE_cross_all_24_sample_pairs.txt")) %>% 
  mutate(
    Plant = ifelse(str_detect(Plant, "\xf8"), str_replace(Plant, "\xf8", "ø"), Plant),
    Sign = if_else(mean_log2 > 0, "Increase", "Decrease"),
    Sign = if_else(p_adjust > 0.05, "Insignificant", Sign)) 

data_genus_all_samples <- 
  read.csv(paste0(OutputPath, "files/genus_wilcox_test_2022-10-06_rare_60000_min_12_all_samples.txt")) %>% 
  mutate(
    Plant = ifelse(str_detect(Plant, "\xf8"), str_replace(Plant, "\xf8", "ø"), Plant),
    Sign = if_else(mean_log2 > 0, "Increase", "Decrease"),
    Sign = if_else(p_adjust > 0.05, "Insignificant", Sign)) 

data_species_all_samples <- 
  read.csv(paste0(OutputPath, "files/species_wilcox_test_2022-10-06_rare_60000_min_12_all_samples.txt"))  %>% 
  mutate(
    Plant = ifelse(str_detect(Plant, "\xf8"), str_replace(Plant, "\xf8", "ø"), Plant),
    Sign = if_else(mean_log2 > 0, "Increase", "Decrease"),
    Sign = if_else(p_adjust > 0.05, "Insignificant", Sign))



############################################
# Figure 1: COD removal
#############################################

COD_removal_result <- 
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
                           "COD-removal<br>[%]"))) %>% 
  ggplot() + 
  geom_jitter(aes(x = Plant, y = values, color = name), 
              position = position_jitterdodge(jitter.width = 0.20), size = 2) + 
  geom_boxplot(aes(x = Plant, y = values, 
                   color = name, fill = name),
               alpha = 0.8,
               position = position_dodge(), outlier.shape = NA) + 
  scale_fill_manual(values = c("gray30", "#708090", "gray70")) +
  scale_color_manual(values = c("gray30", "#708090", "gray70")) +
  #scale_fill_manual(values = c("white", "white","#211a52")) +
  #facet_wrap(~Plant, ncol = 4, scales = "free_x") +
  scale_y_continuous(expand = c(0,100),
                     "COD [mg/L]", 
                     sec.axis = sec_axis(~ . /15, name = "COD-removal [%]", 
                                         labels = c(seq(0,100,by=20)),
                                         breaks = c(seq(0,100,by=20)))
  ) + 
  guides(fill = guide_legend()) +
  theme(axis.title.x = element_blank(),
        legend.position = "right", 
        axis.text.y = element_text(size = 20, color = "black"),#, family = "Comic Sans MS"), 
        axis.text.x = element_text(size = 20, color = "black"),#, family = "Arial"), 
        legend.text = element_markdown(size = 16, color = "black",
                                       margin = margin(t = 10, b = 5), lineheight = 0.001,
                                       #family = "Arial"
                                       ), 
        legend.title = element_blank(), 
        axis.line = element_line(color = "black", linewidth = 0.4),
        axis.ticks.y.left = element_line(color = "black", linewidth = 0.4),
        axis.ticks.y.right = element_line(color = "black", linewidth = 0.4),
        axis.ticks.x = element_line(color = "black", linewidth = 0.4),
        axis.title.y.left = element_markdown(size = 20, color = "black"),
        axis.title.y.right = element_markdown(size = 20, color = "black"), 
        panel.grid.major = element_line(linewidth = 0.2, color = "grey80"),
        panel.grid.minor.x =element_line(linewidth = 0.2, color = "grey80"),
        panel.background = element_rect(fill = "white"),
        panel.spacing = unit(0,'lines'))


#ggsave(paste0(OutputPath, "plots/results/COD_removal_.png"), width = 8, height = 4, units = "in")
#ggsave(paste0(OutputPath, "plots/results/COD_removal.pdf"), width = 8, height = 4, units = "in", scale = 2)

agg_pdf(paste0(OutputPath,"plots/results/Figure1_COD_removal_", Sys.Date(),".pdf"), 
        width = 12, height = 6, units = "in", res = 600)
COD_removal_result
invisible(dev.off())

############################################
# Figure 2: Overall PCA
#######################################

# Plot function: 
PCA_plantwise <- source(paste0(SourcePath, "/PCA_plots.R"), local = knitr::knit_global())

amp_merged_species <- data[[4]] 
all <- 
  PCA_paired_season_color("Aalborg West", amp_object = amp_merged_species) + labs(title = paste0("__Aalborg West__ (n=36)")) +
  PCA_paired_season_color("Ejby Mølle", amp_object = amp_merged_species) + labs(title = paste0("__Ejby Mølle__ (n=29)")) +
  PCA_paired_season_color("Esbjerg West", amp_object = amp_merged_species) + labs(title = paste0("__Esbjerg West__ (n=24)")) +
  PCA_paired_season_color("Randers", amp_object = amp_merged_species) + labs(title = paste0("__Randers__ (n=42)")) 

PCA_article <- all + plot_layout(guides = "collect") & theme(legend.position = "bottom",
                                              legend.box = "horizontal")

#ggsave(paste0(OutputPath, "plots/results/overall_PCA.png"), width = 7.5, height = 7, units = "in")

agg_pdf(paste0(OutputPath,"plots/results/Figure2_PCoA_", Sys.Date(),".pdf"), 
        width = 12, height = 12, units = "in", res = 600)
PCA_article
invisible(dev.off())


############################################
# Figure 3: Influent streams
#######################################

influent_streams <- plot_all_influent_streams(amp_object = amp_merged_species) + 
  plot_all_influent_streams(amp_object = amp_merged_species, primary_settler = "After") + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom",legend.box = "horizontal")


agg_pdf(paste0(OutputPath,"plots/results/Figure3_influent_streams_", Sys.Date(),".pdf"), 
        width = 12, height = 7, units = "in", res = 600)
influent_streams
invisible(dev.off())


#ggsave(paste0(OutputPath, "plots/results/all_influentstreams.png"), width = 10, height = 5.5, units = "in")


####################################
# Figure 5: Upset plot
###################################

source(paste0(SourcePath, "/upset_plot.R"))

upset_plot_artcile <- upset_plot(data_genus_all_samples)

agg_pdf(paste0(OutputPath,"plots/results/Figure5_upset_", Sys.Date(),".pdf"), 
        width = 14, height = 8, units = "in", res = 600)
upset_plot_artcile
invisible(dev.off())

#ggsave(paste0(primarysettling_folder, "output/plots/PrimarySettling_article/upset.png"), width = 7.5, height = 4)


####################################
# Figure 4 + S10: Sankey
###################################

source(paste0(SourcePath, "/sankey_plot.R"))

## All 
sankey_all <- 
  san_key_plot("Randers") + theme(axis.text.x = element_blank()) +
  san_key_plot("Ejby Mølle")  + theme(axis.text.x = element_blank())+ 
  san_key_plot("Esbjerg West")  + theme(axis.text.x = element_blank()) +
  san_key_plot("Aalborg West") + theme(axis.text.x = element_markdown(size = 40, lineheight = 0.003)) +
  plot_layout(ncol = 1)

agg_png(paste0(OutputPath,"plots/supplementary/Figure_S10_sankey_all_", Sys.Date(),".png"), 
        width = 5, height = 5.2, units = "in", res = 600)
sankey_all
invisible(dev.off())


#ggsave(paste0(OutputPath, "plots/supplementary_results/sankey_rev.png"), width = 6.5, height = 6.5,  units = "in")

## Only Esbjerg West
sankey_esw <- 
  san_key_plot("Esbjerg West") + theme(axis.text.x = element_markdown(size = 40, lineheight = 0.003)) +
  plot_layout(ncol = 1)

agg_pdf(paste0(OutputPath,"plots/results/Figure4_sankey_Esbjerg_", Sys.Date(),".pdf"), 
        width = 6, height = 1.6, units = "in", res = 600)
sankey_esw
invisible(dev.off())


#ggsave(paste0(OutputPath, "plots/results/sankey_Esbjerg_rev.png"), width = 5, height = 2,  units = "in")

rm(san_key_plot)


###################################################
# Figure 6: Stacked bar with cummulative abundance
###################################################

abun_tested_genus <- 
  data[[3]] %>% 
  mutate(samples = map(samples, ~
                         ungroup(.) %>% 
                         #select(Genus, rel_abun_genus) %>% 
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
  summarise(sum_abun = sum(rel_abun_genus), .groups = "drop") %>% 
  group_by(Plant, PrimarySettler, tested, n) %>%
  summarise(mean_abun = mean(sum_abun), 
            sd = sd(sum_abun)) %>% 
  mutate(Tax = "Genus")

stacked_bar <- 
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
  geom_text(size = 7, position = position_stack(vjust = 0.6), lineheight = 0.8, family = "sans") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_manual(values = c(#"#797597",  "#6F8FAF", "#708090", "gray80"),
    "Not tested"="gray45","Decrease"="#CE7E7E", 
    "Increase"="#6F8FAF", "Insignificant"="gray85")
  ) +
  ylab("Relative abundance [%]") +
  theme(
    strip.text.x = element_text(size = 28, lineheight = 0.0005, margin = margin(b = 1.3, t = 1), color = "gray10"),
    axis.text.y = element_text(size = 20, color = "gray10"),
    axis.ticks.x = element_blank(), 
    axis.title.y = element_markdown(size = 24, color = "gray10",
                                    linewidth = 0.00000001, lineheight = 0.0001),
    #axis.title.x = element_markdown(size = 24), 
    axis.text.x = element_markdown(size =20, linewidth = 0.00000001, lineheight = 0.0001, color = "gray10"),
    axis.title.x = element_blank(), 
    legend.position = "bottom", 
    legend.key.size = unit(1, "cm"),
    legend.text = element_markdown(margin = margin(t = 0.1, b = 0.1), color = "gray10", size = 20), 
    legend.title = element_blank(), 
    axis.line.y = element_line(color = "gray10", linewidth = 0.2),
    axis.ticks.y = element_line(color = "gray10", linewidth = 0.2),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(linewidth = 0.2, color = "gray90")
  )


stacked_bar + theme(legend.text = element_markdown(margin = margin(t = 0.1, b = 0.1), color = "gray10", size = 48), 
)
ggsave(paste0(OutputPath, "plots/results/legend.png"), width =16, height =8 , units = "in", scale = 0.5)


agg_pdf(paste0(OutputPath,"plots/results/Figure6_stacked_bar_", Sys.Date(),".pdf"), 
        width = 16, height = 10, units = "in", res = 600)
stacked_bar
invisible(dev.off())


####################################
# Figure 7 + S9: Fold change
###################################

source(paste0(SourcePath, "/fold_change_plot.R"))


agg_png(paste0(OutputPath,"plots/results/Figure7_fold_change_", Sys.Date(),".png"), 
        width = 4.8, height = 6, units = "in", res = 600)
plot_article
invisible(dev.off())


#ggsave(paste0(OutputPath, "plots/results/Figure7_fold_change_", Sys.Date(),".png"), width = 6, height = 8,  units = "in")



agg_png(paste0(OutputPath,"plots/supplementary/FigureS9_fold_change_", Sys.Date(),".png"), 
        width = 4.5, height = 5.5, units = "in", res = 600)
plot_supplementary
invisible(dev.off())





