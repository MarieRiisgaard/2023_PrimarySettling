##################################################
# Abundance before + Fold change 
###################################################


traffic_plot_with_bars <- function(
    plot_df_1 = plot_df, 
    #round = round_d, 
    limit_vec = c(min(plot_df_1$mean, na.rm = T), max(plot_df_1$mean, na.rm = T)),
    wilcox_genus = data_genus_all_samples){
  

  p1 <- plot_df_1 %>% 
    ggplot(aes(x = PrimarySettler, y = lab)) +
    geom_text(aes(label = paste0(weights::rd(mean, 
                                       digits = ifelse(mean < 0.1, ifelse(mean < 0.01, 3, 2), 1
                                                     ), 
                                 add = F)), 
                   color = Sing_PS),
              show.legend = F, size=12
              ) +
    scale_color_manual(values = c("Before<br>primary settling"="black", 
                                  "Decrease after<br>primary settling"="black", 
                                  "Increase after<br>primary settling"="grey99", 
                                  "Insignificant after<br>primary settling"="gray70")) +
    scale_x_discrete(position = "top", name = "Before") +
    scale_fill_manual(values = c("Before<br>primary settling"="#D7CBA5", 
                                 "Decrease after<br>primary settling"="#CE7E7E", 
                                 "Increase after<br>primary settling"="#6F8FAF", 
                                 "Insignificant after<br>primary settling"="gray80")) +
    #theme_xaringan(css_file = "xaringan-themer.css") +
    theme(axis.text.x.top = element_blank(), #element_text(size = 26), 
          axis.ticks.x.top = element_blank(), axis.ticks.y = element_blank(), 
          axis.title.x.top = element_markdown(size = 26,lineheight = 0.001, color = "grey10"), 
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = "white"),
          #strip.text.x = element_markdown(size = 32, face = "bold"),
          #legend.spacing.y = unit(2, 'cm'), 
          #strip.text.y = element_markdown(size = 32),
          axis.text.x.bottom = element_blank(), 
          #strip.background = element_blank(), 
          axis.text.y = element_markdown(size = 30, color = "black", family = "sans", vjust = 0.5, hjust = 1),
          #strip.placement = "outside", 
          legend.title = element_blank(),
          legend.position = "none",
          panel.grid = element_line(linewidth = 0.2, color = "gray90"), 
          panel.grid.major.x = element_blank(),
          legend.text = element_markdown(size = 26, lineheight = 0.1)) + 
    guides(size = "none", label = "none",
           fill = "none" #guide_legend(override.aes = list(size = 4, label = "", byrow = TRUE))
    ) 
  p2 <- plot_df_1 %>% ###
    ggplot(aes(x = mean_diff, y = lab, fill = Sign)) + 
    geom_col() + 
    scale_x_continuous(position = "top",  name = "Mean fold change", expand = c(0,0), 
                       limits = c(0, max(plot_df_1$mean_diff)+0.01)
                       ) +
    #coord_trans(x="sqrt") +
    #theme_xaringan(css_file = "xaringan-themer.css") +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.line.x.top = element_line(color = "grey10", linewidth = 0.2),
          legend.position = "none",
          legend.text = element_markdown(size = 26, lineheight = 0.1), 
          legend.title = element_blank(),
          axis.title.x.top = element_text(size = 26, lineheight = 0.001, color = "grey10"), 
          axis.ticks.x.top = element_line(color = "grey10", linewidth = 0.2), 
          axis.text.x.top = element_text(size = 26, color = "black"),
          axis.title.y = element_blank(), 
          panel.background = element_rect(fill = "white"),
          #axis.text.x.bottom = element_text(size = 20), axis.text.x.top = element_blank(),
          panel.grid = element_line(linewidth = 0.2, color = "gray90"), 
          panel.grid.minor = element_blank(), 
          ) + 
    scale_fill_manual(values = c("Decrease"="#CE7E7E", 
                                 "Increase"="#6F8FAF", 
                                 "Insignificant"="gray80"))
  
  list(p1, p2)
}



patch_plots <- function(data, select_tax_vec, vlabel_name){ 
  #data = df_traffic %>% filter(Genus == "g__Arcobacter")
  #select_tax_vec = ("g__Arcobacter")
#select_tax_vec = select_tax, 
#vlabel_name = "**15 most abundant genera**",
  plot_df <- data %>% 
    filter(PrimarySettler == "Before") %>% 
    mutate(
      Sing_PS = ifelse(PrimarySettler == "Before", 
                       "Before<br>primary settling", 
                       paste0(Sign, " after<br>primary settling")),
      lab = Genus,
      lab = str_sub(lab, start = 4),
      lab = if_else(str_detect(lab, "midas", negate = T) & str_detect(lab, "Ca_", negate = T), 
                    paste0("*", lab, "*"), lab), 
      lab = str_replace(lab, "Ca_", "*Ca.* "), 
      lab = if_else(str_length(lab) > 32, paste0(str_sub(start = 1, end = 20, string = lab), ".*"), lab),
      lab = factor(lab), 
      lab = fct_reorder(lab, (mean))
    ) %>% 
    filter(Genus %in% unlist(select_tax_vec))  

E <- plot_df %>% 
  filter(Plant == "Esbjerg West") %>% 
  traffic_plot_with_bars()  
A <- plot_df %>% filter(Plant == "Aalborg West") %>% 
  traffic_plot_with_bars()  
EM <- plot_df %>% filter(Plant == "Ejby Mølle") %>% 
  traffic_plot_with_bars()  
R <- plot_df %>% filter(Plant == "Randers") %>% 
  traffic_plot_with_bars()  

list(A, EM, E, R)

}



####################################################
# Plotting
################################################

fold_change_all <- data[[3]] %>% 
  mutate(samples = map(samples, ~ 
                         ungroup(.) %>% 
                         filter(Genus %in% unlist(data_genus_all_samples$Tax)) %>% 
                         distinct(Genus, rel_abun_genus))) %>% 
  unnest(samples) %>% 
  group_by(Genus, Plant, Date_rawdata) %>% 
  mutate(filter_both_zero = sum(rel_abun_genus)) %>% #Filter both before and after = 0 
  filter(filter_both_zero != 0) %>% 
  ungroup() %>% select(-filter_both_zero) %>% 
  mutate(rel_abun_genus = rel_abun_genus + 0.001) %>%  # Adding a pseudo-count
  mutate(rel_abun_genus = ifelse(PrimarySettler == "Before", 1/rel_abun_genus, rel_abun_genus)) %>% 
  arrange(PrimarySettler) %>% 
  group_by(Date_rawdata, Plant, Genus) %>% 
  summarise(fold = prod(rel_abun_genus), values = paste0(rel_abun_genus, collapse = ";"), .groups = "drop", n = n()) %>% 
  left_join(., data[[2]], by = c("Date_rawdata", "Plant")) %>% 
  left_join(., data_genus_all_samples %>% rename("Genus" = "Tax"), by = c("Genus", "Plant")) 


fold_change_mean <- fold_change_all %>% 
  group_by(Plant, Genus, Sign) %>% 
  summarise(mean_diff = mean(fold), .groups = "drop") %>% 
  mutate(mean_diff = case_when(Sign == "Insignificant" ~ 0.5,
                          Sign == "Decrease" ~ 1/mean_diff, 
                          Sign == "Increase" ~ mean_diff))


# Makes df form plotting
df_traffic <- data[[3]] %>%
  mutate(samples = map(samples, ~
                         ungroup(.) %>% 
                         filter(Genus %in% unlist(data_genus_all_samples$Tax)) %>%
                         select(Genus, rel_abun_genus, Guild) %>%
                         arrange(Guild) %>%
                         distinct(Genus, rel_abun_genus, .keep_all = T)
  )) %>%
  unnest(samples) %>%
  left_join(., data_genus_all_samples %>% rename("Genus" = "Tax"), by = c("Genus", "Plant")) %>%
  left_join(., fold_change_mean, by = c("Genus", "Plant", "Sign")) %>%
  arrange(Guild) %>%
  distinct() %>%
  group_by(Plant, PrimarySettler, Genus, Sign, Guild, mean_diff) %>%
  summarise(mean = median(rel_abun_genus), .groups = "drop") %>%
  mutate(Top = "__15 most abundant genera__")


# Top 15
T_15 <- 
  patch_plots(data = df_traffic, 
              select_tax_vec = 
                df_traffic %>% filter(PrimarySettler == "Before") %>% group_by(Plant) %>% 
                slice_max(order_by = mean, n = 15) %>% ungroup() %>% 
                distinct(Genus), 
      vlabel_name = "**15 most abundant genera**")
      
#PAO 
PAO <- 
  patch_plots(data = df_traffic, 
              select_tax_vec = df_traffic %>% filter(Guild %in% c("__PAO__")), 
              vlabel_name = "**PAO**")

#GAO 
GAO <- 
  patch_plots(data = df_traffic, 
              select_tax_vec = df_traffic %>% filter(Guild %in% c("__GAO__")), 
              vlabel_name = "**GAO**")
#Nitri 
Nit <- 
  patch_plots(data = df_traffic, 
              select_tax_vec = df_traffic %>% filter(Guild %in% c("__Nitrifiers__")), 
              vlabel_name = "**Nitri.**")
# Filamnets
Fila <- 
  patch_plots(data = df_traffic, 
              select_tax_vec = df_traffic %>% filter(Guild %in% c("__Filaments__")) %>% 
                filter(str_detect(Genus, "Tricho", negate = T)) %>% 
                #filter(str_detect(Genus, "Thio", negate = T)) %>% 
                filter(str_detect(Genus, "Lacto", negate = T)) %>% 
                filter(str_detect(Genus, "Strep", negate = T)), 
              vlabel_name = "**Filaments**")


# Design
design <- c(
  #area(1,1), # empty
  area(1, 2, r = 3),# text 1
  area(1, 4, r = 5),# text 2
  area(1, 6, r = 7),# text 3
  area(1, 8, r = 9),# text 4
  # Top abun
  area(2,1), # Names
  area(2,2), area(2,3), # top 
  area(2,4), area(2,5), # top
  area(2,6), area(2,7),# top
  area(2,8), area(2,9), # top
  area(t = 3, l=1, r = 10),  # plotspacer
  ## PAO 
  area(4,1), # Names
  area(4,2), area(4,3), #   
  area(4,4), area(4,5), # 
  area(4,6), area(4,7),# 
  area(4,8), area(4,9), # 
  area(5,l = 1, r = 10),  # plotspacer
  ## GAO
  area(6,1), # Names
  area(6,2), area(6,3), #  
  area(6,4), area(6,5), # 
  area(6,6), area(6,7),# 
  area(6,8), area(6,9), # 
  area(7,l = 1, r = 10),  # plotspacer
  ## Nitrifiers
  area(8,1), # Names
  area(8,2), area(8,3), #  
  area(8,4), area(8,5), # 
  area(8,6), area(8,7),# 
  area(8,8), area(8,9), #
  area(9,l = 1, r = 10),  # plotspacer
  # ## Filaments TH
  # area(6,1), # Names
  # area(6,2), area(6,3), #  
  # area(6,4), area(6,5), # 
  # area(6,6), area(6,7),# 
  # area(6,8), area(6,9), # 
  ## Filaments
  area(10,1), # Names
  area(10,2), area(10,3), #  
  area(10,4), area(10,5), # 
  area(10,6), area(10,7),# 
  area(10,8), area(10,9), # 
  # vertical text lines
  area(t = 2,l = 10), ## text line TOP 
  area(t = 4,l = 10), ## text line
  area(t = 6,l = 10), ## text line
  area(t = 8,l = 10), ## text line
  area(t = 10,l = 10) ## text line
  #area(t = 7,l = 10, b = 8) ## text line
)
#

 #### Plotting

label_height = 0.08
plot_height = 1
genus_name_width = 0.0004
p1_width = 0.50 
p2_width = 1.05
vlabel_width = 0.17


empty_plot <- ggplot() + scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) + 
  #theme_xaringan(css_file = "xaringan-themer.css") +
  theme(plot.background = element_rect(fill = "gray80", color = "gray80", inherit.blank = T
                                       ), axis.title = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), 
        panel.background = element_rect(fill = "gray80", color = "gray80", inherit.blank = T
                                        ), 
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.01, 0.005, 0.005, 0.005),
                           "inches"))

plot_article <- empty_plot + geom_richtext(aes(x = 0.46, y = 0.4, label = "**Aalborg West**"), size=15, fill = NA, label.color = NA) + 
  empty_plot + geom_richtext(aes(x = 0.37, y = 0.4, label = "**Ejby Mølle**"), size=15, fill = NA, label.color = NA) + 
  empty_plot + geom_richtext(aes(x = 0.48, y = 0.4, label = "**Esbjerg West**"), size=15, fill = NA, label.color = NA) + 
  empty_plot + geom_richtext(aes(x = 0.28, y = 0.4, label = "**Randers**"), size=15, fill = NA, label.color = NA) +
## TOP 15
  T_15[[1]][[1]] + theme(axis.title.x.top = element_blank(), plot.margin = unit(c(0.001, 0.001, 0.001, 0.001),  "inches")) +
  T_15[[1]][[1]] + theme(axis.text.y = element_blank()) + T_15[[1]][[2]] + scale_x_continuous(limits = c(0, 6.1), position = "top",  name = "Mean fold change", expand = c(0,0))+
  T_15[[2]][[1]]+ theme(axis.text.y = element_blank()) + T_15[[2]][[2]]  + scale_x_continuous(limits = c(0, 6.1), position = "top",  name = "Mean fold change", expand = c(0,0))+
  T_15[[3]][[1]]+ theme(axis.text.y = element_blank()) + T_15[[3]][[2]]  + scale_x_continuous(limits = c(0, 6.1), position = "top",  name = "Mean fold change", expand = c(0,0))+
  T_15[[4]][[1]]+ theme(axis.text.y = element_blank()) + T_15[[4]][[2]]  + scale_x_continuous(limits = c(0, 6.1), position = "top",  name = "Mean fold change", expand = c(0,0))+
  plot_spacer() +
  ## PAO 
  PAO[[1]][[1]] + theme(axis.title.x.top = element_blank()) +theme(plot.margin = unit(c(0.001, 0.001, 0.001, 0.001),  "inches")) +
  PAO[[1]][[1]] + theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + PAO[[1]][[2]] + theme(axis.title.x.top = element_blank()) + scale_x_continuous(limits = c(0, 5), position = "top",  name = "Mean fold change", expand = c(0,0))+
  PAO[[2]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + PAO[[2]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 5), position = "top",  name = "Mean fold change", expand = c(0,0))+
  PAO[[3]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + PAO[[3]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 5), position = "top",  name = "Mean fold change", expand = c(0,0))+
  PAO[[4]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + PAO[[4]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 5), position = "top",  name = "Mean fold change", expand = c(0,0))+
  plot_spacer() +
  ## GAO 
  GAO[[1]][[1]] + theme(axis.title.x.top = element_blank()) +theme(plot.margin = unit(c(0.001, 0.001, 0.001, 0.001),  "inches")) +
  GAO[[1]][[1]] + theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + GAO[[1]][[2]] + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 4.5), position = "top",  name = "Mean fold change", expand = c(0,0))+
  GAO[[2]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + GAO[[2]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 4.5), position = "top",  name = "Mean fold change", expand = c(0,0))+
  GAO[[3]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + GAO[[3]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 4.5), position = "top",  name = "Mean fold change", expand = c(0,0))+
  GAO[[4]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + GAO[[4]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 4.5), position = "top",  name = "Mean fold change", expand = c(0,0))+
  plot_spacer() +
  ## Nitrifiers
  Nit[[1]][[1]] + theme(axis.title.x.top = element_blank()) + theme(plot.margin = unit(c(0.001, 0.001, 0.001, 0.001),  "inches")) +
  Nit[[1]][[1]] + theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + Nit[[1]][[2]] + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 4.5), position = "top",  name = "Mean fold change", expand = c(0,0))+
  Nit[[2]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + Nit[[2]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 4.5), position = "top",  name = "Mean fold change", expand = c(0,0))+
  Nit[[3]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + Nit[[3]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 4.5), position = "top",  name = "Mean fold change", expand = c(0,0))+
  Nit[[4]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + Nit[[4]][[2]]  +theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 4.5), position = "top",  name = "Mean fold change", expand = c(0,0))+
  plot_spacer() +
  ## Filaments
  Fila[[1]][[1]] + theme(axis.title.x.top = element_blank()) + theme(plot.margin = unit(c(0.001, 0.001, 0.001, 0.001),  "inches")) +
  Fila[[1]][[1]] + theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + Fila[[1]][[2]] + theme(axis.title.x.top = element_blank()) + scale_x_continuous(limits = c(0, 14.1), position = "top",  name = "Mean fold change", expand = c(0,0))+
  Fila[[2]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + Fila[[2]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 14.1), position = "top",  name = "Mean fold change", expand = c(0,0))+
  Fila[[3]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + Fila[[3]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 14.1), position = "top",  name = "Mean fold change", expand = c(0,0))+
  Fila[[4]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + Fila[[4]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 14.1), position = "top",  name = "Mean fold change", expand = c(0,0))+
  # Top 
  empty_plot +  geom_richtext(aes(x = 0.32, y = 0.5, label = "**15 most abundant genera**", angle = 270), 
                size=13, fill = NA, label.color = NA) + 
# Pao
empty_plot + geom_richtext(aes(x = 0.32, y = 0.5, label = "**PAO**", angle = 270), 
                size=13, fill = NA, label.color = NA) + 
# GAO
  empty_plot + geom_richtext(aes(x = 0.32, y = 0.5, label = "**GAO**", angle = 270), 
                size=13, fill = NA, label.color = NA) + 
  # Nit
  empty_plot + geom_richtext(aes(x = 0.32, y = 0.5, label = "**Nit.**", angle = 270), 
                size=13, fill = NA, label.color = NA, label.padding = unit(0.01, "mm")) + 
# Fila 
  empty_plot + geom_richtext(aes(x = 0.32, y = 0.5, label = "**Filaments**", angle = 270), 
                size=13, fill = NA, label.color = NA) + 
# PLOTLAYOUT 
  plot_layout(nrow = 2, guides = "collect", 
              heights = c(1, 24, -3.2, 6 ,-3.2, 4.2, -3.2, 1.3, -3.2, 7.2),
              widths = c(genus_name_width, 
                         p1_width, p2_width,p1_width, p2_width, p1_width, p2_width,p1_width, p2_width, 
                         vlabel_width), 
              design = design) & 
  theme(legend.position = "none", axis.title.x = element_blank())




###################### For appendix  
#########COMMON


common <- data_genus_all_samples %>% 
  group_by(Tax, Sign) %>% 
  summarise(n = n()) %>% 
  filter(n == 4) %>% 
  filter(Sign != "Insignificant")

### Makes df form plotting common
df_traffic_common <- data[[3]] %>%
  mutate(samples = map(samples, ~
                         ungroup(.) %>% 
                         filter(Genus %in% unlist(data_genus_all_samples$Tax)) %>%
                         select(Genus, rel_abun_genus, Guild) %>%
                         arrange(Guild) %>%
                         distinct(Genus, rel_abun_genus, .keep_all = T)
  )) %>%
  unnest(samples) %>%
  left_join(., data_genus_all_samples %>% rename("Genus" = "Tax"), by = c("Genus", "Plant")) %>%
  left_join(., fold_change_mean, by = c("Genus", "Plant", "Sign")) %>%
  arrange(Guild) %>%
  distinct() %>%
  group_by(Plant, PrimarySettler, Genus, Sign, Guild, mean_diff) %>%
  summarise(mean = median(rel_abun_genus), .groups = "drop") %>%
  filter(PrimarySettler != "After") %>% 
  mutate(Top = "__15 most abundant genera__") %>% 
  filter(Genus %in% unlist(common$Tax)) %>%    # FILTER for only common genera
  #filter(mean == 0) #%>% 
  group_by(Genus) %>% 
  mutate(Top = case_when(              # Divide into groups based on abundance 
    min(mean) >= 0.1 ~ "large", 
    min(mean) >= 0.01 & min(mean) < 0.1 ~ "middle", 
    min(mean) < 0.01 ~ "small")
  ) %>% 
  ungroup()

all <-  patch_plots(data = df_traffic_common, 
                    select_tax_vec = df_traffic_common %>% filter(Top == "large"), 
                    vlabel_name = " ")
all1 <-  patch_plots(data = df_traffic_common, 
                     select_tax_vec = df_traffic_common %>% filter(Top == "middle") , 
                     vlabel_name = " ")
all2 <-  patch_plots(data = df_traffic_common, 
                    select_tax_vec = df_traffic_common %>% filter(Top == "small") , 
                    vlabel_name = " ")

# Design
design <- c(
  #area(1,1), # empty
  area(1, 2, r = 3),# text 1
  area(1, 4, r = 5),# text 2
  area(1, 6, r = 7),# text 3
  area(1, 8, r = 9),# text 4
  # Top abun
  area(2,1), # Names
  area(2,2), area(2,3), # top 
  area(2,4), area(2,5), # top
  area(2,6), area(2,7),# top
  area(2,8), area(2,9), # top
  #area(t = 7,l = 10, b = 8) ## text line
  area(3,1), # Names
  area(3,2), area(3,3), # top 
  area(3,4), area(3,5), # top
  area(3,6), area(3,7),# top
  area(3,8), area(3,9), # top
  area(4,1), # Names
  area(4,2), area(4,3), # top 
  area(4,4), area(4,5), # top
  area(4,6), area(4,7),# top
  area(4,8), area(4,9) # top
  )
#


plot_supplementary <- 
  empty_plot + geom_richtext(aes(x = 0.46, y = 0.5, label = "**Aalborg West**"), size=15, fill = NA, label.color = NA) + 
  empty_plot + geom_richtext(aes(x = 0.37, y = 0.5, label = "**Ejby Mølle**"), size=15, fill = NA, label.color = NA) + 
  empty_plot + geom_richtext(aes(x = 0.48, y = 0.5, label = "**Esbjerg West**"), size=15, fill = NA, label.color = NA) + 
  empty_plot + geom_richtext(aes(x = 0.28, y = 0.5, label = "**Randers**"), size=15, fill = NA, label.color = NA) +
  ## TOP 15
  all[[1]][[1]] + theme(axis.title.x.top = element_blank()) +
  all[[1]][[1]] + theme(axis.text.y = element_blank()) + all[[1]][[2]] + scale_x_continuous(limits = c(0, 6.1), position = "top",  name = "Mean fold change", expand = c(0,0))+
  all[[2]][[1]]+ theme(axis.text.y = element_blank()) + all[[2]][[2]]  + scale_x_continuous(limits = c(0, 6.1), position = "top",  name = "Mean fold change", expand = c(0,0))+
  all[[3]][[1]]+ theme(axis.text.y = element_blank()) + all[[3]][[2]]  + scale_x_continuous(limits = c(0, 6.1), position = "top",  name = "Mean fold change", expand = c(0,0))+
  all[[4]][[1]]+ theme(axis.text.y = element_blank()) + all[[4]][[2]] +scale_x_continuous(limits = c(0, 6.1), position = "top",  name = "Mean fold change", expand = c(0,0))+
  ## TOP 15
  all1[[1]][[1]] + theme(axis.title.x.top = element_blank()) +
  all1[[1]][[1]] + theme(axis.text.y = element_blank(), axis.title.x.top = element_blank())  + all1[[1]][[2]] + theme(axis.title.x.top = element_blank()) + scale_x_continuous(limits = c(0, 11.7), position = "top",  name = "Mean fold change", expand = c(0,0))+
  all1[[2]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + all1[[2]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 11.7), position = "top",  name = "Mean fold change", expand = c(0,0))+
  all1[[3]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + all1[[3]][[2]]  + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 11.7), position = "top",  name = "Mean fold change", expand = c(0,0))+
  all1[[4]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + all1[[4]][[2]] +theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 11.7), position = "top",  name = "Mean fold change", expand = c(0,0))+
  ## TOP 15
  all2[[1]][[1]] + theme(axis.title.x.top = element_blank()) +
  all2[[1]][[1]] + theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) +all2[[1]][[2]] + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 12.3), position = "top",  name = "Mean fold change", expand = c(0,0))+
  all2[[2]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + all2[[2]][[2]]  + theme(axis.title.x.top = element_blank()) + scale_x_continuous(limits = c(0, 12.3), position = "top",  name = "Mean fold change", expand = c(0,0))+
  all2[[3]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank())  + all2[[3]][[2]]  +theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 12.3), position = "top",  name = "Mean fold change", expand = c(0,0))+
  all2[[4]][[1]]+ theme(axis.text.y = element_blank(), axis.title.x.top = element_blank()) + all2[[4]][[2]] + theme(axis.title.x.top = element_blank()) +scale_x_continuous(limits = c(0, 12.3), position = "top",  name = "Mean fold change", expand = c(0,0))+
  # PLOTLAYOUT 
  plot_layout(nrow = 2, guides = "collect", 
              heights = c(2, 11.5, 27, 28),
              widths = c(genus_name_width, 
                         p1_width, p2_width,p1_width, p2_width, p1_width, p2_width,p1_width, p2_width, 
                         vlabel_width), 
              design = design) & 
  theme(legend.position = "none")




rm(fold_change_all, fold_change_mean, df_traffic, T_15, PAO, GAO, Nit,Fila, empty_plot, 
   design, label_height, plot_height, genus_name_width, p1_width, p2_width, vlabel_width, 
   common, all, all1, all2)

rm(patch_plots, traffic_plot_with_bars)



############# Abstract ###########

# all <- patch_plots(data = df_traffic %>% 
#                      filter(Genus %in% c("g__Tetrasphaera", "g__Ca_Accumulibacter", "g__Dechloromonas", "g__Ca_Microthrix", "g__Gordonia")) %>% 
#                      mutate(Genus = factor(Genus, levels = c("g__Ca_Accumulibacter", "g__Dechloromonas", "g__Tetrasphaera", "g__Ca_Microthrix", "g__Gordonia"))), 
#               select_tax_vec = 
#                 df_traffic %>% 
#               filter(Genus %in% 
#                        c("g__Tetrasphaera", "g__Ca_Accumulibacter", "g__Dechloromonas", "g__Ca_Microthrix", "g__Gordonia")),
#               vlabel_name = "Abstract")
# 
# # Design
# design <- c(
#   #area(1,1), # empty
#   area(1, 2, r = 3),# text 1
#   area(1, 4, r = 5),# text 2
#   area(1, 6, r = 7),# text 3
#   area(1, 8, r = 9),# text 4
#   # Top abun
#   area(2,1), # Names
#   area(2,2), area(2,3), # top 
#   area(2,4), area(2,5), # top
#   area(2,6), area(2,7),# top
#   area(2,8), area(2,9)) # top
# 
# label_height = 0.005
# plot_height = 1
# genus_name_width = 0.0004
# p1_width = 0.50 
# p2_width = 1.01
# vlabel_width = 0.2
# 
# 
# plot_supplementary <- 
#   empty_plot + geom_richtext(aes(x = 0.46, y = 0.5, label = "**Aalborg West**"), size=15, fill = NA, label.color = NA) + 
#   empty_plot + geom_richtext(aes(x = 0.37, y = 0.5, label = "**Ejby Mølle**"), size=15, fill = NA, label.color = NA) + 
#   empty_plot + geom_richtext(aes(x = 0.48, y = 0.5, label = "**Esbjerg West**"), size=15, fill = NA, label.color = NA) + 
#   empty_plot + geom_richtext(aes(x = 0.28, y = 0.5, label = "**Randers**"), size=15, fill = NA, label.color = NA) +
#   ## TOP 15
#   all[[1]][[1]] + theme(axis.title.x.top = element_blank()) +
#   all[[1]][[1]] + theme(axis.text.y = element_blank()) + all[[1]][[2]] + 
#   all[[2]][[1]]+ theme(axis.text.y = element_blank()) + all[[2]][[2]]  + theme(legend.position = "none") +
#   all[[3]][[1]]+ theme(axis.text.y = element_blank()) + all[[3]][[2]]  + theme(legend.position = "none") +
#   all[[4]][[1]]+ theme(axis.text.y = element_blank()) + all[[4]][[2]] +  theme(legend.position = "none") +
#   plot_layout(nrow = 2, guides = "collect", 
#               heights = c(2, 8.5, 29, 31),
#               widths = c(genus_name_width, 
#                          p1_width, p2_width,p1_width, p2_width, p1_width, p2_width,p1_width, p2_width, 
#                          vlabel_width), 
#               design = design) & 
#   theme(legend.position = "right")
# plot_supplementary
# ggsave(paste0(OutputPath, "/plots/abstract.png"), height = 2, width = 8)
