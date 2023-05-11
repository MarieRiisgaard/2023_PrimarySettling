#########################################
# Make supplemetary data 1, 2 and 3     #
#########################################

# Set WD
setwd("C:/Users/marie/Desktop/visual_studio_code/2023_PrimarySettling")

## Load data into ampvis format
DataPath <- "C:/Users/marie/Desktop/visual_studio_code/2023_PrimarySettling/data/"
OutputPath <- "C:/Users/marie/Desktop/visual_studio_code/2023_PrimarySettling/output/"
SourcePath <- "C:/Users/marie/Desktop/visual_studio_code/2023_PrimarySettling/scripts/"

#################################################
# Load data
#################################################

#Load workspace back to RStudio
load(paste0(OutputPath, "R_environments/","Environment_20230315.RData"))

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



# Supplemetary 1: Metadata paired samples

## Plant parameters 
V = tibble(
  Plant = c("Randers",      "Ejby Mølle", "Esbjerg West",   "Aalborg West"), 
  v =     c(3*1425,          7*1200,       2*1046,           2*1900), 
  s =     c(3*3.14*13.25^2 - (3*3.14*2.55^2),  #Randers
            7*58*8,                          #Ejby M?lle"
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


## Calculate metadata for each sample
meta_data_paired_samples <- data[[3]] %>% 
  mutate(samples = map(samples, ~ 
                         filter(., Genus %in% unlist(data_genus_all_samples$Tax)) %>% 
                         distinct(Genus, rel_abun_genus))) %>% 
  unnest(samples) %>% 
  group_by(Genus, Plant, Date_rawdata) %>% 
  mutate(filter_both_zero = sum(rel_abun_genus)) %>% #Filter both before and after = 0 
  #filter(filter_both_zero != 0) %>% 
  ungroup() %>% select(-filter_both_zero) %>% 
  mutate(rel_abun_genus = rel_abun_genus + 0.001) %>%  # Adding a pseudo-count
  mutate(rel_abun_genus = ifelse(PrimarySettler == "Before", 1/rel_abun_genus, rel_abun_genus)) %>% 
  arrange(PrimarySettler) %>% 
  group_by(Date_rawdata, Plant, Genus) %>% 
  summarise(fold = prod(rel_abun_genus), values = paste0(ifelse(PrimarySettler == "Before", 1/rel_abun_genus, rel_abun_genus), collapse = ";"), .groups = "drop", n = n()) %>% 
  separate(values, sep = ";", into = c("rel_abun_genus_beforePS", "rel_abun_genus_afterPS")) %>% 
  left_join(., data[[2]], by = c("Date_rawdata", "Plant")) %>% 
  left_join(., plant_parameters, by = c("Date_rawdata", "Plant")) %>% # Add SOR
  left_join(., data_genus_all_samples %>% rename("Genus" = "Tax"), by = c("Genus", "Plant")) 

write.csv(meta_data_paired_samples, paste0(OutputPath, "files/Supplementary_file_1_meta_data_paired_samples.csv"))


# Supplemetary 2: Metadata paired samples

# Mean fold change
fold_change_mean <- meta_data_paired_samples %>% 
  filter(!(rel_abun_genus_beforePS == 0.001 & rel_abun_genus_afterPS == 0.001)) %>%  # Filter samples which are 0 (equal to pseudocount) before and after
  group_by(Plant, Genus, Sign) %>% 
  summarise(mean_fold_change = mean(fold), .groups = "drop") %>% 
  mutate(mean_fold_change = case_when(Sign == "Insignificant" ~ 0.5,
                               Sign == "Decrease" ~ 1/mean_fold_change, 
                               Sign == "Increase" ~ mean_fold_change))

# Mean difference --> test for coorelation with SOR

## Calculate mean difference 
diff_all <- data[[3]] %>% 
  mutate(samples = map(samples, ~ 
                         filter(., Genus %in% 
                                  unlist(data_genus_all_samples$Tax)) %>% 
                         distinct(Genus, rel_abun_genus))) %>% 
  unnest(samples) %>% 
  group_by(Genus, Plant, Date_rawdata) %>% 
  mutate(filter_both_zero = sum(rel_abun_genus)) %>% #Filter both before and after = 0 
  filter(filter_both_zero != 0) %>% 
  ungroup() %>% select(-filter_both_zero) %>% 
  mutate(rel_abun_genus = ifelse(PrimarySettler == "Before", -rel_abun_genus, rel_abun_genus)) %>% 
  group_by(Date_rawdata, Plant, Genus) %>% 
  summarise(diff = sum(rel_abun_genus), .groups = "drop") %>% 
  left_join(., plant_parameters, by = c("Date_rawdata", "Plant")) %>% # Add SOR
  left_join(., data_genus_all_samples %>% rename("Genus" = "Tax"), by = c("Genus", "Plant")) %>%  # add wilcox
  left_join(., data[[2]], by = c("Date_rawdata", "Plant"))  ## Add temperature

diff_mean <- diff_all %>% 
  rename("p_adjust_wilcox" = "p_adjust") %>% 
  group_by(Plant, Genus, p_adjust_wilcox) %>% 
  summarise(mean_diff = (mean(diff)), .groups = "drop")

## Correlation mean differnece to SOR 
corr_diff_SOR <- diff_all %>% 
  group_by(Plant, Genus, Sign) %>% 
  summarize(correlation_coeff_SOR = stats::cor.test(diff, SOR, method = "spearman")$estimate,
            pval = stats::cor.test(diff, SOR, method = "spearman")$p.value,
            .groups = "drop") %>%
  group_by(Plant) %>% 
  mutate(p_adjust_SOR = p.adjust(pval, method = "BH"), 
         #p_adjust_SOR = ifelse(p_adjust_SOR >= 0.05, NA, p_adjust_SOR),
         correlation_coeff_SOR = ifelse(p_adjust_SOR >= 0.05, NA, correlation_coeff_SOR))%>% 
  select(-pval)
  

corr_diff_SOR %>% filter(p_adjust_SOR < 0.05) %>% group_by(Plant) %>%  summarise(n())


## Mean relative abundance  before and after (genus level)
abun_genus_level <- data[[3]] %>% 
  mutate(samples = map(samples, ~ 
                         filter(., Genus %in% 
                                  unlist(data_genus_all_samples$Tax)) %>% 
                         distinct(Genus, rel_abun_genus))) %>% 
  unnest(samples) %>% 
  group_by(Genus, Plant, PrimarySettler) %>%
  summarise(mean_rel_abun_genus = mean(rel_abun_genus)) %>%
  pivot_wider(names_from = PrimarySettler, values_from = mean_rel_abun_genus, names_prefix = "mean_rel_abun_genus_")


supplementary_2 <- 
  corr_diff_SOR %>% 
  full_join(diff_mean, by = c("Plant", "Genus")) %>% 
  full_join(fold_change_mean, c("Plant", "Genus", "Sign")) %>% 
  full_join(abun_genus_level, by = c("Plant", "Genus")) %>% 
  distinct() 

write.csv(supplementary_2, paste0(OutputPath, "files/Supplementary_file_2_Genus_test_results.csv"))


## Mean relative abundance  before and after (Species level)
abun_species_level <- data[[3]] %>% 
  mutate(samples = map(samples, ~ 
                         filter(., Species %in% 
                                  unlist(data_species_all_samples$Tax)) %>% 
                         distinct(Species, rel_abun_species))) %>% 
  unnest(samples) %>% 
  group_by(Species, Plant, PrimarySettler) %>%
  summarise(mean_rel_abun_species = mean(rel_abun_species) )%>%
  pivot_wider(names_from = PrimarySettler, values_from = mean_rel_abun_species, names_prefix = "mean_rel_abun_species_")



supplementary_3 <- 
  data_species_all_samples %>% 
  rename("Species" = "Tax") %>% 
  select(-mean_log2, -X) %>% 
  full_join(abun_species_level, by = c("Species", "Plant"))

write.csv(supplementary_3, paste0(OutputPath, "files/Supplementary_file_3_Species_test_results.csv"))
  
