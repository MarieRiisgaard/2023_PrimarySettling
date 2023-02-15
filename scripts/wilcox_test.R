###########################################
# Performing a pairwise wilcox test
#################################################3

# This test whether the different Genus or Species are significant different before and after primary settling 
# The test is conducted plantwise 
# 
# Assumptions 
# - I filter samples that do not have both before and after measurements
# - I mean over the relative abundances when there are replicates --> differnet number of observations!!

# log2_diff = ifelse(log2_diff == Inf, yes = 1, no = log2_diff) 
# 
# I can change
# - Filter all samples with a rel_abun of 0 
# - Filter to only test those with X observations before and X observations after (larger than 0)


tidy_all = data[[3]]

## Make random subsampling 
random_subsampling = T

if (random_subsampling == T) {
  set.seed(33)
  df_for_function <- tidy_all %>% 
    group_by(Plant, Date_rawdata) %>% 
    mutate(dublicate = cur_group_id()) %>% 
    ungroup()
  select_samples <- df_for_function %>% 
    distinct(Plant, dublicate) %>% 
    group_by(Plant) %>% 
    sample_n(24) %>% 
    ungroup() %>% 
    select(dublicate) %>% 
    unlist()
  df_for_function <- df_for_function %>% 
    filter(dublicate %in% select_samples) %>% 
    select(-dublicate)
} else {
  df_for_function <- tidy_all
   
}


df_for_function <- df_for_function %>% 
  mutate(samples = map(samples, ~group_by(., Family) %>% mutate(rel_abun_family = sum(rel_abun_ASV))))
  

# Function that filter taxa based on how many times they are observed in a sample
top_n_of_tax <- function(tax_level, rel_abun_tax, n_obs){
   
  species_vector = df_for_function %>% 
  mutate(samples = 
           map(samples, ~ distinct(., {{tax_level}}, {{rel_abun_tax}}) %>% 
                 filter(., {{rel_abun_tax}} > 0)
               )) %>%
  unnest(samples) %>% 
  distinct({{tax_level}}, PrimarySettler, Plant, Date_rawdata) %>% 
  group_by(across(c({{tax_level}}, PrimarySettler, Plant))) %>% 
  summarise(count = n(), .groups = "drop") %>%
  mutate(count = ifelse(count >= n_obs, TRUE, FALSE)) %>% 
    filter(count != FALSE) %>% 
    group_by(across(c(PrimarySettler, {{tax_level}}))) %>% 
    summarise(n = n(), .groups = "drop") %>% 
    filter(PrimarySettler == "Before" & n == 4 | 
           PrimarySettler == "After" & n == 4) %>% 
    distinct({{tax_level}}) %>% 
    unlist()
 
}
    

# Starting function
wilcox_test_group <- function(group_vec, group, rel_abun_group){
  
  print(group_vec)

# Making dataframe where the MEAN before and after cross replicates are taken
  sig_genera <- test %>%  
    filter({{group}} == group_vec) %>% 
    select(SampleID, Plant, PrimarySettler, Date_rawdata, {{group}} ,{{rel_abun_group}}) %>% 
    mutate(rel_abun_group = {{rel_abun_group}}) %>% 
    select(-{{rel_abun_group}}) %>% 
    group_by(across(c(Plant, PrimarySettler, Date_rawdata, {{group}}))) %>% 
    summarise(mean = mean(rel_abun_group, na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = PrimarySettler, values_from = mean) %>% 
    filter(!is.na(After)) %>% filter(!is.na(Before)) %>% 
    mutate(log2_diff = log2(After) - log2(Before),
           log2_diff = ifelse(log2_diff == -Inf, yes = -1, no = log2_diff), 
           log2_diff = ifelse(log2_diff == Inf, yes = 1, no = log2_diff) 
    )   %>% 
    group_by(Plant) %>% 
    mutate(
      mean_log2 = mean(log2_diff, na.rm = T), 
    ) %>% 
    ungroup()

  tax = select(sig_genera, {{group}}) %>% distinct() %>% unlist()
  mean_log2 <- sig_genera %>% distinct(Plant, mean_log2)
  
## Function for wilcox
  group_test_func <- function(plant){
    #plant = "Aalborg West"
    #species = "s__midas_s_4"
    data <- sig_genera %>% 
      filter(Plant == plant)
      wil <- wilcox.test(data$Before, data$After,
                         paired = T, alternative = "two.sided", exact = F) %>% tidy() %>%
        mutate(
          Plant = {{plant}},
          Tax = tax
          )
  }
  
  
# Appling function to make wilcox for each plant 
  x <- lapply(unique(df_for_function$Plant), group_test_func)
  x <-  as.data.frame(do.call(rbind, x))
  x <- x %>% inner_join(., mean_log2)
  x
}


## Make dataframes for testing
species_100 <- top_n_of_tax(Species, rel_abun_species, 12) %>% str_subset("ASV", negate = T)
genus_100 <- top_n_of_tax(Genus, rel_abun_genus, 12) %>% str_subset("ASV", negate = T)

############ SPECIES ################################
#Adding relative abundancefor the group: 
test <- df_for_function %>% 
  mutate(samples = map(samples, ~ 
                         distinct(., Species , rel_abun_species))) %>% 
  unnest(cols = samples)

Species = sapply(unlist(species_100), wilcox_test_group, Species, rel_abun_species, simplify = F) 
Species = as.data.frame(do.call(rbind, Species))
Species = Species %>% 
  filter(!is.na(method)) %>% 
  group_by(Plant) %>% 
  mutate(p_adjust = p.adjust(p.value, method = "BH")) %>% 
  select(Tax, Plant, p_adjust, mean_log2) %>% as_tibble()

write.csv(Species, file = paste0(outputPath, "/wilcox_rare_60000/species_wilcox_test_", Sys.Date(),"_rare_60000_min_12_all_samples.txt"))

  ############ Genus ################################
#Adding relative abundancefor the group: 
test <- df_for_function %>% 
  mutate(samples = map(samples, ~ 
                         distinct(., Genus , rel_abun_genus))) %>% 
  unnest(cols = samples)

Genus = sapply(unlist(genus_100), wilcox_test_group, Genus , rel_abun_genus, simplify = F) 
Genus = as.data.frame(do.call(rbind, Genus))
Genus = Genus %>% 
  #filter(!is.na(method)) %>% 
  group_by(Plant) %>% 
  mutate(p_adjust = p.adjust(p.value, method = "BH")) %>% ungroup() %>% 
  select(Tax, Plant, p_adjust, mean_log2) %>% as_tibble() 

write.csv(Genus, file = paste0(outputPath, 
                               "/wilcox_rare_60000/genus_wilcox_test_", Sys.Date(), "_rare_60000_min_12_random_subsampling.txt"))
write.csv(Genus, file = paste0(outputPath, 
                               "/wilcox_rare_60000/genus_wilcox_test_", Sys.Date(), "_rare_60000_min_12_all_samples.txt"))






################################################################
## Wilcox test on genus level for data rarefied at 100,000 reads
###############################################################


# Generate new dataframe with rarefaction at 100,000 reads
# setwd("C:/Users/marie/Desktop/GitKraken/PrimarySettling/")
# source("scripts/amp_load_2208.R")
# data <- master_function(reads_randers = 100000,reads_other = 100000,rarefy = T, save_non_merged_ampvis = T)
# rm(d_COD_3, d_meta_by_date, metadata, tidy_meta, master_function, mice_function_plantwise_ST, randers_prop_flow)
# 
# df_for_function <- data[[3]] 
# save(df_for_function, file = "C:/Users/marie/Desktop/GitKraken/PrimarySettling/output/environment_Rdata/Randers_rare_100000_20220930.Rdata")

# Load df that was generated
load(file = "C:/Users/marie/Desktop/GitKraken/PrimarySettling/output/environment_Rdata/Randers_rare_100000_20220930.Rdata")

top_n_of_tax_randers <- function(tax_level, rel_abun_tax, n_obs){
  
  species_vector = df_for_function %>% 
    filter(Plant == "Randers") %>% 
    mutate(samples = 
             map(samples, ~ distinct(., {{tax_level}}, {{rel_abun_tax}}) %>% 
                   filter(., {{rel_abun_tax}} > 0)
             )) %>%
    unnest(samples) %>% 
    distinct({{tax_level}}, PrimarySettler, Plant, Date_rawdata) %>% 
    group_by(across(c({{tax_level}}, PrimarySettler, Plant))) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(count = ifelse(count >= n_obs, TRUE, FALSE)) %>% 
    filter(count != FALSE) %>% 
    distinct({{tax_level}}) %>% 
    filter(str_detect({{tax_level}}, "nclassified", negate = T)) %>% 
    unlist()
  
}


# Run wilcox test for only randers
df_for_function <- df_for_function %>% mutate(Plant = as.character(Plant)) %>% filter(Plant == "Randers")

test <- df_for_function %>%  
  mutate(samples = map(samples, ~ 
                         distinct(., Genus , rel_abun_genus))) %>% 
  unnest(cols = samples)

Genus = sapply(unlist(top_n_of_tax_randers(Genus, rel_abun_genus, 12)), wilcox_test_group, Genus , rel_abun_genus, simplify = F) 
Genus = as.data.frame(do.call(rbind, Genus))
Genus = Genus %>% 
  #filter(!is.na(method)) %>% 
  group_by(Plant) %>% 
  mutate(p_adjust = p.adjust(p.value, method = "BH")) %>% ungroup() %>% 
  select(Tax, Plant, p_adjust, mean_log2) %>% as_tibble() 

write.csv(Genus, file = paste0(outputPath, 
                               "/wilcox_rare_60000/genus_wilcox_test_2022-09_30_rare_100000_min_12_all_samples_Randers.txt"))





##########################################################################
# Wilcox test made on hiher taxonomic level - not included in the article
##########################################################################


family_100 <- top_n_of_tax(Family, rel_abun_family, 12) %>% str_subset("ASV", negate = T)
order <- top_n_of_tax(Order, rel_abun_order, 5) %>% str_subset("ASV", negate = T)
class <- top_n_of_tax(Class, rel_abun_class, 5) %>% str_subset("ASV", negate = T)
phylum <- top_n_of_tax(Phylum, rel_abun_phylum, 5) %>% str_subset("ASV", negate = T)
guild_100 <- top_n_of_tax(Guild, rel_abun_guild, 5) 
growth_100 <- top_n_of_tax(growth_group_assignment, rel_abun_growth, 5) 



############ Guild ################################
test <- df_for_function %>% 
  mutate(samples = map(samples, ~ 
                         distinct(., Guild , rel_abun_guild))) %>% 
  unnest(cols = samples)
guild = sapply(unlist(guild_100), wilcox_test_group, Guild, rel_abun_guild, simplify = F) 
guild = as.data.frame(do.call(rbind, guild)) %>% 
  filter(!is.na(method)) %>% 
  group_by(Plant) %>% 
  mutate(p_adjust = p.adjust(p.value, method = "BH")) %>% 
  select(Tax, Plant, p_adjust, mean_log2) %>% as_tibble()
write.csv(guild, file = paste0(outputPath, "/wilcox_rare_60000/guild_wilcox_test_2022-03_20_rare_60000_n_20_min_0.005_rel_abun.txt"))

############ Family ################################
#Adding relative abundancefor the group: 
test <- df_for_function %>% 
  mutate(samples = map(samples, ~ 
                         distinct(., Family , rel_abun_family))) %>% 
  unnest(cols = samples)

Family = sapply(unlist(family_100), wilcox_test_group, Family , rel_abun_family, simplify = F) 
Family = as.data.frame(do.call(rbind, Family))
Family = Family %>% 
  filter(!is.na(method)) %>% 
  group_by(Plant) %>% 
  mutate(p_adjust = p.adjust(p.value, method = "BH")) %>% 
  select(Tax, Plant, p_adjust, mean_log2) %>% as_tibble()

write.csv(Family, file = paste0(outputPath, 
                               "/wilcox_rare_60000/family_wilcox_test_2022-10_06_rare_60000_min_12_all_samples.txt"))

############ Order ################################
#Adding relative abundancefor the group: 

test <- df_for_function %>% 
  mutate(samples = map(samples, ~ 
                         distinct(., Order , rel_abun_order))) %>% 
  unnest(cols = samples)

Order = sapply(unlist(order), wilcox_test_group, Order , rel_abun_order, simplify = F) 
Order = as.data.frame(do.call(rbind, Order))
Order = Order %>% 
  filter(!is.na(method)) %>% 
  group_by(Plant) %>% 
  mutate(p_adjust = p.adjust(p.value, method = "BH")) %>% 
  select(Tax, Plant, p_adjust, mean_log2) %>% as_tibble()
write.csv(Order, file = paste0(outputPath, "/wilcox_rare_60000/order_wilcox_test_2022-03_20_rare_60000_n_10_min_0.01_rel_abun.txt"))


############ Class ################################
#Adding relative abundancefor the group: 

test <- df_for_function %>% 
  mutate(samples = map(samples, ~ 
                         distinct(., Class , rel_abun_class))) %>% 
  unnest(cols = samples)

Class = sapply(unlist(class), wilcox_test_group, Class , rel_abun_class, simplify = F) 
Class = as.data.frame(do.call(rbind, Class))
Class = Class %>% 
  filter(!is.na(method)) %>% 
  group_by(Plant) %>% 
  mutate(p_adjust = p.adjust(p.value, method = "BH")) %>% 
  select(Tax, Plant, p_adjust, mean_log2) %>% as_tibble()
write.csv(Class, file = paste0(outputPath, "/wilcox_rare_60000/class_wilcox_test_2022-03_20_rare_60000_n_10_min_0.01_rel_abun.txt"))

############ Phylum ################################
#Adding relative abundancefor the group: 

test <- df_for_function %>% 
  mutate(samples = map(samples, ~ 
                         distinct(., Phylum , rel_abun_phylum))) %>% 
  unnest(cols = samples)

Phylum = sapply(unlist(phylum), wilcox_test_group, Phylum , rel_abun_phylum, simplify = F) 
Phylum = as.data.frame(do.call(rbind, Phylum))
Phylum = Phylum %>% 
  filter(!is.na(method)) %>% 
  group_by(Plant) %>% 
  mutate(p_adjust = p.adjust(p.value, method = "BH")) %>% 
  select(Tax, Plant, p_adjust, mean_log2) %>% as_tibble()
write.csv(Phylum, file = paste0(outputPath, "/wilcox_rare_60000/phylum_wilcox_test_2022-03_20_rare_60000_n_10_min_0.01_rel_abun.txt"))


############################################



