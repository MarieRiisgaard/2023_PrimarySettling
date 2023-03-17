#############################################################################
#  Run this script first to generate the different dataframes
#############################################################################

#Packages
#library(metR) # Season function 
pacman::p_load(ampvis2,
               readxl, 
               vroom, 
               tidyverse)

############################################################################
#   Load data and Filter
###############################################################################

metadata = read_xlsx(paste0(DataPath, "sequencing_data/metadata.xlsx"))

# function from https://newbedev.com/determine-season-from-date-using-lubridate-in-r
# the season cut dates (in the form MMDD) are based on the astronomical seasons. 
# correlates better with the process tank temperature profile than cutting by months.
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  cuts <- base::cut(numeric.date, breaks = c(0,320,0621,0923,1222,1231))
  levels(cuts) <- c("Winter","Spring","Summer","Autumn","Winter")
  return(cuts)
}

## Filter samples that I suspect has been mixmatched
filter_samples <- c("MQ200420-49", ## Aalborg West --> cluster with negative
                    "MQ210607-46", ## Esbjerg West --> cluster with positives
                    "MQ211004-150") ## Esbjerg West --> cluster somewhat with positives

## Organize metadata
metadata <- metadata %>% 
  group_by(SampleSite, SampleDate, PrimarySettler, LocationBeforeSettler) %>% 
  mutate(dublicate = cur_group_id()) %>% 
  ungroup() %>% 
  rename("Plant" = "SampleSite", 
         "Location" = "SampleContent", 
         "Date_rawdata" = "SampleDate") %>%
  select(-RunID, -LibraryID) %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata)) %>% 
  filter(!SampleID %in% filter_samples) %>% 
  filter(Date_rawdata > "2019-03-01" & 
           Date_rawdata < "2020-05-01") %>% 
  filter(!is.na(Date_rawdata)) %>% 
  filter(Location != "AS") %>% 
  filter(Plant != "AAU")
  
rm(filter_samples)

master_function <- function(
    only_paired_dates = T,    # <- condition 
    rarefy = T,
    reads_randers = 60000,   # <- condition  (no_rarefaction, both10000, Randers1000000_others60000)
    reads_other = 60000,
    metadata_load = T, 
    merge_influent_streams = T,
    save_non_merged_ampvis = F
){

## Filter samples based on minimun reads
  print(paste0("Min reads Randers: ", reads_randers, "; Min reads others: ", reads_other))
  
  d <- amp_load(otutable = paste0(DataPath, "sequencing_data/ASVtable_notax.tsv"), 
                metadata = metadata,
                taxonomy = paste0(DataPath, "sequencing_data/ASVs.R1.sintax"))
  
  randers_samples <- d %>% 
   amp_subset_samples(Plant == "Randers", minreads = reads_randers) 
 others_samples <- d %>% 
   amp_subset_samples(Plant != "Randers", minreads = reads_other) 
 sampleIDs <- c(randers_samples$metadata$SampleID, others_samples$metadata$SampleID) %>% unlist()
  
 metadata <- metadata %>% filter(SampleID %in% sampleIDs)
 
## Filtering by only taking paired samples 
print(paste0("only_paired_dates is: ", only_paired_dates))
if (only_paired_dates == T) {
  paired_samples <-  
    metadata %>% 
    distinct(Plant, LocationBeforeSettler, Location, PrimarySettler, Date_rawdata) %>% 
    group_by(Plant, Date_rawdata) %>% 
    summarise(count = n(), .groups = "drop") %>% 
    filter(if_else(Plant == "Randers", count == 4, count == 2)) %>%
    left_join(., metadata, by = c("Plant", "Date_rawdata")) %>% 
    distinct(SampleID) %>% unlist()
} else {
  paired_samples <- metadata %>% distinct(SampleID) %>% unlist() 
}

# Filter samples in metadata before loarding new dataframe 
metadata <- metadata %>% filter(SampleID %in% paired_samples)

## Load Ampvis object
d <- amp_load(otutable = paste0(DataPath, "sequencing_data/ASVtable_notax.tsv"), 
              metadata = metadata,
              taxonomy = paste0(DataPath, "sequencing_data/ASVs.R1.sintax"))


# Ampvis object not-merged
d_min_non_merged <- d

###############################################################################
# Metadata and merge Randers influents
###############################################################################

# Load meta data
print(paste0("Load metadate: ", metadata_load))
if (metadata_load == T) {
  source(file = paste0(SourcePath, "/load_metadata.R"), echo = F, verbose = F)
} else {
  d <- d
}


d <- d %>% amp_merge_replicates(merge_var = "dublicate", round = "down")


###############################################################################
# Rarefy
###############################################################################

#rarefy = T
#reads_randers = 5000   # <- condition  (no_rarefaction, both10000, Randers1000000_others60000)
#reads_other = 5000

print(paste0("Rarefy: ", rarefy, "! Reads Randers: ", reads_randers, "; reads others: ", reads_other))

## Randers
if (rarefy == T) {
  d1 <- d %>% 
    amp_subset_samples(Plant == "Randers", rarefy = reads_randers)
} else {
  d1 <- d %>% 
    amp_subset_samples(Plant == "Randers")
  }

d$metadata %>% group_by(Plant, PrimarySettler) %>% summarise(n())

## Others
if (rarefy == T) {
  d2 <- d %>% 
    amp_subset_samples(Plant != "Randers", rarefy = reads_other)
} else {
  d2 <- d %>% 
    amp_subset_samples(Plant != "Randers")
}


## Merge replicates by mean
d <- amp_merge_ampvis2(d1, d2, by_refseq = F)


## Merge Randers samples 
print(paste0("Merge influent streams for Randers: ", merge_influent_streams))
if (merge_influent_streams == T) {
  source(file = paste0(SourcePath, "/flow_proportinal_mean.R"))
  d <- randers_prop_flow(d)
  rm(randers_prop_flow)
} else {
  d <- d
}

d_min_read <- d
rm(d, d1, d2)


###############################################################################
# Assign tax to last unclassified  
################################################################################

d_min_read$tax <- d_min_read$tax %>% 
  mutate(Phylum = if_else(condition = str_detect(Phylum, 'p__'), true = Phylum, false = paste0("unclassified_", Kingdom))) %>% 
  mutate(Class = if_else(condition = str_detect(Class, 'c__'), true = Class, false = paste0("unclassified_", Phylum))) %>% 
  mutate(Order = if_else(condition = str_detect(Order, 'o__'), true = Order, false = paste0("unclassified_", Class))) %>% 
  mutate(Family = if_else(condition = str_detect(Family, 'f__'), true = Family, false = paste0("unclassified_", Order))) %>% 
  mutate(Genus = if_else(condition = str_detect(Genus, 'g__'), true = Genus, false = paste0("unclassified_", Family))) %>% 
  mutate(Species = if_else(condition = str_detect(Species, 's__'), true = Species, false = paste0("unclassified_", Genus))) %>% 
  mutate(Class = str_replace(string = Class, pattern = ".*(_[a-z]__.*)", replacement = "unclassified\\1")) %>% 
  mutate(Order = str_replace(string = Order, pattern = ".*(_[a-z]__.*)", replacement = "unclassified\\1")) %>% 
  mutate(Family = str_replace(string = Family, pattern = ".*(_[a-z]__.*)", replacement = "unclassified\\1")) %>% 
  mutate(Genus= str_replace(string = Genus, pattern = ".*(_[a-z]__.*)", replacement = "unclassified\\1")) %>% 
  mutate(Species = str_replace(string = Species, pattern = ".*(_[a-z]__.*)", replacement = "unclassified\\1")) %>% 
  mutate(Phylum = if_else(str_detect(Phylum, "unclassified"), paste0(Phylum, "_", OTU), Phylum),
         Class = if_else(str_detect(Class, "unclassified"), paste0(Class, "_", OTU), Class),
         Order = if_else(str_detect(Order, "unclassified"), paste0(Order, "_", OTU), Order),
         Family = if_else(str_detect(Family, "unclassified"), paste0(Family, "_", OTU), Family),
         Genus = if_else(str_detect(Genus, "unclassified"), paste0(Genus, "_", OTU), Genus),
         Species = if_else(str_detect(Species, "unclassified"), paste0(Species, "_", OTU), Species)
  )


##################################################################################
### Introducing growth group and guilds
####################################################################################

growth <- read_xlsx(path = paste0(DataPath, "growth_groups_dottorini_2021/growth_in_bio_tank.xlsx")) %>% 
  select(Species_rename, growth_group_assignment, Guild) %>% 
  rename(Species = Species_rename)


### Seperating genus and species level --> to be at OTU level!!!
tidy_all_OTU <- d_min_read$tax %>% select(Species, OTU) %>% distinct(.keep_all = T)
growth_species <- growth %>%  filter(str_detect(string = Species, pattern = "s__")) %>% 
  left_join(., tidy_all_OTU, by = "Species") %>% select(-Species) %>% distinct(.keep_all = T)
growth_ASV <- growth %>% filter(!str_detect(string = Species, pattern = "s__")) %>% 
  separate(col = Species, into = c("tax", "OTU"), sep = "_(?=[^_]+$)") %>% select(-tax) %>% 
  left_join(., tidy_all_OTU, by = "OTU") %>% select(-Species) %>%  distinct(.keep_all = T) 
growth <- rbind(growth_ASV, growth_species) %>% filter(!str_detect(OTU, "newASV")) %>% distinct(OTU, .keep_all = T)



############################################################################################
####### Calculation of rel_abun          ################################################
#########################################################################################


tidy_plant <- function(plant){
  
  #d_min_read <- amp_subset_samples(d_min_read,SampleID == "MQ211110-6")
  #plant = c("Randers")
  
  tidy_test <- d_min_read %>% 
    amp_subset_samples(Plant == plant) %>% 
    amp_export_long() %>% 
    #filter(count != 0) %>%   #  <--- filtering step to make datahandling faster (8/8-22) 
    left_join(., growth, by = c("OTU")) %>% 
    replace_na(list(growth_group_assignment = 'not_known', Guild = 'not_known')) %>% 
    mutate(
      growth_group_assignment = 
        ifelse(str_detect(string = Genus, pattern = "unclassified") & growth_group_assignment != "not_known", yes = "not_known", no = growth_group_assignment), 
      growth_group_assignment = 
        ifelse(str_detect(string = Species, pattern = "unclassified") & growth_group_assignment != "not_known", yes = "Unclassified ASVs", no = growth_group_assignment), 
      Guild = 
        ifelse(str_detect(string = Genus, pattern = "unclassified") & Guild != "not_known", yes = "not_known", no = Guild), 
      Guild = 
        ifelse(str_detect(string = Species, pattern = "unclassified") & Guild == "not_known", yes = "Unclassified ASVs", no = Guild), 
    ) %>% 
    nest(samples = c(Kingdom, Phylum, Class, Order, Family, Genus, Species, OTU, count, growth_group_assignment, Guild)) %>%
    mutate(samples = map(.x = samples, 
                         ~mutate(.x, 
                                 tot_count = sum(count)))) %>% 
    mutate(samples = map(.x = samples, 
                         ~mutate(.x, 
                                 rel_abun_ASV = count/tot_count*100))) %>% 
    mutate(samples = map(.x = samples, ~group_by(.x, Species) %>% mutate(rel_abun_species = sum(rel_abun_ASV)))) %>% 
    mutate(samples = map(.x = samples, ~group_by(.x, Genus) %>% mutate(rel_abun_genus = sum(rel_abun_ASV)))) %>% 
    mutate(samples = map(.x = samples, ~group_by(.x, growth_group_assignment) %>% mutate(rel_abun_growth = sum(rel_abun_ASV)))) %>% 
    mutate(samples = map(.x = samples, ~group_by(.x, Guild) %>% mutate(rel_abun_guild = sum(rel_abun_ASV)))) %>% 
    mutate(samples = map(.x = samples, ~mutate(.x, growth_group_assignment = factor(growth_group_assignment, 
                                                                                    levels = c("growing",  "disappearing", "surviving", "ambiguous", 
                                                                                               "not_known", "Unclassified ASVs"),
                                                                                    labels=c("__Growing__", "__Disapearing__", "__Surviving__", "__Ambiguous__", 
                                                                                             "__Not known__", "__Unclassified ASVs__"))))) %>%
    mutate(samples = map(.x = samples, ~mutate(.x, Guild = factor(Guild, 
                                                                  levels = c( "PAO", "nitrifiers", "filaments", "GAO", "Other", 
                                                                              "not_known", "Unclassified ASVs"),
                                                                  labels = c("__PAO__","__Nitrifiers__", "__Filaments__", "__GAO__" ,"__Other__", 
                                                                             "__Not known__", "__Unclassified ASVs__")))))
  
  
  
}


tidy_Randers <- tidy_plant("Randers")
tidy_Esbjerg_W <- tidy_plant("Esbjerg West")
tidy_Ejby_Molle <- tidy_plant("Ejby Mølle")
tidy_Aalborg_W <- tidy_plant("Aalborg West")
tidy_all <- rbind(tidy_Randers, tidy_Esbjerg_W, tidy_Ejby_Molle, tidy_Aalborg_W)
tidy_all <- tidy_all %>% mutate(PrimarySettler = factor(PrimarySettler, 
                                                        levels = c("before", "after"), 
                                                        labels = c("Before", "After")))



rm(growth, tidy_Randers, tidy_Ejby_Molle, tidy_Esbjerg_W, tidy_Aalborg_W, tidy_plant)


#############################################
# Adding meta data to read data
################################################

tidy_meta <- tidy_all %>% 
  select(SampleID, Plant, Location, PrimarySettler, Date_rawdata) %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata)) %>% 
  left_join(., d_meta_by_date) %>% 
  mutate(Season = getSeason(Date_rawdata), 
         Date_numeric = as.numeric(Date_rawdata))

# Load new metadata in ampvis dataframe
d_min_read <- amp_load(otutable = d_min_read$abund, 
                       taxonomy = d_min_read$tax, 
                       metadata = tidy_meta)

# Load new metadata in tidy dataframe
tidy_all <- tidy_all %>%  
  select(SampleID, Plant, Date_rawdata, PrimarySettler, Location, samples) 

tidy_meta <- tidy_all %>% select(-samples)



################################################
## Merging relative abundance at the species level:
#######################################################

species_abun_tab <- d_min_read$abund %>% 
  rownames_to_column("OTU") %>% 
  left_join(d_min_read$tax, by = "OTU") %>% 
  relocate(OTU,Kingdom, Phylum, Class, Order, Family, Genus, Species) %>% 
  mutate(OTU_no = OTU, 
         OTU = Species) %>% 
  group_by(OTU,Kingdom, Phylum, Class, Order, Family, Genus, Species) %>%
  summarise(across(where(is.numeric), sum)) %>% 
  relocate(c(Kingdom, Phylum, Class, Order, Family, Genus, Species), 
           .after = last_col())


amp_merged_species <- 
  amp_load(otutable = species_abun_tab,
           metadata = d_min_read$metadata) %>% 
  amp_subset_samples(normalise = T)


amp_merged_species$metadata <- amp_merged_species$metadata %>% 
  mutate(PrimarySettler = factor(PrimarySettler, 
                                 levels = c("before", "after"), 
                                 labels = c("Before", "After")))


###############################################################################################
# 
###############################################################################################

data <- list(d_min_read, d_meta_by_date, tidy_all, amp_merged_species, d_min_non_merged)


data
}















