############################################################
# Make flow-proportinal mean of the 3 influents from Randers
############################################################

# dates <- d_min_read$metadata %>% 
#   filter(Plant == "Randers") %>% 
#   group_by(Date_rawdata) %>% 
#   summarise(count = n()) %>% 
#   filter(count == 4) %>% 
#   select(Date_rawdata)

randers_prop_flow <- function(d){

tidy_randers <- d %>% 
  amp_subset_samples(Plant == "Randers") %>% 
  amp_subset_samples(PrimarySettler != "after") %>% 
  amp_export_long() %>%  
  select(-dublicate) %>% 
  left_join(., 
            d_COD_3 %>% mutate(
              LocationBeforeSettler = as.character(LocationBeforeSettler),
              PrimarySettler = as.character(PrimarySettler), 
              PrimarySettler = str_to_lower(PrimarySettler)) %>%
              distinct(Plant, Date_rawdata, PrimarySettler, LocationBeforeSettler, flow_loc_m3, Flow_beforePS_m3), 
            by = c("Plant", "PrimarySettler", "Date_rawdata")) %>%
  nest(samples = c(-Plant, -Location, -PrimarySettler, -Date_rawdata)) %>% #sample_n(3) %>% 
  mutate(samples = map(.x = samples, 
                       ~mutate(.x, 
                         count = flow_loc_m3/Flow_beforePS_m3*count) %>% 
                         distinct(OTU, SampleID, .keep_all = T) %>% 
                         group_by(OTU) %>% 
                         summarise(
                           check = n(),
                           count = sum(count), 
                           SampleID = paste0(SampleID, collapse = "-"),
                           Kingdom, Phylum, Class, Order, Family, Genus, Species,
                           .groups = "drop") %>% 
                       filter(check == 3) %>% 
                       select(-check) %>% 
                       distinct(OTU, .keep_all = T)
                       )) 

print("Made tidy_randers_df")

randers_OTU <- 
  tidy_randers %>% sample_n(1) %>% unnest(samples) %>% 
  distinct(OTU, Kingdom, Phylum, Class, Order, Family, Genus, Species) %>% 
  column_to_rownames(var = "OTU")


randers_abund <- 
  tidy_randers %>% 
  unnest(samples) %>% 
  select(SampleID, OTU, count) %>% 
  mutate(count = round(count)) %>%
  pivot_wider(names_from = SampleID, values_from = count#, values_fill = 0
              ) %>% 
  column_to_rownames(var = "OTU")

randers_meta <- tidy_randers %>% 
  mutate(samples = map(.x = samples, 
                       ~distinct(.x, SampleID))) %>%
  unnest(samples) %>% 
  distinct(SampleID, Plant, Date_rawdata, PrimarySettler) %>% 
  relocate(SampleID) %>% 
  mutate(Location = "IWW")

randers_merged <- amp_load(
  otutable = randers_abund, 
  taxonomy = randers_OTU,
  metadata = randers_meta
)


# Filter dataframe 'd' of Randers samples
d <- d %>% amp_subset_samples(!Plant == "Randers" | !PrimarySettler == "before")

# Add the merged Randers samples
d <- amp_merge_ampvis2(d, randers_merged, by_refseq = F)

print("Merged into ampvis")

rm(rander_abund, randers_OTU)
d
}











