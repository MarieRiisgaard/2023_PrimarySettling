#########################################
# Make metadata for NCBI
###################################


# see <- data[[3]] %>% 
#   mutate(samples = 
#          map(.x = samples, ~
#                filter(.x, Genus == "g__Defluviicoccus" & rel_abun_species > 0))) %>% 
#   unnest(samples) %>% 
#   distinct(SampleID, Plant, Genus, Species) %>% 
#   group_by(Plant, Genus, Species) %>%
#   summarise(n())


pacman::p_load(openxlsx,
               tidyverse)
      
# Load data and exstract samples that were used in the analysis 
OutputPath <- "C:/Users/HD95LP/OneDrive - Aalborg Universitet/visual_studio_code/2023_PrimarySettling/output/"
load(paste0(OutputPath, "R_environments/","Environment_20230315.RData"))
#data[[5]]$metadata %>% group_by(Plant, PrimarySettler) %>% summarise(n())
select_samples <- data[[5]]$metadata %>% distinct(SampleID) %>% unlist()
data[[5]]$metadata %>% distinct(SampleID) %>% write_tsv(., "2023_PrimarySettling/output/files/filenames_20230317.txt", col_names = F)

gps <- tibble(
  SampleSite = c("Aalborg West", "Esbjerg West", "Ejby Mølle", "Randers", "AAU"), 
  lat_lon = c("57.0480 N 9.8655 E", "55.4880 N 8.4307 E", "55.3980 N 10.4150 E", "56.4539 N 10.0708 E", "57.0146 N 9.9847 E") 
)


fastq_ID = read.delim("C:/Users/HD95LP/OneDrive - Aalborg Universitet/visual_studio_code/2023_PrimarySettling/data/sequencing_data/fastq_IDs_20230317", header = F)
fastq_ID <- fastq_ID %>% 
  mutate(SampleID = sub("_.*", "", V2)) %>% 
  filter(SampleID != "Undetermined") %>% 
  arrange(V2) %>% 
  group_by(SampleID) %>% 
  mutate(ID = row_number()) %>% 
         # reads = ifelse(str_detect(V2, "_R1_"), "forward_reads", "NA"),
         # reads = ifelse(str_detect(V2, "_R2_"), "reverse_reads", reads),
         # position = sub("^.*?_([^_]+).*", "\\1", V2)
         #) %>% 
  #distinct() %>% 
  pivot_wider(names_from = ID, values_from = V2, names_prefix = "file_")

make_metadata_NCBI <- 
  readxl::read_xlsx("C:/Users/HD95LP/OneDrive - Aalborg Universitet/visual_studio_code/2023_PrimarySettling/data/sequencing_data/metadata.xlsx") %>% 
  filter(SampleID %in% select_samples)
make_metadata_NCBI <- 
  left_join(make_metadata_NCBI, fastq_ID, multiple = "all") %>% 
  filter(SampleContent != "AS") %>% 
  filter(!is.na(file_1)) %>% 
  left_join(gps)

make_metadata_NCBI <- make_metadata_NCBI %>% 
  mutate(
    sample_name = SampleID,
    runID = V1,
    library_strategy = "AMPLICON",
    instrument_model = "Illumina MiSeq",
    organism = "wastewater metagenome", 
    collection_date = SampleDate, 
    sample_title = NA, 
    bioproject_accession = NA,
    collection_date = ifelse(is.na(SampleDate), "XXXX-XX-XX", collection_date),
    env_broad_scale =  "aquatic biome [ENVO:00002030]",
    env_local_scale =  "Waste Water [ENVO:00002001]", 
    env_medium = "Waste Water [ENVO:00002001]",
    sample_location = ifelse(is.na(LocationBeforeSettler) | LocationBeforeSettler == "NA", 
                       paste0(PrimarySettler, " primary settling"), 
                       paste0(PrimarySettler, " primary settling (",LocationBeforeSettler, ")")
                       ),
    sample_content = ifelse(SampleSite == "AAU", SampleContent, env_medium),
    geo_loc_name = paste0("Denmark", ":", SampleSite), 
    file_format = "fastq",
  ) %>% 
  mutate(geo_loc_name = str_replace(geo_loc_name, "ø", "oe"), 
         collection_date = str_replace(collection_date, "XXXX-XX-XX", "not applicable")
  )

######################
# Sample metadat #
####################

metadata <- make_metadata_NCBI %>% 
  select(sample_name, sample_title, bioproject_accession,
         organism, collection_date, env_broad_scale, env_local_scale, env_medium, geo_loc_name, lat_lon,
         sample_location, SampleContent, runID, LibraryID
         #library_strategy, instrument_model, LibraryID,
         #file_format, file_1, file_2#, design_description
         ) 
 
# Create a new workbook
my_wb <- createWorkbook()
# Add a worksheet to the workbook
addWorksheet(my_wb, "Sheet1")
# Write the data to the worksheet
writeData(my_wb, "Sheet1", metadata)
# Save the workbook to a file
saveWorkbook(my_wb, "2023_PrimarySettling/output/files/2023_03_17_metadata_ncbi.xlsx")


########################
# SRA metadata ### 
#################################

SRA_metadata <- make_metadata_NCBI %>% 
  mutate(
    library_ID = LibraryID, 
    title = "Amplicon sequencing of influent wastewater community before and after primary settling", 
    library_source = "METAGENOMIC", 
    library_selection = "PCR", 
    #library_layout = "single",
    library_layout = case_when(str_detect(file_1, "_R2_") | str_detect(file_2, "_R2_") ~ "PAIRED",.default = "single"), 
    platform = "ILLUMINA", 
    design_description = "DNA extraction, sample preparation including amplification of V1-3 region of 16S rRNA gene using the 27F (AGAGTTTGATCCTGGCTCAG) (Lane, 1991) and 534R 194 (ATTACCGCGGCTGCTGG) (Muyzer et al., 1993) primers, and amplicon sequencing were conducted as described by Dottorini et al., (2021)",
    filename = file_1, 
    filename2 = file_2, 
    filetype = file_format
    ) %>% 
  select(sample_name, library_ID, title, library_strategy,library_source,	library_selection,
         library_layout,	platform,	instrument_model,	design_description,	filetype,	filename,	filename2)

# Create a new workbook
my_wb <- createWorkbook()
# Add a worksheet to the workbook
addWorksheet(my_wb, "Sheet1")
# Write the data to the worksheet
writeData(my_wb, "Sheet1", SRA_metadata)
# Save the workbook to a file
saveWorkbook(my_wb, "2023_PrimarySettling/output/files/2023_03_17_SRA_metadata.xlsx")

  
         

#######################################
# File for subsetting fastq files
#######################################


files_names_to_upload <- make_metadata_NCBI %>% select(file_1, file_2) %>% 
  pivot_longer(cols = c(file_1, file_2), values_to = "file_names", names_to = "reads") %>% 
  filter(!is.na(file_names)) %>% 
  select(file_names)

write_tsv(files_names_to_upload, "2023_PrimarySettling/output/files/filenames_ncbi_20230317.txt", col_names = F)

unique(files_names_to_upload$file_names)

# Checking that all samples has a corresponding rawdatafile
check <- files_names_to_upload %>% 
  mutate(SampleID = sub("_.*", "", file_names)) %>% 
  distinct(SampleID) #%>% summarise(n())
data[[5]]$metadata %>% distinct(SampleID) #%>% summarise(n())
anti_join(data[[5]]$metadata, check)
                                 