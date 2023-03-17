###############################################################
###### This script orders metadata sent from plant operators ##
###############################################################

# Input is the 'metadata' dataframe generated in 'load_data.R' + files in the 'WWTP_metadata' folder


# Subset to only metadata that can be combined using sampleID or date
tidy_meta <- metadata %>% 
  select(SampleID, Plant, Location, PrimarySettler, LocationBeforeSettler, Date_rawdata) %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata), 
         week_year = as.Date(Date_rawdata) %>% format("%W-%Y"))


###############################
#     Aalborg West            #
###############################

## Load metadata by the location. Data is combined based on the "fra" date 
    # COD is reparted weekly (if availible)
        # --> in case of more measurement pr. week, these are meaned (not the case for Aalborg W)

d_COD_1 <- read_xlsx(
  path = paste0(DataPath, "WWTP_metadata","/AalborgWest_Dataindsamling 2018 til 2020.xlsx"), 
  sheet = "Sheet1") %>% 
  rename("Date_rawdata" = "Fra") %>% #, "Døgnflow_m3_before" = "Døgnflow_m3", "COD_mg_L_before" = "COD_mg_L") %>% 
  select(-Til, -Timeflow_m3_h, -Døgnflow_m3) %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata) %>% format("%Y-%m-%d")) %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata))

d_COD <- d_COD_1 %>% 
  mutate(week_year = as.Date(Date_rawdata) %>% format("%W-%Y")) %>% select(-Date_rawdata) %>% 
  group_by(PrimarySettler, week_year, Plant) %>%
  summarise(mean_COD_mg_l = mean(COD_mg_L),
            .groups = "drop") %>%
  mutate(Plant = "Aalborg West")

d_flow <-  read_xlsx(
  path =  paste0(DataPath, "WWTP_metadata","/AalborgWest_Dataindsamling 2018 til 2020.xlsx"),
  sheet = "Sheet2") %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata) %>% format("%Y-%m-%d")) %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata)) %>%
  mutate(Flow_afterPS_m3 = as.numeric(Flow_afterPS_m3), 
         Plant = "Aalborg West")

## Make dataframe for d_meta_by_date
d_flow_AAW <- d_flow %>% 
  full_join(., d_COD_1 %>%
              pivot_wider(names_from = PrimarySettler, values_from = COD_mg_L)) %>% 
  rename("COD_beforePS" = "Before", "COD_afterPS" = "After", "Precipitation_mm" = "Nedbør_mm")
  

rm(d_COD, d_COD_1, d_flow)

###############################
#      Randers                #
###############################

## Read flow_data and removes data with no date
d_flow <- read_xlsx(
  path = paste0(DataPath, "WWTP_metadata","/Randers_indløbs_flow.xlsx"), 
  skip = 7) %>% 
  select(-...2, -...3, -...4, -Beskrivelse) %>% slice(4:nrow(.)) %>% 
  mutate(...8 = as.Date(...8)) %>% rename("Date_rawdata" = "...8") %>% 
  mutate(Plant = "Randers", 
         Flow_nord = as.numeric(`Flowmåling Nord 2 (F_CV)`), 
         Flow_syd= as.numeric(`Flowmåler syd (F_CV)`), 
         Flow_vest = as.numeric(`Flowmåling Vest (F_CV)`), 
         Flow_beforePS_m3 = Flow_vest + Flow_syd + Flow_nord, 
         Flow_afterPS_m3 = as.numeric("NA")) %>%
  select(-`Flowmåling Nord 2 (F_CV)`, -`Flowmåler syd (F_CV)`, -`Flowmåling Vest (F_CV)`) %>% 
  filter(!is.na(Date_rawdata))

# Read COD data
d_COD <- read_xlsx(
  path = paste0(DataPath, "WWTP_metadata","/COD_Randers.xlsx"), skip = 10) %>% 
  select("mg/l...5", "mg/l...6", "mg/l...7", "mg/l...8", "mg/l...12", "Enhed...13", "mg/l...17") %>% 
  rename("COD_nord" = "mg/l...5", 
         "COD_syd" = "mg/l...6", 
         "COD_vest" = "mg/l...7", 
         "COD_beforePS"= "mg/l...8", 
         "COD_afterPS"= "mg/l...17",
         "COD_effluent" = "mg/l...12",
         "Date_rawdata" = "Enhed...13") %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata) %>% format("%Y-%m-%d")) %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata), 
         COD_nord = as.numeric(COD_nord), 
         COD_syd = as.numeric(COD_syd),
         COD_vest = as.numeric(COD_vest), 
         COD_beforePS = as.numeric(COD_beforePS)) %>%
  mutate(week_year = as.Date(Date_rawdata) %>% format("%W-%Y")) %>% 
  filter(!is.na(Date_rawdata))

## Join flow and COD to make flow proportianalt COD calculation 
d_COD <- full_join(d_flow, d_COD) %>% 
  mutate(COD_beforePS = if_else(is.na(COD_beforePS), 
                   (Flow_nord/Flow_beforePS_m3 * COD_nord) + 
                   (Flow_syd/Flow_beforePS_m3 * COD_syd) + 
                   (Flow_vest/Flow_beforePS_m3 * COD_vest)/ 3, 
         COD_beforePS), 
         COD_effluent = as.numeric(COD_effluent), 
         COD_afterPS = as.numeric(COD_afterPS), 
         ) %>% 
  filter(!is.na(Date_rawdata))

# Make flow and COD with all dates cotaining the correkt columns
d_flow_Ran <- d_COD %>% 
  select(Date_rawdata, Plant, Flow_beforePS_m3, Flow_afterPS_m3, COD_beforePS, COD_afterPS, COD_effluent) 

## 
d_COD_1 <- d_COD %>%  # Pivot longer with flows by location 
  select(Date_rawdata, Plant, Flow_nord, Flow_syd, Flow_vest, week_year) %>% 
  pivot_longer(cols = c(Flow_nord, Flow_syd, Flow_vest), values_to = "flow_loc_m3", names_to = "LocationBeforeSettler") %>% 
  mutate(LocationBeforeSettler = if_else(LocationBeforeSettler == "Flow_nord", "NV", LocationBeforeSettler), 
         LocationBeforeSettler = if_else(LocationBeforeSettler == "Flow_syd", "SØ", LocationBeforeSettler), 
         LocationBeforeSettler = if_else(LocationBeforeSettler == "Flow_vest", "SV", LocationBeforeSettler)) %>% 
  filter(!is.na(flow_loc_m3)) %>% 
  mutate(PrimarySettler = "Before")
  
# Order the COD measurement on the different influent locations 
d_COD_2 <- d_COD %>%  # Pivot longer with COD by location 
  select(Date_rawdata, Plant, week_year, COD_nord, COD_syd, COD_vest) %>% 
  pivot_longer(cols = c(COD_nord, COD_syd, COD_vest), values_to = "COD_loc_mg_L", names_to = "LocationBeforeSettler") %>% 
  mutate(LocationBeforeSettler = if_else(LocationBeforeSettler == "COD_nord", "NV", LocationBeforeSettler), 
         LocationBeforeSettler = if_else(LocationBeforeSettler == "COD_syd", "SØ", LocationBeforeSettler), 
         LocationBeforeSettler = if_else(LocationBeforeSettler == "COD_vest", "SV", LocationBeforeSettler)) %>% 
  select(-Date_rawdata) %>% 
  group_by(week_year, Plant, LocationBeforeSettler) %>%
  summarise(mean_loc_COD_mg_l = mean(COD_loc_mg_L, na.rm = T),
            .groups = "drop") %>%
  filter(!is.na(mean_loc_COD_mg_l)) %>% 
  mutate(PrimarySettler = "Before") 
  
# Calculate mean COD before and after
d_COD_3 <- d_COD %>% select(Date_rawdata, Plant, week_year, COD_beforePS) %>% 
  filter(!is.na(COD_beforePS)) %>% 
  group_by(week_year, Plant) %>%
  summarise(mean_COD_mg_l = mean(COD_beforePS),
            .groups = "drop") %>%
  mutate(PrimarySettler = "Before") %>% distinct(.keep_all = T) %>% 
  full_join(.,d_COD_2) %>% 
  filter(!is.na(mean_loc_COD_mg_l)) %>% full_join(d_COD_1) %>% 
  full_join(., d_COD %>% select(Date_rawdata, Plant, week_year, COD_afterPS) %>% 
          filter(!is.na(COD_afterPS)) %>% 
          group_by(week_year, Plant) %>%
          summarise(mean_COD_mg_l = mean(COD_afterPS), Date_rawdata,
                    .groups = "drop") %>%
          mutate(PrimarySettler = "After",
          PrimarySettler = factor(PrimarySettler, levels = c("Before", "After")))
          ) %>% 
  full_join(., select(d_COD, Date_rawdata, Flow_beforePS_m3, Flow_afterPS_m3, COD_effluent)) %>% 
  mutate(PrimarySettler = 
           factor(PrimarySettler, levels = c("Before", "After"), labels = c("Before", "After")), 
         LocationBeforeSettler = factor(LocationBeforeSettler))

tidy_meta_Ran <- full_join(tidy_meta %>% filter(Plant == "Randers") %>% 
                           mutate(LocationBeforeSettler = factor(LocationBeforeSettler)), 
                         d_COD_3) %>% filter(!is.na(SampleID)) 

rm(d_COD, d_COD_1, d_COD_2, d_flow)


##############################
#     Esbjerg                #
##############################


## All dates 
d_flow_EsW <- read_xlsx(
  path = paste0(DataPath, "WWTP_metadata","/EsbjergWest-monthly_Bio-Immigration - Aalborg Uni rådata.xlsx"), 
  sheet = "Rådata") %>% 
  rename("Date_rawdata" = "Dato", 
        "Flow_beforePS_m3" = 2, 
        "Flow_afterPS_m3" = 5, 
        "Precipitation_mm" = "Nedbør - Indløb (mm)", 
        "COD_beforePS" = "COD - Indløb (mg/l)", 
        "COD_afterPS" = "COD - BioInd (mg/l)" ) %>% 
  select(-7) %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata) %>% format("%Y-%m-%d")) %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata), 
         Plant = "Esbjerg West", 
         week_year = as.Date(Date_rawdata) %>% format("%W-%Y"))

## All dates pH data
d_pH_EsW <- read_xlsx(
  path = paste0(DataPath, "WWTP_metadata","/Esbjerg_West_pH_målinger.xlsx"), 
  sheet = "Ark1") %>% 
  rename("Date_rawdata" = "Dato", 
         "pH_BeforePS" = "pH - Indløb", 
         "COD_beforePS" = "COD - Indløb (mg/l)", 
         "COD_afterPS" = "COD - BioInd (mg/l)" ) %>% 
  select(Date_rawdata, pH_BeforePS) %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata), 
         Plant = "Esbjerg West", 
         pH_BeforePS = as.numeric(pH_BeforePS))

## Merge pH to the rest
d_flow_EsW <- left_join(d_flow_EsW, d_pH_EsW) 


tidy_meta_EsW <- d_flow_EsW %>% 
  select(Date_rawdata, COD_beforePS, COD_afterPS, Plant) %>% 
  rename("Before" = "COD_beforePS", 
         "After" = "COD_afterPS") %>% 
  pivot_longer(cols = c(Before, After), names_to = "PrimarySettler", values_to = "COD") %>% 
  filter(!is.na(COD)) %>% 
  mutate(week_year = as.Date(Date_rawdata) %>% format("%W-%Y")) %>% 
  group_by(PrimarySettler, week_year, Plant) %>%
  summarise(mean_COD_mg_l = mean(COD), .groups = "drop") %>% 
  mutate(PrimarySettler = factor(PrimarySettler, levels = c("Before", "After"))) %>% 
  full_join(., select(d_flow_EsW, Date_rawdata, Flow_beforePS_m3, Flow_afterPS_m3, week_year, pH_BeforePS)) %>% 
  full_join(., filter(tidy_meta, Plant == "Esbjerg West"))%>% filter(!is.na(SampleID)) %>% 
  mutate(LocationBeforeSettler = factor(LocationBeforeSettler))



##############################
#     Ejby Mølle              #
##############################

d_Ejby_2019 <- read_xlsx(
  path = paste0(DataPath, "WWTP_metadata","/MIDAS data Ejby Mølle 2019.xlsx"), 
  sheet = "Dag") %>% 
  select("Date_rawdata" = "Date", 
         "Flow_beforePS_m3" = "Inflent_to_plant (m3/day)",
         "Flow_afterPS_m3" = "Influent_to_Biology_Daily",
         "COD_beforePS" = "CODtot_Influent_to_plant", 
         "COD_afterPS" = "CODtot_to_Biology_Daily",
         "COD_effluent" = "CODtot_Effluent",
         "pH_BeforePS" = "pH_Influent_to_plant", 
         "Iron_dosage_beforePS" = "Iron_dosage_Influent_to_plant") %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata), 
         Plant = "Ejby Mølle")

d_Ejby_2020 <- read_xlsx(
  path = paste0(DataPath, "WWTP_metadata","/MIDAS data Ejby Mølle 2020.xlsx"), 
  sheet = "Dag")%>% 
  select("Date_rawdata" = "Date", 
         "Flow_beforePS_m3" = "Inflent_to_plant",
         "Flow_afterPS_m3" = "Influent_to_Biology_Daily",
         "COD_beforePS" = "CODtot_Influent_to_plant", 
         "COD_afterPS" = "CODtot_to_Biology_Daily",
         "COD_effluent" = "CODtot_Effluent",
         "pH_BeforePS" = "pH_Influent_to_plant", 
         "Iron_dosage_beforePS" = "Iron_dosage_Influent_to_plant") %>% 
  mutate(Date_rawdata = as.Date(Date_rawdata), 
         Plant = "Ejby Mølle")

d_Ejby_2019 <- rbind(d_Ejby_2019, d_Ejby_2020)


######################
# Making the dataframes 
#####################

d_meta_by_date <- full_join(d_flow_EsW, d_flow_Ran) %>% full_join(., d_flow_AAW) %>%
  full_join(., d_Ejby_2019) 

rm(d_flow_EsW, d_flow_Ran, d_flow_AAW, tidy_meta_EsW, tidy_meta_Ran, 
   tidy_meta_AAW, d_Ejby_2019, d_Ejby_2020, d_pH_EsW)


##########################################################
#       Precipitation from DMI                           #
##########################################################

##Randers
DMI_randers <- list.files(path = paste0(DataPath, "DMI_data"), 
                          pattern = "randers", full.names = T
                          )
DMI_randers_df <- vroom(DMI_randers, delim = ";", col_names = F, skip = 1, col_types = "cc")
DMI_randers_df <- DMI_randers_df %>% 
  rename(DateTime = X1, Rainfall = X2) %>%  
  mutate(Rainfall = as.numeric(sub(",", ".", Rainfall, fixed = TRUE)), 
         DateTime = as.Date(DateTime, format ="%Y-%m-%d"), 
         Plant = "Randers"
  )

##Aalborg
DMI_aalborg <- list.files(path = paste0(DataPath, "DMI_data"),  
                          pattern = ".*aalborg*", full.names = T)
DMI_aalborg_df <- vroom(DMI_aalborg, delim = ";", col_names = F, skip = 1, col_types = "cc")
DMI_aalborg_df <- rename(DMI_aalborg_df, DateTime = X1, Rainfall = X2) 
DMI_aalborg_df <- DMI_aalborg_df %>% 
  mutate(Rainfall = as.numeric(sub(",", ".", Rainfall, fixed = TRUE)),
         DateTime = as.Date(DateTime, format ="%Y-%m-%d"), 
         Plant = "Aalborg West")

##esbjerg
DMI_esbjerg <- list.files(path = paste0(DataPath, "DMI_data"), 
                          pattern = ".*esbjerg*", full.names = T)

DMI_esbjerg_df <- vroom(DMI_esbjerg, delim = ";", col_names = F, skip = 1, col_types = "cc")
DMI_esbjerg_df <- rename(DMI_esbjerg_df, DateTime = X1, Rainfall = X2) 
DMI_esbjerg_df <- DMI_esbjerg_df %>% 
  mutate(Rainfall = as.numeric(sub(",", ".", Rainfall, fixed = TRUE)), 
         DateTime = as.Date(DateTime, format ="%Y-%m-%d"), 
         Plant = "Esbjerg West")


##ejbyMolle
DMI_ejbymolle <- list.files(path = paste0(DataPath, "DMI_data"),
                            pattern = ".*odense", full.names = T)
DMI_ejbymolle_df <- vroom(DMI_ejbymolle, delim = ";", col_names = F, skip = 1, col_types = "cc")
DMI_ejbymolle_df <- rename(DMI_ejbymolle_df, DateTime = X1, Rainfall = X2) 
DMI_ejbymolle_df <- DMI_ejbymolle_df %>% 
  mutate(Rainfall = as.numeric(sub(",", ".", Rainfall, fixed = TRUE)), 
         DateTime = as.Date(DateTime, format ="%Y-%m-%d"), 
         Plant = "Ejby Mølle")


DMI <- rbind(DMI_randers_df, DMI_aalborg_df, DMI_esbjerg_df, DMI_ejbymolle_df) %>% 
  rename("Date_rawdata" ="DateTime") %>% 
  filter(Date_rawdata > "2016-01-01") %>% 
  rename("DMI_rain_mm" = "Rainfall")


rm(DMI_esbjerg, DMI_esbjerg_df, DMI_randers, 
   DMI_aalborg, DMI_randers_df, DMI_aalborg_df, DMI_ejbymolle_df, DMI_ejbymolle)

# Merging with DMI data 
d_meta_by_date <- 
  d_meta_by_date %>% full_join(., DMI) %>% 
  filter(Date_rawdata > "2019-03-01" & Date_rawdata < "2020-05-01") %>% 
  distinct(.keep_all = T)


##Add temperature data on mothly avarage 
mean_temp_DK <- list.files(path = paste0(DataPath, "DMI_data"),  
                           pattern = "hele-landet", full.names = T
)
mean_temp_DK <- vroom(mean_temp_DK, delim = ";", col_names = F, skip = 1, col_types = "cccc")
mean_temp_DK <- mean_temp_DK %>% 
  rename(DateTime = X1, Temp_cel = X4) %>%
  select(DateTime, Temp_cel) %>% 
  mutate(Temp_cel = as.numeric(sub(",", ".", Temp_cel, fixed = TRUE)), 
         DateTime = as.Date(DateTime, format ="%Y-%m-%d"), 
         DateTime = format(DateTime,'%Y-%m')
  )
d_meta_by_date <- d_meta_by_date %>% 
  mutate(DateTime = format(Date_rawdata,'%Y-%m')) %>% 
  left_join(., mean_temp_DK) %>% 
  select(-DateTime)


rm(DMI, mean_temp_DK, rain_evets, DMI_randers)


print("Metadata loaded")





