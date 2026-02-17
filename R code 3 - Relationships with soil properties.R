###################################################
##### Guo et al., 2026 - Phragmites expansion #####
###################################################

### R code 3 - Relationships with soil properties ###

# Soil data cleaning
setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Soil_Properties")
data1 <- read.csv("CRMS_Soil_Properties.csv")

colnames(data1)[colnames(data1) == "Station.ID"] <- "Station_ID"
colnames(data1)[colnames(data1) == "Sample.Date..mm.dd.yyyy."] <- "Collection_year"
colnames(data1)[colnames(data1) == "Sample.Depth..cm."] <- "Sample_depth"
colnames(data1)[colnames(data1) == "Wet.Soil.pH..pH.units."] <- "Wet_soil_pH"
colnames(data1)[colnames(data1) == "Dry.Soil.pH..pH.units."] <- "Dry_soil_pH"
colnames(data1)[colnames(data1) == "Soil.Specific.Conductance..uS.cm."] <- "Soil_specific_conductance"
colnames(data1)[colnames(data1) == "Soil.Salinity..ppt."] <- "Soil_salinity"
colnames(data1)[colnames(data1) == "Soil.Moisture.Content...."] <- "Soil_Moisture_Content"
colnames(data1)[colnames(data1) == "Bulk.Density..g.cm3."] <- "Bulk_density"
colnames(data1)[colnames(data1) == "Organic.Matter...."] <- "Organic_matter"
colnames(data1)[colnames(data1) == "Wet.Volume..cm3."] <- "Wet_volume"
colnames(data1)[colnames(data1) == "Dry.Volume..cm3."] <- "Dry_volume"
colnames(data1)[colnames(data1) == "Belowground.Live.Biomass..g.m2."] <- "Belowground_live_biomass"
colnames(data1)[colnames(data1) == "Belowground.Dead.Biomass..g.m2."] <- "Belowground_dead_biomass"
colnames(data1)[colnames(data1) == "Organic.Density..g.cm3."] <- "Organic_density"
colnames(data1)[colnames(data1) == "Total.Carbon..g.kg."] <- "Total_carbon"
colnames(data1)[colnames(data1) == "Carbon.Density..mg.cm3."] <- "Carbon_density"
colnames(data1)[colnames(data1) == "Total.Nitrogen..g.kg."] <- "Total_nitrogen"
colnames(data1)[colnames(data1) == "Total.Phosphorus..mg.kg."] <- "Total_phosphorus"
colnames(data1)[colnames(data1) == "Sand...."] <- "Sand"
colnames(data1)[colnames(data1) == "Silt...."] <- "Silt"
colnames(data1)[colnames(data1) == "Clay...."] <- "Clay"
colnames(data1)[colnames(data1) == "Particle.Size.Mean..phi."] <- "Particle_size_mean"
colnames(data1)[colnames(data1) == "Particle.Size.Median..phi."] <- "Particle_size_median"
data_soil <- data1[, c("Station_ID", 
                       "Collection_year", 
                       "Sample_depth",
                       "Wet_soil_pH",
                       "Dry_soil_pH",
                       "Soil_specific_conductance",
                       "Soil_salinity",
                       "Soil_Moisture_Content",
                       "Bulk_density",
                       "Organic_matter",
                       "Wet_volume",
                       "Dry_volume",
                       "Belowground_live_biomass",
                       "Belowground_dead_biomass",
                       "Organic_density",
                       "Total_carbon",
                       "Carbon_density",
                       "Total_nitrogen",
                       "Total_phosphorus",
                       "Sand",
                       "Silt",
                       "Clay",
                       "Particle_size_mean",
                       "Particle_size_median")]

data_soil$Site_ID <- sapply(strsplit(data_soil$Station_ID, "-"), function(x) x[1])
data_soil$Collection_year <- sapply(strsplit(data_soil$Collection_year, "/"), function(x) paste(x[3:length(x)], collapse = "/"))

data_soil <- data_soil %>% 
  mutate(
    Basin = case_when(
      Site_ID %in% calcasieu_sabine_sites          ~ "Calcasieu-Sabine",
      Site_ID %in% mermentau_basin_sites           ~ "Mermentau",
      Site_ID %in% teche_vermilion_basin_sites     ~ "Teche-Vermilion",
      Site_ID %in% terrebonne_basin_sites          ~ "Terrebonne",
      Site_ID %in% atchafalaya_basin_sites         ~ "Atchafalaya",
      Site_ID %in% barataria_basin_sites           ~ "Barataria",
      Site_ID %in% breton_sound_basin_sites        ~ "Breton Sound",
      Site_ID %in% mississippi_river_delta_basin_sites ~ "Mississippi River Delta",
      Site_ID %in% pontchartrain_basin_sites       ~ "Pontchartrain",
      TRUE                                         ~ NA_character_
    )
  )
data_summary <- data_soil %>%
  dplyr::select(
    Wet_soil_pH, 
    Dry_soil_pH, 
    Soil_specific_conductance, 
    Soil_salinity,
    Soil_Moisture_Content, 
    Bulk_density, 
    Organic_matter, 
    Wet_volume,
    Dry_volume, 
    Belowground_live_biomass, 
    Belowground_dead_biomass,
    Organic_density, 
    Total_carbon, 
    Carbon_density, 
    Total_nitrogen,
    Total_phosphorus, 
    Sand, 
    Silt, 
    Clay,
    Particle_size_mean, 
    Particle_size_median
  ) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ min(.x, na.rm = TRUE),
      max  = ~ max(.x, na.rm = TRUE),
      mean = ~ mean(.x, na.rm = TRUE),
      sd   = ~ sd(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary
data_phragmites_zr_site <- data_phragmites_zr %>%
  group_by(Site_ID) %>%
  mutate(zr.phragmites_site = mean(zr.phragmites, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# Wet_soil_pH
data_soil_Wet_soil_pH <- data_soil[, c("Site_ID",
                                       "Station_ID", 
                                       "Collection_year", 
                                       "Sample_depth",
                                       "Wet_soil_pH",
                                       "Basin")]
data_soil_Wet_soil_pH <- data_soil_Wet_soil_pH %>% filter(!is.na(Wet_soil_pH))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Wet_soil_pH <- data_soil_Wet_soil_pH %>% filter(Sample_depth %in% depth_levels)
data_soil_Wet_soil_pH_station <- data_soil_Wet_soil_pH %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Wet_soil_pH_station = mean(Wet_soil_pH, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Wet_soil_pH_site <- data_soil_Wet_soil_pH_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Wet_soil_pH_site = mean(Wet_soil_pH_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_soil_Wet_soil_pH_site$Collection_year <- as.numeric(data_soil_Wet_soil_pH_site$Collection_year)
model <- lm(Wet_soil_pH_site ~ Collection_year, data = data_soil_Wet_soil_pH_site)
summary(model)
data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Wet_soil_pH_site, by = c("Site_ID"))
data_merged_model$Collection_year.y <- as.numeric(data_merged_model$Collection_year.y)
model <- lm(Wet_soil_pH_site ~ Collection_year.x, data = data_merged_model)
summary(model)

data_summary <- data_soil_Wet_soil_pH_site %>%
  dplyr::select(Wet_soil_pH_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Wet_soil_pH_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Wet_soil_pH_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Wet_soil_pH_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Wet_soil_pH_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Wet_soil_pH_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Wet_soil_pH_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Wet_soil_pH_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Wet_soil_pH_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Wet_soil_pH_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary



# Dry_soil_pH
data_soil_Dry_soil_pH <- data_soil[, c("Site_ID",
                                       "Station_ID", 
                                       "Collection_year", 
                                       "Sample_depth",
                                       "Dry_soil_pH",
                                       "Basin")]
data_soil_Dry_soil_pH <- data_soil_Dry_soil_pH %>% filter(Dry_soil_pH != 0 & !is.na(Dry_soil_pH))
data_soil_Dry_soil_pH_station <- data_soil_Dry_soil_pH %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Dry_soil_pH_station = mean(Dry_soil_pH, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Dry_soil_pH_site <- data_soil_Dry_soil_pH_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Dry_soil_pH_site = mean(Dry_soil_pH_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_soil_Dry_soil_pH_site$Collection_year <- as.numeric(data_soil_Dry_soil_pH_site$Collection_year)
model <- lm(Dry_soil_pH_site ~ Collection_year, data = data_soil_Dry_soil_pH_site)
summary(model)
data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Dry_soil_pH_site, by = c("Site_ID"))
data_merged_model$Collection_year.y <- as.numeric(data_merged_model$Collection_year.y)
model <- lm(Dry_soil_pH_site ~ Collection_year.x, data = data_merged_model)
summary(model)

data_summary <- data_soil_Dry_soil_pH_site  %>%
  dplyr::select(Dry_soil_pH_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Dry_soil_pH_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Dry_soil_pH_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Dry_soil_pH_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Dry_soil_pH_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Dry_soil_pH_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Dry_soil_pH_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Dry_soil_pH_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Dry_soil_pH_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Dry_soil_pH_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Dry_soil_pH_site$Collection_year <- as.character(data_soil_Dry_soil_pH_site$Collection_year)
data_soil_Dry_soil_pH_site <- filter(data_soil_Dry_soil_pH_site, Dry_soil_pH_site != 0)



# Soil_specific_conductance
data_soil_Soil_specific_conductance <- data_soil[, c("Site_ID",
                                                     "Station_ID", 
                                                     "Collection_year", 
                                                     "Sample_depth",
                                                     "Soil_specific_conductance",
                                                     "Basin")]
data_soil_Soil_specific_conductance <- data_soil_Soil_specific_conductance %>% filter(Soil_specific_conductance != 0 & !is.na(Soil_specific_conductance))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Soil_specific_conductance <- data_soil_Soil_specific_conductance %>% filter(Sample_depth %in% depth_levels)
data_soil_Soil_specific_conductance_station <- data_soil_Soil_specific_conductance %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Soil_specific_conductance_station = mean(Soil_specific_conductance, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Soil_specific_conductance_site <- data_soil_Soil_specific_conductance_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Soil_specific_conductance_site = mean(Soil_specific_conductance_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Soil_specific_conductance_site  %>%
  dplyr::select(Soil_specific_conductance_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Soil_specific_conductance_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Soil_specific_conductance_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Soil_specific_conductance_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Soil_specific_conductance_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Soil_specific_conductance_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Soil_specific_conductance_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Soil_specific_conductance_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Soil_specific_conductance_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Soil_specific_conductance_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Soil_specific_conductance_site$Collection_year <- as.character(data_soil_Soil_specific_conductance_site$Collection_year)
data_soil_Soil_specific_conductance_site <- filter(data_soil_Soil_specific_conductance_site, Soil_specific_conductance_site != 0)



# Soil_salinity
data_soil_Soil_salinity <- data_soil[, c("Site_ID",
                                         "Station_ID", 
                                         "Collection_year", 
                                         "Sample_depth",
                                         "Soil_salinity",
                                         "Basin")]
data_soil_Soil_salinity <- data_soil_Soil_salinity %>% filter(Soil_salinity != 0 & !is.na(Soil_salinity))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Soil_salinity <- data_soil_Soil_salinity %>% filter(Sample_depth %in% depth_levels)
data_soil_Soil_salinity_station <- data_soil_Soil_salinity %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Soil_salinity_station = mean(Soil_salinity, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Soil_salinity_site <- data_soil_Soil_salinity_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Soil_salinity_site = mean(Soil_salinity_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Soil_salinity_site  %>%
  dplyr::select(Soil_salinity_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Soil_salinity_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Soil_salinity_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Soil_salinity_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Soil_salinity_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Soil_salinity_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Soil_salinity_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Soil_salinity_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Soil_salinity_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Soil_salinity_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Soil_salinity_site$Collection_year <- as.character(data_soil_Soil_salinity_site$Collection_year)
data_soil_Soil_salinity_site <- filter(data_soil_Soil_salinity_site, Soil_salinity_site != 0)



# Soil_Moisture_Content
data_soil_Soil_Moisture_Content <- data_soil[, c("Site_ID",
                                                 "Station_ID", 
                                                 "Collection_year", 
                                                 "Sample_depth",
                                                 "Soil_Moisture_Content",
                                                 "Basin")]
data_soil_Soil_Moisture_Content <- data_soil_Soil_Moisture_Content %>% filter(Soil_Moisture_Content != 0 & !is.na(Soil_Moisture_Content))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Soil_Moisture_Content <- data_soil_Soil_Moisture_Content %>% filter(Sample_depth %in% depth_levels)
data_soil_Soil_Moisture_Content_station <- data_soil_Soil_Moisture_Content %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Soil_Moisture_Content_station = mean(Soil_Moisture_Content, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Soil_Moisture_Content_site <- data_soil_Soil_Moisture_Content_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Soil_Moisture_Content_site = mean(Soil_Moisture_Content_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Soil_Moisture_Content_site %>%
  dplyr::select(Soil_Moisture_Content_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary
soil_tagged <- data_soil_Soil_Moisture_Content_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Soil_Moisture_Content_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Soil_Moisture_Content_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Soil_Moisture_Content_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Soil_Moisture_Content_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Soil_Moisture_Content_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Soil_Moisture_Content_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Soil_Moisture_Content_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Soil_Moisture_Content_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Soil_Moisture_Content_site$Collection_year <- as.character(data_soil_Soil_Moisture_Content_site$Collection_year)
data_soil_Soil_Moisture_Content_site <- filter(data_soil_Soil_Moisture_Content_site, Soil_Moisture_Content_site != 0)



# Bulk_density
data_soil_Bulk_density <- data_soil[, c("Site_ID",
                                        "Station_ID", 
                                        "Collection_year", 
                                        "Sample_depth",
                                        "Bulk_density",
                                        "Basin")]
data_soil_Bulk_density <- data_soil_Bulk_density %>% filter(Bulk_density != 0 & !is.na(Bulk_density))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Bulk_density <- data_soil_Bulk_density %>% filter(Sample_depth %in% depth_levels)
data_soil_Bulk_density_station <- data_soil_Bulk_density %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Bulk_density_station = mean(Bulk_density, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Bulk_density_site <- data_soil_Bulk_density_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Bulk_density_site = mean(Bulk_density_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Bulk_density_site %>%
  dplyr::select(Bulk_density_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Bulk_density_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Bulk_density_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Bulk_density_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Bulk_density_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Bulk_density_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Bulk_density_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Bulk_density_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Bulk_density_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Bulk_density_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Bulk_density_site$Collection_year <- as.character(data_soil_Bulk_density_site$Collection_year)
data_soil_Bulk_density_site <- filter(data_soil_Bulk_density_site, Bulk_density_site != 0)



# Organic_matter
data_soil_Organic_matter <- data_soil[, c("Site_ID",
                                          "Station_ID", 
                                          "Collection_year", 
                                          "Sample_depth",
                                          "Organic_matter",
                                          "Basin")]
data_soil_Organic_matter <- data_soil_Organic_matter %>% filter(Organic_matter != 0 & !is.na(Organic_matter))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Organic_matter <- data_soil_Organic_matter %>% filter(Sample_depth %in% depth_levels)
data_soil_Organic_matter_station <- data_soil_Organic_matter %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Organic_matter_station = mean(Organic_matter, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Organic_matter_site <- data_soil_Organic_matter_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Organic_matter_site = mean(Organic_matter_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Organic_matter_site %>%
  dplyr::select(Organic_matter_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Organic_matter_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Organic_matter_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Organic_matter_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Organic_matter_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Organic_matter_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Organic_matter_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Organic_matter_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Organic_matter_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Organic_matter_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Organic_matter_site$Collection_year <- as.character(data_soil_Organic_matter_site$Collection_year)
data_soil_Organic_matter_site <- filter(data_soil_Organic_matter_site, Organic_matter_site != 0)



# Wet_volume
data_soil_Wet_volume <- data_soil[, c("Site_ID",
                                      "Station_ID", 
                                      "Collection_year", 
                                      "Sample_depth",
                                      "Wet_volume",
                                      "Basin")]

data_soil_Wet_volume <- data_soil_Wet_volume %>% filter(Wet_volume != 0 & !is.na(Wet_volume))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Wet_volume <- data_soil_Wet_volume %>% filter(Sample_depth %in% depth_levels)
data_soil_Wet_volume_station <- data_soil_Wet_volume %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Wet_volume_station = mean(Wet_volume, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Wet_volume_site <- data_soil_Wet_volume_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Wet_volume_site = mean(Wet_volume_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Wet_volume_site %>%
  dplyr::select(Wet_volume_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Wet_volume_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Wet_volume_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Wet_volume_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Wet_volume_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Wet_volume_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Wet_volume_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Wet_volume_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Wet_volume_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Wet_volume_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Wet_volume_site$Collection_year <- as.character(data_soil_Wet_volume_site$Collection_year)
data_soil_Wet_volume_site <- filter(data_soil_Wet_volume_site, Wet_volume_site != 0)



# Dry_volume
data_soil_Dry_volume <- data_soil[, c("Site_ID",
                                      "Station_ID", 
                                      "Collection_year", 
                                      "Sample_depth",
                                      "Dry_volume",
                                      "Basin")]
data_soil_Dry_volume <- data_soil_Dry_volume %>% filter(Dry_volume != 0 & !is.na(Dry_volume))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Dry_volume <- data_soil_Dry_volume %>% filter(Sample_depth %in% depth_levels)
data_soil_Dry_volume_station <- data_soil_Dry_volume %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Dry_volume_station = mean(Dry_volume, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Dry_volume_site <- data_soil_Dry_volume_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Dry_volume_site = mean(Dry_volume_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Dry_volume_site %>%
  dplyr::select(Dry_volume_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Dry_volume_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Dry_volume_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Dry_volume_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Dry_volume_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Dry_volume_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Dry_volume_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Dry_volume_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Dry_volume_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Dry_volume_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Dry_volume_site$Collection_year <- as.character(data_soil_Dry_volume_site$Collection_year)
data_soil_Dry_volume_site <- filter(data_soil_Dry_volume_site, Dry_volume_site != 0)



# Organic_density
data_soil_Organic_density <- data_soil[, c("Site_ID",
                                           "Station_ID", 
                                           "Collection_year", 
                                           "Sample_depth",
                                           "Organic_density",
                                           "Basin")]
data_soil_Organic_density <- data_soil_Organic_density %>% filter(Organic_density != 0 & !is.na(Organic_density))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Organic_density <- data_soil_Organic_density %>% filter(Sample_depth %in% depth_levels)
data_soil_Organic_density_station <- data_soil_Organic_density %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Organic_density_station = mean(Organic_density, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Organic_density_site <- data_soil_Organic_density_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Organic_density_site = mean(Organic_density_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_summary <- data_soil_Organic_density_site %>%
  dplyr::select(Organic_density_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Organic_density_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Organic_density_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Organic_density_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Organic_density_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Organic_density_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Organic_density_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Organic_density_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Organic_density_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Organic_density_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Organic_density_site$Collection_year <- as.character(data_soil_Organic_density_site$Collection_year)
data_soil_Organic_density_site <- filter(data_soil_Organic_density_site, Organic_density_site != 0)



# Total_carbon
data_soil_Total_carbon <- data_soil[, c("Site_ID",
                                        "Station_ID", 
                                        "Collection_year", 
                                        "Sample_depth",
                                        "Total_carbon",
                                        "Basin")]
data_soil_Total_carbon <- data_soil_Total_carbon %>% filter(!is.na(Total_carbon))
data_soil_Total_carbon %>% distinct(Sample_depth)
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Total_carbon <- data_soil_Total_carbon %>% filter(Sample_depth %in% depth_levels)
data_soil_Total_carbon %>% distinct(Sample_depth)
data_soil_Total_carbon_station <- data_soil_Total_carbon %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Total_carbon_station = mean(Total_carbon, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Total_carbon_site <- data_soil_Total_carbon_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Total_carbon_site = mean(Total_carbon_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_soil_Total_carbon_site$Collection_year <- as.numeric(data_soil_Total_carbon_site$Collection_year)
model <- lm(Total_carbon_site ~ Collection_year, data = data_soil_Total_carbon_site)
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Total_carbon_site, by = c("Site_ID"))
data_merged_model$Collection_year.y <- as.numeric(data_merged_model$Collection_year.y)
model <- lm(Total_carbon_site ~ Collection_year.y, data = data_merged_model)
summary(model)

data_summary <- data_soil_Total_carbon_site %>%
  dplyr::select(Total_carbon_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Total_carbon_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Total_carbon_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Total_carbon_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Total_carbon_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Total_carbon_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Total_carbon_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Total_carbon_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Total_carbon_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Total_carbon_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Total_carbon_site$Collection_year <- as.character(data_soil_Total_carbon_site$Collection_year)
data_soil_Total_carbon_site <- filter(data_soil_Total_carbon_site, Total_carbon_site != 0)



# Carbon_density
data_soil_Carbon_density <- data_soil[, c("Site_ID",
                                          "Station_ID", 
                                          "Collection_year", 
                                          "Sample_depth",
                                          "Carbon_density",
                                          "Basin")]

data_soil_Carbon_density <- data_soil_Carbon_density %>% filter(Carbon_density != 0 & !is.na(Carbon_density))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Carbon_density <- data_soil_Carbon_density %>% filter(Sample_depth %in% depth_levels)
data_soil_Carbon_density_station <- data_soil_Carbon_density %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Carbon_density_station = mean(Carbon_density, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Carbon_density_site <- data_soil_Carbon_density_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Carbon_density_site = mean(Carbon_density_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Carbon_density_site %>%
  dplyr::select(Carbon_density_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Carbon_density_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Carbon_density_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Carbon_density_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Carbon_density_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Carbon_density_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Carbon_density_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Carbon_density_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Carbon_density_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Carbon_density_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Carbon_density_site$Collection_year <- as.character(data_soil_Carbon_density_site$Collection_year)
data_soil_Carbon_density_site <- filter(data_soil_Carbon_density_site, Carbon_density_site != 0)



# Total_nitrogen
data_soil_Total_nitrogen <- data_soil[, c("Site_ID",
                                          "Station_ID", 
                                          "Collection_year", 
                                          "Sample_depth",
                                          "Total_nitrogen",
                                          "Basin")]
data_soil_Total_nitrogen <- data_soil_Total_nitrogen %>% filter(Total_nitrogen != 0 & !is.na(Total_nitrogen))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Total_nitrogen <- data_soil_Total_nitrogen %>% filter(Sample_depth %in% depth_levels)
data_soil_Total_nitrogen_station <- data_soil_Total_nitrogen %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Total_nitrogen_station = mean(Total_nitrogen, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Total_nitrogen_site <- data_soil_Total_nitrogen_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Total_nitrogen_site = mean(Total_nitrogen_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Total_nitrogen_site %>%
  dplyr::select(Total_nitrogen_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Total_nitrogen_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Total_nitrogen_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Total_nitrogen_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Total_nitrogen_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Total_nitrogen_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Total_nitrogen_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Total_nitrogen_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Total_nitrogen_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Total_nitrogen_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Total_nitrogen_site$Collection_year <- as.character(data_soil_Total_nitrogen_site$Collection_year)
data_soil_Total_nitrogen_site <- filter(data_soil_Total_nitrogen_site, Total_nitrogen_site != 0)



# Total_phosphorus
data_soil_Total_phosphorus <- data_soil[, c("Site_ID",
                                            "Station_ID", 
                                            "Collection_year", 
                                            "Sample_depth",
                                            "Total_phosphorus",
                                            "Basin")]
data_soil_Total_phosphorus <- data_soil_Total_phosphorus %>% filter(Total_phosphorus != 0 & !is.na(Total_phosphorus))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Total_phosphorus <- data_soil_Total_phosphorus %>% filter(Sample_depth %in% depth_levels)
data_soil_Total_phosphorus_station <- data_soil_Total_phosphorus %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Total_phosphorus_station = mean(Total_phosphorus, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Total_phosphorus_site <- data_soil_Total_phosphorus_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Total_phosphorus_site = mean(Total_phosphorus_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Total_phosphorus_site %>%
  dplyr::select(Total_phosphorus_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Total_phosphorus_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Total_phosphorus_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Total_phosphorus_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Total_phosphorus_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Total_phosphorus_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Total_phosphorus_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Total_phosphorus_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Total_phosphorus_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Total_phosphorus_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Total_phosphorus_site$Collection_year <- as.character(data_soil_Total_phosphorus_site$Collection_year)
data_soil_Total_phosphorus_site <- filter(data_soil_Total_phosphorus_site, Total_phosphorus_site != 0)



# Sand
data_soil_Sand <- data_soil[, c("Site_ID",
                                "Station_ID", 
                                "Collection_year", 
                                "Sample_depth",
                                "Sand",
                                "Basin")]
data_soil_Sand <- data_soil_Sand %>% filter(Sand != 0 & !is.na(Sand))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Sand <- data_soil_Sand %>% filter(Sample_depth %in% depth_levels)
data_soil_Sand_station <- data_soil_Sand %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Sand_station = mean(Sand, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Sand_site <- data_soil_Sand_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Sand_site = mean(Sand_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Sand_site %>%
  dplyr::select(Sand_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Sand_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Sand_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Sand_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Sand_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Sand_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Sand_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Sand_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Sand_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Sand_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Sand_site$Collection_year <- as.character(data_soil_Sand_site$Collection_year)
data_soil_Sand_site <- filter(data_soil_Sand_site, Sand_site != 0)



# Silt
data_soil_Silt <- data_soil[, c("Site_ID",
                                "Station_ID", 
                                "Collection_year", 
                                "Sample_depth",
                                "Silt",
                                "Basin")]
data_soil_Silt <- data_soil_Silt %>% filter(Silt != 0 & !is.na(Silt))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Silt <- data_soil_Silt %>% filter(Sample_depth %in% depth_levels)
data_soil_Silt_station <- data_soil_Silt %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Silt_station = mean(Silt, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Silt_site <- data_soil_Silt_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Silt_site = mean(Silt_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Silt_site %>%
  dplyr::select(Silt_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Silt_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Silt_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Silt_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Silt_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Silt_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Silt_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Silt_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Silt_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Silt_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Silt_site$Collection_year <- as.character(data_soil_Silt_site$Collection_year)
data_soil_Silt_site <- filter(data_soil_Silt_site, Silt_site != 0)



# Clay
data_soil_Clay <- data_soil[, c("Site_ID",
                                "Station_ID", 
                                "Collection_year", 
                                "Sample_depth",
                                "Clay",
                                "Basin")]
data_soil_Clay <- data_soil_Clay %>% filter(Clay != 0 & !is.na(Clay))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Clay <- data_soil_Clay %>% filter(Sample_depth %in% depth_levels)
data_soil_Clay_station <- data_soil_Clay %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Clay_station = mean(Clay, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Clay_site <- data_soil_Clay_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Clay_site = mean(Clay_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Clay_site %>%
  dplyr::select(Clay_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Clay_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Clay_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Clay_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Clay_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Clay_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Clay_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Clay_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Clay_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Clay_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Clay_site$Collection_year <- as.character(data_soil_Clay_site$Collection_year)
data_soil_Clay_site <- filter(data_soil_Clay_site, Clay_site != 0)



# Particle_size_mean
data_soil_Particle_size_mean <- data_soil[, c("Site_ID",
                                              "Station_ID", 
                                              "Collection_year", 
                                              "Sample_depth",
                                              "Particle_size_mean",
                                              "Basin")]
data_soil_Particle_size_mean <- data_soil_Particle_size_mean %>% filter(Particle_size_mean != 0 & !is.na(Particle_size_mean))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Particle_size_mean <- data_soil_Particle_size_mean %>% filter(Sample_depth %in% depth_levels)
data_soil_Particle_size_mean_station <- data_soil_Particle_size_mean %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Particle_size_mean_station = mean(Particle_size_mean, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Particle_size_mean_site <- data_soil_Particle_size_mean_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Particle_size_mean_site = mean(Particle_size_mean_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Particle_size_mean_site %>%
  dplyr::select(Particle_size_mean_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Particle_size_mean_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Particle_size_mean_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Particle_size_mean_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Particle_size_mean_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Particle_size_mean_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Particle_size_mean_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Particle_size_mean_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Particle_size_mean_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Particle_size_mean_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Particle_size_mean_site$Collection_year <- as.character(data_soil_Particle_size_mean_site$Collection_year)
data_soil_Particle_size_mean_site <- filter(data_soil_Particle_size_mean_site, Particle_size_mean_site != 0)



# Particle_size_median
data_soil_Particle_size_median <- data_soil[, c("Site_ID",
                                                "Station_ID", 
                                                "Collection_year", 
                                                "Sample_depth",
                                                "Particle_size_median",
                                                "Basin")]
data_soil_Particle_size_median <- data_soil_Particle_size_median %>% filter(Particle_size_median != 0 & !is.na(Particle_size_median))
depth_levels <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16 to 20", "20 to 24")
data_soil_Particle_size_median <- data_soil_Particle_size_median %>% filter(Sample_depth %in% depth_levels)
data_soil_Particle_size_median_station <- data_soil_Particle_size_median %>%
  group_by(Station_ID, Collection_year) %>%
  mutate(Particle_size_median_station = mean(Particle_size_median, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
data_soil_Particle_size_median_site <- data_soil_Particle_size_median_station %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Particle_size_median_site = mean(Particle_size_median_station, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_summary <- data_soil_Particle_size_median_site %>%
  dplyr::select(Particle_size_median_site) %>%
  summarise(across(
    .cols = everything(),
    .fns  = list(
      min  = ~ formatC(min(.x, na.rm = TRUE), format = "f", digits = 3),
      max  = ~ formatC(max(.x, na.rm = TRUE), format = "f", digits = 3),
      mean = ~ formatC(mean(.x, na.rm = TRUE), format = "f", digits = 3),
      sd   = ~ formatC(sd(.x, na.rm = TRUE), format = "f", digits = 3)
    ),
    .names = "{.col}_{.fn}"
  ))
data_summary

soil_tagged <- data_soil_Particle_size_median_site %>% 
  mutate(
    BasinStatus = case_when(
      Basin %in% c("Calcasieu-Sabine", "Mermentau", "Teche-Vermilion", "Terrebonne") ~ "Expanding",
      Basin %in% c("Atchafalaya", "Barataria", "Breton Sound")                       ~ "Stable",
      Basin ==  "Mississippi River Delta"                                            ~ "Declining",
      TRUE                                                                           ~ NA_character_
    )
  )
overall <- soil_tagged %>% 
  summarise(
    min  = formatC(min(Particle_size_median_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Particle_size_median_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Particle_size_median_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Particle_size_median_site,   na.rm = TRUE), format = "f", digits = 3)
  ) %>% 
  mutate(BasinStatus = "Overall")
by_status <- soil_tagged %>% 
  group_by(BasinStatus) %>% 
  summarise(
    min  = formatC(min(Particle_size_median_site,  na.rm = TRUE), format = "f", digits = 3),
    max  = formatC(max(Particle_size_median_site,  na.rm = TRUE), format = "f", digits = 3),
    mean = formatC(mean(Particle_size_median_site, na.rm = TRUE), format = "f", digits = 3),
    sd   = formatC(sd(Particle_size_median_site,   na.rm = TRUE), format = "f", digits = 3),
    .groups = "drop"
  )
data_summary <- bind_rows(overall, by_status)
data_summary
data_soil_Particle_size_median_site$Collection_year <- as.character(data_soil_Particle_size_median_site$Collection_year)
data_soil_Particle_size_median_site <- filter(data_soil_Particle_size_median_site, Particle_size_median_site != 0)



# correlation coefficient of soil properties
aggregate_soil_data <- function(df, site_col = "Site_ID") {
  df %>%
    group_by(.data[[site_col]]) %>%
    summarise(
      across(
        .cols = where(is.numeric),
        .fns  = ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}
wet_soil_agg <- aggregate_soil_data(data_soil_Wet_soil_pH_site)
dry_soil_agg <- aggregate_soil_data(data_soil_Dry_soil_pH_site)
sp_cond_agg  <- aggregate_soil_data(data_soil_Soil_specific_conductance_site)
salinity_agg <- aggregate_soil_data(data_soil_Soil_salinity_site)
moist_agg    <- aggregate_soil_data(data_soil_Soil_Moisture_Content_site)
bulk_dens_agg <- aggregate_soil_data(data_soil_Bulk_density_site)
org_matter_agg <- aggregate_soil_data(data_soil_Organic_matter_site)
wet_vol_agg  <- aggregate_soil_data(data_soil_Wet_volume_site)
dry_vol_agg  <- aggregate_soil_data(data_soil_Dry_volume_site)
org_dens_agg <- aggregate_soil_data(data_soil_Organic_density_site)
tot_c_agg    <- aggregate_soil_data(data_soil_Total_carbon_site)
c_dens_agg   <- aggregate_soil_data(data_soil_Carbon_density_site)
tot_n_agg    <- aggregate_soil_data(data_soil_Total_nitrogen_site)
tot_p_agg    <- aggregate_soil_data(data_soil_Total_phosphorus_site)
sand_agg     <- aggregate_soil_data(data_soil_Sand_site)
silt_agg     <- aggregate_soil_data(data_soil_Silt_site)
clay_agg     <- aggregate_soil_data(data_soil_Clay_site)
ps_mean_agg  <- aggregate_soil_data(data_soil_Particle_size_mean_site)
ps_med_agg   <- aggregate_soil_data(data_soil_Particle_size_median_site)

data_merged <- data_phragmites_zr_site %>%
  left_join(wet_soil_agg, by = "Site_ID") %>%
  left_join(dry_soil_agg, by = "Site_ID") %>%
  left_join(sp_cond_agg,  by = "Site_ID") %>%
  left_join(salinity_agg, by = "Site_ID") %>%
  left_join(moist_agg,    by = "Site_ID") %>%
  left_join(bulk_dens_agg, by = "Site_ID") %>%
  left_join(org_matter_agg, by = "Site_ID") %>%
  left_join(wet_vol_agg,  by = "Site_ID") %>%
  left_join(dry_vol_agg,  by = "Site_ID") %>%
  left_join(org_dens_agg, by = "Site_ID") %>%
  left_join(tot_c_agg,    by = "Site_ID") %>%
  left_join(c_dens_agg,   by = "Site_ID") %>%
  left_join(tot_n_agg,    by = "Site_ID") %>%
  left_join(tot_p_agg,    by = "Site_ID") %>%
  left_join(sand_agg,     by = "Site_ID") %>%
  left_join(silt_agg,     by = "Site_ID") %>%
  left_join(clay_agg,     by = "Site_ID") %>%
  left_join(ps_mean_agg,  by = "Site_ID") %>%
  left_join(ps_med_agg,   by = "Site_ID")

data_merged <- data_merged1[, c("Basin",
                                "Dry_soil_pH_site",
                                "Soil_salinity_site",
                                "Soil_Moisture_Content_site",
                                "Organic_matter_site",
                                "Organic_density_site",
                                "Total_carbon_site",
                                "Carbon_density_site",
                                "Total_nitrogen_site",
                                "Total_phosphorus_site",
                                "Sand_site",
                                "Silt_site",
                                "Clay_site",
                                "Particle_size_mean_site")]
data_merged$Basin <- as.factor(data_merged$Basin)

soil_factors_for_corr <- data_merged %>%
  select(
    Wet_soil_pH_site,
    Dry_soil_pH_site,
    Soil_specific_conductance_site,
    Soil_salinity_site,
    Soil_Moisture_Content_site,
    Bulk_density_site,
    Organic_matter_site,
    Wet_volume_site,
    Dry_volume_site,
    Organic_density_site,
    Total_carbon_site,
    Carbon_density_site,
    Total_nitrogen_site,
    Total_phosphorus_site,
    Sand_site,
    Silt_site,
    Clay_site,
    Particle_size_mean_site,
    Particle_size_median_site)
corr_matrix <- cor(soil_factors_for_corr, use = "complete.obs", method = "pearson")
corr_matrix
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)



# linear regression model between soil properties and Phrag expansion
data_phragmites_zr_site <- data_phragmites_zr %>%
  group_by(Site_ID) %>%
  mutate(zr.phragmites_site = mean(zr.phragmites, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Wet_soil_pH_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Wet_soil_pH_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Wet_soil_pH_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Wet_soil_pH_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Wet_soil_pH_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Dry_soil_pH_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Dry_soil_pH_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Dry_soil_pH_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Dry_soil_pH_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Dry_soil_pH_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Soil_specific_conductance_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Soil_specific_conductance_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Soil_specific_conductance_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Soil_specific_conductance_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Soil_specific_conductance_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Soil_salinity_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Soil_salinity_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Soil_salinity_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Soil_salinity_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Soil_salinity_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Soil_Moisture_Content_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Soil_Moisture_Content_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Soil_Moisture_Content_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Soil_Moisture_Content_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Soil_Moisture_Content_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Organic_matter_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Organic_matter_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Organic_matter_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Organic_matter_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Organic_matter_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Wet_volume_site,by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Wet_volume_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Wet_volume_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Wet_volume_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Wet_volume_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Organic_density_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Organic_density_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Organic_density_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Organic_density_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Organic_density_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Total_carbon_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Total_carbon_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Total_carbon_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Total_carbon_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Total_carbon_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Carbon_density_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Carbon_density_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Carbon_density_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Carbon_density_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Carbon_density_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Total_nitrogen_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Total_nitrogen_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Total_nitrogen_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Total_nitrogen_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Total_nitrogen_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Total_phosphorus_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Total_phosphorus_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Total_phosphorus_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Total_phosphorus_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Total_phosphorus_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Sand_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Sand_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Sand_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Sand_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Sand_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Silt_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Silt_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Silt_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Silt_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Silt_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Clay_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Clay_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Clay_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Clay_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Clay_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Particle_size_mean_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Particle_size_mean_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Particle_size_mean_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Particle_size_mean_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Particle_size_mean_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)

data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Particle_size_median_site, by = c("Site_ID"))
model <- lm(zr.phragmites_site ~ Particle_size_median_site,  data = data_merged_model)
summary(model)
model <- lm(zr.phragmites_site ~ Particle_size_median_site, data = subset(data_merged_model, Status == "Expanding"))
summary(model)
model <- lm(zr.phragmites_site ~ Particle_size_median_site, data = subset(data_merged_model, Status == "Stable"))
summary(model)
model <- lm(zr.phragmites_site ~ Particle_size_median_site, data = subset(data_merged_model, Status == "Declining"))
summary(model)



# Figure 5A - Dry_soil_pH
data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Dry_soil_pH_site, by = c("Site_ID"))
p <- ggplot(data_merged_model, aes(x = Dry_soil_pH_site, y = zr.phragmites_site)) +
  geom_point(aes(color = Status), size = 4, shape = 16, stroke = 0.25, alpha = 0.9) +
  geom_smooth(aes(color = "Total", fill = "Total", linetype = "Total"),
              method = "lm", se = TRUE, alpha = 0.15) +
  geom_smooth(aes(color = Status, fill = Status, linetype = Status),
              method = "lm", se = TRUE, alpha = 0.15) +
  scale_color_manual(values = c("Total"     = "#2E2E2E",
                                "Expanding" = "#f16c23",
                                "Stable"    = "#1b7c3d",
                                "Declining" = "#2b6a99")) +
  scale_fill_manual(values = c("Total"     = "#2E2E2E",
                               "Expanding" = "#f16c23",
                               "Stable"    = "#1b7c3d",
                               "Declining" = "#2b6a99"), guide = "none") +
  scale_linetype_manual(values = c("Total"     = "solid",
                                   "Expanding" = "dashed",
                                   "Stable"    = "dashed",
                                   "Declining" = "solid"), guide = "none") +
  guides(color = guide_legend(title = "Status")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x        = element_text(size = 12, hjust = 0.5),
        axis.title.x       = element_text(size = 12),
        legend.position    = "none",
        legend.justification = c(0, 1),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 12),
        axis.text          = element_text(size = 12)) +
  xlab("Dry Soil pH") +
  ylab("Phragmites zr") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-2, 2),
                     labels = scales::number_format(accuracy = 0.1))
p_marginal <- ggMarginal(p,
                         type        = "density",
                         margins     = "both",
                         groupColour = TRUE,
                         groupFill   = TRUE,
                         alpha       = 0.4)
print(p_marginal)
ggsave("Figure - Dry_soil_pH_site.pdf", p_marginal, width = 120, height = 90, units = "mm")



# Figure 5B - Soil_salinity
data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Soil_salinity_site, by = c("Site_ID"))
p <- ggplot(data_merged_model, aes(x = Soil_salinity, y = zr.phragmites_site)) +
  geom_point(aes(color = Status), size = 4, shape = 16, stroke = 0.25, alpha = 0.9) +
  geom_smooth(aes(color = "Total", fill = "Total", linetype = "Total"),
              method = "lm", se = TRUE, alpha = 0.15) +
  geom_smooth(aes(color = Status, fill = Status, linetype = Status),
              method = "lm", se = TRUE, alpha = 0.15) +
  scale_color_manual(values = c("Total"     = "#2E2E2E",
                                "Expanding" = "#f16c23",
                                "Stable"    = "#1b7c3d",
                                "Declining" = "#2b6a99")) +
  scale_fill_manual(values = c("Total"     = "#2E2E2E",
                               "Expanding" = "#f16c23",
                               "Stable"    = "#1b7c3d",
                               "Declining" = "#2b6a99"), guide = "none") +
  scale_linetype_manual(values = c("Total"     = "solid",
                                   "Expanding" = "dashed",
                                   "Stable"    = "dashed",
                                   "Declining" = "solid"), guide = "none") +
  guides(color = guide_legend(title = "Status")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x        = element_text(size = 12, hjust = 0.5),
        axis.title.x       = element_text(size = 12),
        legend.position    = "none",  # 不显示图例
        legend.justification = c(0, 1),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 12),
        axis.text          = element_text(size = 12)) +
  xlab("Soil Salinity (ppt)") +
  ylab("Phragmites zr") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-1.5, 1.5),
                     labels = scales::number_format(accuracy = 0.1))
p_marginal <- ggMarginal(p,
                         type        = "density",
                         margins     = "both",
                         groupColour = TRUE,
                         groupFill   = TRUE,
                         alpha       = 0.4)
print(p_marginal)
ggsave("Figure - Soil_salinity.pdf", p_marginal, width = 120, height = 90, units = "mm")



# Figure 5C - Soil_moisture_content
data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Soil_Moisture_Content_site, by = c("Site_ID"))
p <- ggplot(data_merged_model, aes(x = Soil_Moisture_Content, y = zr.phragmites_site)) +
  geom_point(aes(color = Status), size = 4, shape = 16, stroke = 0.25, alpha = 0.9) +
  geom_smooth(aes(color = "Total", fill = "Total", linetype = "Total"),
              method = "lm", se = TRUE, alpha = 0.15) +
  geom_smooth(aes(color = Status, fill = Status, linetype = Status),
              method = "lm", se = TRUE, alpha = 0.15) +
  scale_color_manual(values = c("Total"     = "#2E2E2E",
                                "Expanding" = "#f16c23",
                                "Stable"    = "#1b7c3d",
                                "Declining" = "#2b6a99")) +
  scale_fill_manual(values = c("Total"     = "#2E2E2E",
                               "Expanding" = "#f16c23",
                               "Stable"    = "#1b7c3d",
                               "Declining" = "#2b6a99"), guide = "none") +
  scale_linetype_manual(values = c("Total"     = "solid",
                                   "Expanding" = "dashed",
                                   "Stable"    = "dashed",
                                   "Declining" = "dashed"), guide = "none") +
  guides(color = guide_legend(title = "Status")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x        = element_text(size = 12, hjust = 0.5),
        axis.title.x       = element_text(size = 12),
        legend.position    = "none",
        legend.justification = c(0, 1),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 12),
        axis.text          = element_text(size = 12)) +
  xlab("Soil moisture content (%)") +
  ylab("Phragmites zr") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-2, 2.3),
                     labels = scales::number_format(accuracy = 0.1))
p_marginal <- ggMarginal(p,
                         type        = "density",
                         margins     = "both",
                         groupColour = TRUE,
                         groupFill   = TRUE,
                         alpha       = 0.4)
print(p_marginal)
ggsave("Figure - Soil_moisture_content.pdf", p_marginal, width = 120, height = 90, units = "mm")



# Figure 5D - Organic matter
data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Organic_matter_site, by = c("Site_ID"))
p <- ggplot(data_merged_model, aes(x = Organic_matter, y = zr.phragmites_site)) +
  geom_point(aes(color = Status), size = 4, shape = 16, stroke = 0.25, alpha = 0.9) +
  geom_smooth(aes(color = "Total", fill = "Total", linetype = "Total"),
              method = "lm", se = TRUE, alpha = 0.15) +
  geom_smooth(aes(color = Status, fill = Status, linetype = Status),
              method = "lm", se = TRUE, alpha = 0.15) +
  scale_color_manual(values = c("Total"     = "#2E2E2E",
                                "Expanding" = "#f16c23",
                                "Stable"    = "#1b7c3d",
                                "Declining" = "#2b6a99")) +
  scale_fill_manual(values = c("Total"     = "#2E2E2E",
                               "Expanding" = "#f16c23",
                               "Stable"    = "#1b7c3d",
                               "Declining" = "#2b6a99"), guide = "none") +
  scale_linetype_manual(values = c("Total"     = "solid",
                                   "Expanding" = "dashed",
                                   "Stable"    = "dashed",
                                   "Declining" = "dashed"), guide = "none") +
  guides(color = guide_legend(title = "Status")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x        = element_text(size = 12, hjust = 0.5),
        axis.title.x       = element_text(size = 12),
        legend.position    = "none",  # 不显示图例
        legend.justification = c(0, 1),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 12),
        axis.text          = element_text(size = 12)) +
  xlab("Organic matter (%)") +
  ylab("Phragmites zr") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-2, 2),
                     labels = scales::number_format(accuracy = 0.1))
p_marginal <- ggMarginal(p,
                         type        = "density",
                         margins     = "both",
                         groupColour = TRUE,
                         groupFill   = TRUE,
                         alpha       = 0.4)
print(p_marginal)
ggsave("Figure - Organic_matter.pdf", p_marginal, width = 120, height = 90, units = "mm")



# Figure 5E - Organic_density
data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Organic_density_site, by = c("Site_ID"))
p <- ggplot(data_merged_model, aes(x = Organic_density, y = zr.phragmites_site)) +
  geom_point(aes(color = Status), size = 4, shape = 16, stroke = 0.25, alpha = 0.9) +
  geom_smooth(aes(color = "Total", fill = "Total", linetype = "Total"),
              method = "lm", se = TRUE, alpha = 0.15) +
  geom_smooth(aes(color = Status, fill = Status, linetype = Status),
              method = "lm", se = TRUE, alpha = 0.15) +
  scale_color_manual(values = c("Total"     = "#2E2E2E",
                                "Expanding" = "#f16c23",
                                "Stable"    = "#1b7c3d",
                                "Declining" = "#2b6a99")) +
  scale_fill_manual(values = c("Total"     = "#2E2E2E",
                               "Expanding" = "#f16c23",
                               "Stable"    = "#1b7c3d",
                               "Declining" = "#2b6a99"), guide = "none") +
  scale_linetype_manual(values = c("Total"     = "solid",
                                   "Expanding" = "dashed",
                                   "Stable"    = "solid",
                                   "Declining" = "dashed"), guide = "none") +
  guides(color = guide_legend(title = "Status")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x        = element_text(size = 12, hjust = 0.5),
        axis.title.x       = element_text(size = 12),
        legend.position    = "none",
        legend.justification = c(0, 1),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 12),
        axis.text          = element_text(size = 12)) +
  xlab("Organic density (g/cm3)") +
  ylab("Phragmites zr") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-2, 2.5),
                     labels = scales::number_format(accuracy = 0.1))
p_marginal <- ggMarginal(p,
                         type        = "density",
                         margins     = "both",
                         groupColour = TRUE,
                         groupFill   = TRUE,
                         alpha       = 0.4)
print(p_marginal)
ggsave("Figure - Organic_density.pdf", p_marginal, width = 120, height = 90, units = "mm")



# Figure 5F - Total_carbon
data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Total_carbon_site, by = c("Site_ID"))
p <- ggplot(data_merged_model, aes(x = Total_carbon, y = zr.phragmites_site)) +
  geom_point(aes(color = Status), size = 4, shape = 16, stroke = 0.25, alpha = 0.9) +
  geom_smooth(aes(color = "Total", fill = "Total", linetype = "Total"),
              method = "lm", se = TRUE, alpha = 0.15) +
  geom_smooth(aes(color = Status, fill = Status, linetype = Status),
              method = "lm", se = TRUE, alpha = 0.15) +
  scale_color_manual(values = c("Total"     = "#2E2E2E",
                                "Expanding" = "#f16c23",
                                "Stable"    = "#1b7c3d",
                                "Declining" = "#2b6a99")) +
  scale_fill_manual(values = c("Total"     = "#2E2E2E",
                               "Expanding" = "#f16c23",
                               "Stable"    = "#1b7c3d",
                               "Declining" = "#2b6a99"), guide = "none") +
  scale_linetype_manual(values = c("Total"     = "solid",
                                   "Expanding" = "dashed",
                                   "Stable"    = "dashed",
                                   "Declining" = "dashed"), guide = "none") +
  guides(color = guide_legend(title = "Status")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x        = element_text(size = 12, hjust = 0.5),
        axis.title.x       = element_text(size = 12),
        legend.position    = "none",
        legend.justification = c(0, 1),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 12),
        axis.text          = element_text(size = 12)) +
  xlab("Total carbon (g/kg)") +
  ylab("Phragmites zr") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-2, 2),
                     labels = scales::number_format(accuracy = 0.1))
p_marginal <- ggMarginal(p,
                         type        = "density",
                         margins     = "both",
                         groupColour = TRUE,
                         groupFill   = TRUE,
                         alpha       = 0.4)
print(p_marginal)
ggsave("Figure - Total_carbon.pdf", p_marginal, width = 120, height = 90, units = "mm")



# Figure 5G - Carbon_density
data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Carbon_density_site, by = c("Site_ID"))
p <- ggplot(data_merged_model, aes(x = Carbon_density, y = zr.phragmites_site)) +
  geom_point(aes(color = Status), size = 4, shape = 16, stroke = 0.25, alpha = 0.9) +
  geom_smooth(aes(color = "Total", fill = "Total", linetype = "Total"),
              method = "lm", se = TRUE, alpha = 0.15) +
  geom_smooth(aes(color = Status, fill = Status, linetype = Status),
              method = "lm", se = TRUE, alpha = 0.15) +
  scale_color_manual(values = c("Total"     = "#2E2E2E",
                                "Expanding" = "#f16c23",
                                "Stable"    = "#1b7c3d",
                                "Declining" = "#2b6a99")) +
  scale_fill_manual(values = c("Total"     = "#2E2E2E",
                               "Expanding" = "#f16c23",
                               "Stable"    = "#1b7c3d",
                               "Declining" = "#2b6a99"), guide = "none") +
  scale_linetype_manual(values = c("Total"     = "solid",
                                   "Expanding" = "dashed",
                                   "Stable"    = "solid",
                                   "Declining" = "dashed"), guide = "none") +
  guides(color = guide_legend(title = "Status")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x        = element_text(size = 12, hjust = 0.5),
        axis.title.x       = element_text(size = 12),
        legend.position    = "none",
        legend.justification = c(0, 1),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 12),
        axis.text          = element_text(size = 12)) +
  xlab("Carbon density (mg/cm3)") +
  ylab("Phragmites zr") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-2, 2),
                     labels = scales::number_format(accuracy = 0.1))
p_marginal <- ggMarginal(p,
                         type        = "density",
                         margins     = "both",
                         groupColour = TRUE,
                         groupFill   = TRUE,
                         alpha       = 0.4)
print(p_marginal)
ggsave("Figure - Carbon_density.pdf", p_marginal, width = 120, height = 90, units = "mm")



# Figure 5H - Total_nitrogen
data_merged_model <- inner_join(data_phragmites_zr_site, data_soil_Total_nitrogen_site, by = c("Site_ID"))
p <- ggplot(data_merged_model, aes(x = Total_nitrogen, y = zr.phragmites_site)) +
  geom_point(aes(color = Status), size = 4, shape = 16, stroke = 0.25, alpha = 0.7) +
  geom_smooth(aes(color = "Total", fill = "Total", linetype = "Total"),
              method = "lm", se = TRUE, alpha = 0.15) +
  geom_smooth(aes(color = Status, fill = Status, linetype = Status),
              method = "lm", se = TRUE, alpha = 0.15) +
  scale_color_manual(values = c("Total"     = "#2E2E2E",
                                "Expanding" = "#f16c23",
                                "Stable"    = "#1b7c3d",
                                "Declining" = "#2b6a99")) +
  scale_fill_manual(values = c("Total"     = "#2E2E2E",
                               "Expanding" = "#f16c23",
                               "Stable"    = "#1b7c3d",
                               "Declining" = "#2b6a99"), guide = "none") +
  scale_linetype_manual(values = c("Total"     = "solid",
                                   "Expanding" = "dashed",
                                   "Stable"    = "dashed",
                                   "Declining" = "dashed"), guide = "none") +
  guides(color = guide_legend(title = "Status")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x        = element_text(size = 12, hjust = 0.5),
        axis.title.x       = element_text(size = 12),
        legend.position    = "none",
        legend.justification = c(0, 1),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 12),
        axis.text          = element_text(size = 12)) +
  xlab("Total nitrogen (g/kg)") +
  ylab("Phragmites zr") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-2, 2),
                     labels = scales::number_format(accuracy = 0.1))
p_marginal <- ggMarginal(p,
                         type        = "density",
                         margins     = "both",
                         groupColour = TRUE,
                         groupFill   = TRUE,
                         alpha       = 0.4)
print(p_marginal)
ggsave("Figure - Total_nitrogen.pdf", p_marginal, width = 120, height = 90, units = "mm")