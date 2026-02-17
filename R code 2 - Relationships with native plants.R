###################################################
##### Guo et al., 2026 - Phragmites expansion #####
###################################################

### R code 2 - Relationships with native plants ###

library(glmmTMB)
library(cowplot)
library(nortest)
library(sf)
library(ggplot2) 
library(knitr)
library(tidyverse)
library(brms)
library(ape)
library(coda)
library(modelr)
library(gridExtra)
library(pBrackets)
library(RColorBrewer)
library(performance)
library(phytools)
library(kableExtra)
library(tidybayes)
library(formattable)
library(grid)
library(Taxonstand)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(metafor)
library(ggbeeswarm)
library(pander)
library(raster)
library(maps)
library(mapdata)
library(rworldmap)
library(readxl)
library(utils)
library(reshape2)
library(ggpubr)
library(stringr)
library(ggstance)
library(ggridges)
library(sp)
library(geodata)
library(multcompView)
library(multcomp)
library(rotl)
library(psych)
library(meta)
library(devtools)
library(processx)
library(metaAidR)
library(gghalves)
library(ggtree)
library(sf)
library(leaflet)
library(ggspatial)
library(clipr)
library(gstat)
library(knitr)
library(tidyverse)
library(brms)
library(ape)
library(coda)
library(modelr)
library(gridExtra)
library(pBrackets)
library(RColorBrewer)
library(performance)
library(phytools)
library(kableExtra)
library(tidybayes)
library(formattable)
library(grid)
library(Taxonstand)
library(base)
library(bayesplot)
library(metafor)
library(ggbeeswarm)
library(pander)
library(raster)
library(maps)
library(mapdata)
library(rworldmap)
library(readxl)
library(utils)
library(reshape2)
library(ggpubr)
library(stringr)
library(ggstance)
library(ggridges) 
library(sp)
library(raster)
library(geodata)
library(car)
library(dplyr)
library(broom)
library(extrafont)
library(tidyverse)
library(tidyselect)
library(lme4)
library(emmeans)
library(broom)
library(cowplot)
library(reshape2)
library(ggcorrplot)
library(MASS)
library(geodata)
library(ggbeeswarm)
library(ggsignif)
library(emmeans)
library(sf)
library(dplyr)
library(tidyverse)
library(agricolae)
library(scales)
library(ggExtra)

# Calculate frequency of co-occurring native plants
phragmites_groups <- data %>% filter(Cover != 0)
phragmites_groups <- phragmites_groups %>%
  group_by(Station_ID, Collection_year) %>%
  filter(any(Plant_species == "Phragmites australis"))
species_frequency <- phragmites_groups %>%
  filter(Plant_species != "Phragmites australis") %>% 
  group_by(Plant_species) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))
print(species_frequency, n = 196)



# Species 1 - supplement the Spartina_patens data 
all_columns <- colnames(data)
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Spartina patens")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Spartina patens"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Spartina_patens <- do.call(rbind, result_list)
data_Spartina_patens <- data_Spartina_patens[data_Spartina_patens$Plant_species == "Spartina patens", ]
data_Spartina_patens_site <- data_Spartina_patens %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Spartina_patens$Collection_year <- as.numeric(data_Spartina_patens$Collection_year)
data_Spartina_patens$Cover <- as.numeric(data_Spartina_patens$Cover)

fisher_z_transform <- function(r) {
  if (is.na(r)) {
    return(NA)
  }
  if (r >= 1) {
    return(Inf)
  }
  if (r <= -1) {
    return(-Inf)
  }
  return(0.5 * log((1 + r) / (1 - r)))
}

data_Spartina_patens_zr  <- data_Spartina_patens %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Spartina_patens = pr_value, zr.Spartina_patens = zr_value, var.Spartina_patens = var_value, n.Spartina_patens = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Spartina_patens_zr <- data_Spartina_patens_zr %>% filter(n.Spartina_patens > 3)
data_Spartina_patens_zr <- data_Spartina_patens_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Spartina_patens_zr <- data_Spartina_patens_zr %>% filter(!is.na(zr.Spartina_patens))

data_merged_1 <- inner_join(data_phragmites_zr, data_Spartina_patens_zr, by = "Station_ID")
model <- lm(zr.Spartina_patens ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Spartina_patens ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Spartina_patens ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Spartina_patens ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 2 - supplement the Vigna luteola data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Vigna luteola")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Vigna luteola"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Vigna_luteola <- do.call(rbind, result_list)
data_Vigna_luteola <- data_Vigna_luteola[data_Vigna_luteola$Plant_species == "Vigna luteola", ]
unique_sites <- unique(data_Vigna_luteola$Site_ID)
length(unique_sites)
data_Vigna_luteola_site <- data_Vigna_luteola %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
unique_sites <- unique(data_Vigna_luteola$Site_ID)
length(unique_sites)

# calculate zr
data_Vigna_luteola$Collection_year <- as.numeric(data_Vigna_luteola$Collection_year)
data_Vigna_luteola$Cover <- as.numeric(data_Vigna_luteola$Cover)

data_Vigna_luteola_zr  <- data_Vigna_luteola %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Vigna_luteola = pr_value, zr.Vigna_luteola = zr_value, var.Vigna_luteola = var_value, n.Vigna_luteola = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Vigna_luteola_zr <- data_Vigna_luteola_zr %>% filter(n.Vigna_luteola > 3)
data_Vigna_luteola_zr <- data_Vigna_luteola_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Vigna_luteola_zr <- data_Vigna_luteola_zr %>% filter(!is.na(zr.Vigna_luteola))

data_merged_1 <- inner_join(data_phragmites_zr, data_Vigna_luteola_zr, by = "Station_ID")
model <- lm(zr.Vigna_luteola ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Vigna_luteola ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Vigna_luteola ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Vigna_luteola ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 3 - supplement the Ipomoea sagittata data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()
for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Ipomoea sagittata")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Ipomoea sagittata"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Ipomoea_sagittata <- do.call(rbind, result_list)
data_Ipomoea_sagittata <- data_Ipomoea_sagittata[data_Ipomoea_sagittata$Plant_species == "Ipomoea sagittata", ]
data_Ipomoea_sagittata_site <- data_Ipomoea_sagittata %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
unique_sites <- unique(data_Ipomoea_sagittata$Site_ID)
length(unique_sites)

# calculate zr
data_Ipomoea_sagittata$Collection_year <- as.numeric(data_Ipomoea_sagittata$Collection_year)
data_Ipomoea_sagittata$Cover <- as.numeric(data_Ipomoea_sagittata$Cover)

data_Ipomoea_sagittata_zr  <- data_Ipomoea_sagittata %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Ipomoea_sagittata = pr_value, zr.Ipomoea_sagittata = zr_value, var.Ipomoea_sagittata = var_value, n.Ipomoea_sagittata = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Ipomoea_sagittata_zr <- data_Ipomoea_sagittata_zr %>% filter(n.Ipomoea_sagittata > 3)
data_Ipomoea_sagittata_zr <- data_Ipomoea_sagittata_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Ipomoea_sagittata_zr <- data_Ipomoea_sagittata_zr %>% filter(!is.na(zr.Ipomoea_sagittata))

data_merged_1 <- inner_join(data_phragmites_zr, data_Ipomoea_sagittata_zr, by = "Station_ID")
model <- lm(zr.Ipomoea_sagittata ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Ipomoea_sagittata ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Ipomoea_sagittata ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Ipomoea_sagittata ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 4 - supplement the Bolboschoenus robustus data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Bolboschoenus robustus")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Bolboschoenus robustus"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Bolboschoenus_robustus <- do.call(rbind, result_list)
data_Bolboschoenus_robustus <- data_Bolboschoenus_robustus[data_Bolboschoenus_robustus$Plant_species == "Bolboschoenus robustus", ]
data_Bolboschoenus_robustus_site <- data_Bolboschoenus_robustus %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
unique_sites <- unique(data_Bolboschoenus_robustus$Site_ID)
length(unique_sites)

# calculate zr
data_Bolboschoenus_robustus$Collection_year <- as.numeric(data_Bolboschoenus_robustus$Collection_year)
data_Bolboschoenus_robustus$Cover <- as.numeric(data_Bolboschoenus_robustus$Cover)

data_Bolboschoenus_robustus_zr  <- data_Bolboschoenus_robustus %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Bolboschoenus_robustus = pr_value, zr.Bolboschoenus_robustus = zr_value, var.Bolboschoenus_robustus = var_value, n.Bolboschoenus_robustus = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Bolboschoenus_robustus_zr <- data_Bolboschoenus_robustus_zr %>% filter(n.Bolboschoenus_robustus > 3)
data_Bolboschoenus_robustus_zr <- data_Bolboschoenus_robustus_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Bolboschoenus_robustus_zr <- data_Bolboschoenus_robustus_zr %>% filter(!is.na(zr.Bolboschoenus_robustus))

data_merged_1 <- inner_join(data_phragmites_zr, data_Bolboschoenus_robustus_zr, by = "Station_ID")
model <- lm(zr.Bolboschoenus_robustus ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Bolboschoenus_robustus ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Bolboschoenus_robustus ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Bolboschoenus_robustus ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 5 - supplement the Sagittaria lancifolia data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Sagittaria lancifolia")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Sagittaria lancifolia"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Sagittaria_lancifolia <- do.call(rbind, result_list)
data_Sagittaria_lancifolia <- data_Sagittaria_lancifolia[data_Sagittaria_lancifolia$Plant_species == "Sagittaria lancifolia", ]
unique_sites <- unique(data_Sagittaria_lancifolia$Site_ID)
length(unique_sites)
data_Sagittaria_lancifolia_site <- data_Sagittaria_lancifolia %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Sagittaria_lancifolia$Collection_year <- as.numeric(data_Sagittaria_lancifolia$Collection_year)
data_Sagittaria_lancifolia$Cover <- as.numeric(data_Sagittaria_lancifolia$Cover)

data_Sagittaria_lancifolia_zr  <- data_Sagittaria_lancifolia %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Sagittaria_lancifolia = pr_value, zr.Sagittaria_lancifolia = zr_value, var.Sagittaria_lancifolia = var_value, n.Sagittaria_lancifolia = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Sagittaria_lancifolia_zr <- data_Sagittaria_lancifolia_zr %>% filter(n.Sagittaria_lancifolia > 3)
data_Sagittaria_lancifolia_zr <- data_Sagittaria_lancifolia_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Sagittaria_lancifolia_zr <- data_Sagittaria_lancifolia_zr %>% filter(!is.na(zr.Sagittaria_lancifolia))

data_merged_1 <- inner_join(data_phragmites_zr, data_Sagittaria_lancifolia_zr, by = "Station_ID")
model <- lm(zr.Sagittaria_lancifolia ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Sagittaria_lancifolia ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Sagittaria_lancifolia ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Sagittaria_lancifolia ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 6 - supplement the Polygonum punctatum data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Polygonum punctatum")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Polygonum punctatum"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Polygonum_punctatum <- do.call(rbind, result_list)
data_Polygonum_punctatum <- data_Polygonum_punctatum[data_Polygonum_punctatum$Plant_species == "Polygonum punctatum", ]
unique_sites <- unique(data_Polygonum_punctatum$Site_ID)
length(unique_sites)
data_Polygonum_punctatum_site <- data_Polygonum_punctatum %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Polygonum_punctatum$Collection_year <- as.numeric(data_Polygonum_punctatum$Collection_year)
data_Polygonum_punctatum$Cover <- as.numeric(data_Polygonum_punctatum$Cover)

data_Polygonum_punctatum_zr  <- data_Polygonum_punctatum %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Polygonum_punctatum = pr_value, zr.Polygonum_punctatum = zr_value, var.Polygonum_punctatum = var_value, n.Polygonum_punctatum = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Polygonum_punctatum_zr <- data_Polygonum_punctatum_zr %>% filter(n.Polygonum_punctatum > 3)
data_Polygonum_punctatum_zr <- data_Polygonum_punctatum_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Polygonum_punctatum_zr <- data_Polygonum_punctatum_zr %>% filter(!is.na(zr.Polygonum_punctatum))

data_merged_1 <- inner_join(data_phragmites_zr, data_Polygonum_punctatum_zr, by = "Station_ID")
model <- lm(zr.Polygonum_punctatum ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Polygonum_punctatum ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Polygonum_punctatum ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Polygonum_punctatum ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 7 - supplement the Schoenoplectus americanus data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Schoenoplectus americanus")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Schoenoplectus americanus"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Schoenoplectus_americanus <- do.call(rbind, result_list)
data_Schoenoplectus_americanus <- data_Schoenoplectus_americanus[data_Schoenoplectus_americanus$Plant_species == "Schoenoplectus americanus", ]
unique_sites <- unique(data_Schoenoplectus_americanus$Site_ID)
length(unique_sites)
data_Schoenoplectus_americanus_site <- data_Schoenoplectus_americanus %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Schoenoplectus_americanus$Collection_year <- as.numeric(data_Schoenoplectus_americanus$Collection_year)
data_Schoenoplectus_americanus$Cover <- as.numeric(data_Schoenoplectus_americanus$Cover)

data_Schoenoplectus_americanus_zr  <- data_Schoenoplectus_americanus %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Schoenoplectus_americanus = pr_value, zr.Schoenoplectus_americanus = zr_value, var.Schoenoplectus_americanus = var_value, n.Schoenoplectus_americanus = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Schoenoplectus_americanus_zr <- data_Schoenoplectus_americanus_zr %>% filter(n.Schoenoplectus_americanus > 3)
data_Schoenoplectus_americanus_zr <- data_Schoenoplectus_americanus_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Schoenoplectus_americanus_zr <- data_Schoenoplectus_americanus_zr %>% filter(!is.na(zr.Schoenoplectus_americanus))

data_merged_1 <- inner_join(data_phragmites_zr, data_Schoenoplectus_americanus_zr, by = "Station_ID")
model <- lm(zr.Schoenoplectus_americanus ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Schoenoplectus_americanus ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Schoenoplectus_americanus ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Schoenoplectus_americanus ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 8 - supplement the Leersia_hexandra data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Leersia hexandra")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Leersia hexandra"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Leersia_hexandra <- do.call(rbind, result_list)
data_Leersia_hexandra <- data_Leersia_hexandra[data_Leersia_hexandra$Plant_species == "Leersia hexandra", ]
unique_sites <- unique(data_Leersia_hexandra$Site_ID)
length(unique_sites)
data_Leersia_hexandra_site <- data_Leersia_hexandra %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Leersia_hexandra$Collection_year <- as.numeric(data_Leersia_hexandra$Collection_year)
data_Leersia_hexandra$Cover <- as.numeric(data_Leersia_hexandra$Cover)

data_Leersia_hexandra_zr  <- data_Leersia_hexandra %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Leersia_hexandra = pr_value, zr.Leersia_hexandra = zr_value, var.Leersia_hexandra = var_value, n.Leersia_hexandra = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Leersia_hexandra_zr <- data_Leersia_hexandra_zr %>% filter(n.Leersia_hexandra > 3)
data_Leersia_hexandra_zr <- data_Leersia_hexandra_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Leersia_hexandra_zr <- data_Leersia_hexandra_zr %>% filter(!is.na(zr.Leersia_hexandra))

data_merged_1 <- inner_join(data_phragmites_zr, data_Leersia_hexandra_zr, by = "Station_ID")
model <- lm(zr.Leersia_hexandra ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Leersia_hexandra ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Leersia_hexandra ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Leersia_hexandra ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 9 - supplement the Distichlis_spicata data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Distichlis spicata")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Distichlis spicata"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Distichlis_spicata <- do.call(rbind, result_list)
data_Distichlis_spicata <- data_Distichlis_spicata[data_Distichlis_spicata$Plant_species == "Distichlis spicata", ]
unique_sites <- unique(data_Distichlis_spicata$Site_ID)
length(unique_sites)
data_Distichlis_spicata_site <- data_Distichlis_spicata %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Distichlis_spicata$Collection_year <- as.numeric(data_Distichlis_spicata$Collection_year)
data_Distichlis_spicata$Cover <- as.numeric(data_Distichlis_spicata$Cover)

data_Distichlis_spicata_zr  <- data_Distichlis_spicata %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Distichlis_spicata = pr_value, zr.Distichlis_spicata = zr_value, var.Distichlis_spicata = var_value, n.Distichlis_spicata = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Distichlis_spicata_zr <- data_Distichlis_spicata_zr %>% filter(n.Distichlis_spicata > 3)
data_Distichlis_spicata_zr <- data_Distichlis_spicata_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Distichlis_spicata_zr <- data_Distichlis_spicata_zr %>% filter(!is.na(zr.Distichlis_spicata))

data_merged_1 <- inner_join(data_phragmites_zr, data_Distichlis_spicata_zr, by = "Station_ID")
model <- lm(zr.Distichlis_spicata ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Distichlis_spicata ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Distichlis_spicata ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Distichlis_spicata ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 10 - supplement the Spartina_alterniflora data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Spartina alterniflora")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Spartina alterniflora"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Spartina_alterniflora <- do.call(rbind, result_list)
data_Spartina_alterniflora <- data_Spartina_alterniflora[data_Spartina_alterniflora$Plant_species == "Spartina alterniflora", ]
unique_sites <- unique(data_Spartina_alterniflora$Site_ID)
length(unique_sites)
data_Spartina_alterniflora_site <- data_Spartina_alterniflora %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Spartina_alterniflora$Collection_year <- as.numeric(data_Spartina_alterniflora$Collection_year)
data_Spartina_alterniflora$Cover <- as.numeric(data_Spartina_alterniflora$Cover)

data_Spartina_alterniflora_zr  <- data_Spartina_alterniflora %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Spartina_alterniflora = pr_value, zr.Spartina_alterniflora = zr_value, var.Spartina_alterniflora = var_value, n.Spartina_alterniflora = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Spartina_alterniflora_zr <- data_Spartina_alterniflora_zr %>% filter(n.Spartina_alterniflora > 3)
data_Spartina_alterniflora_zr <- data_Spartina_alterniflora_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Spartina_alterniflora_zr <- data_Spartina_alterniflora_zr %>% filter(!is.na(zr.Spartina_alterniflora))

data_merged_1 <- inner_join(data_phragmites_zr, data_Spartina_alterniflora_zr, by = "Station_ID")
model <- lm(zr.Spartina_alterniflora ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Spartina_alterniflora ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Spartina_alterniflora ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Spartina_alterniflora ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 11 - supplement the Sagittaria_platyphylla data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Sagittaria platyphylla")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Sagittaria platyphylla"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Sagittaria_platyphylla <- do.call(rbind, result_list)
data_Sagittaria_platyphylla <- data_Sagittaria_platyphylla[data_Sagittaria_platyphylla$Plant_species == "Sagittaria platyphylla", ]
unique_sites <- unique(data_Sagittaria_platyphylla$Site_ID)
length(unique_sites)
data_Sagittaria_platyphylla_site <- data_Sagittaria_platyphylla %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Sagittaria_platyphylla$Collection_year <- as.numeric(data_Sagittaria_platyphylla$Collection_year)
data_Sagittaria_platyphylla$Cover <- as.numeric(data_Sagittaria_platyphylla$Cover)

data_Sagittaria_platyphylla_zr  <- data_Sagittaria_platyphylla %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Sagittaria_platyphylla = pr_value, zr.Sagittaria_platyphylla = zr_value, var.Sagittaria_platyphylla = var_value, n.Sagittaria_platyphylla = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Sagittaria_platyphylla_zr <- data_Sagittaria_platyphylla_zr %>% filter(n.Sagittaria_platyphylla > 3)
data_Sagittaria_platyphylla_zr <- data_Sagittaria_platyphylla_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Sagittaria_platyphylla_zr <- data_Sagittaria_platyphylla_zr %>% filter(!is.na(zr.Sagittaria_platyphylla))

data_merged_1 <- inner_join(data_phragmites_zr, data_Sagittaria_platyphylla_zr, by = "Station_ID")
model <- lm(zr.Sagittaria_platyphylla ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Sagittaria_platyphylla ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Sagittaria_platyphylla ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Sagittaria_platyphylla ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 12 - supplement the Typha_latifolia data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Typha latifolia")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Typha latifolia"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Typha_latifolia <- do.call(rbind, result_list)
data_Typha_latifolia <- data_Typha_latifolia[data_Typha_latifolia$Plant_species == "Typha latifolia", ]
unique_sites <- unique(data_Typha_latifolia$Site_ID)
length(unique_sites)
data_Typha_latifolia_site <- data_Typha_latifolia %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Typha_latifolia$Collection_year <- as.numeric(data_Typha_latifolia$Collection_year)
data_Typha_latifolia$Cover <- as.numeric(data_Typha_latifolia$Cover)

data_Typha_latifolia_zr  <- data_Typha_latifolia %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Typha_latifolia = pr_value, zr.Typha_latifolia = zr_value, var.Typha_latifolia = var_value, n.Typha_latifolia = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Typha_latifolia_zr <- data_Typha_latifolia_zr %>% filter(n.Typha_latifolia > 3)
data_Typha_latifolia_zr <- data_Typha_latifolia_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Typha_latifolia_zr <- data_Typha_latifolia_zr %>% filter(!is.na(zr.Typha_latifolia))

data_merged_1 <- inner_join(data_phragmites_zr, data_Typha_latifolia_zr, by = "Station_ID")
model <- lm(zr.Typha_latifolia ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Typha_latifolia ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Typha_latifolia ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Typha_latifolia ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 13 - supplement the Panicum_hemitomon data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Panicum hemitomon")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Panicum hemitomon"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Panicum_hemitomon <- do.call(rbind, result_list)
data_Panicum_hemitomon <- data_Panicum_hemitomon[data_Panicum_hemitomon$Plant_species == "Panicum hemitomon", ]
unique_sites <- unique(data_Panicum_hemitomon$Site_ID)
length(unique_sites)
data_Panicum_hemitomon_site <- data_Panicum_hemitomon %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Panicum_hemitomon$Collection_year <- as.numeric(data_Panicum_hemitomon$Collection_year)
data_Panicum_hemitomon$Cover <- as.numeric(data_Panicum_hemitomon$Cover)

data_Panicum_hemitomon_zr  <- data_Panicum_hemitomon %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Panicum_hemitomon = pr_value, zr.Panicum_hemitomon = zr_value, var.Panicum_hemitomon = var_value, n.Panicum_hemitomon = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Panicum_hemitomon_zr <- data_Panicum_hemitomon_zr %>% filter(n.Panicum_hemitomon > 3)
data_Panicum_hemitomon_zr <- data_Panicum_hemitomon_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Panicum_hemitomon_zr <- data_Panicum_hemitomon_zr %>% filter(!is.na(zr.Panicum_hemitomon))

data_merged_1 <- inner_join(data_phragmites_zr, data_Panicum_hemitomon_zr, by = "Station_ID")
model <- lm(zr.Panicum_hemitomon ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Panicum_hemitomon ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Panicum_hemitomon ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Panicum_hemitomon ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 14 - supplement the Typha domingensis data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Typha domingensis")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Typha domingensis"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Typha_domingensis <- do.call(rbind, result_list)
data_Typha_domingensis <- data_Typha_domingensis[data_Typha_domingensis$Plant_species == "Typha domingensis", ]
unique_sites <- unique(data_Typha_domingensis$Site_ID)
length(unique_sites)
data_Typha_domingensis_site <- data_Typha_domingensis %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Typha_domingensis$Collection_year <- as.numeric(data_Typha_domingensis$Collection_year)
data_Typha_domingensis$Cover <- as.numeric(data_Typha_domingensis$Cover)

data_Typha_domingensis_zr  <- data_Typha_domingensis %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Typha_domingensis = pr_value, zr.Typha_domingensis = zr_value, var.Typha_domingensis = var_value, n.Typha_domingensis = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Typha_domingensis_zr <- data_Typha_domingensis_zr %>% filter(n.Typha_domingensis > 3)
data_Typha_domingensis_zr <- data_Typha_domingensis_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Typha_domingensis_zr <- data_Typha_domingensis_zr %>% filter(!is.na(zr.Typha_domingensis))

data_merged_1 <- inner_join(data_phragmites_zr, data_Typha_domingensis_zr, by = "Station_ID")
model <- lm(zr.Typha_domingensis ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Typha_domingensis ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Typha_domingensis ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Typha_domingensis ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 15 - supplement the Amaranthus_australis data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Amaranthus australis")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Amaranthus australis"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Amaranthus_australis <- do.call(rbind, result_list)
data_Amaranthus_australis <- data_Amaranthus_australis[data_Amaranthus_australis$Plant_species == "Amaranthus australis", ]
unique_sites <- unique(data_Amaranthus_australis$Site_ID)
length(unique_sites)
data_Amaranthus_australis_site <- data_Amaranthus_australis %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Amaranthus_australis$Collection_year <- as.numeric(data_Amaranthus_australis$Collection_year)
data_Amaranthus_australis$Cover <- as.numeric(data_Amaranthus_australis$Cover)

data_Amaranthus_australis_zr  <- data_Amaranthus_australis %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Amaranthus_australis = pr_value, zr.Amaranthus_australis = zr_value, var.Amaranthus_australis = var_value, n.Amaranthus_australis = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Amaranthus_australis_zr <- data_Amaranthus_australis_zr %>% filter(n.Amaranthus_australis > 3)
data_Amaranthus_australis_zr <- data_Amaranthus_australis_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Amaranthus_australis_zr <- data_Amaranthus_australis_zr %>% filter(!is.na(zr.Amaranthus_australis))

data_merged_1 <- inner_join(data_phragmites_zr, data_Amaranthus_australis_zr, by = "Station_ID")
model <- lm(zr.Amaranthus_australis ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Amaranthus_australis ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Amaranthus_australis ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Amaranthus_australis ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 16 - supplement the Ludwigia_grandiflora data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Ludwigia grandiflora")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Ludwigia grandiflora"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Ludwigia_grandiflora <- do.call(rbind, result_list)
data_Ludwigia_grandiflora <- data_Ludwigia_grandiflora[data_Ludwigia_grandiflora$Plant_species == "Ludwigia grandiflora", ]
unique_sites <- unique(data_Ludwigia_grandiflora$Site_ID)
length(unique_sites)
data_Ludwigia_grandiflora_site <- data_Ludwigia_grandiflora %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Ludwigia_grandiflora$Collection_year <- as.numeric(data_Ludwigia_grandiflora$Collection_year)
data_Ludwigia_grandiflora$Cover <- as.numeric(data_Ludwigia_grandiflora$Cover)

data_Ludwigia_grandiflora_zr  <- data_Ludwigia_grandiflora %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Ludwigia_grandiflora = pr_value, zr.Ludwigia_grandiflora = zr_value, var.Ludwigia_grandiflora = var_value, n.Ludwigia_grandiflora = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Ludwigia_grandiflora_zr <- data_Ludwigia_grandiflora_zr %>% filter(n.Ludwigia_grandiflora > 3)
data_Ludwigia_grandiflora_zr <- data_Ludwigia_grandiflora_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Ludwigia_grandiflora_zr <- data_Ludwigia_grandiflora_zr %>% filter(!is.na(zr.Ludwigia_grandiflora))

data_merged_1 <- inner_join(data_phragmites_zr, data_Ludwigia_grandiflora_zr, by = "Station_ID")
model <- lm(zr.Ludwigia_grandiflora ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Ludwigia_grandiflora ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Ludwigia_grandiflora ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Ludwigia_grandiflora ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 17 - supplement the Mikania scandens data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Mikania scandens")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Mikania scandens"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Mikania_scandens <- do.call(rbind, result_list)
data_Mikania_scandens <- data_Mikania_scandens[data_Mikania_scandens$Plant_species == "Mikania scandens", ]
unique_sites <- unique(data_Mikania_scandens$Site_ID)
length(unique_sites)
data_Mikania_scandens_site <- data_Mikania_scandens %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Mikania_scandens$Collection_year <- as.numeric(data_Mikania_scandens$Collection_year)
data_Mikania_scandens$Cover <- as.numeric(data_Mikania_scandens$Cover)

data_Mikania_scandens_zr  <- data_Mikania_scandens %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Mikania_scandens = pr_value, zr.Mikania_scandens = zr_value, var.Mikania_scandens = var_value, n.Mikania_scandens = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Mikania_scandens_zr <- data_Mikania_scandens_zr %>% filter(n.Mikania_scandens > 3)
data_Mikania_scandens_zr <- data_Mikania_scandens_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Mikania_scandens_zr <- data_Mikania_scandens_zr %>% filter(!is.na(zr.Mikania_scandens))

data_merged_1 <- inner_join(data_phragmites_zr, data_Mikania_scandens_zr, by = "Station_ID")
model <- lm(zr.Mikania_scandens ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Mikania_scandens ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Mikania_scandens ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Mikania_scandens ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)



# Species 18 - supplement the Cicuta maculata data
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Cicuta maculata")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Cicuta maculata"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data_Cicuta_maculata <- do.call(rbind, result_list)
data_Cicuta_maculata <- data_Cicuta_maculata[data_Cicuta_maculata$Plant_species == "Cicuta maculata", ]
unique_sites <- unique(data_Cicuta_maculata$Site_ID)
length(unique_sites)
data_Cicuta_maculata_site <- data_Cicuta_maculata %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

# calculate zr
data_Cicuta_maculata$Collection_year <- as.numeric(data_Cicuta_maculata$Collection_year)
data_Cicuta_maculata$Cover <- as.numeric(data_Cicuta_maculata$Cover)

data_Cicuta_maculata_zr  <- data_Cicuta_maculata %>%
  group_by(Station_ID) %>%
  group_modify(~ {
    n_value <- nrow(.x)
    if(n_value > 1) {
      pr_value <- cor(.x$Collection_year, .x$Cover, use = "complete.obs")
      zr_value <- fisher_z_transform(pr_value)
    } else {
      pr_value <- NA
      zr_value <- NA
    }
    var_value <- ifelse(n_value > 3, 1 / (n_value - 3), NA)
    .x <- .x %>%
      mutate(pr.Cicuta_maculata = pr_value, zr.Cicuta_maculata = zr_value, var.Cicuta_maculata = var_value, n.Cicuta_maculata = n_value)
    return(.x)
  }) %>%
  ungroup()

data_Cicuta_maculata_zr <- data_Cicuta_maculata_zr %>% filter(n.Cicuta_maculata > 3)
data_Cicuta_maculata_zr <- data_Cicuta_maculata_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_Cicuta_maculata_zr <- data_Cicuta_maculata_zr %>% filter(!is.na(zr.Cicuta_maculata))

data_merged_1 <- inner_join(data_phragmites_zr, data_Cicuta_maculata_zr, by = "Station_ID")
model <- lm(zr.Cicuta_maculata ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Cicuta_maculata ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Cicuta_maculata ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Cicuta_maculata ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)


# Figure 2
# Figure 2A - Spartina patens
data_merged_1 <- inner_join(data_phragmites_zr, data_Spartina_patens_zr, by = "Station_ID")
model <- lm(zr.Spartina_patens ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Spartina_patens ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Spartina_patens ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Spartina_patens ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)

p <- ggplot(data_merged_1, aes(x = zr.phragmites, y = zr.Spartina_patens)) +
  geom_point(aes(color=Status), size = 4, shape = 16, stroke = 0.25, alpha = 0.9) +
  geom_smooth(aes(color="Total", fill="Total", linetype="Total"),
              method="lm", se=TRUE, alpha=0.15) +
  geom_smooth(aes(color=Status, fill=Status, linetype=Status),
              method="lm", se=TRUE, alpha=0.15) +
  scale_color_manual(values=c("Total"     = "#2E2E2E",
                              "Expanding" = "#f16c23",
                              "Stable"    = "#1b7c3d",
                              "Declining" = "#2b6a99")) +    
  scale_fill_manual(values=c("Total"     = "#2E2E2E",
                             "Expanding" = "#f16c23",
                             "Stable"    = "#1b7c3d",
                             "Declining" = "#2b6a99"), guide="none") +
  scale_linetype_manual(values=c("Total"     = "dashed",
                                 "Expanding" = "solid",
                                 "Stable"    = "dashed",
                                 "Declining" = "dashed"), guide="none") +
  guides(color = guide_legend(title="Status")) +
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(size = 12, hjust=0.5),
        axis.title.x=element_text(size = 12),
        legend.position     = c(0.03, 0.97),
        legend.justification= c(0,1),
        axis.line.x = element_line(color="black", linewidth=0.5),
        axis.line.y = element_line(color="black", linewidth=0.5),
        axis.ticks  = element_line(color="black", linewidth=0.5),
        axis.title  = element_text(size = 12),
        axis.text   = element_text(size = 12)) +
  xlab("Phragmites zr") +
  ylab("Spartina patens zr") + 
  scale_x_continuous(labels=scales::number_format(accuracy=0.1)) +
  scale_y_continuous(limits = c(-1.9, 2.2), labels=scales::number_format(accuracy=0.1))
p_marginal <- ggMarginal(p,
                         type="density",
                         margins="both",
                         groupColour=TRUE,
                         groupFill=TRUE,
                         alpha=0.4)
print(p_marginal)
ggsave("Figure - Spartina patens.pdf", p_marginal, width=160, height=120, units="mm")



# Figure 2B - Vigna luteola
data_merged_1 <- inner_join(data_phragmites_zr, data_Vigna_luteola_zr, by = "Station_ID")
model <- lm(zr.Vigna_luteola ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Vigna_luteola ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Vigna_luteola ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Vigna_luteola ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)

p <- ggplot(data_merged_1, aes(x = zr.phragmites, y = zr.Vigna_luteola)) +
  geom_point(aes(color=Status), size = 4, shape = 16, stroke = 0.25, alpha = 0.9) +
  geom_smooth(aes(color="Total", fill="Total", linetype="Total"),
              method="lm", se=TRUE, alpha=0.15) +
  geom_smooth(aes(color=Status, fill=Status, linetype=Status),
              method="lm", se=TRUE, alpha=0.15) +
  scale_color_manual(values=c("Total"     = "#2E2E2E",
                              "Expanding" = "#f16c23",
                              "Stable"    = "#1b7c3d",
                              "Declining" = "#2b6a99")) +
  scale_fill_manual(values=c("Total"     = "#2E2E2E",
                             "Expanding" = "#f16c23",
                             "Stable"    = "#1b7c3d",
                             "Declining" = "#2b6a99"), guide="none") +
  scale_linetype_manual(values=c("Total"     = "dashed",
                                 "Expanding" = "dashed",
                                 "Stable"    = "solid",
                                 "Declining" = "dashed"), guide="none") +
  guides(color = guide_legend(title="Status")) +
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(size = 12, hjust=0.5),
        axis.title.x=element_text(size = 12),
        legend.position     = "none",
        legend.justification= c(0,1),
        axis.line.x = element_line(color="black", linewidth=0.5),
        axis.line.y = element_line(color="black", linewidth=0.5),
        axis.ticks  = element_line(color="black", linewidth=0.5),
        axis.title  = element_text(size = 12),
        axis.text   = element_text(size = 12)) +
  xlab("Phragmites zr") +
  ylab("Vigna luteola zr") +
  scale_x_continuous(labels=scales::number_format(accuracy=0.1)) +
  scale_y_continuous(limits = c(-1.9, 1.9), labels=scales::number_format(accuracy=0.1))
p_marginal <- ggMarginal(p,
                         type="density",
                         margins="both",
                         groupColour=TRUE,
                         groupFill=TRUE,
                         alpha=0.4)
print(p_marginal)
ggsave("Figure - Vigna luteola.pdf", p_marginal, width=160, height=120, units="mm")



### Figure 2C - Sagittaria lancifolia
data_merged_1 <- inner_join(data_phragmites_zr, data_Sagittaria_lancifolia_zr, by = "Station_ID")
model <- lm(zr.Sagittaria_lancifolia ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Sagittaria_lancifolia ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Sagittaria_lancifolia ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Sagittaria_lancifolia ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)

p <- ggplot(data_merged_1, aes(x = zr.phragmites, y = zr.Sagittaria_lancifolia)) +
  geom_point(aes(color=Status), size = 4, shape = 16, stroke = 0.25, alpha = 0.9) +
  geom_smooth(aes(color="Total", fill="Total", linetype="Total"),
              method="lm", se=TRUE, alpha=0.15) +
  geom_smooth(aes(color=Status, fill=Status, linetype=Status),
              method="lm", se=TRUE, alpha=0.15) +
  scale_color_manual(values=c("Total"     = "#2E2E2E",
                              "Expanding" = "#f16c23",
                              "Stable"    = "#1b7c3d",
                              "Declining" = "#2b6a99")) +
  scale_fill_manual(values=c("Total"     = "#2E2E2E",
                             "Expanding" = "#f16c23",
                             "Stable"    = "#1b7c3d",
                             "Declining" = "#2b6a99"), guide="none") +
  scale_linetype_manual(values=c("Total"     = "solid",
                                 "Expanding" = "solid",
                                 "Stable"    = "dashed",
                                 "Declining" = "dashed"), guide="none") +
  guides(color = guide_legend(title="Status")) +
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(size=12, hjust=0.5),
        axis.title.x=element_text(size=12),
        legend.position     = "none",
        legend.justification= c(0,1),
        axis.line.x = element_line(color="black", linewidth=0.5),
        axis.line.y = element_line(color="black", linewidth=0.5),
        axis.ticks  = element_line(color="black", linewidth=0.5),
        axis.title  = element_text(size=12),
        axis.text   = element_text(size=12)) +
  xlab("Phragmites zr") +
  ylab("Sagittaria lancifolia zr") +
  scale_x_continuous(labels = scales::number_format(accuracy=0.1)) +
  scale_y_continuous(limits = c(-1.2, 2.3),
                     labels = scales::number_format(accuracy=0.1))
p_marginal <- ggMarginal(p,
                         type = "density",
                         margins = "both",
                         groupColour = TRUE,
                         groupFill   = TRUE,
                         alpha       = 0.4)
print(p_marginal)
ggsave("Figure - Sagittaria lancifolia.pdf", p_marginal, width=160, height=120, units="mm")



### Figure 2D - Sagittaria platyphylla
data_merged_1 <- inner_join(data_phragmites_zr, data_Sagittaria_platyphylla_zr, by = "Station_ID")
model <- lm(zr.Sagittaria_platyphylla ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Sagittaria_platyphylla ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Sagittaria_platyphylla ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Sagittaria_platyphylla ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)

p <- ggplot(data_merged_1, aes(x = zr.phragmites, y = zr.Sagittaria_platyphylla)) +
  geom_point(aes(color=Status), size = 4, shape = 16, stroke = 0.25, alpha = 0.9) +
  geom_smooth(aes(color="Total", fill="Total", linetype="Total"),
              method="lm", se=TRUE, alpha=0.15) +
  geom_smooth(aes(color=Status, fill=Status, linetype=Status),
              method="lm", se=TRUE, alpha=0.15) +
  scale_color_manual(values=c("Total"     = "#2E2E2E",
                              "Expanding" = "#f16c23",
                              "Stable"    = "#1b7c3d",
                              "Declining" = "#2b6a99")) +
  scale_fill_manual(values=c("Total"     = "#2E2E2E",
                             "Expanding" = "#f16c23",
                             "Stable"    = "#1b7c3d",
                             "Declining" = "#2b6a99"), guide="none") +
  scale_linetype_manual(values=c("Total"     = "solid",
                                 "Expanding" = "dashed",
                                 "Stable"    = "dashed",
                                 "Declining" = "solid"), guide="none") +
  guides(color = guide_legend(title="Status")) +
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(size=12, hjust=0.5),
        axis.title.x=element_text(size=12),
        legend.position     = "none",
        legend.justification= c(0,1),
        axis.line.x = element_line(color="black", linewidth=0.5),
        axis.line.y = element_line(color="black", linewidth=0.5),
        axis.ticks  = element_line(color="black", linewidth=0.5),
        axis.title  = element_text(size=12),
        axis.text   = element_text(size=12)) +
  xlab("Phragmites zr") +
  ylab("Sagittaria platyphylla zr") +
  scale_x_continuous(labels=scales::number_format(accuracy=0.1)) +
  scale_y_continuous(limits = c(-2.0, 2.0),
                     labels=scales::number_format(accuracy=0.1))
p_marginal <- ggMarginal(p,
                         type="density",
                         margins="both",
                         groupColour=TRUE,
                         groupFill=TRUE,
                         alpha=0.4)
print(p_marginal)
ggsave("Figure - Sagittaria platyphylla.pdf", p_marginal, width=160, height=120, units="mm")



### Figure 2E - Panicum hemitomon
data_merged_1 <- inner_join(data_phragmites_zr, data_Panicum_hemitomon_zr, by = "Station_ID")
model <- lm(zr.Panicum_hemitomon ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Panicum_hemitomon ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Panicum_hemitomon ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Panicum_hemitomon ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)

p <- ggplot(data_merged_1, aes(x = zr.phragmites, y = zr.Panicum_hemitomon)) +
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
  scale_linetype_manual(values = c("Total"     = "dashed",
                                   "Expanding" = "solid",
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
  xlab("Phragmites zr") +
  ylab("Panicum hemitomon zr") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-2.2, 2.2),
                     labels = scales::number_format(accuracy = 0.1))
p_marginal <- ggMarginal(p,
                         type        = "density",
                         margins     = "both",
                         groupColour = TRUE,
                         groupFill   = TRUE,
                         alpha       = 0.4)
print(p_marginal)
ggsave("Figure - Panicum hemitomon.pdf", p_marginal, width = 160, height = 120, units = "mm")



### Figure 2F - Typha domingensis
data_merged_1 <- inner_join(data_phragmites_zr, data_Typha_domingensis_zr, by = "Station_ID")
model <- lm(zr.Typha_domingensis ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Typha_domingensis ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Typha_domingensis ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Typha_domingensis ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary

p <- ggplot(data_merged_1, aes(x = zr.phragmites, y = zr.Typha_domingensis)) +
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
                                   "Expanding" = "solid",
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
  xlab("Phragmites zr") +
  ylab("Typha domingensis zr") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-1.2, 1.9),
                     labels = scales::number_format(accuracy = 0.1))
p_marginal <- ggMarginal(p,
                         type        = "density",
                         margins     = "both",
                         groupColour = TRUE,
                         groupFill   = TRUE,
                         alpha       = 0.4)
print(p_marginal)
ggsave("Figure - Typha domingensis.pdf", p_marginal, width = 160, height = 120, units = "mm")



### Figure 2G - Amaranthus australis
data_merged_1 <- inner_join(data_phragmites_zr, data_Amaranthus_australis_zr, by = "Station_ID")
model <- lm(zr.Amaranthus_australis ~ zr.phragmites,  data = data_merged_1)
summary(model)
model <- lm(zr.Amaranthus_australis ~ zr.phragmites, data = subset(data_merged_1, Status == "Expanding"))
summary(model)
model <- lm(zr.Amaranthus_australis ~ zr.phragmites, data = subset(data_merged_1, Status == "Stable"))
summary(model)
model <- lm(zr.Amaranthus_australis ~ zr.phragmites, data = subset(data_merged_1, Status == "Declining"))
summary(model)

p <- ggplot(data_merged_1, aes(x = zr.phragmites, y = zr.Amaranthus_australis)) +
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
                                   "Expanding" = "solid",
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
  xlab("Phragmites zr") +
  ylab("Amaranthus australis zr") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-1.0, 1.7),
                     labels = scales::number_format(accuracy = 0.1))
p_marginal <- ggMarginal(p,
                         type        = "density",
                         margins     = "both",
                         groupColour = TRUE,
                         groupFill   = TRUE,
                         alpha       = 0.4)
print(p_marginal)
ggsave("Figure - Amaranthus australis.pdf", p_marginal, width = 160, height = 120, units = "mm")