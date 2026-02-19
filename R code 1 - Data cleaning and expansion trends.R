###################################################
##### Guo et al., 2026 - Phragmites expansion #####
###################################################

### R code 1 - Data cleaning and expansion trends ###

library(tidyverse)
library(glmmTMB)
library(car)
library(metafor)
library(ggExtra)
library(ggpubr)
library(cowplot)
library(sf)
library(lme4)
library(gridExtra) 
library(agricolae)

### Data cleaning ###
setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation")
data <- read.csv("CRMS_Marsh_Vegetation.csv")

colnames(data)[colnames(data) == "Station.ID"] <- "Station_ID"
colnames(data)[colnames(data) == "Plot.Size..m2."] <- "Plot_size"
colnames(data)[colnames(data) == "Collection.Date..mm.dd.yyyy."] <- "Collection_year"
colnames(data)[colnames(data) == "Sample.Type"] <- "Sample_type"
colnames(data)[colnames(data) == "Vegetation.Type"] <- "Vegetation_type"
colnames(data)[colnames(data) == "X..Cover.Total"] <- "Cover_total"
colnames(data)[colnames(data) == "X..Cover.Tree"] <- "Cover_tree"
colnames(data)[colnames(data) == "X..Cover.Shrub"] <- "Cover_shrub"
colnames(data)[colnames(data) == "X..Cover.Herb"] <- "Cover_herb"
colnames(data)[colnames(data) == "X..Cover.Carpet"] <- "Cover_carpet"
colnames(data)[colnames(data) == "Average.Height.Dominant..cm."] <- "Average_hight_dominant"
colnames(data)[colnames(data) == "Average.Height.Tree..cm."] <- "Average_height_tree"
colnames(data)[colnames(data) == "Average.Height.Shrub..cm."] <- "Average_hight_shrub"
colnames(data)[colnames(data) == "Average.Height.Herb..cm."] <- "Average_hight_herb"
colnames(data)[colnames(data) == "Average.Height.Carpet..cm."] <- "Average_hight_carpet"
colnames(data)[colnames(data) == "Scientific.Name.As.Originally.Observed"] <- "Scientific_name_as_originally_observed"
colnames(data)[colnames(data) == "Common.Name.As.Originally.Observed"] <- "Common_name_as_originally_observed"
colnames(data)[colnames(data) == "Scientific.Name.As.Currently.Recognized"] <- "Plant_species"
colnames(data)[colnames(data) == "Common.Name.As.Currently.Recognized"] <- "Common_name_as_currently_recognized"
colnames(data)[colnames(data) == "X..Cover"] <- "Cover"
colnames(data)[colnames(data) == "Braun.Blanquet.Rank"] <- "Braun_Blanquet_rank"
colnames(data)[colnames(data) == "In.Out"] <- "In_out"
colnames(data)[colnames(data) == "Number.Planted"] <- "Number_planted"
colnames(data)[colnames(data) == "Number.Alive"] <- "Number_alive"
colnames(data)[colnames(data) == "Additional.Species.Description"] <- "Additional_species_description"

data$Station_ID <- as.character(data$Station_ID)
data$Site_ID <- sapply(strsplit(data$Station_ID, "-"), function(x) x[1])
unique_sites <- unique(data$Site_ID)
length(unique_sites)

calcasieu_sabine_sites <- c("CRMS0684", "CRMS2189", "CRMS0669", "CRMS0662", "CRMS0663", 
                            "CRMS2166", "CRMS0665", "CRMS2156", "CRMS2154", "CRMS1838", 
                            "CRMS0697", "CRMS0660", "CRMS0683", "CRMS0693", "CRMS0661", 
                            "CRMS0658", "CRMS2219", "CRMS1205", "CRMS0682", "CRMS0694", 
                            "CRMS0677", "CRMS0651", "CRMS0641", "CRMS2334", "CRMS0638", 
                            "CRMS1858", "CRMS0680", "CRMS0635", "CRMS0639", "CRMS0642", 
                            "CRMS0647", "CRMS6302", "CRMS0672", "CRMS0655", "CRMS0685", 
                            "CRMS0687", "CRMS0644", "CRMS0656", "CRMS1743", "CRMS1738", 
                            "CRMS0645", "CRMS2418", "CRMS0648", "CRMS0650", "CRMS0691",
                            "CRMS6301")

mermentau_basin_sites <- c("CRMS1413", "CRMS1409", "CRMS0553", "CRMS0622", "CRMS0575", 
                           "CRMS0605", "CRMS0583", "CRMS0587", "CRMS0624", "CRMS2493", 
                           "CRMS0614", "CRMS0584", "CRMS0590", "CRMS1446", "CRMS0588", 
                           "CRMS0615", "CRMS0589", "CRMS0556", "CRMS0630", "CRMS0593", 
                           "CRMS0603", "CRMS0562", "CRMS0604", "CRMS0581", "CRMS0557", 
                           "CRMS0554", "CRMS0608", "CRMS0599", "CRMS0595", "CRMS0574", 
                           "CRMS0610", "CRMS0626", "CRMS0609", "CRMS0560", "CRMS0600", 
                           "CRMS0623", "CRMS1277", "CRMS1965", "CRMS0576", "CRMS0567", 
                           "CRMS0565", "CRMS1100", "CRMS0568", "CRMS0570", "CRMS1130", 
                           "CRMS0632", "CRMS0580", "CRMS0633", "CRMS0572", "CRMS0571", 
                           "CRMS0618", "CRMS0616", "CRMS0619")

teche_vermilion_basin_sites <- c("CRMS0501", "CRMS0536", "CRMS0508", "CRMS0507", "CRMS2041", 
                                 "CRMS0535", "CRMS0552", "CRMS1650", "CRMS0541", "CRMS0511",
                                 "CRMS0531", "CRMS0532", "CRMS0530", "CRMS0549", "CRMS0514",
                                 "CRMS0529", "CRMS0520", "CRMS0504", "CRMS0499", "CRMS0523",
                                 "CRMS0524", "CRMS0498", "CRMS0522", "CRMS0527", "CRMS0494",
                                 "CRMS0493", "CRMS0550", "CRMS0542", "CRMS0543", "CRMS0513",
                                 "CRMS0545", "CRMS5992", "CRMS0545", "CRMS0488", "CRMS0551",
                                 "CRMS0544", "CRMS0490", "CRMS0496", "CRMS0489", "CRMS0517",
                                 "CRMS0547")

atchafalaya_basin_sites <- c("CRMS4782", "CRMS4779", "CRMS6008", "CRMS4809", "CRMS4808", 
                             "CRMS0479", "CRMS6042", "CRMS6038", "CRMS0482", "CRMS2568", 
                             "CRMS4016", "CRMS0465", "CRMS0464", "CRMS4938", "CRMS0461", 
                             "CRMS4014", "CRMS0463", "CRMS6304", "CRMS5003", "CRMS4900")

terrebonne_basin_sites <- c("CRMS0324", "CRMS0403", "CRMS5536", "CRMS5770", "CRMS0414", 
                            "CRMS5035", "CRMS0301", "CRMS0365", "CRMS0305", "CRMS0293", 
                            "CRMS0309", "CRMS0329", "CRMS0399", "CRMS0322", "CRMS2682", 
                            "CRMS0327", "CRMS0371", "CRMS0326", "CRMS0303", "CRMS2785", 
                            "CRMS0290", "CRMS0332", "CRMS2881", "CRMS4045", "CRMS0354", 
                            "CRMS0377", "CRMS0411", "CRMS0294", "CRMS4455", "CRMS0383", 
                            "CRMS2887", "CRMS0409", "CRMS0296", "CRMS0398", "CRMS0302", 
                            "CRMS0395", "CRMS0396", "CRMS0381", "CRMS0382", "CRMS0394", 
                            "CRMS0376", "CRMS0307", "CRMS0421", "CRMS0311", "CRMS0374", 
                            "CRMS0345", "CRMS0434", "CRMS0369", "CRMS0347", "CRMS0390", 
                            "CRMS0392", "CRMS0385", "CRMS0367", "CRMS2939", "CRMS0315", 
                            "CRMS0331", "CRMS0355", "CRMS0400", "CRMS0341", "CRMS0416", 
                            "CRMS3296", "CRMS2825", "CRMS0338", "CRMS0312", "CRMS0336", 
                            "CRMS0335", "CRMS0386", "CRMS0337", "CRMS0387", "CRMS0319", 
                            "CRMS0318", "CRMS0978", "CRMS0310", "CRMS0397", "CRMS0292",
                            "CRMS2862")

barataria_basin_sites <- c("CRMS0200", "CRMS0194", "CRMS0197", "CRMS5116", "CRMS0217", 
                           "CRMS5672", "CRMS0218", "CRMS0206", "CRMS3136", "CRMS0268", 
                           "CRMS0192", "CRMS0211", "CRMS0241", "CRMS2991", "CRMS3054", 
                           "CRMS0273", "CRMS0219", "CRMS3166", "CRMS3169", "CRMS0189", 
                           "CRMS0190", "CRMS0278", "CRMS6303", "CRMS4218", "CRMS0164", 
                           "CRMS3985", "CRMS0175", "CRMS0188", "CRMS4245", "CRMS0234", 
                           "CRMS0261", "CRMS3565", "CRMS0185", "CRMS0248", "CRMS0220", 
                           "CRMS0253", "CRMS4103", "CRMS4690", "CRMS0178", "CRMS0287", 
                           "CRMS0225", "CRMS0251", "CRMS0232", "CRMS0276", "CRMS3601", 
                           "CRMS3590", "CRMS0237", "CRMS3617", "CRMS0224", "CRMS0263", 
                           "CRMS0226", "CRMS0260", "CRMS0258", "CRMS4529", "CRMS3680", 
                           "CRMS0209", "CRMS0176", "CRMS0171", "CRMS0282", "CRMS0174", 
                           "CRMS0173", "CRMS0172", "CRMS0272", "CRMS0179", "CRMS0181")

breton_sound_basin_sites <- c("CRMS0125", "CRMS0128", "CRMS0117", "CRMS0115", "CRMS0114", 
                              "CRMS0131", "CRMS0132", "CRMS0120", "CRMS4355", "CRMS0121", 
                              "CRMS0136", "CRMS0135", "CRMS0146", "CRMS0148", "CRMS0147", 
                              "CRMS0119", "CRMS0129", "CRMS0118", "CRMS0139", "CRMS2614")

mississippi_river_delta_basin_sites <- c("CRMS0163", "CRMS2608", "CRMS2634", "CRMS4626", "CRMS4448", 
                                         "CRMS0161", "CRMS2627", "CRMS0156", "CRMS0162", "CRMS0157", 
                                         "CRMS0154", "CRMS0153", "CRMS0159")

pontchartrain_basin_sites <- c("CRMS0065", "CRMS5167", "CRMS0008", "CRMS0038", "CRMS5845", 
                               "CRMS0039", "CRMS0097", "CRMS0061", "CRMS5414", "CRMS0063", 
                               "CRMS5373", "CRMS5255", "CRMS0089", "CRMS0090", "CRMS0058", 
                               "CRMS0047", "CRMS0059", "CRMS3913", "CRMS0056", "CRMS5267", 
                               "CRMS5452", "CRMS0046", "CRMS0034", "CRMS0033", "CRMS0030", 
                               "CRMS0103", "CRMS6209", "CRMS4094", "CRMS2854", "CRMS0006", 
                               "CRMS3667", "CRMS4407", "CRMS4406", "CRMS0002", "CRMS3626", 
                               "CRMS4107", "CRMS3650", "CRMS6299", "CRMS2830", "CRMS3641", 
                               "CRMS3639", "CRMS3664", "CRMS4548", "CRMS4551", "CRMS1024", 
                               "CRMS0108", "CRMS4572", "CRMS4596", "CRMS4557", "CRMS0003", 
                               "CRMS1069", "CRMS3784", "CRMS4110", "CRMS0035", "CRMS6088", 
                               "CRMS6090", "CRMS0086", "CRMS3800")

data <- data %>% mutate(Basin = case_when(Site_ID %in% calcasieu_sabine_sites ~ "Calcasieu Sabine",
                                          Site_ID %in% mermentau_basin_sites ~ "Mermentau",
                                          Site_ID %in% teche_vermilion_basin_sites ~ "Teche Vermilion",
                                          Site_ID %in% atchafalaya_basin_sites ~ "Atchafalaya",
                                          Site_ID %in% terrebonne_basin_sites ~ "Terrebonne",
                                          Site_ID %in% barataria_basin_sites ~ "Barataria",
                                          Site_ID %in% breton_sound_basin_sites ~ "Breton Sound",
                                          Site_ID %in% mississippi_river_delta_basin_sites ~ "Mississippi River Delta",
                                          Site_ID %in% pontchartrain_basin_sites ~ "Pontchartrain",
                                          TRUE ~ NA_character_)) %>% filter(!is.na(Basin))

data$Plant_species <- sapply(strsplit(data$Plant_species, " "), function(x) paste(x[1:2], collapse = " "))
data$Collection_year <- sapply(strsplit(data$Collection_year, "/"), function(x) paste(x[3:length(x)], collapse = "/"))

data <- data[,c("Site_ID",
                "Station_ID",
                "Collection_year",                 
                "Community",   
                "Plant_species",
                "Cover", 
                "In_out",                                 
                "Latitude",                            
                "Longitude",
                "Basin")]

data <- data %>% filter(!(is.na(Community) | Community == "" | is.na(In_out) | In_out == ""))

tmp <- data %>%
  mutate(Cover_raw = as.character(Cover),
         Cover_num = suppressWarnings(as.numeric(Cover_raw))) %>%
  filter(In_out != "Out", is.na(Cover_num), !is.na(Cover_raw), Cover_raw != "") %>%
  count(Cover_raw, sort = TRUE)
tmp

data <- data %>%
  mutate(
    Cover_raw = as.character(Cover),
    Cover_num = suppressWarnings(as.numeric(Cover_raw)),
    Cover_num = if_else(In_out == "Out", 0, Cover_num),
    Cover_num = if_else(In_out != "Out" & is.na(Cover_num) & !is.na(Cover_raw) & Cover_raw != "", 1, Cover_num),
    Cover = Cover_num
  ) %>%
  dplyr::select(-Cover_raw, -Cover_num)

write.csv(data, file = "data.csv", row.names = FALSE)

# supplement the Phragmites cover data
all_columns <- colnames(data)
grouped_data <- unique(data[c("Station_ID", "Collection_year")])
result_list <- list()

for (i in seq_len(nrow(grouped_data))) {
  station_id <- grouped_data$Station_ID[i]
  collection_year <- grouped_data$Collection_year[i]
  group_data <- data[data$Station_ID == station_id & data$Collection_year == collection_year, ]
  if (!any(group_data$Plant_species == "Phragmites australis")) {
    new_row <- group_data[1, ]
    new_row$Plant_species <- "Phragmites australis"
    new_row$Cover <- 0
    group_data <- rbind(group_data, new_row)
  }
  result_list[[i]] <- group_data
}

data <- do.call(rbind, result_list)
data_all <- do.call(rbind, result_list)
write.csv(data, file = "data.csv", row.names = FALSE)

data_phragmites <- data[data$Plant_species == "Phragmites australis", ]
write.csv(data_phragmites, file = "data_phragmites.csv", row.names = FALSE)

data_phragmites_site <- data_phragmites %>%
  group_by(Site_ID, Collection_year) %>%
  mutate(Cover_site = mean(Cover, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()
write.csv(data_phragmites_site, file = "data_phragmites_site.csv", row.names = FALSE)

data_phragmites$Collection_year <- as.numeric(data_phragmites$Collection_year)
data_phragmites$Basin <- as.factor(data_phragmites$Basin)

# count database information
site_counts_by_basin <- data_phragmites %>%
  group_by(Basin) %>%
  summarise(total_sites = n_distinct(Site_ID), non_zero_sites = n_distinct(Site_ID[Cover != 0]))
print(site_counts_by_basin)
station_counts_by_basin <- data_phragmites %>%
  group_by(Basin) %>%
  summarise(total_stations = n_distinct(Station_ID), non_zero_stations = n_distinct(Station_ID[Cover != 0]))
print(station_counts_by_basin)
##################################################
##################################################









 
### fit model for all basin Phragmites cover ###

data_phragmites_beta <- data_phragmites %>%
  mutate(
    Cover_prop = Cover / 100,
    Cover_prop = if_else(Cover_prop >= 1, 0.999999, Cover_prop)  # 只处理 100% cover
  )

sum(data_phragmites_beta$Cover_prop == 0, na.rm = TRUE)
sum(data_phragmites_beta$Cover_prop >= 1, na.rm = TRUE)
range(data_phragmites_beta$Cover_prop, na.rm = TRUE)

# 2) zero-inflated beta GLMM
data_phragmites_beta <- data_phragmites_beta %>%
  mutate(Year_c = Collection_year - 2006)

model_zibeta <- glmmTMB(
  Cover_prop ~ Year_c * Basin + (1 | Basin:Site_ID),
  ziformula = ~1,
  family = beta_family(link="logit"),
  data = data_phragmites_beta
)

summary(model_zibeta)
car::Anova(model_zibeta, type = 2)

# post-hoc analysis for all phrag data
model_year <- lmer(Cover ~ Collection_year + (1|Basin:Site_ID), data = data_phragmites)
Anova(model_year)

kruskal_result <- kruskal(data_phragmites$Cover, data_phragmites$Basin, group = TRUE)
print(kruskal_result$groups)
kruskal_test <- kruskal.test(Cover ~ Basin, data = data_phragmites)
print(kruskal_test)

unique_sites <- unique(data_phragmites$Station_ID)
length(unique_sites)
unique_sites <- unique(data_phragmites$Site_ID)
length(unique_sites)
##################################################
##################################################









### Figure S1 ###

# Figure S1B
data_phragmites$Cover <- as.numeric(data_phragmites$Cover)
data_phragmites$Collection_year <- as.numeric(data_phragmites$Collection_year)
data_phragmites$Basin <- as.factor(data_phragmites$Basin)
data_phragmites_site$Basin <- factor(data_phragmites_site$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p <- ggplot(data_phragmites, aes(x = Basin, y = Cover)) + 
  stat_summary(fun = "mean", geom = "col", fill = "grey", color = "grey", width = 0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, color = "black") +
  theme_bw() + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(colour = "black", size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        title = element_text(size = 10),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5)) +
  labs(x = "Basins", y = "Phragmites cover (%)", x = NULL) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p
ggsave('./Figure S1B.pdf', p, height = 100, width = 150, units = c("mm"))

# Figure S1A
Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
phrag_mean_station <- data_phragmites %>%
  group_by(Station_ID) %>%
  summarise(
    Longitude  = mean(as.numeric(Longitude), na.rm = TRUE),
    Latitude   = mean(as.numeric(Latitude),  na.rm = TRUE),
    Cover_mean = mean(Cover, na.rm = TRUE),
    .groups = "drop"
  )

data_sf <- st_as_sf(phrag_mean_station, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  arrange(Cover_mean)

p <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover_mean), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites cover (%)") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (° N)") + ylab("Latitude (° W)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -91.35, y = 29.5, xend = -91.35, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.35, y = 30.2, label = "Atchafalaya", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black", fontface = "bold")
p
ggsave('./Figure S1A - Map.pdf', p, height = 100, width = 200, units = c("mm"))

# Figure S1C
df_site_year <- data_phragmites %>%
  group_by(Collection_year, Site_ID) %>%
  summarise(MeanCover_site = mean(Cover, na.rm = TRUE),    
            SdCover_site   = sd(Cover, na.rm = TRUE),
            N_station      = n(),
            .groups        = "drop") %>%
  mutate(SE_site = SdCover_site / sqrt(N_station))
df_year <- df_site_year %>%
  group_by(Collection_year) %>%
  summarise(MeanCover_all = mean(MeanCover_site, na.rm = TRUE),
            SdCover_all   = sd(MeanCover_site, na.rm = TRUE), 
            N_site        = n(),
            .groups       = "drop") %>%
  mutate(SE_all = SdCover_all / sqrt(N_site))

p_all <- ggplot(df_year, aes(x = Collection_year, y = MeanCover_all)) +
  geom_line(color = "#2E2E2E", size = 0.75) +
  geom_point(color = "#2E2E2E", size = 2.5) +
  geom_errorbar(aes(ymin = MeanCover_all - SE_all, ymax = MeanCover_all + SE_all), width = 0, color = "#2E2E2E", size = 0.75) +
  labs(x = "Year", y = ""~italic(Phragmites)~" cover (%)") +
  theme_minimal() +
  theme(plot.title = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  geom_text(data = data.frame(x = 2006, y = 4.25, label = "All basins"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0) +
  geom_text(data = data.frame(x = 2023, y = 4.25, label = "A"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0)
print(p_all)
ggsave("./Figure - All basins.pdf", p_all, height = 75, width = 150, units = "mm")
##################################################
##################################################










### Figure 2 ###

# all basin map
setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation")
Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/state.shp")
Basin_map <- st_transform(Basin_map, crs = 4326)
Basin_map <- Basin_map %>% filter(name == "Louisiana")
Basin_map
df_rect <- data.frame(xmin = -94.2,
                      xmax = -88.5,
                      ymin = 28.5,
                      ymax = 30.5)
p <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 0.3) +
  coord_sf(xlim   = c(-94.5, -88), 
           ylim   = c(28, 33.5), 
           expand = FALSE, 
           datum  = NA) +
  theme_minimal() +
  theme(panel.grid       = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.line        = element_blank(),
        panel.border     = element_rect(color = "black", fill = NA, linewidth = 1)) +
  geom_rect(data = df_rect,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill  = NA,
            color = "blue",
            size  = 0.5)
p

# Figure 2A - 2006 map
data_phragmites_2006 <- data_phragmites %>% filter(Collection_year == 2006)
cover_means_2006 <- data_phragmites_2006 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2006 = mean(Cover, na.rm = TRUE))
data_phragmites_2006 <- data_phragmites_2006 %>%
  left_join(cover_means_2006, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2006, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 0.65) +
  geom_sf(data = data_sf, aes(fill = Cover_2006), shape = 21, color = "black", size = 3.5, stroke = 0.5) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites cover (%)") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -91.35, y = 29.5, xend = -91.35, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.35, y = 30.2, label = "Atchafalaya", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black", fontface = "bold") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2006"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2006$Cover <- as.numeric(data_phragmites_2006$Cover)
data_phragmites_2006$Collection_year <- as.numeric(data_phragmites_2006$Collection_year)
data_phragmites_2006$Basin <- as.factor(data_phragmites_2006$Basin)
data_phragmites_2006$Basin <- factor(data_phragmites_2006$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))
p2 <- ggplot(data_phragmites_2006, aes(x = Basin, y = Cover)) + 
  stat_summary(fun = "mean", geom = "col", fill = "grey", color = "grey", width = 0.5) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.15, color = "black") +  
  theme_bw() +  
  theme(legend.position = "none",
        axis.text.x     = element_text(colour = "black", size = 5.5, hjust = 0.5),
        axis.text.y     = element_text(size = 5.5),
        axis.title.y    = element_text(size = 5.5),
        panel.border    = element_blank(),
        axis.line       = element_line(colour = "black", size = 0.5),
        panel.grid.major= element_blank(),
        panel.grid.minor= element_blank(),
        axis.line.x     = element_line(color = "black", linewidth = 0.5),
        axis.line.y     = element_line(color = "black", linewidth = 0.5)) +
  labs(y = "Cover (%)", x = NULL) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_x_discrete(labels = c("Calcasieu Sabine"       = "CS",
                              "Mermentau"             = "Me",
                              "Teche Vermilion"       = "TV",
                              "Atchafalaya"           = "At",
                              "Terrebonne"            = "Te",
                              "Barataria"             = "Ba",
                              "Breton Sound"          = "BS",
                              "Mississippi River Delta" = "MD",
                              "Pontchartrain"         = "Po"))
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure 2A - 2006.pdf', p4, height = 100, width = 200, units = c("mm"))

# Figure 2B - 2023
data_phragmites_2023 <- data_phragmites %>% filter(Collection_year == 2023)
cover_means_2023 <- data_phragmites_2023 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2023 = mean(Cover, na.rm = TRUE))
data_phragmites_2023 <- data_phragmites_2023 %>%
  left_join(cover_means_2023, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2023, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 0.65) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 3.5, stroke = 0.5) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites cover (%)") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -91.35, y = 29.5, xend = -91.35, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.35, y = 30.2, label = "Atchafalaya", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black", fontface = "bold") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2023"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2023$Cover <- as.numeric(data_phragmites_2023$Cover)
data_phragmites_2023$Collection_year <- as.numeric(data_phragmites_2023$Collection_year)
data_phragmites_2023$Basin <- as.factor(data_phragmites_2023$Basin)
data_phragmites_2023$Basin <- factor(data_phragmites_2023$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))
p2 <- ggplot(data_phragmites_2023, aes(x = Basin, y = Cover)) + 
  stat_summary(fun = "mean", geom = "col", fill = "grey", color = "grey", width = 0.5) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.15, color = "black") +  
  theme_bw() +  
  theme(legend.position = "none",
        axis.text.x     = element_text(colour = "black", size = 5.5, hjust = 0.5),
        axis.text.y     = element_text(size = 5.5),
        axis.title.y    = element_text(size = 5.5),
        panel.border    = element_blank(),
        axis.line       = element_line(colour = "black", size = 0.5),
        panel.grid.major= element_blank(),
        panel.grid.minor= element_blank(),
        axis.line.x     = element_line(color = "black", linewidth = 0.5),
        axis.line.y     = element_line(color = "black", linewidth = 0.5)) +
  labs(y = "Cover (%)", x = NULL) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_x_discrete(labels = c("Calcasieu Sabine"       = "CS",
                              "Mermentau"             = "Me",
                              "Teche Vermilion"       = "TV",
                              "Atchafalaya"           = "At",
                              "Terrebonne"            = "Te",
                              "Barataria"             = "Ba",
                              "Breton Sound"          = "BS",
                              "Mississippi River Delta" = "MD",
                              "Pontchartrain"         = "Po"))
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
ggsave('./Figure 2A - 2023.pdf', p4, height = 100, width = 200, units = c("mm"))

# Figure 2C-K
# Calcasieu Sabine
df_cal <- data_phragmites %>% filter(Basin == "Calcasieu Sabine")
df_site_year <- df_cal %>% group_by(Collection_year, Site_ID) %>%
  summarise(MeanCover_site = mean(Cover, na.rm = TRUE),    
            SdCover_site   = sd(Cover, na.rm = TRUE),
            N_station      = n(),
            .groups        = "drop") %>%
  mutate(SE_site = SdCover_site / sqrt(N_station))
df_year <- df_site_year %>% group_by(Collection_year) %>%
  summarise(MeanCover_all = mean(MeanCover_site, na.rm = TRUE),
            SdCover_all   = sd(MeanCover_site, na.rm = TRUE),
            N_site        = n(),                      
            .groups       = "drop") %>%
  mutate(SE_all = SdCover_all / sqrt(N_site))

p_Calcasieu_Sabine <- ggplot(df_year, aes(x = Collection_year, y = MeanCover_all)) +
  geom_line(color = "#2E2E2E", size = 0.75) +
  geom_point(color = "#2E2E2E", size = 2.5) +
  geom_errorbar(aes(ymin = MeanCover_all - SE_all, ymax = MeanCover_all + SE_all),
                width = 0, color = "#2E2E2E", size = 0.75) +
  labs(x = "Year",
       y = ""~italic(Phragmites)~" cover (%)") +
  theme_minimal() +
  theme(plot.title = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  geom_text(data = data.frame(x = 2006, y = 8.5, label = "Calcasieu Sabine"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0) +
  geom_text(data = data.frame(x = 2023, y = 8.5, label = "B"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0)
print(p_Calcasieu_Sabine)
ggsave("./Figure - Calcasieu Sabine.pdf", p_Calcasieu_Sabine, height = 75, width = 150, units = "mm")

# Mermentau
df_cal <- data_phragmites %>% filter(Basin == "Mermentau")
df_site_year <- df_cal %>% group_by(Collection_year, Site_ID) %>%
  summarise(MeanCover_site = mean(Cover, na.rm = TRUE),    
            SdCover_site   = sd(Cover, na.rm = TRUE),
            N_station      = n(),
            .groups        = "drop") %>%
  mutate(SE_site = SdCover_site / sqrt(N_station))
df_year <- df_site_year %>%
  group_by(Collection_year) %>%
  summarise(MeanCover_all = mean(MeanCover_site, na.rm = TRUE),
            SdCover_all   = sd(MeanCover_site, na.rm = TRUE),
            N_site        = n(),
            .groups       = "drop") %>%
  mutate(SE_all = SdCover_all / sqrt(N_site))

p_Mermentau <- ggplot(df_year, aes(x = Collection_year, y = MeanCover_all)) +
  geom_line(color = "#2E2E2E", size = 0.75) +
  geom_point(color = "#2E2E2E", size = 2.5) +
  geom_errorbar(aes(ymin = MeanCover_all - SE_all, ymax = MeanCover_all + SE_all),
                width = 0, color = "#2E2E2E", size = 0.75) +
  labs(x = "Year",
       y = ""~italic(Phragmites)~" cover (%)") +
  theme_minimal() +
  theme(plot.title = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  geom_text(data = data.frame(x = 2006, y = 12.5, label = "Mermentau"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0) +
  geom_text(data = data.frame(x = 2023, y = 12.5, label = "C"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0)
print(p_Mermentau)
ggsave("./Figure - Mermentau.pdf", p_Mermentau, height = 75, width = 150, units = "mm")

# Teche Vermilion
df_cal <- data_phragmites %>% filter(Basin == "Teche Vermilion")
df_site_year <- df_cal %>% group_by(Collection_year, Site_ID) %>%
  summarise(MeanCover_site = mean(Cover, na.rm = TRUE),    
            SdCover_site   = sd(Cover, na.rm = TRUE),
            N_station      = n(),
            .groups        = "drop") %>%
  mutate(SE_site = SdCover_site / sqrt(N_station))
df_year <- df_site_year %>% group_by(Collection_year) %>%
  summarise(MeanCover_all = mean(MeanCover_site, na.rm = TRUE),
            SdCover_all   = sd(MeanCover_site, na.rm = TRUE),
            N_site        = n(),
            .groups       = "drop") %>%
  mutate(SE_all = SdCover_all / sqrt(N_site))

p_Teche_Vermilion <- ggplot(df_year, aes(x = Collection_year, y = MeanCover_all)) +
  geom_line(color = "#2E2E2E", size = 0.75) +
  geom_point(color = "#2E2E2E", size = 2.5) +
  geom_errorbar(aes(ymin = MeanCover_all - SE_all, ymax = MeanCover_all + SE_all),
                width = 0, color = "#2E2E2E", size = 0.75) +
  labs(x = "Year",
       y = ""~italic(Phragmites)~" cover (%)") +
  theme_minimal() +
  theme(plot.title = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  geom_text(data = data.frame(x = 2006, y = 5.5, label = "Teche Vermilion"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0) +
  geom_text(data = data.frame(x = 2023, y = 5.5, label = "D"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0)
print(p_Teche_Vermilion)
ggsave("./Figure - Teche Vermilion.pdf", p_Teche_Vermilion, height = 75, width = 150, units = "mm")

# Atchafalaya
df_cal <- data_phragmites %>% filter(Basin == "Atchafalaya")
df_site_year <- df_cal %>% group_by(Collection_year, Site_ID) %>%
  summarise(MeanCover_site = mean(Cover, na.rm = TRUE),    
            SdCover_site   = sd(Cover, na.rm = TRUE),
            N_station      = n(),
            .groups        = "drop") %>%
  mutate(SE_site = SdCover_site / sqrt(N_station))
df_year <- df_site_year %>% group_by(Collection_year) %>%
  summarise(MeanCover_all = mean(MeanCover_site, na.rm = TRUE),
            SdCover_all   = sd(MeanCover_site, na.rm = TRUE),
            N_site        = n(),
            .groups       = "drop") %>%
  mutate(SE_all = SdCover_all / sqrt(N_site))

p_Atchafalaya <- ggplot(df_year, aes(x = Collection_year, y = MeanCover_all)) +
  geom_line(color = "#2E2E2E", size = 0.75) +
  geom_point(color = "#2E2E2E", size = 2.5) +
  geom_errorbar(aes(ymin = MeanCover_all - SE_all, ymax = MeanCover_all + SE_all),
                width = 0, color = "#2E2E2E", size = 0.75) +
  labs(x = "Year", y = ""~italic(Phragmites)~" cover (%)") +
  theme_minimal() +
  theme(plot.title = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  geom_text(data = data.frame(x = 2006, y = 2.75, label = "Atchafalaya"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0) +
  geom_text(data = data.frame(x = 2023, y = 2.75, label = "E"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0)
print(p_Atchafalaya)
ggsave("./Figure - Atchafalaya.pdf", p_Atchafalaya, height = 75, width = 150, units = "mm")

# Terrebonne
df_cal <- data_phragmites %>% filter(Basin == "Terrebonne")
df_site_year <- df_cal %>% group_by(Collection_year, Site_ID) %>%
  summarise(MeanCover_site = mean(Cover, na.rm = TRUE),    
            SdCover_site   = sd(Cover, na.rm = TRUE),
            N_station      = n(),
            .groups        = "drop") %>%
  mutate(SE_site = SdCover_site / sqrt(N_station))
df_year <- df_site_year %>% group_by(Collection_year) %>%
  summarise(MeanCover_all = mean(MeanCover_site, na.rm = TRUE),
            SdCover_all   = sd(MeanCover_site, na.rm = TRUE),
            N_site        = n(),            
            .groups       = "drop") %>%
  mutate(SE_all = SdCover_all / sqrt(N_site))

p_Terrebonne <- ggplot(df_year, aes(x = Collection_year, y = MeanCover_all)) +
  geom_line(color = "#2E2E2E", size = 0.75) +
  geom_point(color = "#2E2E2E", size = 2.5) +
  geom_errorbar(aes(ymin = MeanCover_all - SE_all, ymax = MeanCover_all + SE_all),
                width = 0, color = "#2E2E2E", size = 0.75) +
  labs(x = "Year", y = ""~italic(Phragmites)~" cover (%)") +
  theme_minimal() +
  theme(plot.title = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  geom_text(data = data.frame(x = 2006, y = 2, label = "Terrebonne"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0) +
  geom_text(data = data.frame(x = 2023, y = 2, label = "F"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0)
print(p_Terrebonne)
ggsave("./Figure - Terrebonne.pdf", p_Terrebonne, height = 75, width = 150, units = "mm")

# Barataria
df_cal <- data_phragmites %>% filter(Basin == "Barataria")
df_site_year <- df_cal %>% group_by(Collection_year, Site_ID) %>%
  summarise(MeanCover_site = mean(Cover, na.rm = TRUE),    
            SdCover_site   = sd(Cover, na.rm = TRUE),
            N_station      = n(),
            .groups        = "drop") %>%
  mutate(SE_site = SdCover_site / sqrt(N_station))
df_year <- df_site_year %>% group_by(Collection_year) %>%
  summarise(MeanCover_all = mean(MeanCover_site, na.rm = TRUE),
            SdCover_all   = sd(MeanCover_site, na.rm = TRUE),
            N_site        = n(),
            .groups       = "drop") %>%
  mutate(SE_all = SdCover_all / sqrt(N_site))

p_Barataria <- ggplot(df_year, aes(x = Collection_year, y = MeanCover_all)) +
  geom_line(color = "#2E2E2E", size = 0.75) +
  geom_point(color = "#2E2E2E", size = 2.5) +
  geom_errorbar(aes(ymin = MeanCover_all - SE_all, ymax = MeanCover_all + SE_all),
                width = 0, color = "#2E2E2E", size = 0.75) +
  labs(x = "Year", y = ""~italic(Phragmites)~" cover (%)") +
  theme_minimal() +
  theme(plot.title = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  geom_text(data = data.frame(x = 2006, y = 0.7, label = "Barataria"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0) +
  geom_text(data = data.frame(x = 2023, y = 0.7, label = "G"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0)
print(p_Barataria)
ggsave("./Figure - Barataria.pdf", p_Barataria, height = 75, width = 150, units = "mm")

# Breton Sound
df_cal <- data_phragmites %>% filter(Basin == "Breton Sound")
df_site_year <- df_cal %>% group_by(Collection_year, Site_ID) %>%
  summarise(MeanCover_site = mean(Cover, na.rm = TRUE),    
            SdCover_site   = sd(Cover, na.rm = TRUE),
            N_station      = n(),
            .groups        = "drop") %>%
  mutate(SE_site = SdCover_site / sqrt(N_station))
df_year <- df_site_year %>% group_by(Collection_year) %>%
  summarise(MeanCover_all = mean(MeanCover_site, na.rm = TRUE),
            SdCover_all   = sd(MeanCover_site, na.rm = TRUE),
            N_site        = n(),
            .groups       = "drop") %>%
  mutate(SE_all = SdCover_all / sqrt(N_site))

p_Breton_Sound <- ggplot(df_year, aes(x = Collection_year, y = MeanCover_all)) +
  geom_line(color = "#2E2E2E", size = 0.75) +
  geom_point(color = "#2E2E2E", size = 2.5) +
  geom_errorbar(aes(ymin = MeanCover_all - SE_all, ymax = MeanCover_all + SE_all),
                width = 0, color = "#2E2E2E", size = 0.75) +
  labs(x = "Year", y = ""~italic(Phragmites)~" cover (%)") +
  theme_minimal() +
  theme(plot.title = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  geom_text(data = data.frame(x = 2006, y = 7.5, label = "Breton Sound"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0) +
  geom_text(data = data.frame(x = 2023, y = 7.5, label = "H"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0)
print(p_Breton_Sound)
ggsave("./Figure - Breton_Sound.pdf", p_Breton_Sound, height = 75, width = 150, units = "mm")

# Mississippi River Delta
df_cal <- data_phragmites %>% filter(Basin == "Mississippi River Delta")
df_site_year <- df_cal %>% group_by(Collection_year, Site_ID) %>%
  summarise(MeanCover_site = mean(Cover, na.rm = TRUE),    
            SdCover_site   = sd(Cover, na.rm = TRUE),
            N_station      = n(),
            .groups        = "drop") %>%
  mutate(SE_site = SdCover_site / sqrt(N_station))
df_year <- df_site_year %>% group_by(Collection_year) %>%
  summarise(MeanCover_all = mean(MeanCover_site, na.rm = TRUE),
            SdCover_all   = sd(MeanCover_site, na.rm = TRUE),
            N_site        = n(),
            .groups       = "drop") %>% 
  mutate(SE_all = SdCover_all / sqrt(N_site))

p_Mississippi_River_Delta <- ggplot(df_year, aes(x = Collection_year, y = MeanCover_all)) +
  geom_line(color = "#2E2E2E", size = 0.75) +
  geom_point(color = "#2E2E2E", size = 2.5) +
  geom_errorbar(aes(ymin = MeanCover_all - SE_all, ymax = MeanCover_all + SE_all),
                width = 0, color = "#2E2E2E", size = 0.75) +
  labs(x = "Year", y = ""~italic(Phragmites)~" cover (%)") +
  theme_minimal() +
  theme(plot.title = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  geom_text(data = data.frame(x = 2006, y = 85, label = "Mississippi River Delta"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0) +
  geom_text(data = data.frame(x = 2023, y = 85, label = "I"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0)
print(p_Mississippi_River_Delta)
ggsave("./Figure - Mississippi River Delta.pdf", p_Mississippi_River_Delta, height = 75, width = 150, units = "mm")

# Pontchartrain
df_cal <- data_phragmites %>% filter(Basin == "Pontchartrain")
df_site_year <- df_cal %>% group_by(Collection_year, Site_ID) %>%
  summarise(MeanCover_site = mean(Cover, na.rm = TRUE), 
            SdCover_site   = sd(Cover, na.rm = TRUE),
            N_station      = n(),
            .groups        = "drop") %>%
  mutate(SE_site = SdCover_site / sqrt(N_station))
df_year <- df_site_year %>% group_by(Collection_year) %>%
  summarise(MeanCover_all = mean(MeanCover_site, na.rm = TRUE),
            SdCover_all   = sd(MeanCover_site, na.rm = TRUE),
            N_site        = n(),
            .groups       = "drop") %>%
  mutate(SE_all = SdCover_all / sqrt(N_site))

p_Pontchartrain <- ggplot(df_year, aes(x = Collection_year, y = MeanCover_all)) +
  geom_line(color = "#2E2E2E", size = 0.75) +
  geom_point(color = "#2E2E2E", size = 2.5) +
  geom_errorbar(aes(ymin = MeanCover_all - SE_all, ymax = MeanCover_all + SE_all),
                width = 0, color = "#2E2E2E", size = 0.75) +
  labs(x = "Year", y = ""~italic(Phragmites)~" cover (%)") +
  theme_minimal() +
  theme(plot.title = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")) +
  geom_text(data = data.frame(x = 2006, y = 2, label = "Pontchartrain"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0) +
  geom_text(data = data.frame(x = 2023, y = 2, label = "J"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 5,
            inherit.aes = FALSE,
            hjust = 0)
print(p_Pontchartrain)
ggsave("./Figure - Pontchartrain.pdf", p_Pontchartrain, height = 75, width = 150, units = "mm")

fig_2x5 <- grid.arrange(p_all, 
                        p_Calcasieu_Sabine, 
                        p_Mermentau, 
                        p_Teche_Vermilion, 
                        p_Atchafalaya,
                        p_Terrebonne, 
                        p_Barataria, 
                        p_Breton_Sound, 
                        p_Mississippi_River_Delta, 
                        p_Pontchartrain,
                        ncol = 2,
                        nrow = 5)
ggsave(filename = "All_Figures_2x5.pdf",
       plot     = fig_2x5,
       width    = 300,
       height   = 400,
       units    = "mm")
##################################################
##################################################








# Figure 3 - Phragmites expansion trend ###

# calculate Phragmites zr
data_phragmites$Collection_year <- as.numeric(data_phragmites$Collection_year)
data_phragmites$Cover <- as.numeric(data_phragmites$Cover)
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
data_phragmites_zr  <- data_phragmites %>%
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
      mutate(pr.phragmites = pr_value, zr.phragmites = zr_value, var.phragmites = var_value, n.phragmites = n_value)
    return(.x)
  }) %>%
  ungroup()

data_phragmites_zr <- data_phragmites_zr %>% filter(n.phragmites > 3)
data_phragmites_zr <- data_phragmites_zr %>% group_by(Station_ID) %>% slice(1) %>% ungroup()
data_phragmites_zr <- data_phragmites_zr %>% filter(!is.na(zr.phragmites))

unique_site_count <- n_distinct(data_phragmites_zr$Site_ID)
print(unique_site_count)
unique_site_count <- n_distinct(data_phragmites_zr$Station_ID)
print(unique_site_count)
non_zero_zr_by_basin <- tapply(data_phragmites_zr$zr.phragmites, data_phragmites_zr$Basin, function(x) sum(x != 0))
print(non_zero_zr_by_basin)
write.csv(data_phragmites_zr, file = "data_phragmites_zr.csv", row.names = FALSE)

rm <- rma.mv(zr.phragmites, var.phragmites,
             mods = ~ 1,
             random = list( ~ 1 | Site_ID),
             method = "REML",
             data = data_phragmites_zr)
rm
rm1 <- rma.mv(zr.phragmites, var.phragmites,
              mods = ~ Basin - 1,
              random = list( ~ 1 | Site_ID),
              method = "REML",
              data = data_phragmites_zr)
rm1

# expansion map
Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_zr, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(zr.phragmites)

p <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 0.65) +
  geom_sf(data = data_sf, aes(fill = zr.phragmites), shape = 21, color = "black", size = 3.5, stroke = 0.5) +
  scale_fill_gradient2(low = "#0000CD",
                       mid = "#FFFFFF",
                       high = "#8B0000",
                       midpoint = 0, limits = c(-2, 2), name = "Phragmites zr") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (° N)") + ylab("Latitude (° W)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -91.35, y = 29.5, xend = -91.35, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.35, y = 30.2, label = "Atchafalaya", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black", fontface = "bold") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black", fontface = "bold")
p
ggsave('./Figure 3A - Map.pdf', p, height = 100, width = 200, units = c("mm"))
data_phragmites_zr$Longitude <- as.numeric(data_phragmites_zr$Longitude)
data_phragmites_zr$zr.phragmites <- as.numeric(data_phragmites_zr$zr.phragmites)

# Figure 3B
dat1 <- data.frame(coef = coef(rm), 
                   se = sqrt(diag(vcov(rm))),
                   Type = c("All basin"))
dat1$lower <- dat1$coef - 1.96 * dat1$se
dat1$upper <- dat1$coef + 1.96 * dat1$se
dat1
dat2 <- data.frame(coef = coef(rm1), 
                   se = sqrt(diag(vcov(rm1))),
                   Type = c("Atchafalaya",
                            "Barataria",
                            "Breton Sound",
                            "Calcasieu Sabine",
                            "Mermentau",
                            "Mississippi River Delta",
                            "Pontchartrain",
                            "Teche Vermilion",
                            "Terrebonne"))
dat2$lower <- dat2$coef - 1.96 * dat2$se
dat2$upper <- dat2$coef + 1.96 * dat2$se
dat2
dat <- rbind(dat1, dat2)
dat$Type <- as.character(dat$Type)
dat$Type <- factor(dat$Type, levels = c("Pontchartrain",
                                        "Mississippi River Delta",
                                        "Breton Sound",
                                        "Barataria",
                                        "Terrebonne",
                                        "Atchafalaya",
                                        "Teche Vermilion",
                                        "Mermentau",
                                        "Calcasieu Sabine",
                                        "All basin"))
p <- ggplot(dat, aes(x = coef, y = Type)) +
  geom_errorbarh(mapping = aes(xmin = lower, xmax = upper), 
                 width = 0, size = 1, colour = "grey10", height = 0.2) +
  geom_point(size = 2, shape = 21, color = "grey10", fill = "grey", stroke = 1) +
  xlim(c(-1.5, 1.5)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "steelblue", size = 1) +
  theme_classic() +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        axis.text.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.text.y = element_text(size = 10, hjust = 1),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "none") +
  scale_y_discrete(limits = rev(levels(dat$Type))) +
  xlab("Phragmites expansion rate (zr)") +
  ylab("Basins") +
  coord_flip()
p
ggsave('./Figure 3B.pdf', p, height = 100, width = 150, units = c("mm"))

basin_counts <- data_phragmites_zr %>%
  group_by(Basin) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

print(basin_counts)
unique_count <- length(unique(data_phragmites_zr$Site_ID))
print(unique_count)
non_zero_count <- sum(data_phragmites_zr$zr.phragmites != 0, na.rm = TRUE)
print(non_zero_count)
unique_sites <- unique(data_phragmites_zr$Site_ID)
length(unique_sites)
unique_sites <- unique(data_phragmites_zr$zr.phragmites)
length(unique_sites)
##################################################
##################################################