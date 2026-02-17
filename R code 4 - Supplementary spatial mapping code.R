###################################################
##### Guo et al., 2026 - Phragmites expansion #####
###################################################

### R code 4 - Supplementary spatial mapping code ###

# Figure S2 - Whole map
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

# Figure S2 - 2006
data_phragmites_2006 <- data_phragmites %>% filter(Collection_year == 2006)
cover_means_2006 <- data_phragmites_2006 %>% group_by(Site_ID) %>%
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
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(color = Cover), shape = 1, size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2006.pdf', p4, height = 100, width = 150, units = c("mm"))



# Figure S2 - 2007
data_phragmites_2007 <- data_phragmites %>% filter(Collection_year == 2007)
cover_means_2007 <- data_phragmites_2007 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2007 = mean(Cover, na.rm = TRUE))
data_phragmites_2007 <- data_phragmites_2007 %>%
  left_join(cover_means_2007, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2007, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2007"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2007$Cover <- as.numeric(data_phragmites_2007$Cover)
data_phragmites_2007$Collection_year <- as.numeric(data_phragmites_2007$Collection_year)
data_phragmites_2007$Basin <- as.factor(data_phragmites_2007$Basin)
data_phragmites_2007$Basin <- factor(data_phragmites_2007$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2007, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2007.pdf', p4, height = 100, width = 150, units = c("mm"))



# Figure S2 - 2008
data_phragmites_2008 <- data_phragmites %>% filter(Collection_year == 2008)
cover_means_2008 <- data_phragmites_2008 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2008 = mean(Cover, na.rm = TRUE))
data_phragmites_2008 <- data_phragmites_2008 %>%
  left_join(cover_means_2008, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2008, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +  # 绘制边界
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) + # 填充颜色，黑色外圈
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2008"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)

p1

data_phragmites_2008$Cover <- as.numeric(data_phragmites_2008$Cover)
data_phragmites_2008$Collection_year <- as.numeric(data_phragmites_2008$Collection_year)
data_phragmites_2008$Basin <- as.factor(data_phragmites_2008$Basin)
data_phragmites_2008$Basin <- factor(data_phragmites_2008$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2008, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,  
                             height = 0.17)
p4
ggsave('./Figure S1 - 2008.pdf', p4, height = 100, width = 150, units = c("mm"))
p2008 <- p4



# Figure S2 - 2009
data_phragmites_2009 <- data_phragmites %>% filter(Collection_year == 2009)
cover_means_2009 <- data_phragmites_2009 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2009 = mean(Cover, na.rm = TRUE))
data_phragmites_2009 <- data_phragmites_2009 %>%
  left_join(cover_means_2009, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2009, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2009"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2009$Cover <- as.numeric(data_phragmites_2009$Cover)
data_phragmites_2009$Collection_year <- as.numeric(data_phragmites_2009$Collection_year)
data_phragmites_2009$Basin <- as.factor(data_phragmites_2009$Basin)
data_phragmites_2009$Basin <- factor(data_phragmites_2009$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2009, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2009.pdf', p4, height = 100, width = 150, units = c("mm"))



# Figure S2 - 2010
data_phragmites_2010 <- data_phragmites %>% filter(Collection_year == 2010)
cover_means_2010 <- data_phragmites_2010 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2010 = mean(Cover, na.rm = TRUE))
data_phragmites_2010 <- data_phragmites_2010 %>%
  left_join(cover_means_2010, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2010, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2010"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2010$Cover <- as.numeric(data_phragmites_2010$Cover)
data_phragmites_2010$Collection_year <- as.numeric(data_phragmites_2010$Collection_year)
data_phragmites_2010$Basin <- as.factor(data_phragmites_2010$Basin)
data_phragmites_2010$Basin <- factor(data_phragmites_2010$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))
p2 <- ggplot(data_phragmites_2010, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2010.pdf', p4, height = 100, width = 150, units = c("mm"))



# Figure S2 - 2011
data_phragmites_2011 <- data_phragmites %>% filter(Collection_year == 2011)
cover_means_2011 <- data_phragmites_2011 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2011 = mean(Cover, na.rm = TRUE))
data_phragmites_2011 <- data_phragmites_2011 %>%
  left_join(cover_means_2011, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2011, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +  # 绘制边界
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) + # 填充颜色，黑色外圈
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2011"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2011$Cover <- as.numeric(data_phragmites_2011$Cover)
data_phragmites_2011$Collection_year <- as.numeric(data_phragmites_2011$Collection_year)
data_phragmites_2011$Basin <- as.factor(data_phragmites_2011$Basin)
data_phragmites_2011$Basin <- factor(data_phragmites_2011$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2011, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2011.pdf', p4, height = 100, width = 150, units = c("mm"))



# Figure S2 - 2012
data_phragmites_2012 <- data_phragmites %>% filter(Collection_year == 2012)
cover_means_2012 <- data_phragmites_2012 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2012 = mean(Cover, na.rm = TRUE))
data_phragmites_2012 <- data_phragmites_2012 %>%
  left_join(cover_means_2012, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2012, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2012"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2012$Cover <- as.numeric(data_phragmites_2012$Cover)
data_phragmites_2012$Collection_year <- as.numeric(data_phragmites_2012$Collection_year)
data_phragmites_2012$Basin <- as.factor(data_phragmites_2012$Basin)
data_phragmites_2012$Basin <- factor(data_phragmites_2012$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2012, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2012.pdf', p4, height = 100, width = 150, units = c("mm"))



# Figure S2 - 2013
data_phragmites_2013 <- data_phragmites %>% filter(Collection_year == 2013)
cover_means_2013 <- data_phragmites_2013 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2013 = mean(Cover, na.rm = TRUE))
data_phragmites_2013 <- data_phragmites_2013 %>%
  left_join(cover_means_2013, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2013, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2013"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2013$Cover <- as.numeric(data_phragmites_2013$Cover)
data_phragmites_2013$Collection_year <- as.numeric(data_phragmites_2013$Collection_year)
data_phragmites_2013$Basin <- as.factor(data_phragmites_2013$Basin)
data_phragmites_2013$Basin <- factor(data_phragmites_2013$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2013, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2013.pdf', p4, height = 100, width = 150, units = c("mm"))



# Figure S2 - 2014
data_phragmites_2014 <- data_phragmites %>% filter(Collection_year == 2014)
cover_means_2014 <- data_phragmites_2014 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2014 = mean(Cover, na.rm = TRUE))
data_phragmites_2014 <- data_phragmites_2014 %>%
  left_join(cover_means_2014, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2014, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +  # 绘制边界
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) + # 填充颜色，黑色外圈
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2014"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2014$Cover <- as.numeric(data_phragmites_2014$Cover)
data_phragmites_2014$Collection_year <- as.numeric(data_phragmites_2014$Collection_year)
data_phragmites_2014$Basin <- as.factor(data_phragmites_2014$Basin)
data_phragmites_2014$Basin <- factor(data_phragmites_2014$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2014, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2014.pdf', p4, height = 100, width = 150, units = c("mm"))



# Figure S2 - 2015
data_phragmites_2015 <- data_phragmites %>% filter(Collection_year == 2015)
cover_means_2015 <- data_phragmites_2015 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2015 = mean(Cover, na.rm = TRUE))
data_phragmites_2015 <- data_phragmites_2015 %>%
  left_join(cover_means_2015, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2015, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2015"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2015$Cover <- as.numeric(data_phragmites_2015$Cover)
data_phragmites_2015$Collection_year <- as.numeric(data_phragmites_2015$Collection_year)
data_phragmites_2015$Basin <- as.factor(data_phragmites_2015$Basin)
data_phragmites_2015$Basin <- factor(data_phragmites_2015$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2015, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,  
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2015.pdf', p4, height = 100, width = 150, units = c("mm"))



# Figure S2 - 2016
data_phragmites_2016 <- data_phragmites %>% filter(Collection_year == 2016)
cover_means_2016 <- data_phragmites_2016 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2016 = mean(Cover, na.rm = TRUE))
data_phragmites_2016 <- data_phragmites_2016 %>%
  left_join(cover_means_2016, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2016, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +  # 绘制边界
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) + # 填充颜色，黑色外圈
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2016"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2016$Cover <- as.numeric(data_phragmites_2016$Cover)
data_phragmites_2016$Collection_year <- as.numeric(data_phragmites_2016$Collection_year)
data_phragmites_2016$Basin <- as.factor(data_phragmites_2016$Basin)
data_phragmites_2016$Basin <- factor(data_phragmites_2016$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2016, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2016.pdf', p4, height = 100, width = 150, units = c("mm"))
p2016 <- p4



# Figure S2 - 2017
data_phragmites_2017 <- data_phragmites %>% filter(Collection_year == 2017)
cover_means_2017 <- data_phragmites_2017 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2017 = mean(Cover, na.rm = TRUE))
data_phragmites_2017 <- data_phragmites_2017 %>%
  left_join(cover_means_2017, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2017, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2017"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2017$Cover <- as.numeric(data_phragmites_2017$Cover)
data_phragmites_2017$Collection_year <- as.numeric(data_phragmites_2017$Collection_year)
data_phragmites_2017$Basin <- as.factor(data_phragmites_2017$Basin)
data_phragmites_2017$Basin <- factor(data_phragmites_2017$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2017, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2017.pdf', p4, height = 100, width = 150, units = c("mm"))
p2017 <- p4



# Figure S2 - 2018
data_phragmites_2018 <- data_phragmites %>% filter(Collection_year == 2018)
cover_means_2018 <- data_phragmites_2018 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2018 = mean(Cover, na.rm = TRUE))
data_phragmites_2018 <- data_phragmites_2018 %>%
  left_join(cover_means_2018, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2018, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2018"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2018$Cover <- as.numeric(data_phragmites_2018$Cover)
data_phragmites_2018$Collection_year <- as.numeric(data_phragmites_2018$Collection_year)
data_phragmites_2018$Basin <- as.factor(data_phragmites_2018$Basin)
data_phragmites_2018$Basin <- factor(data_phragmites_2018$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2018, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2018.pdf', p4, height = 100, width = 150, units = c("mm"))
p2018 <- p4



# Figure S2 - 2019
data_phragmites_2019 <- data_phragmites %>% filter(Collection_year == 2019)
cover_means_2019 <- data_phragmites_2019 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2019 = mean(Cover, na.rm = TRUE))
data_phragmites_2019 <- data_phragmites_2019 %>%
  left_join(cover_means_2019, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2019, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2019"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2019$Cover <- as.numeric(data_phragmites_2019$Cover)
data_phragmites_2019$Collection_year <- as.numeric(data_phragmites_2019$Collection_year)
data_phragmites_2019$Basin <- as.factor(data_phragmites_2019$Basin)
data_phragmites_2019$Basin <- factor(data_phragmites_2019$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2019, aes(x = Basin, y = Cover)) + 
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
  scale_x_discrete(
    labels = c("Calcasieu Sabine"       = "CS",
               "Mermentau"             = "Me",
               "Teche Vermilion"       = "TV",
               "Atchafalaya"           = "At",
               "Terrebonne"            = "Te",
               "Barataria"             = "Ba",
               "Breton Sound"          = "BS",
               "Mississippi River Delta" = "MD",
               "Pontchartrain"         = "Po"))
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,  
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2019.pdf', p4, height = 100, width = 150, units = c("mm"))
p2019 <- p4



# Figure S2 - 2020
data_phragmites_2020 <- data_phragmites %>% filter(Collection_year == 2020)
cover_means_2020 <- data_phragmites_2020 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2020 = mean(Cover, na.rm = TRUE))
data_phragmites_2020 <- data_phragmites_2020 %>%
  left_join(cover_means_2020, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2020, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2020"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2020$Cover <- as.numeric(data_phragmites_2020$Cover)
data_phragmites_2020$Collection_year <- as.numeric(data_phragmites_2020$Collection_year)
data_phragmites_2020$Basin <- as.factor(data_phragmites_2020$Basin)
data_phragmites_2020$Basin <- factor(data_phragmites_2020$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2020, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2020.pdf', p4, height = 100, width = 150, units = c("mm"))

# Figure S2 - 2021
data_phragmites_2021 <- data_phragmites %>% filter(Collection_year == 2021)
cover_means_2021 <- data_phragmites_2021 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2021 = mean(Cover, na.rm = TRUE))
data_phragmites_2021 <- data_phragmites_2021 %>%
  left_join(cover_means_2021, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2021, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2021"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2021$Cover <- as.numeric(data_phragmites_2021$Cover)
data_phragmites_2021$Collection_year <- as.numeric(data_phragmites_2021$Collection_year)
data_phragmites_2021$Basin <- as.factor(data_phragmites_2021$Basin)
data_phragmites_2021$Basin <- factor(data_phragmites_2021$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2021, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2021.pdf', p4, height = 100, width = 150, units = c("mm"))
p2021 <- p4



# Figure S2 - 2022
data_phragmites_2022 <- data_phragmites %>% filter(Collection_year == 2022)
cover_means_2022 <- data_phragmites_2022 %>%
  group_by(Site_ID) %>%
  summarise(Cover_2022 = mean(Cover, na.rm = TRUE))
data_phragmites_2022 <- data_phragmites_2022 %>%
  left_join(cover_means_2022, by = "Site_ID")

Basin_map <- st_read("/Users/yaolin/Desktop/Postdoc in LSU/Project - Phragmites/Data/CRMS_Marsh_Vegetation/BasinsCoastal.shp")
st_crs(Basin_map) <- 3857
Basin_map <- st_transform(Basin_map, crs = 4326)
data_sf <- st_as_sf(data_phragmites_2022, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- data_sf %>% arrange(Cover)
plot(Basin_map)

p1 <- ggplot() +
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) +
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
  geom_text(data = data.frame(x = -88.75, y = 30.75, label = "2022"),
            aes(x = x, y = y, label = label),
            color = "black", fontface = "bold", size = 4,
            inherit.aes = FALSE,
            hjust = 0)
p1

data_phragmites_2022$Cover <- as.numeric(data_phragmites_2022$Cover)
data_phragmites_2022$Collection_year <- as.numeric(data_phragmites_2022$Collection_year)
data_phragmites_2022$Basin <- as.factor(data_phragmites_2022$Basin)
data_phragmites_2022$Basin <- factor(data_phragmites_2022$Basin,
                                     levels = c("Calcasieu Sabine",
                                                "Mermentau",
                                                "Teche Vermilion",
                                                "Atchafalaya",
                                                "Terrebonne",
                                                "Barataria",
                                                "Breton Sound",
                                                "Mississippi River Delta",
                                                "Pontchartrain"))

p2 <- ggplot(data_phragmites_2022, aes(x = Basin, y = Cover)) + 
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2022.pdf', p4, height = 100, width = 150, units = c("mm"))

# Figure S2 - 2023
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
  geom_sf(data = Basin_map, fill = NA, color = "black", linewidth = 1) +  # 绘制边界
  geom_sf(data = data_sf, aes(fill = Cover), shape = 21, color = "black", size = 2, stroke = 1) + # 填充颜色，黑色外圈
  scale_fill_gradient2(low = "#0000CD", mid = "#FFFFFF", high = "#8B0000", 
                       midpoint = 0, limits = c(0, 100), name = "Phragmites Cover") +
  coord_sf(xlim = c(-94.5, -88), ylim = c(28, 31), expand = FALSE) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  annotate("segment", x = -93.5, y = 29.85, xend = -93.5, yend = 29.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -93.5, y = 29.4, label = "Calcasieu Sabine", size = 3, color = "black") +
  annotate("segment", x = -92.5, y = 29.75, xend = -92.5, yend = 30.25, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92.5, y = 30.35, label = "Mermentau", size = 3, color = "black") +
  annotate("segment", x = -92, y = 29.6, xend = -92, yend = 29.2, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -92, y = 29.1, label = "Teche Vermilion", size = 3, color = "black") +
  annotate("segment", x = -91.45, y = 29.5, xend = -91.45, yend = 30.1, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91.58, y = 30.2, label = "Atchafalaya", size = 3, color = "black") +
  annotate("segment", x = -91, y = 29.25, xend = -91, yend = 28.85, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -91, y = 28.75, label = "Terrebonne", size = 3, color = "black") +
  annotate("segment", x = -90, y = 29.25, xend = -90, yend = 28.65, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90, y = 28.55, label = "Barataria", size = 3, color = "black") +
  annotate("segment", x = -89.5, y = 29.6, xend = -89.5, yend = 30.5, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.5, y = 30.6, label = "Breton Sound", size = 3, color = "black") +
  annotate("segment", x = -89.25, y = 29.1, xend = -89.25, yend = 28.4, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -89.25, y = 28.3, label = "Mississippi River Delta ", size = 3, color = "black") +
  annotate("segment", x = -90.5, y = 30.25, xend = -90.5, yend = 30.75, 
           color = "black", linewidth = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -90.5, y = 30.85, label = "Pontchartrain", size = 3, color = "black") +
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
p2
p3 <- ggdraw(p1) + draw_plot(p,
                             x = 0.023,
                             y = 0.64,
                             width = 0.28,
                             height = 0.18)
p3
p4 <- ggdraw(p3) + draw_plot(p2,
                             x = 0.11,
                             y = 0.28,
                             width = 0.29,
                             height = 0.17)
p4
ggsave('./Figure S1 - 2023.pdf', p4, height = 100, width = 150, units = c("mm"))

figS1 <- plot_grid(p2006, 
                   p2007, 
                   p2008,
                   p2009, 
                   p2010, 
                   p2011,
                   p2012, 
                   p2013, 
                   p2014,
                   p2015, 
                   p2016, 
                   p2017,
                   p2018, 
                   p2019, 
                   p2020,
                   p2021, 
                   p2022, 
                   p2023,
                   ncol = 3,
                   nrow = 6,
                   align = "hv",
                   rel_heights = c(1, 1),
                   rel_widths  = c(1, 1, 1), 
                   labels = NULL,
                   label_size = 12,
                   label_x = 0.5, 
                   label_y = 1,
                   hjust = 0.5, 
                   vjust = 0.5)

ggsave(filename = "Figure S2.pdf",
       plot     = figS1,
       width    = 450,
       height   = 600,
       units    = "mm")