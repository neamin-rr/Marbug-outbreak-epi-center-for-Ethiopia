# ============================================================
# 1. LIBRARIES & DATA PREPARATION
# ============================================================
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(cowplot)
library(ggnewscale)
library(grid)

# Load data
ethiopia <- st_read("F:/Cholera/shape_22/Adm_1/eth_admbnda_adm1_csa_bofedb_2021.shp", quiet = TRUE) %>%
  st_transform(20138) %>% st_make_valid()

landcover <- st_read("F:/date_map/landcover_modis_shapefile/ethiopia_modis_lc_veg_water.shp", quiet = TRUE) %>%
  st_transform(20138) %>% st_make_valid()

south <- ethiopia %>% filter(grepl("Southern", ADM1_EN, ignore.case = TRUE))
st_agr(landcover) <- "constant"; st_agr(south) <- "constant"
landcover_south <- st_intersection(landcover, south)

# Town Data
cities_df <- data.frame(
  name  = c("Arba Minch", "Jinka", "Konso", "Wolaita Sodo"),
  lon   = c(37.55, 36.65, 37.43, 37.76),
  lat   = c(6.03, 5.78, 5.33, 6.86),
  color = c("#00C2FF", "#C1121F", "#FFD000", "#3EFF00")
)
cities_sf <- st_as_sf(cities_df, coords = c("lon", "lat"), crs = 4326) %>% st_transform(20138)

# ============================================================
# 2. MAIN MAP (CLEAN BOX WITH INTERNAL LEGEND)
# ============================================================
main_map_fixed <- ggplot() +
  geom_sf(data = landcover_south, aes(fill = class), color = NA, alpha = 0.9) +
  scale_fill_manual(
    values = c("NVT" = "#2E5E3A", "WAT" = "#4C88B5"), 
    labels = c("Natural vegetation", "Inland water"), 
    name = "Land cover"
  ) +
  new_scale_fill() +
  geom_sf(data = south, fill = NA, color = "black", linewidth = 0.8) +
  geom_sf(data = cities_sf, aes(fill = name), shape = 21, size = 4.5, color = "white", stroke = 1.2) +
  scale_fill_manual(values = setNames(cities_df$color, cities_df$name), name = "Regional towns") +
  
  # Coordinate limits with internal white space for the legend
  coord_sf(
    xlim = c(st_bbox(south)$xmin - 15000, st_bbox(south)$xmax + 25000),
    ylim = c(st_bbox(south)$ymin - 20000, st_bbox(south)$ymax + 15000),
    expand = FALSE
  ) +
  
  # Scale bar and North arrow with padding
  annotation_scale(location = "bl", width_hint = 0.15, style = "ticks", pad_x = unit(0.6, "cm"), pad_y = unit(0.6, "cm")) +
  annotation_north_arrow(location = "tl", height = unit(0.7, "cm"), width = unit(0.7, "cm"), 
                         style = north_arrow_fancy_orienteering()) +
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
    panel.grid.major = element_line(color = "gray95", linetype = "dashed"),
    
    # Small, discrete legend positioned away from axis numbers
    legend.position = c(0.98, 0.02),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray70", linewidth = 0.3),
    legend.key.size = unit(0.35, "cm"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    
    axis.text = element_text(size = 8, color = "black"),
    axis.title = element_blank()
  )

# ============================================================
# 3. INSET MAP (SLATE GREY BACKGROUND)
# ============================================================
inset_map <- ggplot() +
  geom_sf(data = st_transform(ethiopia, 4326), fill = "gray90", color = "gray60", linewidth = 0.15) +
  geom_sf(data = st_transform(south, 4326), fill = "#C1121F", color = "black", linewidth = 0.4) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#E6E9ED", color = "gray40", linewidth = 0.6),
    plot.margin = margin(4, 4, 4, 4)
  )

# ============================================================
# 4. FINAL COMPOSITION (CENTERED FOOTER)
# ============================================================
final_map <- ggdraw() +
  # Main Frame (The Big Box)
  draw_grob(rectGrob(gp = gpar(fill = "white", col = "black", lwd = 2.5))) + 
  
  # Main Map
  draw_plot(main_map_fixed, x = 0.04, y = 0.09, width = 0.92, height = 0.83) +
  
  # Ethiopia Inset (Positioned for breathing space)
  draw_plot(inset_map, x = 0.81, y = 0.74, width = 0.14, height = 0.17) +
  
  # Titles
  draw_label("Southern Ethiopia Regional State", x = 0.5, y = 0.96, size = 17, fontface = "bold") +
  draw_label("Land cover and major town distribution", x = 0.5, y = 0.93, size = 11, fontface = "italic") +
  
  # Inset Label
  draw_label("Ethiopia", x = 0.88, y = 0.915, size = 9, fontface = "bold", color = "gray20") +
  
  # --- CENTERED SOURCE TEXT ---
  draw_label(
    "Source: MODIS Land Cover & OpenStreetMap (2025) | Coordinate System: EPSG:20138", 
    x = 0.5,           
    y = 0.04,          
    size = 8, 
    fontface = "italic", 
    color = "gray40", 
    hjust = 0.5        
  )

# ============================================================
# 5. EXPORT
# ============================================================
ggsave(
  "South_Ethiopia_Centered_Final.png",
  final_map,
  width = 12,
  height = 9,
  dpi = 600,
  bg = "white"
)
