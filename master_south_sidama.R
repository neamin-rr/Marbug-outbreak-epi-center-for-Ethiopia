# ============================================================
# ETHIOPIA SOUTH & SIDAMA – FINAL PUBLICATION MAP
# Features: Glow borders, zoomed main map, high inset, 600 DPI
# ============================================================

library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(cowplot)
library(grid)
library(ggnewscale)

# ------------------------------------------------------------
# 1. LOAD & PREPARE ADMINISTRATIVE DATA
# ------------------------------------------------------------
ethiopia <- st_read(
  "F:/Cholera/shape_22/Adm_1/eth_admbnda_adm1_csa_bofedb_2021.shp",
  quiet = TRUE
) %>%
  st_transform(20138) %>%
  st_make_valid()

# Filter for Southern Nations + Sidama
south_sidama <- ethiopia %>%
  filter(grepl("Southern", ADM1_EN, ignore.case = TRUE) |
           grepl("Sidama", ADM1_EN, ignore.case = TRUE)) %>%
  mutate(region = case_when(
    grepl("Southern", ADM1_EN, ignore.case = TRUE) ~ "Southern Nations",
    grepl("Sidama", ADM1_EN, ignore.case = TRUE) ~ "Sidama"
  ))

# ------------------------------------------------------------
# 2. LOAD & CLIP LANDCOVER
# ------------------------------------------------------------
landcover <- st_read(
  "F:/Epi_gene_map/date_map/landcover_modis_shapefile/ethiopia_modis_lc_veg_water.shp",
  quiet = TRUE
) %>%
  st_transform(20138) %>%
  st_make_valid()

# Set constant geometry for overlay operations
st_agr(landcover) <- "constant"
st_agr(south_sidama) <- "constant"

# Clip landcover to south & Sidama region
landcover_region <- st_intersection(landcover, south_sidama)

# ------------------------------------------------------------
# 3. TOWN LOCATIONS & STYLES
# ------------------------------------------------------------
cities_df <- data.frame(
  name = c("Arba Minch", "Jinka", "Hawassa", "Dasenech"),
  lon  = c(37.55, 36.65, 38.45, 36.05), 
  lat  = c(6.03, 5.78, 7.00, 4.80), 
  col  = c("#00C2FF", "#C1121F", "#FFD000", "#3EFF00")
)

cities_sf <- st_as_sf(cities_df, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(20138)
cities_xy <- cbind(cities_df, st_coordinates(cities_sf))

# Border colors for main map
southern_border <- "#0F1F40"  # Dark blue
sidama_border   <- "#5D2906"  # Dark brown

# Glow layer colors (lighter)
glow_colors <- c("Southern Nations" = "#6C8EBF", "Sidama" = "#C97C5D")

# ------------------------------------------------------------
# 4. MAIN MAP – LANDCOVER + GLOWED BORDERS + CITIES
# ------------------------------------------------------------
main_map <- ggplot() +
  
  # Landcover layer
  geom_sf(data = landcover_region, aes(fill = class), color = NA, alpha = 0.9) +
  scale_fill_manual(values = c("NVT" = "#2E5E3A", "WAT" = "#4C88B5"), guide = "none") +
  new_scale_fill() +
  
  # Glow layer (lighter colored borders)
  geom_sf(data = south_sidama, aes(color = region), fill = NA, 
          linewidth = 0.7, lineend = "round", linejoin = "round", alpha = 0.4) +
  scale_color_manual(values = glow_colors, guide = "none") +
  new_scale_color() +
  
  # Main colored borders
  geom_sf(data = south_sidama, aes(color = region), fill = NA, 
          linewidth = 0.35, lineend = "round", linejoin = "round") +
  scale_color_manual(values = c("Southern Nations" = southern_border, 
                                "Sidama" = sidama_border), guide = "none") +
  new_scale_fill() +
  
  # City points
  geom_point(data = cities_xy, aes(x = X, y = Y), color = "white", fill = "white", 
             shape = 21, size = 8.0) + 
  geom_point(data = cities_xy, aes(x = X, y = Y, fill = col), shape = 21, 
             size = 6.2, stroke = 0.4, color = "black") +
  scale_fill_identity() +
  
  # Zoom to south & Sidama
  coord_sf(
    xlim = c(st_bbox(south_sidama)$xmin - 10000, st_bbox(south_sidama)$xmax + 20000),
    ylim = c(st_bbox(south_sidama)$ymin - 15000, st_bbox(south_sidama)$ymax + 15000),
    expand = FALSE
  ) +
  
  # Scale bar and north arrow
  annotation_scale(location = "bl", width_hint = 0.15, style = "bar",
                   bar_cols = c("black", "white"), text_cex = 0.7,
                   pad_x = unit(1.8, "cm"), pad_y = unit(0.4, "cm")) +
  annotation_north_arrow(location = "tl", height = unit(0.8, "cm"), width = unit(0.8, "cm"),
                         style = north_arrow_fancy_orienteering(),
                         pad_x = unit(1.2, "cm"), pad_y = unit(1.2, "cm")) +
  
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.7),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed", linewidth = 0.3),
    axis.title = element_blank(),
    axis.text  = element_text(size = 8, color = "black"),
    panel.background = element_rect(fill = "white")
  )

# ------------------------------------------------------------
# 5. INSET MAP – ETHIOPIA CONTEXT
# ------------------------------------------------------------
inset_map <- ggplot() +
  geom_sf(data = st_transform(ethiopia, 4326), fill = "#F5F5F5", color = "#B0B0B0", linewidth = 0.22) +
  geom_sf(data = st_transform(south_sidama, 4326), aes(fill = region, color = region), linewidth = 0.5) +
  scale_fill_manual(values = glow_colors, guide = "none") +
  scale_color_manual(values = c("Southern Nations" = southern_border, 
                                "Sidama" = sidama_border), guide = "none") +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8))

# ------------------------------------------------------------
# 6. FINAL COMPOSITION
# ------------------------------------------------------------
final_map <- ggdraw() +
  draw_grob(rectGrob(gp = gpar(fill = "white", col = "black", lwd = 1.5))) +  # Outer border
  draw_plot(main_map, x = 0.02, y = 0.05, width = 0.70, height = 0.90) +       # Main map
  draw_plot(inset_map, x = 0.73, y = 0.60, width = 0.24, height = 0.35)        # Ethiopia inset

# ------------------------------------------------------------
# 7. EXPORT FINAL MAP
# ------------------------------------------------------------
ggsave("final_South_Sidama_Ethiopia_Final_V1021.png", 
       final_map, width = 14, height = 9, dpi = 600, bg = "white")

cat("✅ Map successfully exported: Glow borders, enhanced inset, 600 DPI.\n")
