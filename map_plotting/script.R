library(pacman)

pacman::p_load(tidyverse, 
               spocc, 
               ggmap, 
               maptools, 
               maps, 
               ggplot2,
               scrubr, 
               mapr, 
               tidyr, 
               stringr,
               rnaturalearth, 
               rnaturalearthdata, 
               rlang, 
               sf, 
               ggspatial,
               raster,
               stars,
               here,
               colorspace)

# set the ggplot theme

theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", 
                                              fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), 
                                                            "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), 
                                                            "mm")),
                  legend.position = "none"))

# import the shape file
biome_shp <- sf::st_read("vegm2006_biomes_withforests/vegm2006_biomes_withforests.shp")

biome_df <- as_tibble(biome_shp)

# What levels are present within BIOMENAME?
biome_df %>%
  distinct(BIOMENAME)

###########################################################
# Plot SA map coloured by biome  
###########################################################

# Plot basic biome map coloured by biome 
ggplot(data = biome_shp) +
  # Colours by biome, alpha makes colour a bit transparent
  geom_sf(aes(fill = BIOMENAME),
          alpha = 0.3) +
  # We want a legend on the right side of the graph
  theme(legend.position = "right") +
  # Add a colour palette of yellow, orange and brown
  scale_fill_brewer(palette = "Accent") +
  # Add x and y-axis labels, and change legend title 
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "Biome") + 
  # Limit y and x axis limits 
  coord_sf(xlim = c(15.5, 33.5), 
           ylim = c(-35, -21.75), 
           expand = FALSE) +
  # Limit y and x axis limits (western cape only)
  # coord_sf(xlim = c(17, 25.5), 
  #         ylim = c(-35, -31), 
  #         expand = FALSE) +
  # Add a scale bar
  annotation_scale(location = "br",  
                   style = "ticks", 
                   width_hint = 0.150) +
  # Add a north arrow 
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.175, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

###########################################################
# Plot SA map coloured by biome, with province borders
###########################################################

# read in shape file for provincial borders
province_shp <- st_read("ZAF_adm/ZAF_adm1.shp") 

province_shp$NAME_1

# predefine the colours for each biome

cols <- c("Albany Thicket" = "darkgreen", 
          "Desert" = "orange", 
          "Forests" = "black", 
          "Fynbos" = "steelblue2",
          "Grassland" = "grey80",
          "Indian Ocean Coastal Belt" = "turquoise",
          "Nama-Karoo" = "pink",
          "Savanna" = "khaki",
          "Succulent Karoo" = "darkolivegreen3")

# plot the new map:

ggplot(data = biome_shp) +
  geom_sf(aes(fill = BIOMENAME),
          alpha = 1) +
  ###############
# Only new bit
geom_sf(data = province_shp, aes(fill = NA),
        # Colour of the outline
        colour = "black",
        # Width of the province border lines
        size = 0.5) +
  # ###########
theme(legend.position = "right") +
  #scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = cols) +
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "Biome") + 
  coord_sf(xlim = c(15.5, 33.5), 
           ylim = c(-35, -21.75), 
           expand = FALSE) +
  annotation_scale(location = "br",  
                   style = "ticks", 
                   width_hint = 0.150) +
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.175, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

###########################################################
# Plot GPS coordinates onto the map
###########################################################

# Import GPS points from excel file on GitHub
sample_gps <- readr::read_csv("tetramesa_sample_info.csv")

# Check data import 
head(sample_gps)

# Clean the column names
sample_gps <- sample_gps %>%
  janitor::clean_names()

head(sample_gps)

# find the distinct host plants (ie. factors)
sample_gps %>% distinct(host)

# plot the map with the added GPS coordinates as points
map_plot = ggplot() +
  # geom_sf(data = biome_shp,
  #         aes(fill = "BIOMENAME"),
  #         alpha = 0.8) +
  geom_sf(data = province_shp,
          aes(fill = NA),
          colour = "black",
          size = 0.35) +
  #Add the points onto the map
  geom_point(data = sample_gps, aes(x = longitude, 
                                    y = latitude,
                                    colour = host
  ),
  size = 2, shape = 16) +
  # Colour the points manually according to host plant:
  # E. curvula = red, S. pyramidalis = blue and H. hirta = green
  scale_colour_manual(values = c("blue", "royalblue", "green", "cyan", "purple", "black", "red", "gold", "maroon", "lightblue", "orange", "tan")) +
  theme(legend.position = "right") +
  # Manually specifcy fill colours for different biomes
  scale_fill_manual(values = cols) + 
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "Biome",
       colour = "Host Plant") + 
  coord_sf(xlim = c(15.5, 33.5), 
           ylim = c(-35, -21.75), 
           expand = FALSE) +
  annotation_scale(location = "br",  
                   style = "ticks", 
                   width_hint = 0.150) +
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.325, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

map_plot

ggsave(plot = map_plot, width = 10, height = 10, dpi = 450, filename = "map_plot.png", units = "cm")


#####################################################
# PLOT A PARTICULAR PROVINCE
#####################################################

province_shp_LMP <- province_shp %>%
  dplyr::filter(NAME_1 == "Free State") 

province_shp$NAME_1

map_plot = ggplot() +
  # geom_sf(data = biome_shp,
  #         aes(fill = "BIOMENAME"),
  #         alpha = 0.8) +
  geom_sf(data = province_shp_LMP,
          aes(fill = NA),
          colour = "black",
          size = 0.35) +
  #Add the points onto the map
  geom_point(data = sample_gps, aes(x = longitude,
                                    y = latitude,
                                    colour = host
  ),
  size = 2, shape = 16) +
  # Colour the points manually according to host plant:
  # E. curvula = red, S. pyramidalis = blue and H. hirta = green
  scale_colour_manual(values = c("blue", "royalblue", "green", "cyan", "purple", "black", "red", "gold", "maroon", "lightblue", "orange", "tan")) +
  theme(legend.position = "right") +
  # Manually specifcy fill colours for different biomes
  scale_fill_manual(values = cols) + 
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "Biome",
       colour = "Host Plant") + 
  coord_sf(xlim = c(24, 30), 
           ylim = c(-31, -26.5), 
           expand = FALSE) +
  annotation_scale(location = "br",  
                   style = "ticks", 
                   width_hint = 0.150) +
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.325, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

map_plot

#####################################################

# Count number of known Tetramesa species per province

tetra_dist =  sample_gps %>% dplyr::count(province)
colnames(tetra_dist) = c("NAME_1", "n")

SA_data <- full_join(province_shp, tetra_dist, by = "NAME_1")

SA_data <- SA_data %>%
  mutate(no_rows = as.numeric(n))

SA_data$no_rows[SA_data$no_rows == 0] <- NA

# Plot the map

p <- ggplot(data = province_shp) +
  geom_sf() +
  geom_sf(data = SA_data, aes(fill = n)) +
  scale_fill_gradientn(colours = colorspace::heat_hcl(n=7, alpha = 0.5), na.value = "white") +
  labs(fill = "No. of species per province") + 
  coord_sf(xlim = c(15.5, 33.5), 
           ylim = c(-35, -21.75),
           crs = 4326, 
           expand = FALSE) +
  guides(fill = guide_colorbar(ticks = FALSE),
         colour = guide_legend(order = 1))

p + theme(legend.position = "bottom")

p + scale_fill_gradient(low="lightblue", high="red", na.value = "white") + theme(legend.position = "bottom") 
