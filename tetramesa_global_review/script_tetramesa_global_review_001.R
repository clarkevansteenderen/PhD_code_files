# Load required packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(splitstackshape)
library(cowplot)

# -----------------------------------------------------------------------------
# Set global theme for graphics 
# -----------------------------------------------------------------------------

theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                  legend.position = "none"))

# Load data
raw_data <- readxl::read_xlsx("tetramesa_inventory_global.xlsx")
head(raw_data)

# Restructure the data
raw_data <- raw_data %>%
  # Add a column of no. of years since described 
  mutate(years_since_desc = 2021 - year_described) %>%
  # Change character variables to factors 
  mutate_if(is.character, as.factor) 
head(raw_data)

##############################################################
# - How many host-plants are attacked by Tetramesa species?  
##############################################################

# How many host plant genera is each species associated with? 
genera_plot <- raw_data %>%
  mutate(plant_genera = as.factor(plant_genera)) %>% 
  drop_na(plant_genera) %>%
  ggplot(aes(x = plant_genera)) +
  geom_bar() + 
  scale_y_continuous(breaks = seq(0, 100, 25),
                     limits = c(0, 100)) +
  theme_classic() +
  labs(x = "No. of host plant genera",
       y = "Frequency",
       subtitle = "(a)") +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
        legend.position = "none")
genera_plot

# How many host plant species is each species associated with? 
species_plot <- raw_data %>%
  mutate(plant_species = as.factor(plant_species)) %>% 
  drop_na(plant_species) %>%
  ggplot(aes(x = plant_species)) +
  geom_bar() + 
  scale_y_continuous(breaks = seq(0, 100, 25),
                     limits = c(0, 100)) +
  theme_classic() +
  labs(x = "No. of host plant species",
       y = "Frequency",
       subtitle = "(b)") +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
        legend.position = "none")
species_plot

# Save these plots together 
both_plots <- plot_grid(genera_plot, species_plot)
ggsave("host_plant_distribution_plot.png",
       dpi = 600,
       width = 6,
       height = 3)

# Has the number of species being described change over time? 
ggplot(data = raw_data, aes(x = year_described)) +
  geom_histogram() + 
  labs(x = "Year originally described",
       y = "No. of species") +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
        legend.position = "none")

# Does the number of host plant genera increase with time since description?
raw_data %>%
  drop_na(plant_genera) %>%
  ggplot(aes(x = years_since_desc,
                            y = plant_genera)) +
  geom_point() + 
  labs(x = "Years since described",
       y = "No. of host plant genera") +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
        legend.position = "none")

# Does the number of host plant species increase with time since description? 
raw_data %>%
  drop_na(plant_species) %>%
  ggplot(aes(x = years_since_desc,
           y = plant_species)) +
  geom_point() + 
  labs(x = "Years since described",
       y = "No. of host plant species") +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
        legend.position = "none")

###
# - How does Tetramesa species distribution differ by plant characteristics? 
###

# Are Tetramesa distributed equally amongst perennial/annual species? 
life_hist_plot <- raw_data %>%
  tidyr::drop_na(plant_life_hist) %>%
  dplyr::group_by(plant_life_hist) %>%
  dplyr::summarise(no_species = length(plant_life_hist)) %>%
  mutate(plant_life_hist = forcats::fct_reorder(plant_life_hist, 
                                                c("Annual", 
                                                  "Perennial", 
                                                  "Annual + Perennial", 
                                                  "NA"))) %>%
  dplyr::filter(plant_life_hist != c("NA")) %>%
  ggplot(aes(x = plant_life_hist,
             y = no_species)) +
  geom_col() + 
  labs(x = "Plant life-history",
       y = "Frequency",
       subtitle = "(a)") +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(margin = unit(c(4, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
        legend.position = "none")
life_hist_plot

# Are Tetramesa distributed equally amongst C3 vs C4 species? 
pathway_plot <- raw_data %>%
  tidyr::drop_na(plant_photosynthesis) %>%
  dplyr::group_by(plant_photosynthesis) %>%
  dplyr::summarise(no_species = length(plant_photosynthesis)) %>%
  dplyr::filter(plant_photosynthesis != c("NA")) %>%
  ggplot(aes(x = plant_photosynthesis,
             y = no_species)) +
  geom_col() + 
  labs(x = "Plant photosynthetic pathway",
       y = "Frequency",
       subtitle = "(b)") +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(margin = unit(c(4, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
        legend.position = "none")
pathway_plot

# Save these plots together 
both_plots <- plot_grid(life_hist_plot, pathway_plot)
ggsave("plant_characteristics_plot.png",
       dpi = 600,
       width = 8,
       height = 4)

#######################################################################
# - What is the taxonomic distribution of Tetramesa across the Poaceae?
#######################################################################

# What is the distribution of Tetramesa spp. amongst grass sub-families
raw_data %>%
  drop_na(plant_subfam) %>%
  group_by(plant_subfam) %>%
  summarise(count = n())

# What is the distribution of Tetramesa spp. amongst grass tribes
raw_data %>%
  drop_na(plant_tribe) %>%
  group_by(plant_tribe) %>%
  summarise(count = n())

######################################################################
# - Make a map of global distribution of Tetramesa species per country
######################################################################

# Split the countries column into many columns (1 country per column)
countries_data <- cSplit(raw_data, 
                         'countries_recorded', 
                         sep = ",", 
                         fixed = FALSE)
View(countries_data)

# Now reshape into long format
countries_long <- countries_data %>%
  pivot_longer(
    # cols = which columns do we want to pivot/move
    cols = starts_with("countries_recorded_"),
    # names_to = new column name that the names of cols above will be
    # moved to. This effectively creates your categorical
    # factor levels
    names_to = "country",
    # values_to = new column where the row values of cols will be stored
    values_to = "country_present")
View(countries_long)

# Count number of known Tetramesa species per country
tetra_dist_sum <- countries_long %>%
  drop_na(country_present) %>%
  summarise(no_rows = n()) 
tetra_dist_sum

tetra_dist_sum <- countries_long %>%
  drop_na(country_present) %>%
  dplyr::group_by(country_present) %>%
  dplyr::count()
tetra_dist_sum

# Load a world map 
# Load required libraries
library(sp)
library(sf)
library(raster)
library(rasterVis)
library(maptools)
library(tidyverse)
library(dismo)
library(raster)
library(rgeos)
library(rJava)
library(rgdal)
library(geosphere)
library(scales)
library(maptools)
library(mapdata)
library(spThin)
library(ENMeval)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(dplyr)
library(gridSVG)
library(ggspatial)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

world <- ne_countries(scale = "medium", returnclass = "sf")

# Join the Tetramesa data to the world map 
tetra_dist <- tetra_dist_sum %>%
  mutate(name = country_present)
levels(tetra_dist$name)[levels(tetra_dist$name)=='USA'] <- 'United States'
levels(tetra_dist$name)[levels(tetra_dist$name)=='PRC'] <- 'China'
levels(tetra_dist$name)[levels(tetra_dist$name)=='USSR'] <- 'Russia'

world_data <- full_join(world, tetra_dist, by = "name")

world_data <- world_data %>%
  mutate(no_rows = as.numeric(n))


# Set the theme for the plot 
theme_opts<-list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_rect(fill = 'white', colour = NA),
                       plot.background = element_rect(),
                       axis.line = element_blank(),
                       axis.text.x = element_text(colour = "black"),
                       axis.text.y = element_text(colour = "black"),
                       axis.ticks = element_line(colour = "black"),
                       axis.title.x = element_text(colour = "black"),
                       axis.title.y = element_text(colour = "black"),
                       plot.title = element_text(colour = "black"),
                       panel.border = element_rect(fill = NA),
                       legend.key=element_blank()))

# Set 0 to NA
world_data$no_rows[world_data$no_rows == 0] <- NA

p <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = world_data, aes(fill = n)) +
  scale_fill_viridis_c(na.value = "white",
                       direction = -1,
                       alpha = 0.5) + 
  labs(fill = "No. of species per country") + 
  coord_sf(xlim = c(-180, 180), 
           ylim = c(-90, 90), 
           crs = 4326, 
           expand = FALSE) +
  theme_opts +
  guides(fill = guide_colorbar(ticks = FALSE),
         colour = guide_legend(order = 1))

p + theme(legend.position = "bottom")

colorspace::

p <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = world_data, aes(fill = n)) +
  scale_fill_gradientn(colours = colorspace::heat_hcl(n=7, alpha = 0.5), na.value = "white") +
  labs(fill = "No. of species per country") + 
  coord_sf(xlim = c(-180, 180), 
           ylim = c(-90, 90), 
           crs = 4326, 
           expand = FALSE) +
  theme_opts +
  guides(fill = guide_colorbar(ticks = FALSE),
         colour = guide_legend(order = 1))

p + theme(legend.position = "bottom")

p + scale_fill_gradient(low="lightblue", high="red", na.value = "white") + theme(legend.position = "bottom")

?scale_fill_gradient

# Save the plot 
ggsave("./tetramesa_global_map.png",
       width = 8,
       height = 6, dpi = 400)




raw_data %>%
  mutate(plant_genera = as.factor(plant_genera)) %>% 
  drop_na(plant_genera) %>%
  group_by(plant_genera) %>%
  count()

raw_data %>%
  mutate(plant_species = as.factor(plant_species)) %>% 
  drop_na(plant_species) %>%
  group_by(plant_species) %>%
  count()
raw_data %>%
  mutate(plant_species = as.factor(plant_species)) %>% 
  drop_na(plant_species) %>%
  group_by(plant_species) %>%
  median(plant_spe)

median(raw_data$plant_genera, na.rm = T)
median(raw_data$plant_species, na.rm = T)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(raw_data$plant_genera)
Mode(raw_data$plant_species)

# What is the host-specificity of Tetramesa attacking annual versus perennial grasses?
raw_data %>%
  tidyr::drop_na(plant_photosynthesis) %>%
  dplyr::group_by(plant_photosynthesis) %>%
  dplyr::summarise(no_host_plants = mean(plant_species),
                   median_host_species = median(plant_species),
                   median_host_genera = median(plant_genera))

genera_plot <- raw_data %>%
  tidyr::drop_na(plant_life_hist) %>%
  dplyr::group_by(plant_life_hist) %>% 
  dplyr::filter(plant_life_hist != "NA") %>%
  ggplot(aes(x = plant_life_hist, 
             y = plant_genera)) +
  geom_jitter(width = 0.15, height = 0.15) +
  labs(x = "Plant life-history",
       y = "No. of genera as hosts",
       subtitle = "(a)") +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(margin = unit(c(4, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
        legend.position = "none")
genera_plot

species_plot <- raw_data %>%
  tidyr::drop_na(plant_life_hist) %>%
  dplyr::group_by(plant_life_hist) %>% 
  dplyr::filter(plant_life_hist != "NA") %>%
  ggplot(aes(x = plant_life_hist, 
             y = plant_species)) +
  geom_jitter(width = 0.15, height = 0.15) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6)) +
  labs(x = "Plant life-history",
       y = "No. of species as hosts",
       subtitle = "(b)") +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(margin = unit(c(4, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
        legend.position = "none")
species_plot

both_plots <- plot_grid(genera_plot, species_plot)
ggsave("host_range_by_plant_life_history_plot.png",
       dpi = 600,
       width = 8,
       height = 4)

###
# - Do non-monophagous species attack close relatives?
###

# Species, by genus 
raw_data %>%
  # Limit to only non monophagous species
  dplyr::filter(plant_species > 1) %>%
  ggplot(aes(x = plant_genera,
             y = plant_species)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_jitter(height = 0.15,
              width = 0.1) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7),
                     limits = c(0, 7),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5),
                     limits = c(0, 5),
                     expand = c(0, 0)) +
  coord_cartesian() + 
  labs(x = "No. of genera as hosts",
       y = "No. of species as hosts",
       subtitle = "") +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(margin = unit(c(4, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
        legend.position = "none")

####
# - Sampling effort and host-range
####

# Does the reported host-range increase with more citations? 
# How many host plant species is each species associated with? 
citations_species <- raw_data %>%
  mutate(plant_species = as.factor(plant_species)) %>% 
  drop_na(plant_hosts_total_citations) %>%
  ggplot(aes(x = plant_hosts_total_citations,
             y = plant_species)) +
  geom_jitter() +
  scale_x_continuous(breaks = seq(0, 12, 2),
                     limits = c(0, 12)) +
  labs(x = "No. of citations",
       y = "No. of recorded host species",
       subtitle = "(b)")

# Run a linear model
effort_data <- raw_data %>%
  drop_na(plant_hosts_total_citations) 

lm1 <- lm(plant_species  ~ plant_hosts_total_citations,
          data = effort_data)
summary(lm1)

# How many host plant genera is each species associated with? 
citations_genera <- raw_data %>%
  mutate(plant_genera = as.factor(plant_genera)) %>% 
  drop_na(plant_hosts_total_citations) %>%
  ggplot(aes(x = plant_hosts_total_citations,
             y = plant_genera)) +
  geom_jitter() +
  scale_x_continuous(breaks = seq(0, 12, 2),
                     limits = c(0, 12)) +
  labs(x = "No. of citations",
       y = "No. of recorded host genera",
       subtitle = "(a)")

# Save these plots together 
citation_plots <- plot_grid(citations_genera, citations_species)

citation_plots

ggsave("no_citations_known_host_range.png",
       dpi = 600,
       width = 8,
       height = 4)






lm2 <- lm(plant_genera ~ plant_hosts_total_citations,
          data = effort_data)
summary(lm2)

######################################################################
# - How many citations per host plant record? 
######################################################################

# Now reshape into long format
cit_long <- raw_data %>%
  pivot_longer(
    # cols = which columns do we want to pivot/move
    cols = starts_with("x"),
    # names_to = new column name that the names of cols above will be
    # moved to. This effectively creates your categorical
    # factor levels
    names_to = "no_citations",
    # values_to = new column where the row values of cols will be stored
    values_to = "n")
View(cit_long)

# Plot the distribution of host plant records 
cit_long %>%
  tidyr::drop_na(n) %>%
  ggplot(aes(x = n)) +
  geom_histogram(bins=8, binwidth = 0.5) + 
  scale_y_continuous(breaks = seq(0, 150, 25),
                     limits = c(0, 150)) +
  scale_x_continuous(breaks = seq(0, 7, 1),
                     limits = c(0.5, 7)) +
  labs(x = "No. of citations per host plant record",
       y = "Frequency")

ggsave("no_citations_per_plant_host_range.png",
       dpi = 600,
       width = 8,
       height = 4)







