# IMCC7 Workshop:
# Conservation Planning for the marine environment: An introduction to tools and techniques
# Part 1: Spatial prioritisation: Preparing input data
# 14/10/2024
# Sandra Neubert (s.neubert@uq.edu.au)

# Load packages
pacman::p_load(tidyverse, sf, terra, stars, rnaturalearth, mregions, tmap, prioritizr, purrr)

# Define CRS##also give ESRI example
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"

# Define file paths
inputDat <- file.path("Input")

# Load data ---------------------------------------------------------------

# Load Planning Units ---------------------------------------------------------------
PUs <- st_read(file.path(inputDat,  "PUs","Galapagos_Planning_Units.shp")) %>%
  st_transform(cCRS) %>%
  select(-"cost") %>%
  rename(cellID = puid)

ggplot() +
  geom_sf(data = PUs)

# Load Features ---------------------------------------------------------------
# Create an sf object for all features

features <- readRDS(file.path(inputDat, "Features", "fans.rds")) %>%
  left_join(readRDS(file.path(inputDat, "Features", "plateau.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "ridge.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "seamount.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "abyssal_hills.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "abyssal_plains.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "abyssal_mountains.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "basin.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "escarpments.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "rift_valleys.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "shelf_class_high.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "spreading_ridges.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "blue_footed_booby.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "great_frigatebird.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "green_turtle.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "silky_shark.rds")) %>% st_drop_geometry(), by = "cellID") %>%
  left_join(readRDS(file.path(inputDat, "Features", "tiger_shark.rds")) %>% st_drop_geometry(), by = "cellID")

# Load Cost  ---------------------------------------------------------------
cost <- st_read(file.path(inputDat, "Cost", "cost_surface.shp")) %>%
  st_transform(cCRS) %>%
  rename(cellID = puid)

ggplot() +
  geom_sf(data = cost, aes(fill = .data$cost))

# Create combined sf object  ---------------------------------------------------------------
out_sf <- features %>%
  left_join(cost %>% sf::st_drop_geometry(), by = "cellID")

