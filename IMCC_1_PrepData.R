# IMCC7 Workshop:
# Conservation Planning for the marine environment: An introduction to tools and techniques
# Part 1: Spatial prioritisation: Preparing input data
# 14/10/2024
# Sandra Neubert (s.neubert@uq.edu.au)

# Load packages
pacman::p_load(tidyverse, sf, rnaturalearth, patchwork, prioritizr)

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

features <- readRDS(file.path(inputDat, "Features", "Features_Combined.rds"))

# quick look at data
ggplot() + geom_sf(data = features, aes(fill = tiger_shark))

ggplot() + geom_sf(data = features, aes(fill = seamount))

# Load Cost  ---------------------------------------------------------------
cost <- st_read(file.path(inputDat, "Cost", "cost_surface.shp")) %>%
  st_transform(cCRS) %>%
  rename(cellID = puid)

ggplot() +
  geom_sf(data = cost, aes(fill = .data$cost))

# Create combined sf object  ---------------------------------------------------------------
out_sf <- features %>%
  left_join(cost %>% sf::st_drop_geometry(), by = "cellID")

# Get landmass  ---------------------------------------------------------------
landmass <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  sf::st_transform(cCRS)
landmass <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  sf::st_transform(cCRS)

