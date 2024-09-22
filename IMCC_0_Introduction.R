# IMCC7 Workshop:
# Conservation Planning for the marine environment: An introduction to tools and techniques
# Introduction
# 14/10/2024
# Sandra Neubert (s.neubert@uq.edu.au)

# Create a dataset boundary
dat_bndry <- tibble(x = 100, y = seq(-50, 0, by = 1)) %>%
  bind_rows(tibble(x = seq(100, 160, by = 1), y = 0)) %>%
  bind_rows(tibble(x = 160, y = seq(0, -50, by = -1))) %>%
  bind_rows(tibble(x = seq(160, 100, by = -1), y = -50)) %>%
  as.matrix() %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs = "EPSG:4326") %>%
  st_sf() %>%
  st_make_valid()

# Use boundary to create grid
dat_PUs <- st_make_grid(dat_bndry, cellsize = 2) %>%
  st_sf() %>%
  mutate(cellID = row_number()) # Add a cell ID reference

(gg_PUs <- splnr_plot_PUs(dat_PUs))

#create some random species data
dat_species_prob <- dat_PUs %>%
  st_sf() %>%
  mutate(
    Spp1 = runif(n = dim(.)[[1]]),
    Spp2 = runif(n = dim(.)[[1]]),
    Spp3 = runif(n = dim(.)[[1]]),
    Spp4 = runif(n = dim(.)[[1]]),
    Spp5 = runif(n = dim(.)[[1]])
  )
print(dat_species_prob)

#convert to binary data
dat_species_bin <- dat_species_prob %>%
  as_tibble() %>%
  mutate(across(
    -any_of(c("cellID", "geometry")), # Apply to all columns except geometry and cellID
    ~ case_when(
      . >= 0.5 ~ 1,
      . < 0.5 ~ 0,
      is.na(.data) ~ 0
    )
  )) %>%
  st_as_sf()

#Run prioritisation

#extract feature names
col_name <- dat_species_bin %>%
  select(-"cellID") %>%
  st_drop_geometry() %>%
  colnames()

#add a cost layer
out_sf <- dat_species_bin %>%
  mutate(CostArea = rep(1, 780))

#create targets object
targets <- rep(0.3, length(col_name))

#create a conservation problem
dat_problem <- problem(out_sf,
                       features = col_name,
                       cost_column = "CostArea") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

#solve conservation problem
dat_soln <- dat_problem %>%
  solve.ConservationProblem()

(gg_sol <- splnr_plot_Solution(dat_soln))

