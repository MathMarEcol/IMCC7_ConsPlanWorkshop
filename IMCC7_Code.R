## ----eval=FALSE----------------------------------------------------------------------------------------
## install.packages("pacman")


## ------------------------------------------------------------------------------------------------------
pacman::p_load(tidyverse, sf, rnaturalearth, patchwork, prioritizr, viridis)
source("utils-functions.R")


## ----filepath1-----------------------------------------------------------------------------------------
inputDat <- file.path("Input") # Define file paths


## ------------------------------------------------------------------------------------------------------
cCRS <- "ESRI:54009" 


## ------------------------------------------------------------------------------------------------------
PUs <- st_read(file.path(inputDat, "PUs", "Galapagos_Planning_Units.shp")) %>%
  st_transform(cCRS) %>%
  select(-"cost") %>%
  rename(cellID = puid)

ggplot() +
  geom_sf(data = PUs)


## ------------------------------------------------------------------------------------------------------
features <- readRDS(file.path(inputDat, "Features", "Features_Combined.rds"))


## ------------------------------------------------------------------------------------------------------
ggplot() +
  geom_sf(data = features, aes(fill = tiger_shark))

ggplot() +
  geom_sf(data = features, aes(fill = seamount))


## ------------------------------------------------------------------------------------------------------
cost <- st_read(file.path(inputDat, "Cost", "cost_surface.shp")) %>%
  st_transform(cCRS) %>%
  rename(cellID = puid)

ggplot() +
  geom_sf(data = cost, aes(fill = .data$cost))


## ------------------------------------------------------------------------------------------------------
out_sf <- features %>%
  left_join(cost %>% sf::st_drop_geometry(), by = "cellID")


## ------------------------------------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------
# Extract feature names
col_name <- features %>%
  select(-"cellID") %>%
  st_drop_geometry() %>%
  colnames()


## ------------------------------------------------------------------------------------------------------
# Same target for all
targets <- rep(0.3, length(col_name))


# Assign higher target for species with tracking data
targets <- data.frame(feature = col_name) %>%
  mutate(Category = c(rep("Geomorphic", 12), rep("Tracking Data", 5))) %>%
  mutate(target = if_else(Category == "Tracking Data", 50 / 100, 5 / 100))


## ------------------------------------------------------------------------------------------------------
dat_problem <- problem(out_sf,
                       features = col_name,
                       cost_column = "cost"
) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)


## ------------------------------------------------------------------------------------------------------
dat_soln <- dat_problem %>%
  solve.ConservationProblem()


## ----eval=FALSE----------------------------------------------------------------------------------------
## saveRDS(dat_soln, file.path("Output", "Solution1.rds"))


## ------------------------------------------------------------------------------------------------------
# Plot solution with a function we have defined (i.e., it is not in prioritizr)
# This makes a prettier plot than using the default plot function in prioritizr
(gg_sol <- splnr_plot_Solution(dat_soln))


## ------------------------------------------------------------------------------------------------------
targ_coverage <- eval_target_coverage_summary(dat_problem, dat_soln[, "solution_1"])


## ------------------------------------------------------------------------------------------------------
soln <- dat_soln %>%
  as_tibble()

# Ferrier score
ferrier <- eval_ferrier_importance(dat_problem, soln[, "solution_1"]) %>%
  select("total") %>%
  mutate(geometry = dat_soln$geometry) %>%
  rename(score = "total") %>%
  st_as_sf()


## ----eval=FALSE----------------------------------------------------------------------------------------
## # Rarity weighted richness
## rwr <- eval_rare_richness_importance(dat_problem, soln[, "solution_1"]) %>%
##   mutate(geometry = soln$geometry) %>%
##   rename(score = "rwr") %>%
##   st_as_sf()
## 
## # Replacement cost
## replacement <- eval_replacement_importance(dat_problem, soln[, "solution_1"]) %>%
##   mutate(geometry = soln$geometry) %>%
##   drename(score = "rc") %>%
##   st_as_sf()


## ------------------------------------------------------------------------------------------------------
#prep data to allow to see the results better
quant99fs <- round(stats::quantile(ferrier$score, 0.99), 4)
ferrier$score[ferrier$score >= quant99fs] <- quant99fs

# plot results
ggplot() +
  geom_sf(data = ferrier, aes(fill = .data$score), colour = NA)


## ------------------------------------------------------------------------------------------------------
dat_soln_portfolio <- dat_problem %>%
  add_cuts_portfolio(5) %>% # create a portfolio of solutions
  solve.ConservationProblem()


## ------------------------------------------------------------------------------------------------------
selFreq <- dat_soln_portfolio %>% # calculate selection frequency
  st_drop_geometry() %>%
  mutate(selFreq = as.factor(rowSums(
   select(., starts_with("solution_"))
  ))) %>%
  st_as_sf(geometry = dat_soln_portfolio$geometry) %>%
  select(selFreq)

ggplot() +
  geom_sf(data = selFreq, aes(fill = .data$selFreq), colour = NA)



## ------------------------------------------------------------------------------------------------------
# Extract feature names

col_name <- features %>%
  select(-"cellID") %>%
  st_drop_geometry() %>%
  colnames()

# Create targets object

# Assign higher target for species with tracking data
targets <- data.frame(feature = col_name) %>%
  mutate(Category = c(rep("Geomorphic", 12), rep("Tracking Data", 5))) %>%
  mutate(target = if_else(Category == "Tracking Data", 50 / 100, 5 / 100))


## ------------------------------------------------------------------------------------------------------
datEx_problem <- problem(out_sf,
                         features = col_name,
                         cost_column = "cost"
) %>%
  add_min_set_objective() %>% # Add objective function
  add_relative_targets(targets$target) %>% # specify the column with targets
  add_binary_decisions() %>% # Binary answer (Yes/No)
  add_default_solver(verbose = FALSE) # Add solver

# Solve conservation problem
datEx_soln <- datEx_problem %>%
  solve.ConservationProblem() # Solve the problem


## ------------------------------------------------------------------------------------------------------
(gg_soln <- splnr_plot_Solution(datEx_soln) +
   geom_sf(data = landmass, colour = "black", fill = "black", show.legend = FALSE) +
   coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))


## ------------------------------------------------------------------------------------------------------
# How were the targets met?
targetsMet <- eval_feature_representation_summary(
  datEx_problem, 
  datEx_soln[, "solution_1"])


## ------------------------------------------------------------------------------------------------------
datEx_problem_P <- problem(out_sf,
                           features = col_name,
                           cost_column = "cost"
) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_boundary_penalties(0.5) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.5, verbose = FALSE) # Larger optimality gap to speed up code


## ----solve_bp------------------------------------------------------------------------------------------
datEx_soln_P <- datEx_problem_P %>%
  solve.ConservationProblem()


## ------------------------------------------------------------------------------------------------------
(gg_solnP <- splnr_plot_Solution(datEx_soln_P) +
   geom_sf(data = landmass, 
           colour = "black", fill = "black", show.legend = FALSE) +
   coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))


## ------------------------------------------------------------------------------------------------------
# Add another cost for area
out_sf <- out_sf %>%
  mutate(cost_area = rep(1, nrow(.)))


## ------------------------------------------------------------------------------------------------------
datEx_problem_linP <- problem(out_sf,
                           features = col_name,
                           cost_column = "cost_area") %>%
  add_linear_penalties(10, data = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.2, verbose = FALSE) # we're adjusting the optimality gap here to speed up the process


## ------------------------------------------------------------------------------------------------------
datEx_soln_linP <- datEx_problem_linP %>%
  solve.ConservationProblem()


## ------------------------------------------------------------------------------------------------------
(gg_solnlinP <- splnr_plot_Solution(datEx_soln_linP) +
    geom_sf(data = landmass, colour = "black", fill = "black", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))


## ------------------------------------------------------------------------------------------------------
mpas <- readRDS(file.path(inputDat, "MPAs", "mpas.rds"))

# First look at the data
(gg_mpas <- ggplot() +
    geom_sf(data = mpas, aes(fill = .data$mpas), 
            colour = NA, size = 0.001, show.legend = FALSE))


## ------------------------------------------------------------------------------------------------------
datEx_problem_LIC <- problem(out_sf,
                             features = col_name,
                             cost_column = "cost"
) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_locked_in_constraints(as.logical(mpas$mpas)) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve conservation problem
datEx_soln_LIC <- datEx_problem_LIC %>%
  solve.ConservationProblem()

(gg_solnLIC <- splnr_plot_Solution(datEx_soln_LIC) +
    geom_sf(data = landmass, 
            colour = "black", fill = "black", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))


## ------------------------------------------------------------------------------------------------------
# Set up conservation problem
datEx_problem_LC <- problem(out_sf,
                            features = col_name,
                            cost_column = "cost"
) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_linear_constraints(sum(out_sf$cost_area) * 0.4, 
                         sense = "<=", out_sf$cost_area) %>% # set area-based budget
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve conservation problem
datEx_soln_LC <- datEx_problem_LC %>%
  solve.ConservationProblem()

(gg_solnLC <- splnr_plot_Solution(datEx_soln_LC) +
    geom_sf(data = landmass, 
            colour = "black", fill = "black", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))


## ------------------------------------------------------------------------------------------------------
# Targets
dfTarget <- splnr_get_featureRep(datEx_soln, datEx_problem,
                                 solnCol = "solution_1"
) %>%
  mutate(category = targets$Category)

(ggTarget <- splnr_plot_featureRep(
  df = dfTarget,
  nr = 1, showTarget = TRUE
))


## ------------------------------------------------------------------------------------------------------
# Set up conservation problem
datEx_problem_minS <- problem(out_sf,
                              features = col_name,
                              cost_column = "cost_area"
) %>%
  add_min_shortfall_objective(sum(out_sf$cost_area) * 0.05) %>%
  add_relative_targets(targets$target) %>% # specify column with targets
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve conservation problem
datEx_soln_minS <- datEx_problem_minS %>%
  solve.ConservationProblem()

(gg_solnminS <- splnr_plot_Solution(datEx_soln_minS) +
    geom_sf(data = landmass, 
            colour = "black", fill = "black", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

# Feature representation
dfTarget_minS <- splnr_get_featureRep(datEx_soln_minS, datEx_problem_minS,
                                      solnCol = "solution_1"
) %>%
  mutate(category = targets$Category)

(ggTargetminS <- splnr_plot_featureRep(
  df = dfTarget_minS,
  nr = 1, showTarget = TRUE
))


## ------------------------------------------------------------------------------------------------------
(ggTarget_comparison <- ggTarget + ggTargetminS +
   plot_layout(guides = "collect", axes = "collect"))


## ------------------------------------------------------------------------------------------------------
datEx_problem_maxU <- problem(out_sf,
                              features = col_name,
                              cost_column = "cost_area"
) %>%
  add_max_utility_objective(sum(out_sf$cost_area) * 0.05) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve conservation problem
datEx_soln_maxU <- datEx_problem_maxU %>%
  solve.ConservationProblem()

(gg_solnmaxU <- splnr_plot_Solution(datEx_soln_maxU) +
    geom_sf(data = landmass, 
            colour = "black", fill = "black", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

dfTarget_maxU <- splnr_get_featureRep(datEx_soln_maxU, datEx_problem_maxU,
                                      solnCol = "solution_1",
                                      maxUtility = TRUE
) %>%
  mutate(category = targets$Category)

(ggTargetmaxU <- splnr_plot_featureRep(
  df = dfTarget_maxU,
  nr = 1, showTarget = FALSE,
  maxUtility = TRUE
))


## ------------------------------------------------------------------------------------------------------
(ggTarget_comparison2 <- ggTarget + ggTargetminS + ggTargetmaxU +
   plot_layout(guides = "collect", axes = "collect"))


## ------------------------------------------------------------------------------------------------------
# Create Targets
targetsZones <- data.frame(feature = col_name) %>%
  mutate(Category = c(rep("Geomorphic", 12), rep("Tracking Data", 5))) %>%
  mutate(
    targetZ1 = dplyr::if_else(Category == "Tracking Data", 30 / 100, 0),
    targetZ2 = dplyr::if_else(Category != "Tracking Data", 10 / 100, 0)
  ) %>%
  dplyr::select("targetZ1", "targetZ2") %>%
  as.matrix()

# Create zones object
z1 <- prioritizr::zones("zone 1" = col_name, "zone 2" = col_name)

# Set up conservation problem
# NOTE: when using sf input, we need as many cost columns as we have zones
datEx_problem_zones <- prioritizr::problem(
  out_sf %>%
    mutate(
      Cost1 = rep(1, nrow(.)),
      Cost2 = rep(1, nrow(.))
    ),
  z1,
  cost_column = c("Cost1", "Cost2")
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(targetsZones) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

# Solve conservation problem
datEx_soln_zones <- datEx_problem_zones %>%
  prioritizr::solve.ConservationProblem()


## ------------------------------------------------------------------------------------------------------
(gg_soln_zones <- splnr_plot_Solution(
  datEx_soln_zones,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "#003366"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
) +
  geom_sf(data = landmass, 
          colour = "black", fill = "lightgrey", show.legend = FALSE) +
  coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

# Feature representation
targetsMet_zones <- datEx_soln_zones %>%
  dplyr::select(tidyselect::starts_with(c("solution"))) %>%
  sf::st_drop_geometry() %>%
  tibble::as_tibble() %>%
  prioritizr::eval_feature_representation_summary(datEx_problem_zones, .)

