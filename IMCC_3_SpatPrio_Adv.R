# IMCC7 Workshop:
# Conservation Planning for the marine environment: An introduction to tools and techniques
# Part 2: Spatial prioritisation: Advanced
# 14/10/2024
# Sandra Neubert (s.neubert@uq.edu.au)

# Run prioritisation
source("IMCC_1_PrepData.R")
source("IMCC_utils-functions.R")

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


# Create basic conservation problem -------------------------------------------
# Set up conservation problem
datEx_problem <- problem(out_sf,
  features = col_name,
  cost_column = "cost"
) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% # we have a data frame now, so need to specify the column with targets
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve conservation problem
datEx_soln <- datEx_problem %>%
  solve.ConservationProblem()

(gg_soln <- splnr_plot_Solution(datEx_soln) +
  geom_sf(data = landmass, colour = "black", fill = "black", show.legend = FALSE) +
  coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

# how were the targets met?
targetsMet <- eval_feature_representation_summary(datEx_problem, datEx_soln[, "solution_1"])

# there were a lot of individual PUs selected in the solution
# Can be problematic to include in an actual protected area

# Penalties ----------------------------------------------

# Penalties can be added to the conservation problem as a way to penalize specific conditions or metrics in the planning units.
# Penalizing certain criteria (e.g. certain environmental conditions, activities etc.) leads to a trade-off with the objective function which aims to minimise the cost.
# Increasing the "cost"  of a planning units through a penalty causes planning units that are not penalized over to be selected over those that are.

# Boundary penalty ----------------------------------------------
# Penalizes the number of boundaries in spatial plan
# Set up conservation problem
datEx_problem_P <- problem(out_sf,
  features = col_name,
  cost_column = "cost"
) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.2, verbose = FALSE) # we're adjusting the optimality gap here to speed up the process

# Solve conservation problem
datEx_soln_P <- datEx_problem_P %>%
  solve.ConservationProblem()

(gg_solnP <- splnr_plot_Solution(datEx_soln_P) +
    geom_sf(data = landmass, colour = "black", fill = "black", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

# selected PUs are more aggregated
# BUT: in an example with varying cost this might also lead to a more expensive solution

# Constraints ----------------------------------------------

# The conservation problems so far were very simplistic. In reality, it is often required to add more complexity.
# This can be done with constraints.

# Locked-in areas ----------------------------------------------
# One example for constraints is locking in specific areas, for example already existing MPAs
mpas <- readRDS(file.path(inputDat, "MPAs", "mpas.rds"))

# Look at data
(gg_mpas <- ggplot() +
  geom_sf(data = mpas, aes(fill = .data$mpas), colour = NA, size = 0.001, show.legend = FALSE))

# Set up conservation problem
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
    geom_sf(data = landmass, colour = "black", fill = "black", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

# Linear Constraints ----------------------------------------------

# Add another cost for area
out_sf <- out_sf %>%
  mutate(cost_area = rep(1, nrow(.)))

# Set up conservation problem
datEx_problem_LC <- problem(out_sf,
  features = col_name,
  cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_linear_constraints(sum(out_sf$cost_area) * 0.4, sense = "<=", out_sf$cost_area) %>% # set an area-based budget
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve conservation problem
datEx_soln_LC <- datEx_problem_LC %>%
  solve.ConservationProblem()

(gg_solnLC <- splnr_plot_Solution(datEx_soln_LC) +
    geom_sf(data = landmass, colour = "black", fill = "black", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

# Objective Functions ----------------------------------------------
# Minimum Set ----------------------------------------------
# Targets
dfTarget <- splnr_get_featureRep(datEx_soln, datEx_problem,
                                 solnCol = "solution_1") %>%
  mutate(category = targets$Category)

(ggTarget <- splnr_plot_featureRep(
  df = dfTarget,
  nr = 1, showTarget = TRUE
))

# Minimum shortfall ----------------------------------------------
# Set up conservation problem
datEx_problem_minS <- problem(out_sf,
                         features = col_name,
                         cost_column = "cost_area") %>%
  add_min_shortfall_objective(sum(out_sf$cost_area) * 0.05) %>%
  add_relative_targets(targets$target) %>% # we have a data frame now, so need to specify the column with targets
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve conservation problem
datEx_soln_minS <- datEx_problem_minS %>%
  solve.ConservationProblem()

(gg_solnminS <- splnr_plot_Solution(datEx_soln_minS) +
    geom_sf(data = landmass, colour = "black", fill = "black", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

# Feature representation
dfTarget_minS <- splnr_get_featureRep(datEx_soln_minS, datEx_problem_minS,
                                 solnCol = "solution_1") %>%
  mutate(category = targets$Category)

(ggTargetminS <- splnr_plot_featureRep(
  df = dfTarget_minS,
  nr = 1, showTarget = TRUE
))

# Compare how targets are met with minimum set and minimum shortfall
(ggTarget_comparison <- ggTarget + ggTargetminS +
  plot_layout(guides = 'collect', axes = "collect"))

# Different approach: Maximum utility ----------------------------------------------
# Set up conservation problem
datEx_problem_maxU <- problem(out_sf,
                              features = col_name,
                              cost_column = "cost_area") %>%
  add_max_utility_objective(sum(out_sf$cost_area) * 0.05) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve conservation problem
datEx_soln_maxU <- datEx_problem_maxU %>%
  solve.ConservationProblem()

(gg_solnmaxU <- splnr_plot_Solution(datEx_soln_maxU) +
    geom_sf(data = landmass, colour = "black", fill = "black", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

# Feature representation
dfTarget_maxU <- splnr_get_featureRep(datEx_soln_maxU, datEx_problem_maxU,
                                      solnCol = "solution_1",
                                      maxUtility = TRUE) %>%
  mutate(category = targets$Category)

(ggTargetmaxU <- splnr_plot_featureRep(
  df = dfTarget_maxU,
  nr = 1, showTarget = FALSE,
  maxUtility = TRUE
))

# Compare maximum utility with minimum set and minimum shortfall
(ggTarget_comparison2 <- ggTarget + ggTargetminS + ggTargetmaxU +
    plot_layout(guides = 'collect', axes = "collect"))

# Zones ----------------------------------------------
# We will now look how this spatial plan needs to be extended to include multiple
# management zones by using zones().

# Create Targets
targetsZones <- data.frame(feature = col_name) %>%
  mutate(Category = c(rep("Geomorphic", 12), rep("Tracking Data", 5))) %>%
  mutate(targetZ1 = dplyr::if_else(Category == "Tracking Data", 30 / 100, 0),
          targetZ2 = dplyr::if_else(Category != "Tracking Data", 10 / 100, 0)) %>%
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

(gg_soln_zones <- splnr_plot_Solution(
  datEx_soln_zones,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "#003366"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
) +
    geom_sf(data = landmass, colour = "black", fill = "lightgrey", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

# Feature representation
targetsMet_zones <- datEx_soln_zones %>%
  dplyr::select(tidyselect::starts_with(c("solution"))) %>%
  sf::st_drop_geometry() %>%
  tibble::as_tibble() %>%
  prioritizr::eval_feature_representation_summary(datEx_problem_zones, .)
