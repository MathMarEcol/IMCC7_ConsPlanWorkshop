# IMCC7 Workshop:
# Conservation Planning for the marine environment: An introduction to tools and techniques
# Part 2: Spatial prioritisation: Basic
# 14/10/2024
# Sandra Neubert (s.neubert@uq.edu.au)

# Preliminaries ------------------------------------------------------

source("IMCC_1_PrepData.R")
source("utils-functions.R")

# Extract feature names
col_name <- features %>%
  select(-"cellID") %>%
  st_drop_geometry() %>%
  colnames()

# Create targets object
# Same target for all
targets <- rep(0.3, length(col_name))

# Assign higher target for species with tracking data
targets <- data.frame(feature = col_name) %>%
  mutate(Category = c(rep("Geomorphic", 12), rep("Tracking Data", 5))) %>%
  mutate(target = if_else(Category == "Tracking Data", 50 / 100, 5 / 100))


# Create a conservation problem -------------------------------------------

# If you already have a solver in your machine, comment these out and make sure that the solver is loaded.

# If you are a Windows user, lpsympony might work better
# if (!require(remotes)) install.packages("remotes")
# remotes::install_bioc("lpsymphony")
# Check their website for more details: https://www.bioconductor.org/packages/release/bioc/html/lpsymphony.html
# library(lpsymphony)

# If you are a Mac/Linux, rcbc might work better
# if (!require(remotes)) install.packages("remotes")
# remotes::install_github("dirkschumacher/rcbc")
# Check their README for more details https://github.com/dirkschumacher/rcbc
# library(rcbc)
# library(lpsymphony)

dat_problem <- problem(out_sf,
                       features = col_name,
                       cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve conservation problem ----------------------------------------------

dat_soln <- dat_problem %>%
  solve.ConservationProblem()

saveRDS(dat_soln, file.path("Output", "Solution1.rds"))


# Plot the solution -------------------------------------------------------

# Plot solution with predefined function
(gg_sol <- splnr_plot_Solution(dat_soln))

ggsave(file.path("Figures", "gg_sol.png"),  width = 6, height = 8, dpi = 200)

# How well were targets met?

targ_coverage <- eval_target_coverage_summary(dat_problem, dat_soln[, "solution_1"])

# Irreplaceability -------------------------------------------------------

soln <- dat_soln %>%
  tibble::as_tibble()

ferrier <- prioritizr::eval_ferrier_importance(dat_problem, soln[, "solution_1"]) %>%
  dplyr::select("total") %>%
  dplyr::mutate(geometry = dat_soln$geometry) %>%
  dplyr::rename(score = "total") %>%
  sf::st_as_sf()

rwr <- prioritizr::eval_rare_richness_importance(dat_problem, soln[, "solution_1"]) %>%
  dplyr::mutate(geometry = soln$geometry) %>%
  dplyr::rename(score = "rwr") %>%
  sf::st_as_sf()

# replacement <- prioritizr::eval_replacement_importance(dat_problem, soln[, "solution_1"]) %>%
#   dplyr::mutate(geometry = soln$geometry) %>%
#   dplyr::rename(score = "rc") %>%
#   sf::st_as_sf()

quant99fs <- round(stats::quantile(ferrier$score, 0.99), 4)
ferrier$score[ferrier$score >= quant99fs] <- quant99fs

ggplot2::ggplot() +
  ggplot2::geom_sf(data = ferrier, ggplot2::aes(fill = .data$score), colour = NA)

quant99fs <- round(stats::quantile(rwr$score, 0.99), 4)
rwr$score[rwr$score >= quant99fs] <- quant99fs

ggplot2::ggplot() +
  ggplot2::geom_sf(data = rwr, ggplot2::aes(fill = .data$score), colour = NA)

# Portfolios (or no-regret areas) -------------------------------------------------------
dat_soln_portfolio <- dat_problem %>%
  prioritizr::add_cuts_portfolio(5) %>% # create a portfolio of solutions
  prioritizr::solve.ConservationProblem()

selFreq <- dat_soln_portfolio %>% # calculate selection frequency
  sf::st_drop_geometry() %>%
  dplyr::mutate(selFreq = as.factor(rowSums(
    dplyr::select(., dplyr::starts_with("solution_"))
  ))) %>%
  sf::st_as_sf(geometry = dat_soln_portfolio$geometry) %>%
  dplyr::select(selFreq)

ggplot2::ggplot() +
  ggplot2::geom_sf(data = selFreq, ggplot2::aes(fill = .data$selFreq), colour = NA)
