# IMCC7 Workshop:
# Conservation Planning for the marine environment: An introduction to tools and techniques
# Helper functions from spatialplanr R package (mathmarecol.github.io/spatialplanr/)
# 14/10/2024
# Sandra Neubert (s.neubert@uq.edu.au)

splnr_get_PlanningUnits <- function(Bndry,
                                    InnerB,
                                    CellArea = 1000,
                                    Shape = "hexagon",
                                    inverse = FALSE) {
  if (Shape %in% c("hexagon", "Hexagon")) {
    sq <- FALSE
    diameter <- 2 * sqrt((CellArea * 1e6) / ((3 * sqrt(3) / 2))) * sqrt(3) / 2 # Diameter in m's
  }

  if (Shape %in% c("square", "Square")) {
    sq <- TRUE
    diameter <- sqrt(CellArea * 1e6) # Diameter in m's
  }

  # First create planning units for the whole region
  PUs <- sf::st_make_grid(Bndry,
                          square = sq,
                          cellsize = c(diameter, diameter),
                          what = "polygons"
  ) %>%
    sf::st_sf()

  # First get all the PUs partially/wholly within the planning region
  logi_Reg <- sf::st_centroid(PUs) %>%
    sf::st_intersects(Bndry) %>%
    lengths() > 0 # Get logical vector instead of sparse geometry binary

  PUs <- PUs[logi_Reg, ] # Get TRUE

  # Second, get all the pu's with < 50 % area on land (approximated from the centroid)
  logi_Ocean <- sf::st_centroid(PUs) %>%
    sf::st_intersects(InnerB) %>%
    lengths() > 0 # Get logical vector instead of sparse geometry binary

  if (inverse == FALSE) {
    PUs <- PUs[!logi_Ocean, ] # Get FALSE
  } else {
    PUs <- PUs[logi_Ocean == TRUE, ] # Get TRUE
  }

  PUs <- PUs %>%
    dplyr::mutate(cellID = dplyr::row_number()) # Add a cell ID reference

  return(PUs)
}


splnr_plot_Solution <- function(soln, colorVals = c("TRUE" = "#3182bd", "FALSE" = "#c6dbef"),
                                legendTitle = "Planning Units") {
  soln <- soln %>%
    dplyr::select("solution_1") %>%
    dplyr::mutate(solution_1 = as.logical(.data$solution_1)) # Making it logical helps with the plotting

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = .data$solution_1), colour = NA, size = 0.1, show.legend = TRUE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim) +
    ggplot2::scale_fill_manual(
      name = legendTitle,
      values = colorVals,
      labels = c("Not Selected", "Selected"),
      aesthetics = c("colour", "fill"),
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        nrow = 2,
        order = 1,
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    theme_bw()
}

splnr_get_featureRep <- function(soln, pDat, targetsDF = NA,
                                 climsmart = FALSE, climsmartApproach = 0, solnCol = "solution_1") {
  s_cols <- pDat$data$features[[1]]

  # Get data for features not chosen
  not_selected <- soln %>%
    dplyr::select(
      -tidyselect::starts_with(c("Cost", "solution_")),
      -tidyselect::any_of(c("metric", "cellID")),
      -tidyselect::any_of(s_cols)
    ) %>%
    sf::st_drop_geometry()

  ns_cols <- not_selected %>%
    colnames()

  if (length(ns_cols) > 0) {
    ns1 <- not_selected %>%
      dplyr::select(c(tidyselect::all_of(ns_cols))) %>%
      dplyr::mutate(solution = dplyr::pull(soln, !!rlang::sym(solnCol)))

    area_feature <- ns1 %>%
      dplyr::select(-c("solution")) %>%
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "feature", values_to = "total_amount") %>%
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(total_amount = sum(.data$total_amount))

    selected_feature <- ns1 %>%
      dplyr::filter(.data$solution == 1) %>%
      dplyr::select(-c("solution")) %>%
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "feature", values_to = "absolute_held") %>%
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(absolute_held = sum(.data$absolute_held))

    ns1 <- dplyr::left_join(area_feature, selected_feature, by = "feature") %>%
      dplyr::mutate(
        relative_held = (.data$absolute_held / .data$total_amount),
        incidental = TRUE
      )
  } else {
    ns1 <- tibble::tibble(
      feature = "DummyVar",
      total_amount = 0,
      absolute_held = 0,
      relative_held = 0,
      incidental = TRUE
    )
  }

  ## Now do the selected features

  s1 <- soln %>%
    dplyr::rename(solution = !!rlang::sym(solnCol)) %>%
    tibble::as_tibble()

  s1 <- prioritizr::eval_feature_representation_summary(pDat, s1[, "solution"]) %>%
    dplyr::select(-"summary")

  if (climsmart == TRUE & climsmartApproach == 1) {
    s1 <- s1 %>%
      dplyr::select(-.data$relative_held) %>%
      dplyr::mutate(
        feature = stringr::str_remove_all(.data$feature, "_CS"),
        feature = stringr::str_remove_all(.data$feature, "_NCS")
      ) %>% # Ensure all features have the same name.
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(
        total_amount = sum(.data$total_amount), # Sum the features together
        absolute_held = sum(.data$absolute_held)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(relative_held = .data$absolute_held / .data$total_amount) %>% # Calculate proportion
      dplyr::select(-"total_amount", -"absolute_held") %>% # Remove extra columns
      dplyr::left_join(targetsDF, by = "feature") #%>% # Add targets to df
    # dplyr::select(-"type")

  } else if (climsmart == TRUE & climsmartApproach == 3) {

    s1 <- s1 %>%
      dplyr::left_join(targetsDF, by = "feature")

  } else {
    # Add targets to df
    s1 <- s1 %>%
      dplyr::left_join(pDat$targets$data[["targets"]], by = "feature") %>%
      dplyr::select(-"type")
  }

  s1 <- s1 %>%
    dplyr::mutate(
      relative_held = .data$relative_held,
      incidental = FALSE
    ) %>%
    stats::na.omit()


  # Now join the selected and non-selected values
  if ((length(ns_cols) > 0)) { # Only if there are values in ns1
    df <- dplyr::bind_rows(s1, ns1)
  } else {
    df <- s1
  }

  return(df)
}

splnr_plot_featureRep <- function(df,
                                  nr = 1, showTarget = NA,
                                  plotTitle = "") {

  if (max(df$relative_held < 1)) {
    df <- df %>%
      dplyr::mutate(
        relative_held = .data$relative_held * 100,
        target = .data$target * 100
      )
  }

  uniqueCat <- unique(df$category[!is.na(df$category)])

  colr <- tibble::tibble(
    Category = uniqueCat,
    Colour = viridis::viridis(length(uniqueCat))
  ) %>%
    tibble::deframe()

  gg_target <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = df, stat = "identity", ggplot2::aes(x = .data$feature, y = .data$relative_held, fill = .data$category), na.rm = TRUE) +
    ggplot2::geom_bar(data = df %>% dplyr::filter(.data$incidental == TRUE), stat = "identity", ggplot2::aes(x = .data$feature, y = .data$relative_held), na.rm = TRUE, fill = "NA", colour = "black") +
    ggplot2::labs(title = plotTitle, x = "Feature", y = "Representation of features \nin total selected area (%)") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(limits = c(0, ymax <- max(df$relative_held, na.rm = TRUE) + 10), expand = c(0, 0)) + # only works for min shortfall without incidental yet
    ggplot2::scale_fill_manual(
      values = colr,
      guide = ggplot2::guide_legend(nrow = nr)
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 16, colour = "black"),
      axis.text.y = ggplot2::element_text(size = 16, colour = "black"),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = 16),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 16),
      legend.position.inside = c(0.5, 0.95),
      legend.direction = "horizontal",
      legend.background = ggplot2::element_rect(fill = "NA"),
      title = ggplot2::element_text(size = 16)
    )

  if (!(is.na(showTarget))) {
    gg_target <- gg_target +
      ggplot2::geom_point(data = df, ggplot2::aes(x = .data$feature, y = .data$target), shape = 3, size = 10, na.rm = TRUE)
  }

  return(gg_target)
}

