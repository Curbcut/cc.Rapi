#' Explore Graph Function
#'
#' This function creates a ggplot for the explore panel of Curbcut, based on the
#' input parameters.
#'
#' @param vars <`named list`> A list object of variable codes with classes. The
#' output of \code{\link{vars_build}}.
#' @param select_id <`character`> A string indicating the ID of the currently
#' selected region (if any). Usually `r[[id]]$select_id()`
#' @param data <`data.frame`> A data frame containing the variables and
#' observations. The output of \code{\link{data_get}}.
#' @param scale <`character`> Current scale. The output of
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param scales_as_DA <`character vector`> A character vector of `scales`
#' that should be handled as a "DA" scale, e.g. `building` and `street`. By default,
#' their graph will be the one of their DA.
#' @param lang <`character`> A character string indicating the language to
#' translate variable titles to.
#' @param font_family <`character`> A string specifying the font family for the
#' plot, default is "acidgrotesk-book".
#' @param ... Additional arguments passed to the specific method.
#'
#' @return A ggplot2 object representing the plot.
#' @export
explore_graph <- function(vars, select_id, scale, data, time, schemas, variables,
                          lang = NULL, font_family = "acidgrotesk-book",
                          ...) {
  UseMethod("explore_graph", vars)
}

#' @rdname explore_graph
#' @export
explore_graph.q5_ind <- function(vars, select_id, scale, data, time, schemas,
                                 variables, lang = NULL,
                                 font_family = "acidgrotesk-book", ...) {
  explore_graph_q5_ind(
    vars = vars, select_id = select_id, scale = scale,
    data = data, time = time, variables = variables,
    lang = lang, font_family = "acidgrotesk-book", ...
  )
}

#' @rdname explore_graph
#' @export
explore_graph.q5 <- function(vars, select_id, scale, data, time, schemas,
                             variables, lang = NULL,
                             font_family = "acidgrotesk-book", ...) {
  # Appease R CMD check
  var_left <- x <- ..count.. <- NULL

  # Grab the shared info between the graphs
  shared_info <- explore_graph_info(
    vars = vars, font_family = font_family,
    variables = variables, select_id = select_id,
    data = data, lang = lang, scale = scale, ...
  )

  # Color as function
  clr_df <- shared_info$colours_dfs$left_5
  clr <- \(x) clr_df$fill[2:6]

  rcol <- match_schema_to_col(data = data, time = time, schemas = schemas)

  # Keep the data inside the breaks
  vl_breaks <- attr(data, "breaks_var_left")
  data_inrange <- filter_inrange(
    data = data, col = rcol, range = vl_breaks,
    select_id = shared_info$select_id
  )

  # Get the scales ggplot function
  x_scale <- explore_graph_scale(
    var = vars$var_left,
    x_y = "x",
    data_vals = data_inrange[[rcol]]
  )

  # Graph an appropriate number of bins
  var_left_num <- length(unique(data_inrange[[rcol]]))
  bin_number <- min(15, ceiling(0.8 * var_left_num))

  # Get the breaks
  vals <- vl_breaks
  vals[1] <- -Inf
  vals[length(vals)] <- Inf

  # # Get the graph range
  # range <- if (attr(data_inrange, sprintf("updated_range_%s", rcol))) NULL else range(vl_breaks)

  # Check if data is empty or has only one unique value in the relevant column
  if (nrow(data_inrange) == 0 || length(unique(data_inrange[[rcol]])) <= 1) {
    return(NULL) # Exit function early if condition is met
  }

  # Draw the plot
  plot <-
    data_inrange |>
    ggplot2::ggplot(ggplot2::aes(!!ggplot2::sym(rcol))) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(..count.. / sum(..count..)),
                                         fill = ggplot2::after_stat(x)),
                            bins = bin_number
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::binned_scale(
      aesthetics = "fill",
      palette = clr,
      breaks = vals
    ) +
    # ggplot2::coord_cartesian(xlim = range) +
    x_scale +
    shared_info$labs +
    shared_info$theme_default

  # Add selection
  if (!is.na(shared_info$select_id)) {
    val <- data_inrange[[rcol]][data_inrange$ID == shared_info$select_id]
    if (!any(is.na(val))) {
      plot <-
        plot +
        ggplot2::geom_vline(
          xintercept = val,
          colour = "black", linewidth = 1.5
        )
    }
  }

  # Return
  return(plot)
}

#' @rdname explore_graph
#' @export
explore_graph.bivar <- function(vars, select_id, scale, data, time, schemas,
                                variables, lang = NULL,
                                font_family = "acidgrotesk-book", ...) {
  # Appease R CMD check
  var_left <- var_right <- group <- NULL

  # Grab the shared info between the graphs
  shared_info <- explore_graph_info(
    vars = vars, font_family = font_family,
    variables = variables, select_id = select_id,
    data = data, lang = lang, scale = scale, ...
  )
  # Color as function
  clr_df <- shared_info$colours_dfs$bivar

  # Remove outliers from data before getting the x-y labels
  vr_col <- match_schema_to_col(
    data = data,
    col = "var_right",
    time = time,
    schemas = schemas
  )
  vl_col <- match_schema_to_col(
    data = data,
    col = "var_left",
    time = time,
    schemas = schemas
  )
  vr_breaks <- attr(data, "breaks_var_right")
  vl_breaks <- attr(data, "breaks_var_left")

  # Remove out-of-bounds
  data_in_range <- filter_inrange(
    data = data, col = vr_col, range = vr_breaks,
    select_id = shared_info$select_id
  )
  data_in_range <- filter_inrange(
    data = data_in_range, col = vl_col, range = vl_breaks,
    select_id = shared_info$select_id
  )
  # If there aren't enough observations, revert back to automatic breaks
  if (nrow(data_in_range) < 10) {
    vr_breaks <- NULL
    vl_breaks <- NULL
    data_in_range <- data
  } else {
    # Update the ranges if the value is outside the range!
    if (attr(data_in_range, sprintf("updated_range_%s", vr_col))) vr_breals <- NULL
    if (attr(data_in_range, sprintf("updated_range_%s", vl_col))) vl_breals <- NULL
  }
  # Remove NAs
  data_in_range <- data_in_range[!is.na(data_in_range[[vr_col]]) & !is.na(data_in_range[[vl_col]]), ]

  # Get the scales ggplot function
  x_scale <- explore_graph_scale(
    var = vars$var_right,
    x_y = "x",
    scale = scale,
    data_vals = data_in_range[[vr_col]]
  )
  y_scale <- explore_graph_scale(
    var = vars$var_left,
    x_y = "y",
    scale = scale,
    data_vals = data_in_range[[vl_col]]
  )

  # Get the stat smooth line opacity
  opac_line <- abs(stats::cor(data_in_range[[vl_col]], data_in_range[[vr_col]], use = "complete.obs"))

  # Get the point size
  point_size <- if (nrow(data_in_range) > 1000) {
    0.5
  } else if (nrow(data_in_range) > 500) {
    1
  } else {
    2
  }

  # Grab group column
  group_col <- match_schema_to_z_col(
    data = data_in_range, time = time,
    col = "group", vl_vr = "var_left",
    schemas = schemas
  )


  # # Breaks range
  # if (!is.null(vr_breaks)) vr_breaks <- range(vr_breaks)
  # if (!is.null(vl_breaks)) vl_breaks <- range(vl_breaks)

  plot <-
    data_in_range |>
    ggplot2::ggplot(ggplot2::aes(!!ggplot2::sym(vr_col), !!ggplot2::sym(vl_col))) +
    explore_graph_point_jitter(
      dat = data_in_range, cols = c(vr_col, vl_col),
      ggplot2::aes(colour = !!ggplot2::sym(group_col)), size = point_size
    ) +
    ggplot2::stat_smooth(
      geom = "line", se = FALSE, method = "loess", span = 1,
      formula = y ~ x, alpha = opac_line
    ) +
    ggplot2::coord_cartesian(ylim = c(min(data_in_range[[vl_col]]), max(data_in_range[[vl_col]]))) +
    ggplot2::scale_colour_manual(values = stats::setNames(
      clr_df$fill, clr_df$group
    )) +
    # ggplot2::coord_cartesian(xlim = vr_breaks, ylim = vl_breaks) +
    x_scale +
    y_scale +
    shared_info$labs +
    shared_info$theme_default

  # Add selection
  if (!is.na(shared_info$select_id)) {
    val <- data_in_range[data_in_range$ID == shared_info$select_id, ]
    if (!any(is.na(val))) {
      plot <-
        plot +
        ggplot2::geom_point(
          data = val, shape = 21,
          colour = "white", fill = "black", size = 4
        )
    }
  }

  # Return
  return(plot)
}

#' @rdname explore_graph
#' @export
explore_graph.delta_ind <- function(vars, select_id, scale, data, time, schemas,
                                    variables, lang = NULL,
                                    font_family = "acidgrotesk-book", ...) {
  explore_graph_delta_ind(
    vars = vars, select_id = select_id, scale = scale,
    data = data, time = time, schemas = schemas,
    variables = variables, lang = lang,
    font_family = font_family, ...
  )
}

#' @rdname explore_graph
#' @param val <`numeric`> If the value is not part of `data`. It happens on raster
#' data where we show region values for the highest resolution possible, but we still
#' want to allow user to select grid cells of lower resolutions.
#' @export
explore_graph.delta <- function(vars, select_id, scale, data, time, schemas,
                                variables, lang = NULL,
                                font_family = "acidgrotesk-book", val = NULL, ...) {
  # Appease R CMD check
  var_left_1 <- var_left_2 <- group <- NULL

  # Grab the shared info between the graphs
  shared_info <- explore_graph_info(
    vars = vars, font_family = font_family,
    variables = variables, select_id = select_id,
    data = data, lang = lang, scale = scale, time = time, ...
  )

  # Color as function
  clr_df <- delta_which_colors(data)

  # Get the scales ggplot function
  ycol <- match_schema_to_col(data = data, time = time$var_left[2], col = "var_left", schemas = schemas)
  xcol <- match_schema_to_col(data = data, time = time$var_left[1], col = "var_left", schemas = schemas)


  x_scale <- explore_graph_scale(
    var = structure(vars$var_left,
      class = class(vars$var_left)
    ),
    x_y = "x",
    df = scale,
    data_vals = data[[xcol]]
  )
  y_scale <- explore_graph_scale(
    var = structure(vars$var_left,
      class = class(vars$var_left)
    ),
    df = scale,
    x_y = "y",
    data_vals = data[[ycol]]
  )

  # Get the point size
  point_size <- if (nrow(data) > 1000) {
    0.5
  } else if (nrow(data) > 500) {
    1
  } else {
    2
  }

  # Filter out rows where xcol, ycol, or group have NA or non-finite values
  filtered_data <- data[stats::complete.cases(data[, c(xcol, ycol, "group")]), ]
  filtered_data <- filtered_data[is.finite(rowSums(filtered_data[, c(xcol, ycol)])), ]

  # Draw plot
  plot <-
    filtered_data |>
    ggplot2::ggplot(ggplot2::aes(!!ggplot2::sym(xcol), !!ggplot2::sym(ycol)))

  plot <-
    plot +
    explore_graph_point_jitter(
      dat = plot$data, cols = c(xcol, ycol),
      ggplot2::aes(colour = group)
    ) +
    ggplot2::geom_smooth(
      se = FALSE, method = "lm", formula = y ~ x,
      colour = "black", linewidth = 0.5
    ) +
    ggplot2::scale_colour_manual(values = stats::setNames(
      clr_df$fill, clr_df$group
    )) +
    # ggplot2::coord_cartesian(xlim = range(attr(data, "breaks_var_left")),
    #                          ylim = range(attr(data, "breaks_var_left"))) +
    x_scale +
    y_scale +
    shared_info$labs +
    shared_info$theme_default

  if (!is.na(shared_info$select_id) | !is.null(val)) {
    dat <- if (!is.null(val)) {
      dat <- data.frame(matrix(ncol = 2, nrow = 1))
      names(dat) <- c(xcol, ycol)
      dat[1, xcol] <- val[1]
      dat[1, ycol] <- val[2]
      dat
    } else data[data$ID == shared_info$select_id, ]
    if (!any(is.na(dat))) {
      plot <-
        plot +
        ggplot2::geom_point(
          data = dat, shape = 21,
          colour = "white", fill = "black", size = 4
        )
    }
  }

  return(plot)
}

#' @rdname explore_graph
#' @export
explore_graph.delta_bivar <- function(vars, select_id, scale, data, time, schemas,
                                      variables, lang = NULL,
                                      font_family = "acidgrotesk-book", ...) {
  # Appease R CMD check
  var_left <- var_right <- group <- NULL

  # Grab the shared info between the graphs
  shared_info <- explore_graph_info(
    vars = vars, font_family = font_family,
    variables = variables, select_id = select_id,
    data = data, lang = lang, scale = scale, time = time, ...
  )

  # Color as function
  clr_df <- shared_info$colours_dfs$bivar

  # Get the scales ggplot function
  x_scale <- explore_graph_scale.pct(x_y = "x")
  y_scale <- explore_graph_scale.pct(x_y = "y")

  # Get the stat smooth line opacity
  opac_line <- abs(stats::cor(data$var_left, data$var_right, use = "complete.obs"))

  # Get the point size
  point_size <- if (nrow(data) > 1000) {
    0.5
  } else if (nrow(data) > 500) {
    1
  } else {
    2
  }

  # Which are the column names containing data?
  data_cols <- c("var_right", "var_left")

  # Draw plot
  plot <-
    data |>
    remove_outliers_df(cols = data_cols) |>
    ggplot2::ggplot(ggplot2::aes(var_right, var_left))

  plot <-
    plot +
    explore_graph_point_jitter(
      dat = plot$data, cols = data_cols,
      ggplot2::aes(colour = group), size = point_size
    ) +
    ggplot2::stat_smooth(
      geom = "line", se = FALSE, method = "loess", span = 1,
      formula = y ~ x, alpha = opac_line
    ) +
    ggplot2::coord_cartesian(ylim = c(min(data$var_left), max(data$var_left))) +
    ggplot2::scale_colour_manual(values = stats::setNames(
      clr_df$fill, clr_df$group
    )) +
    x_scale +
    y_scale +
    shared_info$labs +
    shared_info$theme_default

  # Add selection
  if (!is.na(shared_info$select_id)) {
    val <- data[data$ID == shared_info$select_id, ]
    if (!any(is.na(val))) {
      plot <-
        plot +
        ggplot2::geom_point(
          data = val, shape = 21,
          colour = "white", fill = "black", size = 4
        )
    }
  }

  # Return
  return(plot)
}

#' @rdname explore_graph
#' @export
explore_graph.bivar_ind <- function(vars, select_id, scale, data, time, schemas,
                                    variables, lang = NULL,
                                    font_family = "acidgrotesk-book", ...) {
  explore_graph_bivar_ind(
    vars = vars, select_id = select_id, scale = scale, data = data, time = time, schemas = schemas,
    variables = variables, lang = lang, font_family = "acidgrotesk-book", ...
  )
}
