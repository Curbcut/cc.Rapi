#' Get information (labels, breaks, themes, ...) for legend creation
#'
#' Given variables and optional arguments, this function computes the legend
#' labels and breaks, and returns them with a default theme for the legend.
#'
#' @param vars <`named list`> A list object with a pre-determined class. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param scale <`character`> Scale under study.
#' @param data <`data.frame`> The current data. The output of \code{\link{data_get}}.
#' @param ... Additional arguments to be passed to \code{\link{legend_labels}}
#' and \code{\link{legend_breaks}}, such as `lang`, `time`, ...
#'
#' @return A list with the legend labels and breaks computed by
#' \code{legend_labels()} and \code{legend_breaks()}, and a default theme for
#' the legend.
legend_get_info <- function(vars, font_family = "acidgrotesk-book",
                            scale, data, variables, ...) {
  labs_xy <- legend_labels(vars, variables = variables, ...)
  break_labs <- legend_breaks(vars, data = data, scale = scale,
                              variables = variables, ...)
  theme_default <- list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      text = ggplot2::element_text(family = font_family, size = 9),
      legend.position = "none",
      panel.grid = ggplot2::element_blank()
    )
  )
  colours_dfs <- colours_get()

  return(list(
    labs_xy = labs_xy,
    break_labs = break_labs,
    theme_default = theme_default,
    colours_dfs = colours_dfs
  ))
}

#' Generic legend render function for Curbcut legends
#'
#' `legend_render` is a generic function used to produce the legend for the
#' Curbcut map pages. The function invokes particular methods which depend on
#' the class of the `vars` argument.
#'
#' @param vars <`named list`> A list object with a pre-determined class. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param scale <`character`> Scale under study.
#' @param data <`data.frame`> The current data. The output of \code{\link{data_get}}.
#'
#' @param ... Arguments to be passed to the methods, e.g. optionally `lang`
#'
#' @return It returns a ggplot object
#' @export
legend_render <- function(vars, font_family = "acidgrotesk-book", scale,
                          data, variables, ...) {
  UseMethod("legend_render", vars)
}

#' @describeIn legend_render q5 method
#' @param data <`data.frame`> The current data. The output of \code{\link{data_get}}.
#' @export
legend_render.q5 <- function(vars, font_family = "acidgrotesk-book", scale,
                             data, variables, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  group <- y <- fill <- xmin <- xmax <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    font_family = font_family,
    scale = scale, data = data, variables = variables, ...
  )

  # Adapt breaks to add the `NA` bar
  leg <- leg_info$colours_dfs$left_5[2:6, 2:3]

  # Get real q5 breaks
  brks <- attr(data, "breaks_var_left") |> unlist()

  # If all NA, don't bother draw a legend. Return NULL
  if (all(is.na(brks))) {
    return(NULL)
  }

  # Complete the xmin and xmax
  leg$xmin <- brks[1:(length(brks) - 1)]
  leg$xmax <- brks[2:(length(brks))]

  # Tweak the min and max breaks so that they take a minimum of 15% of the
  # plot space
  rect_size <- leg$xmax - leg$xmin
  size_pct <- rect_size / sum(rect_size)

  # Go over each value (that isn't the blank space or the NA) and make sure it
  # takes at least 15% of the plot space. Reduce the size of the other rectangle
  # that are larger than 15% of the plot space.
  rect_pct_vals <- size_pct

  if ((sum(rect_pct_vals < 0.15) > 0)) {
    while (sum(rect_pct_vals < 0.15) > 0) {
      for (i in which(rect_pct_vals < 0.15)) {
        # How much it's increased
        inc <- 0.15 - rect_pct_vals[i]

        # Its new value
        rect_pct_vals[i] <- 0.15

        # Which other vars can be increased
        can_reduce <- which(rect_pct_vals > 0.15)
        can_reduce_vals <- rect_pct_vals[can_reduce]
        for (c in can_reduce) {
          prop_reduce <- rect_pct_vals[c] / sum(rect_pct_vals[can_reduce])

          rect_pct_vals[c] <- rect_pct_vals[c] - (inc * prop_reduce)
        }
      }
    }
  }

  # The new values for each rectangle that leads to a minimum of 15% plot space
  values_updated <- rect_pct_vals * (brks[length(brks)] - brks[1])
  # cumsum the values to get the xmax and xmax
  values_updated <- cumsum(values_updated)
  leg$xmax <- values_updated
  leg$xmin <- c(0, values_updated[1:(length(values_updated) - 1)])

  # Blank space addition
  blank <- (brks[length(brks)] - brks[1]) / 16
  leg <- rbind(
    data.frame(
      y = 1,
      fill = "#FFFFFFFF",
      xmin = leg$xmin[1] - blank,
      xmax = leg$xmin[1]
    ),
    leg
  )

  # NA (grey) space addition
  leg <- rbind(
    data.frame(
      y = 1,
      fill = "#B3B3BB",
      xmin = leg$xmin[1] - blank,
      xmax = leg$xmin[1]
    ),
    leg
  )

  # Breaks placement
  breaks_placement <- c(-(blank + (blank / 2)), 0, values_updated)

  # Grab labels to check length
  breaks_label <- leg_info$break_labs[!is.na(leg_info$break_labs)]
  breaks_label <- c("NA", breaks_label)
  if (length(breaks_placement) != length(breaks_label)) {
    warning(paste0(
      "The number of breaks is not the same as the number of break ",
      "labels. For a `q5` map, there needs to be 6 breaks in the ",
      "`variables$break_q5` table (for ranks 0:5)."
    ))
    return(NULL)
  }

  # Make the plot
  leg |>
    ggplot2::ggplot(
      ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = y - 1,
        ymax = y, fill = fill
      )
    ) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = breaks_placement,
      labels = breaks_label
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(leg$fill, leg$fill)) +
    leg_info$labs_xy +
    leg_info$theme_default
}

#' @describeIn legend_render q5_ind method
#' @export
legend_render.q5_ind <- function(vars, font_family = "acidgrotesk-book", scale,
                                 data, variables, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  group <- y <- fill <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    font_family = font_family,
    scale = scale, data = data, variables = variables, ...
  )

  # Adapt breaks to add the `NA` bar
  leg <- leg_info$colours_dfs$left_5[1:6, ]
  leg$group <- suppressWarnings(as.double(leg$group))
  leg[1, ]$group <- 0.5
  leg[seq(2 + 1, nrow(leg) + 1), ] <- leg[seq(2, nrow(leg)), ]
  leg[2, ] <- list(x = 0.75, y = 1, fill = "#FFFFFFFF")

  # Adjust break placements if breaks are characters
  breaks_placement <- c(-0.375, c(0:4) + 0.5)

  # Grab labels to check length
  breaks_label <- c("NA", leg_info$break_labs)
  if (length(breaks_placement) != length(breaks_label)) {
    warning(paste0(
      "The number of breaks is not the same as the number of break ",
      "labels.  For a `q5` map with a variable whose breaks are ",
      "characters, there must  be 5 breaks in the `variables$break_q5` ",
      "(for ranks 1:5) with the exception of rank 0 that can be NA, ",
      "which will be filtered out automatically."
    ))
    return(NULL)
  }

  # Make the plot
  leg |>
    ggplot2::ggplot(
      ggplot2::aes(
        xmin = group - 1, xmax = group, ymin = y - 1,
        ymax = y, fill = fill
      )
    ) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = breaks_placement,
      labels = breaks_label
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(leg$fill, leg$fill)) +
    leg_info$labs_xy +
    leg_info$theme_default
}

#' @describeIn legend_render qual method
#' @export
legend_render.qual <- function(vars, font_family = "acidgrotesk-book", scale, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  group <- y <- fill <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    font_family = font_family,
    scale = scale, ...
  )

  # Cut for the number of breaks
  leg_info$break_labs <- leg_info$break_labs[!is.na(leg_info$break_labs)]
  if (length(leg_info$break_labs) < nrow(leg_info$colours_dfs$qual)) {
    stop(paste0(
      "There are not enough colours in the qualitative colours ",
      "table `colours$qual`."
    ))
  }
  colours_qual <- leg_info$colours_dfs$qual[1:length(leg_info$break_labs), ]

  # Switch the `group` character vector to a numeric
  colours_qual$group <- suppressWarnings(as.double(colours_qual$group))

  # Make the plot
  colours_qual |>
    ggplot2::ggplot(ggplot2::aes(
      xmin = group - 1, xmax = group, ymin = y - 1,
      ymax = y, fill = fill
    )) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = colours_qual$group - 0.5,
      labels = leg_info$break_labs
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      colours_qual$fill, colours_qual$fill
    )) +
    leg_info$labs_xy +
    leg_info$theme_default
}

#' @describeIn legend_render bivar method
#' @param lang <`character`> String indicating the language to translate the
#' breaks to. Defaults to `NULL`, which is no translation.
#' @export
legend_render.bivar <- function(vars, font_family = "acidgrotesk-book",
                                scale, data, variables, lang = NULL, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  x <- y <- fill <- label <- label_colour <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    lang = lang, font_family = font_family, scale = scale, data = data, variables = variables, ...
  )

  # Prepare the grid's labels location and the colours
  leg <- leg_info$colours_dfs$bivar[1:9, ]
  leg$label <-
    c(
      cc_t(lang = lang, "Both low"), " ",
      paste0(leg_info$labs_xy$y_short, "\n", cc_t(lang = lang, "high only")),
      " ", " ", " ",
      paste0(leg_info$labs_xy$x_short, "\n", cc_t(lang = lang, "high only")),
      " ",
      cc_t(lang = lang, "Both high")
    )
  leg$label_colour <- c(rep("black", 8), "white")
  leg$x <- leg$x - 0.5
  leg$y <- leg$y - 0.5

  # Maker the plot
  ggplot2::ggplot(leg, ggplot2::aes(y, x, fill = fill)) +
    ggplot2::geom_raster() +
    ggplot2::geom_text(ggplot2::aes(y, x, label = label, colour = label_colour),
      inherit.aes = FALSE, size = 3
    ) +
    ggplot2::scale_x_continuous(breaks = 0:3, labels = leg_info$break_labs$x) +
    ggplot2::scale_y_continuous(breaks = 0:3, labels = leg_info$break_labs$y) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      leg_info$colours_dfs$bivar$fill[1:9], leg_info$colours_dfs$bivar$fill[1:9]
    )) +
    ggplot2::scale_colour_manual(values = c("black" = "black", "white" = "white")) +
    leg_info$labs_xy[[1]] +
    leg_info$theme_default
}

#' @describeIn legend_render delta method
#' @param time <`named list`> Object built using the \code{\link{vars_build}}
#' function. It contains the time for both var_left and var_right variables.
#' @export
legend_render.delta <- function(vars, font_family = "acidgrotesk-book",
                                scale, data, variables, time, lang = NULL, ...) {
  legend_render_delta(
    vars = vars, data = data, variables = variables, time = time, font_family = font_family,
    scale = scale, lang = lang, ...
  )
}

#' Internal function for dispatching `legend_render_delta`.
#'
#' @param vars <`named list`> A list object with a `delta` class. The
#' output of \code{\link{vars_build}}.
#' @param font_family <`character`> Which font family should be used to render
#' the legend (breaks, axis titles, ...). Defaults to `acidgrotesk-book`. To use
#' the default font family og ggplot2, use `NULL`.
#' @param scale <`character`> Scale under study.
#' @param data <`data.frame`> The current data. The output of \code{\link{data_get}}.
#' @param time <`named list`> Object built using the \code{\link{vars_build}}
#' function. It contains the time for both var_left and var_right variables.
#' @param ... Additional arguments passed to other functions.
#'
#' @return A ggplot object that represents the `delta` legend.
#' @export
legend_render_delta <- function(vars, font_family = "acidgrotesk-book",
                                scale, data, variables, time, lang = NULL, ...) {
  UseMethod("legend_render_delta", vars)
}

#' @describeIn legend_render_delta scalar method
#' @return A ggplot object representing the `delta` legend for scalar data.
legend_render_delta.scalar <- function(vars, font_family = "acidgrotesk-book",
                                       scale, data, variables, time, lang = NULL, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  group <- y <- fill <- xmin <- xmax <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    font_family = font_family,
    scale = scale, data = data, variables = variables, time = time,
    lang = lang, ...
  )

  # Adapt breaks to add the `NA` bar
  leg <- delta_which_colors(data)[1:5, 2:3]

  # Get breaks as numeric
  brks <- breaks_delta(vars = vars, scale = scale, character = FALSE, data = data)

  # If all NA, don't bother draw a legend. Return NULL
  if (all(is.na(brks))) {
    return(NULL)
  }

  # Complete the xmin and xmax
  leg$xmin <- brks[1:(length(brks) - 1)]
  leg$xmax <- brks[2:(length(brks))]

  # Tweak the min and max breaks so that they take a minimum of 15% of the
  # plot space
  rect_size <- leg$xmax - leg$xmin
  size_pct <- rect_size / sum(rect_size)

  # Go over each value (that isn't the blank space or the NA) and make sure it
  # takes at least 15% of the plot space. Reduce the size of the other rectangle
  # that are larger than 15% of the plot space.
  rect_pct_vals <- size_pct

  if ((sum(rect_pct_vals < 0.15) > 0)) {
    while (sum(rect_pct_vals < 0.15) > 0) {
      for (i in which(rect_pct_vals < 0.15)) {
        # How much it's increased
        inc <- 0.15 - rect_pct_vals[i]

        # Its new value
        rect_pct_vals[i] <- 0.15

        # Which other vars can be increased
        can_reduce <- which(rect_pct_vals > 0.15)
        can_reduce_vals <- rect_pct_vals[can_reduce]
        for (c in can_reduce) {
          prop_reduce <- rect_pct_vals[c] / sum(rect_pct_vals[can_reduce])

          rect_pct_vals[c] <- rect_pct_vals[c] - (inc * prop_reduce)
        }
      }
    }
  }

  # The new values for each rectangle that leads to a minimum of 15% plot space
  values_updated <- rect_pct_vals * (brks[length(brks)] - brks[1])
  # cumsum the values to get the xmax and xmax
  values_updated <- cumsum(values_updated)
  leg$xmax <- values_updated
  leg$xmin <- c(0, values_updated[1:(length(values_updated) - 1)])

  # Blank space addition
  blank <- (brks[length(brks)] - brks[1]) / 16
  leg <- rbind(
    data.frame(
      y = 1,
      fill = "#FFFFFFFF",
      xmin = leg$xmin[1] - blank,
      xmax = leg$xmin[1]
    ),
    leg
  )

  # NA (grey) space addition
  leg <- rbind(
    data.frame(
      y = 1,
      fill = "#B3B3BB",
      xmin = leg$xmin[1] - blank,
      xmax = leg$xmin[1]
    ),
    leg
  )

  # Breaks placement
  breaks_placement <- c(-(blank + (blank / 2)), 0, values_updated)

  # Grab labels to check length
  breaks_label <- leg_info$break_labs[!is.na(leg_info$break_labs)]
  breaks_label <- c("NA", breaks_label)
  if (length(breaks_placement) != length(breaks_label)) {
    warning(paste0(
      "The number of breaks is not the same as the number of break ",
      "labels. For a `q5` map, there needs to be 6 breaks in the ",
      "`variables$break_q5` table (for ranks 0:5)."
    ))
    return(NULL)
  }

  # Make the plot
  leg |>
    ggplot2::ggplot(
      ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = y - 1,
        ymax = y, fill = fill
      )
    ) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = breaks_placement,
      labels = breaks_label
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(leg$fill, leg$fill)) +
    leg_info$labs_xy +
    leg_info$theme_default
}

#' @describeIn legend_render_delta ordinal method
#' @param lang <`character`> Language in use. `en` or `fr`. Defaults to NULL.
#'
#' @return A ggplot object representing the `delta` legend for ordinal data.
legend_render_delta.ordinal <- function(vars, font_family = "acidgrotesk-book",
                                        scale, data, variables, lang = NULL, time, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  group <- y <- fill <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    font_family = font_family,
    scale = scale, time = time,
    lang = lang, data = data, variables = variables,
    ...
  )

  # Adapt breaks to add the `NA` bar
  leg <- rbind(
    data.frame(group = 0, y = 1, fill = "#B3B3BB"),
    leg_info$colours_dfs$delta[1:5, ]
  )
  leg$group <- suppressWarnings(as.double(leg$group))
  leg[1, ]$group <- 0.5
  leg[seq(2 + 1, nrow(leg) + 1), ] <- leg[seq(2, nrow(leg)), ]
  leg[2, ] <- list(x = 0.75, y = 1, fill = "#FFFFFFFF")

  # Adjust breaks and labels
  breaks <- c(-0.375, 0.75, 2.5, 4.25)
  labels <- c("NA", "Decrease", "No change", "Increase")
  labels <- sapply(labels, cc_t, lang = lang, USE.NAMES = FALSE)

  # Make the plot
  leg |>
    ggplot2::ggplot(
      ggplot2::aes(
        xmin = group - 1, xmax = group, ymin = y - 1,
        ymax = y, fill = fill
      )
    ) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = breaks,
      labels = labels
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(leg$fill, leg$fill)) +
    leg_info$labs_xy[[1]] +
    leg_info$theme_default
}


#' @describeIn legend_render q100 method
#' @export
legend_render.q100 <- function(vars, font_family = "acidgrotesk-book", scale, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  group <- y <- fill <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    font_family = font_family,
    scale = scale, ...
  )

  # Adapt breaks
  # leg <- leg_info$colours_dfs$viridis
  ### Instead of viridis, q100 now also uses the well known left_5 scale
  leg <- leg_info$colours_dfs$left_5[2:6, ]
  leg$group <- as.double(leg$group)

  # Make the plot
  leg |>
    ggplot2::ggplot(ggplot2::aes(
      xmin = group - 1, xmax = group, ymin = y - 1,
      ymax = y, fill = fill
    )) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = 0:5,
      labels = leg_info$break_labs
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(leg$fill, leg$fill)) +
    leg_info$labs_xy[[1]] +
    leg_info$theme_default
}

#' @describeIn legend_render delta_bivar method
#' @export
legend_render.delta_bivar <- function(vars, font_family = "acidgrotesk-book",
                                      scale, data, variables, time, lang = NULL, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  x <- y <- fill <- label <- label_colour <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    lang = lang, font_family = font_family, scale = scale,
    data = data, variables = variables, time = time, ...
  )

  # Prepare the grid's labels location and the colours
  leg <- leg_info$colours_dfs$bivar[1:9, ]
  leg$label <- c(
    cc_t(lang = lang, "Both low"), " ",
    paste0(leg_info$labs_xy$y_short, "\n", cc_t(lang = lang, "high only")),
    " ", " ", " ",
    paste0(leg_info$labs_xy$x_short, "\n", cc_t(lang = lang, "high only")),
    " ",
    cc_t(lang = lang, "Both high")
  )
  leg$label_colour <- c(rep("black", 8), "white")
  leg$x <- leg$x - 0.5
  leg$y <- leg$y - 0.5

  # Make the plot
  ggplot2::ggplot(leg, ggplot2::aes(y, x, fill = fill)) +
    ggplot2::geom_raster() +
    ggplot2::geom_text(ggplot2::aes(y, x, label = label, colour = label_colour),
      inherit.aes = FALSE, size = 3
    ) +
    ggplot2::scale_x_continuous(breaks = 0:3, labels = leg_info$break_labs$x) +
    ggplot2::scale_y_continuous(breaks = 0:3, labels = leg_info$break_labs$y) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      leg_info$colours_dfs$bivar$fill[1:9], leg_info$colours_dfs$bivar$fill[1:9]
    )) +
    ggplot2::scale_colour_manual(values = c("black" = "black", "white" = "white")) +
    leg_info$labs_xy[[1]] +
    leg_info$theme_default
}

#' @describeIn legend_render bivar_ldelta_rq3 method
#' @export
legend_render.bivar_ldelta_rq3 <- function(vars, font_family = "acidgrotesk-book",
                                           scale, data, variables, time, lang = NULL, ...) {
  # NULL out problematic variables for the R CMD check (no visible binding for
  # global variable)
  x <- y <- fill <- label <- label_colour <- NULL

  # Get all necessary information
  leg_info <- legend_get_info(vars,
    lang = lang, font_family = font_family, scale = scale,
    time = time, data = data, variables = variables, ...
  )

  # Prepare the grid's labels location and the colours
  leg <- leg_info$colours_dfs$bivar[1:9, ]
  leg$label <- c(
    cc_t(lang = lang, "Both low"), " ",
    paste0(leg_info$labs_xy$y_short, "\n", cc_t(lang = lang, "high only")),
    " ", " ", " ",
    paste0(leg_info$labs_xy$x_short, "\n", cc_t(lang = lang, "high only")),
    " ",
    cc_t(lang = lang, "Both high")
  )
  leg$label_colour <- c(rep("black", 8), "white")
  leg$x <- leg$x - 0.5
  leg$y <- leg$y - 0.5

  # Make the plot
  ggplot2::ggplot(leg, ggplot2::aes(y, x, fill = fill)) +
    ggplot2::geom_raster() +
    ggplot2::geom_text(ggplot2::aes(y, x, label = label, colour = label_colour),
      inherit.aes = FALSE, size = 3
    ) +
    ggplot2::scale_x_continuous(breaks = 0:3, labels = leg_info$break_labs$x) +
    ggplot2::scale_y_continuous(breaks = 0:3, labels = leg_info$break_labs$y) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      leg_info$colours_dfs$bivar$fill[1:9], leg_info$colours_dfs$bivar$fill[1:9]
    )) +
    ggplot2::scale_colour_manual(values = c("black" = "black", "white" = "white")) +
    leg_info$labs_xy[[1]] +
    leg_info$theme_default
}

#' @describeIn legend_render default method
#' @export
legend_render.default <- function(vars, ...) {
  return(NULL)
}
