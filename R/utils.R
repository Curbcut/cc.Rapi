#' Generate English or French ordinal form of a number
#'
#' This function returns the English or French ordinal form of a number. The
#' ordinal form can be selected by the lang argument, which takes either
#' "en" for English or "fr" for French.
#'
#' @param lang <`character`> String indicating the language for the ordinal form,
#' either "en" or "fr".
#' @param x <`numeric`> The number to be transformed into ordinal form.
#' @param en_first <`character`> The English word for first
#' (default is `"first"`). Useful for place explorer where `ranks 'best'` is more
#' suitable.
#'
#' @return A character string, the ordinal form of x in the selected language.
#'
#' @examples
#' ordinal_form("en", 1) # "first"
#' ordinal_form("fr", 2) # "deuxième"
#' ordinal_form("en", 23) # "23rd"
#' @export
ordinal_form <- function(lang, x, en_first = "first") {
  if (!lang %in% c("en", "fr")) {
    stop("Only languages supported are `en` and `fr`")
  }
  if (!is.numeric(x)) {
    stop("`x` must be numeric")
  }

  # French
  if (lang == "fr") {
    return(switch(as.character(x),
                  "1" = "premier",
                  "2" = "deuxi\u00e8me",
                  "3" = "troisi\u00e8me",
                  paste0(as.character(x), "i\u00e8me")
    ))
  }

  # English
  if (x > 20) {
    if (x %% 100 %in% c(11, 12, 13)) {
      form <- "th "
    } else {
      form <- switch(as.character(x %% 10),
                     "1" = "st",
                     "2" = "nd",
                     "3" = "rd",
                     "th"
      )
    }
    paste0(x, form)
  } else {
    switch(as.character(x),
           "1" = en_first,
           "2" = "second",
           "3" = "third",
           "4" = "fourth",
           "5" = "fifth",
           "6" = "sixth",
           "7" = "seventh",
           "8" = "eighth",
           "9" = "ninth",
           "10" = "tenth",
           paste0(as.character(x), "th")
    )
  }
}

#' Compute the depth of a nested data structure
#'
#' This function returns the depth of a nested data structure. The depth of a
#' vector is the number of lists it is nested within. Atomic vectors have a
#' depth of 1, and NULL has a depth of 0. It is a reimplementation of
#' \code{\link[purrr]{vec_depth}} in base R.
#'
#' @param x A vector, a list or a NULL.
#'
#' @return An integer indicating the depth of the data structure.
#'
#' @examples
#' vec_dep(NULL) # 0
#' vec_dep(1:10) # 1
#' vec_dep(list(1, list(2, 3))) # 2
#'
#' @seealso The purrr package provides a vec_depth function with similar
#' functionality.
#'
#' @export
vec_dep <- function(x) {
  if (is.null(x)) {
    0L
  } else if (is.atomic(x)) {
    1L
  } else if (is.list(x)) {
    depths <- vapply(x, vec_dep, vector("integer", 1))
    1L + max(depths, 0L)
  } else {
    stop("`x` must be a vector")
  }
}

#' Calculate the ntile for a given vector of values
#'
#' This function divides a vector into n bins of equal size and returns the
#' bin number for each element in the vector. It is a reimplementation of
#' \code{\link[dplyr]{ntile}} in base R.
#'
#' @param x <`numeric vector`> Vector of numeric values
#' @param n <`integer`> An integer specifying the number of quantiles to
#' calculate
#'
#' @return An integer vector indicating the bin number for each element in x
#'
#' @seealso The dplyr package provides a ntile function with similar
#' functionality.
#'
#' @export
ntile <- function(x, n) {
  x <- rank(x, ties.method = "first", na.last = "keep")
  len <- length(x) - sum(is.na(x))
  if (len == 0L) {
    rep(NA_integer_, length(x))
  } else {
    n <- as.integer(floor(n))
    n_larger <- as.integer(len %% n)
    n_smaller <- as.integer(n - n_larger)
    size <- len / n
    larger_size <- as.integer(ceiling(size))
    smaller_size <- as.integer(floor(size))
    larger_threshold <- larger_size * n_larger
    bins <- ifelse(x <= larger_threshold,
                   (x + (larger_size - 1L)) / larger_size,
                   (x + (-larger_threshold + smaller_size - 1L)) /
                     smaller_size + n_larger
    )
    as.integer(floor(bins))
  }
}

#' Test if x scale is under study
#'
#' @param scales <`character vector`> All scales to test if `scale` is in them..
#' @param scale <`character`> Scale at which the user is on.
#' @param vectorized <`logical`> Should all elements of `scales` be evaluated
#' and return a logical vector the same length of `scales`?
#'
#' @return Returns TRUE or FALSE
#' @export
is_scale_in <- function(scales, scale, vectorized = FALSE) {
  if (!is.character(scale)) stop("`scale` cannot be empty.")

  if (!vectorized) {
    return(scale %in% scales)
  }

  scales == scale
}

#' Extract substrings from a character vector that matches a regular
#' expression pattern
#'
#' It is a reimplementation of \code{\link[stringr]{str_extract}} in base R.
#'
#' @param x <`character`> A vector from which to extract the substring
#' @param pattern <`character`> A regular expression pattern to match
#'
#' @return The substrings from \code{x} that matches \code{pattern}, or
#' \code{NA} if no matches are found
#' @export
#'
#' @examples
#' s_extract("hello", c("hello world", "goodbye")) # hello"
#' s_extract("[0-9]+", c("123abc", "def456")) # "123", "456"
s_extract <- function(pattern, x) {
  sapply(regmatches(x, regexec(pattern, x, perl = TRUE)), `[`, 1)
}

#' Extract all occurrences of a regular expression pattern from a character vector
#'
#' It is a reimplementation of \code{\link[stringr]{str_extract_all}} in base R.
#'
#' @param pattern <`character`> A regular expression pattern to match
#' @param x <`character`> A vector from which to extract the substring
#'
#' @return A character vector containing all occurrences of the regular
#' expression pattern found in the input vector.
#'
#' @details The function uses the `gregexpr` function to locate all occurrences of
#' the specified regular expression pattern in the input character vector `x`.
#' The resulting match positions are then passed to the `regmatches` function to
#' extract the matching substrings.
#'
#' @export
s_extract_all <- function(pattern, x) {
  unlist(regmatches(x, gregexpr(pattern, x, perl = TRUE)))
}

#' Capitalize the first letter of a sentence
#'
#' This function takes a string as input and returns the same string with the
#' first etter capitalized, assuming it is the first letter of a sentence. It does this by
#' extracting the first letter, capitalizing it, extracting the rest of the string,
#' and then combining the two parts.
#'
#' @param x <`character`> A character string to capitalize
#'
#' @return A character string with the first letter capitalized
#' @export
#'
#' @examples
#' s_sentence("hello world") # "Hello world"
s_sentence <- function(x) {
  # Extract the first letter and capitalize it
  first_letter <- toupper(substring(x, 1, 1))

  # Extract the rest of the string and leave it as is
  rest_of_string <- substring(x, 2)

  # Combine the first letter and the rest of the string
  capitalized_string <- paste(first_letter, rest_of_string, sep = "")

  # Return the result
  return(capitalized_string)
}

#' Build color palettes for data visualization
#'
#' This function generates a set of color palettes for data visualization
#' purposes. It returns a list containing six data frames that can be used
#' for setting color scales in rdeck, ggplot or other visualization packages.
#'
#' @param left_5 <`colours vector`> for a sequential scale of 5 values
#' for the left side of the color scale.
#' @param left_3 <`colours vector`> for a sequential scale of 3 values
#' for the left side of the color scale.
#' @param right_5 <`colours vector`> for a sequential scale of 5 values
#' for the right side of the color scale.
#' @param right_3 <`colours vector`> for a sequential scale of 3 values
#' for the right side of the color scale.
#' @param delta_5 <`colours vector`> scale used for variation of 5 values.
#' By defaults starts with red and go to blue.
#' @param delta_neg_5 <`colours vector`> scale used only when a delta only shows
#' negative values. Default choses using the first and second color of `delta_5`.
#' @param delta_pos_5 <`colours vector`>  scale used only when a delta only shows
#' positive values. Default choses using the fourth and fifth color of `delta_5`.
#' @param bivar <`colours vector`> for a bivariate scale of 9 values, by default
#' the stevens.greenblue palette.
#' @param qual <`colours vector`> for a qualitative scale of 6 values.
#' @param col_NA <`colours vector`> for missing values.
#' @param viridis <`colours vector`> for a viridis color scale.
#'
#' @return A list of data.frame with color scales to be used for the rdeck map,
#' legend and other plots.
#' @export
colours_get <- function(
    left_5 = c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448"),
    left_3 = c("#E8E8E8", "#B5C0DA", "#6C83B5"),
    right_5 = c("#C7DFCC", "#9DC6A6", "#73AE80", "#517A5A", "#2E4633"),
    right_3 = c("#E8E8E8", "#B8D6BE", "#73AE80"),
    delta_5 = c("#6D5271", "#DCD3E2", "#E8E8E8", "#f9e699", "#F69036"),
    delta_neg_5 = c(grDevices::colorRampPalette(delta_5[c(1, 2)])(4), "#E8E8E8"),
    delta_pos_5 = c("#E8E8E8", grDevices::colorRampPalette(delta_5[c(4, 5)])(4)),
    bivar = c(
      "#E8E8E8", "#B5C0DA", "#6C83B5", "#B8D6BE", "#90B2B3", "#567994",
      "#73AE80", "#5A9178", "#2A5A5B"
    ),
    qual = c(
      "#73AE80", "#6C83B5", "#5B362A", "#B58A6C", "#2A5A5B",
      "#AE7673"
    ),
    col_NA = c("#B3B3BB"),
    viridis = scales::viridis_pal()(25)) {
  # rdeck colours -----------------------------------------------------------

  c_NA <- data.frame(
    palette = "NA",
    group = "0",
    value = col_NA
  )

  c_q5 <- data.frame(
    palette = "q5",
    group = as.character(1:5),
    value = left_5
  )

  c_bivar <- data.frame(
    palette = "bivar",
    group = as.character(6:14),
    value = bivar
  )

  c_delta <- data.frame(
    palette = "delta",
    group = as.character(15:19),
    value = delta_5
  )

  c_qual <- data.frame(
    palette = "qual",
    group = as.character(20:25),
    value = qual
  )

  c_viridis <- data.frame(
    palette = "viridis",
    group = as.character(26:50),
    value = viridis
  )

  colour_table <-
    do.call(rbind, list(c_NA, c_q5, c_bivar, c_delta, c_qual, c_viridis))


  # Other -------------------------------------------------------------------

  left_5 <-
    data.frame(
      group = c(0:5, "NA"),
      y = 1,
      fill = c(col_NA, left_5, col_NA)
    )

  bivar_colors <- c(bivar, rep(col_NA, 7))
  bivar <-
    data.frame(
      group = c(
        "1 - 1", "2 - 1", "3 - 1",
        "1 - 2", "2 - 2", "3 - 2",
        "1 - 3", "2 - 3", "3 - 3",
        "NA - 1", "NA - 2", "NA - 3",
        "1 - NA", "2 - NA", "3 - NA",
        "NA - NA"
      ),
      x = c(rep(c(1, 2, 3), 3), rep(NA, length(bivar_colors) - 9)),
      y = c(
        unlist(lapply(c(1, 2, 3), rep, 3)),
        rep(NA, length(bivar_colors) - 9)
      ),
      fill = c(bivar, rep(col_NA, 7))
    )

  delta <- data.frame(
    group = c(1:5, "NA"),
    y = 1,
    fill = c(delta_5, col_NA)
  )

  delta_neg <- data.frame(
    group = c(1:5, "NA"),
    y = 1,
    fill = c(delta_neg_5, col_NA)
  )

  delta_pos  <- data.frame(
    group = c(1:5, "NA"),
    y = 1,
    fill = c(delta_pos_5, col_NA)
  )

  qual <- data.frame(
    group = as.character(seq_along(qual) - 1),
    y = 1,
    fill = qual
  )

  variant_5 <- data.frame(group = as.character(1:5), y = 1, fill = right_5)

  viridis <- data.frame(
    group = as.character(1:10), y = 1,
    fill = scales::viridis_pal()(10)
  )


  # Return ------------------------------------------------------------------

  return(list(
    table = colour_table,
    left_5 = left_5,
    bivar = bivar,
    delta = delta,
    delta_neg = delta_neg,
    delta_pos = delta_pos,
    variant_5 = variant_5,
    qual = qual,
    viridis = viridis
  ))
}


#' Treat data frames as a DA (DA) scale
#'
#' This function takes a list of scales that should be treated like DAs.
#' If they should, the function appends "_DA" to the `df` string instead of the
#' current scale, and returns the new name. If not, the original `df` is returned.
#'
#' @param scales_as_DA <`character vector`> dfs to check if they should be
#' treated like a DA scale
#' @param scale <`character`> The `scale` to check if it is a DA scale
#' @return If the current `df` is part of the scales that should be treated
#' as a DA, thye function appends "_DA" to the `df` string instead of the
#' current scale, and returns the new name. If not, the original `df` is returned.
#' @export
treat_to_DA <- function(scales_as_DA, scale) {
  if (length(scales_as_DA) == 0) {
    return(scale)
  }
  if (is_scale_in(scales_as_DA, scale)) {
    return("DA")
  }
  return(scale)
}

#' Get object from global environment
#'
#' This function retrieves an object from the global environment by the name of
#' the object. If the object is not found in the global environment, an error is
#' thrown.
#'
#' @param x <`character`> The name of the object to retrieve from the global
#' environment.
#' @param stop_if_missing <`logical`> Should it trigger an error if the object
#' is missing?
#'
#' @return The requested object from the global environment.
#' @export
get_from_globalenv <- function(x, stop_if_missing = TRUE) {
  out <- get0(x, envir = .GlobalEnv)

  if (stop_if_missing & is.null(out)) {
    stop(glue::glue_safe("`{x}` object not found in the global environment."))
  }
  return(out)
}

#' Calculate the distance between two points on the Earth's surface using the
#' Haversine formula
#'
#' The function calculates the distance between two points on the Earth's
#' surface using the Haversine formula. If \code{x} is a matrix or data frame,
#' the function extracts the longitude and latitude vectors for each point.
#' The latitude values are converted from degrees to radians, and the function
#' calculates the central angle between the two points using the Haversine formula.
#' The distance between the points in meters is then calculated and returned.
#'
#' @param x A vector or matrix/data frame containing the longitude and latitude
#' of one or more points
#' @param y A vector containing the longitude and latitude of a single point
#'
#' @return The distance between the two points in meters
#' @export
get_dist <- function(x, y) {
  # For consistent indexing
  if (!is.null(dim(x))) x <- as.matrix(x)
  # If x is matrix or df, take lon/lat vectors
  lon_1 <- if (!is.null(dim(x))) x[, 1] else x[1]
  lat_1 <- if (!is.null(dim(x))) x[, 2] else x[2]
  lon_2 <- y[1]
  lat_1_r <- lat_1 * pi / 180
  lat_2 <- y[2]
  lat_2_r <- lat_2 * pi / 180
  delta_lat <- (lat_2 - lat_1) * pi / 180
  delta_lon <- (lon_2 - lon_1) * pi / 180
  a_dist <- sin(delta_lat / 2) * sin(delta_lat / 2) + cos(lat_1_r) *
    cos(lat_2_r) * sin(delta_lon / 2) * sin(delta_lon / 2)
  c_dist <- 2 * atan2(sqrt(a_dist), sqrt(1 - a_dist))
  6371e3 * c_dist
}

#' Whether a value can be transformed to numeric
#'
#' Purpose mostly for the bookmark. To detect if "2" can be numeric.
#'
#' @param x <`character`> String to detect if a transformation to numeric
#' is possible.
#'
#' @return Logical indicating whether the value can be numeric or not.
is_numeric <- function(x) {
  # Convert the input value to a numeric type using as.numeric()
  # Check whether the conversion was successful using is.na()
  # If the conversion was successful, the value is numeric
  !is.na(suppressWarnings(as.numeric(x)))
}

#' Find and remove outliers in specified columns of a data frame.
#'
#' This function package consists of two functions: find_outliers() and
#' remove_outliers_df(). find_outliers() identifies outliers in a numeric vector
#' using the 1.5 * IQR rule. remove_outliers_df() removes outliers in specified columns
#' of a data frame, preserving the original row indices.
#'
#' @param x <`numeric`> A numeric vector for which to find outliers.
#' @param lower_bracket <`numeric`> The lower percentile to use for the
#' IQR calculation. Defaults to 0.02.
#' @param higher_bracket <`numeric`> The higher percentile to use for the
#' IQR calculation. Defaults to 0.98.
#' @param iqr_multiplier <`numeric`> Multiplier for the IQR to determine
#' outlier thresholds. Defaults to 1.5.
#'
#' @return For find_outliers(): A vector of indices of the outliers in x.
#' For remove_outliers_df(): A data frame with outliers removed from specified
#' columns.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5, 100)
#' find_outliers(x)
#'
#' df <- data.frame(a = c(1, 2, 3, 4, 5, 100), b = c(1, 2, 3, 4, 5, 6))
#' remove_outliers_df(df, cols = "a")
#'
#' @export
find_outliers <- function(x, lower_bracket = 0.02, higher_bracket = 0.98, iqr_multiplier = 1.5) {
  q1 <- stats::quantile(x, lower_bracket, na.rm = TRUE)
  q3 <- stats::quantile(x, higher_bracket, na.rm = TRUE)
  iqr <- (q3 - q1) * iqr_multiplier
  which(x < q1 - iqr | x > q3 + iqr)
}

#' @rdname find_outliers
#' @param df <`data.frame`> A data frame from which to remove outliers.
#' @param cols <`character vector`> Column names in df for which to remove o
#' utliers.
#' @export
remove_outliers_df <- function(df, cols) {
  outliers <- lapply(cols, \(c) {
    nas <- which(is.na(df[[c]]))
    outliers <- find_outliers(df[[c]])
    remove <- unique(c(nas, outliers))
    return(remove)
  })

  remove <- Reduce(c, outliers)
  remove <- unique(remove)

  out <- if (length(remove) > 0) df[-remove, ] else df

  return(out)
}

#' Calculate Weighted Mean
#'
#' This function computes the weighted mean of a numeric vector, with the option
#' to remove NA values on both x and w. The calculation is based on weights provided.
#'
#' @param x <`numeric vector`> A numeric vector for which the weighted mean is
#' to be computed.
#' @param w <`numeric vector`> A numeric vector of weights, where each weight
#' corresponds to the elements in `x`. The length of `w` should match the
#' ength of `x`.
#' @param ... Additional arguments passed to `stats::weighted.mean`.
#' @param na.rm <`logical`> A logical value indicating whether NA values in `x`
#' and `w` should be stripped before the computation proceeds. Defaults to
#' `FALSE`.
#'
#' @return A numeric value representing the weighted mean of the elements in `x`.
#'
#' @examples
#' x <- c(1, 2, 3, 4, NA)
#' w <- c(1, 2, 3, 4, 5)
#' weighted_mean(x, w) # NA due to NA in `x`
#' weighted_mean(x, w, na.rm = TRUE) # weighted mean excluding NA
#'
#' @export
weighted_mean <- function(x, w, ..., na.rm = FALSE) {
  # Check if the lengths of x and w are equal
  if (length(x) != length(w)) {
    stop("Lengths of 'x' and 'w' must be equal.")
  }

  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    w <- w[keep]
    x <- x[keep]
  }
  stats::weighted.mean(x, w, ..., na.rm = na.rm)
}

#' Grab DA_ID from a building-street-like dataframe (large!)
#'
#' This function fetches the DA_ID matching with the selected ID from a given
#' dataframe `scale` based on a provided `select_id`. It can also fetch the DA_ID
#' from a connected database when the `df` is not found in the global environment
#' but is identified as a scales_as_DA'. In such cases, it uses the established
#' connection and fetches the DA_ID via a SQL query.
#'
#' @param scale <`character`> A string, the name of the dataframe in which to look
#' for row. The dataframe should be in the global environment, and if it isn't,
#' there must be an established sqlite connection to it.
#' @param select_id <`character`> A value representing the ID that needs to be
#' selected in order to fetch the corresponding DA_ID.
#'
#' @return The DA_ID corresponding to the given `select_id`. If
#' `select_id` is found in the `df` in the global environment, the corresponding
#' DA_ID is returned. If `df` is a 'scales_as_DA' and not in the global
#' environment, the function fetches DA_ID from the connected database.
grab_DA_ID_from_bslike <- function(scale, select_id) {
  dat <- get0(scale, envir = .GlobalEnv)
  # If it's a 'scales_as_DA', and the `df` is not in the global environment,
  # search for a connection.
  if (is.null(dat)) {
    out <- db_get(select = "da_id", from = scale, where = list(id = select_id))
    out <- unname(unlist(out))

    # If length is zero, it means the selection was for another scale before,
    # and the user zoomed on building. Return as if it's NA.
    if (length(out) == 0) out <- NA
  } else {
    out <- dat$da_id[dat$id == select_id]
  }

  return(out)
}

#' Grab full dataframe from a building-street-like dataframe (large!)
#'
#' This function fetches the full dataframe matching with `df`. It can also fetch
#' the full dataframe from a connected database when the `df` is not found in the
#' global environment.
#'
#' @param scale <`character`>  A string, the name of the dataframe in which to look
#' for full dataframe. The dataframe should be in the global environment, and if it isn't,
#' there must be an established sqlite connection to it.
#'
#' @return The full datraframe corresponding to the given `scale`. If
#' `select_id` is found in the `scale` in the global environment, the corresponding
#' table is returned. If `scale` is not in the global
#' environment, the function fetches the dataframe from the connected database.
grab_df_from_bslike <- function(scale) {
  dat <- get0(scale, envir = .GlobalEnv)
  if (!is.null(dat)) {
    return(dat)
  }

  # If not in the global environment
  out <- db_get(select = "*", from = scale)

  return(out)
}

#' Retrieve a specific page
#'
#' This function retrieves a specific page based on the provided `ns_id`.
#' It substracts the namespace doubling noise and retrieves the page from
#' the modules stored in the global environment.
#'
#' @param ns_id <`character`> ID of the page identifier including the namespace.
#'
#' @return A subset of the `modules` global data frame matching the `ns_id`.
#' @export
page_get <- function(ns_id) {
  # Grab the modules table
  modules <- get_from_globalenv("modules")

  # Grab only the `id` (take out namespace doubling noise)
  solo_id <- gsub("-.*$", "", ns_id)

  # Subset the current page
  page <- modules[modules$id == solo_id, ]

  # Return
  return(page)
}

#' Convert a HEX6 color code to an RGB color string
#'
#' This function takes a HEX6 color code and converts it to an RGB string. The
#' HEX6 code must include the hash symbol (#) at the beginning.
#'
#' @param hex6 <`character`> A string representing the HEX6 color code (e.g.
#' "#FF5733").
#'
#' @return A string representing the RGB color (e.g. "rgb(255, 87, 51)").
#' @export
hex6_to_rgb <- function(hex6) {
  # Extract the RGB components
  r <- strtoi(substr(hex6, 2, 3), 16L)
  g <- strtoi(substr(hex6, 4, 5), 16L)
  b <- strtoi(substr(hex6, 6, 7), 16L)

  # Return as rgb string
  return(paste0("rgb(", r, ", ", g, ", ", b, ")"))
}

#' Convert a HEX8 color code to an RGBA color string
#'
#' This function takes a HEX8 color code (including alpha channel) and converts
#' it to an RGBA string. The HEX8 code must include the hash symbol (#) at the
#' beginning.
#'
#' @param hex8 <`character`> A string representing the HEX8 color code (e.g.
#' "#FF5733FF").
#'
#' @return A string representing the RGBA color (e.g. "rgba(255, 87, 51, 1)").
#' @export
hex8_to_rgba <- function(hex8) {
  # Extract the RGB and alpha components
  r <- strtoi(substr(hex8, 2, 3), 16L)
  g <- strtoi(substr(hex8, 4, 5), 16L)
  b <- strtoi(substr(hex8, 6, 7), 16L)
  a <- strtoi(substr(hex8, 8, 9), 16L)

  # Convert alpha from 0-255 to 0-1 scale
  a <- round(a / 255, 2)

  # Return as rgba string
  return(paste0("rgba(", r, ", ", g, ", ", b, ", ", a, ")"))
}

#' Convert a HEX color code to an RGB or RGBA color string
#'
#' This function takes a HEX color code and converts it to either an RGB or
#' RGBA string based on its length. HEX6 codes are converted to RGB, while
#' HEX8 codes are converted to RGBA.
#'
#' @param hex <`character`> A string representing the HEX color code. It can be
#' either HEX6 (e.g. "#FF5733") or HEX8 (e.g. "#FF5733FF").
#'
#' @return A string representing the RGB or RGBA color.
#' @export
hex_to_rgb_or_rgba <- function(hex) {
  # Determine if the input is HEX6 or HEX8
  len <- nchar(hex)

  if (len == 7) { # HEX6
    return(hex6_to_rgb(hex))
  } else if (len == 9) { # HEX8
    return(hex8_to_rgba(hex))
  } else {
    stop("Invalid HEX code length. Must be HEX6 or HEX8.")
  }
}

#' Match schema list to right column in `data`
#'
#' This function identifies the column in a `data` (output of \code{\link{data_get}})
#' that matches a given schema named list. It is designed to be used in situations where
#' the schema for the data frame can be dynamic. For example, if the schema is
#' based on the time of the data, this function can be used to identify the
#' column in the data frame that corresponds to the given time.
#'
#' @param data <`data.frame`> A data frame containing the data. The output of
#' output of \code{\link{data_get}}.
#' @param time <`numeric named list`> The `time` at which data is displayed.
#' A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
#' Can also be a simple numeric.
#' @param col <`character`> Which column should be extracted? `var_left` or `var_right`.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' Can be NULL if there are none (ex. taking a parent variable, no schemas).
#' @param closest_time <`logical`> Should the closest time be used if the exact
#' time is not found? Default is `FALSE`.
#'
#' @return Returns the name of the variable that corresponds to the given
#' schema as a character string.
#' @export
match_schema_to_col <- function(data, time, col = "var_left",
                                data_schema = attr(data, sprintf("schema_%s", col)),
                                schemas, closest_time = FALSE) {
  # Default data_get method does not return schema_*
  if (is.null(data_schema)) {
    if (!is.null(attr(data, "schema"))) {
      data_schema <- attr(data, "schema")
    } else {
      # If schema is not supplied (so it's not a `default` method), but we want
      # another column, ex. `group`.
      data_schema <- attr(data, "schema_var_left")["time"]
    }
  }

  # If time is supplied as a list, subset. If not, use the numeric
  time_col <-
    if (is.list(time)) {
      # If `col` can be subset from the `time` list, grab it. If not, defaults
      # to taking var_left.
      if (col %in% names(time)) time[[col]] else time$var_left
    } else {
      time
    }

  # Get the possible variables that hold the schema
  pv <- grep(data_schema$time, names(data), value = TRUE)
  pv <- grep(col, pv, value = TRUE)

  # Extract available time
  avail_time <- s_extract(data_schema$time, pv) |> unique()
  avail_time <- gsub("_", "", avail_time)

  # Which var out of those correspond to the right time
  var <- pv[avail_time %in% time_col]

  if (closest_time) {
    if (length(var) == 0) {
      # Get the closest years
      avail_time <- as.numeric(avail_time)
      time_col <- as.numeric(time_col)
      closest_t <- which.min(abs(avail_time - time_col))
      var <- pv[closest_t]
    }
  }

  # Subset from schemas the col schema
  sch <- schemas[[col]]
  # If time is part of the schema, it's already been treated. Forget it.
  sch <- sch[names(sch) != "time"]
  # If time was the only `sch`
  if (length(sch) == 0) return(var)

  for (i in names(sch)) {
    regex <- data_schema[[i]]

    # Extract possibilities
    avail <- s_extract(regex, var) |> unique()
    avail <- gsub("_", "", avail)

    # Which var out of those correspond to the current schema value
    var <- var[which(avail %in% sch[[i]])]
  }

  # Return the var as a character
  return(var)
}


#' Match schema list to a specific column (`col`) in `data` based on `vl_vr` parameter
#'
#' This function extends the functionality of `match_schema_to_col` by adding support
#' for dynamic selection of schema based on the `vl_vr` parameter. It identifies the
#' column in a `data` frame that matches a given schema, considering whether to use
#' the `var_left` or `var_right` schema. This is useful in scenarios where the column
#' itself to select is not `var_left` nor `var_right`, but for example `group`.
#'
#' @param data <`data.frame`> A data frame containing the data.
#' @param time <`numeric named list`> A named list specifying the time at which
#' data is displayed. The names should correspond to `var_left` and `var_right`
#' @param col <`character`> The specific column to be extracted, e.g. `group`.
#' @param vl_vr <`character`> Determines which schema to use: 'var_left' or 'var_right'.
#' @param data_schema <`named list`> The schema information for the data, typically
#' an attribute of the data frame. Defaults to taking the attributes of data (of vl_vr).
#' @param schemas <`named list`> Current schema information, which can impact the
#' choice of data column. If `NULL`, no additional schemas are considered.
#'
#' @return The name of the variable corresponding to the given schema and `vl_vr`
#' selection as a character string.
#' @export
match_schema_to_z_col <- function(data, time, col, vl_vr,
                                  data_schema = attr(data, sprintf("schema_%s", vl_vr)),
                                  schemas) {
  if (!is.null(schemas)) {
    group_schema <- schemas
    names(group_schema) <- gsub(vl_vr, col, names(group_schema))
  } else {
    group_schema <- NULL
  }

  match_schema_to_col(
    data = data, time = time[[vl_vr]], col = col,
    data_schema = data_schema,
    schemas = group_schema
  )
}

#' Are we currently in production environment?
#'
#' @return A boolean telling if we are or not in a production environment
#' @export
in_prod <- function() {
  !grepl("(/curbcut$)|(/curbcut/tests/testthat$)|(curbcut.Rcheck/tests/testthat)",
         getwd())
}

#' Filter Rows Based on a Column Value Range
#'
#' This function filters the rows of a table where a specified column's values
#' fall within a given range.
#'
#' @param data <`dataframe`> The table to be filtered.
#' @param col <`character`> The name of the column to filter on.
#' @param range <`numeric vector`> A vector indicating the lower and upper
#' range to filter the column by.
#' @param select_id <`character`> Selection. Defaults to NA. If there is an ID
#' specified, the range will be tweaked to make sure it keeps select_id in the
#' range.
#'
#' @return <`data.table`> A data.table containing only rows where the specified
#' column's values are within the given range. If a select_id is supplied, the
#' range is tweak to make sure to include the selection. The new range is then
#' output as an attribute (`range_{col}`)
filter_inrange <- function(data, col, range, select_id = NA) {
  lower <- range[1]
  upper <- range[length(range)]

  # Remove missing values
  dat <- data[!is.na(data[[col]]), ]

  # If selection, tweak range to keep ID in range
  if (!is.na(select_id)) {
    # Get the ID value
    id_val <- dat[[col]][dat$id == select_id]

    if (length(id_val) == 0) {
      attr(data, sprintf("updated_range_%s", col)) <- FALSE
      return(data)
    }

    # If the ID value is not in the range, tweak the range
    if (id_val < lower) lower <- id_val
    if (id_val > upper) upper <- id_val
  }

  # Which data is in range
  out_ind <- data.table::inrange(dat[[col]], lower, upper)

  # Filter data
  out <- dat[out_ind, ]

  # Add a range as attribute
  attr(out, sprintf("updated_range_%s", col)) <-
    !identical(range[c(1, length(range))], c(lower, upper))

  return(out)
}

#' Check if data is present in specified scale
#'
#' This function checks if a particular variable is present within a specified scale.
#'
#' @param scale <`character`> The name of the scale for which the data presence
#' is to be checked. It is expected to correspond to a variable containing a list
#' of file names.
#' @param var <`character`> The variable name to check for in the scale's file list.
#' @param variables <`table`>
#'
#' @return <`logical`> Returns `TRUE` if the variable and scale is found together
#' in the variables table
is_data_present_in_scale <- function(var, scale, variables) {
  avail_scales <- variables$avail_scale[variables$var_code == var][[1]]
  all(scale %in% avail_scales)
}

#' Convert time value to character representation
#'
#' This function takes a variable and a time value, and returns the character
#' representation of the time value if it is named in the variables table. If the
#' `dates` column of the variable in the `variables` table is not a named vector,
#' it returns the time value itself.
#'
#' @param var <`character vector`> String representing the code of the variable
#' to retrieve information for.
#' @param time_val <`numeric`> The time value to be checked.
#'
#' @return <`character`> Returns the name of the 'time_val' if its corresponding var in
#' the `dates` column in the variables table is named. If  not named, returns 'time_val' itself.
#' @export
time_chr <- function(var, time_val, variables) {
  dates <- var_get_info(var, what = "dates", variables = variables)[[1]]
  if (is.null(names(dates))) return(time_val)

  # If dates is named, return the character
  value <- names(dates)[which(dates == time_val)]

  return(value)
}

#' Determine Delta Colors Based on Data Class
#'
#' This function selects the appropriate color set from a predefined set of
#' colors (`colours_dfs`) based on the class of the input data. It supports
#' three classes: 'normal', 'negative', and 'positive'. Depending on the class
#' of the data, it returns a corresponding set of delta colors.
#'
#' @param data <`ANY`> The data object for which the color set needs to be
#' determined. The function checks for specific classes ('normal', 'negative',
#' 'positive') in the data object to decide which color set to return.
#'
#' @return A color set from `colours_dfs`. If the class of `data` is 'normal',
#' `colours_dfs$delta` is returned. If the class is 'negative',
#' `colours_dfs$delta_neg` is returned. If the class is 'positive',
#' `colours_dfs$delta_pos` is returned. The return type is dependent on the
#' structure of `colours_dfs`.
delta_which_colors <- function(data) {
  colours_dfs <- colours_get()

  if ("normal" %in% class(data)) return(colours_dfs$delta)
  if ("negative" %in% class(data)) return(colours_dfs$delta_neg)
  if ("positive" %in% class(data)) return(colours_dfs$delta_pos)
}

