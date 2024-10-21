#' Retrieve data from a QS file based on variable and scale
#'
#' This function takes in a variable code and the scale to retrieve data from a
#' QS file. The function constructs the file path based on the input, then reads
#' the data using the qs package's \code{\link[qs]{qread}} function.
#'
#' @param vars_vector <`character`> A string specifying the name of the table to
#' retrieve data from. Single variable = single table. e.g. `housing_tenant`
#' @param scale <`character`> A string specifying the scale at which to retrieve
#' data, corresponding to a path on disk, e.g. `DA` or `CSD`.
#' @param region <`character`> String of the region under study.
#'
#' @return A data.frame object with the selected data from the specified table.
db_get_data_from_sql <- function(vars_vector, scales, region) {

  with_ids_as <- list()
  for (scale in scales) {
    with_ids_as[[scale]] <-
      sprintf("SELECT jsonb_array_elements_text(\"scales\"->'%s')::text AS ID
  FROM \"%s\".regions_dictionary
  WHERE \"region\" = '%s'", scale, "mtl", region)
  }
  with_ids_as <- paste0("WITH ids AS (", paste0(with_ids_as, collapse = " UNION "), ")")

  all_vars <- list()
  for (var in vars_vector) {

    var_selections <- list()
    for (scale in scales) {
      var_selections[[scale]] <-
        sprintf("SELECT *, '%s' AS scale
  FROM %s.\"%s_%s\"
  WHERE \"ID\" IN (SELECT ID FROM ids)", scale, "mtl", scale, var)
    }
    all_vars[[var]] <- paste0(var, " AS (", paste0(var_selections, collapse = " UNION ALL "), ")")

  }
  all_vars <- paste0(all_vars, collapse = ", ")


  # Construct the main SELECT and JOIN clauses
  main_select <- paste(sprintf("%s.*", vars_vector[[1]]), collapse = ", ")
  join_clauses <- ""
  if (length(vars_vector) > 1) {
    for (i in 2:length(vars_vector)) {
      main_select <- paste(main_select, sprintf(", %s.*", vars_vector[[i]]), collapse = ", ")
      join_clauses <- paste(join_clauses, sprintf(
        "LEFT JOIN %s ON %s.\"ID\" = %s.\"ID\" AND %s.scale = %s.scale ",
        vars_vector[[i]], vars_vector[[1]], vars_vector[[i]], vars_vector[[1]], vars_vector[[i]]
      ))
    }
  }

  # Final query construction
  final_call <- paste(with_ids_as, all_vars, sep = ", ")
  final_call <- paste(final_call, sprintf("SELECT %s FROM %s %s", main_select, vars_vector[[1]], join_clauses))

  # Execute the final query
  result <- db_get_helper(final_call)

  # Remove ID... and scale...
  if (sum(grepl("ID\\.\\.|scale\\.\\.", names(result))) > 0)
    result <- result[-grep("ID\\.\\.|scale\\.\\.", names(result))]

  # Go over potentially expected duplicated columns
  duplicate_ID <- grep("^ID$", names(result))
  if (length(duplicate_ID) > 1)
    result <- result[-duplicate_ID[2:length(duplicate_ID)]]
  duplicate_scale <- grep("^scale$", names(result))
  if (length(duplicate_scale) > 1)
    result <- result[-duplicate_scale[2:length(duplicate_scale)]]

  # Remove scale..
  if (sum(grepl("scale\\.", names(result))) > 0)
    result <- result[-grep("scale\\.", names(result))]

  return(result)

}

#' Calculate the percentage change between two variables over two years
#'
#' This function takes two variables representing the same quantity measured two
#' years apart and calculates the percentage change between the two values.
#'
#' @param vars <`character vector`> A var_code. The variable to get data for.
#' @param time <`character vector`> A character vector of length 2. The
#' two years for which the delta should be calculated.
#' @param scale <`character`> A string specifying the scale at which to retrieve
#' data, corresponding to a path on disk, e.g. `DA` or `CSD`.
#' @param variables <`data.frame`> Dataframe of the variables dictionary, containing
#' both var_left and var_right, and any potential parent variable aswell.
#' @param region <`character`> String of the region under study.
#' @param vl_vr <`character`> Which of var_left or var_right is this delta supposed
#' to be for. Defaults to var_left.
#' @param reduce <`logical`> Should the dataframe be reduced to a single table
#' (if there are multiple scales)
#'
#' @return A data frame with the following columns: ID, var_1, var_2, and var.
#' `ID` is the ID column from the original data, `var_1` and `var_2` are the
#' values of the two variables being compared, and `var` is the percentage
#' change between the two variables.
data_get_delta <- function(vars, time, scale, variables, region, vl_vr = "var_left",
                           reduce = TRUE) {
  # Grab the correct var/time
  var <- vars[[vl_vr]]
  time_col <- time[[vl_vr]]

  # Retrieve
  data <- db_get_data_from_sql(var = var, scale = scale, region = region)

  # Calculate breaks for the right columns
  data_schema <- variables$schema[variables$var_code == var][[1]]
  attr(data, sprintf("schema_%s", vl_vr)) <- data_schema

  cols <- match_schema_to_col(data = data, time = time_col, col = var, schemas = NULL)
  breaks_var <- variables$breaks_var[variables$var_code == var]
  keep_cols <- c("ID", "scale", cols, if (!is.na(breaks_var)) breaks_var) # keep the breaks_var and use it to calculate breaks
  data <- data[unique(keep_cols)]

  # So that it works as a single SQL call for breaks, we will iterate over
  # scales here.
  data <- split(data, data$scale)

  # Append breaks
  data <- lapply(data, \(data) {
    data <- data_append_breaks(
      var = var,
      data = data,
      q3_q5 = "q5",
      rename_col = vl_vr,
      variables = variables
    )
    data <- data$data

    # Keep columns of the two years
    cols <- match_schema_to_col(data = data, time = time_col, col = vl_vr,
                                schemas = NULL)

    # Preserve attributes before subsetting
    attrs <- attributes(data)
    # Subset the data
    data_subset <- data[c("ID", "scale", grep(paste0(cols, collapse = "|"), names(data), value = TRUE))]
    # Restore the original attributes
    attributes(data_subset) <- attrs

    # Calculate the relative difference
    result <- (data[[4]] - data[[3]]) / data[[3]]
    # Identify positions where data[[3]] is equal to data[[2]] and neither are NAs
    equal_non_na <- !is.na(data[[4]]) & !is.na(data[[3]]) & data[[4]] == data[[3]]
    # Set result to 0 where conditions are met
    result[equal_non_na] <- 0

    # Replace NaNs and infinite values with NA
    data[[vl_vr]] <- result
    data[[vl_vr]] <- replace(data[[vl_vr]], is.na(data[[vl_vr]]), NA)
    data[[vl_vr]] <- replace(data[[vl_vr]], is.infinite(data[[vl_vr]]), NA)

    data
  })

  # Bind, which won't have any effect if there's a single scale.
  if (reduce) data <- Reduce(rbind, data)

  # Return output
  return(data)
}

#' Get data
#'
#' This function retrieves data from QS files on disk or an SQLite database
#' using the appropriate method based on the class of the input vars object.
#' vars should be a named list with a class, built using the
#' \code{\link{vars_build}} function. Depending on the class of vars, different
#' methods will be used to retrieve and process the data.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function. The class of the vars object is
#' used to determine how to grab de data and output it.
#' @param scale <`character`> The scale of the data to be retrieved, e.g. `CSD`.
#' @param region <`character vector`> Character of the region under study
#' @param variables <`data.frame`> Dataframe of the variables dictionary, containing
#' both var_left and var_right, and any potential parent variable aswell.
#' @param reduce <`logical`> Should the dataframe be reduced to a single table
#' (if there are multiple scales)
#' @param ... Additional arguments passed to methods.
#'
#' @return A dataframe containing the data according to the class of `vars`,
#' with an ID column, one column per year of data, and one `group` column per
#' year of data.
#' @export
data_get <- function(vars, scale, region, variables, ...) {
  UseMethod("data_get", vars)
}

#' @describeIn data_get The method for q5.
#' @param vl_vr <`character`> Is the parent data coming from the var_left
#' or var_right? How should it be renamed.
#' @export
data_get.q5 <- function(vars, scale, region = NULL, variables, vl_vr = "var_left",
                        reduce = TRUE, ...) {
  # Get data
  data <- db_get_data_from_sql(vars$var_left, scale = scale, region = region)

  # So that it works as a single SQL call for breaks, we will iterate over
  # scales here.
  data <- split(data, data$scale)

  # Append breaks
  data <- lapply(data, \(data) {
    data_append_breaks(
      var = vars$var_left,
      data = data,
      q3_q5 = "q5",
      rename_col = vl_vr,
      variables = variables
    )
  })

  # Grab only data
  data <- lapply(data, `[[`, "data")

  # Bind, which won't have any effect if there's a single scale.
  if (reduce) data <- Reduce(rbind, data)

  # Return output
  return(data)
}

#' @describeIn data_get The method for bivar.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @export
data_get.bivar <- function(vars, scale, region, variables, schemas,
                           reduce = TRUE, ...) {
  # If data isn't present, throw an empty tibble
  if (!is_data_present_in_scale(var = vars$var_right, scale = scale, variables = variables)) {
    return(data.frame())
  }

  # Get var_left and var_right data
  data_sql <- db_get_data_from_sql(unlist(vars), scale = scale, region = region)

  # So that it works as a single SQL call for breaks, we will iterate over
  # scales here.
  data_split <- split(data_sql, data_sql$scale)
  data <- lapply(data_split, \(data) {

    # Split data in var_left and var_right
    data <- lapply(vars, \(v) {
      this_var_name <- names(data)
      for (s in variables$schema[variables$var_code == v][[1]]) {
        this_var_name <- gsub(s, "", this_var_name)
      }
      data[this_var_name %in% c("ID", "scale", v)]
    })

    # Append breaks
    all_data <- mapply(
      \(var, d, rename_col) {
        data_append_breaks(
          var = var, data = d, q3_q5 = "q3",
          rename_col = rename_col,
          variables = variables
        )
      }, c(vars$var_left, vars$var_right),
      data,
      c("var_left", "var_right"),
      SIMPLIFY = FALSE
    )

    # Grab all the time of the var_left (which are going to be the possible
    # value if `time`, as time usually follows the left variable)
    time_regex <- unique(all_data[[1]]$attr$schema_var_left$time)
    possible_vl_times <- grep(time_regex, names(all_data[[1]]$data), value = TRUE)
    possible_vl_times <- s_extract(time_regex, possible_vl_times)
    possible_vl_times <- unique(possible_vl_times)
    possible_vl_times <- gsub("_", "", possible_vl_times)

    # Possible other vl_schemas
    other_vl_schemas <- all_data[[1]]$attr$schema_var_left
    other_vl_schemas <- other_vl_schemas[names(other_vl_schemas) != "time"]
    if (length(other_vl_schemas) > 0) {
      possible_other_schemas <- NULL
      for (i in names(other_vl_schemas)) {
        sch_rege <- other_vl_schemas[[i]]
        possible_other_schemas <- grep(sch_rege, names(all_data[[1]]$data), value = TRUE)
        possible_other_schemas <- s_extract(sch_rege, possible_other_schemas)
        possible_other_schemas <- gsub("_", "", possible_other_schemas)
      }
    }

    # Keep left and right breaks
    breaks_vl <- attr(all_data[[1]]$data, "breaks_var_left")
    breaks_vr <- attr(all_data[[2]]$data, "breaks_var_right")

    # Merge (remove scale from one first)
    all_data[[2]]$data <- all_data[[2]]$data[names(all_data[[2]]$data) != "scale"]
    data <- merge(all_data[[1]]$data, all_data[[2]]$data, by = "ID", all = TRUE)

    # Re-add breaks
    attr(data, "breaks_var_left") <- breaks_vl
    attr(data, "breaks_var_right") <- breaks_vr

    # Re-add the attributes
    for (i in names(all_data[[1]]$attr)) {
      attr(data, i) <- all_data[[1]]$attr[[i]]
    }
    for (i in names(all_data[[2]]$attr)) {
      attr(data, i) <- all_data[[2]]$attr[[i]]
    }

    # Make the group columns, with the years (group_2016, group_2021, ...)
    # If there are possible_other_schemas:
    if (length(other_vl_schemas) > 0) {
      for (i in possible_vl_times) {
        for (s in possible_other_schemas) {
          vr_year <- var_closest_year(vars$var_right, i, variables = variables)$closest_year
          out <- paste(data[[sprintf("var_left_%s_%s_q3", s, i)]],
                       data[[sprintf("var_right_%s_q3", vr_year)]],
                       sep = " - "
          )
          data[[sprintf("group_%s_%s", s, i)]] <- out
        }
      }
    } else {
      for (i in possible_vl_times) {
        vr_year <- var_closest_year(vars$var_right, i, variables = variables)$closest_year

        # Give it a try. Does this year exist? If not, use the default
        out <- if (!is.null(data[[sprintf("var_right_%s_q3", vr_year)]])) {
          paste(data[[sprintf("var_left_%s_q3", i)]],
                data[[sprintf("var_right_%s_q3", vr_year)]],
                sep = " - "
          )
        } else {
          vr <- variables$breaks_var[variables$var_code == vars$var_right]
          removed_year <- gsub(attributes(data)$schema_var_right$time, "", vr)
          removed_year <- gsub(vars$var_right, "var_right", removed_year)

          paste(data[[sprintf("var_left_%s_q3", i)]],
                data[[sprintf("%s_%s_q3", removed_year, vr_year)]],
                sep = " - "
          )
        }

        data[[sprintf("group_%s", i)]] <- out
      }
    }

    data

  })

  # Bind, which won't have any effect if there's a single scale.
  if (reduce) data <- Reduce(rbind, data)

  # Return
  return(data)
}

#' @describeIn data_get The method for delta.
#' @param time <`named list`> Object built using the \code{\link{vars_build}}
#' function. It contains the time for both var_left and var_right variables.
#' @export
data_get.delta <- function(vars, scale, region, variables,
                           reduce = TRUE, time, ...) {
  data_get_delta_fun(
    vars = vars, scale = scale, region = region, variables = variables,
    reduce = reduce, time = time, ...
  )
}

#' @title Inner function to get data based on the type of `vars`
#'
#' @description This function dispatches the data retrieval based on the class
#' of the `vars` object.
#'
#' @param vars <`named list`> Named list with a class. Object built using the
#' \code{\link{vars_build}} function.
#' @param scale <`character`> The scale at which the user is on.
#' @param region <`character vector`> String of the region under study
#' @param variables <`data.frame`> Dataframe of the variables dictionary, containing
#' both var_left and var_right, and any potential parent variable aswell.
#' @param reduce <`logical`> Should the dataframe be reduced to a single table
#' (if there are multiple scales)
#' @param time <`named list`> Object built using the \code{\link{vars_build}}
#' function. It contains the time for both var_left and var_right variables.
#' @param ... Additional arguments passed to methods.
#'
#' @seealso \code{\link{data_get.delta}}
data_get_delta_fun <- function(vars, scale, region, variables,
                               reduce = TRUE, time, ...) {
  UseMethod("data_get_delta_fun", vars)
}

#' @describeIn data_get_delta_fun The method for scalar variables.
data_get_delta_fun.scalar <- function(vars, scale, region, variables, time,
                                      reduce = TRUE, ...) {

  # Get data
  data <- data_get_delta(
    vars = vars, time = time,
    scale = scale, variables = variables, region = region,
    reduce = reduce
  )

  process_data <- \(data) {
    # Is delta ONLY positive or ONLY negative? Inform which color scale to use
    vec <- data$var_left
    vec <- vec[!is.na(vec)]
    current <- "normal"
    if (all(vec >= 0)) current <- "positive" else if (all(vec <= 0)) current <- "negative"
    class(data) <- c(current, class(data))

    # Grab the breaks in the data
    breaks <- breaks_delta(vars = vars, scale = scale, character = FALSE, data = data)

    # Add the breaks attribute
    attr(data, "breaks_var_left") <- breaks

    # Add the `group` for the map colouring
    data$var_left_q5 <- 5
    data$var_left_q5[data$var_left < breaks[5]] <- 4
    data$var_left_q5[data$var_left < breaks[4]] <- 3
    data$var_left_q5[data$var_left < breaks[3]] <- 2
    data$var_left_q5[data$var_left < breaks[2]] <- 1
    data$var_left_q5[is.na(data$var_left)] <- NA
    data$group <- as.character(data$var_left_q5)

    # Return
    return(data)
  }

  # Main function
  if (reduce) {
    data <- process_data(data)
    return(data)
  }

  result <- lapply(data, process_data)

  return(result)
}

#' @describeIn data_get_delta_fun The method for ordinal variables.
data_get_delta_fun.ordinal <- function(vars, scale, region, variables,
                                       reduce = TRUE, time, ...) {

  # Get data
  data <- data_get_delta(
    vars = vars, time = time,
    scale = scale, variables = variables, region = region,
    reduce = reduce
  )

  process_data <- \(data) {
    # Is delta ONLY positive or ONLY negative? Inform which color scale to use
    vec <- data$var_left
    vec <- vec[!is.na(vec)]
    current <- "normal"
    if (all(vec >= 0)) current <- "positive" else if (all(vec <= 0)) current <- "negative"
    class(data) <- c(current, class(data))

    # Grab the breaks in the data
    breaks <- breaks_delta(vars = vars, scale = scale, character = FALSE, data = data)

    # Add the breaks attribute
    attr(data, "breaks_var_left") <- breaks

    # var_left_q5 will go off of bins change. 0 bin change vs 1 bin change vs multiple
    # bin changes.
    var_left_binchange <- data[[3]] - data[[2]]

    # Add the `group` for the map colouring
    data$var_left_q5 <- 5
    data$var_left_q5[var_left_binchange == 1] <- 4
    data$var_left_q5[var_left_binchange == 0] <- 3
    data$var_left_q5[var_left_binchange == -1] <- 2
    data$var_left_q5[var_left_binchange < -1] <- 1
    data$var_left_q5[is.na(data$var_left)] <- NA
    data$group <- as.character(data$var_left_q5)

    # Return
    return(data)
  }

  # Main function
  if (reduce) {
    data <- process_data(data)
    return(data)
  }

  result <- lapply(data, process_data)

  return(result)
}

#' @describeIn data_get The method for bivar.
#' @export
data_get.delta_bivar <- function(vars, scale, region, variables, time, ...) {

  # Retrieve
  data_vl <- data_get_delta(
    vars = vars, time = time, vl_vr = "var_left",
    scale = scale, variables = variables, region = region
  )
  data_vr <- data_get_delta(
    vars = vars, time = time, vl_vr = "var_right",
    scale = scale, variables = variables, region = region
  )[-1]

  # Prepare for merge, keep attributes
  prev_attr_vl <- attributes(data_vl)
  prev_attr_vl <- prev_attr_vl[!names(prev_attr_vl) %in% c("names", "row.names", "class")]
  prev_attr_vr <- attributes(data_vr)
  prev_attr_vr <- prev_attr_vr[!names(prev_attr_vr) %in% c("names", "row.names", "class")]

  # Merge
  data <- cbind(data_vl, data_vr)

  # Keep the previous attributes
  for (i in names(prev_attr_vl)) {
    attr(data, i) <- prev_attr_vl[[i]]
  }
  for (i in names(prev_attr_vr)) {
    attr(data, i) <- prev_attr_vr[[i]]
  }

  # Add the `group` for the map colouring
  data$var_left_q3 <- ntile(data$var_left, 3)
  data$var_right_q3 <- ntile(data$var_right, 3)
  data$group <- paste(data$var_left_q3, "-", data$var_right_q3)

  # Return
  return(data)
}

#' @describeIn data_get The method for bivar_ldelta_rq3.
#' @export
data_get.bivar_ldelta_rq3 <- function(vars, scale, region, variables, time, ...) {
  # Reconstruct vars for delta
  vl_vars <- vars_build(var_left = vars$var_left, scale = scale, time = time$var_left)
  vl_time <- vl_vars$time
  vl_vars <- vl_vars$vars
  data_vl <- data_get(vl_vars,
                      scale = scale, time = vl_time, region = region
  )
  data_vl$var_left_q3 <- ntile(data_vl$var_left, 3)

  # Reconstruct vars for q3
  vr_vars <- vars_build(var_left = vars$var_right, scale = scale, time = time$var_right)
  vr_time <- vr_vars$time
  vr_vars <- vr_vars$vars
  data_vr <- data_get(vr_vars,
                      scale = scale, time = vr_time, region = region
  )
  cv <- match_schema_to_col(data_vr, time = vr_time, schemas = NULL)
  data_vr <- data_vr[cv]
  names(data_vr) <- gsub("var_left", "var_right", names(data_vr))
  data_vr$var_right_q3 <- ntile(data_vr[[1]], 3)

  # Prepare for merge, keep attributes
  prev_attr_vl <- attributes(data_vl)
  prev_attr_vl <- prev_attr_vl[!names(prev_attr_vl) %in% c("names", "row.names", "class")]
  prev_attr_vr <- attributes(data_vr)
  prev_attr_vr <- prev_attr_vr[!names(prev_attr_vr) %in% c("names", "row.names", "class")]
  names(prev_attr_vr) <- gsub("var_left", "var_right", names(prev_attr_vr))

  # Bind vl and vr
  data <- cbind(data_vl, data_vr)

  # Keep the previous attributes
  for (i in names(prev_attr_vl)) {
    attr(data, i) <- prev_attr_vl[[i]]
  }
  for (i in names(prev_attr_vr)) {
    attr(data, i) <- prev_attr_vr[[i]]
  }

  # Create the `group` column for map colouring
  data$group <- sprintf("%s - %s", data$var_left_q3, data$var_right_q3)

  # Return
  return(data)
}

#' @describeIn data_get The default method.
#' @param vr_vl <`character`> Is the parent data coming from the var_left
#' or var_right? How should it be renamed.
#' @export
data_get.default <- function(vars, scale, region, variables, vr_vl, ...) {
  # Check if `vars` has been entered without being subset from `vars_build()`
  if (all(c("vars", "time") %in% names(vars))) {
    stop("`vars` is invalid. Subset `$vars` from the output of `vars_build()`.")
  }

  # Grab var and modify if necessary
  var <- vars[[1]]
  if (var == "population") var <- "c_population"
  if (var == "households") var <- "private_households"

  # Default method retrieves the data of the first element of `vars`
  data <- if (var == "area") {
    db_get(select = c("ID", "area"), from = scale, schema = "mtl")
  } else {
    db_get_data_from_sql(var = var, scale = scale, region = region)
  }

  # To keep it constant, rename with var_left
  names(data) <- gsub(var, vr_vl, names(data))

  # Return
  return(data)
}
