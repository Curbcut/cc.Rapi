#' Extracts the `time` component from variable codes
#'
#' @param var <`character vector`> String representing the variable codes.
#'
#' @return A character string representing the year component of the variable
#' name.
#' @export
#'
#' @examples
#' var_get_time("housing_tenant_2016") # 2016
#' var_get_time(c("housing_tenant_2016", "housing_tenant_2021")) # "2016" "2021"
var_get_time <- function(var) {
  s_extract("(?<=_)\\d{4}$", var)
}

#' Remove the time component from variable codes
#'
#' This function removes the time component from variable codes The time
#' component is identified by the "_YYYY" format at the end of the variable name.
#'
#' @param var <`character vector`> String representing the variable codes.
#'
#' @return A character string with the variable code without the time component.
#' If the input was a variable code duplicate of different year, the output is
#' the unique variable code.
#' @export
#'
#' @examples
#' var_remove_time("housing_tenant_2016") # "housing_tenant"
#' var_remove_time(c("housing_tenant_2016", "housing_tenant_2021")) # "housing_tenant"
var_remove_time <- function(var) {
  unique(sub("_\\d{4}$", "", var))
}

#' Get information about a variable from the `variables` table
#'
#' This function retrieves information about a given variable from the `variables`
#' table in the global environment. The `variables` table must have columns for
#' `var_code` and the requested information specified by `what`. The function
#' first removes the time component from the variable name using
#' \code{\link{var_remove_time}}, hen looks up the variable code in
#' the `variables` table and returns the requested information for the variable.
#'
#' @param var <`character vector`> String representing the code of the variable
#' to retrieve information for.
#' @param what <`character`> String indicating the column name in the
#' `variables` table to retrieve information from. Defaults to `"var_title"`.
#' @param translate <`logical`> Indicating whether or not to translate the retrieved
#' information using the \code{\link{cc_t}} function. Defaults to `FALSE`.
#' @param lang <`character`> String indicating the language to translate to, if
#' `translate` is TRUE. If not specified, the function will not attempt to translate.
#' @param check_year <`logical`> Should the year be removed from `var` to grab
#' variable's info? Defaults to TRUE
#' @param variables <`data.frame`> The `variables` df. Defaults to grabbing it
#' from the global environment using \code{\link{get_from_globalenv}}.
#' @param schemas_col <`named list`> One subset of the current schema information.
#' The additional widget values that have an impact on which data column to pick.
#' Usually `r[[id]]$schema()`, with `var_left` or`var_right` subset.
#' @return The requested information about the variable, with optional translation
#' using the \code{\link{cc_t}} function.
#' @export
var_get_info <- function(var, what = "var_title", translate = FALSE,
                         lang = NULL, check_year = TRUE,
                         variables, schemas_col = NULL) {
  if (!what %in% names(variables)) {
    stop(glue::glue_safe("`{what}` is not a column of the `variables` table."))
  }

  subset_vector <- if (check_year) {
    sub <- variables$var_code == var_remove_time(var)

    # If the latter is not present in the `variables` table
    if (sum(sub) == 0) {
      final_var <- var_remove_time(var)
      stop(glue::glue_safe(
        "`{final_var}` is not a variable code in the ",
        "`variables` table."
      ))
    }

    sub
  } else {
    sub <- which(variables$var_code == var)

    # If the latter is not present in the `variables` table
    if (length(sub) == 0) {
      stop(glue::glue_safe(
        "`{var}` is not a variable code in the ",
        "`variables` table."
      ))
    }
    sub
  }

  out <- variables[[what]][subset_vector]
  if (translate) out <- cc_t(out, lang = lang)

  # If schema isn't NULL, see if it needs to be switched in explanations.
  # And is there anything to replace?
  if (!is.null(schemas_col) & grepl("__.*__", out)) {
    if (grepl("explanation|explanation_nodet|exp_q5", what)) {
      for (sch in names(schemas_col)) {
        value <-  schemas_col[[sch]]

        # Special case if time needs to be seen as character
        if (sch == "time") {
          value <- time_chr(var, value, variables = variables)
        }

        scm <- sprintf("__%s__", sch)

        # Determine the number of occurrences to replace
        num_replacements <- min(length(value), gregexpr(scm, out)[[1]] |> length())

        # Loop through each occurrence and replace with corresponding value
        for (i in 1:num_replacements) {
          out <- sub(scm, value[i], out, fixed = TRUE)
        }
      }
    }
  }

  return(out)
}

#' Get the title of a variable code
#'
#' Given a variable name, get the title of the variable from the 'variables' table.
#' If a 'short_treshold' is provided, return the short title if the title length
#' exceeds the treshold. The returned title can be translated to a different
#' language using the 'lang' argument.
#'
#' @param var <`character`> String representing the code of the variable
#' to retrieve the title for.
#' @param short_treshold <`numeric`> An optional threshold for the title length,
#' above which the short title will be returned.
#' @param translate <`logical`> Indicates whether to translate the title to a
#' different language.
#' @param lang <`character`> String indicating the language to translate the title
#' to. Defaults to `NULL`, which is no translation.
#'
#' @return A character string representing the variable title.
#' @export
var_get_title <- function(var, variables, short_treshold = NULL,
                          translate = FALSE, lang = NULL) {
  # In the case where this is the non-selected comparison
  if (var[1] == " ") {
    return(NULL)
  }

  title <-
    var_get_info(
      var = var, what = "var_title", variables = variables,
      translate = TRUE, lang = lang
    )
  if (!is.null(short_treshold) && nchar(title) > short_treshold) {
    title <-
      var_get_info(
        var = var, what = "var_short", variables = variables,
        translate = TRUE, lang = lang
      )
  }

  return(title)
}

#' Get information about the parent variable of a variable from the `variables`
#' table
#'
#' This function retrieves information about the parent variable of a given
#' variable from the `variables` table in the global environment.
#'
#' It is using the \code{\link{var_get_info}} function to retrieve the correct
#' information.
#'
#' @param var <`character vector`> String representing the code of the variable
#' to retrieve the parent variable information for.
#' @param what <`character`> String indicating the column name in the
#' `variables` table to retrieve information from. Defaults to `"var_title"`.
#' @param translate <`logical`> Indicating whether or not to translate the retrieved
#' information using the \code{\link{cc_t}} function. Defaults to `FALSE`.
#' @param lang <`character`> String indicating the language to translate to, if
#' `translate` is TRUE. If not specified, the function will not attempt to translate.
#' @param check_year <`logical`> Should the year be removed from `var` to grab
#' variable's info? Defaults to TRUE
#' @param ... Any additional arguments to pass to \code{\link{var_get_info}}
#'
#' @return The requested information about the parent variable, with optional
#' translation using the \code{\link{cc_t}} function.
#' @export
var_get_parent_info <- function(var, what = "explanation", translate = FALSE,
                                lang = NULL, check_year = TRUE, ...) {
  parent <- var_get_info(var, what = "parent_vec")
  parent_info <- var_get_info(parent,
    what = what, translate = translate,
    lang = lang, check_year = check_year, ...
  )

  return(parent_info)
}

#' Return variable based on time and input
#'
#' This function returns a variable code based on the given \code{time} and
#' \code{input}. If \code{time} is \code{NULL}, it returns the \code{input} as
#' is. If \code{input} is an empty string (" "), it returns an empty string.
#' Otherwise, it finds the closest year in which the variable is available, and
#' attaches it to the variable code.
#'
#' @param input <`character`> A character string representing the variable code.
#' @param time <`numeric`> A numeric value indicating the time in years for
#' which the variable is needed. If \code{NULL}, the function returns the
#' \code{input} as is.
#' @param variables <`data.frame`> The `variables` df. Defaults to grabbing it
#' from the global environment using \code{\link{get_from_globalenv}}.
#'
#' @return A character string representing the variable code with the closest
#' year attached, or the original \code{input} if \code{time} is \code{NULL} or
#' \code{input} is an empty string.
#'
#' @details The function uses the \code{\link{var_get_info}} function to obtain the
#' dates at which the variable is available, and then finds the closest year to
#' the given \code{time} value.
var_closest_year <- function(input, time, variables) {
  # If `time` is NULL, return the input
  if (is.null(time)) {
    return(input)
  }

  # If input isn't in `variables` returns input
  if (!input %in% variables$var_code) {
    return(input)
  }

  # Grab the dates at which the variable is available
  dates <- var_get_info(input, variables = variables, what = "dates")[[1]]
  dates <- stats::setNames(dates, names(dates))

  # If no dates, return the input
  if (all(is.na(dates))) {
    return(input)
  }

  # Get the closest years
  closest_year <- if (is.numeric(time)) {
    dates <- as.numeric(dates)
    sapply(time, \(x) dates[which.min(abs(dates - x))],
           USE.NAMES = FALSE
    )
  } else if (all(time %in% dates)) {
    dates[dates %in% time]
  } else {
    # If it's character and it doesn't fit in dates, return the latest year
    max(as.numeric(dates))
  }

  unique_cy <- unique(closest_year)
  names(unique_cy) <- names(closest_year)[match(unique_cy, closest_year)]

  # Return the var
  return(list(var = input, closest_year = unique_cy))
}
