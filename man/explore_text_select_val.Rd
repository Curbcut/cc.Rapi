% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore_text_helpers.R
\name{explore_text_select_val}
\alias{explore_text_select_val}
\alias{explore_text_select_val.pct}
\alias{explore_text_select_val.ind}
\alias{explore_text_select_val.default}
\title{Generate values for the given variable and selection}
\usage{
explore_text_select_val(var, variables, ...)

\method{explore_text_select_val}{pct}(
  var,
  variables,
  select_id,
  data,
  scale,
  col = "var_left",
  time,
  schemas = NULL,
  region,
  ...
)

\method{explore_text_select_val}{ind}(
  var,
  variables,
  data,
  select_id,
  col = "var_left",
  time,
  lang,
  schemas = NULL,
  ...
)

\method{explore_text_select_val}{default}(
  var,
  variables,
  data,
  select_id,
  col = "var_left",
  time,
  schemas = NULL,
  ...
)
}
\arguments{
\item{var}{<\code{character}> The variable code of the variable for which the
values need to be generated. Usually one element of the output of
\code{\link{vars_build}}.}

\item{...}{Additional arguments passed to the dispatched function.}

\item{select_id}{<\code{character}> The ID of the selected zone.}

\item{data}{<\code{data.frame}> A data frame containing the variables and
observations. The output of \code{\link{data_get}}.}

\item{scale}{<\code{character}> Current scale. The output of}

\item{col}{<\code{character}> Which column of \code{data} should be selected to grab the
value information. Defaults to \code{var_left}, but could also be \code{var_right} or
\code{var_left_1} in delta.}

\item{time}{<\verb{numeric named list}> The \code{time} at which data is displayed.
A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.}

\item{schemas}{<\verb{named list}> Current schema information. The additional widget
values that have an impact on which data column to pick. Usually \code{r[[id]]$schema()}.}

\item{region}{<\code{character}> String of the region under study}

\item{lang}{<\code{character}> Active language. \code{"en"} or \code{"fr"}}
}
\value{
The resulting values
}
\description{
This function dispatches to the appropriate value-generating function based on
the variable type and returns the resulting values. It is only used when
there is a selection, and replaces the value of \code{\link{explore_text_region_val_df}}
}
\section{Methods (by class)}{
\itemize{
\item \code{explore_text_select_val(pct)}: Method for pct

\item \code{explore_text_select_val(ind)}: Method for \code{ind}

\item \code{explore_text_select_val(default)}: Default method

}}
