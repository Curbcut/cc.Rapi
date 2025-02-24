% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore_text_helpers.R
\name{explore_get_parent_data}
\alias{explore_get_parent_data}
\title{Get parent data for a given variable and id}
\usage{
explore_get_parent_data(
  var,
  variables,
  select_id,
  scale,
  col = "var_left",
  time_col,
  region,
  schema = schema
)
}
\arguments{
\item{var}{<\code{character}> The code of the variable for which to retrieve the
parent data.}

\item{select_id}{<\code{character}> The id of the selected zone for which to
retrieve the parent data.}

\item{scale}{<\code{character}> The crrent scale, e.g. \code{"CT"}}

\item{col}{<\code{character}> Which column of \code{data} should be selected to grab the
value information. Defaults to \code{var_left}, but could also be \code{var_right} or
\code{var_left_1} in delta.}

\item{time_col}{<\code{numeric}> Time at which to show the data.
directory containing the QS files. Default is "data/".}

\item{region}{<\code{character}> Character string specifying the name of the region.
Usually equivalent of \code{r$region()}.}

\item{schema}{<\code{character}>}
}
\value{
A vector containing the parent value for the zone.
}
\description{
This function retrieves the parent data for a given variable and id.
If a time variable is present in the dataset, the time value is
added to the parent string to retrieve the corresponding data.
}
