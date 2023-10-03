#' Building Impact Assessment attributes look-up table
#'
#' A table used to relate integer attribute values in the BIA database to
#' attribute labels, e.g. a value of 3 for the attribute 'ImpactAssessment'in
#' the database table indicates 'House destroyed'.
#'
#' @format ## `bia_lookup`
#' A data frame with 78 rows and 3 variables:
#' \describe{
#'   \item{attribute}{name of the attribute}
#'   \item{value}{integer value}
#'   \item{label}{label associated with this integer value}
#' }
#'
"bia_lookup"
