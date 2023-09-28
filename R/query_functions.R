#' Extract attached photos for selected BIA features
#'
#' @param query_bounds An \code{sf} spatial data frame containing one or more
#'   polygon features that define the bounds to be queried.
#'
#' @param BIA_path (character) Path to the ESRI geodatabase of BIA features and
#'   associated attachments.
#'
#' @param dest_dir (character) Directory in which to write the JPG files for
#'   photos. If the provided directory path does not exist it will be created.
#'
#' @param dest_separate (logical) If \code{TRUE} (default) and the query bounds
#'   consist of more than one feature, the photos for each polygon will be
#'   written to a separate sub-directory of \code{dest_dir}. Sub-directory names
#'   will be \code{'001', '002'} etc. corresponding to the index of the query
#'   feature. If \code{FALSE}, all photos will be written directly in
#'   \code{dest_dir}. Note that photos for a MULTIPOLYGON feature will be
#'   written to a single sub-directory, even if that feature is made up of
#'   spatially discrete parts.
#'
#' @param impact_values (integer) An optional vector of one or more values. If
#'   provided, BIA records will be filtered to those whose
#'   \code{ImpactAssessment} value is in this set. Default is to filter on
#'   values 1, 2 and 3. Set to \code{NULL} or an empty vector for no filtering
#'   of records.
#'
#' @param feature_layer (character) Name of the point feature layer in the
#'   GeoDatabase for BIA locations. Default value is \code{'BIAver2'}.
#'
#' @param attachment_layer (character) Name of the GeoDatabase table of
#'   attachments. Default value is \code{'BIAver2__ATTACH'}.
#'
#' @param feature_key (character) Name of the feature layer key field. Default
#'   value is \code{'globalid'}.
#'
#' @param attachment_key (character) Name of the attachment layer key field
#'   linking records to features. Default value is \code{'rel_globalid'}.
#'
#' @return An sf spatial data frame of the BIA point features within the query
#'   bounds with all fields from the BIA table plus the columns
#'   \code{'photo_dir'} and \code{'num_photos'}.
#'
#'
#' @examples
#' \dontrun{
#' # Read in polygon(s) defining the query bounds
#' dat_bounds <- sf::st_read("conjola_boundary.shp")
#'
#' # Get photos for all BIA features within this query area.
#' dat_query_features <- get_bia_photos(dat_bounds, BIA_PATH = "data/BIA_SQL2008.gdb", dest_dir = "conjola")
#'
#' # Only get the photos for BIA features where impact assessment values are 1-3
#' dat_query_features <- get_bia_photos(dat_bounds, BIA_PATH = "data/BIA_SQL2008.gdb", dest_dir = "conjola",
#'                                      impact_values = 1:3)
#' }
#'
#' @export
#
get_bia_photos <- function(query_bounds,
                           BIA_PATH,
                           dest_dir,
                           dest_separate = TRUE,
                           impact_values = 1:3,
                           feature_layer = "BIAver2",
                           feature_key = "globalid",
                           attachment_layer = "BIAver2__ATTACH",
                           attachment_key = "rel_globalid") {

  checkmate::assert_directory_exists(BIA_PATH, access = "r")

  checkmate::assert_data_frame(query_bounds, min.rows = 1)
  checkmate::assert_class(query_bounds, "sf")
  checkmate::assert( all(sf::st_geometry_type(query_bounds) %in% c("POLYGON", "MULTIPOLYGON")) )

  if (is.na(sf::st_crs(query_bounds))) {
    stop("Query polygon(s) must have a coordinate reference system assigned")
  }

  # Check feature and attachment layers are present
  layer_names <- sf::st_layers(BIA_PATH)$name

  if (!(feature_layer %in% layer_names)) {
    msg <- glue::glue("Can't find feature layer {feature_layer} in the GeoDatabase.
                      Available layers are: {paste(layer_names, collapse = '; ')}")
    stop(msg)
  }

  if (!(attachment_layer %in% layer_names)) {
    msg <- glue::glue("Can't find attachment layer {attachment_layer} in the GeoDatabase.
                      Available layers are: {paste(layer_names, collapse = '; ')}")
    stop(msg)
  }

  # Filtering on ImpactAssessment values
  if (length(impact_values) == 0) impact_values <- NULL
  ok <- checkmate::test_integerish(impact_values, lower = 1, any.missing = FALSE, null.ok = TRUE)
  if (!ok) stop("impact_values argument should be a vector of integer values >= 1")

  # Destination directory for photos
  ok <- checkmate::test_string(dest_dir, min.chars = 1)
  if (!ok) stop("Please provide a valid destination directory name via argument dest_dir")

  # If there is only one query feature set dest_separate to FALSE
  if (nrow(query_bounds) == 1) dest_separate <- FALSE

  ok <- checkmate::test_logical(dest_separate, min.len = 1, max.len = 1, any.missing = FALSE)
  if (!ok) stop("The dest_separate argument should be a single value: TRUE or FALSE")

  # Read the BIA feature layer
  message("Reading BIA features")
  dat_bia <- sf::st_read(BIA_PATH, layer = feature_layer, quiet = TRUE)

  # Ensure query bounds are in the same CRS as the BIA data
  query_bounds <- sf::st_transform(query_bounds, sf::st_crs(dat_bia))

  # Identify BIA features within the query polygon(s).
  xsect <- sf::st_intersects(dat_bia, query_bounds)
  ii <- which( lengths(xsect) > 0 )
  nf_init <- length(ii)

  if (nf_init == 0) {
    message("No BIA features within the query bounds")
    return(NULL)
  }

  dat_query_features <- dat_bia[ii, ]
  xsect <- xsect[ii]

  s <- if (nf_init > 1) "features" else "feature"
  msg <- glue::glue("{nf_init} {s} within the query bounds")

  if (is.null(impact_values)) {
    # No filtering being applied
    message(msg)

  } else {
    ii <- which(dat_query_features$ImpactAssessment %in% impact_values)
    nf_filter <- length(ii)

    if (nf_filter == 0) {
      msg <- glue::glue("{msg}. None in specified impact classes.")
      message(msg)
      return(NULL)

    } else {
      msg <- glue::glue("{msg}. {nf_filter} in specified impact classes.")
      message(msg)

      dat_query_features <- dat_query_features[ii, ]
      xsect <- xsect[ii]
    }
  }

  # Prepare output dir(s) for photos
  dat_query_features$photo_dir <- dest_dir
  if (dest_separate) {
    # Reduce any intersections of multiple query bounds features to the one
    # with the lowest index
    xsect <- sapply(xsect, min)

    if (length(unique(xsect)) > 1) {
      dat_query_features$photo_dir <- sprintf("%s/%03d", dest_dir, xsect)
    }
  }

  for (d in unique(dat_query_features$photo_dir)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  ###################################################################
  # Query each BIA feature and export photos
  #
  r <- paste0("^", feature_key, "$")
  feature_key_colnum <- grep(r, colnames(dat_query_features), ignore.case = TRUE)
  fkeys <- sf::st_drop_geometry(dat_query_features)[[feature_key_colnum]]

  pb <- progress::progress_bar$new(total = length(fkeys), format = "  :fkey :nphotos image(s) [:bar] :percent")

  nphotos <- sapply(seq_len(nrow(dat_query_features)), function(k) {
    fkey <- fkeys[k]
    photo_dir <- dat_query_features$photo_dir[k]

    # Find related attachment record(s)
    cmd <- glue::glue("select {attachment_key}, DATA from {attachment_layer} where {attachment_key} = '{fkey}'")

    # Suppress warning about no geometry in the table
    suppressWarnings(
      res <- sf::st_read(dsn = BIA_PATH, query = cmd, as_tibble = TRUE, quiet = TRUE)
    )

    nphotos <- nrow(res)

    for (ires in seq_len(nphotos)) {
      fname <- gsub("\\{|\\}", "", fkey)
      fname <- sprintf("%s_%03d.jpg", fname, ires)
      outpath <- file.path(photo_dir, fname)
      writeBin(res$DATA[ires][[1]], con = outpath)
    }

    pb$tick(tokens = list(fkey = fkey, nphotos = nrow(res)))

    nphotos
  })

  dat_query_features$num_photos <- nphotos

  dat_query_features
}

