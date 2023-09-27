#' Extract attached photos for selected BIA features
#'
#' @param bounds An \code{sf} spatial data frame containing one or more polygon
#'   features that define the bounds to be queried.
#'
#' @param BIA_path (character) Path to the ESRI geodatabase of BIA features and
#'   associated attachments.
#'
#' @param dest_dir (character) Directory in which to write the JPG files for
#'   photos. If the provided directory path does not exist it will be created.
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
#'   bounds with all fields from the BIA table plus the column
#'   \code{'num_photos'} with number of photos associated with each feature.
#'
#'
#' @examples
#' \dontrun{
#' # Read in polygon(s) defining the query bounds
#' dat_bounds <- sf::st_read("conjola_boundary.shp")
#'
#' # Get the photos for BIA features within this query area.
#' dat_query_features <- get_bia_photos(dat_bounds, BIA_PATH = "data/BIA_SQL2008.gdb", dest_dir = "conjola")
#' }
#'
#' @export
#
get_bia_photos <- function(query_bounds,
                           BIA_PATH,
                           dest_dir,
                           feature_layer = "BIAver2",
                           feature_key = "globalid",
                           attachment_layer = "BIAver2__ATTACH",
                           attachment_key = "rel_globalid") {

  checkmate::assert_directory_exists(BIA_PATH, access = "r")

  checkmate::assert_data_frame(query_bounds, min.rows = 1)
  checkmate::assert_class(query_bounds, "sf")

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

  # Destination directory for photos
  ok <- checkmate::test_string(dest_dir, min.chars = 1)
  if (!ok) stop("Please provide a valid destination directory name via argument dest_dir")

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  # Read the BIA feature layer
  message("Reading BIA features")
  dat_bia <- sf::st_read(BIA_PATH, layer = feature_layer, quiet = TRUE)

  # Ensure query bounds are in the same CRS as the BIA data
  query_bounds <- sf::st_transform(query_bounds, sf::st_crs(dat_bia))

  # Identify BIA features within the query polygon(s).
  ii <- which( lengths( sf::st_intersects(dat_bia, query_bounds) ) > 0 )
  nf <- length(ii)

  if (nf == 0) {
    message("No BIA features within the query bounds")
    return(NULL)

  } else {
    s <- if (nf > 1) "features" else "feature"
    msg <- glue::glue("{nf} {s} within the query bounds" )
    message(msg)

    dat_query_features <- dat_bia[ii, ]

    r <- paste0("^", feature_key, "$")
    feature_key_colnum <- grep(r, colnames(dat_query_features), ignore.case = TRUE)
    fkeys <- sf::st_drop_geometry(dat_query_features)[[feature_key_colnum]]

    pb <- progress::progress_bar$new(total = length(fkeys), format = "  :fkey :nphotos image(s) [:bar] :percent")

    nphotos <- sapply(fkeys, function(fkey) {
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
        outpath <- file.path(dest_dir, fname)
        writeBin(res$DATA[ires][[1]], con = outpath)
      }

      pb$tick(tokens = list(fkey = fkey, nphotos = nrow(res)))

      nphotos
    })
  }

  dat_query_features$num_photos <- nphotos

  dat_query_features
}

