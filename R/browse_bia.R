#' App to display a map of BIA feature locations and associated photos
#'
#' This function launches a Shiny app with a map of BIA feature locations.
#' Clicking on a feature will query attachments table in the BIA database to
#' retrieve associated photographs and display them.
#'
#' @param BIA_path (character) Path to the ESRI geodatabase of BIA features and
#'   associated attachments.
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
#' @export
#'
browse_bia <- function(BIA_PATH,
                       impact_values = 1:3,
                       feature_layer = "BIAver2",
                       feature_key = "GlobalID",
                       attachment_layer = "BIAver2__ATTACH",
                       attachment_key = "REL_GLOBALID") {

  if (!("shiny" %in% installed.packages()[, "Package"])) {
    stop("You need to install the 'shiny' package to use this function")
  }

  library(shiny)
  library(leaflet)


  # Check BIA data is accessible and feature and attachment layers are present

  checkmate::assert_directory_exists(BIA_PATH, access = "r")

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

  message("Reading BIA features")
  dat_bia <- sf::st_read(BIA_PATH, layer = feature_layer, quiet = TRUE)

  if (!is.null(impact_values)) {
    ii <- which(dat_bia$ImpactAssessment %in% impact_values)
    dat_bia <- dat_bia[ii, ]
  }

  ui <- fixedPage(
    titlePanel("Building Impact Assessment"),

    fixedRow(
      column(width = 6,
        leafletOutput("map", height = 800),
      ),
      column(width = 6,
        textOutput("info"),
        selectInput("photo_num", "Photo", choices = NULL),
        imageOutput("photo", width = "auto", height = "auto")
      )
    )
  )


  # Paths to JPG files for photos associated with a currently
  # selected BIA feature
  photo_paths <- reactiveVal(character(0))


  server <- function(input, output, session) {
    # Function to set marker colours based on ImpactAssessment value
    impact_colour <- leaflet::colorFactor(topo.colors(12), 1:12)

    output$info <- renderText({
      n <- length(photo_paths())
      if (n == 0) {
        "No photos"
      } else if (n == 1) {
        "1 photo"
      } else {
        paste(n, "photos")
      }
    })

    observeEvent(photo_paths(), {
      if (length(photo_paths()) == 0) {
        choices <- character(0)
        selected <- character(0)
      } else {
        choices <- as.character(1:length(photo_paths()))
        selected <- "1"
      }
      updateSelectInput(session, "photo_num", choices = choices, selected = selected)
    })

    output$map <- renderLeaflet({
      leaflet(data = dat_bia) %>%
        addProviderTiles("Stamen.Toner", group = "Toner by Stamen") %>%
        addProviderTiles("OpenTopoMap", group = "Open Topo Map") %>%
        addProviderTiles("OpenStreetMap", group = "Open Street Map") %>%

        addCircleMarkers(
          radius = 10,
          fillOpacity = 0.7,
          stroke = FALSE,
          color = ~impact_colour(ImpactAssessment),
          label = ~GlobalID,
          clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=2, maxClusterRadius = 30),
          group = "Markers",
          layerId = ~GlobalID  # unique id for each feature
          ) %>%

        addLayersControl(
          baseGroups = c("OpenStreetMap", "OpenTopoMap", "Toner by Stamen"),
          overlayGroups = c("Markers")
        )
    })

    output$photo <- renderImage({
      i <- as.integer(input$photo_num)
      req(length(i) > 0)

      path <- photo_paths()[i]
      list(src = path, width = 600)
      },
      deleteFile = FALSE
    )


    observe({
      ev <- input$map_marker_click

      if (length(photo_paths()) > 0) {
        invisible(file.remove(photo_paths()))
      }

      req(!(is.null(ev) || is.null(ev$id)))

      cmd <- glue::glue("select {attachment_key} as fkey, GLOBALID as photokey, DATA from {attachment_layer}
                        where {attachment_key} = '{ev$id}'")

      print(cmd)

      # Suppress warning about no geometry in the table
      suppressWarnings(
        res <- sf::st_read(dsn = BIA_PATH, query = cmd, as_tibble = TRUE, quiet = TRUE)
      )

      nphotos <- nrow(res)
      print(paste(nphotos, "photos found"))

      if (nphotos == 0) {
        photo_paths(character(0))

      } else {
        photo_dir <- tempdir(check = TRUE)

        paths <- sapply(seq_len(nphotos), function(k) {
          fname <- gsub("\\{|\\}", "", res$fkey[k])
          fname <- sprintf("%s_%03d.jpg", fname, k)
          outpath <- file.path(photo_dir, fname)
          writeBin(res$DATA[k][[1]], con = outpath)
          outpath
        })

        photo_paths(paths)
      }
    })

    onStop(
      function() {
        print("Removing cached photos")
        invisible( file.remove(dir(tempdir(), pattern = "\\.jpg$", full.names = TRUE)) )
      }
    )
  }

  shinyApp(ui, server)
}
