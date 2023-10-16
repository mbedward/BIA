#' Display an interactive map of BIA feature locations and associated photos
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
#' @param debug_info (logical) If \code{TRUE}, print SQL query commands and
#'   other internal information for debugging purposes. Default is \code{FALSE}
#'   for no debug information.
#'
#' @examples
#' \dontrun{
#' browse_bia(BIA_PATH = "data/BIA_SQL2008.gdb", impact_values = 1:3)
#' }
#'
#' @export
#'
browse_bia <- function(BIA_PATH,
                       impact_values = 1:3,
                       feature_layer = "BIAver2",
                       feature_key = "GlobalID",
                       attachment_layer = "BIAver2__ATTACH",
                       attachment_key = "REL_GLOBALID",
                       debug_info = FALSE) {

  if (!("shiny" %in% installed.packages()[, "Package"])) {
    stop("You need to install the 'shiny' package to use this function")
  }

  suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs, warn.conflicts = FALSE)
    library(leaflet)
  })


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
  if (length(impact_values) == 0) impact_values <- 1:12
  ok <- checkmate::test_integerish(impact_values, lower = 1, upper = 12, any.missing = FALSE, null.ok = TRUE)
  if (!ok) stop("impact_values argument should be a vector of integer values between 1 and 12")

  impact_labels <- {
    ii <- with(bia_lookup, attribute == "ImpactAssessment" & value %in% impact_values)
    bia_lookup$label[ii]
  }

  if (debug_info) {
    print(paste("Impact values:", paste(impact_values, collapse = ", ")))
    print(paste("Impact labels:", paste(impact_labels, collapse = ", ")))
  }

  message("Reading BIA features")
  dat_bia <- sf::st_read(BIA_PATH, layer = feature_layer, quiet = TRUE)

  ii <- which(dat_bia$ImpactAssessment %in% impact_values)
  dat_bia <- dat_bia[ii, ]


  ###################################################################
  # UI
  ###################################################################

  ui <- fluidPage(
    useShinyjs(),

    titlePanel("Building Impact Assessment"),

    sidebarLayout(
      sidebarPanel(
        width = 5,
        leafletOutput("map", height = 800),
      ),

      mainPanel(
        width = 7,
        tabsetPanel(type = "tabs",
          tabPanel("Photos",
            div(style="display: inline-block;vertical-align:top;",
                actionButton("btn_first_photo", label = NULL, icon = icon("angles-left", lib = "font-awesome")),
                actionButton("btn_prev_photo", label = NULL, icon = icon("angle-left", lib = "font-awesome")),
                actionButton("btn_next_photo", label = NULL, icon = icon("angle-right", lib = "font-awesome")),
                actionButton("btn_last_photo", label = NULL, icon = icon("angles-right", lib = "font-awesome")),

                div(style="display: inline-block;vertical-align:top; width: 20px;", HTML("<br>")),

                downloadButton("save_photo", label = NULL, icon = icon("floppy-disk", lib = "font-awesome"))
            ),

            textOutput("info"),
            imageOutput("photo", width = "auto", height = "auto")
          ),

          tabPanel("Data",
            DT::DTOutput("feature_data")
          )
        )
      )
    )
  )


  ###################################################################
  # Server
  ###################################################################

  server <- function(input, output, session) {

    # Data for feature the user has selected on the map (initially no data)
    cur_feature_data <- reactiveVal(dat_bia[integer(0), ])

    # Paths to JPG files for photos associated with a currently
    # selected BIA feature
    photo_paths <- reactiveVal(character(0))
    cur_photo <- reactiveVal(integer(0))

    ##### Display map of BIA features

    # Function to set marker colours based on ImpactAssessment value
    get_impact_colour <- leaflet::colorFactor(palette.colors(12, palette = "Alphabet"), 1:12)

    # Function to map integer impact values to impact labels
    get_impact_label <- function(x) {
      k <- match(x, impact_values)
      ifelse(is.na(k), "Unknown", impact_labels[k])
    }

    format_feature_label <- function(id) {
      ii <- match(id, dat_bia$GlobalID)
      x <- dat_bia$ImpactAssessment[ii]
      il <- get_impact_label(x)
      il <- paste(id, il, sep="<br>")
      lapply(il, htmltools::HTML)
    }

    output$map <- renderLeaflet({
      leaflet(data = dat_bia) %>%
        addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
        addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") %>%

        addLegend(
          title = "Impact",
          position = "bottomright",
          pal = get_impact_colour,
          values = ~ImpactAssessment,
          labFormat  = labelFormat(
            transform = get_impact_label),
        ) %>%

        addCircleMarkers(
          radius = 6,
          fillOpacity = 0.7,
          stroke = FALSE,
          color = ~get_impact_colour(ImpactAssessment),

          label = ~format_feature_label(GlobalID),

          clusterOptions = markerClusterOptions(
            spiderfyDistanceMultiplier=2,
            maxClusterRadius = 30),

          group = "BIA features",
          layerId = ~GlobalID  # unique id for each feature
        ) %>%

       addLayersControl(
         baseGroups = c("Open Street Map", "Open Topo Map", "ESRI Imagery"),
         overlayGroups = c("BIA features")
       )
    })

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
      n <- length(photo_paths())
      if (n == 0) {
        status_text <- "No photos"
        cur_photo(integer(0))
        shinyjs::disable("save_photo")

      } else if (n == 1) {
        status_text <- "1 photo"
        cur_photo(1)
        shinyjs::enable("save_photo")

      } else if (n > 1) {
        status_text <- paste(n, "photos")
        cur_photo(1)
        shinyjs::enable("save_photo")
      }
    })

    output$photo <- renderImage({
      i <- cur_photo()
      req(length(i) > 0)

      path <- photo_paths()[i]
      list(src = path, width = 600)
    },
    deleteFile = FALSE
    )


    output$feature_data <- DT::renderDT({
      if (debug_info) print(cur_feature_data())

      format_data_for_display(cur_feature_data())
    },
    options = list(scrollX = TRUE) )


    ##### Photo select buttons

    observeEvent(input$btn_first_photo, {
      req(length(cur_photo()) > 0)
      cur_photo(1)
    })

    observeEvent(input$btn_prev_photo, {
      req(length(cur_photo()) > 0)
      i <- cur_photo()
      if (i > 1) cur_photo(i - 1)
    })

    observeEvent(input$btn_next_photo, {
      req(length(cur_photo()) > 0)
      i <- cur_photo()
      if (i < length(photo_paths())) cur_photo(i + 1)
    })

    observeEvent(input$btn_last_photo, {
      req(length(cur_photo()) > 0)
      cur_photo(length(photo_paths()))
    })

    ##### Save photo to file
    output$save_photo <- downloadHandler(
      filename = function() {
        i <- cur_photo()
        fs::path_file( photo_paths()[i] )
      },
      content = function(dest) {
        i <- cur_photo()
        invisible(file.copy(photo_paths()[i], dest))
      },
      contentType = "image/jpeg"
    )

    # Feature selected via mouse click
    observe({
      ev <- input$map_marker_click

      if (length(photo_paths()) > 0) {
        invisible(file.remove(photo_paths()))
      }

      req(!(is.null(ev) || is.null(ev$id)))

      # Get feature data
      i <- which(dat_bia$GlobalID == ev$id)
      if (length(i) == 1) {
        #x <- sf::st_drop_geometry(dat_bia)[i,]
        x <- dat_bia[i,]
        cur_feature_data(x)
      }

      photo_dir <- tempdir(check = TRUE)
      res <- cache_photos_for_feature(ev$id, BIA_PATH,
                                      cache_dir = photo_dir,
                                      attachment_layer_name = attachment_layer,
                                      attachment_key_name = attachment_key)

      nphotos <- length(res$paths)

      if (debug_info) {
        print(paste("Query:", res$cmd))
        print(paste(nphotos, "photos found"))
      }

      if (nphotos > 0) {
        photo_paths(res$paths)
        cur_photo(1)
      } else {
        photo_paths(character(0))
        cur_photo(integer(0))
      }

      # Highlight the currently selected feature
      lprox <- leaflet::leafletProxy("map")

      # Note: suppress any warnings about bodgy map projection of BIA data
      suppressWarnings({
        lprox %>%
          clearGroup("selected_feature") %>%
          addCircleMarkers(data = cur_feature_data(),
                           color = "yellow",
                           opacity = 0.9,
                           fill = FALSE,
                           radius = 10,
                           group = "selected_feature")
      })
    })


    # Clean-up the cache directory when we're finished
    onStop(
      function() {
        if (debug_info) print("Removing cached photos")

        invisible( file.remove(dir(tempdir(), pattern = "\\.jpg$", full.names = TRUE)) )
      }
    )
  }

  shinyApp(ui, server)
}


# Private function to retrieve photos for a BIA feature and cache them to disk.
# Returns a list with elements:
#   cmd - SQL command used to query the attachments table
#   paths - character vector of paths to cached photos
#
cache_photos_for_feature <- function(fkey,
                                     BIA_PATH,
                                     cache_dir,
                                     attachment_layer_name,
                                     attachment_key_name) {

  cmd <- glue::glue("select {attachment_key_name} as fkey, GLOBALID as photokey, DATA from {attachment_layer_name}
                        where {attachment_key_name} = '{fkey}'")

  # Suppress warning about no geometry in the table
  suppressWarnings(
    res <- sf::st_read(dsn = BIA_PATH, query = cmd, as_tibble = TRUE, quiet = TRUE)
  )

  nphotos <- nrow(res)

  if (nphotos > 0) {
    paths <- sapply(seq_len(nphotos), function(k) {
      fname <- gsub("\\{|\\}", "", res$fkey[k])
      fname <- sprintf("%s_%03d.jpg", fname, k)
      outpath <- file.path(cache_dir, fname)
      writeBin(res$DATA[k][[1]], con = outpath)
      outpath
    })
  } else {
    paths <- character(0)
  }

  list(cmd = cmd, paths = paths)
}


# Private function to format a BIA data record for display
format_data_for_display <- function(xsf_rec) {
  if (is.null(xsf_rec)) return(NULL)

  dat_rec <- as.data.frame(sf::st_drop_geometry(xsf_rec))

  dat_formatted <- data.frame(attribute = colnames(dat_rec),
                              value = "",
                              label = "")

  cl <- sapply(dat_rec, class)

  values <- sapply(seq_len(ncol(dat_rec)), function(i) {
    x <- dat_rec[1,i]
    if (lubridate::is.Date(x)) {
      format(x, "%Y-%m-%d")
    } else if (lubridate::is.POSIXt(x)) {
      format(x, "%Y-%m-%d %H:%M")
    } else {
      as.character(x)
    }
  })

  dat_formatted$value <- values

  # Look up attribute values
  found <- which(tolower(dat_formatted$attribute) %in% tolower(bia_lookup$attribute))

  labels <- sapply(seq_along(found), function(i) {
    lab <- ""
    if (found[i]) {
      ii <- grep(dat_formatted$attribute[i], bia_lookup$attribute, ignore.case = TRUE)
      if (length(ii) > 0) {
        k <- match(as.integer(dat_formatted$value[i]), bia_lookup$value[ii])
        if (!is.na(k)) {
          lab <- bia_lookup$label[ii[k]]
        }
      }
    }
    lab
  })

  dat_formatted$label[found] <- labels

  dat_formatted
}
