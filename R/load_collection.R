# function object. Will be assigned to the process as an "operation"
#' @export
load_collection_opp = function(id, spatial_extent, crs = 4326, temporal_extent, bands = NULL, resolution = 30, job) {
  # temporal extent preprocess
  t0 <- temporal_extent[[1]]
  t1 <- temporal_extent[[2]]
  duration <- c(t0, t1)
  time_range <- paste(duration, collapse = "/")
  message("....After Temporal extent")

  # spatial extent for cube view
  xmin <- as.numeric(spatial_extent$west)
  ymin <- as.numeric(spatial_extent$south)
  xmax <- as.numeric(spatial_extent$east)
  ymax <- as.numeric(spatial_extent$north)
  message("...After Spatial extent")

  # spatial extent for STAC API call
  xmin_stac <- xmin
  ymin_stac <- ymin
  xmax_stac <- xmax
  ymax_stac <- ymax
  message("....After default Spatial extent for STAC")

  if (crs != 4326) {
    message("....crs is not 4326")
    min_pt <- sf::st_sfc(st_point(c(xmin, ymin)), crs = crs)
    min_pt <- sf::st_transform(min_pt, crs = 4326)
    min_bbx <- sf::st_bbox(min_pt)
    xmin_stac <- min_bbx$xmin
    ymin_stac <- min_bbx$ymin
    max_pt <- sf::st_sfc(st_point(c(xmax, ymax)), crs = crs)
    max_pt <- sf::st_transform(max_pt, crs = 4326)
    max_bbx <- sf::st_bbox(max_pt)
    xmax_stac <- max_bbx$xmax
    ymax_stac <- max_bbx$ymax

    message("....transformed to 4326")
  }


  tryCatch(
    {
      # connect to STAC API using rstac and get satellite data
      message("STAC API call.....")
      stac_object <- rstac::stac("https://earth-search.aws.element84.com/v0")
      items <- stac_object %>%
        stac_search(
          collections = id,
          bbox = c(xmin_stac, ymin_stac, xmax_stac, ymax_stac),
          datetime = time_range,
          limit = 10000
        ) %>%
        post_request() %>%
        items_fetch()

      # create image collection from STAC items features
      img.col <- gdalcubes::stac_image_collection(items$features,
                                                  property_filter =
                                                    function(x) {
                                                      x[["eo:cloud_cover"]] < 30
                                                    }
      )

      # Define cube view with bi weekly aggregation
      crs <- c("EPSG", crs)
      crs <- paste(crs, collapse = ":")
      v.overview <- gdalcubes::cube_view(
        srs = crs, dx = resolution, dy = resolution, dt = "P15D",
        aggregation = "median", resampling = "average",
        extent = list(
          t0 = t0, t1 = t1,
          left = xmin, right = xmax,
          top = ymax, bottom = ymin
        )
      )

      # data cube creation
      cube <- gdalcubes::raster_cube(img.col, v.overview)

      if (!is.null(bands)) {
        cube <- gdalcubes::select_bands(cube, bands)
      }
    },
    error= function(err)
    {
      message(toString(err))
      stop(err$message)
    }
  )


  message(gdalcubes::dimensions(cube))


  message("The data cube is created....")
  message(gdalcubes::as_json(cube))
  return(cube)
}


datacube_schema <- function() {
  info <- list(
    description = "A data cube for further processing",
    schema = list(type = "object", subtype = "raster-cube")
  )
  return(info)
}

#' return object for the processes
eo_datacube <- datacube_schema()


#' load collection
load_collection <- Process$new(
  id = "load_collection",
  description = "Loads a collection from the current back-end by its id and returns it as processable data cube",
  categories = as.array("cubes", "import"),
  summary = "Load a collection",
  parameters = list(
    Parameter$new(
      name = "id",
      description = "The collection id",
      schema = list(
        type = "string",
        subtype = "collection-id"
      )
    ),
    Parameter$new(
      name = "spatial_extent",
      description = "Limits the data to load from the collection to the specified bounding box",
      schema = list(
        list(
          title = "Bounding box",
          type = "object",
          subtype = "bounding-box",
          properties = list(
            east = list(
              description = "East (upper right corner, coordinate axis 1).",
              type = "number"
            ),
            west = list(
              description = "West lower left corner, coordinate axis 1).",
              type = "number"
            ),
            north = list(
              description = "North (upper right corner, coordinate axis 2).",
              type = "number"
            ),
            south = list(
              description = "South (lower left corner, coordinate axis 2).",
              type = "number"
            )
          ),
          required = c("east", "west", "south", "north")
        ),
        list(
          title = "GeoJson",
          type = "object",
          subtype = "geojson"
        ),
        list(
          title = "No filter",
          description = "Don't filter spatially. All data is included in the data cube.",
          type = "null"
        )
      )
    ),
    Parameter$new(
      name = "crs",
      description = "Coordinate Reference System, default = 4326",
      schema = list(
        type = "number",
        subtype = "epsg-code"
      ),
      optional = TRUE
    ),
    Parameter$new(
      name = "temporal_extent",
      description = "Limits the data to load from the collection to the specified left-closed temporal interval.",
      schema = list(
        type = "array",
        subtype = "temporal-interval"
      )
    ),
    Parameter$new(
      name = "bands",
      description = "Only adds the specified bands into the data cube so that bands that don't match the list of band names are not available.",
      schema = list(
        type = "array"
      ),
      optional = TRUE
    ),
    Parameter$new(
      name = "resolution",
      description = "Specify resolution for spatial resampling.",
      schema = list(
        type = "integer"
      ),
      optional = TRUE
    )
  ),
  returns = eo_datacube,
  operation = load_collection_opp
)
