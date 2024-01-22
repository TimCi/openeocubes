#' @export
predict_model_opp = function(data, model_id, job) {
  # show call stack for debugging
  message("predict_model called...")

  message("\nCall parameters: ")

  message("\ndata")
  print(data)

  message("\nmodel_id:")
  message(model_id)

  xmin = gdalcubes::dimensions(data)$x$low
  ymin = gdalcubes::dimensions(data)$y$low
  xmax = gdalcubes::dimensions(data)$x$high
  ymax = gdalcubes::dimensions(data)$y$high



  tryCatch({
    message("\nCreate AOI Polygon...")

    aoi_polygon_df = data.frame(x = c(xmin,xmax), y = c(ymin ,ymax))

    poly <- aoi_polygon_df |>
      # create sf_point object
      sf::st_as_sf(coords = c("x", "y"), crs = gdalcubes::srs(data)) |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      # create sf_polygon object
      sf::st_as_sf()

    message("AOI Polygon created!")
  },
  error = function(err)
  {
    message("An Error occured!")
    message(toString(err))
    stop()
  })

  # add "x" and "y" coords as bands to the cube. Both are the respective barycenter of the pixel.
  data = gdalcubes::apply_pixel(data, c("(left + right) / 2", "(top + bottom) / 2"), names = c("x", "y"), keep_bands = TRUE)


  tryCatch({
    message("\nExtract features...")

    # extract features from cube
    features = gdalcubes::extract_geom(data, poly)

    # reset FID to prevent mismatch after extraction
    features$FID = NULL
    features$FID = rownames(features)

    message("All features extracted!")
  },
  error = function(err)
  {
    message("An Error occured!")
    message(toString(err))
    stop()
  })

  tryCatch({
    message("\nPreparing prediction dataset...")

    # copy features to filter out unwanted data
    features_filtered = features
    features_filtered$time = NULL
    features_filtered$FID = NULL
    features_filtered$geometry = NULL
    features_filtered$x = NULL
    features_filtered$y = NULL

    message("Data preperation finished!")
  },
  error = function(err)
  {
    message("An Error occured!")
    message(toString(err))
    stop()
  })

  tryCatch({
    message("\nPerform predicition...")

    # get model from user workspace
    model = readRDS(paste0(Session$getConfig()$workspace.path, "/", model_id, ".rds"))

    # predict classes
    predicted_classes = stats::predict(model, newdata = features_filtered)

    # get class probalilities
    prediction_confidence = stats::predict(model, newdata = features_filtered, type = "prob")

    # get column with only the highest class prob
    max_confidence_per_pixel = apply(prediction_confidence, 1, base::max)

    message("Prediction completed!")
  },
  error = function(err)
  {
    message("An Error occured!")
    message(toString(err))
    stop()
  })


  tryCatch({
    message("\nCreate output dataframe...")

    # create data.frame of same length as features
    output_dataframe = base::as.data.frame(base::matrix(NA,
                                                        nrow = nrow(features),
                                                        ncol = 1,
                                                        dimnames = list(c(), "FID")))

    output_dataframe$FID = features$FID
    output_dataframe$class = predicted_classes
    output_dataframe$class_confidence = max_confidence_per_pixel
    output_dataframe$x = features$x
    output_dataframe$y = features$y

    # convert output to spatial dataframe
    output_dataframe = sf::st_as_sf(output_dataframe, coords = c("x", "y"), crs = gdalcubes::srs(data))

    message("Output dataframe created!")
  },
  error = function(err)
  {
    message("An Error occured!")
    message(toString(err))
    stop()
  })

  return(output_dataframe)
}


#' predict_model
predict_model <- Process$new(
  id = "predict_model",
  description = "Perform a prediction on a datacube based on the given model. This approach extracts each pixel value as a row of a data.frame. This data.frame is then used to predict class values for each pixel. Outputs of this process can only be saved as RDS or NetCDF!",
  categories = as.array("machine-learning", "cubes"),
  summary = "Predict data on datacube based on a data.frame.",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "The data to work with.",
      schema = list(
        type = "object",
        subtype = "raster-cube"
      )
    ),
    Parameter$new(
      name = "model_id",
      description = "Id of the model that should be used for prediction. The model will be searched in the workspace of openeocubes.",
      schema = list(
        type = "string"
      )
    )
  ),
  returns = list(
    description = "Spatial data frame containing the geometry, class and class confidence for each pixel",
    schema = list(type = "data.frame")
  ),
  operation = predict_model_opp
)
