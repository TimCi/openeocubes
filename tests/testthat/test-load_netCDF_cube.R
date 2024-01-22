test_that("loading netCDF cube works", {

  # create Session-object for testing
  config <- SessionConfig(api.port = 8000, host = "127.0.0.1")

  # set workspace for testing
  config$workspace.path = paste0(getwd(),"/", test_path("testData"))

  # this silently return "Session"
  createSessionInstance(config)

  # try loading the datacube
  datacube = load_netCDF_cube_opp("cube")

  comparioson_datacube = gdalcubes::ncdf_cube(test_path("testData", "cube.nc"))

  # test for same class as both datacube have the same path (e.g. are identical)
  expect_equal(class(datacube), class(comparioson_datacube))
})
