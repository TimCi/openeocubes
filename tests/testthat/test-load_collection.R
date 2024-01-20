test_that("loading a collection works", {

  datacube = gdalcubes::ncdf_cube(test_path("testData", "cube.nc"))

  xmin = 854523.1986700408
  ymin = 6797063.516360302
  xmax = 857831.1917130196
  ymax = 6799315.301182906

  bands = c("B02", "B03", "B04", "B05", "B06","B07", "B08", "B8A", "B11", "B12")



  result = spsUtil::quiet(load_collection_opp(id = "sentinel-s2-l2a-cogs",
                      spatial_extent = list(west= xmin ,
                                            south= ymin,
                                            east= xmax,
                                            north= ymax),
                      crs = 3857,
                      temporal_extent = c("2022-06-01", "2022-06-30"),
                      bands = bands,
                      resolution = 30))



  cube_xmin = gdalcubes::dimensions(result)$x$low
  cube_ymin = gdalcubes::dimensions(result)$y$low
  cube_xmax = gdalcubes::dimensions(result)$x$high
  cube_ymax = gdalcubes::dimensions(result)$y$high

  expect_equal(cube_xmin, gdalcubes::dimensions(datacube)$x$low)
  expect_equal(cube_ymin, gdalcubes::dimensions(datacube)$y$low)
  expect_equal(cube_xmax, gdalcubes::dimensions(datacube)$x$high)
  expect_equal(cube_ymax, gdalcubes::dimensions(datacube)$y$high)
})
