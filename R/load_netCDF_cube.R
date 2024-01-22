#' @export
load_netCDF_cube_opp = function(cube_id, job)
{
  message("\nload_netCDF_cube called...")
  message("\ncube_id:")
  message(cube_id)

  tryCatch({
    message("\nTry loading netCDF cube...")

    cubepath = paste0(Session$getConfig()$workspace.path, "/", cube_id, ".nc")

    message("\npath to cube: ", cubepath)

    cube = gdalcubes::ncdf_cube(cubepath)

    message("netCDF cube loaded!")
  },
  error = function(err)
  {
    message("An Error occured!")
    message(toString(err))
    stop()
  })

  return(cube)
}


#' load_netCDF_cube
load_netCDF_cube <- Process$new(
  id = "load_netCDF_cube",
  description = "Loads netCDF cube from user workspace via its file name.",
  categories = as.array("cubes"),
  summary = "Load netCDF cube..",
  parameters = list(
    Parameter$new(
      name = "cube_id",
      description = "Filename of the netCDF cube.",
      schema = list(
        type = "string"
      )
    )),
  returns = eo_datacube,
  operation = load_netCDF_cube_opp
)
