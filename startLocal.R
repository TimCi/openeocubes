# build and install package locally (use for development)
remotes::install_local("./", dependencies = FALSE, force = TRUE)

# run test for processes of the package
test_res = devtools::test(stop_on_failure = TRUE)

# Start service
library(openeocubes)

config <- SessionConfig(api.port = 8000, host = "127.0.0.1")
# set workspace for testing
config$workspace.path = paste0(getwd(), "/test_workspace")
createSessionInstance(config)

message("\nSession workspace path:")
print(config$workspace.path)

Session$startSession()


