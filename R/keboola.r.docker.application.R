#' Application which serves as a backend for component which runs 
#'  inside Docker with interface to docker-bundle.
#' @import methods
#' @import keboola.r.application
#' @export DockerApplication
#' @exportClass DockerApplication
DockerApplication <- setRefClass(
    'DockerApplication',
    contains = c("Application"),
    fields = list(
        'dataDir' = 'character',
        'configData' = 'list'
    ),
    methods = list(
        #' Constructor.
        #'
        #' @param Optional name of data directory, if not supplied then it
        #'  will be read from command line argument.
        #' @exportMethod
        initialize = function(args = NULL) {
            if (is.null(args)) {
                args <- commandArgs(trailingOnly = TRUE)
            }
            dataDir <<- args[1]
            if (empty(dataDir)) {
                stop("Data directory must be entered as first argument.")
            }
        },
       
        #' Read configuration file
        #' 
        #' @return List with parsed configuration file structure,
        #'  also accessible as configData property.
        #' @exportMethod
        readConfig = function() {
            configFile <- file.path(normalizePath(dataDir, mustWork = TRUE), 'config.json')
            if (!file.exists(configFile)) {
                stop(paste0("Configuration file not found ", configFile))
            }
            data <- readChar(configFile, file.info(configFile)$size)
            
            tryCatch({
                configData <<- jsonlite::fromJSON(data)
            }, error = function (e) {
                stop(paste0("Failed to parse JSON configuration ", e$message));
            })
            if (!is.null(configData$parameters$debug)) {
                debugMode <<- as.logical(configData$parameters$debug)
            }
            configData
        }
    )
)