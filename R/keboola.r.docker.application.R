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
            callSuper(FALSE)
            
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
            } else {
                debugMode <<- FALSE
            }
            configData
        },
        
        
        #' Write manifest for output file. Manifest is used for the file to be stored in KBC Storage.
        #'  
        #' @param fileName Local file name of the file to be stored, including path.
        #' @param fileTags Vector of file tags. Note that a tag 'rDocker' is added automatically.
        #' @param isPublic Logical true if the file should be stored as public.
        #' @param isPermananet Logical false if the file should be stored only temporarily (for days), otherwise it will be stored until deleted.
        #' @param notify Logical true if members of the project should be notified about the file upload.
        writeFileManifest = function(fileName, fileTags = vector(), isPublic = FALSE, isPermanent = TRUE, notify = FALSE)
        {
            content = list()
            content[['is_public']] <- isPublic
            content[['is_permanent']] <- isPermanent
            content[['notify']] <- notify
            if (length(fileTags) > 0) {
                content[['tags']] <- fileTags
            }
            json <- jsonlite::toJSON(x = content, auto_unbox = TRUE, pretty = TRUE)
            fileConn <- file(paste0(fileName, '.manifest'))
            writeLines(json, fileConn)
            close(fileConn)
        }        
    )
)