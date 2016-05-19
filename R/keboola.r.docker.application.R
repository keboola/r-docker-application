#' Application which serves as a backend for component which runs 
#'  inside Docker with interface to docker-bundle.
#' @import methods keboola.r.application jsonlite
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
        initialize = function(args = NULL) {
            "Constructor.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{args} Optional name of data directory, if not supplied then it
            will be read from command line argument or environment KBC_DATADIR.}
            }}
            \\subsection{Return Value}{Response body - either list or string in case the body cannot be parsed as JSON.}"
            callSuper(FALSE)
            
            if (is.null(args)) {
                args <- commandArgs(trailingOnly = TRUE)
            }
            dataDir <<- args[1]
            if (empty(dataDir)) {
                dataDir <<- Sys.getenv("KBC_DATADIR")
            }
            if (empty(dataDir)) {
                stop("Data directory must be entered as first argument.")
            }
        },

        readConfig = function() {
            "Read configuration file.
            List with parsed configuration file structure is accessible as configData property.
            \\subsection{Return Value}{TRUE}"
            configFile <- file.path(normalizePath(dataDir, mustWork = FALSE), 'config.json')
            if (!file.exists(configFile)) {
                stop(paste0("Configuration file not found ", configFile))
            }
            data <- readChar(configFile, file.info(configFile)$size)
            
            tryCatch({
                configData <<- jsonlite::fromJSON(data)
                print(configData)
            }, error = function (e) {
                stop(paste0("Failed to parse JSON configuration ", e$message));
            })
            if (!is.null(configData$parameters$debug)) {
                debugMode <<- as.logical(configData$parameters$debug)
            } else {
                debugMode <<- FALSE
            }
            TRUE
        },
        
        writeFileManifest = function(fileName, fileTags = vector(), isPublic = FALSE, isPermanent = TRUE, notify = FALSE) {
            "Write manifest for output file. Manifest is used for the file to be stored in KBC Storage.
            List with parsed configuration file structure is accessible as configData property.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{fileName} Local file name of the file to be stored, including path.}
            \\item{\\code{fileTags} Vector of file tags.}
            \\item{\\code{isPublic} Logical true if the file should be stored as public.}
            \\item{\\code{isPermananet} Logical false if the file should be stored only temporarily (for days)
            otherwise it will be stored until deleted.}
            \\item{\\code{notify} Logical TRUE if members of the project should be notified about the file upload.}
            }}
            \\subsection{Return Value}{TRUE}"
            content = list()
            content[['is_public']] <- jsonlite::unbox(isPublic)
            content[['is_permanent']] <- jsonlite::unbox(isPermanent)
            content[['notify']] <- jsonlite::unbox(notify)
            if (length(fileTags) > 0) {
                content[['tags']] <- fileTags
            }
            json <- jsonlite::toJSON(x = content, auto_unbox = FALSE, pretty = TRUE)
            fileConn <- file(paste0(fileName, '.manifest'))
            writeLines(json, fileConn)
            close(fileConn)
            TRUE
        },
        
        writeTableManifest = function(fileName, destination, primaryKey = vector()) {
            "Write manifest for output table Manifest is used for the table to be stored in KBC Storage.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{fileName} Local file name of the CSV with table data.}
            \\item{\\code{destination} String name of the table in Storage.}
            \\item{\\code{primaryKey} Vector of columns used for primary key.}
            }}
            \\subsection{Return Value}{TRUE}"
            content = list()
            content[['destination']] <- jsonlite::unbox(destination)
            if (length(primaryKey) > 0) {
                content[['primary_key']] <- primaryKey
            }
            json <- jsonlite::toJSON(x = content, auto_unbox = FALSE, pretty = TRUE)
            fileConn <- file(paste0(fileName, '.manifest'))
            writeLines(json, fileConn)
            close(fileConn)
            TRUE
        },        
        
        getParameters = function() {
            "Get arbitrary parameters specified in the configuration file.
            \\subsection{Return Value}{List with parameters}"
            return(configData$parameters)
        },
        
        getAction = function() {
            "Get  action parameter passed to the configuration
            \\subsection{Return Value}{Action parameter value}"
            return(configData$action)
        },
        
        getInputFiles = function() {
            "Get names of input files. Returns fully classified pathnames.
            \\subsection{Return Value}{List with file names}"
            files <- list.files(file.path(.self$dataDir, 'in', 'files'))
            files <- files[which(substr(files, nchar(files) - 8, nchar(files)) != '.manifest')]
            files <- file.path(.self$dataDir, 'in', 'files', files)
            return(sort(files))
        },
        
        getFileManifest = function(fileName) {
            "Get additional file information stored in file manifest.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{fileName} Destination file name (without .manifest extension).}
            }}
            \\subsection{Return Value}{List with manifest options}"
            baseDir <- file.path(.self$dataDir, 'in', 'files')
            if (substr(fileName, 0, nchar(baseDir)) != baseDir) {
                fileName <- file.path(baseDir, fileName)
            }
            manifestPath <- paste0(fileName, '.manifest')
            data <- readChar(manifestPath, file.info(manifestPath)$size)
            manifest <- jsonlite::fromJSON(data)
            return(manifest)
        },
               
        getExpectedOutputFiles = function() {
            "Get files which are supposed to be returned when the application finishes.
            \\subsection{Return Value}{data.frame with output files}"
            files <- .self$configData$storage$output$files
            return(files)
        },
        
        getInputTables = function() {
            "Get input tables specified in the configuration file. Tables are identified by
            their destination (.csv file) or full_path.
            \\subsection{Return Value}{data.frame with output tables}"
            tables <- .self$configData$storage$input$tables
            tables$full_path <- file.path(.self$dataDir, 'in', 'tables', tables$destination)
            return(tables)
        },
        
        getTableManifest = function(tableName) {
            "Get additional table information stored in table manifest.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{tableName} Destination table name (name of .csv file).}
            }}
            \\subsection{Return Value}{List with manifest options}"
            if (substr(tableName, nchar(tableName) - 3, nchar(tableName)) != '.csv') {
                tableName <- paste0(tableName, '.csv')
            }
            manifestPath <- file.path(.self$dataDir, 'in', 'tables', paste0(tableName, '.manifest'))
            data <- readChar(manifestPath, file.info(manifestPath)$size)
            manifest <- jsonlite::fromJSON(data)
            return(manifest)
        },
        
        getExpectedOutputTables = function() {
            "Get tables which are supposed to be returned when the application finishes.
            \\subsection{Return Value}{data.frame with expected output tables.}"
            tables <- .self$configData$storage$output$tables
            tables$full_path <- file.path(.self$dataDir, 'out', 'tables', tables$source)
            return(tables)
        }
    )
)
