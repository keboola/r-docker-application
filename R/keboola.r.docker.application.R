#' Application which serves as a backend for component which runs 
#'  inside Docker with interface to docker-bundle.
#' @import methods
#' @import keboola.r.application
#' @import jsonlite
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
        #'  will be read from command line argument or environment KBC_DATA_DIR.
        #' @exportMethod
        initialize = function(args = NULL) {
            callSuper(FALSE)
            
            if (is.null(args)) {
                args <- commandArgs(trailingOnly = TRUE)
            }
            dataDir <<- args[1]
            if (empty(dataDir)) {
                dataDir <<- Sys.getenv("KBC_DATA_DIR")
            }
            if (empty(dataDir)) {
                stop("Data directory must be entered as first argument.")
            }
        },
       
        #' Read configuration file
        #' 
        #' List with parsed configuration file structure is accessible as configData property.
        #' @return logical TRUE
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
            TRUE
        },
        
        
        #' Write manifest for output file. Manifest is used for the file to be stored in KBC Storage.
        #'  
        #' @param fileName Local file name of the file to be stored, including path.
        #' @param fileTags Vector of file tags.
        #' @param isPublic Logical true if the file should be stored as public.
        #' @param isPermananet Logical false if the file should be stored only temporarily (for days), otherwise it will be stored until deleted.
        #' @param notify Logical true if members of the project should be notified about the file upload.
        #' @exportMethod
        writeFileManifest = function(fileName, fileTags = vector(), isPublic = FALSE, isPermanent = TRUE, notify = FALSE)
        {
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
        },
        
        
        #' Write manifest for output table Manifest is used for the table to be stored in KBC Storage.
        #'  
        #' @param primaryKey Vector of columns used for primary key.
        #' @param indexedColumns Vector of columns which are indexed.
        #' @exportMethod
        writeTableManifest = function(fileName, primaryKey = vector(), indexedColumns = vector())
        {
            content = list()
            if (length(primaryKey) > 0) {
                content[['primary_key']] <- primary_key
            }
            if (length(indexedColumns) > 0) {
                content[['indexed_columns']] <- indexedColumns
            }
            json <- jsonlite::toJSON(x = content, auto_unbox = FALSE, pretty = TRUE)
            fileConn <- file(paste0(fileName, '.manifest'))
            writeLines(json, fileConn)
            close(fileConn)
        },        
        
        
        #' Get arbitrary parameters specified in the configuration file.
        #' 
        #' @return list
        #' @exportMethod 
        getParameters = function()
        {
            return(configData$parameters)
        },
        
        
        #' Get names of input files. Returns fully classified pathnames.
        #'
        #' @return character 
        #' @exportMethod 
        getInputFiles = function()
        {
            files <- list.files(file.path(.self$dataDir, 'in', 'files'))
            files <- files[which(substr(files, nchar(files) - 8, nchar(files)) != '.manifest')]
            files <- file.path(.self$dataDir, 'in', 'files', files)
            return(sort(files))
        },
        
        
        #' Get additional file information stored in file manifest
        #' 
        #' @param string Destination table name (name of .csv file).
        #' @return list 
        #' @exportMethod
        getFileManifest = function(fileName)
        {
            baseDir <- file.path(.self$dataDir, 'in', 'files')
            if (substr(fileName, 0, nchar(baseDir)) != baseDir) {
                fileName <- file.path(baseDir, fileName)
            }
            manifestPath <- paste0(fileName, '.manifest')
            data <- readChar(manifestPath, file.info(manifestPath)$size)
            manifest <- jsonlite::fromJSON(data)
            return(manifest)
        },
               
        
        #' Get files which are supposed to be returned when the application finishes.
        #' 
        #' @return data.frame
        #' @exportMethod 
        getExpectedOutputFiles = function()
        {
            files <- .self$configData$storage$output$files
            return(files)
        },
        
        
        #' Get input tables specified in the configuration file. Tables are identified by 
        #'  their destination (.csv file) or full_path.
        #' 
        #' @return data.frame
        #' @exportMethod
        getInputTables = function()
        {
            tables <- .self$configData$storage$input$tables
            tables$full_path <- file.path(.self$dataDir, 'in', 'tables', tables$destination)
            return(tables)
        },
        
        
        #' Get additional table information stored in table manifest
        #' 
        #' @param string Destination table name (name of .csv file).
        #' @return list
        #' @exportMethod
        getTableManifest = function(tableName)
        {
            if (substr(tableName, nchar(tableName) - 3, nchar(tableName)) != '.csv') {
                tableName <- paste0(tableName, '.csv')
            }
            manifestPath <- file.path(.self$dataDir, 'in', 'tables', paste0(tableName, '.manifest'))
            data <- readChar(manifestPath, file.info(manifestPath)$size)
            manifest <- jsonlite::fromJSON(data)
            return(manifest)
        },
        
        
        #' Get tables which are supposed to be returned when the application finishes.
        #' 
        #' @return data.frame
        #' @exportMethod 
        getExpectedOutputTables = function()
        {
            tables <- .self$configData$storage$output$tables
            tables$full_path <- file.path(.self$dataDir, 'out', 'tables', tables$source)
            return(tables)
        }
    )
)