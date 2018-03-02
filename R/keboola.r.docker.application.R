#' Application which serves as a backend for component which runs 
#'  inside Docker with interface to docker-bundle.
#' @import methods jsonlite
#' @export DockerApplication
#' @exportClass DockerApplication
DockerApplication <- setRefClass(
    'DockerApplication',
    contains = c("Component"),
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
        
        writeTableManifest = function(fileName, destination = NULL, primaryKey = vector(),
                                      columns = vector(), incremental = FALSE, metadata = list(),
                                      columnMetadata = list(), deleteWhere = list()) {
            "Write manifest for output table Manifest is used for the table to be stored in KBC Storage.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{fileName} Local file name of the CSV with table data.}
            \\item{\\code{destination} String name of the table in Storage.}
            \\item{\\code{primaryKey} Vector of columns used for primary key.}
            \\item{\\code{columns} Vector of columns names for headless CSV file.}
            \\item{\\code{incremental} Set to TRUE to enable incremental loading.}
            \\item{\\code{metadata} Key value list of table metadata.}
            \\item{\\code{columnMetadata} List indexed by column name of key-value list of column metadata.}
            \\item{\\code{deleteWhere} List with items 'column', 'values', 'operator' to specify rows to delete.}
            }}
            \\subsection{Return Value}{TRUE}"
            content = list()
            if (!is.null(destination)) {
                if (typeof(destination) != 'character') {
                    stop("Destination must be a string.")
                }
                content[['destination']] <- jsonlite::unbox(destination)
            }
            if (length(primaryKey) > 0) {
                if (typeof(primaryKey) != 'character') {
                    stop("PrimaryKey must be a character vector")
                }
                content[['primary_key']] <- primaryKey
            }
            if (length(columns) > 0) {
                content[['columns']] <- columns
            }
            if (incremental) {
                content[['incremental']] <- jsonlite::unbox(incremental)
            }
            content <- .self$processMetadata(content, metadata)
            content <- .self$processColumnMetadata(content, columnMetadata)
            content <- .self$processDeleteWhere(content, deleteWhere)
            json <- jsonlite::toJSON(x = content, auto_unbox = FALSE, pretty = TRUE)
            fileConn <- file(paste0(fileName, '.manifest'))
            writeLines(json, fileConn)
            close(fileConn)
            TRUE
        },
        
        processMetadata = function(manifest, metadata) {
            if (typeof(metadata) != 'list') {
                stop("Table metadata must be a list.");
            }
            if (length(metadata) > 0) {
                manifest[['metadata']] = list()
                i = 1
                for (name in names(metadata)) {
                    manifest[['metadata']][[i]] = c(
                        manifest[['metadata']], 
                        list('key' = jsonlite::unbox(name),
                             'value' = jsonlite::unbox(metadata[[name]]))
                    )
                    i = i + 1
                }
            }
            return(manifest)
        },
        
        processColumnMetadata = function(manifest, columnMetadata) {
            if (typeof(columnMetadata) != 'list') {
                stop("Column metadata must be a list.")
            }
            if (length(columnMetadata) > 0) {
                manifest[['column_metadata']] = list()
                for (column in names(columnMetadata)) {
                    manifest[['column_metadata']][[column]] = list()
                    if (typeof(columnMetadata[[column]]) != 'list') {
                        stop("Column metadata must be a list of lists.")
                    }
                    i = 1
                    for (name in names(columnMetadata[[column]])) {
                        manifest[['column_metadata']][[column]][[i]] <- list('key' = jsonlite::unbox(name),
                                 'value' = jsonlite::unbox(columnMetadata[[column]][[name]]))
                        i = i + 1
                    }
                }
            }
            return(manifest)
        },
        
        processDeleteWhere = function(manifest, deleteWhere) {
            if (typeof(deleteWhere) != 'list') {
                stop("Delete-where specification must be a list.")
            }
            if (length(deleteWhere) > 0) {
                if (is.null(deleteWhere[['column']]) ||
                    is.null(deleteWhere[['values']])) {
                    stop("Delete-where list must contain items 'column' and 'values'.")
                }
                if (typeof(deleteWhere[['values']]) != 'character') {
                    stop('Delete-where values must be a character vector')
                }
                if (typeof(deleteWhere[['column']]) != 'character') {
                    stop('Delete-where column must be a character')
                }
                if (is.null(deleteWhere[['operator']])) {
                    op = 'eq'
                } else {
                    op = deleteWhere[['operator']]
                }
                if (op != 'eq' && op != 'ne') {
                    stop('Delete-where operator must be "eq" or "ne".')
                }
                manifest[['delete_where_column']] <- jsonlite::unbox(deleteWhere[['column']])
                manifest[['delete_where_values']] <- deleteWhere[['values']]
                manifest[['delete_where_operator']] <- jsonlite::unbox(deleteWhere[['operator']])
            }
            return(manifest)
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
