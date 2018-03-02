#' Abstract class for a component. Provides basic functions
#'  like logging, working with environment and error handling
#' @import methods
#' @export Component
#' @exportClass Component
Component <- setRefClass(
    'Component',
    fields = list(
        debugMode = 'logical',
        # for wrapTryCatch
        hasFailed = 'logical',
        messages = 'list',
        warnings = 'list',
        # internal for splitString
        logTokens = 'ANY'
    ),
    methods = list(
        initialize = function(debugMode = FALSE) {
            "Constructor.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{debugMode} Set higher logging level so that debug messages are printed.}
            }}"
            debugMode <<- debugMode
        },

        setDebugMode = function(debugMode) {
            "Change debugging mode.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{debugMode} TRUE to turn debugging on.}
            }}
            \\subsection{Return Value}{New debugMode}"
            debugMode <<- debugMode
        },

        empty = function(obj) {
            "Verify that a value is empty
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{obj} Character or number or vector of those.}
            }}
            \\subsection{Return Value}{TRUE if the value is empty (array or string)}"
            ret <- FALSE
            if (length(obj) == 0) {
                ret <- TRUE
            } else {
                tmp <- sapply(obj, is.na)
                if (length(tmp[tmp == TRUE]) == length(tmp)) {
                    ret <- TRUE
                } else {
                    tmp <- sapply(obj, is.null)
                    if (length(tmp[tmp == TRUE]) == length(tmp)) {
                        ret <- TRUE
                    } else {
                        if (class(obj) == 'character') {
                            tmp <- (nchar(obj) == 0)
                        } else if (class(obj) == 'numeric') {
                            tmp <- obj == 0
                        }
                        if (length(tmp[tmp == TRUE]) == length(tmp)) {
                            ret <- TRUE
                        }
                    }
                }
            }
            ret
        },
        
        getEnv = function(name) {
            "Get system environment variable
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{name} Name of the environment variable.}
            }}
            \\subsection{Return Value}{Value of the environment variable or null if it does not exist.}"
            value <- Sys.getenv(name)
            if (.self$empty(value)) {
                ret <- NULL
            } else {
                ret <- value
            }
            ret
        },
                
        logDebug = function(obj) {
            "Log a debugging message.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{obj} Arbitrary message or printable object.}
            }}
            \\subsection{Return Value}{NULL}"
            if (debugMode) {
                .self$printLog(obj, 'stdout')
            }
            NULL
        },
        
        logInfo = function(obj) {
            "Log an informational message.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{obj} Arbitrary message or printable object.}
            }}
            \\subsection{Return Value}{NULL}"
            .self$printLog(obj, 'stdout')
            NULL
        },
        
        logWarning = function(obj) {
            "Log a warning message.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{obj} Arbitrary message or printable object.}
            }}
            \\subsection{Return Value}{NULL}"
            .self$printLog(obj, 'stdout')
            NULL
        },

        logError = function(obj) {
            "Log an error message.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{obj} Arbitrary message or printable object.}
            }}
            \\subsection{Return Value}{NULL}"
            .self$printLog(obj, 'stderr')
            NULL
        },

        printLog = function(msg, mode = 'stdout') {
            "Helper function to print timestamp with each message.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{msg} Arbitrary message or printable object.}
            \\item{\\code{mode} Logging mode either \\code{stdout} or \\code{stderr}.}
            }}
            \\subsection{Return Value}{NULL}"
            printOut <- ""
            if (!interactive()) {
                con <- textConnection("printOut", open = "w", local = TRUE)
                sink(con, type = c("output", "message"))
            }
            if (is.character(msg)) {
                if (length(msg) > 1) {
                    for (i in msg) {
                        cat(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ':', i))
                    }
                } else {
                    cat(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ':', msg))
                }
            } else {
                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"))
                print(msg)
            }
            if (!interactive()) {
                sink(NULL, type = c("output", "message"))
                close(con)
            }
            if (mode == 'stdout') {
                write(printOut, stdout())
            } else {
                write(printOut, stderr())
            }
            NULL
        },

        wrapTryCatch = function(expr, silentSuccess = FALSE, stopIsFatal = TRUE) {
            "Error handling wrapper which prints Java like stack trace in case of error.
            Comes from http://stackoverflow.com/a/24884348/41640
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{expr} An expression to execute.}
            \\item{\\code{silentSuccess} In case of success, do not print any messages.}
            \\item{\\code{stopIsFatal} In case of error, do stop the script execution.}
            }}
            \\subsection{Return Value}{NULL}"
            hasFailed <<- FALSE
            messages <<- list()
            warnings <<- list()
            logger <- function(obj) {
                # Change behaviour based on type of message
                level = sapply(class(obj), switch, debug="DEBUG", message="INFO", warning="WARN", caughtError = "ERROR",
                               error=if (stopIsFatal) "FATAL" else "ERROR", "")
                level = c(level[level != ""], "ERROR")[1]
                simpleMessage = switch(level, DEBUG=,INFO=TRUE, FALSE)
                quashable = switch(level, DEBUG=,INFO=,WARN=TRUE, FALSE)
                
                # Format message
                time  = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
                txt   = conditionMessage(obj)
                if (!simpleMessage) txt = paste(txt, "\n", sep="")
                msg = paste(time, level, txt, sep=" ")
                calls = sys.calls()
                calls = calls[1:length(calls)-1]
                trace = limitedLabels(c(calls, attr(obj, "calls")))
                if (!simpleMessage && length(trace) > 0) {
                    trace = trace[length(trace):1]
                    msg = paste(msg, "  ", paste("at", trace, collapse="\n  "), "\n", sep="")
                }
                
                # Output message
                if (silentSuccess && !hasFailed && quashable) {
                    messages <<- append(messages, msg)
                    if (level == "WARN") warnings <<- append(warnings, msg)
                } else {
                    if (silentSuccess && !hasFailed) {
                        .self$logInfo(messages)
                        hasFailed <<- TRUE
                    }
                    .self$logError(msg)
                }
                
                # Muffle any redundant output of the same message
                optionalRestart = function(r) { res = findRestart(r); if (!is.null(res)) invokeRestart(res) }
                optionalRestart("muffleMessage")
                optionalRestart("muffleWarning")
            }
            vexpr = withCallingHandlers(
                withVisible(expr),
                debug=logger, message=logger, warning=logger, caughtError=logger, error=logger
            )
            if (silentSuccess && !hasFailed) {
                .self$logWarning(warnings)
            }
            if (vexpr$visible) vexpr$value else invisible(vexpr$value)
        },

        splitString = function(string, splitChar, asLogical = FALSE) {
            "Split a string by a specificed split character.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{string} Arbitrary string.}
            \\item{\\code{splitChar} Split character.}
            \\item{\\code{asLogical} If \\code{TRUE} than a vector of TRUEs indexed by token name will be returned
            if \\code{FALSE} (default) then a vector of tokens will be returned.}
            }}
            \\subsection{Return Value}{Vector of tokens}"
            # split
            tokens <- strsplit(string, splitChar)[[1]]
            # trim whitespace from each item
            tokens <- lapply(tokens, function (x) {gsub("^\\s+|\\s+$", "", x)})                   
            if (asLogical) {
                logTokens <<- logical()
                lapply(tokens, function (x) {logTokens[x] <<- TRUE})
                tokens <- logTokens
            }
            unlist(tokens)
        }
    )
)
