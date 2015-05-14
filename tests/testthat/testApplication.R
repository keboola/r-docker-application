test_that("config file", {
    configFile <- file.path(tempdir(), 'config.json')
    configData <- '
        {
            "parameters": {
                "debug": true,
                "script": [
                    "line 1",
                    "line 2"
                ],
                "scriptParameters": {
                    "fooBar": "baz"
                }
            }
        }
    '
    write(file = configFile, x = configData)
    app <- DockerApplication$new(dirname(configFile))
    app$readConfig()
    
    configData <- app$configData
      
    # verify config structure
    expect_false(is.null(configData$parameters))
    expect_false(is.null(configData$parameters$debug))
    expect_false(is.null(configData$parameters$script))
    expect_false(is.null(configData$parameters$scriptParameters))
    
    expect_true(app$debugMode)
    file.remove(configFile)
})

test_that("config directory exception", {
    configFile <- file.path(paste(tempdir()), 'config.json')
    file.remove(configFile)
    app <- DockerApplication$new(dirname(configFile))
    expect_that(
        app$readConfig(),
        throws_error()
    )
})

test_that("config file exception", {
    configFile <- file.path(tempdir(), 'foobar')
    configData <- '
        {
            "parameters": {
                "debug": true,
                "script": [
                    "line 1",
                    "line 2"
                ],
                "scriptParameters": {
                    "fooBar": "baz"
                }
            }
        }
    '    
    write(file = configFile, x = configData)
    
    app <- DockerApplication$new(dirname(configFile))
    expect_that(
        app$readConfig(),
        throws_error()
    )
    file.remove(configFile)    
})

test_that("config file format exception", {
    configFile <- file.path(tempdir(), 'config.json')
    configData <- 'foobar'    
    write(file = configFile, x = configData)
    app <- DockerApplication$new(dirname(configFile))
    
    expect_that(
        app$readConfig(),
        throws_error()
    )
    file.remove(configFile)    
})

test_that("file manifest 1", {
    someFile <- file.path(tempdir(), 'someFile.txt')
    app <- DockerApplication$new(dirname(someFile))
    
    app$writeFileManifest(someFile, fileTags = c('foo', 'bar'), isPublic = TRUE, isPermanent = FALSE, notify = TRUE)
    manifestFile = paste0(someFile, '.manifest')
    data <- readChar(manifestFile, file.info(manifestFile)$size)
    config <- jsonlite::fromJSON(data)
    
    expect_equal(
        config,
        list(
            'is_public' = TRUE,
            'is_permanent' = FALSE,
            'notify' = TRUE,
            'tags' = c('foo', 'bar')
        )
    )
    file.remove(manifestFile)
})

test_that("file manifest 2", {
    someFile <- file.path(tempdir(), 'someFile.txt')
    app <- DockerApplication$new(dirname(someFile))
    
    app$writeFileManifest(someFile)
    manifestFile = paste0(someFile, '.manifest')
    data <- readChar(manifestFile, file.info(manifestFile)$size)
    config <- jsonlite::fromJSON(data)
    
    expect_equal(
        config,
        list(
            'is_public' = FALSE,
            'is_permanent' = TRUE,
            'notify' = FALSE
        )
    )
    file.remove(manifestFile)
})

test_that("file manifest 3", {
    someFile <- file.path(tempdir(), 'someFile.txt')
    app <- DockerApplication$new(dirname(someFile))
    
    app$writeFileManifest(someFile, fileTags = c('foo'), isPublic = TRUE, isPermanent = FALSE, notify = TRUE)
    manifestFile = paste0(someFile, '.manifest')
    data <- readChar(manifestFile, file.info(manifestFile)$size)
    config <- jsonlite::fromJSON(data)
    
    expect_equal(
        config,
        list(
            'is_public' = TRUE,
            'is_permanent' = FALSE,
            'notify' = TRUE,
            'tags' = c('foo')
        )
    )
    file.remove(manifestFile)
})

test_that("run without init", {
    app <- DockerApplication$new()
    app$logDebug("test")
})
