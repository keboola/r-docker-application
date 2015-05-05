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
    configData <- app$readConfig()
    
    expect_equal(configData, app$configData)
      
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
        configData <- app$readConfig(),
        throws_error()
    )
    file.remove(configFile)    
})
