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

test_that("invalid directory", {
    app <- DockerApplication$new('non-existent-dir')
    
    expect_that(
        app$readConfig(),
        throws_error()
    )
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

test_that("table manifest 1", {
    someFile <- file.path(tempdir(), 'some-table.csv')
    app <- DockerApplication$new(dirname(someFile))
    
    app$writeTableManifest(someFile, 'out.c-main.some-table', primaryKey = c('foo', 'bar'))
    manifestFile = paste0(someFile, '.manifest')
    data <- readChar(manifestFile, file.info(manifestFile)$size)
    config <- jsonlite::fromJSON(data)
    
    expect_equal(
        config,
        list(
            'destination' = 'out.c-main.some-table',
            'primary_key' = c('foo', 'bar')
        )
    )
    file.remove(manifestFile)
})

test_that("run without init", {
    app <- DockerApplication$new()
    app$logDebug("test")
})

test_that("config accessors", {
    app <- DockerApplication$new(KBC_DATA_DIR)
    app$readConfig()
    
    params <- app$getParameters()
    expect_equal('bazBar', params$baz)
    expect_equal(42, params$fooBar$foo)
    expect_equal(24, params$fooBar$bar)

    tables <- app$getInputTables()
    expect_equal(2, nrow(tables))
    expect_equal('in.c-main.test', tables[which(tables$destination == 'sample.csv'), 'source'])
    expect_equal('in.c-main.test2', tables[which(tables$destination == 'fooBar.csv'), 'source'])
    expect_equal(c(TRUE, TRUE), file.exists(tables$full_path))
    
    table <- app$getTableManifest('sample.csv')
    expect_equal('in.c-main.test', table$id)
    expect_equal(13, length(table$columns))
    
    table2 <- app$getTableManifest('sample')
    expect_equal(table, table2)
    
    files <- app$getInputFiles()
    expect_equal(5, length(files))
    expect_equal('21702.strip.print.gif', substr(files[1], nchar(files[1]) - 20, nchar(files[1])))
    
    file <- app$getFileManifest(files[1])
    expect_equal(151971405, file$id)
    expect_equal('21702.strip.print.gif', file$name)
    expect_equal('dilbert', file$tags)
    file2 <- app$getFileManifest('151971405_21702.strip.print.gif')
    expect_equal(file, file2)
    
    files <- app$getExpectedOutputFiles()
    expect_equal(1, nrow(files))
    expect_equal('processed.png', files[1, 'source'])
    
    tables <- app$getExpectedOutputTables()
    expect_equal(2, nrow(tables))
    expect_equal('results.csv', tables[1, 'source'])
    expect_equal('results-new.csv', tables[2, 'source'])
})