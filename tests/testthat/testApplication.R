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
            },
            "action": "test"
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
    expect_false(is.null(configData$action))
    
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

test_that("table manifest minimal", {
    someFile <- file.path(tempdir(), 'some-table-1.csv')
    app <- DockerApplication$new(dirname(someFile))
    
    app$writeTableManifest(someFile, primaryKey = c('foo', 'bar'))
    manifestFile = paste0(someFile, '.manifest')
    data <- readChar(manifestFile, file.info(manifestFile)$size)
    config <- jsonlite::fromJSON(data)
    file.copy(manifestFile, "D:/sync/")
    
    expect_equal(
        config,
        list(
            'primary_key' = c('foo', 'bar')
        )
    )
    file.remove(manifestFile)
})

test_that("table manifest maximal", {
    someFile <- file.path(tempdir(), 'some-table-2.csv')
    app <- DockerApplication$new(dirname(someFile))
    
    app$writeTableManifest(someFile, primaryKey = c('foo'),
        destination = 'out.c-main.some-table', columns = c('foo', 'bar'), 
        incremental = TRUE, metadata = list('bar' = 'kochba'),
        columnMetadata = list('foo' = list('name' = 'gogo')), 
        deleteWhere = list('column' = 'pale', 'values' = c('horse', 'inn'), 'operator' = 'ne'))
    manifestFile = paste0(someFile, '.manifest')
    data <- readChar(manifestFile, file.info(manifestFile)$size)
    metadata = list()
    metadata[[1]] = list('key' = jsonlite::unbox('bar'), 'value' = jsonlite::unbox('kochba'))
    columnMetadata = list()
    columnMetadata[['foo']] = list()
    columnMetadata[['foo']][[1]] = list('key' = jsonlite::unbox('name'), 'value' = jsonlite::unbox('gogo'))
    target = list(
        'destination' = jsonlite::unbox('out.c-main.some-table'),
        'primary_key' = c('foo'),
        'columns' = c('foo', 'bar'),
        'incremental' = jsonlite::unbox(TRUE),
        'metadata' = metadata,
        'column_metadata' = columnMetadata,
        'delete_where_column' = jsonlite::unbox('pale'),
        'delete_where_values' = c('horse', 'inn'),
        'delete_where_operator' = jsonlite::unbox('ne')
    )
    jsText <- as.character(jsonlite::toJSON(target, auto_unbox = FALSE, pretty = TRUE))
    jsText <- gsub("[\r\n]", "", jsText)
    data <- gsub("[\r\n]", "", data)
    expect_equal(
        data,
        jsText
    )
    file.remove(manifestFile)
})

test_that("table manifest destination error", {
    someFile <- file.path(tempdir(), 'some-table.csv')
    app <- DockerApplication$new(dirname(someFile))
    expect_error(
        app$writeTableManifest(someFile, destination = list('out.c-main.some-table'))
    )
})

test_that("table manifest primary key error", {
    someFile <- file.path(tempdir(), 'some-table.csv')
    app <- DockerApplication$new(dirname(someFile))
    expect_error(
        app$writeTableManifest(someFile, primaryKey = FALSE)
    )
})

test_that("table manifest metadata error", {
    someFile <- file.path(tempdir(), 'some-table.csv')
    app <- DockerApplication$new(dirname(someFile))
    expect_error(
        app$writeTableManifest(someFile, metadata = c("a", "b"))
    )
})

test_that("table manifest column metadata error", {
    someFile <- file.path(tempdir(), 'some-table.csv')
    app <- DockerApplication$new(dirname(someFile))
    expect_error(
        app$writeTableManifest(someFile, columnMetadata = c("a", "b"))
    )
})

test_that("table manifest column metadata error 2", {
    someFile <- file.path(tempdir(), 'some-table.csv')
    app <- DockerApplication$new(dirname(someFile))
    expect_error(
        app$writeTableManifest(someFile, columnMetadata = list("a" = "b"))
    )
})

test_that("table manifest delete where error", {
    someFile <- file.path(tempdir(), 'some-table.csv')
    app <- DockerApplication$new(dirname(someFile))
    expect_error(
        app$writeTableManifest(someFile, deleteWhere = list("a" = "b"))
    )
})

test_that("table manifest delete where error 2", {
    someFile <- file.path(tempdir(), 'some-table.csv')
    app <- DockerApplication$new(dirname(someFile))
    expect_error(
        app$writeTableManifest(someFile, deleteWhere = list("values" = c("b"), "column" = "a", "operator" = "invalid"))
    )
})

test_that("run without init", {
    app <- DockerApplication$new(KBC_DATADIR)
    # no error should be thrown
    app$logDebug("test")
})

test_that("config accessors", {
    app <- DockerApplication$new(KBC_DATADIR)
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
    
    action <- app$getAction()
    expect_equal('test', action)
})