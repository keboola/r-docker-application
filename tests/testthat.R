library(testthat)

# override with config if any
if (file.exists("config.R")) {
    source("config.R")
}

test_check("keboola.r.docker.application")
