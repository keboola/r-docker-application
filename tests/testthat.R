library(testthat)

KBC_DATA_DIR = '../data/'

# override with config if any
if (file.exists("config.R")) {
    source("config.R")
}

# override with environment if any
if (nchar(Sys.getenv("KBC_DATA_DIR")) > 0) {
    KBC_DATA_DIR <- Sys.getenv("KBC_DATA_DIR")  
}

test_check("keboola.r.docker.application")
