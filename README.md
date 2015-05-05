# R Docker application ancestor

[![Build Status](https://travis-ci.org/keboola/r-docker-application.svg?branch=master)](https://travis-ci.org/keboola/r-docker-application)

Application "framework" which provides very basic functions related to docker-bundle.

## Installation
Package is available only on Github, so you need to use `devtools` to install the package
```
library('devtools')
install_github('keboola/r-docker-application', ref = 'master')
```

## Examples
```
# Subclass the class to do something useful
MyApplication <- setRefClass(
    'MyApplication',
    contains = c("DockerApplication"),
    fields = list(
    ...
    ),
    methods = list(
    ...
    )
)

app <- MyApplication$new()
app->readConfig()
print(app$configData$parameters)

```
