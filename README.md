# R Docker application

[![Build Status](https://travis-ci.org/keboola/r-docker-application.svg?branch=master)](https://travis-ci.org/keboola/r-docker-application)

Helper library for KBC component development. The library provides basic functions related to [docker Runner](https://developers.keboola.com/extend/docker-runner/) 
[common interface](https://developers.keboola.com/extend/common-interface/).

## Installation
Package is available only on Github, so you need to use `devtools` to install the package
```
devtools::install_github('keboola/r-docker-application', ref = 'master')
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

Also see the See [development guide](https://developers.keboola.com/extend/component/implementation/r/) for help with extending KBC and for more examples.
