# R Docker application

[![Build Status](https://travis-ci.org/keboola/r-docker-application.svg?branch=master)](https://travis-ci.org/keboola/r-docker-application)

Application "framework" which provides basic functions related to [docker-bundle](https://github.com/keboola/docker-bundle).

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

Also see the See [development guide](http://developers.keboola.com/extend/custom-science/r/) for help with KBC integration and for more examples.
