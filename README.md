## requirements

[![Build Status](https://travis-ci.org/bmewing/requirements.svg?branch=master)](https://travis-ci.org/bmewing/requirements) [![Coverage Status](https://img.shields.io/codecov/c/github/bmewing/requirements/master.svg)](https://codecov.io/github/bmewing/requirements?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/requirements)](https://CRAN.R-project.org/package=requirements) ![](http://cranlogs.r-pkg.org/badges/requirements)

An R package to support requirements.txt style project dependencies.

### requirements.txt

Simply include a text file named requirements.txt in your project root directory. It should look something like:

```
mgsub >= 1.5.0
dplyr == 0.7.0
```

Note that you must have spaces between package names, the comparison operator, and the version.

`requirements()` will parse this file and check against installed packages if you need to install a different version. If a new version is needed, `devtools::install_version` is used to fetch it. There is an option to load all required packages to the namespace.

## But why?

Package installation and loading is often a struggle in R. Lots of scripts start with code like:

```
if(!require(mgsub)) install.packages('mgsub')
```

This checks if a package is already installed and if not, installs it. Of course, this example fails to then load the package so the code will fail. But this always installs the latest version available for the person running the code. This can be an issue because the pacakges your code depends on may have made breaking changes. Now you've installed an incompatible version.

### packrat

packrat is a great way of bundling code and packages together. It allows you to have multiple versions of packages installed across different projects. `requirements` is 100% comaptible with packrat, if you're using packrat it will check against project specifc versions and install into the packrat library. This allows you to have a standard set of packages that you use, including versions, which can initialize new projects without multiple installation commands.

### pacman

pacman provides a new command, `pacman::p_load()` which will install a package if it's not available. However, it does not allow you to specify specifc versions.
