# requirements <img src='https://s3.amazonaws.com/mgsub/requirements_hex.png' width=125 align='right'/>

[![Build Status](https://travis-ci.org/bmewing/requirements.svg?branch=master)](https://travis-ci.org/bmewing/requirements) 
[![Coverage Status](https://img.shields.io/codecov/c/github/bmewing/requirements/master.svg)](https://codecov.io/github/bmewing/requirements?branch=master) 
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/requirements)](https://CRAN.R-project.org/package=requirements) 
![](http://cranlogs.r-pkg.org/badges/requirements)

An R package to support `pip` style project dependency management.

## requirements.txt

The key player for the `requirements` package is the `requirements.txt` file in your project directory. It stores all the external package dependencies for your project along with optional version details. It is typically placed in the root of the project directory as shown below.

```
your_project_directory
|-- requirements.txt
|-- analysis.R
|-- data
|   |-- first_data.csv
|   |-- second_data.Rdata
|-- .git
```

It should be a plain text file with one requirement per line and an empty final line. In the example below we indicate that we need any version of `lexRankr`, a minimum version of `mgsub`, and a specific version of `dplyr`. You can write this file by hand or use `requirements::rip_freeze()` to discover all packages currently in use in your project and automatically generate the file (which can then be edited by hand).

```
lexRankr
mgsub >= 1.5.0
dplyr == 0.7.0

```

If you're starting work in a new environment (e.g. new server, new container) you can easily install all requirements via `requirements::rip_install()`. This package leverages `remotes` and `metacran` to install specific versions that match your packages requirements.

### Non-CRAN dependencies

You can also detail non-CRAN dependencies in `requirements.txt`, similar to how `pip` supports these types of dependencies. Beyond supporting version control systems like git/svn, there's support for Bioconductor, generic URLs and local files.

* git/github: `git+davidgohel/officer`
* svn: `svn+svn://scm.r-forge.r-project.org/svnroot/daewr/pkg/mixexp`
* bioconductor: `bioc+release/SummarizedExperiment`
* generic url source: `https://github.com/bmewing/mgsub/releases/download/v.1.5/mgsub_1.5.0.tar.gz`
* local file: `testdata/my_package.tar.gz`

### Nesting requirements

You can also source in a requirements file from a requirements file. If you had one 'standard' set of requirements plus one project specific you could have a directory structure that looks like this.

```
your_project_directory
|-- requirements.txt
|-- standard_reqs.txt
|-- analysis.R
|-- data
|   |-- first_data.csv
|   |-- second_data.Rdata
|-- .git
```

#### `standard_reqs.txt`

```
# Standard packages for all projects
data.table #need to identify standard version
magrittr

```

#### `requirements.txt`

```
lexRankr
mgsub >= 1.5.0
-r standard_reqs.txt

```

If you were to run `rip_install('requirements.txt')` you would install all the dependencies across both files. The line `-r standard_reqs.txt` indicates the contents of the file should be read and included in the process. Also note that you can include comments (denoted with `#`) without impacting the functionality.

* `lexRankr`
* `mgsub >= 1.5.0`
* `data.table`
* `magrittr`

## But why?

Collaboration (both with others and future self) can be challenging. Ensuring all packages that a project depends on are available is not easy with the base R toolset. Another time this is valuable is when upgrading R, and installed packages are not longer available to the new version of R. 

With `requirements` you only need to install one lightweight package and write one human-readable file detailing your dependencies, then a single function call ensures you have all your dependencies available.

### In-line installation

```
if(!require(mgsub)) install.packages('mgsub')
```

Often code may start with a line that checks if a package is already available and if not, install it. Of course, this example fails to load the package after installation so a user may still have issues running a file. There are good arguments that analysis code should not be in the business of installing packages which might affect other projects, so code like this could be considered undesirable.

The other issue here is that this always installs the latest version available for the person running the code. This can be an issue because the packages your code depends on may have made breaking changes. Now you've installed an incompatible version.

### `pacman`

`pacman` provides a new command, `pacman::p_load()` which will install a package if it's not available and then load it. However, it does not allow you to specify specific versions, so it's not really a solution to managing requirements.

### `packrat`

`packrat` is a great way of bundling code and packages together. It allows you to have multiple versions of packages installed across different projects. `requirements` is 100% compatible with `packrat`, if you're using `packrat` it will check against project specific versions and install into the `packrat` library. This allows you to have a standard set of packages that you use, including versions, which can initialize new projects without multiple installation commands. It also frees you from the complexities of sharing `packrat` environments between different people and provides a more human readable list of project dependencies.

## Why `rip_*`?

`pip` stand for 'Python Installs Packages' and so we wanted to call the project `rip` for 'R Installs Packages'. Sadly, that name is already in use on CRAN, so we satisfied ourselves by leveraging `rip` in our function names.

* `rip_install()` == `pip install -r requirements.txt`
* `rip_freeze()` == `pip freeze`
