# Trelliscope: Detailed Visualization of Large Complex Data in R

Trelliscope is an R package to be used in conjunction with [datadr](https://github.com/hafen/datadr) and [RHIPE](https://github.com/saptarshiguha/RHIPE) to provide a framework for detailed visualization of large, complex data.

To get started, see the package documentation and function reference located [here](http://hafen.github.com/trelliscope/).

## Installation

For simple use, all that is needed for Trelliscope is R.  Trelliscope depends on the `datadr` package.  To install this package from github, do the following from the R console:

```s
library(devtools)
install_github("datadr", "hafen")
install_github("trelliscope", "hafen")
```

## Optional Components

Trelliscope can operate on its own, but there are several optional components that help Trelliscope scale and make displays available for others to view.  These components are:

- Hadoop
- RHIPE
- MongoDB
- Shiny-server

#### Hadoop

Hadoop can be difficult to install and configure.  There are many resources on the web for this.

#### RHIPE

To use RHIPE with this package, get version 0.73.1 from [here](https://github.com/saptarshiguha/RHIPE).

Installation instructions for RHIPE can be found [here](http://www.datadr.org/install.html).

#### MongoDB

Trelliscope can optionally store "cognostics" for a display in MongoDB.  This is useful for cases where there are millions of subsets in the data.

MongoDB intallation instructions are found [here](http://docs.mongodb.org/manual/installation/).

#### Shiny-server

To share your displays with others, you can deploy your visualizations on a web server using shiny-server.

Installation instructions for shiny-server are found [here](https://github.com/rstudio/shiny-server).

#### Getting Started

Visit the Trelliscope [tutorial page](http://hafen.github.com/trelliscope/) for examples to get started.

## Notes

This is "research-grade" software, meaning that it is prototypical, potentially buggy, and will likely be changing as design issues are addressed.  While it is being actively used in many projects, the main intent currently for sharing on github is for collaboration on development and not for distribution to the masses.  Use at your own risk.

## License

This software is currently under the BSD license.  Please read the [license](https://github.com/hafen/trelliscope/blob/master/LICENSE.md) document.

