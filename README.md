# Getting started

## Packrat
Packrat is a package/dependency manager for R.

Run the deploy.R script via RStudio.
This will install the dependencies located in via Packrat.

See https://rstudio.github.io/packrat/walkthrough.html for more Packrat information.
## R Shiny
R Shiny has a default file size maximum of 5 MB. If you want to change that, put the next line in server.R:
```
options(shiny.maxRequestSize=30*1024^2)
```
