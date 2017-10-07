# Getting started
Run the deploy.R script via the console with
```
Rscript deploy.R
```
This will install the dependencies via Packrat (dependency manager for R).

## R Shiny
R Shiny has a default file size maximum of 5 MB. If you want to change that, put the next line in server.R:
```
options(shiny.maxRequestSize=30*1024^2)
```
