# socialcharging
The Social Charging Big Data project.

## R Shiny
R Shiny has a default file size maximum of 5 mb. If you want to change that, put the next line in the server.R:
```
options(shiny.maxRequestSize=30*1024^2)
```
