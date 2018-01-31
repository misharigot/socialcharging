# Quick start
1. Clone this project via a Git GUI or preferably via the command line: 
```
$ git clone https://github.com/misharigot/socialcharging.git
```
  >To open this project for the first time in RStudio:
  File > New Project... > Click on 'Existing Directory' > Browse to the cloned project > Click on 'Create Project'

2. Create a copy of `config.yml.dist` and rename it to `config.yml`.
3. Change the parameter values in `config.yml` to your own personal values.
3. Run the `/deploy.R` script (you can do this in RStudio) to install dependencies via Packrat and create a CSV with predicted data.

**Note: during the execution of the `deploy.R` script, an error might occur while installing packages. When this happens, keep running the `deploy.R` script, until all packages are installed.**

---

# General Info
### Data
The data used by this project is not included in this repository, due to privacy reasons.
To acquire your own data set, contact Social Charging.

The csv we received from Social Charging that is being used by the project has the following columns:
```
session_id;user_id;smart_charging;start_date;end_date;ev_provider;car;corporate;evse_id;latitude;longitude;address;kw  charge point speed);outlets;charged_kwh,
```

The parameter `scDataset` in your `config.yml` file should contain the path to your the Social Charging csv.

### Packrat
[Packrat](https://rstudio.github.io/packrat/walkthrough.html) is a package/dependency manager for R.
Run the `deploy.R` script via RStudio.
This will make Packrat install the dependencies and put them in the `/packrat` folder, based off of the dependencies/packages that are described in `packrat/packrat.lock`.

### R Shiny
To run R Shiny, open the `server.R` or `ui.R` file in RStudio and click on **Run App** (ctrl/cmd + shift + enter).

# In depth look of the project
This project consists of several directories and files that each have a different purpose. In this section the majority of these directories, modules and files will be explained, sometimes with an accompanying screenshot of the functionality's view in the R Shiny application.

## Root
##### Location: `/`
The root directory has several folders and files:

**Directories**
- `/src`, contains the core source code of the project.
- `/packrat`, the directory for the dependency manager [Packrat](https://rstudio.github.io/packrat/). Dependencies/packages are located here, instead of on your machine outside of your project. The `packrat.lock` file describes the packages this project uses and their versions.
- `/www`, resources, like images, that are used in the Shiny app.

**Notable files**
- `/config.yml.dist`, this file contains the global parameters that are used throughout the project. These parameters are specific to the user that checks out this project.
**Important:** In order to change these parameters to your personal values, a copy of this file should be made and named `config.yml`. This file is ignored by Git and will include your own personal parameter values. Examples of the values are in the .dist file.
- `/deploy.R`, this file can be run to deploy the project. This will install dependencies via [Packrat](https://rstudio.github.io/packrat/) and create a csv with values predicted by our machine learning models. If all goes well, the project can be run without problems after running this R file and configuring your personal `/config.yml` file.
- `lint-script.R`, this file can be used to lint the file in the path in `config.yml` under `fileToLint`.
- `server.R`, this file holds the server function for the Shiny application.
For more information, see the shiny documentation or follow the tutorial on https://shiny.rstudio.com/tutorial/.
- `ui.R`, this file holds the UI function for the Shiny application.
For more information, see the shiny documentation or follow the tutorial on https://shiny.rstudio.com/tutorial/.

## Core
##### Location: `/src`
This folder contains the core functionality of the Shiny App. Each of these files are in some way being used by the Shiny dashboard. The goal of the directories and files will be explained in the following part.

The `/src/base_clean.R` script cleans the CSV that Social Charging provided in the most generic manner, making the data fairly clean, ready to be used by other scripts throughout the project with the most basic cleaning already done.

### Corrupted Explorer
##### Location: `/src/corrupted_explorer`
This module visualizes which data is 'usable' by our machine learning prediction models, based on several filters you can manually change.

![Corrupted Explorer](https://image.prntscr.com/image/JFMqMa9wRXuBitNapI7ukw.png)

This module is located at `/src/corrupted_explorer/corrupted_explorer_module.R`. This script uses **Shiny Modules** to modularize Shiny code. A smaller version of `/server.R` and `/ui.R` are used, specifically for the logic of this module, so that `/server.R` and `/ui.R` stay clean and maintainable.
For more information on Shiny Modules, see https://shiny.rstudio.com/articles/modules.html.

### CSS
##### Location: `/src/css`
This directory holds css files. These files can be used in shiny's UI, by calling `tags$head(includeCSS("src/css/your_file_here.css"))`.

### Helpers
##### Location: `/src/helpers`
This directory holds all the helper scripts that contain utility functions.

- **helpers/data_helper.R** contains the `generatePredictionCsv()` function, that generates a prediction CSV in the config parameter `dataFolder`, in your `config.yml` file. This prediction CSV is used by several views visualizing our prediction modeling.
- **helpers/date_helper.R** contains functions regarding date, containing custom functions that [lubridate](https://github.com/tidyverse/lubridate) doesn't provide. These functions are mainly used by our Week Schedule module.
- **helpers/multiplot_helper.R** helps with displaying multiple plots as a single output.

### Map
##### Location: `/src/map`
This is where the map module exists. The map is being rendered by [leaflet](https://rstudio.github.io/leaflet/), a wrapper around a JavaScript map library.

![..](https://image.prntscr.com/image/cAMkVgRTQtuHBpAiiIKnsg.png)

- **map_functions.R** has functions that help create the objects within the leaflet map, like adding circles to the map, giving them color, different sizes based on data, or adding a legend to the map.
- **map_module.R** is the [Shiny Module](https://shiny.rstudio.com/articles/modules.html) for the leaflet map. It outputs the map (and the table when clicking on a station). The output is sent to the UI. This module can then be used by `/ui.R` and `/server.R`.

### Models
##### Location: `/src/models`
This directory contains the machine learning models. These each have different visualizations, ranging from a bar chart showing the distribution of classes, to a 3d view of the different clusters.

![..](https://image.prntscr.com/image/RnM2kxyfTYKVQgtDS7oSEQ.png)

**Correlation**
Correlations can be discovered interactively via the `Interactive_correlation.R` script. This is useful for discovering viable attributes in the data to be used as features for machine learning.

**Classification**
Classification is done on users (`user_class.R`) and on stations as well (`station_classification.R`).

**Clustering**
We use clustering on stations (`cluster_charging_station.R`) and on users (`user_clust.R`).

***Regression***
Regression is also being used to predict charging sessions for both users (`regression_user_class.R`) and stations (`regression_station_class`), which are based off of our classification models. However, this is not shown in the Shiny App, since the accuracy is currently too low with the data given.

### Plots
##### Location: `/src/plots`
Discovery plots are located here, each giving insight into the data by means of a visualization. Not all plots ended up being used within the Shiny application, but the project team decided to still keep them for reference.

![..](https://image.prntscr.com/image/ujX97016QhGDWkiQA_7hmg.png)

### Week Schedule
##### Location: `/server.R`
This view shows the predicted sessions for a future week for the selected user. These sessions are predicted by first classifying users and then using regression to predict the starting time, duration and charged kWh for sessions for each day of the week.

**Note:** The code for this module is located in `/server.R` under the comment `# Weekschedule ---`.

![..](https://image.prntscr.com/image/sTZkNa1rQgew_g2_-AH0ew.png)
