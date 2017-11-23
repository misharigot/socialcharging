if (require("packrat")) {
  packrat::restore()
} else {
  warning('---\nDeploying failed: packrat is not installed.\nPlease execute install.packages("packrat")')
}
if (!file.exists("config.yml")) {
  warning("---\nconfig.yml file not found.\nDon't forget to create a config.yml file (copied from config.yml.dist) and set your own parameters!")
}
writeLines("\nPackrat finished.")

writeLines("\nStarting prediction csv creation")

source("src/helpers/data_helper.R")
generatePredictionCsv()
writeLines("\nDeploy script finished.")
