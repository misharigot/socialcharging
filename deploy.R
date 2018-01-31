if (require("packrat")) {
  packrat::restore()
} else {
  install.packages("packrat")
  packrat::restore()
}
if (!file.exists("config.yml")) {
  warning("---\nconfig.yml file not found.\nDon't forget to create a config.yml file (copied from config.yml.dist) and set your own parameters!")
}
writeLines("\nPackrat finished.")
writeLines("\nDeploy script finished.")
